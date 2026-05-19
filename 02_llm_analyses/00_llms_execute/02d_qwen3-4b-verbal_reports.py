import os
# ========== ENV VARS FOR CLUSTER ==========
os.environ["HF_HOME"] = "YYY"

import argparse
import sys
import time
import pandas as pd
from transformers import AutoTokenizer
from vllm import LLM, SamplingParams
from tqdm import tqdm 


MODEL_NAME = "Qwen/Qwen3-4B-Instruct-2507"

# ========== PATHS ==========
project_path = "XYZ"
decision_reasons_file = project_path + "00_data/decision_reasons.csv"
decision_problems_file = project_path + "00_data/id_decision_problems.csv"
verbal_reports_file = project_path + "00_data/verbal_reports.csv"
base_res_path = "ZYX/verbal_reports_res/qwen3-4b"


# ========== SYSTEM MESSAGE ==========
system_msg = """You are a decision analyst specializing in evaluating decision-making processes. 
Your task is to assess whether a specific decision reason is present in a verbal report of an individual who have made a choice between two monetary lotteries.


Available information — 
A decision problem poses a choice between two lotteries, A and B, offering different monetary outcomes with different probabilities.

A decision reason specifies a rule to decide which of the two lotteries is preferred by the reason. The decision reason prefers A or B or is indifferent between the lotteries.

A verbal report written by an individual describes, in retrospect, the individual’s deliberation process used to choose one of the lotteries of the decision problem.


Task description —
Your task is to assess, based on the verbal report, whether the individual used the reason to make the decision. 
Conduct the assessment step-by-step. Here are the steps:

Step 1: Assess if the decision reason can be applied to the decision problem. 
Evaluate whether the information relevant to the decision reason can be derived from the lotteries' outcomes and probabilities and summarize this information. 
Proceed to Step 2.

Step 2: Assess the verbal report. First, evaluate and summarize information considered by the individual. 
Second, evaluate and summarize the individual’s justification for the choice. 
Focus on the described deliberation process and ignore information about the individual’s final choice. 
Proceed to Step 3. 

Step 3: Assess whether the decision reason was used by the individual. 
First, compare the information relevant to the decision reason with that considered by the individual. 
Second, compare the decision reason’s rule to the individual’s justification for the choice. 
Based on these two comparisons, assess whether the individual used the decision reason to make the decision. 
Indicate your final assessment on a scale between 0 and 100. 
Interpret the endpoints and the midpoint in the following way:
    - 0: Certain that the decision reason was not used by the individual.
    - 50: Uncetrain whether the decision reason was used or not used by the individual.
    - 100: Certain that the decision reason was used by the individual.


### Output Format
Respond strictly in this JSON format:
{
  "step_1": "...",
  "step_2": "...",
  "step_3": "...",
  "final_assessment_score": <Integer 0-100>
}
"""


# ========== DATA PREPARATION ==========
def prepare_batch_for_subject(subject_id, df_all, df_reasons, tokenizer):
    """
    Filters the global dataframe for a specific subject and creates prompts.
    """
    df_subject = df_all[df_all["subject_id"] == subject_id]

    final_prompts = []
    meta = []

    # Iterate through all 47 reasons for every problem the subject answered
    for _, reason_row in df_reasons.iterrows():
        reason_name = reason_row["decision reason name"]
        reason_desc = reason_row["decision reason description"]

        for _, row in df_subject.iterrows():
            problem_id = row["problemID"]
            verbal = row["response"]
            problem_text = row["context"]
            
            # Construct Chat
            messages = [
                {"role": "system", "content": system_msg},
                {"role": "user", "content": f"Decision problem:\n{problem_text}\n\nDecision reason:\n{reason_desc}\n\nVerbal report:\n{verbal}"}
            ]
            
            # Apply Template
            prompt_str = tokenizer.apply_chat_template(messages, tokenize=False, add_generation_prompt=True)
            
            final_prompts.append(prompt_str)
            meta.append({
                "subject_id": subject_id, 
                "problemID": problem_id, 
                "reason_name": reason_name
            })

    return final_prompts, pd.DataFrame(meta).reset_index(drop=True)

# ========== SAVING RESULTS ==========
def save_results(subject_meta, responses, subject_folder):
    """
    Saves grouped CSVs into the subject's folder.
    """
    subject_meta["response"] = responses
    
    # Ensure folder exists
    os.makedirs(subject_folder, exist_ok=True)

    # Group by reason name to create the specific files (e.g. 'expected_value.csv')
    for reason_name, group_df in subject_meta.groupby("reason_name"):
        filename = reason_name.strip().replace(" ", "_").lower() + ".csv"
        path = os.path.join(subject_folder, filename)
        
        # Check for existing file to append (in case of restarts)
        if os.path.exists(path):
            existing = pd.read_csv(path)
            updated = pd.concat([existing, group_df[["problemID", "response"]]], ignore_index=True)
        else:
            updated = group_df[["problemID", "response"]]
        
        # Deduplicate to be safe
        updated = updated.drop_duplicates(subset=["problemID"]).copy()
        updated.to_csv(path, index=False)

# ========== MAIN EXECUTION ==========
if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    # Removed --subject-id, we now loop over all
    parser.add_argument("--tp-size", type=int, default=4, help="Tensor Parallelism size.")
    parser.add_argument("--max-model-len", type=int, default=1024*8, help="Max context length.")
    parser.add_argument("--max-new-tokens", type=int, default=1024*3, help="New tokens to generate.")
    parser.add_argument("--gpu_mem_util", type=float, default=0.9, help="vLLM gpu_memory_utilization.")
    parser.add_argument("--max_num_batched_tokens", type=int, default=1024*24, help="Prefill token limit.")
    
    args = parser.parse_args()

    # 1. Initialize vLLM (ONCE)
    print(f"Initializing vLLM (TP={args.tp_size})...")
    
    llm_kwargs = dict(
        model=MODEL_NAME,
        tokenizer=MODEL_NAME,
        tensor_parallel_size=args.tp_size,
        gpu_memory_utilization=args.gpu_mem_util,
        dtype="auto",
        max_model_len=args.max_model_len,
        max_num_batched_tokens=args.max_num_batched_tokens,
        trust_remote_code=True
    )

    llm = LLM(**llm_kwargs)
    print("Engine initialized.")
    
    tokenizer = AutoTokenizer.from_pretrained(MODEL_NAME)

    # 2. Load Global Data (ONCE)
    print("Loading datasets...")
    df_reasons = pd.read_csv(decision_reasons_file, sep=';')
    df_problems = pd.read_csv(decision_problems_file)
    df_verbal = pd.read_csv(verbal_reports_file)
    
    # Clean whitespace
    df_reasons["decision reason name"] = df_reasons["decision reason name"].str.strip()
    df_reasons["decision reason description"] = df_reasons["decision reason description"].str.strip()

    # Merge to get Master Dataframe
    df_all = df_verbal.merge(df_problems, on=["subject_id", "problemID"])
    
    # Get unique subjects list
    all_subjects = sorted(df_all["subject_id"].unique())
    print(f"Found {len(all_subjects)} subjects to process.")

    # 3. Sampling Parameters (Qwen Recommended)
    sampling_params = SamplingParams(
        max_tokens=args.max_new_tokens,
        temperature=0.7,
        top_p=0.8,
        top_k=20,
        min_p=0.0
    )

    # 4. Global Loop
    start_total = time.time()
    
    for subject_id in tqdm(all_subjects, desc="Processing Subjects"):
        # Define output folder
        subject_folder = os.path.join(base_res_path, f"S{subject_id:02d}")
        
        # Check completeness (assuming 47 reasons = 47 files)
        if os.path.exists(subject_folder) and len(os.listdir(subject_folder)) >= 47:
            print(f"Subject {subject_id} already done. Skipping.")
            continue
            
        print(f"\nStarting Subject {subject_id:02d}...")
        t0 = time.time()

        # Prepare Batch
        prompts, subject_meta = prepare_batch_for_subject(subject_id, df_all, df_reasons, tokenizer)
        
        if not prompts:
            print(f"No prompts generated for Subject {subject_id}. Check data.")
            continue

        # Run Inference
        # vLLM will batch this internally very efficiently
        outputs = llm.generate(prompts, sampling_params)
        
        # Extract Text
        generated_texts = [output.outputs[0].text.strip() for output in outputs]
        
        # Save
        save_results(subject_meta, generated_texts, subject_folder)
        
        print(f"Subject {subject_id} finished in {time.time() - t0:.1f}s")

    print(f"\n All subjects completed in {time.time() - start_total:.1f}s")
