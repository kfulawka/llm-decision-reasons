import os
# ========== ENV VARS FOR CLUSTER ==========
os.environ["HF_HOME"] = "ZZZ"

import argparse
import sys
import time
import pandas as pd
from transformers import AutoTokenizer
from vllm import LLM, SamplingParams
from tqdm import tqdm 
import re 

# ========== PATHS ==========
# Base project path
project_path = "ZZZ"
decision_reasons_file = project_path + "00_data/decision_reasons.csv"
decision_problems_file = project_path + "00_data/choice_problems_txt.csv"
base_res_path = "ZZZ/undr_valid_res"

# SYSTEM MESSAGE
system_msg ="""You are a decision analyst who accurately applies decision reasons to decision problems and identifies which lottery the decision reason prefers or whether the decision reason is indifferent between the lotteries. 

Available information — 
A decision problem poses a choice between two lotteries, A and B, offering different monetary outcomes with different probabilities.

A decision reason specifies a rule to decide which of the two lotteries is preferred by the reason. The decision reason prefers A or B or is indifferent between the lotteries.

Task description —
Your task is to identify the lottery preferred by the decision reason by applying the reason to the decision problem step-by-step. Here are the steps:

Step 1: Assess if the decision reason can be applied to the decision problem in a strict manner. 
This requires evaluating whether the relevant information can be derived from the lotteries' outcomes and probabilities. 
If the relevant information cannot be derived, you should be indifferent between the lotteries and return INDIFFERENT as the final answer. 
Otherwise, summarize the information about the lotteries relevant to the decision reason and proceed to Step 2.

Step 2: Apply the decision reason to the decision problem by strictly following the decision reason’s rule. 
Use the relevant information summarized in Step 1. If the decision reason prefers both lotteries equally, return as the final answer INDIFFERENT. 
Otherwise, if the decision reason prefers one lottery, return as the final answer A for lottery A or B for lottery B.

### Output Format
Respond strictly in this JSON format:
{
  "step_1": "...",
  "step_2": "...",
  "final_answer": "..."
}
"""

# ========== MODELS ==========
# model_name = "meta-llama/Llama-3.3-70B-Instruct"
# model_name = "microsoft/phi-4"
# model_name = "meta-llama/Llama-3.1-8B-Instruct"
# model_name = "allenai/OLMo-2-0325-32B-Instruct"
# model_name = "Qwen/Qwen2.5-32B-Instruct"
# model_name = "mistralai/Mistral-Small-24B-Instruct-2501"

# ========== DATA PREPARATION ==========
def prepare_all_prompts(df_reasons, df_problems, tokenizer):
    """
    Creates the full list of prompts for every combination of Reason x Problem.
    """
    final_prompts = []
    meta = []

    # Iterate through all reasons
    for r_idx, reason_row in df_reasons.iterrows():
        reason_desc = reason_row['decision reason description'].strip()
        reason_name = reason_row['decision reason name'].strip()

        # Iterate through all problems
        for p_idx, problem_row in df_problems.iterrows():
            problem_text = problem_row['choice_problem'].strip()
            
            # Construct Chat
            messages = [
                {"role": "system", "content": system_msg},
                {"role": "user", "content": f"Decision problem:\n{problem_text}\n\nDecision reason:\n{reason_desc}"}
            ]
            
            # Apply Template
            prompt_str = tokenizer.apply_chat_template(messages, tokenize=False, add_generation_prompt=True)
            
            final_prompts.append(prompt_str)
            meta.append({
                "reason_index": r_idx,
                "reason_name": reason_name,
                "problem_index": p_idx
            })

    return final_prompts, pd.DataFrame(meta)

# ========== SAVING RESULTS ==========
def save_results(subject_meta, responses, output_folder):
    """
    Saves grouped CSVs into the output folder.
    """
    subject_meta["response"] = responses
    
    # Ensure folder exists
    os.makedirs(output_folder, exist_ok=True)

    # Group by reason name to create specific files
    unique_reasons = subject_meta["reason_name"].unique()
    print(f"Saving results for {len(unique_reasons)} reasons into {output_folder}...")

    for reason_name, group_df in subject_meta.groupby("reason_name"):
        filename = reason_name.strip().replace(" ", "_").lower() + ".csv"
        path = os.path.join(output_folder, filename)
        
        # Check for existing file to append (in case of restarts)
        if os.path.exists(path):
            existing = pd.read_csv(path)
            updated = pd.concat([existing, group_df[["problem_index", "response"]]], ignore_index=True)
        else:
            updated = group_df[["problem_index", "response"]]
        
        # Deduplicate to be safe
        updated = updated.drop_duplicates(subset=["problem_index"]).copy()
        updated.to_csv(path, index=False)

# ========== MAIN EXECUTION ==========
if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    # Model argument (required)
    parser.add_argument("--model", type=str, required=True, help="HuggingFace model ID")
    
    # vLLM arguments
    parser.add_argument("--tp-size", type=int, default=4, help="Tensor Parallelism size (number of GPUs).")
    parser.add_argument("--max-model-len", type=int, default=1024*8, help="Max context length.")
    parser.add_argument("--max-new-tokens", type=int, default=1024*2, help="New tokens to generate.")
    parser.add_argument("--gpu_mem_util", type=float, default=0.9, help="vLLM gpu_memory_utilization.")
    parser.add_argument("--max_num_batched_tokens", type=int, default=1024*24, help="Prefill token limit.")
    
    args = parser.parse_args()

    # 1. Generate Folder Name Logic
    # Get short name, lowercase, remove dots
    raw_name = args.model.split("/")[-1].lower().replace(".", "")
    
    # Regex to find the parameter size (digits followed by 'b', e.g., '70b', '32b')
    match = re.search(r"(\d+b)", raw_name)
    if match:
        # Cut the string immediately after the 'b'
        folder_name = raw_name[:match.end()]
    else:
        # Fallback if no size found (e.g. 'gpt2')
        folder_name = raw_name

    output_res_path = os.path.join(base_res_path, folder_name)
    
    print(f"Initializing vLLM for model: {args.model}")
    print(f"Results will be saved to: {output_res_path}")

    # 2. Initialize vLLM
    llm_kwargs = dict(
        model=args.model,
        tokenizer=args.model,
        tensor_parallel_size=args.tp_size,
        gpu_memory_utilization=args.gpu_mem_util,
        dtype="auto",
        max_model_len=args.max_model_len,
        max_num_batched_tokens=args.max_num_batched_tokens,
        trust_remote_code=True
    )

    llm = LLM(**llm_kwargs)
    tokenizer = AutoTokenizer.from_pretrained(args.model)
    print("Engine initialized.")

    # 3. Load Data
    print("Loading datasets...")
    df_reasons = pd.read_csv(decision_reasons_file, sep=';')
    df_problems = pd.read_csv(decision_problems_file, sep=',')
    
    print(f"Found {len(df_reasons)} reasons and {len(df_problems)} problems.")
    
    # 4. Prepare Prompts
    prompts, meta_df = prepare_all_prompts(df_reasons, df_problems, tokenizer)
    print(f"Total prompts generated: {len(prompts)}")

    # 5. Sampling Parameters
    sampling_params = SamplingParams(
        max_tokens=args.max_new_tokens,
        temperature=0.7,
        top_p=0.8,
        top_k=20,
        min_p=0.0
    )

    # 6. Run Inference
    start_time = time.time()
    print("Starting generation...")
    
    outputs = llm.generate(prompts, sampling_params)
    
    # Extract Text
    generated_texts = [output.outputs[0].text.strip() for output in outputs]
    
    duration = time.time() - start_time
    print(f"Generation finished in {duration:.1f}s")

    # 7. Save Results
    save_results(meta_df, generated_texts, output_res_path)
    
    print(f"Done. Results in {output_res_path}")
