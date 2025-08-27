import os
# ========== ENV VARS FOR CLUSTER ==========
os.environ["HF_HOME"] = "/data/horse/ws/kafu622g-rdm_llms/huggingface"
os.environ["TRANSFORMERS_CACHE"] = "/data/horse/ws/kafu622g-rdm_llms/huggingface/hub"

import argparse
import sys
import time
import pandas as pd
from transformers import AutoTokenizer, AutoModelForCausalLM
import torch
from tqdm.auto import tqdm

# ========== PATHS ==========
project_path = "/home/h1/kafu622g/02_llms_hpc/"
decision_reasons_file = project_path + "00_data/decision_reasons.csv"
decision_problems_file = project_path + "00_data/id_decision_problems.csv"
verbal_reports_file = project_path + "00_data/verbal_reports.csv"
base_res_path = project_path + "02_verbal_reports/llama33-70b"

# ========== SYSTEM MESSAGE ==========
system_msg ="""You are a decision analyst specializing in evaluating decision-making processes. 
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
    - 50: Uncertain whether the decision reason was used or not used by the individual.
    - 100: Certain that the decision reason was used by the individual.


Output structure — use the following template:
STEPS: <describe each reasoning step>
FINAL ASSESSMENT: <provide final numerical assessment>
"""

# ========== FUNCTIONS ==========
def load_model_and_tokenizer(model_name):
    model = AutoModelForCausalLM.from_pretrained(
        model_name, device_map="auto", torch_dtype="auto"
    )
    tokenizer = AutoTokenizer.from_pretrained(model_name)
    tokenizer.padding_side = "left"
    tokenizer.pad_token = tokenizer.eos_token
    model.eval()
    return model, tokenizer

def load_and_prepare_subject_data(subject_id):
    df_reasons = pd.read_csv(decision_reasons_file, sep=';')
    df_problems = pd.read_csv(decision_problems_file)
    df_verbal = pd.read_csv(verbal_reports_file)
    df_reasons["decision reason name"] = df_reasons["decision reason name"].str.strip()
    df_reasons["decision reason description"] = df_reasons["decision reason description"].str.strip()

    df_all = df_verbal.merge(df_problems, on=["subject_id", "problemID"])
    df_subject = df_all[df_all["subject_id"] == subject_id]

    prompts, meta = [], []
    for _, reason_row in df_reasons.iterrows():
        reason_name = reason_row["decision reason name"]
        reason_desc = reason_row["decision reason description"]

        for _, row in df_subject.iterrows():
            problem_id, verbal, problem_text = row["problemID"], row["response"], row["context"]
            prompts.append({
                "messages": [
                    {"role": "system", "content": system_msg},
                    {"role": "user", "content": f"Decision problem:\n{problem_text}\n\nDecision reason:\n{reason_desc}\n\nVerbal report:\n{verbal}"}
                ]
            })
            meta.append({"subject_id": subject_id, "problemID": problem_id, "reason_name": reason_name})

    return prompts, pd.DataFrame(meta).reset_index(drop=True), df_reasons

def create_dynamic_batches(prompts, tokenizer, max_total_tokens=90000):
    batches = []
    current_batch, current_tokens = [], 0

    for p in prompts:
        formatted = tokenizer.apply_chat_template(p["messages"], tokenize=False, add_generation_prompt=True)
        token_count = len(tokenizer(formatted)["input_ids"])

        if current_tokens + token_count > max_total_tokens and current_batch:
            batches.append(current_batch)
            current_batch, current_tokens = [], 0

        current_batch.append(p)
        current_tokens += token_count

    if current_batch:
        batches.append(current_batch)
    return batches

def run_inference(subject_id, model, tokenizer, subject_prompts, subject_meta, subject_folder):
    batches = create_dynamic_batches(subject_prompts, tokenizer)
    meta_batches = []
    start_idx = 0
    for batch in batches:
        meta_batches.append(subject_meta.iloc[start_idx:start_idx + len(batch)].copy())
        start_idx += len(batch)

    max_new_tokens = 1024
    for batch_idx, (batch, meta_batch) in enumerate(tqdm(zip(batches, meta_batches), total=len(batches), desc=f"S{subject_id:02d}")):
        formatted = [tokenizer.apply_chat_template(p["messages"], tokenize=False, add_generation_prompt=True) for p in batch]
        inputs = tokenizer(formatted, return_tensors="pt", padding=True, truncation=True)
        input_lengths = [len(tokenizer(p)["input_ids"]) for p in formatted]
        inputs = {k: v.to(model.device) for k, v in inputs.items()}

        start = time.time()
        with torch.no_grad():
            output_ids = model.generate(
                **inputs, max_new_tokens=max_new_tokens, do_sample=True,
                temperature=0.7, top_k=50, top_p=0.95
            )
        print(f"⏱️ Batch {batch_idx + 1} took {time.time() - start:.2f}s")

        responses = [tokenizer.decode(output[in_len:], skip_special_tokens=True).strip() for output, in_len in zip(output_ids, input_lengths)]
        meta_batch["response"] = responses

        for reason_name, group_df in meta_batch.groupby("reason_name"):
            filename = reason_name.strip().replace(" ", "_").lower() + ".csv"
            path = os.path.join(subject_folder, filename)
            if os.path.exists(path):
                existing = pd.read_csv(path)
                updated = pd.concat([existing, group_df[["problemID", "response"]]], ignore_index=True)
            else:
                updated = group_df[["problemID", "response"]]
            updated = updated.drop_duplicates(subset=["problemID"]).copy()
            updated.to_csv(path, index=False)
            print(f"💾 Saved batch to: {path} (+{len(group_df)} responses)")

# ========== MAIN EXECUTION ==========
if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--subject-id", type=int, required=True)
    args = parser.parse_args()
    subject_id = args.subject_id
    print(f"▶️ Subject {subject_id:02d}")

    subject_folder = os.path.join(base_res_path, f"S{subject_id:02d}")
    if os.path.exists(subject_folder) and len(os.listdir(subject_folder)) >= 47:
        print(f"✅ Subject {subject_id} already processed. Skipping.")
        sys.exit(0)
    os.makedirs(subject_folder, exist_ok=True)

    start_sub = time.time()

    subject_prompts, subject_meta, df_reasons = load_and_prepare_subject_data(subject_id)
    print(f"✅ Created {len(subject_prompts)} prompts and {len(subject_meta)} metadata entries.")

    # model_name = "microsoft/phi-4"
    model_name = "meta-llama/Llama-3.3-70B-Instruct"
    model, tokenizer = load_model_and_tokenizer(model_name)

    run_inference(subject_id, model, tokenizer, subject_prompts, subject_meta, subject_folder)

    print(f"✅ Subject {subject_id} completed in {time.time() - start_sub:.2f}s")
