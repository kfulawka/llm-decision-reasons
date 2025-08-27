import os
# os.environ["HF_HOME"] = "/path/to/huggingface"
# os.environ["TRANSFORMERS_CACHE"] = "/path/to/huggingface/hub"

import time
import pandas as pd
from transformers import AutoTokenizer, AutoModelForCausalLM
import torch
from tqdm.auto import tqdm


# PATHS TO DATA FILES
project_path = "/path/to/project/"
decision_reasons_file = project_path + "00_data/decision_reasons.csv"
decision_problems_file = project_path + "00_data/choice_problems_txt.csv"


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

Output structurte — use the following template:
STEPS: <describe each reasoning step>
FINAL ANSWER: <provide final answer>"""

# ADJUST TO MODEL USED!!!
# base_res_path = project_path + "01_valid/res/llama33-70b/"
# base_res_path = project_path + "01_valid/res/phi4/"
# base_res_path = project_path + "01_valid/res/llama31-8b/"
# base_res_path = project_path + "01_valid/res/olmo2-32b"
# base_res_path = project_path + "01_valid/res/qwen25-32b/"
base_res_path = project_path + "01_valid/res/mistral31-24b/"


# LOAD THE MODEL
# model_name = "meta-llama/Llama-3.3-70B-Instruct"
# model_name = "microsoft/phi-4"
# model_name = "meta-llama/Llama-3.1-8B-Instruct"
# model_name = "allenai/OLMo-2-0325-32B-Instruct"
# model_name = "Qwen/Qwen2.5-32B-Instruct"
model_name = "mistralai/Mistral-Small-24B-Instruct-2501"

model = AutoModelForCausalLM.from_pretrained(
    model_name,
    device_map="auto",
    torch_dtype="auto" 
)

tokenizer = AutoTokenizer.from_pretrained(model_name)
tokenizer.padding_side = "left"

# ONLY FOR LLAMA3.1-8B !!!
tokenizer.pad_token = tokenizer.eos_token  # Fix the padding issue

# switch to inference
model.eval()


# load data
reasons = pd.read_csv(decision_reasons_file, delimiter=';')
problems = pd.read_csv(decision_problems_file, delimiter=',')

all_prompts = []
meta_info = []

for r_idx, reason_row in reasons.iterrows():
    reason_desc = reason_row['decision reason description'].strip()
    reason_name = reason_row['decision reason name'].strip()

    for p_idx, problem_row in problems.iterrows():
        problem_text = problem_row['choice_problem'].strip()

        all_prompts.append({
            "messages": [
                {"role": "system", "content": system_msg},
                {"role": "user", "content": f"Decision problem:\n{problem_text}\n\nDecision reason:\n{reason_desc}"}
            ]
        })

        meta_info.append({
            "reason_index": r_idx,
            "reason_name": reason_name,
            "problem_index": p_idx
        })


# INFERENCE SETTINGS
batch_size = 160
max_new_tokens=1024


# --- BATCHED GENERATION + SAVE PER BATCH ---
for i in tqdm(range(0, len(all_prompts), batch_size)):
    batch = all_prompts[i:i + batch_size]
    meta = meta_info[i:i + batch_size]

    formatted = [
        tokenizer.apply_chat_template(p["messages"], tokenize=False, add_generation_prompt=True)
        for p in batch
    ]
    inputs = tokenizer(formatted, return_tensors="pt", padding=True, truncation=True)
    input_lengths = [len(tokenizer(p)["input_ids"]) for p in formatted]

    inputs = {k: v.to(model.device) for k, v in inputs.items()}

    start = time.time()
    with torch.no_grad():
        output_ids = model.generate(
            **inputs,
            max_new_tokens=max_new_tokens,
            do_sample=True,
            temperature=0.7,
            top_k=50,
            top_p=0.95
        )
    elapsed = time.time() - start
    print(f"Batch {i // batch_size + 1} took {elapsed:.2f}s")

    responses = []
    for output, in_len in zip(output_ids, input_lengths):
        gen_ids = output[in_len:]
        response = tokenizer.decode(gen_ids, skip_special_tokens=True).strip()
        responses.append(response)

    # SAVE CURRENT BATCH
    df_batch = pd.DataFrame(meta)
    df_batch["response"] = responses

    for reason_name, group_df in df_batch.groupby("reason_name"):
        filename = reason_name.strip().replace(" ", "_").lower() + ".csv"
        path = os.path.join(base_res_path, filename)
        if os.path.exists(path):
            existing_df = pd.read_csv(path)
            updated_df = pd.concat([existing_df, group_df[["problem_index", "response"]]], ignore_index=True)
        else:
            updated_df = group_df[["problem_index", "response"]]
        updated_df.to_csv(path, index=False)
        print(f"✅ Saved batch to: {path} (+{len(group_df)} responses)")
