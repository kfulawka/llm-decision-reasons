import pandas as pd
import time
from openai import AzureOpenAI

# CLIENT FOR LLM CALLING
client = AzureOpenAI(
  azure_endpoint = "ENDPOINT", 
  api_key="KEY",  
  api_version="DATE"
)

# PATHS TO DATA FILES
project_path = "/path/to/project/"
decision_reasons_file = project_path + "00_data/decision_reasons.csv"
decision_problems_file = project_path + "00_data/choice_problems_txt.csv"
base_res_path = project_path + "02_llms_hpc/01_valid/red/gpt4om/" # or gpt4o for gpt-4o

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


# function for running decision reason analysis
def fetch_decision(problem_description, decision_reason, system_message):

    prompt ="Decision problem:\n"\
    f"{problem_description}\n\n\n"\
    "Decision reason:\n"\
    f"{decision_reason}"
    
    try:
        response = client.chat.completions.create(
            model="gpt-4o-mini", # or gpt-4o
            messages=[{"role": "system",
                       "content": system_message},
                      {"role": "user",
                       "content": prompt}, 
        ])
        return response.choices[0].message.content
    
    except Exception as e:
        print(e)
        return str(e)

def main():
    # Load data frames
    reasons = pd.read_csv(decision_reasons_file, delimiter=';')#.iloc[41:]
    # print(df_reasons)

    problems = pd.read_csv(decision_problems_file, delimiter=',')
    # print(df_problems)

    # Adding columns for each decision reason to the problems DataFrame
    for index, reason in reasons.iterrows():
        reason_name = reason['decision reason name']
        problems[reason_name] = problems.apply(
            lambda x: fetch_decision(problem_description = x['choice_problem'], 
                                    decision_reason = reason['decision reason description'],
                                    system_message = system_msg),
            axis=1
        )

        # Save the updated DataFrame
        res = base_res_path + reason_name + '.csv'
        
        # Add problem_id column (0 to 19)
        output_df = pd.DataFrame({
            "problem_index": range(len(problems)),
            reason_name: problems[reason_name]
        })

        # Save it
        output_df.to_csv(res, sep=',', index=False)
                
        print(reason_name)

if __name__ == "__main__":
    
    # start the timer
    start_time = time.time()
    
    # execute the script
    main()
    
    # End the timer
    end_time = time.time()
    
    # Calculate and print the execution time
    execution_time = end_time - start_time
    print(f"Script completed in {execution_time:.2f} seconds.")
