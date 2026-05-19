import os
# ========== ENV VARS FOR CLUSTER ==========
os.environ["HF_HOME"] = "YYY"

import argparse
import pandas as pd
from vllm import LLM

# ========== PATHS ==========
project_path = "XYZ"
verbal_reports_file = project_path + "00_data/verbal_reports.csv"

output_path = "ZYX/verbal_reports-q3_8b-embeddings/"

# ========== MODEL ==========
MODEL_NAME = "Qwen/Qwen3-Embedding-8B"

# ========== HELPER FUNCTION ==========
def get_detailed_instruct(task_description: str, query: str) -> str:
    """
    Formats the input for Qwen-Embedding queries.
    """
    return f'Instruct: {task_description}\nQuery:{query}'

# ========== MAIN EXECUTION ==========
if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--tp-size", type=int, default=1, help="Tensor Parallelism size.")
    args = parser.parse_args()

    # 1. Ensure Output Directory Exists
    os.makedirs(output_path, exist_ok=True)

    # 2. Load Data
    print("Loading dataset...")
    df_verbal = pd.read_csv(verbal_reports_file)
    print(f"   - Verbal Reports: {len(df_verbal)}")

    # 3. Data Preparation
    print("Formatting queries (Verbal Report ONLY)...")

    # Create the task instruction
    task_instruction = "Given an individual's verbal report about a choice between monetary lotteries, retrieve the relevant information on decision reasons."

    queries = []
    meta_data = [] 
    
    for _, row in df_verbal.iterrows():
        verbal_resp = row['response']
        
        # Apply Qwen Instruction Format
        formatted_query = get_detailed_instruct(task_instruction, verbal_resp)
        queries.append(formatted_query)
        
        # Keep track of metadata for the parquet file
        meta_data.append({
            "subject_id": row["subject_id"],
            "problemID": row["problemID"],
            "response": verbal_resp  
        })

    # 4. Initialize vLLM
    print(f"Initializing vLLM with {MODEL_NAME}...")
    llm = LLM(
        model=MODEL_NAME,
        # task="embed",
        tensor_parallel_size=args.tp_size,
        trust_remote_code=True,
        enforce_eager=True 
    )

    # 5. Generate Embeddings
    print(f"Embedding {len(queries)} Verbal Reports...")
    output_reports = llm.embed(queries)
    
    # Extract embeddings as a list of lists of floats
    emb_reports = [o.outputs.embedding for o in output_reports]

       # 6. Format and Save Results
    print("Expanding matrix and saving to Parquet...")
    
    # Create the metadata DataFrame (1720 rows, 3 cols)
    df_meta = pd.DataFrame(meta_data)
    
    # Create the embeddings DataFrame (1720 rows, 4096 cols)
    df_embs = pd.DataFrame(emb_reports)
    
    # Rename embedding columns to emb_0, emb_1, ... emb_4095
    df_embs.columns = [f"emb_{i}" for i in range(df_embs.shape[1])]

    # Concatenate side-by-side
    df_final = pd.concat([df_meta, df_embs], axis=1)

    # Save Wide Format to parquet with gzip compression
    final_parquet_path = os.path.join(output_path, "verbal_reports_embeddings.parquet.gzip")
    df_final.to_parquet(final_parquet_path, compression='gzip', index=False)
    
    print(f"Dataframe shape: {df_final.shape}") # Should print (1720, 4099)
    print(f"Embeddings saved to: {final_parquet_path}")
    print(f"\nDone. Results located in {output_path}")
