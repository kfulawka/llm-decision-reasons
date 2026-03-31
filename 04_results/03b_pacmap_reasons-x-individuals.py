import pandas as pd
import pacmap

# REASONS ---------------------------------------------------------------------

# Load the dataset
dr = pd.read_csv("03a_reasons-x-individuals.csv")

# Ensure 'id2' is preserved and extract feature columns
sub = dr["subject_id"]
features = dr.drop(columns=["subject_id"])  # Adjust if other non-numeric columns exist

# Initialize and fit PacMAP
pacmap_model = pacmap.PaCMAP(n_components=2,
                            n_neighbors=10,
                            MN_ratio=0.5,
                            FP_ratio=2.0,
                            random_state=27)
X_embedded = pacmap_model.fit_transform(features.values)

# Convert to DataFrame and append id2
result_dr = pd.DataFrame(X_embedded, columns=["dim1", "dim2"])
result_dr["subject_id"] = sub

# Save result
result_dr.to_csv("03b_reasons-x-individuals_pacmap.csv", index=False)
print("2D embedding saved")
