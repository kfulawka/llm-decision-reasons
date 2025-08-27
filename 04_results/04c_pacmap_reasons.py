#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Mar  5 21:12:18 2025

@author: ksmil
"""

import pandas as pd
import pacmap
import numpy as np

# REASONS ---------------------------------------------------------------------

# Load the dataset
dr = pd.read_csv("reasonsXproblems.csv")

# Ensure 'id2' is preserved and extract feature columns
problemID = dr["problemID"]
features = dr.drop(columns=["problemID"])  # Adjust if other non-numeric columns exist

# Initialize and fit PacMAP
pacmap_model = pacmap.PaCMAP(n_components=2,
                            n_neighbors=9,
                            MN_ratio=0.5,
                            FP_ratio=2.0
                            )
X_embedded = pacmap_model.fit_transform(features.values)

# Convert to DataFrame and append id2
result_dr = pd.DataFrame(X_embedded, columns=["dim1", "dim2"])
result_dr["problemID"] = problemID

# Save result
result_dr.to_csv("reasonsXproblems_pacmap.csv", index=False)
print("2D embedding saved to reasonsXproblems_pacmap.csv")
