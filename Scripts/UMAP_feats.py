
# Import Packages
# (skipped umap in-line bit)
import os
import numpy as np
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import StandardScaler
import matplotlib.pyplot as plt
import seaborn as sns
import pandas as pd
import umap.umap_ as umap
import umap.plot
import random

random.seed(123)

# Uppload the csv of features and copy the path
feats_path = r'C:/Reef soundscapes with AI/Results/PCNN_features/Kimbe_indices.csv'

data = pd.read_csv (feats_path) #load dataframe
data.head()

# Create a UMAP mapper
mapper = umap.UMAP(n_neighbors=25, min_dist=0.5, n_components=2).fit(data.iloc[:,2:21])

# Transform the data to lower-dimensional space
umap_data = mapper.transform(data.iloc[:,2:21])

# Convert to DataFrame
umap_df = pd.DataFrame(data=umap_data, columns=[f"UMAP_{i+1}" for i in range(umap_data.shape[1])])

# Optionally, concatenate with original DataFrame to keep 'habitat' and 'reef' information
final_df = pd.concat([data[['minute']], umap_df], axis=1)

final_df.head()

# Save to CSV, insert path to your data folder:
final_df.to_csv('C:/data/UMAP_feats.csv', index=False,)
