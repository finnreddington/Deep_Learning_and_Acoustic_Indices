# README

Steps to replicate the analysis in *Combining Deep Learning with Standard Acoustic Indices Gives Greater Understanding of Coral Reef Soundscapes* (2025), use the scripts (indicated in *italics*) following the steps below.

## In Python, calculate indices and deep-learning features:

Following the analysis by Williams et al. (2024)

The following scripts were altered to read differently labelled data; otherwise, the model is identical to Williams et al. (2024):  

- *feat_extr_with_PCNN_Kimbe_Bay.py*  
- *feat_extr_with_compound_index_Kimbe_Bay.py*  

## Prepare indices for UMAP in R:  

- *Preparing_Indices_for_UMAP_in_Python.R*  

## In Python, perform UMAP on DL feats and indices using:  

- *UMAP_feats.py*  
- *UMAP_indices.py*  

## In R, perform clustering analysis and plot UMAPs:  

- *Cluster_analysis.R*
- *UMAP_plots.R*

## In R, perform redundancy analysis and plot:  

- *Redundancy_Analysis.R*  



