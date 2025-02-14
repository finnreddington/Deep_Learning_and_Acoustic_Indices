# README

Steps to replicate the analysis in *Combining Deep Learning with Standard Acoustic Indices Gives Greater Understanding of Coral Reef Soundscapes* (2025), use the scripts (indicated in *italics*) following the steps below.

## In Python, calculate indices and deep-learning features:

Following the analysis by [Williams et al. (2024)](https://github.com/BenUCL/Reef-acoustics-and-AI).

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

## Recreate the conda environment used for this project:
The analysis in this study was performed on a Dell XPS 15 and Windows 11. To recreate the env, run the following commands:

```
# clone this repo and navigate to it
git clone https://github.com/finnreddington/Deep_Learning_and_Acoustic_Indices.git 
cd Deep_Learning_and_Acoustic_Indices 
```

You will then need to download the vggish_model.ckpt file from the following link: 
https://storage.googleapis.com/audioset/vggish_model.ckpt

Then place this in the Audioset folder and run the following commands:

```
# Recreate the conda env from the .yml
conda env create --file vggish-env.yml --name vggish-env 

# Activate the conda env
conda activate vggish-env 

# Run the smoke test to check vggish is working in the env. Should return 'Looks good to me!'.
cd Audioset
python vggish_smoketest.py
```
