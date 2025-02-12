# Deep_Learning_and_Acoustic_Indices
Repository for the analysis presented in Reddington et al. (2025) 'Combining Deep Learning with Standard Acoustic Indices Gives Greater Understanding of Coral Reef Soundscapes'

TO replicate the analysis, follow the steps below use the scripts (indicated in italics):
  ## 1.	In Python, calculate indices and deep-learning features:
        a.	Following the analysis by Williams et al. (2024)

        b.	The following scripts were altered for to read differently labelled data, otherwise the model is identical to Williams et al (2024): 
            feat_extr_with_PCNN_Kimbe_Bay.py
            *feat_extr_with_compound_index_Kimbe_Bay.py*

        c.	Prepare indices for UMAP in R: 
            Preparing_Indices_for_UMAP_in_Python.R 


  # 2.	In Python Perform UMAP on DL feats and indices using:
        UMAP_feats.py
        UMAP_indices.py

  # 3.	In R, perform clustering analysis and plot:
        Cluster_analysis.R

  # 4.	In R, perform redundancy analysis and plot:
        Redundancy_Analysis.R 



