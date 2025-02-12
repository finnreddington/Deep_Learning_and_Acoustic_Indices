# Deep_Learning_and_Acoustic_Indices
Repository for the analysis presented in Reddington et al. (2025) 'Combining Deep Learning with Standard Acoustic Indices Gives Greater Understanding of Coral Reef Soundscapes'

TO replicate the analysis, follow the steps below use the scripts (indicated in italics):
  ## 1.	In Python, calculate indices and deep-learning features:
        a.	Following the analysis by Williams et al. (2024)

        b.	The following scripts were altered for to read differently labelled data, otherwise the model is identical to Williams et al (2024): 
            _feat_extr_with_PCNN_Kimbe_Bay.py_
            _feat_extr_with_compound_index_Kimbe_Bay.py_

        c.	Prepare indices for UMAP in R: 
          _  Preparing_Indices_for_UMAP_in_Python.R _

  ## 2.	In Python Perform UMAP on DL feats and indices using:
        _UMAP_feats.py
        UMAP_indices.py_

  ## 3.	In R, perform clustering analysis and plot:
 _       Cluster_analysis.R_

  ## 4.	In R, perform redundancy analysis and plot:
       _ Redundancy_Analysis.R _



