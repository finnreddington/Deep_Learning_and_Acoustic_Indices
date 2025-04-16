# Clustering Analysis
library(tidyverse)
library(ClusterR)
library(cluster)
library(caret)
library(factoextra)
library(fpc)
library(NbClust)
library(mclust)


# Load data ----
## Feature embeddings ----

load("Data/UMAP_feats.Rdata")
data_feats <- data_UMAP
rm(data_UMAP)

# wrangle:
data_feats <- data_feats %>%
  mutate(Reef = sub("^(.*?)_.*", "\\1", minute)) %>% 
  select(Reef, everything())

# rename reefs:
data_feats <- data_feats %>%
  mutate(Reef = case_when(
    Reef %in% c("firstreef") ~ "First Reef",
    Reef %in% c("lubaluba") ~ "Lubaluba",
    Reef %in% c("lui") ~ "Lui",
    Reef %in% c("tuare") ~ "Tuare",
    Reef %in% c("inglis") ~ "Inglis",
    Reef %in% c("ema") ~ "Emma",
    Reef %in% c("joelles") ~ "Joelles",
    Reef %in% c("kimbe") ~ "Kimbe Island",
    Reef %in% c("garove") ~ "Garove",
    Reef %in% c("narega1") ~ "Narega West",
    Reef %in% c("narega2") ~ "Narega East",
    TRUE ~ Reef  
  ))

# add habitat column:
data_feats <- data_feats %>%
  mutate(Habitat = case_when(
    Reef %in% c("First Reef", "Lubaluba", "Lui") ~ "Inshore",
    Reef %in% c("Inglis", "Emma", "Joelles") ~ "Pinnacle",
    Reef %in% c("Tuare", "Kimbe Island") ~ "Island",
    Reef %in% c("Garove", "Narega West", "Narega East") ~ "Offshore Island",
    TRUE ~ "unknown" 
  )) %>% 
  select(Habitat, Reef, everything())

# Get just UMAP variables for clustering
data_feat_num <- data_feats %>% 
  select(UMAP_1, UMAP_2)

## Indices ----
data_indices <- read_csv("Data/UMAP_indices.csv")

# rename cols:
data_indices <- data_indices %>% 
  select(-c(minute)) %>% 
  rename('Reef'= habitat)

data_indices <- data_indices %>%
  mutate(Reef = case_when(
    Reef %in% c("firstreef") ~ "First Reef",
    Reef %in% c("lubaluba") ~ "Lubaluba",
    Reef %in% c("lui") ~ "Lui",
    Reef %in% c("tuare") ~ "Tuare",
    Reef %in% c("inglis") ~ "Inglis",
    Reef %in% c("ema") ~ "Emma",
    Reef %in% c("joelles") ~ "Joelles",
    Reef %in% c("kimbe") ~ "Kimbe Island",
    Reef %in% c("garove") ~ "Garove",
    Reef %in% c("narega1") ~ "Narega West",
    Reef %in% c("narega2") ~ "Narega East",
    TRUE ~ Reef  
  ))

data_indices <- data_indices %>%
  mutate(Habitat = case_when(
    Reef %in% c("First Reef", "Lubaluba", "Lui") ~ "Inshore",
    Reef %in% c("Inglis", "Emma", "Joelles") ~ "Pinnacle",
    Reef %in% c("Tuare", "Kimbe Island") ~ "Island",
    Reef %in% c("Garove", "Narega West", "Narega East") ~ "Offshore Island",
    TRUE ~ "unknown" 
  )) %>% 
  select(Habitat, Reef, everything())

# Get just UMAP variables for clustering
data_ind_num <- data_indices %>% 
  select(UMAP_1, UMAP_2)

# Write function ----
# Function to perform k_means, perform chi square, and report ARI:
evaluate_kmeans <- function(data, k, labels){
  set.seed(123) # set the seed
  kmeans.re <- kmeans(data, centers = k, nstart = 25, iter.max = 100) # cluster
  cm <- table(labels, kmeans.re$cluster) # Confusion Matrix 
  print(chisq.test(x=cm, y=NULL))
  print("Adjusted Rand:")
  print(adjustedRandIndex(kmeans.re$cluster, labels))
  return(cm)
}

# Cluster Analysis ----
cm_feats_reef <- evaluate_kmeans(data_feat_num, 11, data_feats$Reef)
cm_feats_hab <- evaluate_kmeans(data_feat_num, 4, data_feats$Habitat)
cm_ind_reef <- evaluate_kmeans(data_ind_num, 11, data_indices$Reef)
cm_ind_hab <- evaluate_kmeans(data_ind_num, 4, data_indices$Habitat)

cm_feats_reef