# Preparing Indices for UMAP in Python

library(tidyverse)

# Load the indices from the output folder ('Kimbe_indices.csv')
data <- read.csv("C:/Reef soundscapes with AI/Results/PCNN_features/Kimbe_indices.csv")

# wrangle
data <- data %>% 
  select(-1)

data <- data %>%
  mutate(habitat = sub("^(.*?)_.*", "\\1", minute)) %>% 
  select(minute, habitat, everything())

cols_to_remove <- grep("_std", names(data))
data <- data[-cols_to_remove]

data <- data %>% # 'fish' becomes 'low_freq'
  rename_with(~ gsub("^fish", "low_freq", .x), starts_with("fish")) 
data <- data %>% # 'shrimp' becomes 'high_freq'
  rename_with(~ gsub("^shrimp", "high_freq", .x), starts_with("shrimp"))

data$minute <- gsub("\\.WAV$", "", data$minute, ignore.case = TRUE) # remove the '.WAV' suffix
data$minute <- gsub("2304", "", data$minute) # remove the year
data$minute <- gsub("_(\\d{2})(\\d{2})\\d{2}$", "_\\1_\\2", data$minute) # format as 'MM_hh_mm'

# save as a csv file
write_csv(data, "data/indices_for_UMAP.csv")

