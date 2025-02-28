library(nlme)

# Concept ----
set.seed(42)
n <- 100
time <- 1:n  # Time index

# Create response (Y) and explanatory (X) variables
Y <- data.frame(Y1 = rnorm(n), Y2 = rnorm(n), Y3 = rnorm(n))  # Convert matrix to dataframe
X <- data.frame(X1 = rnorm(n), X2 = rnorm(n))

# Merge into a single dataframe
df <- cbind(Y, X, time)

# Fit GLS model with AR(1) correlation structure for multiple response variables
Y_hat_all <- sapply(names(Y), function(y) {
  formula <- as.formula(paste(y, "~ X1 + X2"))
  model <- gls(formula, data = df, correlation = corAR1(form = ~ time))
  fitted(model)
})

# Convert to a dataframe
Y_hat_all <- as.data.frame(Y_hat_all)

# Perform PCA on the fitted values of Y
pca_Yhat <- prcomp(Y_hat_all, scale. = TRUE)

# Extract RDA axes
rda_scores <- pca_Yhat$x

library(ggplot2)

df_rda <- data.frame(RDA1 = rda_scores[,1], RDA2 = rda_scores[,2], time = df$time)

ggplot(df_rda, aes(x = RDA1, y = RDA2, color = time)) +
  geom_point(size = 3) +
  scale_color_viridis_c() + 
  labs(title = "RDA Adjusted for Temporal Autocorrelation") +
  theme_minimal()

# Load Kimbe Data and wrangle ----
# Load packages and data ----
library(tidyverse)
library(vegan)
library(corrplot)
library(car)
library(mvabund)
library(scales)
library(ggvegan)
library(ggrepel)
library(ggExtra)
library(ggfortify)
library(ggplot2)
library(GGally)
library(glmmTMB)
library(gllvm)
library(ape)
library(readxl)
library(patchwork)
library(ggpubr)
library(packfor)
library(gridExtra)
library(LaCroixColoR)

# Load data
# Load the DL features that were calulated in Python ('Kimbe_features.csv'):
# For final submission:
# data_feats <- read_csv("C:/Reef soundscapes with AI/Results/PCNN_features/Kimbe_features.csv") 
# For working:
data_feats <- read_csv("C:/Users/fr15610/OneDrive - University of Bristol/Desktop/soundscape_AI/Reef soundscapes with AI/Results/PCNN_features/data/compound_index.csv")
data_feats <- features

## Wrangle feats
DL_feat <- 
  data_feats %>% rename(feat = `...1`) %>% 
  pivot_longer(cols = all_of(colnames(data_feats)[-1]), names_to = "Filename", values_to = "val") %>% 
  # separate_wider_delim(Filename, delim = "T", names = c("Filename", "x"), cols_remove = TRUE) %>% # split to delete extra timestamp info 
  # might need in final code but not for .RData object ^^
  #select(-x) %>% 
  pivot_wider(names_from = feat, values_from = val) 

# Format Filename to match indices:
DL_feat$Filename <- gsub("2304", "", DL_feat$Filename) # remove the year
DL_feat$Filename <- gsub("_(\\d{2})(\\d{2})\\d{2}$", "_\\1_\\2", DL_feat$Filename) # format as 'MM_hh_mm'

#Check for NAs:
DL_feat <- na.omit(DL_feat)

## Wrangle indices data_inds 
# Load the indices from the output folder ('Kimbe_indices.csv')
# data_inds  <- read_csv("C:/Reef soundscapes with AI/Results/PCNN_features/Kimbe_indices.csv") #for final code
data_inds <- data
rm(data)

data_inds <- data_inds %>% 
  rename('Filename' = minute) %>% # rename column
  select(-1) # remove row number col

data_inds <- data_inds %>%
  mutate(reef = sub("^(.*?)_.*", "\\1", Filename)) %>% 
  select(-c(fish_Hf, shrimp_Hf)) %>% # remove Hf as correlated w/ H
  select(Filename, reef, everything())

# remove st dev cols:
cols_to_remove <- grep("_std", names(data_inds)) 
data_inds <- data_inds[-cols_to_remove]

# remove full spectrum cols:
cols_to_remove <- grep("full_", names(data_inds))
data_inds <- data_inds[-cols_to_remove]

# Rename cols to be less suggestive:
data_inds <- data_inds %>% # 'fish' becomes 'low_freq'
  rename_with(~ gsub("^fish", "low_freq", .x), starts_with("fish")) 
data_inds <- data_inds %>% # 'shrimp' becomes 'high_freq'
  rename_with(~ gsub("^shrimp", "high_freq", .x), starts_with("shrimp"))

# Reformat Filename col:
data_inds$Filename <- gsub("\\.WAV$", "", data_inds$Filename, ignore.case = TRUE) # remove the '.WAV' suffix
data_inds$Filename <- gsub("2304", "", data_inds$Filename) # remove the year
data_inds$Filename <- gsub("_(\\d{2})(\\d{2})\\d{2}$", "_\\1_\\2", data_inds$Filename) # format as 'MM_hh_mm'

# Check for NAs:
data_inds <- na.omit(data_inds)

# Split time variable:
data_inds <- data_inds %>%
  extract(Filename, into = c("date", "hour", "minute"), 
          regex = ".*_(\\d{2})_(\\d{2})_(\\d{2})", remove = FALSE) %>%
  mutate(datetime = paste(date, hour, minute, sep = "_"),
         date = as.integer(date), 
         hour = as.integer(hour),
         minute = as.integer(minute)) 

# create a 'diurnal'col
data_inds <- data_inds %>%
  mutate(hour = as.integer(hour), # Ensure 'hour' is an integer for comparison
         diurnal = ifelse(hour >= 6 & hour < 18, "day", "night"))

# Rename the reefs so legend looks good:
data_inds <- data_inds %>%
  rename("Reef" = reef) %>% 
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
    TRUE ~ Reef  # In case there are any other values not covered above
  ))

# Add habitat col:
data_inds <- data_inds %>%
  mutate(Habitat = case_when(
    Reef %in% c("First Reef", "Lubaluba", "Lui") ~ "Inshore",
    Reef %in% c("Inglis", "Emma", "Joelles") ~ "Pinnacle",
    Reef %in% c("Tuare", "Kimbe Island") ~ "Island",
    Reef %in% c("Garove", "Narega West", "Narega East") ~ "Offshore Island",
    TRUE ~ "unknown" 
  ))

# Order and remove redundant cols:
data_inds <- data_inds %>% 
  select(-c(hour, minute)) %>% 
  select(Filename, Reef, Habitat, datetime, date, diurnal, everything()) 

# Prepare data for RDA
# get metadata, left-join ensures its the same order
metadata <- DL_feat %>% select(Filename) %>% 
  left_join(data_inds, by = "Filename")

# Check for NAs
metadata <- na.omit(metadata)

DL_feat_filtered <- inner_join(DL_feat, data_inds, by="Filename", keep = FALSE)
DL_feat_filtered <- DL_feat_filtered %>% 
  select(names(DL_feat))
DL_feat <- DL_feat_filtered

# Make sure they match:
identical(metadata[['Filename']], DL_feat[['Filename']])# check they're the same
rm(data_inds);rm(data_feats); rm(DL_feat_filtered); rm(features)


# Modelling Temporal Autocorrelation ----
# Y = feats
# X =  indices

Y <- DL_feat %>% 
  select(Filename, `1`, `2`) %>% # take 2 variables
  slice_head(n = 200) # take only First Reef
Y$time <- 1:nrow(Y) # add an index to function as a 'time' variable

X <- metadata %>% 
  select(Filename, low_freq_ACI, high_freq_ACI, low_freq_H) %>% 
  slice_head(n = 200) # take only First Reef
X$time <- 1:nrow(X) # add an index to function as a 'time' variable

identical(X$Filename, Y$Filename)

# 200 recs ----
acf(Y[,3], lag.max = 1000) # lag is around 24 steps or 1hr12
pacf(Y[,3]) # Around 24 but not earlier!

acf(X$low_freq_ACI, lag.max = 1000) 
pacf(X$low_freq_ACI)
acf(X$high_freq_ACI, lag.max = 1000)
pacf(X$high_freq_ACI)
acf(X$low_freq_H, lag.max = 1000)
pacf(X$low_freq_H) # different ones at each

# rename Y vars
Y <- Y %>% rename(feat_1 = `1`) %>% 
  rename(feat_2 = `2`) %>% 
  select(Filename, time, feat_1, feat_2)

df <- cbind(Y, X)

## make a model ----
# What order sorts TA out?
# first no temporal term
mod <- gls(feat_1 ~ low_freq_ACI + high_freq_ACI + low_freq_H, 
            data = df)
acf(residuals(mod))

mod1 <- gls(feat_1 ~ low_freq_ACI + high_freq_ACI + low_freq_H, 
            correlation = corARMA(form = ~ time, p = 1), 
            data = df)
acf(residuals(mod1)) # nearly OK!

mod2 <- gls(feat_1 ~ low_freq_ACI + high_freq_ACI + low_freq_H, 
            correlation = corARMA(form = ~ time, p = 2), 
            data = df)
acf(residuals(mod2))

mod3 <- gls(feat_1 ~ low_freq_ACI + high_freq_ACI + low_freq_H, 
            correlation = corARMA(form = ~ time, p = 3), 
            data = df)

acf(residuals(mod3))

mod5 <- gls(feat_1 ~ low_freq_ACI + high_freq_ACI + low_freq_H, 
            correlation = corARMA(form = ~ time, p = 5), 
            data = df)
acf(residuals(mod5))


mod10 <- gls(feat_1 ~ low_freq_ACI + high_freq_ACI + low_freq_H, 
            correlation = corARMA(form = ~ time, p = 10), 
            data = df)
acf(residuals(mod10)) # Still bad!

mod24 <- gls(feat_1 ~ low_freq_ACI + high_freq_ACI + low_freq_H, 
             correlation = corARMA(form = ~ time, p = 24), 
             data = df)
acf(residuals(mod24))

# does increasing p improve the model?
AIC(mod1, mod10, mod24) # Makes model slightly worse

# no clear effect on residuals
acf(residuals(mod1))
acf(residuals(mod24))

## Moving average ----
mod_arma11 <- gls(feat_1 ~ low_freq_ACI + high_freq_ACI + low_freq_H, 
                  correlation = corARMA(form = ~ time, p = 1, q = 1), 
                  data = df)
acf(residuals(mod_arma11))
AIC(mod1, mod10, mod24, mod_arma11)

# 480 recs (1 day) ----
Y <- DL_feat %>% 
  select(Filename, `1`, `2`) %>% # take 2 variables
  slice_head(n = 480) # take only First Reef
Y$time <- 1:nrow(Y) # add an index to function as a 'time' variable

X <- metadata %>% 
  select(Filename, low_freq_ACI, high_freq_ACI, low_freq_H) %>% 
  slice_head(n = 480) # take only First Reef
X$time <- 1:nrow(X) # add an index to function as a 'time' variable

identical(X$Filename, Y$Filename)

acf(Y[,2], lag.max = 1000) # lag is diurnal
pacf(Y[,2], lag.max = 24) # Around 24 and <5

acf(X$low_freq_ACI, lag.max = 1000) 
pacf(X$low_freq_ACI)
acf(X$high_freq_ACI, lag.max = 1000)
pacf(X$high_freq_ACI)
acf(X$low_freq_H, lag.max = 1000)
pacf(X$low_freq_H) # different ones at each

# rename Y vars
Y <- Y %>% rename(feat_1 = `1`) %>% 
  rename(feat_2 = `2`) %>% 
  select(Filename, time, feat_1, feat_2)

df <- cbind(Y, X)

## make a model ----
# What order sorts TA out?
# first no temporal term
mod <- gls(feat_1 ~ low_freq_ACI + high_freq_ACI + low_freq_H, 
           data = df)
acf(residuals(mod))

mod1 <- gls(feat_1 ~ low_freq_ACI + high_freq_ACI + low_freq_H, 
            correlation = corARMA(form = ~ time, p = 1), 
            data = df)
acf(residuals(mod1)) # nearly OK!

mod2 <- gls(feat_1 ~ low_freq_ACI + high_freq_ACI + low_freq_H, 
            correlation = corARMA(form = ~ time, p = 2), 
            data = df)
acf(residuals(mod2))

mod3 <- gls(feat_1 ~ low_freq_ACI + high_freq_ACI + low_freq_H, 
            correlation = corARMA(form = ~ time, p = 3), 
            data = df)

acf(residuals(mod3))

mod5 <- gls(feat_1 ~ low_freq_ACI + high_freq_ACI + low_freq_H, 
            correlation = corARMA(form = ~ time, p = 5), 
            data = df)
acf(residuals(mod5))

mod10 <- gls(feat_1 ~ low_freq_ACI + high_freq_ACI + low_freq_H, 
             correlation = corARMA(form = ~ time, p = 10), 
             data = df)
acf(residuals(mod10)) # Still bad!

mod24 <- gls(feat_1 ~ low_freq_ACI + high_freq_ACI + low_freq_H, 
             correlation = corARMA(form = ~ time, p = 24), 
             data = df)
acf(residuals(mod24))

# does increasing p improve the model?
AIC(mod1, mod10, mod24) # Makes model slightly worse

# no clear effect on residuals
acf(residuals(mod1))
acf(residuals(mod24))

## Moving average ----
mod_arma11 <- gls(feat_1 ~ low_freq_ACI + high_freq_ACI + low_freq_H, 
                  correlation = corARMA(form = ~ time, p = 1, q = 1), 
                  data = df)
acf(residuals(mod_arma11))
AIC(mod1, mod10, mod24, mod_arma11)

# every 30 mins ----
Y <- DL_feat %>% 
  select(Filename, `1`, `2`) %>% # take 2 variables
  slice_head(n = 3000) %>%  # take only First Reef
  filter(row_number() %% 10 == 1) # subset
Y$time <- 1:nrow(Y) # add an index to function as a 'time' variable

X <- metadata %>% 
  select(Filename, low_freq_ACI, high_freq_ACI, low_freq_H) %>% 
  slice_head(n = 3000) %>%  # take only First Reef
  filter(row_number() %% 10 == 1)
X$time <- 1:nrow(X) # add an index to function as a 'time' variable

identical(X$Filename, Y$Filename)

acf(Y[,2], lag.max = 200) # lag is around 1.5 day period but depends on the feature!
pacf(Y[,3], lag.max = 100) # Around 24 and <5

acf(X$low_freq_ACI, lag.max = 1000) 
pacf(X$low_freq_ACI)
acf(X$high_freq_ACI, lag.max = 1000)
pacf(X$high_freq_ACI)
acf(X$low_freq_H, lag.max = 1000)
pacf(X$low_freq_H) # different ones at each

# rename Y vars
Y <- Y %>% rename(feat_1 = `1`) %>% 
  rename(feat_2 = `2`) %>% 
  select(Filename, time, feat_1, feat_2)

df <- cbind(Y, X)

## make a model ----
# What order sorts TA out?
# first no temporal term
mod <- gls(feat_1 ~ low_freq_ACI + high_freq_ACI + low_freq_H, 
           data = df)
acf(residuals(mod))

mod1 <- gls(feat_1 ~ low_freq_ACI + high_freq_ACI + low_freq_H, 
            correlation = corARMA(form = ~ time, p = 1), 
            data = df)
acf(residuals(mod1)) # nearly OK!

mod2 <- gls(feat_1 ~ low_freq_ACI + high_freq_ACI + low_freq_H, 
            correlation = corARMA(form = ~ time, p = 2), 
            data = df)
acf(residuals(mod2))

mod3 <- gls(feat_1 ~ low_freq_ACI + high_freq_ACI + low_freq_H, 
            correlation = corARMA(form = ~ time, p = 3), 
            data = df)

acf(residuals(mod3))

mod5 <- gls(feat_1 ~ low_freq_ACI + high_freq_ACI + low_freq_H, 
            correlation = corARMA(form = ~ time, p = 5), 
            data = df)
acf(residuals(mod5))

mod10 <- gls(feat_1 ~ low_freq_ACI + high_freq_ACI + low_freq_H, 
             correlation = corARMA(form = ~ time, p = 10), 
             data = df)
acf(residuals(mod10)) # Still bad!

mod24 <- gls(feat_1 ~ low_freq_ACI + high_freq_ACI + low_freq_H, 
             correlation = corARMA(form = ~ time, p = 24), 
             data = df)
acf(residuals(mod24))

# does increasing p improve the model?
AIC(mod1, mod10, mod24) # Makes model slightly worse

# no clear effect on residuals
acf(residuals(mod1))
acf(residuals(mod24))

## Moving average ----
mod_arma11 <- gls(feat_1 ~ low_freq_ACI + high_freq_ACI + low_freq_H, 
                  correlation = corARMA(form = ~ time, p = 1, q = 1), 
                  data = df)
acf(residuals(mod_arma11))
AIC(mod1, mod10, mod24, mod_arma11)
