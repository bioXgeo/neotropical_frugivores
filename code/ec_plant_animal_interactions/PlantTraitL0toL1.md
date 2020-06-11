---
title: 'Frugivoria: Ecuadorian Birds, Mammals, and Plants'
author: "Beth Gerstner & Hazel Anderson"
date: "6/1/2020"
output:
  pdf_document: default
  html_document: default
---
# Load packages
```{r}
library(BIEN)
library(ape)
library(maps)
library(sp)
```
# Pull Ecuador plant species list from BIEN
```{r}
Ecuador <- BIEN_occurrence_country("Ecuador")
```

# Trait lists
```{r}
fruit_type <- BIEN_trait_trait(trait = "fruit type")
minimum_fruit_length <- BIEN_trait_trait(trait = "minimum fruit length")
maximum_fruit_lenth <- BIEN_trait_trait(trait = "maximum fruit length")
plant_fruiting_duration <- BIEN_trait_trait(trait = "plant fruiting duration")