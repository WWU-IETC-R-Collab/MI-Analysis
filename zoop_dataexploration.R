
library(tidyverse)
library(sf)
library(here)
library(readxl)
library(ggplot2)

install.packages("vegan")
library(vegan)

source("correlation.R")

zoop <- read.csv("data/zoopsynth_with_RR.csv")

tibble(zoop)

# filter out records outside of risk regions

zoop <- zoop %>%
  filter(!is.na(Subregion))

tibble(zoop)

zoop.emp <- zoop %>%
  filter(X == "EMP")

tibble(zoop.emp)

unique(zoop.emp$Taxname)
unique(zoop.emp$Genus)

table(zoop.emp$Genus)
table(zoop.emp$Taxname)
table(zoop.emp$Phylum)

barplot(zoop.emp)

zoop.cor <- zoop.emp %>%
  select(Secchi, SizeClass, Chl, Temperature, DO)

correlation.matrix(zoop.cor)

plot(zoop.cor)
