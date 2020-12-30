# Eric Lawrence
# 12/6/2020
# Data manipulation for EMP dataset


library(tidyverse)
library(sf)
library(here)
library(readxl)

USFE.riskregions <- here("data/RiskRegions_DWSC_Update_9292020.shp") %>% # path relative to main directory
  st_read()

st_crs(USFE.riskregions)

crs.WGS84 <- st_crs(USFE.riskregions)

######## Load in tabular data

emp <- read_excel("data/emp_edited_for_R.xlsx")
tibble(emp)

emp.tax <- emp %>%
  select(1:6)
tibble(emp.tax)

emp.count <- emp %>%
  select(7:416)
tibble(emp.count)

emp.site <- emp %>%
  select(417:420)

tibble(emp.site)
length(emp.count$"1")

counts <- c(1,2)

for (i in 1:410) {
  string <- as.character(get(i))
  counts <- c(counts, emp.count$string)
}

apply(emp.count, 2, table)

emp.f <- data.frame(Date = )

emp.count <- emp.count %>%
  spread("1","2","3")

names <- names(emp.count)
