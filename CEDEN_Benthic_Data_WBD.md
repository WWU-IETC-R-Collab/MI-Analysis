---
title: "CEDEN Benthic Data"
author: "Eric Lawrence"
date: "12/13/2020"
output:
  html_document:
    code_folding: show
    code_download: true
    keep_md: true
---



**This document is almost identical to Eric's CEDEN_Benthic_Data.rmd, but has been modified to bring in HUC12 watershed boundaries as a shapefile, and join them to the MI and WQ datasets.**

## CEDEN Data 

This document covers the data analysis for the benthic macroinvertebrate dataset obtained from the CEDEN Advanced Query Tool: 

    https://ceden.waterboards.ca.gov/AdvancedQueryTool

I downloaded the CEDEN Benthic data with the following parameters:

   Region selection by County: Contra Costa, Sacramento, San Joaquin, Solano, and Yolo.

   Date Range: 1/1/2010 to 6/19/2019 (latest available date).

I also used the CEDEN water quality data from the Tox Box accessed on 12/13/2020:

   Z:\\Upper San Francisco Project\\Data & Analyses\\Year 2\\CEDEN Files by Topic\\CEDEN Water WQ Data.xlsx

## Risk Regions
   
To assign risk regions to the data with spatial coordinates, I used the RiskRegions_DWSC_Update_9292020.shp shapefile obtained from the Tox Box.


## Load in libraries and data


```r
library(tidyverse)
library(sf)
library(here)
library(readxl)
library(nngeo)
library(data.table)

# Load risk regions shapefile
USFE.riskregions <- here("data/RiskRegions_DWSC_Update_9292020.shp") %>%
  st_read()

# Create WGS84 projection variable
crs.WGS84 <- st_crs(USFE.riskregions)

#Load in CEDEN data
CEDEN <- read_excel("data/ceden_data_edited.xlsx") %>%
  rename(Date = SampleDate) 

# Load in CA Watershed boundaries HUC12 - sourced from https://data.ca.gov/dataset/ca-usgs-watershed-boundary-dataset-12-digit-hydrologic-units
HUC12 <- here("USGS_WBD/") %>%
  st_read() %>% # CRS = WGS 84 pseudomercator
  st_transform(., crs.WGS84) %>%
  st_intersection(.,USFE.riskregions)
```

## Set CEDEN Projection to WGS84

The CEDEN data has data with different coordinate systems depending on the source project. I separated the projects by coordinate systems into separate dataframes, converted the dataframes to WGS84 seperately, then recombined them. There were some data that had no known coordinate system so those were excluded from the analysis.

EPSG numbers for different coordinate systems can be found at https://spatialreference.org/ref/epsg/


```r
### Separate by Datum
unique(CEDEN$Datum)

ceden.wgs84 <- filter(CEDEN, Datum == "WGS84")
ceden.nad83 <- filter(CEDEN, Datum == "NAD83")
ceden.nr <- filter(CEDEN, Datum == "NR")
ceden.NAD27 <- filter(CEDEN, Datum == "NAD27")

### set Datum, shapefiles

ceden.wgs84.sf <- ceden.wgs84 %>%
  st_as_sf(coords=c("TargetLongitude", "TargetLatitude"), crs = crs.WGS84)

ceden.nad83.sf <- ceden.nad83 %>%
  st_as_sf(coords=c("TargetLongitude", "TargetLatitude"), crs = 4269)

ceden.NAD27.sf <- ceden.NAD27 %>%
  st_as_sf(coords=c("TargetLongitude", "TargetLatitude"), crs = 4267)

# ceden.nr does not have a known coordinate system

### Transform into WGS84

ceden.nad83.t.wgs84.sf <- st_transform(ceden.nad83.sf, crs.WGS84)
st_crs(ceden.nad83.t.wgs84.sf)

ceden.NAD27.t.wgs84.sf <- st_transform(ceden.NAD27.sf, crs.WGS84)
st_crs(ceden.NAD27.t.wgs84.sf)

### Join Excel Data to Risk Regions
ceden.wgs84.sf <- st_join(ceden.wgs84.sf, USFE.riskregions["Subregion"])
ceden.nad83.t.wgs84.sf <- st_join(ceden.nad83.t.wgs84.sf, USFE.riskregions["Subregion"])
ceden.NAD27.t.wgs84.sf <- st_join(ceden.NAD27.t.wgs84.sf, USFE.riskregions["Subregion"])

### Combine datasets
ceden.all.sf <- bind_rows(ceden.wgs84.sf, ceden.nad83.t.wgs84.sf)
ceden.all.sf <- bind_rows(ceden.all.sf, ceden.NAD27.t.wgs84.sf)

### Remove records outside of Risk Regions

ceden.all.sf <- ceden.all.sf %>%
  filter(!is.na(Subregion))
```

## Plot Ceden Data and Write csv


```r
# Basic plot
ggplot() +
  geom_sf(data = HUC12, color = "green") +
  geom_sf(data = USFE.riskregions, fill = NA) +
  geom_sf(data = ceden.all.sf, aes(color = Subregion)) +
  scale_color_brewer(palette = "Set1") + # not color-blind safe
  ggtitle("Ceden Benthic Data")
```

![](CEDEN_Benthic_Data_WBD_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
# Write table

# write.csv(ceden.all.sf, "data/ceden_with_RR.csv") # Already printed to csv # This is essentially what is output from CEDENMod
```

# Analysis

## Organize Benthic Data

I built a dataframe that grouped the data by station code and sample date so that each record is a sampling event. I summarized the number of taxa and counts by selected Orders and all Phylums. I also calculated the EPT and ETO indicies and taxa counts.

I selected the orders most commonly used in community indicies.

Orders: 

* Ephemeroptera (n_E)
* Plecoptera (n_P)
* Trichoptera (n_T)
* Odonata (n_O)
* Diptera (n_D)


```r
##### Create dataframe set into groups by station and sample date to isolate individual samples.
##### summarise by taxa presence, number of taxa present, and indexes

samp.df <- ceden.all.sf %>%
  group_by(StationCode, Date) %>%
  summarise(Subregion = first(Subregion),
            StationName = first(StationName),
            Project = first(Project),
            Projectcode = first(Projectcode),
            n_taxa = n(),
            n_E = sum(Orders == "Ephemeroptera", na.rm = TRUE),
            n_P = sum(Orders == "Plecoptera", na.rm = TRUE),
            n_T = sum(Orders == "Trichoptera", na.rm = TRUE),
            n_O = sum(Orders == "Odonata", na.rm = TRUE),
            n_D = sum(Orders == "Diptera", na.rm = TRUE),
            n_Phylum_NA = sum(is.na(Phylum)),
            n_Arthropoda = sum(Phylum == "Arthropoda", na.rm = TRUE),
            n_Annelida = sum(Phylum == "Annelida", na.rm = TRUE),
            n_Nematoda = sum(Phylum == "Nematoda", na.rm = TRUE),
            n_Ectoprocta = sum(Phylum == "Ectoprocta", na.rm = TRUE),
            n_Bacillariophyta = sum(Phylum == "Bacillariophyta", na.rm = TRUE),
            n_Cryptophyta = sum(Phylum == "Cryptophyta", na.rm = TRUE),
            n_Heterokontophyta = sum(Phylum == "Heterokontophyta", na.rm = TRUE),
            n_Ochrophyta = sum(Phylum == "Ochrophyta", na.rm = TRUE),
            n_Coelenterata = sum(Phylum == "Coelenterata", na.rm = TRUE),
            n_Nemertea = sum(Phylum == "Nemertea", na.rm = TRUE),
            n_Mollusca = sum(Phylum == "Mollusca", na.rm = TRUE),
            n_Platyhelminthes = sum(Phylum == "Platyhelminthes", na.rm = TRUE),
            n_Bryozoa = sum(Phylum == "Bryozoa", na.rm = TRUE),
            n_Cyanobacteria = sum(Phylum == "Cyanobacteria", na.rm = TRUE),
            n_Chlorophyta = sum(Phylum == "Chlorophyta", na.rm = TRUE),
            n_Euglenozoa = sum(Phylum == "Euglenozoa", na.rm = TRUE),
            n_Streptophyta = sum(Phylum == "Streptophyta", na.rm = TRUE),
            n_Rhodophyta = sum(Phylum == "Rhodophyta", na.rm = TRUE),
            n_Chordata = sum(Phylum == "Chordata", na.rm = TRUE)
            ) %>%
  mutate(EPT_taxa = sum(n_E, n_P, n_T)) %>%  ### Calculate indexes using mutate
  mutate(EPT_index = EPT_taxa / n_taxa) %>%
  mutate(ETO_taxa = sum(n_E, n_O, n_T)) %>%
  mutate(ETO_index = ETO_taxa / n_taxa)

## Join WBD to MI data
samp.df <- st_join(samp.df, HUC12["HUC12"])
```


```r
tibble(samp.df) # 160 obsv
```

```
## # A tibble: 160 x 38
##    StationCode Date                Subregion StationName Project Projectcode
##    <chr>       <dttm>              <chr>     <chr>       <chr>   <chr>      
##  1 510CR0036   2018-06-18 00:00:00 Sacramen~ Sacramento~ Nation~ EPA_NRSA_2~
##  2 510CR0322   2018-06-20 00:00:00 Sacramen~ Miner Slou~ Nation~ EPA_NRSA_2~
##  3 510CR1007   2013-06-03 00:00:00 Sacramen~ Sacramento~ Nation~ EPA_NRSA_2~
##  4 510CS01xx   2012-03-27 00:00:00 North De~ Cache Slou~ Univer~ UMD        
##  5 510CS01xx   2012-10-12 00:00:00 North De~ Cache Slou~ Univer~ UMD        
##  6 510CS01xx   2013-03-26 00:00:00 North De~ Cache Slou~ Univer~ UMD        
##  7 510CS01xx   2013-10-07 00:00:00 North De~ Cache Slou~ Univer~ UMD        
##  8 510CS01xx   2014-03-28 00:00:00 North De~ Cache Slou~ Univer~ UMD        
##  9 510CS01xx   2014-10-04 00:00:00 North De~ Cache Slou~ Univer~ UMD        
## 10 510CS02xx   2012-03-27 00:00:00 North De~ Cache Slou~ Univer~ UMD        
## # ... with 150 more rows, and 32 more variables: n_taxa <int>, n_E <int>,
## #   n_P <int>, n_T <int>, n_O <int>, n_D <int>, n_Phylum_NA <int>,
## #   n_Arthropoda <int>, n_Annelida <int>, n_Nematoda <int>, n_Ectoprocta <int>,
## #   n_Bacillariophyta <int>, n_Cryptophyta <int>, n_Heterokontophyta <int>,
## #   n_Ochrophyta <int>, n_Coelenterata <int>, n_Nemertea <int>,
## #   n_Mollusca <int>, n_Platyhelminthes <int>, n_Bryozoa <int>,
## #   n_Cyanobacteria <int>, n_Chlorophyta <int>, n_Euglenozoa <int>,
## #   n_Streptophyta <int>, n_Rhodophyta <int>, n_Chordata <int>, geometry <POINT
## #   [°]>, EPT_taxa <int>, EPT_index <dbl>, ETO_taxa <int>, ETO_index <dbl>,
## #   HUC12 <chr>
```


## Organize Water Quality Data

Updated to use the modified WQ data (already with RR joined and filtered)

*Note: "Date" used in this dataset in lieu of SampleDate*

```r
# Bring in WQ data
ceden.wq <- fread("https://github.com/WWU-IETC-R-Collab/CEDEN-mod/raw/main/Data/Output/CEDENMod_WQ.csv")

# Convert to sf Object: needs x, y, CRS
ceden.wq <- ceden.wq %>%
  st_as_sf(coords=c("Longitude", "Latitude"), 
           crs = "NAD83", remove = F) %>%
  st_transform(., crs = crs.WGS84) # transform to projection other df are currently in

# CHECK GEOM
# str(ceden.wq$geometry) # sfc_POINT - good
```

I created a dataframe that grouped by station name and sample date where each record is a water quality sampling event. This way the water quality sampling events can be matched up the the MI sampling events by location and timing.

This dataframe summarized water quality parameters by number of samples during that day, mean, standard deviation, minimum value, and maximum value. Though most of the parameters only had one measurement for the day.

The parameters included were:
* Alkalinity as CaCO3, Total
* Ammonia as N, Total
* Chlorine, Free, Total
* Chlorine, Total Residue, Total
* Oxygen, Dissolved, Total
* pH
* Salinity, Total
* Secchi Depth
* Specific Conductivity, Total
* Temperature
* Turbidity, Total
* Velocity


```r
# While ceden.wq.sf is a sfc_POINT, after this process, wq.stations is a sfc_GEOM, which causes trouble with later analyses. Possible to remove sf prior to this process, or run this before make sf?

wq.stations <- ceden.wq %>%
  group_by(StationName, Date) %>%
  summarise(Project = first(Project),
            n = n(),
            Subregion = first(Subregion),
            Latitude = first(Latitude),
            Longitude = first(Longitude),
            n_Alk = sum(Analyte == "Alkalinity as CaCO3, Total", na.rm = TRUE),
            mean_Alk = mean(Result[Analyte == "Alkalinity as CaCO3, Total"], na.rm = TRUE),
            sd_Alk = sd(Result[Analyte == "Alkalinity as CaCO3, Total"], na.rm = TRUE),
            min_Alk = min(Result[Analyte == "Alkalinity as CaCO3, Total"], na.rm = TRUE),
            max_Alk = max(Result[Analyte == "Alkalinity as CaCO3, Total"], na.rm = TRUE),

            n_N = sum(Analyte == "Ammonia as N, Total", na.rm = TRUE),
            mean_N = mean(Result[Analyte == "Ammonia as N, Total"], na.rm = TRUE),
            sd_N = sd(Result[Analyte == "Ammonia as N, Total"], na.rm = TRUE),
            min_N = min(Result[Analyte == "Ammonia as N, Total"], na.rm = TRUE),
            max_N = max(Result[Analyte == "Ammonia as N, Total"], na.rm = TRUE),
            
            n_Chl_F = sum(Analyte == "Chlorine, Free, Total", na.rm = TRUE),
            mean_Chl_F = mean(Result[Analyte == "Chlorine, Free, Total"], na.rm = TRUE),
            sd_Chl_F = sd(Result[Analyte == "Chlorine, Free, Total"], na.rm = TRUE),
            min_Chl_F = min(Result[Analyte == "Chlorine, Free, Total"], na.rm = TRUE),
            max_Chl_F = max(Result[Analyte == "Chlorine, Free, Total"], na.rm = TRUE),
            
            n_Chl_TR = sum(Analyte == "Chlorine, Total Residual, Total", na.rm = TRUE),
            mean_Chl_TR = mean(Result[Analyte == "Chlorine, Total Residual, Total"], na.rm = TRUE),
            sd_Chl_TR = sd(Result[Analyte == "Chlorine, Total Residual, Total"], na.rm = TRUE),
            min_Chl_TR = min(Result[Analyte == "Chlorine, Total Residual, Total"], na.rm = TRUE),
            max_Chl_TR = max(Result[Analyte == "Chlorine, Total Residual, Total"], na.rm = TRUE),
            
            n_DO = sum(Analyte == "Oxygen, Dissolved, Total", 
                       na.rm = TRUE),
            mean_DO = mean(Result[Analyte == "Oxygen, Dissolved, Total"], 
                           na.rm = TRUE),
            sd_DO = sd(Result[Analyte == "Oxygen, Dissolved, Total"], 
                       na.rm = TRUE),
            min_DO = min(Result[Analyte == "Oxygen, Dissolved, Total"], 
                         na.rm = TRUE),
            max_DO = max(Result[Analyte == "Oxygen, Dissolved, Total"], 
                         na.rm = TRUE),

            n_pH = sum(Analyte == "pH", na.rm = TRUE),
            mean_pH = mean(Result[Analyte == "pH"], na.rm = TRUE),
            sd_pH = sd(Result[Analyte == "pH"], na.rm = TRUE),
            min_pH = min(Result[Analyte == "pH"], na.rm = TRUE),
            max_pH = max(Result[Analyte == "pH"], na.rm = TRUE),
            
            n_Sal = sum(Analyte == "Salinity, Total", na.rm = TRUE),
            mean_Sal = mean(Result[Analyte == "Salinity, Total"], na.rm = TRUE),
            sd_Sal = sd(Result[Analyte == "Salinity, Total"], na.rm = TRUE),
            min_Sal = min(Result[Analyte == "Salinity, Total"], na.rm = TRUE),
            max_Sal = max(Result[Analyte == "Salinity, Total"], na.rm = TRUE),
            
            n_Secc = sum(Analyte == "Secchi Depth", na.rm = TRUE),
            mean_Secc = mean(Result[Analyte == "Secchi Depth"], na.rm = TRUE),
            sd_Secc = sd(Result[Analyte == "Secchi Depth"], na.rm = TRUE),
            min_Secc = min(Result[Analyte == "Secchi Depth"], na.rm = TRUE),
            max_Secc = max(Result[Analyte == "Secchi Depth"], na.rm = TRUE),
            
            n_Cond = sum(Analyte  == "SpecificConductivity, Total", na.rm = TRUE),
            mean_Cond = mean(Result[Analyte == "SpecificConductivity, Total"], na.rm = TRUE),
            sd_Cond = sd(Result[Analyte == "SpecificConductivity, Total"], na.rm = TRUE),
            min_Cond = min(Result[Analyte == "SpecificConductivity, Total"], na.rm = TRUE),
            max_Cond = max(Result[Analyte == "SpecificConductivity, Total"], na.rm = TRUE),
            
            n_Temp = sum(Analyte == "Temperature", na.rm = TRUE),
            mean_Temp = mean(Result[Analyte == "Temperature"], na.rm = TRUE),
            sd_Temp = sd(Result[Analyte == "Temperature"], na.rm = TRUE),
            min_Temp = min(Result[Analyte == "Temperature"], na.rm = TRUE),
            max_Temp = max(Result[Analyte == "Temperature"], na.rm = TRUE),

            n_Turb = sum(Analyte == "Turbidity, Total", na.rm = TRUE),
            mean_Turb = mean(Result[Analyte == "Turbidity, Total"], 
                             na.rm = TRUE),
            sd_Turb = sd(Result[Analyte == "Turbidity, Total"], na.rm = TRUE),
            min_Turb = min(Result[Analyte == "Turbidity, Total"], na.rm = TRUE),
            max_Turb = max(Result[Analyte == "Turbidity, Total"], na.rm = TRUE),
            
            n_Vel = sum(Analyte == "Velocity", na.rm = TRUE),
            mean_Vel = mean(Result[Analyte == "Velocity"], na.rm = TRUE),
            sd_Vel = sd(Result[Analyte == "Velocity"], na.rm = TRUE),
            min_Vel = min(Result[Analyte == "Velocity"], na.rm = TRUE),
            max_Vel = max(Result[Analyte == "Velocity"], na.rm = TRUE),
            ) %>%
  rename(Date.wq = Date) # 3165 records

### Change infinities and NaN values to NA
wq.stations <- wq.stations %>% 
  mutate_if(is.numeric, list(~na_if(., Inf))) %>% 
  mutate_if(is.numeric, list(~na_if(., -Inf))) %>%
  mutate_if(is.numeric, list(~na_if(., "NaN"))) %>%
  mutate_if(is.numeric, list(~na_if(., NaN)))

str(wq.stations$geometry) # sfc_POINT
```


```r
## Join WBD to WQ data
wq.stations <- st_join(wq.stations, HUC12["HUC12"]) 
# Formerly 2133 obsv, now 3165 bc using CEDENMod_WQ
```

## Dataframe with one record for station {.tabset}

These dataframes are used for mapping.

### Benthic

```r
# One record per Benthic Sample Station

st.df <- ceden.all.sf %>%
  group_by(StationCode) %>%
  summarise(n(), n_distinct(Date), first(Subregion)) %>%
  rename(Subregion = `first(Subregion)`)
```


```r
# Sample Stations Plot
ggplot() +
  geom_sf(data = USFE.riskregions) +
  geom_sf(data = st.df, aes(color = Subregion)) +
  scale_color_brewer(palette = "Set1") + # not color-blind safe
  ggtitle("Ceden Benthic Sample Stations")
```

![](CEDEN_Benthic_Data_WBD_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

### Water Quality


```r
# One record per Benthic Sample Station

wq.df <- ceden.wq %>%
  group_by(StationCode) %>%
  summarise(n(), n_distinct(Date), first(Subregion)) %>%
  rename(Subregion = `first(Subregion)`)
```


```r
# Sample Stations Plot
ggplot() +
  geom_sf(data = USFE.riskregions) +
  geom_sf(data = wq.df, aes(color = Subregion)) +
  scale_color_brewer(palette = "Set1") + # not color-blind safe
  ggtitle("Ceden WQ Sample Stations")
```

![](CEDEN_Benthic_Data_WBD_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

## Join MI and WQ data

### Transform Projection to UTM Zone 10n

This transform allows us to compare distances and create buffers. I created a 500 meter buffer around each water quality sampling station.


```r
####### Transform into projection to compare distance

# Transform into UTM Zone 10n EPSG:26910
wq.stations <- st_transform(wq.stations, 26910)
wq.df.u10 <- st_transform(wq.df, 26910)

samp.df.u10 <- st_transform(samp.df, 26910)
st.df.u10 <- st_transform(st.df, 26910)

rr.u10 <- st_transform(USFE.riskregions, 26910)

### Create 500m buffer around WQ sampling locations
wq.stations.buffer <- st_buffer(wq.stations, 1000) # buffer is 1000 meters

### Remove any buffers outside of the risk regions ~ I hashtagged-out this portion, because I'm not sure if this would remove the entire buffer circle (including portions within the project boundary) if a segment of it extends beyond. 
## Theoretically, WQ.Stations was already filtered to exclude stations outside of project boundaries (line 248). Going to see if it changes the number of results:
## WITH this second buffer, com.dates = 64 obsv; 
## W/O  this second buffer, com.dates = 1017 obsv

#wq.stations.buffer <- wq.stations.buffer %>%
#filter(!is.na(Subregion))
```

### Original: Join using buffers

#### Plot WQ buffers and ceden benthic data


```r
ggplot() +
  geom_sf(data = USFE.riskregions) +
  geom_sf(data = wq.stations.buffer, aes(color = Subregion)) +
  scale_color_brewer(palette = "Set1") + # not color-blind safe
  geom_sf(data = st.df.u10, cex=0.7) +
  ggtitle("Ceden WQ Buffers and MI Sampling Locations") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
```

![](CEDEN_Benthic_Data_WBD_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

#### Join on Buffers & filter to same date & watershed


```r
### Join Benthic locations and WQ buffers
samp.wq.com <- st_join(samp.df.u10, wq.stations.buffer) #2517 records when buffer = 2k

### Select records that have sampling data from the same date for benthic and WQ
com.dates <- samp.wq.com %>%
  filter(Date == Date.wq) #83 results (when radius = 2k or 3k m), retains multiple matches of WQ per Benthic

com.dates.HUC <- com.dates %>%
  filter(HUC12.x == HUC12.y) #83 results (when radius = 2k or 3k m)
```

To test what is happening if there are multiple matches, I summarized the results by benthic date:location combinations (There was only one record for each combo prior to the join). 

Yes, multiple matches were retained -> reduced to 58 unique records. The results indicates which benthic samples were duplicated to allow multiple (n) WQ site matches.


```r
check <- com.dates.HUC[,1:3] %>%
  group_by(StationCode, Date) %>%
  summarise(n()) # 58 matches
```


```r
### Plot benthic sampling locations that has WQ available on the same date

ggplot() +
  geom_sf(data = USFE.riskregions) +
  geom_sf(data = com.dates, aes(color = Subregion.x)) +
  scale_color_brewer(palette = "Set1") + # not color-blind safe
  ggtitle("Ceden Benthic Data with WQ on same date")
```

![](CEDEN_Benthic_Data_WBD_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

```r
tibble(com.dates.HUC)
```

```
## # A tibble: 102 x 106
##    StationCode Date                Subregion.x StationName.x Project.x
##    <chr>       <dttm>              <chr>       <chr>         <chr>    
##  1 510CR1007   2013-06-03 00:00:00 Sacramento~ Sacramento R~ National~
##  2 510L12105   2012-07-11 00:00:00 Sacramento~ Lake Greenha~ SWAMP Na~
##  3 510STODWx   2014-06-18 00:00:00 Sacramento~ Stone Lakes   EPA 104b~
##  4 543R00137   2012-05-15 00:00:00 Confluence  Deer Cr_137-~ CCCWP Cr~
##  5 543R01103   2015-04-21 00:00:00 Confluence  West Antioch~ CCCWP Cr~
##  6 544CCC001   2011-06-06 00:00:00 Confluence  San Joaquin ~ RWB5 Cle~
##  7 544CCC001   2011-07-06 00:00:00 Confluence  San Joaquin ~ RWB5 Cle~
##  8 544CCC001   2011-08-19 00:00:00 Confluence  San Joaquin ~ RWB5 Cle~
##  9 544CCC001   2011-09-20 00:00:00 Confluence  San Joaquin ~ RWB5 Cle~
## 10 544CCC001   2011-10-17 00:00:00 Confluence  San Joaquin ~ RWB5 Cle~
## # ... with 92 more rows, and 101 more variables: Projectcode <chr>,
## #   n_taxa <int>, n_E <int>, n_P <int>, n_T <int>, n_O <int>, n_D <int>,
## #   n_Phylum_NA <int>, n_Arthropoda <int>, n_Annelida <int>, n_Nematoda <int>,
## #   n_Ectoprocta <int>, n_Bacillariophyta <int>, n_Cryptophyta <int>,
## #   n_Heterokontophyta <int>, n_Ochrophyta <int>, n_Coelenterata <int>,
## #   n_Nemertea <int>, n_Mollusca <int>, n_Platyhelminthes <int>,
## #   n_Bryozoa <int>, n_Cyanobacteria <int>, n_Chlorophyta <int>,
## #   n_Euglenozoa <int>, n_Streptophyta <int>, n_Rhodophyta <int>,
## #   n_Chordata <int>, geometry <POINT [m]>, EPT_taxa <int>, EPT_index <dbl>,
## #   ETO_taxa <int>, ETO_index <dbl>, HUC12.x <chr>, StationName.y <chr>,
## #   Date.wq <date>, Project.y <chr>, n <int>, Subregion.y <chr>,
## #   Latitude <dbl>, Longitude <dbl>, n_Alk <int>, mean_Alk <dbl>, sd_Alk <dbl>,
## #   min_Alk <dbl>, max_Alk <dbl>, n_N <int>, mean_N <dbl>, sd_N <dbl>,
## #   min_N <dbl>, max_N <dbl>, n_Chl_F <int>, mean_Chl_F <dbl>, sd_Chl_F <dbl>,
## #   min_Chl_F <dbl>, max_Chl_F <dbl>, n_Chl_TR <int>, mean_Chl_TR <dbl>,
## #   sd_Chl_TR <dbl>, min_Chl_TR <dbl>, max_Chl_TR <dbl>, n_DO <int>,
## #   mean_DO <dbl>, sd_DO <dbl>, min_DO <dbl>, max_DO <dbl>, n_pH <int>,
## #   mean_pH <dbl>, sd_pH <dbl>, min_pH <dbl>, max_pH <dbl>, n_Sal <int>,
## #   mean_Sal <dbl>, sd_Sal <dbl>, min_Sal <dbl>, max_Sal <dbl>, n_Secc <int>,
## #   mean_Secc <dbl>, sd_Secc <dbl>, min_Secc <dbl>, max_Secc <dbl>,
## #   n_Cond <int>, mean_Cond <dbl>, sd_Cond <dbl>, min_Cond <dbl>,
## #   max_Cond <dbl>, n_Temp <int>, mean_Temp <dbl>, sd_Temp <dbl>,
## #   min_Temp <dbl>, max_Temp <dbl>, n_Turb <int>, mean_Turb <dbl>,
## #   sd_Turb <dbl>, min_Turb <dbl>, max_Turb <dbl>, n_Vel <int>, mean_Vel <dbl>,
## #   sd_Vel <dbl>, min_Vel <dbl>, max_Vel <dbl>, ...
```


```r
#write.csv(com.dates.HUC, "data/ceden_benthic_WQ_WBD.csv", row.names=F)
```




