### Eric Lawrence
### 12/13/2020

#### CEDEN BENTHIC DATA


library(tidyverse)
library(sf)
library(here)
library(readxl)

USFE.riskregions <- here("data/RiskRegions_DWSC_Update_9292020.shp") %>% # path relative to main directory
  st_read()

st_crs(USFE.riskregions)

crs.WGS84 <- st_crs(USFE.riskregions)


#### Load in CEDEN data

CEDEN <- read_excel("data/ceden_data_edited.xlsx")


#### Separate by Datum

unique(CEDEN$Datum)

ceden.wgs84 <- filter(CEDEN, Datum == "WGS84")
ceden.nad83 <- filter(CEDEN, Datum == "NAD83")
ceden.nr <- filter(CEDEN, Datum == "NR")
ceden.NAD27 <- filter(CEDEN, Datum == "NAD27")

#### set Datum, shapefiles

ceden.wgs84.sf <- ceden.wgs84 %>%
  st_as_sf(coords=c("TargetLongitude", "TargetLatitude"), crs = crs.WGS84)
st_crs(ceden.wgs84.sf)

ceden.nad83.sf <- ceden.nad83 %>%
  st_as_sf(coords=c("TargetLongitude", "TargetLatitude"), crs = 4269)

ceden.NAD27.sf <- ceden.NAD27 %>%
  st_as_sf(coords=c("TargetLongitude", "TargetLatitude"), crs = 4267)

# ceden.nr does not have a known coordinate system

######   Transform into WGS84

ceden.nad83.t.wgs84.sf <- st_transform(ceden.nad83.sf, crs.WGS84)
st_crs(ceden.nad83.t.wgs84.sf)

ceden.NAD27.t.wgs84.sf <- st_transform(ceden.NAD27.sf, crs.WGS84)
st_crs(ceden.NAD27.t.wgs84.sf)


# Join Excel Data to Risk Regions
ceden.wgs84.sf <- st_join(ceden.wgs84.sf, USFE.riskregions["Subregion"])
ceden.nad83.t.wgs84.sf <- st_join(ceden.nad83.t.wgs84.sf, USFE.riskregions["Subregion"])
ceden.NAD27.t.wgs84.sf <- st_join(ceden.NAD27.t.wgs84.sf, USFE.riskregions["Subregion"])

# ##### Comine datasets

ceden.all.sf <- bind_rows(ceden.wgs84.sf, ceden.nad83.t.wgs84.sf)
ceden.all.sf <- bind_rows(ceden.all.sf, ceden.NAD27.t.wgs84.sf)

### Remove records outside of Risk Regions

ceden.all.sf <- ceden.all.sf %>%
  filter(!is.na(Subregion))

# Basic plot
ggplot() +
  geom_sf(data = USFE.riskregions) +
  geom_sf(data = ceden.all.sf, aes(color = Subregion)) +
  scale_color_brewer(palette = "Set1") + # not color-blind safe
  ggtitle("Ceden Benthic Data")

summary(ceden.all.sf$Subregion)

# Write table

write.csv(ceden.all.sf, "data/ceden_with_RR.csv")



###############################################
##########   Analysis  ########################
###############################################

#### Create dataframe for stations

stations <- unique(ceden.all.sf$StationCode)

st.df <- ceden.all.sf %>%
  group_by(StationCode) %>%
  summarise(n(), n_distinct(SampleDate), first(Subregion)) %>%
  rename(Subregion = `first(Subregion)`)

tibble(st.df)

Subregions <- st.df$`first(Subregion)`

# Sample Stations Plot
ggplot() +
  geom_sf(data = USFE.riskregions) +
  geom_sf(data = st.df, aes(color = Subregion)) +
  scale_color_brewer(palette = "Set1") + # not color-blind safe
  ggtitle("Ceden Benthic Sample Stations")


# Isolate Samples

unique(ceden.all.sf$Phylum)


##### Create dataframe set into groups by station and sample date to isolate individual samples.
##### summarise by taxa presence, number of taxa present, and indexes

samp.df <- ceden.all.sf %>%
  group_by(StationCode, SampleDate) %>%
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


tibble(samp.df)
tibble(ceden.all.sf)
unique(ceden.all.sf$Orders)
table(ceden.all.sf$Orders
      )


############################### Bring in WQ data


ceden.wq <- read_excel("data/ceden_wq.xlsx")
tibble(ceden.wq)


#Remove records without lat/lon
ceden.wq <- ceden.wq[!is.na(ceden.wq$TargetLatitude),]

####### Excel Data to sf Object: needs x, y, CRS


ceden.wq <- ceden.wq %>%
  st_as_sf(coords=c("TargetLongitude", "TargetLatitude"), crs = crs.WGS84)

######### Spatial Join

ceden.wq.sf <- st_join(ceden.wq, USFE.riskregions["Subregion"])

### Remove records outside of Risk Regions

ceden.wq.sf <- ceden.wq.sf %>%
  filter(!is.na(Subregion))

# Basic plot
ggplot() +
  geom_sf(data = USFE.riskregions) +
  geom_sf(data = ceden.wq.sf, aes(color = Subregion)) +
  scale_color_brewer(palette = "Set1") + # not color-blind safe
  ggtitle("Ceden WQ Data")

summary(ceden.all.sf$Subregion)

########## Look for proximity to benthic data

tibble(ceden.wq.sf)

wq.stations <- ceden.wq.sf %>%
  group_by(StationName, SampleDate) %>%
  summarise(Project = first(Project),
            n = n(),
            Subregion = first(Subregion),
            
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
            
            n_DO = sum(Analyte == "Oxygen, Dissolved, Total", na.rm = TRUE),
            mean_DO = mean(Result[Analyte == "Oxygen, Dissolved, Total"], na.rm = TRUE),
            sd_DO = sd(Result[Analyte == "Oxygen, Dissolved, Total"], na.rm = TRUE),
            min_DO = min(Result[Analyte == "Oxygen, Dissolved, Total"], na.rm = TRUE),
            max_DO = max(Result[Analyte == "Oxygen, Dissolved, Total"], na.rm = TRUE),
            
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
            
            n_Cond = sum(Analyte == "SpecificConductivity, Total", na.rm = TRUE),
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
            mean_Turb = mean(Result[Analyte == "Turbidity, Total"], na.rm = TRUE),
            sd_Turb = sd(Result[Analyte == "Turbidity, Total"], na.rm = TRUE),
            min_Turb = min(Result[Analyte == "Turbidity, Total"], na.rm = TRUE),
            max_Turb = max(Result[Analyte == "Turbidity, Total"], na.rm = TRUE),
            
            n_Turb = sum(Analyte == "Turbidity, Total", na.rm = TRUE),
            mean_Turb = mean(Result[Analyte == "Turbidity, Total"], na.rm = TRUE),
            sd_Turb = sd(Result[Analyte == "Turbidity, Total"], na.rm = TRUE),
            min_Turb = min(Result[Analyte == "Turbidity, Total"], na.rm = TRUE),
            max_Turb = max(Result[Analyte == "Turbidity, Total"], na.rm = TRUE),
            
            n_Vel = sum(Analyte == "Velocity", na.rm = TRUE),
            mean_Vel = mean(Result[Analyte == "Velocity"], na.rm = TRUE),
            sd_Vel = sd(Result[Analyte == "Velocity"], na.rm = TRUE),
            min_Vel = min(Result[Analyte == "Velocity"], na.rm = TRUE),
            max_Vel = max(Result[Analyte == "Velocity"], na.rm = TRUE),
            
            ) %>%
  rename(SampleDate.wq = SampleDate)


unique(ceden.wq.sf$Analyte)
unique(ben_wq_dates$Analyte)
tibble(wq.stations)

####### Transform into projection to compare distance

# Transform into UTM Zone 10n EPSG:26910

wq.stations <- st_transform(wq.stations, 26910)
st_crs(wq.stations)

#st.df.u10 <- st_transform(st.df, 26910)
#st_crs(st.df.u10)

rr.u10 <- st_transform(USFE.riskregions, 26910)
st_crs(st.df.u10)

samp.df.u10 <- st_transform(samp.df, 26910)

### Create 500m buffer around WQ sampling locations

wq.stations.buffer <- st_buffer(wq.stations, 500)

### Remove any buffers outside of the risk regions

wq.stations.buffer <- wq.stations.buffer %>%
  filter(!is.na(Subregion))

### Plot WQ buffers and ceden benthic data

ggplot() +
  geom_sf(data = USFE.riskregions) +
  geom_sf(data = wq.stations.buffer, aes(color = Subregion)) +
  scale_color_brewer(palette = "Set1") + # not color-blind safe
  geom_sf(data = st.df.u10) +
  ggtitle("Ceden WQ Data")

### Join Benthic locations and WQ buffers

samp.wq.com <- st_join(samp.df.u10, wq.stations.buffer, left = TRUE)

### Select records that have sampling data from the same date for benthic and WQ

com.dates <- samp.wq.com %>%
  filter(SampleDate == SampleDate.wq)

tibble(com.dates)

### Plot benthic sampling locations that has WQ available on the same date

ggplot() +
  geom_sf(data = USFE.riskregions) +
  geom_sf(data = com.dates, aes(color = Subregion.x)) +
  scale_color_brewer(palette = "Set1") + # not color-blind safe
  ggtitle("Ceden Benthic Data with WQ on same date")

### Change infinities and NaN values to NA
com.dates <- com.dates %>% 
  mutate_if(is.numeric, list(~na_if(., Inf))) %>% 
  mutate_if(is.numeric, list(~na_if(., -Inf))) %>%
  mutate_if(is.numeric, list(~na_if(., "NaN"))) %>%
  mutate_if(is.numeric, list(~na_if(., NaN)))

write.csv(com.dates, "data/ceden_benthic_WQ.csv")

############### CEDEN Analysis

ceden.sel <- com.dates %>%
  select(StationCode, SampleDate, Subregion.x, n_taxa, n_E,
         n_P, n_T, n_O, n_D, n_Arthropoda, n_Annelida, 
         n_Nematoda, n_Ectoprocta, n_Bacillariophyta,
         n_Cryptophyta, n_Heterokontophyta, n_Ochrophyta,
         n_Coelenterata, n_Nemertea, n_Mollusca, n_Platyhelminthes,
         n_Bryozoa, n_Cyanobacteria, n_Chlorophyta, n_Euglenozoa,
         n_Streptophyta, n_Rhodophyta, n_Chordata, EPT_taxa, EPT_index,
         ETO_taxa, ETO_index,
         
         mean_Alk, mean_DO, mean_pH, mean_Sal, mean_Secc, mean_Cond,
         mean_Temp, mean_Turb, mean_Vel
         
         )
tibble(ceden.sel)



########## Correlation between env variables

env <- ceden.sel %>%
  select(StationCode, SampleDate, Subregion.x, mean_Alk, mean_DO, mean_pH, mean_Sal, mean_Secc, mean_Cond,
         mean_Temp, mean_Turb, mean_Vel)

env$Subregion.x <- as.factor(env$Subregion.x)

tibble(env)

plot(env)

par(mfrow = c(3,3), mar=c(9,1,1,1))

names <- colnames(env)

for (i in 4:12) {
  
  x <- env[,i, drop = TRUE]
  
  boxplot(x ~ env$Subregion.x,
          ylab = paste(names[i]),
          las = 0.5
          )
}

env[,4]

boxplot(env$mean_Alk ~ env$Subregion.x)

boxplot(env[,4] ~ env$Subregion.x)

env$mean_Alk

plot(ceden.sel[,4:10])
plot(ceden.sel[,10:22])
plot(ceden.sel[,33:41])


boxplot(ceden.sel$n_taxa ~ ceden.sel$Subregion.x)
boxplot(ceden.sel$EPT_taxa ~ ceden.sel$Subregion.x)

plot(ceden.sel$n_taxa ~ ceden.sel$mean_Temp)
plot(ceden.sel$n_taxa ~ ceden.sel$mean_DO)
plot(ceden.sel$n_taxa ~ ceden.sel$mean_pH)
plot(ceden.sel$n_taxa ~ ceden.sel$mean_Sal)
plot(ceden.sel$n_taxa ~ ceden.sel$mean_Cond)
plot(ceden.sel$n_taxa ~ ceden.sel$mean_Turb)
plot(ceden.sel$n_taxa ~ ceden.sel$mean_Alk)

################# Correlation Analysis

source("correlation.r")

ceden.sel <- st_set_geometry(ceden.sel, NULL)

ceden.sel[4:8] <- lapply(ceden.sel[,4:8], as.numeric)

ceden.cor <- correlation.matrix(ceden.sel[, c(4:8)], method="kendall")
ceden.cor

write.table(ceden.cor$statistics, "cedencorSTATS.csv",
            row.names=T, col.names=NA, sep=",")
write.table(ceden.cor$p.values, "cedencorPVALUES.csv",
            row.names=T, col.names=NA, sep=",")
str(ceden.sel[4:8])

cor <- ceden.sel %>%
  select(n_taxa, n_E, n_P, n_T, n_O)

cor(ceden.sel[,c(4:6)])

ceden.cor <- correlation.matrix(as.matrix(cor[, c(2:6)]), method="kendall")

correlation.matrix(c(cor$n_taxa), method = "kendall")

ceden.cor

install.packages("Hmisc")
library(Hmisc)

ceden.cor <- cor(ceden.sel[,4:40])
rcorr(as.matrix(ceden.sel[,4:6]))

## Riffle

source("riffle02.r")

sink("riffleRESULTS.txt")
riffleNR = riffle(ceden.sel[, c(4:41)], 3, numreps=5)
print(riffleNR)
sink()


print(table(wine$type, riffleNR$cluster))
print(chisq.test(wine$type[wine$type!="unk"],
                 riffleNR$cluster[wine$type!="unk"]))