## Creating a quick map using the tidycensus package

## Ages 16-64 labor force

##### ------------- Load packages ------------- ##### 
library(tidycensus)
library(tidyverse)
library(tmap)
library(tmaptools)
library(sf)
library(rnaturalearth)

## Make sure census key is setup
#census_api_key("key_goes_here", install = T)


##### ------------- Census data ------------- ##### 

county.working.under65 <- get_acs(geography = "county",
                          variables = c("B23001_003E", "B23001_004E", "B23001_010E", "B23001_011E", "B23001_017E", "B23001_018E", "B23001_024E", "B23001_025E", "B23001_031E", "B23001_032E", "B23001_038E", "B23001_039E", "B23001_045E", "B23001_046E", "B23001_052E", "B23001_053E", "B23001_059E", "B23001_060E", "B23001_066E", "B23001_067E", "B23001_089E", "B23001_090E", "B23001_096E", "B23001_097E", "B23001_103E", "B23001_104E", "B23001_110E", "B23001_111E", "B23001_117E", "B23001_118E", "B23001_124E", "B23001_125E", "B23001_131E", "B23001_132E", "B23001_138E", "B23001_139E", "B23001_145E", "B23001_146E", "B23001_152E", "B23001_153E"), 
                          output = "wide",
                          geometry = T, 
                          shift_geo = TRUE)


county.working.under65 %>% 
  mutate(pop.under.65 = B23001_003E + B23001_010E + B23001_017E + B23001_024E + B23001_031E + B23001_038E + B23001_045E + B23001_052E + B23001_059E + B23001_066E + B23001_089E + B23001_096E + B23001_103E + B23001_110E + B23001_117E + B23001_124E + B23001_131E + B23001_138E + B23001_145E + B23001_152E,
   pop.under.65.labor = B23001_004E + B23001_011E + B23001_018E + B23001_025E + B23001_032E + B23001_039E + B23001_046E + B23001_053E + B23001_060E + B23001_067E + B23001_090E + B23001_097E + B23001_104E + B23001_111E + B23001_118E + B23001_125E + B23001_132E + B23001_139E + B23001_146E + B23001_153E) %>% 
  select(-B23001_003E:-B23001_153E) %>% 
  mutate(working.pct = 100*(pop.under.65.labor/pop.under.65))-> county.working.under65


##### ------------- Map ------------- ##### 

## Setup Cont. US projection
us.proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

## State data for borders
data("state_laea", package = "tidycensus")

## Make the map -- JENK BINS
work.map <- tm_shape(county.working.under65, 
                     projection = us.proj) +
  tm_fill("working.pct",
          palette = "PuBuGn",
          title = "% Working",
          style = "jenks") +
  tm_shape(state_laea,
           projection = us.proj) +
  tm_borders() +
  tm_layout(main.title = "Percentage of the Population in the Labor Force (age 16-64), 2013-2017",
            legend.title.size = .8,
            legend.text.size = 0.5, 
            legend.position = c("LEFT", "BOTTOM"),
            main.title.size = .7, 
            frame = F) +
  tm_credits("Source: 2013-2017 5-year American Community Survey | Measures include people 16 years to 64 years old", 
             position = c("RIGHT", "BOTTOM"),
             size = .3) 


## save image
tmap_save(work.map, "working_county_map_under65.png", width=1920, height=1080, asp=0)
