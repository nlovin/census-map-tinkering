## Creating a quick map using the tidycensus package

##### ------------- Load packages ------------- ##### 
library(tidycensus)
library(tidyverse)
library(tmap)
library(tmaptools)
library(rnaturalearth)

## Make sure census key is setup
#census_api_key("key_goes_here", install = T)

##### ------------- Census data ------------- ##### 

## Number of 16+ in the workforce
county.working <- get_acs(geography = "county",
                          variables = c("DP03_0002E"), geometry = T) %>% 
  rename(working = estimate)

head(county.working)

## Total number of 16+ in the pop
county.working.total <- get_acs(geography = "county",
                                variables = c("DP03_0001E"), 
                                geometry = F) %>% 
  rename(total = estimate) %>% 
  select(-moe, -NAME, -variable)

## Join 
county.working <- left_join(county.working, county.working.total, by = "GEOID")

## Generate percent working
county.working <- county.working %>% 
  mutate(working.pct = 100*(working/total))

## Create state variable to use for filtering
county.working$state <- str_extract(string = county.working$NAME ,pattern = '\\b[^,]+$')

## Drop Alaska, Hawaii, etc.
county.working <- county.working %>% 
  filter(state != "Puerto Rico",
         state != "Hawaii",
         state != "Alaska")

##### ------------- Setup maps ------------- ##### 

## Call world shapefile from the rnaturalearth package as a sf object
## use this for state border shape
states_shp <- ne_states(returnclass = "sf",
                        country = "United States of America") %>% 
  filter(name != "Alaska" & name != "Hawaii")


## Setup Cont. US projection
us.proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

##### ------------- Map ------------- ##### 

## Make the map
work.map <- tm_shape(county.working, 
         projection = us.proj) +
  tm_fill("working.pct",
          palette = "PuBuGn",
          title = "Active in labor force (%)", ) +
  tm_shape(states_shp,
           projection = us.proj) +
  tm_borders() +
  tm_layout(main.title = "Percentage of the Population in the Labor Force (by county), 2013-2017",
            legend.title.size = .8,
            legend.text.size = 0.5, 
            legend.position = c("LEFT", "BOTTOM"),
            main.title.size = .8) +
  tm_credits("Source: 2013-2017 5-year American Community Survey | Measures include people 16 years and older", 
             position = c("RIGHT", "BOTTOM"),
             size = .3)


## save image
tmap_save(work.map, "working_county_map.png", width=1920, height=1080, asp=0)



##### ------------- Notes ------------- ##### 

#"DP03_0007E" # 16 and over, not in labor force
#"DP03_0002E" # in labor force 
#"DP03_0001E" # total 16+

