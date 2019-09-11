library(tidycensus)
library(tidyverse)
#library(censusapi)

census_api_key("key_goes_here", install = T)


m90 <- get_decennial(geography = "state", variables = "H043A001", year = 1990)

m90 %>%
  ggplot(aes(x = value, y = reorder(NAME, value))) + 
  geom_point()

va <- get_acs(geography = "county",
              variables = "DP05_0033E",
              state = "Virginia", 
              year = 2017, 
              geometry = T)

head(va)

va %>% 
  ggplot(aes(fill = estimate)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(option = "magma")


orange <- get_acs(state = "CA", county = "Orange", geography = "tract", 
                  variables = "B19013_001", geometry = TRUE)

atl <- get_acs(state = "GA", county = "Fulton", geography = "tract",
               variables = c("DP05_0033E"), geometry = T) %>% 
  rename(pop = estimate)

atl.black <- get_acs(state = "GA", county = "Fulton", geography = "tract",
                     variables = c("DP05_0038E"), geometry = F) %>% 
  rename(black = estimate) %>% 
  select(-moe, -variable, -NAME)

atl <- left_join(atl, atl.black, by = "GEOID")

atl <- atl %>% 
  mutate(black.pct = black/pop)

atl %>% 
  ggplot(aes(fill = black.pct)) +
  geom_sf(color = NA) +
  scale_fill_viridis_c(option = "magma") +
  geom_point(data = sf, aes(long,lat), color = "green", size=4)

ggplot() +
  geom_sf(data = atl, aes(fill = black.pct), color = NA) +
  scale_fill_viridis_c(option = "magma") +
  geom_point(data = sf, aes(long,lat), color = "green", size=4)

sf<-data.frame(long=-84.383950,lat=33.715160)


library(tmap)
library(tmaptools)
library(rnaturalearth)
library(grid)

sf.2 <- st_as_sf(sf,coords = c("long", "lat"))

tm_shape(atl) +
  tm_fill("black.pct") +
  tm_shape(sf.2) +
  tm_dots(col = "green", size = .25)

atl.poor <- get_acs(state = "GA", county = "Fulton", geography = "tract",
                    variables = c("DP03_0127PE"), geometry = T) %>% 
  rename(pov = estimate)

tm_shape(atl.poor) +
  tm_fill("pov") +
  tm_shape(sf.2) +
  tm_dots(col = "green", size = .25)


##########################################################################

county.working <- get_acs(geography = "county",
                          variables = c("DP03_0002E"), geometry = T) %>% 
  rename(working = estimate)

head(county.working)

county.working.total <- get_acs(geography = "county",
                                variables = c("DP03_0001E"), geometry = F) %>% 
  rename(total = estimate) %>% 
  select(-moe, -NAME, -variable)

county.working <- left_join(county.working, county.working.total, by = "GEOID")

county.working <- county.working %>% 
  mutate(working.pct = 100*(working/total))

county.working$state <- str_extract(string = county.working$NAME ,pattern = '\\b[^,]+$')

### Drop Alaska, Hawaii, etc.
county.working <- county.working %>% 
  filter(state != "Puerto Rico",
         state != "Hawaii",
         state != "Alaska")


us.proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"

tm_shape(county.working, 
         projection = us.proj) +
  tm_fill("working.pct",
          palette = "PuBuGn",title = "16 years+ in labor force (%)") +
  tm_shape(states_shp,
           projection = us.proj) +
  tm_borders() +
  tm_layout(main.title = "What percentage of people are in the labor force?")

"DP03_0007E" #16 and over, not in labor force
"DP03_0002E" # in labor force 
"DP03_0001E" # total 16+








# Call world shapefile from the rnaturalearth package as a sf object
states_shp <- ne_states(returnclass = "sf",
                        country = "United States of America") %>% 
  filter(name != "Alaska" & name != "Hawaii")

tm_shape(states_shp) +
  tm_borders()
