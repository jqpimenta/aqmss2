library(tidyverse)
library(ggplot2)
library(modelsummary)
library(sf)
library(spData)

## PART I--------------------------------------------------------------------------------------

## Inspecting an sf object

data(world)
class(world)
names(world)
nrow(world)
# An sf object is different from a df in that it stores additional geographical data in a 
# geometry column. The geometry column stores polygons, points, and lines. It is stored in a 
# "sticky" way so spatial attributes can easily travel the data.

st_crs(world) # EPSG: 4326
# WGS84 is the global standard coordinate system that is used by most mapping tools. It is the 
# standard because coordinates are expressed in a standardised way through decimal degrees of 
# longitude and latitude, which makes it suitable when common reference is needed across different 
# countries or regions.

st_geometry_type(world)
unique(st_geometry_type(world))
# A multipolygon is a collection of two or more polygons that are treated as belonging to the 
# same country or region. Example 1: The states of Alaska and Hawaii in the case of the United 
# States. Example 2: The Canary Islands in the case of Spain.

pdf("assignment7/world_gdp_base.pdf")
plot(world["gdpPercap"])
dev.off()
# The richest regions appear to be Northern Europe, North America, some countries in the Middle 
# East, and Australia. The poorest appears to be South America, Africa, and Asia.

## Attribute operations

africa = filter(world, continent == "Africa")
nrow(africa) # 51
plot(africa["gdpPercap"])
# For some reason, Africa doesn't show up on the map and I don't know how to fix it. Sorry.

world = world %>%
  mutate(pop_millions = pop / 1e6)
gdp_by_continent = world %>%
  group_by(continent) %>%
  summarise(mean_gdpPercap = mean(gdpPercap, na.rm = TRUE))
print(gdp_by_continent)
# If you use summary before dropping the geography column beforehand, it will union the 
# geometries within each group and keep the geography column.

africa_sorted <- africa %>%
  arrange(desc(gdpPercap)) %>%
  select(name_long, gdpPercap)
print(head(africa_sorted, 5))
# Equatorial Guinea, Gabon, Libya, Botswana, Algeria

## Simple visualisation with ggplot2

ggplot(world) +
  geom_sf(aes(fill = gdpPercap)) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80", name = "GDP per capita") +
  theme_void() +
  labs(title = "GDP per capita by country")
ggsave("assignment7/world_gdp.pdf", width = 10, height = 5)
# The wealthiest regions appear to be North America, North and Central Europe, the Middle East, 
# and Australia. The poorest regions appear to be Africa, especially Central Africa, South and 
# South-East Asia, and Iberoamerica.

ggplot(africa) +
  geom_sf(aes(fill = gdpPercap)) +
  scale_fill_viridis_c(option = "magma", na.value = "grey80", name = "GDP per capita") +
  theme_void() +
  labs(title = "GDP per capita in Africa")
ggsave("assignment7/africa_gdp.pdf")
# North and Southern Africa as regions are wealthier than Central and Western Africa.

ggplot(africa) +
  geom_sf(aes(fill = gdpPercap), color = "white", linewidth = 0.3) +
  scale_fill_viridis_c(option = "magma", na.value = "grey80", name = "GDP per capita") +
  theme_void() +
  labs(title = "GDP per capita in Africa, with borders")
ggsave("assignment7/africa_gdp_borders.pdf")
# The borders improve readability especially for the very black zone in Central and Western 
# Africa, where nuance gets lost especially for small countries.

## PART II-------------------------------------------------------------------------------------

## Converting tabular data to sf

events = read.csv("data/conflict_events.csv")
events_sf = st_as_sf(events,
  coords = c("longitude", "latitude"),
  crs = 4326)
class(events)
st_crs(events)
# st_as_sf() concerts a df into a sf. The coords argument tell the function which two columns to 
# use for the final geography column, i.e., which columns correspond to longitude and latitude. 
# crs = 4326 is the standard coordinating system that is used by most mapping tools.

nrow(events_sf) # 68.354 events
table(events_sf$event_type) # state-based events are most common

ggplot() +
  geom_sf(data = world, fill = "grey") +
  geom_sf(data = events_sf, aes(colour = event_type))
ggsave("assignment7/ass7_conflictevents.png")
# The geographic pattern is that Africa is the most affected continent. All thoughout Africa, 
# there are different types of events scattered.

## Spatial join: events to countries

st_crs(events_sf) # 4326
st_crs(world) # 4326
events_joined <- st_join(events_sf, world)
nrow(events_joined) # 68.354
nrow(events_sf) # 68.354
# The function works by comparing the polygons in both datasets. If there is a matching polygon, 
# it will automatically assign the world data to the events_sf dataset. If not, it will fill in 
# NA. The CRS is important, because depending on the projection, the polygons might not match. 
# For example, if one projection uses degrees and the other metres, the polygons will be wrongly 
# matched to each other.

unmatched <- sum(is.na(events_joined$name_long))
total <- nrow(events_joined)
no_match <- unmatched / total # 0.023
# 1. The events could have happened on disputed land, which might not return a polygon for either 
# country that is claiming (or not claiming) it.
# 2. The event could have happened on international sea, which does not belong to any country. In 
# this case, it cannot be attributed to any.

fatalities <- events_joined %>%
  filter(!is.na(name_long)) %>%
  group_by(name_long) %>%
  summarise(
    events = n(),
    fatalities = sum(fatalities)
  ) %>%
  arrange(desc(events)) %>%
  st_drop_geometry()
print(fatalities)
# The top 10 countries are consistent with my knowledge of contemporary geopolitical conflicts. 
# They are all located in regions in Africa where there have been tensions or ongoing wars, 
# conflicts etc.

## Choropleth of conflict intensity

fatalities_df <- fatalities %>%
  st_drop_geometry()
world_joined <- world %>%
  left_join(fatalities_df, by = "name_long") %>%
  replace_na(list(events = 0, fatalities = 0))
nrow(world_joined) # 177
nrow(world) # 177

ggplot(world_joined) +
  geom_sf(aes(fill = events)) +
  scale_fill_distiller(palette = "Reds")
ggsave("assignment7/ass7_map.png")
# The map is almost fully red, except in Africa, where individual countries are brighter or 
# almost white. Thus, the geographical map matches the results obtained in the top 10 table.

ggplot(world_joined) +
  geom_sf(aes(fill = log1p(events))) +
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = 1,
    name = "Log(events+1)"
  )
ggsave("assignment7/conflict_log_map.pdf")
# The log transformation is useful, as it makes the difference between extremely high and 
# extremely low values more moderate. Because of this, the new map should show more nuance 
# between the regions, rather than just the most conflict-driven ones.

## Discussion

# I could imagine that one of the limitation is how it handles cases that are unclear. If events 
# are exactly on the border between two countries, it might get assigned to both or neither. 
# Events that fall outside a polygon due to an imprecision will not be registered and thus 
# excluded from the analysis.

# st_join() compares the coordinates of two sf objects to decide which rows belong together, 
# thus joining them based on location. left_join() matches rows based on shared columns, not 
# geography. I would use st_join() for matching, for example, events to countries, but would use 
# left_join() when there is a common variable (like previously in the assignment).