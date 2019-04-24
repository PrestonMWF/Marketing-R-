library(tidyverse)
library(tidycensus)
library(viridis)
library(mapview)
library(gridExtra)

boats <- read.csv("Boats.csv", stringsAsFactors = F)

boats <- boats %>%
  select_if(
    !grepl(x = names(boats), pattern = "Q16")
  )

theme_set(
  theme_minimal()
)

#growth strategy in NA
#better understand current and potential customers (use EDA and clustering)
#build more targeted boats for these customers/potentials

#do the quant: factor analysis to pick variables (lca or pca)- 29 Qs
#segment customers and potentials- Q1 1-15
#pull us census demogrpahic data- find target demographics for best clusters

#census API work
api_key <- "42443004b4611e4345ca3dc6f2e47464c580624e"

census_api_key(key = api_key, install = T)

#loads API key from the previous argument- only needs to be done once
readRenviron("~/.Renviron")

state_income <- get_acs(geography = "state", 
                        variables = c(hh_income = "B19013_001"))

#used to find any of the available variables
#for boats data include: age, income, gender
census_vars <- load_variables(year = 2016,
                              dataset = "acs5")

county_income <- get_acs(geography = "county", 
                         variables = c(hh_income = "B19013_001",
                                       median_age = "B01002_001"),
                         output = "wide")

state_income %>%
  top_n(n = 25, wt = estimate) %>%
  mutate(NAME = reorder(NAME, estimate)) %>%
  ggplot(aes(NAME, estimate)) +
  geom_segment(aes(NAME, xend = NAME, y = 0, 
                   yend = estimate), colour = "darkgray", size = 1) +
  geom_point(size = 4, colour = "dodgerblue2") +
  coord_flip() +
  scale_y_continuous(labels = scales::dollar, 
                     breaks = seq(0, 75000, 15000)) +
  labs(title = "Household median income by state from 2016 American Community Survey (ACS)",
       x = NULL,
       y = "median income")

#land on desirable county and state for mapping vis
options(tigris_use_cache = TRUE)

IL <- get_acs(geography = "tract", 
              variables = "B19013_001", 
              state = "IL", 
              county = "Cook", 
              geometry = TRUE)

#static ggplot map
#link to attribute tracts to community area
#http://robparal.blogspot.com/2012/04/census-tracts-in-chicago-community.html

IL %>%
  ggplot(aes(fill = estimate, color = estimate)) + 
  geom_sf() + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis(option = "magma") + 
  scale_color_viridis(option = "magma")

#interactive map
mapview(IL, zcol = "estimate", legend = TRUE)

#US households over 100K- not all that descriptive so pare down further
us_income <- get_acs(geography = "county", 
                     variables = "B19001_014", 
                     geometry = TRUE)

us_income %>%
  ggplot(aes(fill = estimate, color = estimate)) + 
  geom_sf() + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis(option = "magma") + 
  scale_color_viridis(option = "magma")

#Finding target locations
county_high_income <- get_acs(geography = "county", 
                              variables = c(over_100 = "B19001_014",
                                            over_120 = "B19001_015",
                                            over_140 = "B19001_016",
                                            over_200 = "B19001_017",
                                            median_age = "B01002_001"),
                              output = "wide") %>%
  arrange(desc(over_100E, median_ageE))

county_high_income %>%
  select(over_100E, over_120E, over_140E, over_200E) %>%
  summarise_all(sum)
  
county_high_income %>%
  top_n(n = 100, wt = over_100E) %>%
  mutate(State = str_extract(string = NAME, pattern = "\\,.*"),
         State = tm::removePunctuation(State),
         State = str_trim(string = State, side = "both")) %>%
  count(State) %>%
  arrange(desc(n)) %>%
  slice(1:10) %>%
  mutate(State = reorder(State, n)) %>%
  ggplot(aes(State, n)) +
  geom_segment(aes(State, xend = State, y = 0, 
                   yend = n), colour = "darkgray", size = 1) +
  geom_point(size = 4, colour = "dodgerblue2") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 15, 2)) +
  labs(title = "Numer of households with over $100,000 median income by state from top 100 counties",
       subtitle = "Plot helps make state level market entry decisions",
       x = NULL,
       y = "households > $100,000")

county_high_income %>%
  top_n(n = 25, wt = over_100E) %>%
  mutate(NAME = reorder(NAME, over_100E)) %>%
  ggplot(aes(NAME, over_100E)) +
  geom_segment(aes(NAME, xend = NAME, y = 0, 
                   yend = over_100E), colour = "darkgray", size = 1) +
  geom_point(size = 4, colour = "dodgerblue2") +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 500000, 25000)) +
  labs(title = "Number of households with over $100,000 median income by state from 2016 American Community Survey (ACS)",
       subtitle = "Plot helps make county level market entry decisions",
       x = NULL,
       y = "households > $100,000")

#Target location: Middlesex County
MASS <- get_acs(geography = "tract", 
                variables = "B19001_014", 
                state = "MA", 
                county = "Middlesex County", 
                geometry = TRUE)

middlesex_market <- MASS %>%
  mutate(county_name = "Middlesex_County_MASS") %>%
  ggplot(aes(fill = estimate, color = estimate)) + 
  geom_sf() + 
  coord_sf(crs = 26911) + 
  facet_wrap(facets = "county_name") +
  scale_fill_viridis(option = "magma") + 
  scale_color_viridis(option = "magma")

#Target location: Harris County
TX <- get_acs(geography = "tract", 
              variables = c(over_100K_household = "B19001_014", 
                            median_income = "B19001_001"),
              state = "TX", 
              county = "Harris County", 
              geometry = TRUE)

#harris market sizing
harris_market <- TX %>%
  mutate(county_name = "Harris_County_TX") %>%
  filter(variable == "over_100K_household") %>%
  ggplot(aes(fill = estimate, color = estimate)) + 
  geom_sf() + 
  facet_wrap(facets = "county_name") +
  coord_sf(crs = 26911) + 
  scale_fill_viridis(option = "magma") + 
  scale_color_viridis(option = "magma")

#Target location: King County
WA <- get_acs(geography = "tract", 
              variables = c(over_100K_household = "B19001_014", 
                            median_income = "B19001_001"),
              state = "WA", 
              county = "King County", 
              geometry = TRUE)

king_market <- WA %>%
  mutate(county_name = "King_County_WA") %>%
  filter(variable == "over_100K_household") %>%
  ggplot(aes(fill = estimate, color = estimate)) + 
  geom_sf() + 
  coord_sf(crs = 26911) + 
  facet_wrap(facets = "county_name") +
  scale_fill_viridis(option = "magma") + 
  scale_color_viridis(option = "magma")

#Target location: San Diego County
CA <- get_acs(geography = "tract", 
              variables = "B19001_014", 
              state = "CA", 
              county = "San Diego County", 
              geometry = TRUE)

sandiego_market <- CA %>%
  mutate(county_name = "San_Diego_County_CA") %>%
  ggplot(aes(fill = estimate, color = estimate)) + 
  geom_sf() + 
  coord_sf(crs = 26911) + 
  facet_wrap(facets = "county_name") +
  scale_fill_viridis(option = "magma") + 
  scale_color_viridis(option = "magma")

#four attractive market options with maps
grid.arrange(sandiego_market, king_market, middlesex_market, harris_market,
             top = "County level maps for four attractive market entry locations \n Lighter locations also mark possible dealership areas")

CA_state <- get_acs(geography = "tract", 
              variables = "B19013_001", 
              state = "CA", 
              geometry = TRUE)

CA_state %>%
  ggplot(aes(fill = estimate, color = estimate)) + 
  geom_sf() + 
  coord_sf(crs = 26911) + 
  scale_fill_viridis(option = "magma") + 
  scale_color_viridis(option = "magma") +
  ggtitle("California median income by household levels")
