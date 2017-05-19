library(tidyverse)
library(stringr)

# data from CDC detailed mortality database.  https://wonder.cdc.gov/ucd-icd10.html
# selecting using premade drug-induced death category
ucd <- read_tsv("Underlying Cause of Death, 1999-2015.txt")

# clean coerce to numeric, converting 'unreliable' to NA

ucd$`Crude Rate` <- as.numeric(ucd$`Crude Rate`)
ucd$`Age Adjusted Rate` <- as.numeric(ucd$`Age Adjusted Rate`)
ucd$`% of Total Deaths` <- as.numeric(ucd$`% of Total Deaths`)
ucd$`% of Total Deaths` <- str_replace(ucd$`% of Total Deaths`,"%","")

# Graph all states

ucd %>% 
  ggplot(aes(`Year Code`, `Crude Rate`)) +
  geom_bar(stat = "identity") +
  facet_wrap(~State)
ggsave("graph_facet_states.png", height = 9, width = 16, units = "in", scale = 1)

# facet bar chart: All CA Counties 

ucd %>% 
  filter(State == "California") %>% 
  ggplot(aes(`Year Code`, `Crude Rate`)) +
  geom_bar(stat = "identity") +
  facet_wrap(~County)
ggsave("graph_facet_ca_counties.png", height = 9, width = 16, units = "in", scale = 1)


# facet bar chart: All CA Counties at least 500k+ population in 2015


target <-                         # create list of counties with 500k ppl in 2015
ucd %>% 
  filter(State == "California",
         Population > 500000,
         `Year Code`== 2015)
target <- target$County

ucd %>% 
  filter(County %in% target) %>%  # filter by created list
  ggplot(aes(`Year Code`, `Crude Rate`)) +
  geom_bar(stat = "identity") +
  facet_wrap(~County)
ggsave("graph_facet_ca_counties_500k.png", height = 9, width = 16, units = "in", scale = 1)




# line chart: All CA Counties 500k+ population

ucd %>% 
  filter(County %in% target) %>% 
  ggplot(aes(`Year Code`, `Crude Rate`, color = County)) +
  geom_line()


# All US Counties b/w 600,000 and 1,000,000

target <- 
  ucd %>% 
  filter(Population > 600000,
         Population < 1000000,
         `Year Code`== 2015)
target <- target$County

ucd %>% 
  filter(County %in% target) %>% 
  ggplot(aes(`Year Code`, `Crude Rate`)) +
  geom_bar(stat = "identity") +
  facet_wrap(~County)
ggsave("graph_facet_counties_600k_1M.png", height = 9, width = 16, units = "in", scale = 1)


# All US Cities b/w 700,000 and 900,000

target <- 
  ucd %>% 
  filter(Population > 700000,
         Population < 900000,
         `Year Code`== 2015)
target <- target$County

ucd %>% 
  filter(County %in% target) %>% 
  ggplot(aes(`Year Code`, `Crude Rate`)) +
  geom_bar(stat = "identity") +
  facet_wrap(~County)
ggsave("graph_facet_counties_700k_900k.png", height = 9, width = 16, units = "in", scale = 1)

# Compare select counties.  


ucd %>% 
  filter(County == "Los Angeles County, CA"|
         County == "Alameda County, CA"|
         County == "Cook County, IL"|
         County == "New York County, NY"|
         County == "Bronx County, NY"|
         County == "San Diego County, CA"|
         County == "Philadelphia County, PA"|
         County == "Allegheny County, PA"|
         County == "San Francisco County, CA"|
         County == "Middlesex County, MA"|
         County == "Queens County, NY") %>% 
  ggplot(aes(`Year Code`, `Crude Rate`)) +
  geom_bar(stat = "identity") +
  facet_wrap(~County)
ggsave("graph_facet_counties_select.png", height = 9, width = 16, units = "in", scale = 1)

# SF vs Counties 1M+
target <- ucd %>% 
  filter(Population > 1000000,
         `Year Code` == 2015) 
target <- target$County

ucd %>% 
  filter(County == "San Francisco County, CA"|
           County %in% target) %>% 
  ggplot(aes(`Year Code`, `Crude Rate`)) +
  geom_bar(stat = "identity") +
  facet_wrap(~County)
ggsave("graph_facet_counties_sf_1M+.png", height = 9, width = 16, units = "in", scale = 1)

