library(tidyverse)
library(stringr)

# data from CDC detailed mortality database.  https://wonder.cdc.gov/ucd-icd10.html
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

# facet bar chart: All CA Cities 500k+ population

ucd %>% 
  filter(State == "California",
         Population > 500000) %>% 
  ggplot(aes(`Year Code`, `Crude Rate`)) +
  geom_bar(stat = "identity") +
  facet_wrap(~County)
  

# line chart: All CA Cities 500k+ population

ucd %>% 
  filter(State == "California",
         Population > 500000) %>%
  ggplot(aes(`Year Code`, `Crude Rate`, color = County)) +
  geom_line()


# All US Cities b/w 600,000 and 1,000,000

ucd %>% 
  filter(Population > 600000,
         Population < 1000000) %>% 
  ggplot(aes(`Year Code`, `Crude Rate`)) +
  geom_bar(stat = "identity") +
  facet_wrap(~County)


