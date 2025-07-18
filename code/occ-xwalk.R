# scrape occupational categories

library(rvest)
library(tidyverse)
library(janitor)

# Read and parse the page
raw_table <- read_html("https://hr.sao.texas.gov/compensationsystem/jobdescriptions") %>% 
  html_element("table") %>% 
  html_table(fill = TRUE) %>% 
  clean_names() %>% 
  rename(occ_category = occupational_category_click_on_link_below_for_military_crosswalk) %>% 
  mutate(across(where(is.character),
                ~str_squish(str_to_lower(.))))

saveRDS(raw_table, 
        "data/texas-personnel/occ-xwalk.rds")