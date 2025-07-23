# This file cleans up the DSHS emails crosswalk file 

library(tidyverse)
library(readxl)
library(janitor)

# pull in names to emails crosswalk # # # # # # # # # # # # # # #
raw_xwalk <- read_xlsx("data/texas-emails/DSHSEmails.xlsx") %>% 
  clean_names() %>% 
  mutate(across(everything(),
                ~str_to_lower(str_squish(.))))

# parse the names # # # # # # # # # # # # # # #

# first, suffixes screw up later merges between emails and personnel files, so removing. 
names_xwalk <- raw_xwalk %>% 
  mutate(name = str_remove(name, 
                           "\\sjr|\\ssr|\\siii|\\siv|\\sii")) %>% 
  mutate(name = str_remove_all(name, 
                                     "[[:punct:]&&[^,]]")) %>% 
  mutate(last_name = str_extract(name, 
                                 "^.*(?=,)")) %>% 
  mutate(first_name = case_when(
    str_detect(name,
               ",\\w+$") ~ str_extract(name,
                                       "(?<=,).*"),
    TRUE ~ str_squish(str_extract(name,
                                  "(?<=,)\\w+\\s"))
  )) %>% 
  mutate(middle_initial = str_extract(name, 
                                      "(?<=,).*")) %>% 
  mutate(middle_initial = str_squish(str_extract(middle_initial,
                                      "\\s\\w"))) %>% 
  select(-name) %>% 
  select(first_name,
         last_name,
         middle_initial,
         email_addr) %>% 
  mutate() %>% 
  mutate(across(c(first_name,
                  last_name,
                  middle_initial),
                ~str_remove_all(.,
                                "[:punct:]|\\s")))


# one offs that make life easier
names_xwalk <- names_xwalk %>% 
  mutate(last_name = case_when(
    last_name == 'almendarezzuniga' ~ 'almendarez',
    TRUE ~ last_name
  )) %>% 
  mutate(first_name = case_when(
    last_name == 'marichalar' ~ "michelle",
    last_name == 'cisneros' ~ "rhae",
    last_name == 'sambou' ~ 'lalamariam',
    TRUE ~ first_name
  )) %>% 
  mutate(email_addr = str_remove_all(email_addr,
                                     "-"))

# Save # # # # # # # # # # # # # # #
saveRDS(names_xwalk,
        "data/texas-emails/DSHSEmails-clean.rds")         
         

  
         
         
         
         
         