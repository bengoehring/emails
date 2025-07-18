library(readxl)
library(tidyverse)
library(janitor)

# Ben 4-9
# NOTES: Lots of missing reciever addresses -- currently imputing those as
#   first.last@dshs. . . but can do better
# Need to pull in clean names via worksheet alraedy created

# EMAILS # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Notes
# There appears to be a 255 character max for the email fields, which results
#   in some receivers being cut off. 

# get raw data
paths <- list.files("data/texas-emails") %>% 
  str_subset("Jan|March") %>% 
  str_c("data/texas-emails/", .)

all_raw <- map(paths, 
                read_xlsx) %>% 
  bind_rows() %>% 
  clean_names() %>% 
  mutate(across(where(is.character),
                ~str_squish(str_to_lower(.)))) %>% 
  mutate(across(c(email_to,
                  email_cc,
                  email_bcc),
                str_squish)) %>% 
  distinct()  # tons of dupes for unknown reasons

boop <- all_raw %>% 
  filter(email_date_sent == '2020-03-15t18:28:53z')
str_length(boop$email_to)


# Condesning these by to, cc, bcc to match formating. # This can lead to some dupe emails
#   if they are listed as cced/bcced in multiple rows. Dealing with that below after parsing addresses. 

my_max <- function(x) {
  if(all(is.na(x))) {
    return(NA_real_)
  } else {
    max(x, na.rm = TRUE)
  }
}

all_raw2 <- all_raw %>% 
  group_by(email_sender,
           email_date_sent) %>% 
  mutate(dupe = case_when(
    n() > 1 ~ T,
    T ~ F
  )) %>% 
  filter(case_when(
    dupe & !str_detect(email_to, ">$") ~ FALSE,
    dupe & str_length(email_to) < my_max(str_length(email_to)) ~ FALSE,
    dupe & str_length(email_cc) < my_max(str_length(email_cc)) ~ FALSE,
    dupe & str_length(email_bcc) < my_max(str_length(email_bcc)) ~ FALSE,
    dupe & !all(is.na(email_bcc)) & is.na(email_bcc) ~ FALSE,
    TRUE ~ TRUE
  )) %>% 
  # handful of emails left - just picking one
  slice_head(n = 1) %>% 
  ungroup() %>% 
  select(-dupe)

all_raw2 <- all_raw2 %>% 
  mutate(email_id = row_number()) %>% 
  mutate(sent_datetime = ymd_hms(email_date_sent)) %>% 
  select(-email_date_sent)

boop = all_raw2 %>% 
  filter(!str_detect(email_to, ">$"))


# Only 12 instances of no address and all have only one sender
sum(str_detect(all_raw2$email_sender, "@", negate = T), na.rm = TRUE)
sum(str_detect(all_raw2$email_sender, "<.*>", negate = T), na.rm = TRUE)
table(str_count(all_raw2$email_sender, "<.*>"))

sum(str_detect(all_raw2$email_sender, "\\(.*\\)", negate = T), na.rm = TRUE)



# clean up senders # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
clean_sender <- all_raw2 %>% 
  mutate(sender_address = str_extract(email_sender,
                                      "<.*>")) %>% 
  mutate(sender_address = str_remove_all(sender_address, 
                                         "\\<|\\>")) %>% 
  mutate(sender_last_name = case_when(
    str_detect(sender_address,
               "^\\w+\\.\\w+\\@") ~ str_remove_all(str_extract(sender_address,
                                                           "\\..*(?=@)"),
                                                   "^\\."),
    str_detect(email_sender, ',') ~ str_extract(email_sender, "^.*(?=,)"),
    TRUE ~ NA_character_
  )) %>% 
  mutate(sender_first_name = case_when(
    str_detect(sender_address,
               "^\\w+\\.\\w+\\@") ~ str_extract(sender_address,
                                                "^.*?(?=\\.)"),
    str_detect(email_sender, 
               ',') & str_detect(email_sender, 
                                 '\\s\\(') ~ str_remove(str_extract(email_sender, 
                                                                    ",.*(?=\\s\\()"),
                                                        "^,"),
    str_detect(email_sender, 
               ',') ~ str_remove(str_extract(email_sender, 
                                             ",.*"),
                                 "^,"),
    TRUE ~ NA_character_
  )) %>% 
  mutate(sender_mi = str_squish(str_extract(sender_first_name, 
                                            "\\s\\w$"))) %>% 
  mutate(sender_first_name = str_remove(sender_first_name,
                                        "\\s\\w$")) %>% 
  mutate(across(c(sender_first_name,
                  sender_last_name),
                ~str_remove_all(., 
                                "[:punct:]"))) %>% 
  select(-(email_to:email_bcc))


# clean up receivers # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# > 1 receivers possible but differentiated by semi colons
# spinning it longer by semi colon and cleaning before merging it back in later

raw_to <- all_raw2 %>% 
  select(email_id, 
         email_to) %>% 
  mutate(receiver_type = 'to')

raw_cc <- all_raw2 %>% 
  select(email_id, 
         email_to = email_cc) %>% 
  mutate(receiver_type = 'cc')

raw_bc <- all_raw2 %>% 
  select(email_id, 
         email_to = email_bcc) %>% 
  mutate(receiver_type = 'bcc')

raw_receivers <- bind_rows(raw_to,
                           raw_cc,
                           raw_bc) %>% 
  # not dropping yet because I want them to show up as NA, not be dropped
  mutate(email_to = case_when(
    str_detect(email_to,
               "undisclosed recipient") ~ str_remove(email_to,
                                                      ";"),
    TRUE ~ email_to
  )) %>% 
  separate_longer_delim(email_to,
                        ";") %>% 
  filter(!is.na(email_to)) %>%  # dropping cases where no receiver
  mutate(email_to = case_when(
    str_detect(email_to,
               "undisclosed recipient") ~ NA_character_,
    TRUE ~ email_to
  ))


clean_receivers <- raw_receivers %>% 
  mutate(receiver_address = str_extract(email_to,
                                        "<.*>")) %>% 
  mutate(receiver_address = str_remove_all(receiver_address, 
                                           "\\<|\\>")) %>% 
  mutate(receiver_last_name = case_when(
    str_detect(receiver_address,
               "^\\w+\\.\\w+\\@") ~ str_remove(str_extract(receiver_address,
                                                           "\\..*(?=@)"),
                                               "^\\."),
    str_detect(email_to, ',') ~ str_extract(email_to, "^.*(?=,)"),
    TRUE ~ NA_character_
  )) %>% 
  mutate(receiver_first_name = case_when(
    str_detect(receiver_address,
               "^\\w+\\.\\w+\\@") ~ str_extract(receiver_address,
                                                "^.*?(?=\\.)"),
    str_detect(email_to, 
               ',') & str_detect(email_to, 
                                 '\\s\\(') ~ str_remove(str_extract(email_to, 
                                                                    ",.*(?=\\s\\()"),
                                                        "^,"),
    str_detect(email_to, 
               ',') ~ str_remove(str_extract(email_to, 
                                             ",.*"),
                                 "^,"),
    TRUE ~ NA_character_
  )) %>% 
  mutate(receiver_mi = str_squish(str_extract(receiver_first_name, 
                                            "\\s\\w$"))) %>% 
  mutate(receiver_first_name = str_remove(receiver_first_name,
                                        "\\s\\w$")) %>% 
  mutate(across(c(receiver_first_name,
                  receiver_last_name),
                ~str_remove_all(., 
                               "[:punct:]")))



# Merge together the receiver and sender info and clean it up # # # # # # # # # #
all_clean <- left_join(clean_receivers,
                       clean_sender,
                       by = c('email_id')) 
all_clean_misses <- anti_join(clean_receivers,
                              clean_sender,
                              by = c('email_id')) 
stopifnot(nrow(all_clean_misses) == 0)


all_clean <- all_clean %>% 
  distinct() %>% 
  group_by(email_id, 
           receiver_type) %>% 
  mutate(obs_id = str_c(email_id,
                        "_",
                        receiver_type,
                        "_",
                        row_number())) %>% 
  ungroup() %>% 
  select(obs_id,
         email_id, 
         sent_datetime,
         starts_with("sender_"),
         starts_with("receiver"),
         raw_receivers = email_to,
         raw_sender = email_sender) %>% 
  mutate(across(c(receiver_first_name,
                  receiver_last_name,
                  sender_first_name,
                  sender_last_name,
                  sender_mi,
                  receiver_mi),
                ~str_remove_all(.,
                     "[:punct:]|\\s"))) %>% 
  mutate(across(c(receiver_last_name, 
                  sender_last_name),
                ~str_remove_all(.,
                                "\\d"))) %>%  # some remaining numbers from parsing
  mutate(across(c(receiver_last_name, 
                  sender_last_name,
                  receiver_first_name,
                  sender_first_name),
                ~str_remove_all(., 
                                'dshs'))) # some remaining dept acronyms from regex parsing


# handful of instances of emails not in the given dates ~ dropping 668
all_clean <- all_clean %>% 
  filter(case_when(
    floor_date(sent_datetime, 'days') %within% interval(ymd('2020-01-03'),
                                                        ymd('2020-01-10')) ~ TRUE,
    floor_date(sent_datetime, 'days') %within% interval(ymd('2020-03-13'),
                                                        ymd('2020-03-20')) ~ TRUE,
    TRUE ~ FALSE
  )) %>% 
  mutate(snapshot_month = case_when(
    month(sent_datetime) == 1 ~ 'jan',
    month(sent_datetime) == 3 ~ 'mar'
  )) %>% 
  mutate(across(c(sender_address, 
                  receiver_address),
                ~str_replace(.,
                            '@dshs.letmefixit.net$|@dshs.state.tx.us$|@dshs.texas.go$|@dshs.gov$|@dshs.state.gov$|@dshs.teaxs.gov$|@dshs.texas.com$|@dshs.texas.edu$|@dshs.texas.giv$|@dshs.texas.ov$|@dshs.texas.us$|@dshs.texs.gov$|@dshs.texss.gov$|@dshs.texas.gov.$',
                            '@dshs.texas.gov')
  ))

stopifnot(sum(duplicated(all_clean$obs_id)) == 0)


str_extract(all_clean$receiver_address, "@.*") %>% 
  table()
sum(is.na(all_clean$receiver_address))



# XWALK NAMES # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Running into issues where a large chunk of names are not matching with personnel
#   Therefore, if match exists, pulling in names from the cleaned up xwalk file. 

name_xwalk <- read_rds("data/texas-emails/DSHSEmails-clean.rds") %>% 
  mutate(across(first_name:middle_initial,
                ~str_squish(str_remove_all(., "\\s")))) %>%  
  rename_with(~str_c("xwalk_", .)) 

# first for senders # # # # # ## # # # # # 
senders <- all_clean %>% 
  select(obs_id:sender_mi)

senders_xwalk <- left_join(senders, 
                           name_xwalk,
                           by = c("sender_address" = "xwalk_email_addr"))
stopifnot(nrow(senders_xwalk) == nrow(senders))
senders_xwalk_misses <- anti_join(senders, 
                                  name_xwalk,
                                  by = c("sender_address" = "xwalk_email_addr"))

senders_xwalk <- senders_xwalk %>% 
  mutate(sender_first_name = case_when(
    !is.na(xwalk_first_name) ~ xwalk_first_name,
    TRUE ~ sender_first_name
  )) %>% 
  mutate(sender_last_name = case_when(
    !is.na(xwalk_last_name) ~ xwalk_last_name,
    TRUE ~ sender_last_name
  )) %>% 
  mutate(sender_mi = case_when(
    !is.na(xwalk_middle_initial) ~ xwalk_middle_initial,
    TRUE ~ sender_mi
  )) %>% 
  select(-xwalk_first_name,
         -xwalk_last_name,
         -xwalk_middle_initial)

# second for receivers # # # # # # # # # # 
receivers <- all_clean %>% 
  select(obs_id:sent_datetime,
         receiver_type:snapshot_month)

receivers_xwalk <- left_join(receivers, 
                             name_xwalk,
                             by = c("receiver_address" = "xwalk_email_addr"))
stopifnot(nrow(receivers_xwalk) == nrow(receivers))

receivers_xwalk_misses <- anti_join(receivers, 
                                    name_xwalk,
                                    by = c("receiver_address" = "xwalk_email_addr"))

receivers_xwalk <- receivers_xwalk %>% 
  mutate(receiver_first_name = case_when(
    !is.na(xwalk_first_name) ~ xwalk_first_name,
    TRUE ~ receiver_first_name
  )) %>% 
  mutate(receiver_last_name = case_when(
    !is.na(xwalk_last_name) ~ xwalk_last_name,
    TRUE ~ receiver_last_name
  )) %>% 
  mutate(receiver_mi = case_when(
    !is.na(xwalk_middle_initial) ~ xwalk_middle_initial,
    TRUE ~ receiver_mi
  )) %>% 
  select(-xwalk_first_name,
         -xwalk_last_name,
         -xwalk_middle_initial) %>% 
  mutate(receiver_address = str_remove_all(receiver_address,
                                           "-"))


# pull both together 
all_clean_xwalk <- left_join(senders_xwalk,
                             receivers_xwalk,
                             by = join_by(obs_id, 
                                          email_id, 
                                          sent_datetime))
stopifnot(nrow(all_clean_xwalk) == nrow(all_clean))


# Lots of missing receiver variables - often due to parsing issues. Creating
#   addresses via the standardized formats

# also doing some more one off changes of names that follow inconsistent formats due to parsing
all_clean_xwalk <- all_clean_xwalk %>% 
  select(-sender_mi,
         -receiver_mi) %>% 
  mutate(receiver_address = case_when(
    is.na(receiver_address) ~ str_c(receiver_first_name,
                                    ".",
                                    receiver_last_name,
                                    "@dshs.texas.gov"),
    TRUE ~ receiver_address
  )) %>% 
  mutate(non_standard_receiver = case_when(
    str_detect(receiver_address, 
               "^\\w+\\.\\w+@dshs.texas.gov",
               negate = T) ~ TRUE, 
    str_detect(receiver_address,
               "^hsr\\d|network|budget|account|voapps|^\\d|^region|helpdesk|noreply|xerox") ~ TRUE,
    TRUE ~ FALSE
  )) %>% 
  mutate(non_standard_sender = case_when(
    str_detect(sender_address, 
               "^\\w+\\.\\w+@dshs.texas.gov",
               negate = T) ~ TRUE, 
    str_detect(sender_address,
               "^hsr\\d|network|budget|account|voapps|^\\d|^region|helpdesk|noreply|xerox") ~ TRUE,
    TRUE ~ FALSE
  ))

# addtional changes - these are people whose names are parsed differently in the email data due
#   to different parsings. One of the parsing formats is in the personnel data, meaning that they
#   are not flagged in the personnel names to change file below.
all_clean_xwalk <- all_clean_xwalk %>% 
  mutate(receiver_first_name = case_when(
    receiver_last_name == 'tenorio' & receiver_address == 'maryalice.tenorio@dshs.texas.gov' ~ 'maryalice',
    receiver_address == 'rosario.rocha@dshs.texas.gov' ~ 'maria',
    receiver_address %in% c('.portales@dshs.texas.gov',
                            'ni.portales@dshs.texas.gov',
                            'nicoletten.portales@dshs.texas.gov') ~ 'nicolette',
    TRUE ~ receiver_first_name
  )) %>% 
  mutate(sender_first_name = case_when(
    sender_last_name == 'tenorio' & sender_address == 'maryalice.tenorio@dshs.texas.gov' ~ 'maryalice',
    sender_address == 'rosario.rocha@dshs.texas.gov' ~ 'mariadelrosario',
    sender_address %in% c('.portales@dshs.texas.gov',
                          'ni.portales@dshs.texas.gov',
                          'nicoletten.portales@dshs.texas.gov') ~ 'nicolette',
    TRUE ~ sender_first_name
  )) %>% 
  mutate(across(c(receiver_address,
                  sender_address),
                ~case_when(
    . == 'mariadelrosario.rocha@dshs.texas.gov' ~ 'rosario.rocha@dshs.texas.gov',
    TRUE ~ .
  ))) %>% 
  mutate(receiver_last_name = case_when(
    receiver_address == 'malanne.allen@dshs.texas.gov' ~ 'allen',
    receiver_address %in% c('.portales@dshs.texas.gov',
                            'ni.portales@dshs.texas.gov',
                            'nicoletten.portales@dshs.texas.gov') ~ 'portales',
    TRUE ~ receiver_last_name
  )) %>% 
  mutate(sender_last_name = case_when(
    sender_address == 'malanne.allen@dshs.texas.gov' ~ 'allen',
    sender_address %in% c('.portales@dshs.texas.gov',
                          'ni.portales@dshs.texas.gov',
                          'nicoletten.portales@dshs.texas.gov') ~ 'portales',
    TRUE ~ sender_last_name
  ))



# Because of how messy it is to parse emails, duplicates can emerge among the senders
# drop dupes that have less info in the raw receiver text
# also some instances of people being sent and cced on emails
all_clean_xwalk_2 <- all_clean_xwalk %>% 
  group_by(email_id,
           receiver_address) %>%
  mutate(receiver_dupe = case_when(
    n() > 1 ~ T,
    T ~ F
   )) %>% 
  mutate(unique_rec_types = list(unique(receiver_type))) %>% 
  filter(case_when(
    receiver_dupe & str_length(raw_receivers) < max(str_length(raw_receivers)) ~ FALSE,
    TRUE ~ TRUE
  )) %>% 
  ungroup()
  
all_clean_xwalk_2 <- all_clean_xwalk_2 %>% 
  mutate(cc_to = map_lgl(unique_rec_types,
                         ~setequal(., 
                                   c('cc', 'to')))) %>% 
  mutate(bcc_to = map_lgl(unique_rec_types,
                         ~setequal(., 
                                   c('bcc', 'to')))) %>% 
  mutate(cc_bcc = map_lgl(unique_rec_types,
                         ~setequal(., 
                                   c('cc', 'bcc')))) %>% 
  filter(case_when(
      receiver_dupe & cc_to & receiver_type == 'cc' ~ FALSE,
      receiver_dupe & bcc_to & receiver_type == 'bcc' ~ FALSE,
      receiver_dupe & cc_bcc & receiver_type == 'bcc' ~ FALSE,
      TRUE ~ TRUE
  )) %>% 
  select(-receiver_dupe)


# PERSONNEL DATA # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

raw_personnel <- map(c("data/texas-personnel/GOEHRING.F20HALF1_FINAL.xlsx",
                       "data/texas-personnel/GOEHRING.F20HALF2_FINAL.xlsx"),
                     ~read_xlsx(., 
                                na = c("", "."))) %>% 
  bind_rows()

clean_personnel <- raw_personnel %>%
  clean_names() %>% 
  select(-(org_code:location_name)) %>% 
  mutate(across(where(is.character),
                ~str_squish(str_to_lower(.)))) %>% 
  mutate(last_name = str_remove(last_name, 
                           "\\sjr$|\\ssr$|\\siii$|\\siv$|\\sii$")) %>% 
  filter(agency_name == "department of state health services") %>% 
  filter(month == ymd('2020-01-31') | month == ymd('2020-03-31')) %>% 
  mutate(across(c(first_name, 
                  last_name,
                  mi),
         ~str_remove_all(.,
                         "[:punct:]|\\s"))) %>% 
  mutate(snapshot_month = case_when(
    month == ymd('2020-01-31') ~ 'jan',
    month == ymd('2020-03-31') ~ 'mar',
  )) %>% 
  select(-agency,
         -agency_name) %>% 
  rename(employee_id = statenum) %>% 
  group_by(first_name,
           last_name,
           snapshot_month) %>% 
  # flag dupes
  mutate(dupe = case_when(
    n() > 1 ~ T,
    T ~ F
  )) %>% 
  mutate(last_name = case_when(
    n() > 1 ~ str_c(last_name, "_", str_replace_na(mi)),
    TRUE ~ last_name
  )) %>% 
  ungroup()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# MERGE # # # # # # # # # # # # # # # # # # # # # 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Before merging fix names in personnel data that are not in email data. 
# Merge is done first using non-dupe names then merged using the handful of
#   dupe first and last names in the personnel data via email addresses


# find the first and last names that are in the personnel dataset but not 
#   in the email dataset and clean by hand. 
all_names_merge_1 <- all_clean_xwalk_2 %>% 
  select(first_name = sender_first_name,
         last_name = sender_last_name,
         snapshot_month)
all_names_merge_2 <- all_clean_xwalk_2 %>% 
  select(first_name = receiver_first_name,
         last_name = receiver_last_name,
         snapshot_month)

all_names_merge <- bind_rows(all_names_merge_1,
                             all_names_merge_2) %>% 
  distinct()

names_to_clean <- anti_join(clean_personnel,
                            all_names_merge,
                            by = c('first_name',
                                   'last_name')) %>% 
  select(first_name, 
         last_name,
         class_title) %>% 
  distinct()

write_csv(names_to_clean, 
          "data/texas-emails/names-to-fix-RAW.csv")


# pulling back in the fixed names # # # # # # #
fixed_names <- read_csv("data/texas-emails/names-to-fix-CLEAN.csv",
                               skip = 3) %>% 
  filter(!is.na(new_first_name) & !is.na(new_last_name)) %>% 
  select(first_name,
         last_name,
         new_first_name,
         new_last_name) %>% 
  distinct()

check <- fixed_names %>% 
  group_by(first_name,
           last_name) %>% 
  filter(n() > 1)
stopifnot(nrow(check) == 0)


# and now remerge using the fixed names
clean_personnel_fixed <- left_join(clean_personnel,
                                   fixed_names,
                                   by = join_by(last_name, 
                                                first_name)) %>% 
  mutate(first_name = case_when(
    !is.na(new_first_name) ~ new_first_name,
    TRUE ~ first_name
  )) %>% 
  mutate(last_name = case_when(
    !is.na(new_last_name) ~ new_last_name,
    TRUE ~ last_name
  )) %>% 
  select(-new_first_name,
         -new_last_name)


names_to_clean2 <- anti_join(all_names_merge,
                             clean_personnel_fixed,
                             by = c('first_name',
                                    'last_name')) %>% 
  select(first_name, 
         last_name) %>% 
  distinct()


# Extract the duplicate names from the personnel file. 
dupe_personnel <- clean_personnel_fixed %>% 
  filter(dupe) %>% 
  mutate(address = case_when(
    last_name == 'hernandez_g' ~ 'brenda.hernandez@dshs.texas.gov',
    last_name == 'hernandez_m' ~ 'brendam.hernandez@dshs.texas.gov',
    last_name == 'bailey_l' ~ 'kathy.bailey@dshs.texas.gov',
    last_name == 'bailey_NA' ~ 'katherine.bailey@dshs.texas.gov',
    last_name == 'cantu_a' ~ 'rosa.cantu@dshs.texas.gov',
    last_name == 'cantu_m' ~ 'rosa.fuentes@dshs.texas.gov',
    last_name == 'garcia_i' & str_detect(class_title, 'director') ~ 'imeldam.garcia@dshs.texas.gov',
    last_name == 'garcia_i' & str_detect(class_title, 'public hlth') ~ 'maria.garcia@dshs.texas.gov',
    last_name == 'garcia_c' & first_name == 'maria' ~ 'connie.garcia@dshs.texas.gov',
    last_name == 'garcia_c' & first_name == 'david' ~ 'davidc.garcia@dshs.texas.gov',
    last_name == 'garcia_e' ~ 'david.garcia@dshs.texas.gov',
    last_name == 'garza_a' ~ 'belindaa.garza@dshs.texas.gov',
    last_name == 'garza_j' ~ 'belinda.garza@dshs.texas.gov',
    last_name == 'guerrero_c' ~ 'chris.guerrero@dshs.texas.gov',
    last_name == 'guerrero_r' ~ 'anna.guerrero@dshs.texas.gov',
    last_name == 'hernandez_g' ~ 'brenda.hernandez@dshs.texas.gov',
    last_name == 'hernandez_m' ~ 'brendam.hernandez@dshs.texas.gov',
    last_name == 'johnson_l' & str_detect(class_title, 'sanitarian') ~ 'nikki.johnson@dshs.texas.gov',
    last_name == 'johnson_l' & str_detect(class_title, 'microbio') ~ 'nicole.johnson@dshs.texas.gov',
    last_name == 'jones_m' & str_detect(class_title, 'laboratory technician') ~ 'shannonm.jones@dshs.texas.gov',
    last_name == 'jones_m' & str_detect(class_title, 'program specialist iii')~ 'shannon.jones@dshs.texas.gov',
    last_name == 'lopez_i' ~ 'sylviai.lopez@dshs.texas.gov',
    last_name == 'lopez_j' ~ 'sylviaj.lopez@dshs.texas.gov',
    last_name == 'martinez_a' ~ 'kathie.martinez@dshs.texas.gov',
    last_name == 'martinez_l' ~ 'kathy.martinez@dshs.texas.gov',
    last_name == 'martinez_m' ~ 'davidm.martinez@dshs.texas.gov',
    last_name == 'martinez_r' ~ 'davidr.martinez@dshs.texas.gov',
    last_name == 'patel_a' ~ 'milan.patel2@dshs.texas.gov',
    last_name == 'patel_r' ~ 'milan.patel@dshs.texas.gov',
    last_name == 'rodriguez_i' ~ 'elena.rodriguez@dshs.texas.gov',
    last_name == 'rodriguez_o' ~ 'elenao.rodriguez@dshs.texas.gov',
    last_name == 'zavala_a' & str_detect(class_title, 'research') ~ 'robert.zavala@dshs.texas.gov', # hired first
    last_name == 'zavala_a' & str_detect(class_title, 'systems') ~ 'robert.zavala1@dshs.texas.gov',
    last_name == 'zhang_f' ~ 'jenny.zhang@dshs.texas.gov',
    last_name == 'zhang_x' ~ 'jenny.zhang1@dshs.texas.gov',
  )) %>% 
  select(-dupe)

clean_personnel_fixed <- clean_personnel_fixed %>% 
  filter(!dupe) %>% 
  select(-dupe)

# function to match personnel to emails # # # # # # # # #
run_merge <- function(in_type = c('sender',
                                  'receiver'),
                      in_merge_type = 'matches',
                      in_personnel_data = clean_personnel_fixed,
                      in_personnel_data_dupe = dupe_personnel,
                      in_email_data = all_clean_xwalk_2) {
  
  # subset the email data to senders or receivers
  in_email_data_sub <- in_email_data %>% 
    select(obs_id, 
           email_id, 
           sent_datetime,
           snapshot_month,
           contains(in_type))

  # merge variables
  in_first_name = str_glue("{in_type}_first_name")
  in_last_name = str_glue("{in_type}_last_name")
  in_address = str_glue('{in_type}_address')
  
  # add type to personnel data
  in_personnel_data <- in_personnel_data %>% 
    rename_with(~str_c(in_type, "_", .)) %>% 
    rename(snapshot_month = contains("snapshot_month"))
  
  in_personnel_data_dupe <- in_personnel_data_dupe %>% 
    select(-first_name,
           -last_name) %>% 
    rename_with(~str_c(in_type, "_", .)) %>% 
    rename(snapshot_month = contains("snapshot_month"))
  
    
  # Match on unique first/last names
  out <- inner_join(in_email_data_sub,
                   in_personnel_data,
                   by = join_by({{in_first_name}},
                                {{in_last_name}},
                                snapshot_month))
  
  out_misses <- anti_join(in_email_data_sub,
                          in_personnel_data,
                          by = join_by({{in_first_name}},
                                       {{in_last_name}},
                                       snapshot_month))
  
  stopifnot(sum(nrow(out), nrow(out_misses)) == nrow(in_email_data_sub))

  
  # Now match on duped names
  in_personnel_data_dupe <- in_personnel_data_dupe %>% 
    select(-contains('first_name')) %>% 
    select(-contains('last_name'))
  
  out_2 <- left_join(out_misses, 
                     in_personnel_data_dupe,
                     by = join_by({{in_address}},
                                  snapshot_month))
  
  # and concatenate the multiple merges
  all_out <- bind_rows(out,
                       out_2)
  stopifnot(nrow(all_out) == nrow(in_email_data_sub))
  
  
  # return hits or misses
  if(in_merge_type == 'matches') {

    all_out_sub <- all_out %>% 
      filter(!!sym(str_c('non_standard_', in_type)) == FALSE)
    
    cat("Share of emails matched to personnel information:",
        sum(!is.na(all_out[[str_c(in_type, "_", "employee_id")]])) / nrow(all_out) * 100)
    
    cat("\n\nShare of emails matched to personnel information (minus non standard emails:",
        sum(!is.na(all_out_sub[[str_c(in_type, "_", "employee_id")]])) / nrow(all_out_sub) * 100)
    
    return(all_out)
    
  } else {
    
    misses <- all_out %>% 
      filter(is.na(!!sym(str_c(in_type, "_", 'employee_id')))) %>% 
      mutate(type = in_type)
    
    return(misses)
  }
}

merged_senders <- run_merge('sender')

merged_receivers <- run_merge('receiver')

merged_senders_misses <- run_merge('sender',
                                   'misses') 
merged_receivers_misses <- run_merge('receiver',
                                     'misses')

boop = merged_receivers_misses %>%
  filter(!non_standard_receiver) %>% 
  group_by(receiver_address) %>% 
  summarise(n = n())

boop2 = merged_senders_misses %>%
  filter(!non_standard_sender) %>% 
  group_by(sender_address) %>% 
  summarise(n = n())


merged_all <- left_join(merged_receivers,
                        merged_senders,
                        by = join_by(obs_id,
                                     email_id, 
                                     sent_datetime, 
                                     snapshot_month))

stopifnot(nrow(merged_all) == nrow(merged_receivers))
stopifnot(nrow(merged_all) == nrow(merged_senders))
stopifnot(nrow(merged_all) == nrow(all_clean_xwalk_2))


stopifnot(duplicated(merged_all$obs_id) == 0)

check_dupes <- merged_all %>% 
  group_by(email_id) %>% 
  filter(n_distinct(receiver_address) != n()) %>% 
  ungroup()
stopifnot(nrow(check_dupes) == 0)



# share of emails where we have info on both sender and receiver
n_obs_matched <- merged_all %>% 
  filter(!is.na(receiver_employee_id) & !is.na(sender_employee_id)) %>% 
  nrow()
n_obs_matched / nrow(merged_all)



# final cleaning and reorganizing
merged_all_2 <- merged_all %>% 
  select(-snapshot_month,
         -sender_month,
         -receiver_month,
         -receiver_mi,
         -sender_mi) %>% 
  select(obs_id,
         email_id,
         sender_employee_id,
         receiver_employee_id,
         everything())



# Add a response time variable # # # # # # # # # # # # # # # # # # # # # # # # 
#   Only defined for "to" messages

resp_data <- merged_all_2 %>% 
  filter(receiver_type == 'to') %>% 
  select(obs_id:sent_datetime, 
         sender_address,
         receiver_address) %>% 
  mutate(month = month(sent_datetime))

resp_data_2 <- merged_all_2 %>% 
  filter(receiver_type == 'to') %>% 
  select(obs_id:sent_datetime, 
         sender_address,
         receiver_address) %>% 
  mutate(month = month(sent_datetime))

# merge senders to receivers and vice versa and then find the first 
#   response.
resp_merged <- left_join(resp_data,
                         resp_data_2,
                         by = join_by(sender_address == receiver_address,
                                      receiver_address == sender_address,
                                      month),
                         relationship = 'many-to-many') %>% 
  filter(sent_datetime.y > sent_datetime.x) %>% 
  group_by(obs_id.x) %>% 
  slice_min(sent_datetime.y,
            with_ties = FALSE) %>% # need to dig into this more
  ungroup()

resp_merged2 <- resp_merged %>% 
  mutate(resp_time_min = interval(sent_datetime.x,
                                  sent_datetime.y) %/% minutes()) %>% 
  select(obs_id = obs_id.x,
         email_id = email_id.x,
         resp_time_min)


merged_all_3 <- left_join(merged_all_2,
                                  resp_merged2,
                                  by = join_by(obs_id, email_id))


# merge in occupational categories # # # # # # # # # # # # # # #
data_occ <- merged_all_3 %>% 
  mutate(across(c(receiver_class_code,
                  sender_class_code),
                ~case_when(
                  . == 'c537' ~ '1626', # commissioner -- not listed in xwalk -- using director code
                  . == '5529' ~ '1413',
                  . == '1631' ~ '1622',  # picking generic program manage code
                  . == '0332' ~ '0181',
                  TRUE ~ .
                  ))) %>% 
  mutate(across(c(receiver_class_code,
                  sender_class_code),
                as.numeric))

occ_xwalk <- read_rds("data/texas-personnel/occ-xwalk.rds") %>% 
  select(class_code,
         occ_category)

data_occ_sender <- left_join(data_occ,
                             rename(occ_xwalk, 
                                    sender_occ_category = occ_category),
                             by = join_by(sender_class_code == class_code))
data_occ_sender_misses <- anti_join(data_occ,
                                    rename(occ_xwalk, 
                                           sender_occ_category = occ_category),
                                    by = join_by(sender_class_code == class_code))

data_occ_receiver <- left_join(data_occ_sender,
                               rename(occ_xwalk, 
                                      receiver_occ_category = occ_category),
                               by = join_by(receiver_class_code == class_code))
data_occ_receiver_misses <- anti_join(data_occ_sender,
                                      rename(occ_xwalk, 
                                             receiver_occ_category = occ_category),
                                      by = join_by(receiver_class_code == class_code))

data_occ_sender_misses %>% 
  select(sender_class_title,
         sender_class_code) %>% 
  distinct()

data_occ_receiver_misses %>% 
  select(receiver_class_title,
         receiver_class_code) %>% 
  distinct()

stopifnot(nrow(data_occ_receiver) == nrow(merged_all_3))


# save it # # # # # # # # # 
  
saveRDS(data_occ_receiver, 
        "data/cleaned-tx.rds")

