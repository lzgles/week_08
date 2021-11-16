################################################################################
# GOAL                                                                         #
################################################################################

# This script will clean the childcare dataset such that it will be useful for analysis

childcare_clean <-
  childcare %>%
  
  # Reorder fields
  select(starts_with("resource"), starts_with("site"), everything(), -count) %>% 
  
  # Fix types
  mutate(resource_id = as.character(resource_id),
         site_county = str_to_title(site_county),
         site_opens_at = as.POSIXct(paste(Sys.Date(), site_opens_at), format = "%Y-%m-%d %I:%M %p"),
         site_closes_at = as.POSIXct(paste(Sys.Date(), site_closes_at), format = "%Y-%m-%d %I:%M %p"),
         phone_number = str_remove_all(phone_number, "[^0-9]")) %>% 
  
  # Fix age_range into two numeric fields
  separate(age_range, into = c("age_range_start", "age_range_end"), sep = " through ") %>% 
  mutate(age_range_start = case_when(
    str_detect(age_range_start, "weeks") ~ as.numeric(str_remove(age_range_start, " .+")) / 52,
    str_detect(age_range_start, "month") ~ as.numeric(str_remove(age_range_start, " .+")) / 12,
    str_detect(age_range_start, "Kindergarten") ~ 5,
    TRUE ~ as.numeric(str_remove(age_range_start, " .+"))
  )) %>% 
  mutate(age_range_end = case_when(
    str_detect(age_range_end, "weeks") ~ as.numeric(str_remove(age_range_end, " .+")) / 52,
    str_detect(age_range_end, "month") ~ as.numeric(str_remove(age_range_end, " .+")) / 12,
    str_detect(age_range_end, "Kindergarten") ~ 5,
    TRUE ~ as.numeric(str_remove(age_range_end, " .+"))
  )) %>% 
  
  # Fix financial_arrangements to one-hot encoding
  separate(financial_arrangements, into = c("fin_1", "fin_2", "fin_3", "fin_4"), sep = ";") %>% 
  pivot_longer(starts_with("fin_"),
               names_to = "financial_arrangement",
               values_to = "program") %>% 
  mutate(financial_arrangement = T,
         program = str_to_lower(program),
         program = str_replace_all(program, " ", "_")) %>% 
  mutate(program = if_else(program == "", "none", program),
         program = if_else(is.na(program), "none", program)) %>%
  distinct() %>% 
  pivot_wider(names_from = program,
              values_from = financial_arrangement,
              names_prefix = "fin_",
              values_fill = F) %>% 
  select(-fin_none) %>% 
  
  # Adjust geocoding
  mutate(geocoded_location = str_remove_all(geocoded_location, "[A-Z\\(\\)]"),
         geocoded_location = str_remove_all(geocoded_location, "^ ")) %>% 
  separate(geocoded_location, into = c("longitude", "latitude"), sep = " ") %>% 
  mutate(across(c(longitude, latitude), as.numeric))