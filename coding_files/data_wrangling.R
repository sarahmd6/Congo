## IMPORT DATA

# install necessary packages
install.packages("readxl")
install.packages("writexl")
library(readxl)
library(writexl)
library(tidyr)

# import using reading from relative path
congo_df_raw <- read_excel("coding_files/raw_data/full_DRC_data.xlsx")
#### PREPARE BASIC FRAME STRUCTURE ####

## pare down to what we need
# check current variable names (e.g., column names)
colnames(congo_df_raw)

# check unique values in columns in question
unique(congo_df_raw$School) # keep
unique(congo_df_raw$Commune) # keep
unique(congo_df_raw$`Neighborhood (Location) of the child's residence`) # keep
unique(congo_df_raw$`Class (at which the child is currently studying in)`) # keep
unique(congo_df_raw$`Sex/Gender`) # keep
unique(congo_df_raw$Concern...32)
unique(congo_df_raw$`Recognition(Word)...58`)

# create new data frame using only variables we want
library(dplyr)
congo_df <- congo_df_raw %>% 
  select(-`Part I. Dicator, Distributive justice game, and Empathy scenarios`, 
         -`Dictator Game:\n\nAltruism`,
         -`Distributive Justice Game:\n\nFairness`,
         -`Empathy task`,
         -`Story 1`,
         -`Story 2`,
         -`Story 3`,
         -`Story 4`,
         -`Story 5`,
         -`Story 6`,
         -`Story 7`,
         -`Story 8`,
         -`Story 9`,
         -`Story 10`,
         -(`PART 2. EXPOSURE TO VIOLENCE: THINGS I SAW AND HEARD`:
             `Once the child has answered the above questions correctly, the investigator should ask each of the following questions and show the circles above to the child for each set of questions.`),
         -(`PART 3. CHILDHOOD DEPRESSION INVENTORY`:
             `Does this child have emotional or behavioral problems for which they need help?`)
  )

colnames(congo_df)

#### CREATE VARIABLES ####

## VIOLENCE ####
# change column names
congo_df1 <- rename(congo_df,
                    heard_gunshots = `1.Have you ever heard gunshots?`,
                    heard_gunshots_freq = `How many times?...68`,
                    
                    seen_beatup = `2. Have you ever seen someone get beaten up?`,
                    seen_beatup_freq = `How many times?...70`,
                    
                    seen_stabbed = `3. Have you ever seen someone get stabbed?`,
                    seen_stabbed_freq = `How many times?...72`,
                    
                    seen_shot = `4. Have you ever seen someone get shot?`,
                    seen_shot_freq = `How many times?...74`,
                    
                    firearm_home = `5. Have you ever seen a firearm in your own home?`,
                    firearm_home_freq = `How many times?...76`,
                    
                    attend_school = `6. Do you go to school?`,
                    unsafe_school_freq = `How many times have your felt unsafe at school?`,
                    
                    unsafe_neighborhood = `7. Do you feel unsafe outside in your neighborhood?`,
                    unsafe_neighborhood_freq = `How often did you feel unsafe in your neighborhood?`,
                    
                    seen_body = `8. Have you ever seen a dead body in your neighborhood?`,
                    seen_body_freq = `How many times?...82`,
                    
                    seen_gangs = `9. Have you ever seen gangs in your neighborhood?`,
                    seen_gangs_freq = `How many times?...84`,
                    
                    seen_fired_firearm = `10. Have you ever seen a gun fired at someone?`,
                    seen_fired_firearm_freq = `How many times?...86`
                    )

colnames(congo_df1) # check


# VE: replace Y/N with 0/1 
congo_df2 <- congo_df1 %>%
  mutate(across(
    c(heard_gunshots, seen_beatup, seen_stabbed, seen_shot, firearm_home, 
                  attend_school, unsafe_neighborhood, seen_body, seen_gangs, seen_fired_firearm),
                ~ dplyr::recode(.,
                        "Yes" = 1, 
                        "Yes/ Ndiyo" = 1,
                        "Sometimes/ Mara kwa mara" = 1,
                        "No" = 0,
                        "No /Hapana" = 0)
    ))

congo_df2

# VF: replace strings with integers

congo_df2 <- congo_df2 %>%
  mutate(across(
    c(heard_gunshots_freq, seen_beatup_freq, seen_stabbed_freq, seen_shot_freq, firearm_home_freq, 
      unsafe_school_freq, unsafe_neighborhood_freq, seen_body_freq, seen_gangs_freq, seen_fired_firearm_freq),
    ~ dplyr::recode(.,
                    "Never" = 0, 
                    "Once" = 1,
                    "Twice" = 2,
                    "Three times" = 3,
                    "Four or more times" = 4)
  ))

congo_df2

# drop NAs of exposure
congo_df3 <- congo_df2 %>%
  drop_na(heard_gunshots, seen_beatup, seen_stabbed, seen_shot, firearm_home, 
          attend_school, unsafe_neighborhood, seen_body, seen_gangs, seen_fired_firearm)

# check if there are columns where exposure = 1 and frequency is NA. CODE TAKEN FROM CHAT GPT.
congo_df3 %>% 
  filter(heard_gunshots == 1 & is.na(heard_gunshots_freq)) %>% 
  nrow()

congo_df3 %>% 
  filter(seen_beatup == 1 & is.na(seen_beatup_freq)) %>% 
  nrow() ## RETURNED 2

congo_df3 %>% 
  filter(seen_stabbed == 1 & is.na(seen_stabbed_freq)) %>% 
  nrow()

congo_df3 %>% 
  filter(seen_shot == 1 & is.na(seen_shot_freq)) %>% 
  nrow()

congo_df3 %>% 
  filter(firearm_home == 1 & is.na(firearm_home_freq)) %>% 
  nrow()

congo_df3 %>% 
  filter(attend_school == 1 & is.na(unsafe_school_freq)) %>% 
  nrow()

congo_df3 %>% 
  filter(unsafe_neighborhood == 1 & is.na(unsafe_neighborhood_freq)) %>% 
  nrow() # RETURNED 1

congo_df3 %>% 
  filter(seen_body == 1 & is.na(seen_body_freq)) %>% 
  nrow()

congo_df3 %>% 
  filter(seen_gangs == 1 & is.na(seen_gangs_freq)) %>% 
  nrow()

congo_df3 %>% 
  filter(seen_fired_firearm == 1 & is.na(seen_fired_firearm_freq)) %>% 
  nrow()

# check seen_beatup & unsafe_neighborhood - CODE TAKEN FROM CHAT GPT
congo_df3 %>% 
  filter(seen_beatup == 1 & is.na(seen_beatup_freq)) # returns tibble: ID 603 & 613 are problematic

congo_df3 %>% 
  filter(unsafe_neighborhood == 1 & is.na(unsafe_neighborhood_freq)) # ID 404

# drop those rows (problematic data)
congo_df3 <- congo_df3 %>% 
  filter(!(ID == 404))

congo_df3 <- congo_df3 %>% 
  filter(!(ID == 603))

congo_df3 <- congo_df3 %>% 
  filter(!(ID == 613))

# turn remaining NAs in Frequency into 0s (that should mean they never saw it, right?)
congo_df3 <- congo_df3 %>%
  mutate(across(
    c(heard_gunshots_freq, seen_beatup_freq, seen_stabbed_freq, seen_shot_freq, firearm_home_freq, 
      unsafe_school_freq, unsafe_neighborhood_freq, seen_body_freq, seen_gangs_freq, seen_fired_firearm_freq),
    ~replace_na(., 0)
  ))
  
# create average frequency per participant
congo_df3 <- congo_df3 %>%
  mutate(vifreq_mean = rowMeans(across(
    c(heard_gunshots_freq, seen_beatup_freq, seen_stabbed_freq, seen_shot_freq, 
      firearm_home_freq, unsafe_school_freq, unsafe_neighborhood_freq, 
      seen_body_freq, seen_gangs_freq, seen_fired_firearm_freq)
  )))



## EMOTION RECOGNITION ####
colnames(congo_df3)

# rename columns
congo_df3 <- rename(congo_df3,
                    s1rv = `S1 Recognition (Valence)`,
                    s2rv = `S2 Recognition (Valence)`,
                    s3rv = `Recognition (Valence)...31`,
                    s4rv = `Recognition (Valence)...35`,
                    s5rv = `Recognition (Valence)...39`,
                    s6rv = `Recognition (Valence)...43`,
                    s7rv = `Recognition (Valence)...47`,
                    s8rv = `Recognition (Valence)...51`,
                    s9rv = `Recognition (Valence)...55`,
                    s10rv = `Recognition (Valence)...59`
                    
)

colnames(congo_df3)

# check unique values
unique(congo_df3$s1rv)
unique(congo_df3$s2rv)
unique(congo_df3$s3rv)
unique(congo_df3$s4rv)
unique(congo_df3$s5rv) # includes NA
unique(congo_df3$s6rv)
unique(congo_df3$s7rv)
unique(congo_df3$s8rv)
unique(congo_df3$s9rv)
unique(congo_df3$s10rv)

# code positive/negative on Recognition Valence for S1 - S10 as Correct/Incorrect
### correct responses taken from answer key

# where NEGATIVE = 1 (TRUE)
congo_df3 <- congo_df3 %>%
  mutate(across(
    c(s1rv, s5rv, s6rv, s8rv, s9rv),
    ~ recode(.,
              "Negative" = 1,
              "Négative" = 1,
              "Positive" = 0)
  ))

# where POSITIVE = 1 (TRUE)
congo_df3 <- congo_df3 %>%
  mutate(across(
    c(s2rv, s3rv, s4rv, s7rv, s10rv),
    ~ recode(.,
             "Negative" = 0,
             "Négative" = 0,
             "Positive" = 1)
  ))

# drop NA
congo_df3 <- congo_df3 |>
  drop_na(s5rv)

# create ER score accuracy rate
congo_df3 <- congo_df3 %>%
  mutate(emorec_accuracy = rowMeans(across(
    c(s1rv, s2rv, s3rv, s4rv, s5rv, s6rv, s7rv, s8rv, s9rv, s10rv)
  )))

## EMPATHIC CONCERN ####
?group_by
