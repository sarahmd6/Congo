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

## VIOLENCE ##
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

congo_df2 <- congo_df1 %>%
  mutate(across(
    c(heard_gunshots, seen_beatup, seen_stabbed, seen_shot, firearm_home, 
                  attend_school, unsafe_neighborhood, seen_body, gangs, seen_fired_firearm),
                ~ dplyr::recode(.,
                        "Yes" = 1, 
                        "Yes/ Ndiyo" = 1,
                        "Sometimes/ Mara kwa mara" = 1,
                        "No" = 0,
                        "No /Hapana" = 0)
    ))

congo_df2 <- congo_df1 %>%
  
  dplyr::mutate(dplyr::across(
    
    c(heard_gunshots, seen_beatup, seen_stabbed, seen_shot, firearm_home,
      
      attend_school, unsafe_neighborhood, seen_body, gangs, seen_fired_firearm),
    
    ~ dplyr::case_when(
      
      . %in% c("Yes", "Yes/ Ndiyo", "Sometimes/ Mara kwa mara") ~ 1,
      
      . %in% c("No", "No /Hapana") ~ 0,
      
      TRUE ~ NA_real_
      
    )
    
  ))

colnames(congo_df1)
