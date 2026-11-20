#Preparing cognition, cohort, and sexd
#2026-11-05/SB for Mconlearning


###########################
#Cleaning opposites
###########################

#Removing id with data recorded after death
opposites <- opposites[!(opposites$id == 176722),]

df <- opposites

#Getting frequencies of missing and adding to file
df <- df %>% mutate(
  verbal_missing_count = rowSums(is.na(select(., starts_with("mCat")))))

#remove ID with missing on more than 35 responses (of 45)
df <- df[!(df$verbal_missing_count > 34),]

#Remove ids with first 20 datapoints missing
df <- df %>%
  filter(!if_all(mCat_1:mCat_20, is.na))

#Computing sum score (NOTE,using all 45, also the last 5 added in UGU-LIFE)
df <- df %>%
  mutate(sumopposites = rowSums(across(mCat_1_correct:mCat_45_correct), na.rm = TRUE))

#Adding variable to indicate data-source
df <- df %>%
  mutate(opposites = TRUE)

#Storing back
opposites <- df


###########################
#Cleaning metal
###########################
df <- metal

#Getting frequencies of missing and adding to file
df <- df %>% mutate(
  spatial_missing_count = rowSums(is.na(select(., starts_with("p_Q")))))

#remove ID with missing on more than 34 responses (of 40)
df <- df[!(df$spatial_missing_count > 34),]

#Remove ids with first 20 datapoints missing
df <- df %>%
  filter(!if_all(p_Q1:p_Q20_, is.na))

#Computing sum score
df <- df %>%
  mutate(summetal = rowSums(across(p_Q1_correct:p_Q40_correct), na.rm = TRUE))

#Adding variable to indicate data-source
df <- df %>%
  mutate(metal = TRUE)

#Storing back
metal <- df

###########################
#Cleaning number 
###########################
df <- number

#Fixing duplicate id:s
#142356 --> NO DATA AT ALL --> FIRST ROW DELETED
df <- df %>% group_by(id) %>% mutate(duporder = if_else(id == 142356, row_number(), NA_integer_)) %>% ungroup()
df <- df[!(df$id == 142356 & df$duporder == 1),]
#152899 --> ONLY COMPLETED ONCE (ended_on = form submission), second obs. empty --> SECOND OBS DELETED
df <- df[!(df$id == 152899 & is.na(df$ended_on)),]
#159438 --> ONLY COMPLETED ONCE (ended_on = form submission), second obs. empty --> SECOND OBS DELETED
df <- df[!(df$id == 159438 & is.na(df$ended_on)),]
#160881 --> ONLY COMPLETED ONCE (ended_on = form submission), second obs. empty --> SECOND OBS DELETED
df <- df[!(df$id == 160881 & is.na(df$ended_on)),]
#180195 --> TWO PARTIALLY COMPLETED. First attempt = 10 items (timestamp = 2025-05-28T14:52:06.021Z),
#According to Jonas notes, first attempt had technical errors
#Second attempt = 30 items (timestamp = 2025-06-02T12:15:15.398Z)  --> FIRST OBS DELETED
df <- df[!(df$id == 180195 & is.na(df$Tal11a)),]
#183215 --> TWO PARTIALLY COMPLETED. First attempt = 34 items (timestamp = 2025-10-15 11:48:40), Second attempt = 35 items (timestamp = 2025-11-15 15:50:29)  --> SECOND OBS DELETED
df <- df[!(df$id == 183215 & is.na(df$Tal33a)),]

#Getting frequencies of missing and adding to file
df <- df %>% mutate(
  number_missing_count = rowSums(is.na(select(., ends_with("correct")))))

#remove ID with missing on more than 34 responses (of 40)
df <- df[!(df$number_missing_count > 34),]

#Remove ids with first 20 datapoints missing (none)
df <- df %>%
  filter(!if_all(Tal01_correct:Tal20_correct, is.na))

#Replacing NA with False
df <- df %>%
  mutate(across(Tal01_correct:Tal40_correct, ~ replace_na(., FALSE)))

#Computing sum score (including item 39, which was not possible to get right, all false)
df <- df %>%
  mutate(sumnumber = rowSums(across(Tal01_correct:Tal40_correct), na.rm = TRUE))

#Adding variable to indicate data source
df <- df %>%
  mutate(number = TRUE)

#Storing back
number <- df


#######################
#Selecting variable
#######################

# Bevara rensade uppgiftsdata för 7_Prepcalibration.R innan variabler väljs bort.
# Originalresponserna finns kvar även när rätt/fel har kodats om för summapoäng.
opposites_items <- opposites
metal_items <- metal
number_items <- number

#Selecting relevant variables from the ugu-life files to merge
metal <- metal %>%
  select(id, summetal, metal)
number <- number %>%
  select(id,sumnumber,number)
opposites <- opposites %>%
  select(id,sumopposites,opposites)


