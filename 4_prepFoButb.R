#Preparing data from FoB
#2026-11-05/sb for Mcoglearning

df <- fob

#Change 9 (ospecificerad) to NA
df$UtbNiva[df$UtbNiva == "9"] <- NA

#Recode ID and select id and education
df <- df %>%
  rename(id = ID)
fob <- df %>%
  select(id,UtbNiva)


