#Preparing data from UGU
#2026-11-05/sb for Mconlearning 


#Selecting variables
ugu1948 <- ugu1948 %>%
  select(id,Cohort, RSSEX,RFSUN4,RMSUN4,RS6CLASS, TS6SSRTP, TS6SSWTP, TS6SMSP,
         TS6SESP, TS6IVOTP, TS6ISTP, TS6IITP,RS6NGSW,RS6NGMA,RS6NGEN)
ugu1953 <- ugu1953 %>%
  select(id,Cohort, RSSEX,QS68,QS69,RS6CLASS, TS6SSRWTP, TS6SSRTP, TS6SSWTP,
         TS6SMSP, TS6SESP, TS6IVOTP, TS6ISTP,TS6IITP,RS6NGSW,RS6NGMA,RS6NGEN)

df <- bind_rows(ugu1948,ugu1953)

#Correcting missing code
df$RFSUN4[df$RFSUN4 == 9] <- NA
df$RMSUN4[df$RMSUN4 == 9] <- NA
df$QS68[df$QS68 == 0] <- NA
df$QS69[df$QS69 == 0] <- NA

#Producing parental education variable
df$pedu1948 <- rowMeans(df[, c("RFSUN4","RMSUN4")], na.rm = TRUE)
df$pedu1948[rowSums(!is.na(df[, c("RFSUN4","RMSUN4")])) == 0] <- NA
df$pedu1953 <- rowMeans(df[, c("QS68","QS69")], na.rm = TRUE)
df$pedu1953[rowSums(!is.na(df[, c("QS68","QS69")])) == 0] <- NA

#Nicer names
df <- df %>%
  rename(opposites1 = TS6IVOTP, metalfolding1 = TS6ISTP, numberseries1 = TS6IITP,
         readingtest = TS6SSRTP, writingtest = TS6SSWTP, mathtest = TS6SMSP, englishtest = TS6SESP,
         swedishtest = TS6SSRWTP, swedishgrade = RS6NGSW,
         mathgrade = RS6NGMA, englishgrade = RS6NGEN)

#Selecting final variables
ugu <- df %>%
  select(id,Cohort,RSSEX,RS6CLASS,pedu1948,pedu1953, opposites1, metalfolding1, numberseries1,
         readingtest,writingtest,swedishtest,englishtest,mathtest,
         swedishgrade,englishgrade,mathgrade)



