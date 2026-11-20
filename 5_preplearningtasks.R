#Preparing the learning performance outcomes
#2026-08-29/ML for the theorypredicted paper

#######################
#Food-ordering task
#######################

df <- foodordering

#Removing id with data recording after death
df <- df[!(df$id == 156341),]
df <- df[!(df$id == 176722),]


#Requiring complete data
df <- df %>%
  filter(if_all(trial1:trial8, ~ ! is.na(.)))

#Log the trials
df$logtrial1 <- log(df$trial1)*10
df$logtrial2 <- log(df$trial2)*10
df$logtrial3 <- log(df$trial3)*10
df$logtrial4 <- log(df$trial4)*10
df$logtrial5 <- log(df$trial5)*10
df$logtrial6 <- log(df$trial6)*10
df$logtrial7 <- log(df$trial7)*10
df$logtrial8 <- log(df$trial8)*10

#Computing total AUC (with respect to the ground, classic trapezoid formula ) for overall efficiency 
df$auc_seq <- rowSums((df[, paste0("logtrial", 1:7)] + df[, paste0("logtrial", 2:8)]) / 2) 

#Reflect auc_seq so higher is better
df <- df %>%
  mutate(
    auc_seq = -auc_seq
  ) %>%
  mutate(
  auc_seq =
  auc_seq - min (auc_seq, na.rm = TRUE) + 1
)

#Storing back
foodordering <- df %>%
  select(id, auc_seq, trial1:trial8, logtrial1:logtrial8)

########################
#Word learning task
########################

df <- wordlearning

#Removing id with data recorded after death
df <- df[!(df$id == 156341),]
df <- df[!(df$id == 176722),]

#Change from chr to logical
df[, c("T1_I1_correct", "T1_I2_correct", "T1_I3_correct",
       "T1_I4_correct", "T1_I5_correct", "T1_I6_correct",
       "T1_I7_correct", "T1_I8_correct", "T1_I9_correct",
       "T1_I10_correct")] <- lapply(
         df[, c("T1_I1_correct", "T1_I2_correct", "T1_I3_correct",
                "T1_I4_correct", "T1_I5_correct", "T1_I6_correct",
                "T1_I7_correct", "T1_I8_correct", "T1_I9_correct",
                "T1_I10_correct")],
         as.logical
       )
df[, c("T2_I1_correct", "T2_I2_correct", "T2_I3_correct",
       "T2_I4_correct", "T2_I5_correct", "T2_I6_correct",
       "T2_I7_correct", "T2_I8_correct", "T2_I9_correct",
       "T2_I10_correct")] <- lapply(
         df[, c("T2_I1_correct", "T2_I2_correct", "T2_I3_correct",
                "T2_I4_correct", "T2_I5_correct", "T2_I6_correct",
                "T2_I7_correct", "T2_I8_correct", "T2_I9_correct",
                "T2_I10_correct")],
         as.logical
       )
df[, c("T3_I1_correct", "T3_I2_correct", "T3_I3_correct",
       "T3_I4_correct", "T3_I5_correct", "T3_I6_correct",
       "T3_I7_correct", "T3_I8_correct", "T3_I9_correct",
       "T3_I10_correct", "cheated")] <- lapply(
         df[, c("T3_I1_correct", "T3_I2_correct", "T3_I3_correct",
                "T3_I4_correct", "T3_I5_correct", "T3_I6_correct",
                "T3_I7_correct", "T3_I8_correct", "T3_I9_correct",
                "T3_I10_correct","cheated")],
         as.logical
       )

# Sum per trial
items <- c("T1_I1_correct", "T1_I2_correct", "T1_I3_correct",
           "T1_I4_correct", "T1_I5_correct", "T1_I6_correct",
           "T1_I7_correct", "T1_I8_correct", "T1_I9_correct",
           "T1_I10_correct")
df$correctt1 <- ifelse(
  rowSums(is.na(df[items])) > 0,       # if any of the items are NA
  NA,                                    # set total_correct to NA
  rowSums(df[items] * 1, na.rm = TRUE) # otherwise sum TRUE/FALSEs
)

items <- c("T2_I1_correct", "T2_I2_correct", "T2_I3_correct",
           "T2_I4_correct", "T2_I5_correct", "T2_I6_correct",
           "T2_I7_correct", "T2_I8_correct", "T2_I9_correct",
           "T2_I10_correct")
df$correctt2 <- ifelse(
  rowSums(is.na(df[items])) > 0,       # if any of the items are NA
  NA,                                    # set total_correct to NA
  rowSums(df[items] * 1, na.rm = TRUE) # otherwise sum TRUE/FALSEs
)

items <- c("T3_I1_correct", "T3_I2_correct", "T3_I3_correct",
           "T3_I4_correct", "T3_I5_correct", "T3_I6_correct",
           "T3_I7_correct", "T3_I8_correct", "T3_I9_correct",
           "T3_I10_correct")
df$correctt3 <- ifelse(
  rowSums(is.na(df[items])) > 0,       # if any of the items are NA
  NA,                                    # set total_correct to NA
  rowSums(df[items] * 1, na.rm = TRUE) # otherwise sum TRUE/FALSEs
)

#Recode sum scores to NA för cheated = 1.
df$cheated_num <- as.integer(df$cheated)
df$correctt1[df$cheated_num == 1] <- NA
df$correctt2[df$cheated_num == 1] <- NA
df$correctt3[df$cheated_num == 1] <- NA

#Requiring complete data
df <- df %>%
  filter(if_all(correctt1:correctt3, ~ ! is.na(.)))

#Recode extreme decreases to NA, assuming they are signaling giving up 
df$difft2t1 <- df$correctt2 - df$correctt1
df$difft3t1 <- df$correctt3 - df$correctt1
df$difft3t2 <- df$correctt3 - df$correctt2
df[df$difft2t1 < -4 & !is.na(df$difft2t1),
   c("correctt1", "correctt2", "correctt3")] <- NA
df[df$difft3t1 < -4 & !is.na(df$difft3t1),
   c("correctt1", "correctt2", "correctt3")] <- NA
df[df$difft3t2 < -4 & !is.na(df$difft3t2),
   c("correctt1", "correctt2", "correctt3")] <- NA

#Produce AUC scores (assuming a trial0 = 0)
df$auc_glos<-df$correctt1/2 + (df$correctt1+df$correctt2)/2 +
  (df$correctt2+df$correctt3)/2

#Storing
wordlearning <- df %>%
  select(id, auc_glos, correctt1:correctt3, difft2t1, difft3t1, difft3t2, cheated)










