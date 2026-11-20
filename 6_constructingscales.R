#Preparing the scales
#2026-11-05/sb Mcoglearning

#General function for scoring means, with minimum answered code
score_mean <- function(survey, vars, min_answered) {
  x <- survey[, vars]
  n_ans <- rowSums(!is.na(x))
  out <- rowMeans(x, na.rm = TRUE)
  out[n_ans < min_answered] <- NA
  out
}


#####################
#Achievement motives scale
#######################

hopeforsuccess <- c("QSS34_1","QSS34_2","QSS34_3","QSS34_4","QSS34_5")
fearoffailure <- c("QSS34_6","QSS34_7","QSS34_8","QSS34_9","QSS34_10")

survey$hopeforsuccess <- score_mean(survey, hopeforsuccess, min_answered = 3)
survey$fearoffailure <- score_mean(survey, fearoffailure, min_answered = 3)

###########################
#AEQ-S
###########################

learninganxiety <- c("QSS38_1","QSS38_2","QSS38_3","QSS38_4")
learningboredome <- c("QSS38_5","QSS38_6","QSS38_7","QSS38_8")
learningenjoyment <- c("QSS38_9","QSS38_10","QSS38_11","QSS38_12")
learninghope <- c("QSS38_13","QSS38_14","QSS38_15","QSS38_16")

survey$learninganxiety <- score_mean(survey, learninganxiety, min_answered = 3)
survey$learningboredome <- score_mean(survey, learningboredome, min_answered = 3)
survey$learningenjoyment <- score_mean(survey, learningenjoyment, min_answered = 3)
survey$learninghope <- score_mean(survey, learninghope, min_answered = 3)

###########################
#ITI
###########################

#reversing fixed
survey <- survey %>%
  mutate(across(c(QSS39_1,QSS39_2,QSS39_4,QSS39_6), ~ 11 - .x))

growthmindset <- c("QSS39_1","QSS39_2","QSS39_3","QSS39_4",
                   "QSS39_5","QSS39_6","QSS39_7","QSS39_8")

survey$growthmindset <- score_mean(survey, growthmindset, min_answered = 6)


###########################
#MAI
###########################

metaknowledgeofcognition <- c("QSS42_1","QSS42_2","QSS42_3","QSS42_4","QSS42_5",
                              "QSS42_6","QSS42_7","QSS42_8")
metaregulationofcognition <- c("QSS42_9","QSS42_10","QSS42_11","QSS42_12","QSS42_13",
                              "QSS42_14","QSS42_15","QSS42_16","QSS42_17",
                              "QSS42_18","QSS42_19")

survey$metaknowledgeofcognition <- score_mean(survey, metaknowledgeofcognition, min_answered = 6)
survey$metaregulationofcognition <- score_mean(survey, metaregulationofcognition, min_answered = 8)

###########################
#Individual resource items
###########################

#Reversing so higher better
survey <- survey %>%
  mutate(across(c(QSS3), ~ 6 - .x))
survey <- survey %>%
  mutate(across(c(QSS4), ~ 4 - .x))

survey$financialsatisfaction <- survey$QSS3
survey$cashmargin <- survey$QSS4
survey$numberchildren <- survey$QSS16
survey$numbergrandchildren <- survey$QSS17





