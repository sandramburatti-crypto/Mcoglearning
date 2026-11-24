# PAPER 3: PRELIMINARA MINDSET-SKALOR
# Kor 1_readingdata.R forst. Kan koras oavsett om originalskript 6 har korts.
# Originalfil 6 reverserar QSS39_1, 2, 4 och 6 med 11 - svar och lagger
# sedan ihop alla atta item till ett growth-orienterat medelvarde. Har
# behaller vi de fyra i ursprunglig riktning som entity och anvander de
# fyra ovriga som incremental. Hogre varde = mer av respektive tro.
# Svarsskala 1-10 foljer den avsedda reverseringen; min 3 av 4 ar vart
# analysval (originalfilens grans ar 6 av 8 for sammanslagen skala).
# QSS39 lases pa nytt ur originalfilen: itemen i survey kan redan vara
# reverserade utan att det paverkar resultatet. Ursprungliga item andras inte.

library(dplyr)
if (!exists("survey")) stop("Kor 1_readingdata.R forst.")
# Samma fil som i 1_readingdata.R. Andra sokvagen bara om originalfilen flyttas.
survey_fil_p3 <- "/safe/data/Buratti/Mcoglearning/data/surveycleaned-260401-n5363.csv"
if (!file.exists(survey_fil_p3)) stop("Ursprunglig enkatfil saknas: ", survey_fil_p3)
survey_original_p3 <- read.csv(survey_fil_p3)
fixed_items <- paste0("QSS39_", c(1, 2, 4, 6))
growth_items <- paste0("QSS39_", c(3, 5, 7, 8))
items <- c(fixed_items, growth_items)
missing_items <- setdiff(c("id", items), names(survey_original_p3))
if (length(missing_items)) stop("Saknade variabler: ", paste(missing_items, collapse = ", "))
if (!"id" %in% names(survey) || anyNA(survey$id) || anyDuplicated(survey$id)) stop("survey maste ha unika id utan NA.")
if (anyNA(survey_original_p3$id) || anyDuplicated(survey_original_p3$id)) stop("Originalfilen maste ha unika id utan NA.")
if (!all(vapply(survey_original_p3[items], is.numeric, logical(1)))) stop("QSS39-items maste vara numeriska; kontrollera kodning.")
x <- as.matrix(survey_original_p3[items])
if (any(!is.na(x) & (!is.finite(x) | x < 1 | x > 10)))
  stop("QSS39 har varden utanfor 1-10. Kontrollera svarsskala och bortfallskoder.")
score3 <- function(dat, vars) {
  z <- as.matrix(dat[vars]); n <- rowSums(!is.na(z)); ans <- rowMeans(z, na.rm = TRUE)
  ans[n < 3] <- NA_real_; ans
}
# Hogre entity = mer fixed; hogre incremental = mer growth.
match_p3 <- match(survey$id, survey_original_p3$id)
if (anyNA(match_p3)) stop("Vissa id i survey saknas i originalfilen. Kontrollera datakallan.")
survey$entity_p3 <- score3(survey_original_p3, fixed_items)[match_p3]
survey$incremental_p3 <- score3(survey_original_p3, growth_items)[match_p3]
cat("Paper 3: entity n =", sum(!is.na(survey$entity_p3)),
    "; incremental n =", sum(!is.na(survey$incremental_p3)), "\n")
cat("Itemindelning foljer reverseringen i originalskript 6; minst 3 av 4 besvarade per delskala.\n")
