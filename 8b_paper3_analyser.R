# PAPER 3: BASALA ANALYSER INFÖR ABSTRACT
# Kor i samma R-session: originalskript 1, 2, 3; nya 6b; originalskript 7;
# darefter detta skript 8b. Inget av originalskripten 4, 5, 6, 8, 9, 10 behovs.
# 7 anvander de tre *_items som skapas av 2.
# 6b anvander samma itemindelning som reverseringen i originalskript 6.
# 6b laser ursprungliga QSS39 direkt ur enkaten, aven om skript 6 har korts.

library(dplyr)
library(tidyr)
library(lme4)
library(lmerTest)

# cverbal och cdigital ar academic self-concept enligt anvandaren. De tas med
# automatiskt om de finns i survey. Om de saknas fortsatter mindsetanalyserna
# och en varning samt information i resultatmappen anger vilka som saknas.
# Om self-efficacy finns fardig i survey, ange dess exakta variabelnamn har.
# Programmet konstruerar inte skalorna utanfor QSS39.
self_efficacy_var <- NULL
self_concept_vars <- c("cverbal", "cdigital")

# Ett nytt resultatbibliotek skapas vid varje korning under aktuell katalog.
# Efter originalskript 1 ar aktuell katalog /safe/data/Buratti/Mconlearning/data.
resultat_rot <- getwd()
resultat_mapp <- file.path(resultat_rot, paste0("paper3_resultat_", format(Sys.time(), "%Y%m%d_%H%M%S")))
if (dir.exists(resultat_mapp)) stop("Resultatmappen finns redan. Kor igen om en sekund.")
if (!dir.create(resultat_mapp, recursive = TRUE)) stop("Kunde inte skapa resultatmappen: ", resultat_mapp)

objekt <- c("survey", "ugu", "Antonymer_meta", "metal_meta", "number_meta")
saknas <- objekt[!vapply(objekt, exists, logical(1), inherits = TRUE)]
if (length(saknas)) stop("Kor forberedelserna forst. Saknas: ", paste(saknas, collapse = ", "))
sources <- mget(objekt, inherits = TRUE)
for (nm in names(sources)) {
  a <- sources[[nm]]
  if (!"id" %in% names(a) || anyNA(a$id) || anyDuplicated(a$id))
    stop(nm, " maste ha unika, icke-saknade id.")
}
missing_self_concept <- setdiff(self_concept_vars, names(survey))
if (length(missing_self_concept)) message("SAKNAS i survey: ", paste(missing_self_concept, collapse = ", "),
  ". Dessa self-concept-skalor ingar inte; kontrollera var de skapas.")
optional <- c(self_efficacy_var, intersect(self_concept_vars, names(survey)))
if (anyDuplicated(optional)) stop("Valfria skalor maste vara olika variabler.")
if (any(!optional %in% names(survey))) stop("Valfri skalvariabel saknas i survey: ", paste(setdiff(optional, names(survey)), collapse = ", "))
pred <- c("entity_p3", "incremental_p3", optional)
if (!all(pred %in% names(survey))) stop("Saknade skalor: ", paste(setdiff(pred, names(survey)), collapse = ", "))
if (!all(vapply(survey[pred], is.numeric, logical(1)))) stop("Samtliga prediktorer maste vara numeriska.")
if (!"Cohort" %in% names(ugu)) stop("Cohort saknas i ugu.")

# En rad per person och test. Minst fem matchade svar for alla utfall.
# Andra endast efter diskussion; slope forutsatter dessutom bade ratt och fel.
min_par <- 5L
make_domain <- function(meta, prefix, domain) {
  suffix <- c("n_pairs", "n_correct", "n_incorrect", "conf_paired", "acc_paired", "bias_paired", "abs_bias_paired", "slope")
  cols <- paste0(prefix, "_", suffix)
  if (!all(cols %in% names(meta))) stop("Saknade variabler i ", domain, ": ", paste(setdiff(cols, names(meta)), collapse = ", "))
  d <- meta[, c("id", cols)]; names(d) <- c("id", suffix); d$domain <- domain; d
}
d <- bind_rows(make_domain(Antonymer_meta, "antonyms", "Opposites"),
               make_domain(number_meta, "number", "Number Series"),
               make_domain(metal_meta, "metal", "Metal Folding")) %>%
  left_join(survey[, c("id", pred), drop = FALSE], by = "id") %>%
  left_join(ugu[, c("id", "Cohort"), drop = FALSE], by = "id")
d$domain <- factor(d$domain, levels = c("Opposites", "Number Series", "Metal Folding"))
d$Cohort <- factor(d$Cohort)
if (anyDuplicated(d[c("id", "domain")])) stop("Dubbla id/domankombinationer.")
d$kompletta_prediktorer <- complete.cases(d[, c(pred, "Cohort"), drop = FALSE])

# Beskrivning fore filter: visar urval och bortfall inklusive slope.
description <- d %>% group_by(domain) %>% summarise(
  personer = n(), minst_5_par = sum(n_pairs >= min_par, na.rm = TRUE),
  minst_5_par_och_slope = sum(n_pairs >= min_par & !is.na(slope), na.rm = TRUE),
  med_alla_prediktorer_och_kohort = sum(kompletta_prediktorer),
  n_par_medel = mean(n_pairs, na.rm = TRUE), .groups = "drop")
write.csv(description, file.path(resultat_mapp, "01_urval.csv"), row.names = FALSE, na = "")
d <- d %>% filter(!is.na(n_pairs), n_pairs >= min_par)
outcomes <- c("conf_paired", "bias_paired", "abs_bias_paired", "slope")
summaries <- list()
for (v in c(outcomes, "acc_paired")) {
  x <- d %>% group_by(domain) %>% summarise(n = sum(!is.na(.data[[v]])),
    medel = if (all(is.na(.data[[v]]))) NA_real_ else mean(.data[[v]], na.rm = TRUE),
    sd = if (sum(!is.na(.data[[v]])) < 2) NA_real_ else sd(.data[[v]], na.rm = TRUE), .groups = "drop")
  x$variabel <- v; summaries[[v]] <- x
}
write.csv(bind_rows(summaries), file.path(resultat_mapp, "02_beskrivning.csv"), row.names = FALSE, na = "")

# Domanspecifika linjara regressioner med entity, incremental, eventuella
# verifierade skalor och kohort samtidigt. Ojusterade for accuracy: accuracy
# ingar redan i bias. Complete case per utfall och doman. Ingen kausal tolkning.
coefs <- list(); statuses <- list(); k <- 0L
for (dom in levels(d$domain)) for (y in outcomes) {
  k <- k + 1L
  dat <- d[d$domain == dom, , drop = FALSE]
  vars <- c(y, pred, "Cohort")
  dat <- dat[complete.cases(dat[, vars, drop = FALSE]), , drop = FALSE]
  dat$Cohort <- droplevels(dat$Cohort)
  form <- reformulate(c(pred, "Cohort"), response = y)
  fit <- tryCatch({
    if (nrow(dat) <= length(pred) + 3 || nlevels(dat$Cohort) < 2) stop("For fa observationer eller kohorter.")
    m <- lm(form, data = dat)
    if (anyNA(coef(m))) stop("Modellen ar inte full rang; kontrollera skalorna.")
    m
  }, error = function(e) e)
  if (inherits(fit, "error")) {
    statuses[[k]] <- data.frame(doman = dom, utfall = y, n = nrow(dat), status = conditionMessage(fit))
  } else {
    tab <- summary(fit)$coefficients; ci <- confint(fit)
    coefs[[k]] <- data.frame(doman = dom, utfall = y, n = nrow(dat), term = rownames(tab),
      skattning = tab[, 1], standardfel = tab[, 2], nedre_95 = ci[, 1], ovre_95 = ci[, 2], p = tab[, 4], row.names = NULL)
    statuses[[k]] <- data.frame(doman = dom, utfall = y, n = nrow(dat), status = "OK")
  }
}
write.csv(bind_rows(statuses), file.path(resultat_mapp, "03_modellstatus.csv"), row.names = FALSE, na = "")
write.csv(bind_rows(coefs), file.path(resultat_mapp, "04_modeller_per_doman.csv"), row.names = FALSE, na = "")

# Sekundart: test av om varje prediktors samband varierar mellan domaner.
# Alla prediktorer har domaninteraktion i samma modell; likelihood ratio-test
# jamfor modeller pa identiska observationer, med slumpintercept for person.
# Testerna ar explorativa och p-varden ar inte justerade for multipla test.
tests <- list(); ik <- 0L
for (y in outcomes) {
  dat <- d[complete.cases(d[, c(y, pred, "Cohort"), drop = FALSE]), , drop = FALSE]
  dat$Cohort <- droplevels(dat$Cohort); dat$domain <- droplevels(dat$domain)
  for (p in pred) {
    ik <- ik + 1L
    ans <- tryCatch({
      if (nrow(dat) < 30 || nlevels(dat$domain) < 3 || nlevels(dat$Cohort) < 2) stop("For fa observationer, domaner eller kohorter.")
      full_rhs <- paste(c("domain", "Cohort", pred, paste0("domain:", pred), "(1|id)"), collapse = " + ")
      reduced_rhs <- paste(c("domain", "Cohort", pred, paste0("domain:", setdiff(pred, p)), "(1|id)"), collapse = " + ")
      full <- lme4::lmer(as.formula(paste(y, "~", full_rhs)), data = dat, REML = FALSE)
      reduced <- lme4::lmer(as.formula(paste(y, "~", reduced_rhs)), data = dat, REML = FALSE)
      a <- anova(reduced, full)
      msg <- full@optinfo$conv$lme4$messages
      data.frame(utfall = y, prediktor = p, n_rader = nrow(dat), n_personer = n_distinct(dat$id),
        chi2 = a$Chisq[2], df = a$Df[2], p = a$`Pr(>Chisq)`[2],
        singular = lme4::isSingular(full), status = if (length(msg)) paste(msg, collapse = "; ") else "OK")
    }, error = function(e) data.frame(utfall = y, prediktor = p, n_rader = nrow(dat),
        n_personer = n_distinct(dat$id), chi2 = NA_real_, df = NA_real_, p = NA_real_,
        singular = NA, status = conditionMessage(e)))
    tests[[ik]] <- ans
  }
}
write.csv(bind_rows(tests), file.path(resultat_mapp, "05_domaninteraktioner_explorativa.csv"), row.names = FALSE, na = "")
writeLines(c("PAPER 3: las detta tillsammans med resultatfilerna.",
  "Entity och incremental foljer itemindelningen i originalfil 6; minst 3 av 4 besvarade per delskala.",
  paste("Ytterligare skalor:", if (length(optional)) paste(optional, collapse = ", ") else "INGA: self-efficacy och self-concept analyseras inte."),
  paste("Self-concept-skalor som saknas i survey:", if (length(missing_self_concept)) paste(missing_self_concept, collapse = ", ") else "inga"),
  paste("Self-efficacy:", if (is.null(self_efficacy_var)) "inget variabelnamn angivet; ingar inte" else self_efficacy_var),
  "Minst fem matchade uppgifter; slope kraver minst ett ratt och ett fel.",
  "Konfidens ar 0-100; bias, absolut bias, accuracy och slope ar 0-1-enheter (bias/slope kan vara negativa).",
  "Modellerna ar associativa och explorativa; p-varden ar inte justerade for multipla test.",
  "Slope ar skillnad i medelkonfidens, inte en itemniva-modell.",
  "Kontrollera resultatens rimlighet, osakerhet, bortfall och modellstatus fore abstract."),
  file.path(resultat_mapp, "00_las_mig.txt"))
print(description)
cat("\nResultat finns i:", resultat_mapp, "\n")
if (length(missing_self_concept)) cat("OBS: self-concept saknas i survey:", paste(missing_self_concept, collapse = ", "), "\n")
if (is.null(self_efficacy_var)) cat("OBS: self-efficacy ingar inte; variabelnamn saknas.\n")
