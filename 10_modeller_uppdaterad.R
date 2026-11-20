# 10_systematiska_modeller.R
# Sex grundmodeller och fyra moderationsmodeller enligt modellplanen. Kör efter skript 8.
# VIKTIGT: ange nedan namnen på färdigkonstruerade variabler för verbal
# respektive digital self-concept. Skapa dem först om de ännu inte finns.
# Upprepade läranderesultat: slumpmässigt intercept per person.

library(dplyr)
library(lme4)

mapp <- "/safe/data/Buratti/Mcoglearning/data"
resultat_mapp <- file.path(mapp, "modeller_10_uppdaterad")

# ÄNDRA dessa två namn om era färdiga skalor heter något annat.
# cverbal och cdigital är namn från den lånade syntaxen; deras konstruktion
# och koppling till QSS37 behöver verifieras innan de används.
verbal_selfconcept <- "cverbal"
digital_selfconcept <- "cdigital"

# Om skalorna finns i survey men inte i RDS-filerna kan de läggas till i
# skript 8 och filerna sparas om. Det här skriptet skapar inte några skalor.
if (!dir.exists(mapp)) stop("Datamappen saknas: ", mapp)
filer <- file.path(mapp, c("word_learning_long.rds", "food_ordering_long.rds"))
if (any(!file.exists(filer))) stop("Saknade RDS-filer: ", paste(filer[!file.exists(filer)], collapse = ", "))
ord <- readRDS(filer[1]); meny <- readRDS(filer[2])

matt <- list(
  kalibrering = c("antonyms_abs_bias_paired", "metal_abs_bias_paired", "number_abs_bias_paired"),
  diskrimination = c("antonyms_slope", "metal_slope", "number_slope"),
  konfidens = c("antonyms_conf_paired", "metal_conf_paired", "number_conf_paired")
)
kognition <- c("sumopposites", "summetal", "sumnumber")
extra <- c("growthmindset", "metaknowledgeofcognition", "metaregulationofcognition", "hopeforsuccess", "fearoffailure")
for (par in list(list(data = ord, name = "ordinlärning", self = verbal_selfconcept),
                 list(data = meny, name = "menyuppgift", self = digital_selfconcept))) {
  utfall <- if (par$name == "ordinlärning") "antal_ratt" else "logtrial"
  behovs <- unique(c("id", "forsok", "Cohort", unlist(matt), kognition, extra, par$self, utfall))
  saknas <- setdiff(behovs, names(par$data))
  if (length(saknas)) stop(par$name, ": variabler saknas: ", paste(saknas, collapse = ", "),
    ". Kontrollera namnen och skapa/inkludera self-concept-skalorna i skript 8 innan du fortsätter.")
}
if (!dir.exists(resultat_mapp)) dir.create(resultat_mapp, recursive = TRUE)
if (!dir.exists(resultat_mapp)) stop("Kunde inte skapa resultatmappen.")

# Hela modellserien använder samma personer, inklusive komplett kognition
# och självrapporterade mått. Urvalet kan variera mellan serierna.
forbered <- function(data, uppgift, typ, self) {
  utfall <- if (uppgift == "ord") "antal_ratt" else "logtrial"
  variabler <- c("id", "forsok", "Cohort", matt[[typ]], kognition, extra, self, utfall)
  if (anyDuplicated(variabler)) stop("Samma variabel används i flera roller; kontrollera namnen.")
  d <- data[complete.cases(data[, variabler]), variabler, drop = FALSE]
  d <- d[d$forsok %in% if (uppgift == "ord") 0:2 else 0:3, , drop = FALSE]
  if (!nrow(d)) stop("Inga observationer med komplett data.")
  if (anyDuplicated(d[c("id", "forsok")])) stop("Dubbla id och försök.")
  if (uppgift == "ord" && any(d$antal_ratt < 0 | d$antal_ratt > 10 | d$antal_ratt != floor(d$antal_ratt))) stop("Ogiltigt antal rätt.")
  if (uppgift == "meny" && any(!is.finite(d$logtrial))) stop("Ogiltig logtid.")
  d$Cohort <- factor(d$Cohort)
  if (nlevels(d$Cohort) < 2) stop("Färre än två kohorter.")
  personvars <- c("Cohort", matt[[typ]], kognition, extra, self)
  for (v in personvars) {
    if (any(vapply(split(d[[v]], d$id), function(z) length(unique(z)) != 1L, logical(1))))
      stop("Personvariabeln varierar inom id: ", v)
  }
  p <- d[!duplicated(d$id), , drop = FALSE]
  ursprung <- c(matt[[typ]], kognition, extra, self)
  nya <- c("meta_verbal", "meta_spatial", "meta_numerisk", "kogn_verbal",
           "kogn_spatial", "kogn_numerisk", "mindset", "mai_knowledge", "mai_regulation", "hope_success", "fear_failure", "selfconcept")
  for (j in seq_along(ursprung)) {
    v <- ursprung[j]
    if (!is.numeric(p[[v]])) stop("Variabeln måste vara numerisk: ", v)
    s <- sd(p[[v]])
    if (!is.finite(s) || s == 0) stop("Kan inte standardisera ", v)
    d[[nya[j]]] <- (d[[v]] - mean(p[[v]])) / s
  }
  if (uppgift == "meny") d$log_tid <- d$logtrial / 10
  d
}

# Modellnummer följer den överenskomna tabellen.
plan <- data.frame(
  serie = c("ord_kalibrering", "ord_diskrimination", "ord_konfidens",
            "meny_kalibrering", "meny_diskrimination", "meny_konfidens",
            "ord_kalibrering", "ord_diskrimination", "meny_kalibrering", "meny_diskrimination"),
  steg = c(rep(3, 6), rep(4, 4)), nummer = 1:10
)
# Alla termer inkluderar försök, så huvudeffekter avser första försöket
# och interaktioner med försök avser linjär förändring på modellens skala.
meta_termer <- "meta_verbal + meta_spatial + meta_numerisk"
kogn_termer <- "kogn_verbal + kogn_spatial + kogn_numerisk"
extra_termer <- "mindset + mai_knowledge + mai_regulation + hope_success + fear_failure + selfconcept"

urval <- list(); modeller <- list(); koefficienter <- list(); status <- list()
for (s in unique(plan$serie)) {
  uppgift <- if (startsWith(s, "ord_")) "ord" else "meny"
  typ <- sub("^(ord|meny)_", "", s)
  self <- if (uppgift == "ord") verbal_selfconcept else digital_selfconcept
  data <- if (uppgift == "ord") ord else meny
  urval[[s]] <- tryCatch(forbered(data, uppgift, typ, self), error = function(e) e)
}

for (i in seq_len(nrow(plan))) {
  s <- plan$serie[i]; steg <- plan$steg[i]; nummer <- plan$nummer[i]
  namn <- paste0("modell_", nummer); d <- urval[[s]]
  cat("Kör", namn, "–", s, "\n")
  if (inherits(d, "error")) {
    status[[namn]] <- data.frame(modell = nummer, serie = s, status = "FEL",
      personer = NA_integer_, observationer = NA_integer_, detaljer = conditionMessage(d))
    next
  }
  if (steg == 1) rhs <- paste0("forsok * (", meta_termer, ") + Cohort + (1 | id)")
  if (steg == 2) rhs <- paste0("forsok * (", meta_termer, " + ", kogn_termer, ") + Cohort + (1 | id)")
  if (steg == 3) rhs <- paste0("forsok * (", meta_termer, " + ", kogn_termer, " + ", extra_termer, ") + Cohort + (1 | id)")
  if (steg == 4) rhs <- paste0("forsok * mindset * (", meta_termer, ") + forsok * (", kogn_termer,
                                " + mai_knowledge + mai_regulation + hope_success + fear_failure + selfconcept) + Cohort + (1 | id)")
  lhs <- if (startsWith(s, "ord_")) "cbind(antal_ratt, 10 - antal_ratt)" else "log_tid"
  formel <- as.formula(paste(lhs, "~", rhs))
  varningar <- character()
  fit <- tryCatch(withCallingHandlers({
    if (startsWith(s, "ord_")) glmer(formel, data = d, family = binomial,
                                      control = glmerControl(optimizer = "bobyqa"))
    else lmer(formel, data = d, REML = FALSE)
  }, warning = function(w) { varningar <<- c(varningar, conditionMessage(w)); invokeRestart("muffleWarning") }),
  error = function(e) e)
  if (inherits(fit, "error")) {
    status[[namn]] <- data.frame(modell = nummer, serie = s, status = "FEL", personer = n_distinct(d$id),
      observationer = nrow(d), detaljer = conditionMessage(fit))
    next
  }
  modeller[[namn]] <- fit
  co <- coef(summary(fit)); z <- co[, 1] / co[, 2]
  koefficienter[[namn]] <- data.frame(modell = nummer, serie = s, term = rownames(co),
    skattning = co[, 1], standardfel = co[, 2], nedre_95 = co[, 1] - 1.96 * co[, 2],
    ovre_95 = co[, 1] + 1.96 * co[, 2], p_wald_approx = 2 * pnorm(-abs(z)), row.names = NULL)
  conv <- fit@optinfo$conv$lme4$messages
  info <- paste(c(paste0("singular=", isSingular(fit)), varningar, conv), collapse = " | ")
  status[[namn]] <- data.frame(modell = nummer, serie = s, status = "OK", personer = n_distinct(d$id),
                               observationer = nrow(d), detaljer = info)
}
modellstatus <- bind_rows(status); modellkoefficienter <- bind_rows(koefficienter)
write.csv(modellstatus, file.path(resultat_mapp, "modellstatus.csv"), row.names = FALSE, na = "")
write.csv(modellkoefficienter, file.path(resultat_mapp, "modellkoefficienter.csv"), row.names = FALSE, na = "")
saveRDS(modeller, file.path(resultat_mapp, "modeller.rds"))
print(modellstatus, n = Inf)
cat("Resultaten finns i:", resultat_mapp, "\n")
# Ordmodeller: skattningar på logit-skala. Menymodeller: naturlig log-tid.
# P-värden och intervall är approximativa Waldmått; ingen justering för flera tester.
# Kontrollera särskilt konvergens, singularitet, överdispersion, modellens antaganden
# och antagandet om linjär förändring. Separata lutningar eller kategoriska försök
# samt analys av största möjliga urval kan undersökas som känslighetsanalyser.
