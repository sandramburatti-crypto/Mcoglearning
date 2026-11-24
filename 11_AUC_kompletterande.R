# 11_AUC_kompletterande.R
# Kompletterande analyser av sammanlagd prestation. En rad per person.
# Kör efter att den uppdaterade versionen av skript 8 har sparat df_mconlearning.rds.
# Inga av de tidigare modellerna ändras och inga tidigare resultat skrivs över.

mapp <- "/safe/data/Buratti/Mcoglearning/data"
resultat_mapp <- file.path(mapp, "auc_kompletterande")
fil <- file.path(mapp, "df_mconlearning.rds")
if (!file.exists(fil)) stop("Hittar inte personfilen: ", fil)
df <- readRDS(fil)
if (!dir.exists(resultat_mapp)) dir.create(resultat_mapp, recursive = TRUE)
if (!dir.exists(resultat_mapp)) stop("Kunde inte skapa resultatmappen.")

# Ordinlärning: befintligt auc_glos från skript 5.
# Där antas ett försök 0 med noll rätt: T1 + T2 + 0,5*T3.
# Högre värde = fler rätt sammantaget; ej ett rent förändringsmått.
# Meny: AUC för de fyra observerade upprepningarna av samma beställning.
# Trapezmetoden används på naturligt logaritmerad tid; värdet vänds så
# högre värde betyder kortare tid sammantaget. Ingen hypotetisk startpunkt.
# logtrial i skript 5 är log(tid)*10, så dividera med 10.
# Jämför inte ord- och meny-AUC numeriskt: de har olika skalor.
behovs <- c("id", "auc_glos", paste0("logtrial", 1:4), "Cohort", "cverbal", "cdigital",
             "sumopposites", "summetal", "sumnumber", "growthmindset",
             "metaknowledgeofcognition", "metaregulationofcognition", "hopeforsuccess", "fearoffailure",
             "antonyms_abs_bias_paired", "metal_abs_bias_paired", "number_abs_bias_paired",
             "antonyms_slope", "metal_slope", "number_slope",
             "antonyms_conf_paired", "metal_conf_paired", "number_conf_paired")
saknas <- setdiff(behovs, names(df))
if (length(saknas)) stop("Variabler saknas i personfilen: ", paste(saknas, collapse = ", "))
if (anyNA(df$id) || anyDuplicated(df$id)) stop("Personfilen har saknade eller dubbla id.")
for (v in c("auc_glos", paste0("logtrial", 1:4))) if (!is.numeric(df[[v]])) stop("Måste vara numerisk: ", v)
logtider <- as.matrix(df[, paste0("logtrial", 1:4)]) / 10
komplett_meny <- rowSums(is.finite(logtider)) == 4L
df$auc_meny_1_4 <- NA_real_
df$auc_meny_1_4[komplett_meny] <- -(
  logtider[komplett_meny, 1] / 2 + logtider[komplett_meny, 2] +
  logtider[komplett_meny, 3] + logtider[komplett_meny, 4] / 2)

monitorering <- list(
  kalibrering = c("antonyms_abs_bias_paired", "metal_abs_bias_paired", "number_abs_bias_paired"),
  diskrimination = c("antonyms_slope", "metal_slope", "number_slope"),
  konfidens = c("antonyms_conf_paired", "metal_conf_paired", "number_conf_paired")
)
kognition <- c("sumopposites", "summetal", "sumnumber")
extra <- c("growthmindset", "metaknowledgeofcognition", "metaregulationofcognition", "hopeforsuccess", "fearoffailure")

# Samma kompletta urval används för grund- och moderationsmodell inom varje
# kombination, men urvalen kan skilja sig mellan de olika kombinationerna.
forbered <- function(uppgift, typ) {
  utfall <- if (uppgift == "ord") "auc_glos" else "auc_meny_1_4"
  self <- if (uppgift == "ord") "cverbal" else "cdigital"
  vars <- c("id", "Cohort", utfall, monitorering[[typ]], kognition, extra, self)
  d <- df[complete.cases(df[, vars]), vars, drop = FALSE]
  d <- d[is.finite(d[[utfall]]), , drop = FALSE]
  if (!nrow(d)) stop("Inga personer med kompletta data.")
  d$Cohort <- factor(d$Cohort)
  if (nlevels(d$Cohort) < 2) stop("Färre än två kohorter i urvalet.")
  gamla <- c(monitorering[[typ]], kognition, extra, self)
  nya <- c("meta_verbal", "meta_spatial", "meta_numerisk", "kogn_verbal", "kogn_spatial",
           "kogn_numerisk", "mindset", "mai_knowledge", "mai_regulation", "hope_success",
           "fear_failure", "selfconcept")
  for (j in seq_along(gamla)) {
    x <- d[[gamla[j]]]
    if (!is.numeric(x) || any(!is.finite(x))) stop("Variabeln måste vara numerisk och ändlig: ", gamla[j])
    if (sd(x) == 0) stop("Kan inte standardisera variabel utan variation: ", gamla[j])
    d[[nya[j]]] <- (x - mean(x)) / sd(x)
  }
  d$auc <- d[[utfall]]
  d
}

# Sex grundmodeller: tre monitoreringsmått för vardera uppgift.
# Fyra moderationsmodeller: growth mindset × kalibrering/diskrimination.
# Moderation avser sambandet med SAMMANLAGD prestation, inte inlärningstakt.
plan <- data.frame(
  serie = c("ord_kalibrering", "ord_diskrimination", "ord_konfidens",
            "meny_kalibrering", "meny_diskrimination", "meny_konfidens",
            "ord_kalibrering", "ord_diskrimination", "meny_kalibrering", "meny_diskrimination"),
  modelltyp = c(rep("grund", 6), rep("moderation", 4)), modell = 1:10)
meta <- "meta_verbal + meta_spatial + meta_numerisk"
kontroller <- paste("kogn_verbal + kogn_spatial + kogn_numerisk + Cohort +",
                    "mai_knowledge + mai_regulation + hope_success + fear_failure + selfconcept")
status <- list(); koefficienter <- list(); beskr <- list()
for (i in seq_len(nrow(plan))) {
  serie <- plan$serie[i]; nummer <- plan$modell[i]; modelltyp <- plan$modelltyp[i]
  uppgift <- if (startsWith(serie, "ord_")) "ord" else "meny"
  typ <- sub("^(ord|meny)_", "", serie)
  cat("Kör AUC-modell", nummer, "-", serie, modelltyp, "\n")
  varningar <- character()
  result <- tryCatch(withCallingHandlers({
    d <- forbered(uppgift, typ)
    rhs <- if (modelltyp == "grund") paste(meta, "+ mindset +", kontroller) else
      paste0("mindset * (", meta, ") + ", kontroller)
    modell <- lm(as.formula(paste("auc ~", rhs)), data = d, na.action = na.fail)
    if (modell$rank < length(coef(modell))) stop("Modellmatrisen är rangbristfällig; kontrollera prediktorerna.")
    if (df.residual(modell) < 1) stop("För få frihetsgrader.")
    list(d = d, modell = modell)
  }, warning = function(w) { varningar <<- c(varningar, conditionMessage(w)); invokeRestart("muffleWarning") }),
  error = function(e) e)
  if (inherits(result, "error")) {
    status[[i]] <- data.frame(modell = nummer, serie = serie, modelltyp = modelltyp,
      status = "FEL", personer = NA_integer_, detaljer = conditionMessage(result))
    next
  }
  fit <- result$modell; co <- coef(summary(fit)); krit <- qt(.975, df.residual(fit))
  koefficienter[[i]] <- data.frame(modell = nummer, serie = serie, modelltyp = modelltyp,
    term = rownames(co), skattning = co[, 1], standardfel = co[, 2], t = co[, 3],
    p = co[, 4], nedre_95 = co[, 1] - krit * co[, 2], ovre_95 = co[, 1] + krit * co[, 2],
    row.names = NULL)
  status[[i]] <- data.frame(modell = nummer, serie = serie, modelltyp = modelltyp,
    status = "OK", personer = nrow(result$d), detaljer = paste(varningar, collapse = " | "))
  beskr[[i]] <- data.frame(modell = nummer, serie = serie, modelltyp = modelltyp,
    personer = nrow(result$d), auc_medel = mean(result$d$auc), auc_sd = sd(result$d$auc),
    auc_min = min(result$d$auc), auc_max = max(result$d$auc), r_kvadrat = summary(fit)$r.squared)
}
modellstatus <- do.call(rbind, Filter(Negate(is.null), status))
modelltal <- do.call(rbind, Filter(Negate(is.null), koefficienter))
beskrivning <- do.call(rbind, Filter(Negate(is.null), beskr))
write.csv(modellstatus, file.path(resultat_mapp, "auc_modellstatus.csv"), row.names = FALSE, na = "")
if (!is.null(modelltal)) write.csv(modelltal, file.path(resultat_mapp, "auc_koefficienter.csv"), row.names = FALSE, na = "")
if (!is.null(beskrivning)) write.csv(beskrivning, file.path(resultat_mapp, "auc_beskrivning.csv"), row.names = FALSE, na = "")
# Samlad export: en rad per modellparameter, inklusive status och antal personer.
# Om en modell misslyckas behålls även den raden med felmeddelande.
if (!is.null(modelltal)) {
  samlet <- merge(modellstatus, modelltal, by = c("modell", "serie", "modelltyp"),
                  all.x = TRUE, sort = FALSE)
} else {
  samlet <- modellstatus
}
if (!is.null(beskrivning)) {
  beskrivning_export <- beskrivning[, c("modell", "serie", "modelltyp", "auc_medel", "auc_sd", "auc_min", "auc_max", "r_kvadrat")]
  samlet <- merge(samlet, beskrivning_export, by = c("modell", "serie", "modelltyp"),
                  all.x = TRUE, sort = FALSE)
}
samlet <- samlet[order(samlet$modell), , drop = FALSE]
write.csv(samlet, file.path(resultat_mapp, "auc_samlade_resultat.csv"), row.names = FALSE, na = "")
print(modellstatus)
cat("\nResultaten finns i:", resultat_mapp, "\n")
# Ingen individdata eller modellobjekt exporteras av detta skript.
# P-värden är ojusterade för multipla tester. Bedöm diagnostik för linjär
# regression, inklusive residualer och eventuella tak-/golveffekter, före tolkning.
# Lägg inte till första försöket som kontroll utan en tydlig analysfråga;
# det ingår redan i AUC. AUC är inte en skattning av inlärningstakten.
