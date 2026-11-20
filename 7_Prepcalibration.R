# PREPARING METACOGNITION: CONFIDENCE, BIAS AND SLOPE
# 2026-11-05 / Mconlearning
#
# Syntaxen utgår från opposites_items, metal_items och number_items som sparas i 2_prepcognition.R, med unikt id.
# Conf är medelkonfidens på skalan 0–100. Acc är andel rätt på skalan 0–1.
# Sinerad Bias är conf/100 minus acc; positivt = överkonfidens, negativt = under.
# Abs_bias är absoluttalet av bias (unsigned bias).
# Bias beräknas med separat tillgängliga svar enligt referenssyntaxen.
# Bias_paired och abs_bias_paired använder endast matchade svar som jämförelse.
# Slope är skillnaden mellan medelkonfidens vid rätt och fel, skala -1 till 1. Desto högre värde desto bättre separation. 
# Gamma är ett alternativt sensitivitetmått. Inga jämförbara par ger NA.

###########################
# Gemensamma hjälpfunktioner
###########################

# Kontrollerar värden och omvandlar TRUE/FALSE eller 0/1 till tal.
# Samma funktion används också för konfidens på skalan 0–100.
as_number_checked <- function(x, label) {
  s <- trimws(as.character(x))
  s[is.na(s) | s == ""] <- NA_character_
  s[toupper(s) == "TRUE"] <- "1"
  s[toupper(s) == "FALSE"] <- "0"
  out <- suppressWarnings(as.numeric(s))
  if (any(!is.na(s) & is.na(out))) stop(label, ": ogiltiga värden; kontrollera kodning.")
  out
}

gamma_calc <- function(conf, correct) {
  if (length(conf) < 2L) return(NA_real_)
  ij <- combn(seq_along(conf), 2L)
  d <- (conf[ij[1, ]] - conf[ij[2, ]]) *
       (correct[ij[1, ]] - correct[ij[2, ]])
  concordant <- sum(d > 0)
  discordant <- sum(d < 0)
  if (concordant + discordant == 0L) return(NA_real_)
  (concordant - discordant) / (concordant + discordant)
}


###########################
# Antonymer / VERBAL FÖRMÅGA
###########################

# Hämta data och ange de tio konfidensbedömda uppgifterna.
data <- opposites_items
domain <- "antonyms"
items <- c(24, 31, 35, 37, 40, 41, 42, 43, 44, 45)
confidence_cols <- paste0("m_con_", 1:10)
correct_cols <- paste0("mCat_", items, "_correct")
response_cols <- lapply(paste0("mCat_", items), c)

# Kontrollera kolumner, id och giltiga värden; räkna sedan varje mått.
# Rätt/fel sätts till NA om ursprungssvaret saknas.
  needed <- unique(c("id", confidence_cols, correct_cols, unlist(response_cols)))
  missing_cols <- setdiff(needed, names(data))
  if (length(missing_cols)) stop(domain, ": saknade kolumner: ", paste(missing_cols, collapse = ", "))
  if (anyNA(data$id) || anyDuplicated(data$id)) stop(domain, ": id måste vara unikt och inte saknas. Rengör dubbletter först.")
  if (!nrow(data)) stop(domain, ": inga rader finns.")

  confidence <- do.call(cbind, lapply(confidence_cols, function(nm) as_number_checked(data[[nm]], nm)))
  correct <- do.call(cbind, lapply(correct_cols, function(nm) as_number_checked(data[[nm]], nm)))
  if (any(!is.na(confidence) & (!is.finite(confidence) | confidence < 0 | confidence > 100)))
    stop(domain, ": konfidens måste vara mellan 0 och 100.")
  if (any(!is.na(correct) & !(correct %in% c(0, 1)))) stop(domain, ": rätt/fel måste vara 0 eller 1.")

  # Rätt/fel sätts till NA om ursprungligt svar saknas. För talserier
  # krävs båda delsvaren; använd original före eventuell NA -> FALSE.
  for (j in seq_along(response_cols)) {
    answered <- rep(TRUE, nrow(data))
    for (nm in response_cols[[j]]) {
      raw <- trimws(as.character(data[[nm]]))
      answered <- answered & !is.na(raw) & raw != ""
    }
    correct[!answered, j] <- NA_real_
  }
  paired <- !is.na(confidence) & !is.na(correct)
  rows <- lapply(seq_len(nrow(data)), function(i) {
    # Matchade svar har både konfidensbedömning och rätt/fel på samma uppgift.
    konfidens_par <- confidence[i, paired[i, ]] / 100
    resultat_par <- correct[i, paired[i, ]]
    antal_par <- length(konfidens_par)

    # Antal tillgängliga svar och matchade rätta respektive felaktiga svar.
    n_conf <- sum(!is.na(confidence[i, ]))
    n_answered <- sum(!is.na(correct[i, ]))
    n_correct <- sum(resultat_par == 1)
    n_incorrect <- sum(resultat_par == 0)

    # Medelkonfidens 0–100: högre värde betyder större genomsnittlig säkerhet.
    # Andel rätt 0–1: högre värde betyder bättre prestation.
    conf_all <- confidence[i, !is.na(confidence[i, ])] / 100
    acc_all <- correct[i, !is.na(correct[i, ])]
    conf <- if (length(conf_all)) mean(conf_all) * 100 else NA_real_
    acc <- if (length(acc_all)) mean(acc_all) else NA_real_

    # Signerad bias: medelkonfidens minus andel rätt, båda på skalan 0–1.
    # Positivt = överkonfidens; negativt = underkonfidens; 0 = överensstämmelse.
    # Medelvärdena använder var för sig tillgängliga svar, enligt referenssyntaxen.
    bias <- if (length(conf_all) && length(acc_all)) mean(conf_all) - mean(acc_all) else NA_real_

    # Absolut bias (unsigned bias): absolutbeloppet av signerad bias.
    # 0 betyder ingen genomsnittlig avvikelse; större värde betyder större
    # avvikelse utan att visa om den är över- eller underkonfidens.
    # Exempel: bias +0,20 och -0,20 ger båda absolut bias 0,20.
    abs_bias <- abs(bias)

    # Parvis bias: samma beräkning, men endast uppgifter där både konfidens
    # och rätt/fel finns. Den kan skilja sig från bias ovan om svar saknas.
    bias_paired <- if (antal_par) mean(konfidens_par) - mean(resultat_par) else NA_real_
    abs_bias_paired <- abs(bias_paired)

    # Slope: medelkonfidens för rätta svar minus medelkonfidens för felaktiga.
     
    slope <- if (any(resultat_par == 1) && any(resultat_par == 0))
      mean(konfidens_par[resultat_par == 1]) - mean(konfidens_par[resultat_par == 0]) else NA_real_

    # Gamma är ett alternativt sensitivitetmått baserat på ordningen mellan
    # konfidens och rätt/fel. Inga jämförbara par ger NA.
    gamma <- gamma_calc(konfidens_par, resultat_par)

    # Samla alla mått för en person i den aktuella domänen.
    c(n_conf = n_conf, n_answered = n_answered, n_pairs = antal_par,
      n_correct = n_correct, n_incorrect = n_incorrect,
      conf = conf, conf_paired = if (antal_par) mean(konfidens_par) * 100 else NA_real_,
      acc = acc, acc_paired = if (antal_par) mean(resultat_par) else NA_real_,
      bias = bias, abs_bias = abs_bias, bias_paired = bias_paired,
      abs_bias_paired = abs_bias_paired, slope = slope, gamma = gamma)
  })
  measures <- as.data.frame(do.call(rbind, rows))
  names(measures) <- paste0(domain, "_", names(measures))
  Antonymer_meta <- data.frame(id = data$id, measures, check.names = FALSE)

# Resultatet Antonymer_meta innehåller bara id och mått från denna domän.
cat("\nAntonymer / VERBAL FÖRMÅGA: antal personer =", nrow(Antonymer_meta),
    "; antal med sensitivitet =", sum(!is.na(Antonymer_meta$antonyms_slope)), "\n")

###########################
# METAL FOLDING / SPATIAL FÖRMÅGA
###########################

# Hämta data och ange de tio konfidensbedömda uppgifterna.
data <- metal_items
domain <- "metal"
items <- c(10, 12, 16, 19, 20, 21, 24, 26, 27, 28)
confidence_cols <- paste0("p_con_", 1:10)
correct_cols <- paste0("p_Q", items, "_correct")
response_cols <- lapply(paste0("p_Q", items), c)

# Kontrollera kolumner, id och giltiga värden; räkna sedan varje mått.
# Rätt/fel sätts till NA om ursprungssvaret saknas.
  needed <- unique(c("id", confidence_cols, correct_cols, unlist(response_cols)))
  missing_cols <- setdiff(needed, names(data))
  if (length(missing_cols)) stop(domain, ": saknade kolumner: ", paste(missing_cols, collapse = ", "))
  if (anyNA(data$id) || anyDuplicated(data$id)) stop(domain, ": id måste vara unikt och inte saknas. Rengör dubbletter först.")
  if (!nrow(data)) stop(domain, ": inga rader finns.")

  confidence <- do.call(cbind, lapply(confidence_cols, function(nm) as_number_checked(data[[nm]], nm)))
  correct <- do.call(cbind, lapply(correct_cols, function(nm) as_number_checked(data[[nm]], nm)))
  if (any(!is.na(confidence) & (!is.finite(confidence) | confidence < 0 | confidence > 100)))
    stop(domain, ": konfidens måste vara mellan 0 och 100.")
  if (any(!is.na(correct) & !(correct %in% c(0, 1)))) stop(domain, ": rätt/fel måste vara 0 eller 1.")

  # Rätt/fel sätts till NA om ursprungligt svar saknas. För talserier
  # krävs båda delsvaren; använd original före eventuell NA -> FALSE.
  for (j in seq_along(response_cols)) {
    answered <- rep(TRUE, nrow(data))
    for (nm in response_cols[[j]]) {
      raw <- trimws(as.character(data[[nm]]))
      answered <- answered & !is.na(raw) & raw != ""
    }
    correct[!answered, j] <- NA_real_
  }
  paired <- !is.na(confidence) & !is.na(correct)
  rows <- lapply(seq_len(nrow(data)), function(i) {
    # Matchade svar har både konfidensbedömning och rätt/fel på samma uppgift.
    konfidens_par <- confidence[i, paired[i, ]] / 100
    resultat_par <- correct[i, paired[i, ]]
    antal_par <- length(konfidens_par)

    # Antal tillgängliga svar och matchade rätta respektive felaktiga svar.
    n_conf <- sum(!is.na(confidence[i, ]))
    n_answered <- sum(!is.na(correct[i, ]))
    n_correct <- sum(resultat_par == 1)
    n_incorrect <- sum(resultat_par == 0)

    # Medelkonfidens 0–100: högre värde betyder större genomsnittlig säkerhet.
    # Andel rätt 0–1: högre värde betyder bättre prestation.
    conf_all <- confidence[i, !is.na(confidence[i, ])] / 100
    acc_all <- correct[i, !is.na(correct[i, ])]
    conf <- if (length(conf_all)) mean(conf_all) * 100 else NA_real_
    acc <- if (length(acc_all)) mean(acc_all) else NA_real_

    # Signerad bias: medelkonfidens minus andel rätt, båda på skalan 0–1.
    bias <- if (length(conf_all) && length(acc_all)) mean(conf_all) - mean(acc_all) else NA_real_

    # Absolut bias (unsigned bias): absolutbeloppet av signerad bias.
    # 0 betyder ingen genomsnittlig avvikelse; större värde betyder större
    # avvikelse utan att visa om den är över- eller underkonfidens.
    # Exempel: bias +0,20 och -0,20 ger båda absolut bias 0,20.
    abs_bias <- abs(bias)

    # Parvis bias: samma beräkning, men endast uppgifter där både konfidens
    # och rätt/fel finns. Den kan skilja sig från bias ovan om svar saknas.
    bias_paired <- if (antal_par) mean(konfidens_par) - mean(resultat_par) else NA_real_
    abs_bias_paired <- abs(bias_paired)

    # Slope: medelkonfidens för rätta svar minus medelkonfidens för felaktiga.
      slope <- if (any(resultat_par == 1) && any(resultat_par == 0))
      mean(konfidens_par[resultat_par == 1]) - mean(konfidens_par[resultat_par == 0]) else NA_real_

    # Gamma är ett alternativt sensitivitetmått baserat på ordningen mellan konfidens och rätt/fel. Inga jämförbara par ger NA.
    gamma <- gamma_calc(konfidens_par, resultat_par)

    # Samla alla mått för en person i den aktuella domänen.
    c(n_conf = n_conf, n_answered = n_answered, n_pairs = antal_par,
      n_correct = n_correct, n_incorrect = n_incorrect,
      conf = conf, conf_paired = if (antal_par) mean(konfidens_par) * 100 else NA_real_,
      acc = acc, acc_paired = if (antal_par) mean(resultat_par) else NA_real_,
      bias = bias, abs_bias = abs_bias, bias_paired = bias_paired,
      abs_bias_paired = abs_bias_paired, slope = slope, gamma = gamma)
  })
  measures <- as.data.frame(do.call(rbind, rows))
  names(measures) <- paste0(domain, "_", names(measures))
  metal_meta <- data.frame(id = data$id, measures, check.names = FALSE)

# Resultatet metal_meta innehåller bara id och mått från denna domän.
cat("\nMETAL FOLDING / SPATIAL FÖRMÅGA: antal personer =", nrow(metal_meta),
    "; antal med sensitivitet =", sum(!is.na(metal_meta$metal_slope)), "\n")

###########################
# NUMBER SERIES / TALFÖRMÅGA
###########################

# Hämta data och ange de tio konfidensbedömda uppgifterna.
data <- number_items
domain <- "number"
items <- c(2, 10, 11, 13, 14, 15, 17, 18, 19, 22)
confidence_cols <- paste0("tcat_t", items)
correct_cols <- sprintf("Tal%02d_correct", items)
response_cols <- lapply(items, function(i) sprintf("Tal%02d%s", i, c("a", "b")))

# Kontrollera kolumner, id och giltiga värden; räkna sedan varje mått.
# Rätt/fel sätts till NA om ursprungssvaret saknas.
  needed <- unique(c("id", confidence_cols, correct_cols, unlist(response_cols)))
  missing_cols <- setdiff(needed, names(data))
  if (length(missing_cols)) stop(domain, ": saknade kolumner: ", paste(missing_cols, collapse = ", "))
  if (anyNA(data$id) || anyDuplicated(data$id)) stop(domain, ": id måste vara unikt och inte saknas. Rengör dubbletter först.")
  if (!nrow(data)) stop(domain, ": inga rader finns.")

  confidence <- do.call(cbind, lapply(confidence_cols, function(nm) as_number_checked(data[[nm]], nm)))
  correct <- do.call(cbind, lapply(correct_cols, function(nm) as_number_checked(data[[nm]], nm)))
  if (any(!is.na(confidence) & (!is.finite(confidence) | confidence < 0 | confidence > 100)))
    stop(domain, ": konfidens måste vara mellan 0 och 100.")
  if (any(!is.na(correct) & !(correct %in% c(0, 1)))) stop(domain, ": rätt/fel måste vara 0 eller 1.")

  # Rätt/fel sätts till NA om ursprungligt svar saknas. För talserier
  # krävs båda delsvaren; använd original före eventuell NA -> FALSE.
  for (j in seq_along(response_cols)) {
    answered <- rep(TRUE, nrow(data))
    for (nm in response_cols[[j]]) {
      raw <- trimws(as.character(data[[nm]]))
      answered <- answered & !is.na(raw) & raw != ""
    }
    correct[!answered, j] <- NA_real_
  }
  paired <- !is.na(confidence) & !is.na(correct)
  rows <- lapply(seq_len(nrow(data)), function(i) {
    # Matchade svar har både konfidensbedömning och rätt/fel på samma uppgift.
    konfidens_par <- confidence[i, paired[i, ]] / 100
    resultat_par <- correct[i, paired[i, ]]
    antal_par <- length(konfidens_par)

    # Antal tillgängliga svar och matchade rätta respektive felaktiga svar.
    n_conf <- sum(!is.na(confidence[i, ]))
    n_answered <- sum(!is.na(correct[i, ]))
    n_correct <- sum(resultat_par == 1)
    n_incorrect <- sum(resultat_par == 0)

    # Medelkonfidens 0–100: högre värde betyder större genomsnittlig säkerhet.
    conf_all <- confidence[i, !is.na(confidence[i, ])] / 100
    acc_all <- correct[i, !is.na(correct[i, ])]
    conf <- if (length(conf_all)) mean(conf_all) * 100 else NA_real_
    acc <- if (length(acc_all)) mean(acc_all) else NA_real_

    # Signerad bias: medelkonfidens minus andel rätt.
    bias <- if (length(conf_all) && length(acc_all)) mean(conf_all) - mean(acc_all) else NA_real_

    # Absolut bias (unsigned bias): absolutbeloppet av signerad bias.
    # 0 betyder ingen genomsnittlig avvikelse; större värde betyder större
    # avvikelse utan att visa om den är över- eller underkonfidens.
    # Exempel: bias +0,20 och -0,20 ger båda absolut bias 0,20.
    abs_bias <- abs(bias)

    # Parvis bias: samma beräkning, men endast uppgifter där både konfidens
    # och rätt/fel finns. Den kan skilja sig från bias ovan om svar saknas.
    bias_paired <- if (antal_par) mean(konfidens_par) - mean(resultat_par) else NA_real_
    abs_bias_paired <- abs(bias_paired)

    # Slope: medelkonfidens för rätta svar minus medelkonfidens för felaktiga.
    # Ett högre positivt värde betyder större skillnad i säkerhet mellan rätt
    # och fel. Kräver minst ett rätt och ett fel med konfidens, annars NA.
    # Detta är ett beräknat sensitivitetmått, inte en modellerad sensitivitet.
    slope <- if (any(resultat_par == 1) && any(resultat_par == 0))
      mean(konfidens_par[resultat_par == 1]) - mean(konfidens_par[resultat_par == 0]) else NA_real_

    # Gamma är ett alternativt sensitivitetmått baserat på ordningen mellan
    # konfidens och rätt/fel. Inga jämförbara par ger NA.
    gamma <- gamma_calc(konfidens_par, resultat_par)

    # Samla alla mått för en person i den aktuella domänen.
    c(n_conf = n_conf, n_answered = n_answered, n_pairs = antal_par,
      n_correct = n_correct, n_incorrect = n_incorrect,
      conf = conf, conf_paired = if (antal_par) mean(konfidens_par) * 100 else NA_real_,
      acc = acc, acc_paired = if (antal_par) mean(resultat_par) else NA_real_,
      bias = bias, abs_bias = abs_bias, bias_paired = bias_paired,
      abs_bias_paired = abs_bias_paired, slope = slope, gamma = gamma)
  })
  measures <- as.data.frame(do.call(rbind, rows))
  names(measures) <- paste0(domain, "_", names(measures))
  number_meta <- data.frame(id = data$id, measures, check.names = FALSE)

# Resultatet number_meta innehåller bara id och mått från denna domän.
cat("\nNUMBER SERIES / TALFÖRMÅGA: antal personer =", nrow(number_meta),
    "; antal med sensitivitet =", sum(!is.na(number_meta$number_slope)), "\n")

###########################
# KLART – TRE SEPARATA TABELLER
###########################

# Antonymer_meta: verbal domän
# metal_meta: spatial domän
# number_meta: numerisk domän





