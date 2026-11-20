# 9_deskriptivt_och_datakontroll.R
# Beskrivning och kontroll före modellerna för metakognition och inlärning.
# Kör efter 8_merge_learning.R. Ingen ändring görs i analysfilerna.

library(dplyr)
library(tidyr)
library(ggplot2)

# Kontrollera sökvägen om filerna har sparats på en annan plats.
data_dir <- "/safe/data/Buratti/Mcoglearning/data"
output_dir <- file.path(data_dir, "deskriptivt_mconlearning")
if (!dir.exists(data_dir)) stop("Datakatalogen finns inte: ", data_dir)
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
if (!dir.exists(output_dir)) stop("Kunde inte skapa resultatmappen: ", output_dir)

files <- file.path(data_dir, c("df_mconlearning.rds", "word_learning_long.rds",
                              "food_ordering_long.rds"))
if (any(!file.exists(files)))
  stop("Saknade analysfiler: ", paste(files[!file.exists(files)], collapse = ", "),
       ". Kontrollera var skript 8 sparade filerna.")
df <- readRDS(files[1])
word_long <- readRDS(files[2])
food_long <- readRDS(files[3])

require_cols <- function(data, cols, name) {
  missing <- setdiff(cols, names(data))
  if (length(missing)) stop(name, " saknar: ", paste(missing, collapse = ", "))
}
meta <- c("antonyms_abs_bias_paired", "antonyms_slope",
          "metal_abs_bias_paired", "metal_slope",
          "number_abs_bias_paired", "number_slope")
require_cols(df, c("id", meta, "antonyms_n_pairs", "metal_n_pairs", "number_n_pairs",
                   "antonyms_n_correct", "antonyms_n_incorrect", "metal_n_correct",
                   "metal_n_incorrect", "number_n_correct", "number_n_incorrect",
                   "sumopposites", "summetal", "sumnumber", "growthmindset"), "df")
require_cols(word_long, c("id", "forsok", "antal_ratt"), "word_long")
require_cols(food_long, c("id", "forsok", "trial", "logtrial"), "food_long")
if (anyNA(df$id) || anyDuplicated(df$id)) stop("df har saknade eller duplicerade id.")

# Datakvalitet: kontrollera dubbletter och giltiga värden utan att visa person-id.
check_long <- function(x, allowed, outcome, low, high, name) {
  if (anyNA(x$id) || anyNA(x$forsok)) stop(name, ": id eller försök saknas.")
  if (anyDuplicated(x[c("id", "forsok")])) stop(name, ": dubbla kombinationer av id och försök.")
  if (any(!x$forsok %in% allowed)) stop(name, ": ogiltiga försöksnummer.")
  if (any(!is.finite(x[[outcome]]) | x[[outcome]] < low | x[[outcome]] > high, na.rm = TRUE))
    stop(name, ": kontrollera ogiltiga värden i ", outcome)
  if (any(!x$id %in% df$id)) stop(name, ": id saknas i personfilen.")
}
check_long(word_long, 0:2, "antal_ratt", 0, 10, "word_long")
check_long(food_long, 0:7, "trial", .Machine$double.xmin, Inf, "food_long")
if (any(!is.finite(food_long$logtrial))) stop("Kontrollera logtrial: icke-ändliga värden.")
if (any(word_long$antal_ratt != floor(word_long$antal_ratt), na.rm = TRUE))
  stop("Antal rätt måste vara heltal.")

# Beskrivning per försök. Antalet kan variera mellan försök.
summary_by_trial <- function(x, outcome, label) {
  x %>% filter(!is.na(.data[[outcome]])) %>%
    group_by(forsok) %>%
    summarise(antal_personer = n_distinct(id), medel = mean(.data[[outcome]]),
              sd = sd(.data[[outcome]]), median = median(.data[[outcome]]),
              min = min(.data[[outcome]]), max = max(.data[[outcome]]), .groups = "drop") %>%
    mutate(uppgift = label, forsok_nummer = forsok + 1L, .before = 1)
}
word_summary <- summary_by_trial(word_long, "antal_ratt", "Ordinlärning")
food_summary <- summary_by_trial(food_long, "trial", "Meny tid")
food_log_summary <- summary_by_trial(food_long, "logtrial", "Meny log tid gånger 10")
write.csv(bind_rows(word_summary, food_summary, food_log_summary),
          file.path(output_dir, "resultat_per_forsok.csv"), row.names = FALSE, na = "")

# Figur över medelvärdena; ingen inferens eller korrigering för bortfall.
p_word <- ggplot(word_summary, aes(forsok_nummer, medel)) +
  geom_line() + geom_point(size = 2) + scale_x_continuous(breaks = 1:3) +
  scale_y_continuous(limits = c(0, 10)) +
  labs(title = "Ordinlärning", x = "Försök", y = "Genomsnittligt antal rätt") + theme_minimal()
p_food <- ggplot(filter(food_summary, forsok_nummer <= 4), aes(forsok_nummer, medel)) +
  geom_line() + geom_point(size = 2) + scale_x_continuous(breaks = 1:4) +
  labs(title = "Menyuppgift: samma beställning", x = "Försök", y = "Genomsnittlig tid (originalenhet)") + theme_minimal()
ggsave(file.path(output_dir, "ordinlarning.png"), p_word, width = 7, height = 5, dpi = 150)
ggsave(file.path(output_dir, "menyuppgift_forsok_1_4.png"), p_food, width = 7, height = 5, dpi = 150)

# Beskrivning av monitorering inklusive antal matchade uppgifter.
metric_cols <- c(meta, "antonyms_n_pairs", "metal_n_pairs", "number_n_pairs")
meta_summary <- df %>% select(all_of(metric_cols)) %>%
  pivot_longer(everything(), names_to = "matt", values_to = "varde") %>%
  group_by(matt) %>% summarise(
    totalt = n(), antal = sum(!is.na(varde)), saknas = sum(is.na(varde)),
    medel = if (all(is.na(varde))) NA_real_ else mean(varde, na.rm = TRUE),
    sd = if (sum(!is.na(varde)) < 2) NA_real_ else sd(varde, na.rm = TRUE),
    median = if (all(is.na(varde))) NA_real_ else median(varde, na.rm = TRUE),
    min = if (all(is.na(varde))) NA_real_ else min(varde, na.rm = TRUE),
    max = if (all(is.na(varde))) NA_real_ else max(varde, na.rm = TRUE), .groups = "drop")
write.csv(meta_summary, file.path(output_dir, "monitorering_beskrivning.csv"), row.names = FALSE, na = "")

# Frekvensfördelning för antal matchade svar, rätt och fel per domän.
counts <- c("antonyms_n_pairs", "antonyms_n_correct", "antonyms_n_incorrect",
            "metal_n_pairs", "metal_n_correct", "metal_n_incorrect",
            "number_n_pairs", "number_n_correct", "number_n_incorrect")
count_table <- df %>% select(all_of(counts)) %>%
  pivot_longer(everything(), names_to = "matt", values_to = "antal_svar") %>%
  count(matt, antal_svar, name = "antal_personer")
write.csv(count_table, file.path(output_dir, "monitorering_antal_svar.csv"), row.names = FALSE, na = "")

# Urval per uppgift och monitoreringsmått. Kognition är här de tre samtida
# summapoängen; growthmindset är ett exempel på motivationsmått. Detta är
# endast en inventering, inte en fastslagen specifikation av modellerna.
word_ids <- unique(word_long$id[!is.na(word_long$antal_ratt)])
food_ids <- unique(food_long$id[food_long$forsok %in% 0:3 & !is.na(food_long$trial)])
coverage <- bind_rows(lapply(list(Ordinlarning = word_ids, Meny_forsok_1_4 = food_ids), function(idset) {
  bind_rows(lapply(meta, function(m) {
    x <- df[df$id %in% idset, , drop = FALSE]
    has_meta <- !is.na(x[[m]])
    has_cognition <- complete.cases(x[, c("sumopposites", "summetal", "sumnumber")])
    has_motivation <- !is.na(x$growthmindset)
    data.frame(monitoreringsmatt = m, med_utfall = nrow(x),
               med_monitorering = sum(has_meta),
               med_monitorering_och_kognition = sum(has_meta & has_cognition),
               med_monitorering_kognition_motivation = sum(has_meta & has_cognition & has_motivation))
  }))
}), .id = "uppgift")
write.csv(coverage, file.path(output_dir, "analysunderlag.csv"), row.names = FALSE, na = "")

cat("\nAntal personer i personfil:", nrow(df), "\n")
print(word_summary)
print(food_summary)
print(meta_summary)
print(coverage)
cat("\nTabeller och figurer sparade i:", output_dir, "\n")
cat("Kontrollera urvalskriterierna i skript 5 före tolkning av resultaten.\n")
