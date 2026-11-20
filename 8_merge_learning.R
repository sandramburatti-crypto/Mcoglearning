# Sammanfogning av analysdata för metakognition och inlärning
# Kör efter prepareringsskripten 1–7; se kommentarerna nedan.
# Behåller en rad per person och skapar två dataset i långt format.

# KÖRORDNING: Kör skripten 1–7 före detta skript. Skript 2 bevarar
# opposites_items, metal_items och number_items; skript 7 använder dem
# för att skapa Antonymer_meta, metal_meta och number_meta.

library(dplyr)
library(tidyr)

# Skalor som faktiskt skapas i 6_constructingscales.R.
skalor <- c("hopeforsuccess", "fearoffailure", "learninganxiety",
            "learningboredome", "learningenjoyment", "learninghope",
            "growthmindset", "metaknowledgeofcognition",
            "metaregulationofcognition")

# Kontrollera att varje datakälla har unikt och icke-saknat id. Detta hindrar
# att sammanslagningar skapar dubbletter och ändrar analysens antal personer.
check_id <- function(x, namn) {
  if (!"id" %in% names(x)) stop(namn, " saknar id.")
  if (anyNA(x$id)) stop(namn, " har saknat id.")
  if (anyDuplicated(x$id)) stop(namn, " har duplicerat id; undersök före merge.")
}

required_objects <- c("survey", "foodordering", "wordlearning", "metal",
                      "number", "opposites", "Antonymer_meta", "metal_meta",
                      "number_meta", "ugu", "fob", "charlson")
missing_objects <- required_objects[!vapply(required_objects, exists, logical(1),
                                              inherits = TRUE)]
if (length(missing_objects))
  stop("Saknade objekt: ", paste(missing_objects, collapse = ", "))

sources <- mget(required_objects, inherits = TRUE)
for (namn in names(sources)) check_id(sources[[namn]], namn)

# Kräv de variabler som modellerna bygger på, så äldre versioner av 5_prep
# inte tyst ger ett ofullständigt analysunderlag.
required_columns <- list(
  survey = c("id", skalor),
  foodordering = c("id", "auc_seq", paste0("trial", 1:8), paste0("logtrial", 1:8)),
  wordlearning = c("id", "auc_glos", paste0("correctt", 1:3)),
  metal = c("id", "summetal"), number = c("id", "sumnumber"),
  opposites = c("id", "sumopposites"),
  Antonymer_meta = c("id", "antonyms_abs_bias_paired", "antonyms_slope"),
  metal_meta = c("id", "metal_abs_bias_paired", "metal_slope"),
  number_meta = c("id", "number_abs_bias_paired", "number_slope"),
  ugu = c("id", "Cohort", "RSSEX", "opposites1", "metalfolding1", "numberseries1"),
  fob = c("id", "UtbNiva"), charlson = c("id", "CCIw")
)
for (namn in names(required_columns)) {
  missing_columns <- setdiff(required_columns[[namn]], names(sources[[namn]]))
  if (length(missing_columns))
    stop(namn, " saknar: ", paste(missing_columns, collapse = ", "))
}

# Ett brett personregister. Full join används för datakällor i uppföljningen:
# personer behålls även om en viss uppgift eller enkät saknas. UGU och register
# läggs till för dessa personer med left join; id sparas för senare analyser.
# Metadata från 7 behålls i sin helhet, inklusive antal svar, bias och gamma.
parts <- list(
  survey = survey %>% select(id, all_of(skalor)),
  foodordering = foodordering,
  wordlearning = wordlearning,
  metal = metal %>% select(id, summetal),
  number = number %>% select(id, sumnumber),
  opposites = opposites %>% select(id, sumopposites),
  Antonymer_meta = Antonymer_meta,
  metal_meta = metal_meta,
  number_meta = number_meta
)

# Stoppa vid andra överlappande namn än id, i stället för att tyst skapa .x/.y.
merge_checked <- function(x, y, namn, join = dplyr::full_join) {
  overlap <- intersect(setdiff(names(x), "id"), setdiff(names(y), "id"))
  if (length(overlap)) stop("Överlappande variabler vid merge med ", namn, ": ",
                            paste(overlap, collapse = ", "))
  join(x, y, by = "id")
}

df <- parts[[1]]
for (namn in names(parts)[-1]) df <- merge_checked(df, parts[[namn]], namn)
# UGU innehåller även skolresultat och föräldrautbildning från 3_prepugu.R.
df <- merge_checked(df, ugu, "ugu", dplyr::left_join)
df <- merge_checked(df, fob %>% select(id, UtbNiva), "fob", dplyr::left_join)
df <- merge_checked(df, charlson %>% select(id, CCIw), "charlson", dplyr::left_join)
check_id(df, "df efter merge")

# Långt format: en rad per person och försök med observerat utfall.
# Personvariabler upprepas för varje försök. Försök kodas 0 vid första försöket.
# För menyuppgiften behålls alla 8 försök; filtrera forsok <= 3 för analysen
# av de första fyra upprepningarna av samma beställning.
persondata <- df %>%
  select(-all_of(c(paste0("trial", 1:8), paste0("logtrial", 1:8),
                   paste0("correctt", 1:3))))

word_long <- df %>%
  select(id, all_of(paste0("correctt", 1:3))) %>%
  pivot_longer(-id, names_to = "forsok_namn", values_to = "antal_ratt") %>%
  mutate(forsok = as.integer(sub("correctt", "", forsok_namn)) - 1L) %>%
  filter(!is.na(antal_ratt)) %>%
  select(-forsok_namn) %>%
  left_join(persondata, by = "id")

food_long <- df %>%
  select(id, all_of(paste0("trial", 1:8)), all_of(paste0("logtrial", 1:8))) %>%
  pivot_longer(-id, names_to = c(".value", "forsok_nr"),
               names_pattern = "^(logtrial|trial)([1-8])$") %>%
  mutate(forsok = as.integer(forsok_nr) - 1L) %>%
  filter(!is.na(trial)) %>%
  select(-forsok_nr) %>%
  left_join(persondata, by = "id")

# logtrial är log(tid) * 10 enligt 5_prep; dela med 10 om naturlig log önskas.
# Ingen ytterligare exkludering eller standardisering görs här. Urval och
# tillräckligt antal konfidensbedömningar fastställs inför varje modell.
cat("Personer i df:", nrow(df), "\n")
cat("Ordinlärning: personer =", n_distinct(word_long$id),
    ", observationer =", nrow(word_long), "\n")
cat("Menyuppgift: personer =", n_distinct(food_long$id),
    ", observationer =", nrow(food_long), "\n")

# Anpassa sökvägen om skriptet körs utanför er säkra analysmiljö.
output_dir <- "/safe/data/Buratti/Mcoglearning/data"
if (!dir.exists(output_dir)) stop("Utdatakatalogen finns inte: ", output_dir)
saveRDS(df, file.path(output_dir, "df_mconlearning.rds"))
saveRDS(word_long, file.path(output_dir, "word_learning_long.rds"))
saveRDS(food_long, file.path(output_dir, "food_ordering_long.rds"))
