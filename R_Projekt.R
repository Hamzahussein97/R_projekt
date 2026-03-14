install.packages(c(
  "tidyverse",
  "tidytext",
  "quanteda",
  "ggplot2",
  "manifestoR"
))

library(tidyverse)
library(tidytext)
library(quanteda)
library(ggplot2)
library(manifestoR)

dir.create("results", showWarnings = FALSE)

mp_setapikey(key = "e77533f006772644ee951c7924381a40")

mpds <- mp_maindataset()

wanted_meta <- mpds %>%
  filter(
    countryname == "Germany",
    party %in% c(41521, 41953)
  ) %>%
  mutate(year = format(edate, "%Y")) %>%
  filter(year %in% c("2013", "2017", "2021", "2025")) %>%
  select(party, partyname, date, edate, year) %>%
  arrange(party, edate)

corp <- mp_corpus(wanted_meta)

corp_list <- Map(function(doc, id) {
  df <- as.data.frame(doc)
  df$doc_id <- id
  df
}, corp, names(corp))

corp_df <- bind_rows(corp_list)

meta_lookup <- wanted_meta %>%
  mutate(
    doc_id = paste0(party, "_", date),
    party_label = case_when(
      party == 41521 ~ "CDU/CSU",
      party == 41953 ~ "AfD",
      TRUE ~ as.character(party)
    )
  ) %>%
  select(doc_id, party, party_label, year, edate)

corp_df <- corp_df %>%
  left_join(meta_lookup, by = "doc_id")

chunk_size <- 10

manifesto_chunks <- corp_df %>%
  filter(!is.na(text), text != "", str_length(text) > 5) %>%
  group_by(doc_id) %>%
  arrange(pos, .by_group = TRUE) %>%
  mutate(chunk_id = ceiling(row_number() / chunk_size)) %>%
  group_by(doc_id, party, party_label, year, edate, chunk_id) %>%
  summarise(
    document = paste(text, collapse = " "),
    .groups = "drop"
  ) %>%
  mutate(chunk_doc_id = paste(doc_id, chunk_id, sep = "_"))

theme_dictionary <- tibble::tribble(
  ~theme, ~keyword,
  "Migration_Asyl", "migration",
  "Migration_Asyl", "migrant",
  "Migration_Asyl", "migranten",
  "Migration_Asyl", "einwanderung",
  "Migration_Asyl", "zuwanderung",
  "Migration_Asyl", "asyl",
  "Migration_Asyl", "asylbewerber",
  "Migration_Asyl", "asylbewerbern",
  "Migration_Asyl", "asylrecht",
  "Migration_Asyl", "flüchtling",
  "Migration_Asyl", "flüchtlinge",
  "Migration_Asyl", "schutzsuchende",
  "Migration_Asyl", "abschiebung",
  "Migration_Asyl", "abschiebungen",
  "Migration_Asyl", "grenze",
  "Migration_Asyl", "grenzen",
  "Migration_Asyl", "integration",
  "Innere_Sicherheit", "sicherheit",
  "Innere_Sicherheit", "innere",
  "Innere_Sicherheit", "schutz",
  "Innere_Sicherheit", "polizei",
  "Innere_Sicherheit", "kriminalität",
  "Innere_Sicherheit", "kriminalitaet",
  "Innere_Sicherheit", "terror",
  "Innere_Sicherheit", "terrorismus",
  "Innere_Sicherheit", "gewalt",
  "Innere_Sicherheit", "extremismus",
  "Innere_Sicherheit", "ordnung",
  "Innere_Sicherheit", "recht",
  "Innere_Sicherheit", "rechtsstaat",
  "Innere_Sicherheit", "kontrolle",
  "Innere_Sicherheit", "überwachung",
  "Innere_Sicherheit", "ueberwachung",
  "EU", "eu",
  "EU", "europa",
  "EU", "europäisch",
  "EU", "europäische",
  "EU", "europäischen",
  "EU", "europaeisch",
  "EU", "europaeische",
  "EU", "europaeischen",
  "EU", "union",
  "EU", "brüssel",
  "EU", "bruessel",
  "EU", "euro",
  "EU", "binnenmarkt",
  "EU", "mitgliedstaaten",
  "Wirtschaft", "wirtschaft",
  "Wirtschaft", "unternehmen",
  "Wirtschaft", "arbeit",
  "Wirtschaft", "arbeitsplätze",
  "Wirtschaft", "arbeitsplaetze",
  "Wirtschaft", "wachstum",
  "Wirtschaft", "investitionen",
  "Wirtschaft", "investition",
  "Wirtschaft", "industrie",
  "Wirtschaft", "mittelstand",
  "Wirtschaft", "steuern",
  "Wirtschaft", "steuer",
  "Wirtschaft", "markt",
  "Wirtschaft", "wohlstand",
  "Wirtschaft", "forschung",
  "Wirtschaft", "innovation",
  "Wirtschaft", "digitalisierung"
)

keyword_docs <- manifesto_chunks %>%
  select(chunk_doc_id, party_label, year, document) %>%
  unnest_tokens(word, document) %>%
  mutate(word = str_to_lower(word)) %>%
  filter(str_detect(word, "[a-zäöüß]"))

theme_hits <- keyword_docs %>%
  inner_join(theme_dictionary, by = c("word" = "keyword")) %>%
  count(chunk_doc_id, party_label, year, theme, name = "theme_count")

doc_lengths <- keyword_docs %>%
  count(chunk_doc_id, name = "total_words")

theme_results <- theme_hits %>%
  left_join(doc_lengths, by = "chunk_doc_id") %>%
  mutate(relative_freq = theme_count / total_words)

theme_summary <- theme_results %>%
  group_by(party_label, year, theme) %>%
  summarise(
    mean_theme_count = mean(theme_count),
    mean_relative_freq = mean(relative_freq),
    total_mentions = sum(theme_count),
    .groups = "drop"
  ) %>%
  arrange(theme, year, party_label)

theme_top_words <- keyword_docs %>%
  inner_join(theme_dictionary, by = c("word" = "keyword")) %>%
  count(theme, word, sort = TRUE) %>%
  group_by(theme) %>%
  slice_max(n, n = 15) %>%
  ungroup()

write.csv(theme_summary, "results/theme_summary.csv", row.names = FALSE)
write.csv(theme_top_words, "results/theme_top_words.csv", row.names = FALSE)

p_rel <- ggplot(theme_summary, aes(x = factor(year), y = mean_relative_freq, fill = party_label)) +
  geom_col(position = "dodge") +
  facet_wrap(~ theme, scales = "free_y") +
  labs(
    title = "Relative thematische Gewichtung nach Partei und Wahljahr",
    x = "Wahljahr",
    y = "Durchschnittliche relative Häufigkeit",
    fill = "Partei"
  )

ggsave("results/relative_thematic_weight.png", plot = p_rel, width = 12, height = 8)

print(wanted_meta)
print(table(manifesto_chunks$party_label, manifesto_chunks$year))
print(theme_summary, n = 100)
print(theme_top_words, n = 100)