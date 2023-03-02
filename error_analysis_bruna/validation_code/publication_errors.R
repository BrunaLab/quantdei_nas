
# load libraries ----------------------------------------------------------


require(tidyverse)
require(glue)
require(here)
require(janitor)
here <- rprojroot::find_rstudio_root_file()
source(glue("{here}/code/setup.R"))

# load data ---------------------------------------------------------------


gs <- read_fst(glue("{here}/out/scholarship/google_scholar.fst")) %>%
  as_tibble() %>%
  mutate_all(tolower)

# find duplicates ---------------------------------------------------------


gs_dupes <- gs %>% get_dupes(title, source, year)

gs_deduped <- gs %>% distinct(title, source, year)




# review sources of these scientific articles -------------------------------

# most common sources
top_gs_sources <- gs_deduped %>%
  distinct(title, .keep_all = TRUE) %>%
  group_by(source) %>%
  tally() %>%
  arrange(desc(n))

# %>%
#   print(n=60)

# Quick check for any non-stem sources with key words
gs_sources_not_stem <- gs_deduped %>%
  filter(
    (
      str_detect(source, "law") |
        str_detect(source, "race") |
        str_detect(source, "class") |
        str_detect(source, "education") |
        str_detect(source, "teacher") |
        str_detect(source, "urban") |
        str_detect(source, "sociology") |
        str_detect(source, "cultural anthropology") |
        str_detect(source, "cultural studies") |
        str_detect(source, "social") |
        str_detect(source, "gender")
    ) == TRUE
  ) %>%
  filter(str_detect(source, "psychology") == FALSE) %>%
  mutate(source = gsub("the ", "", source))

top_gs_sources_not_stem <- gs_sources_not_stem %>%
  group_by(source) %>%
  tally() %>%
  arrange(desc(n))

# %>%
#   print(n=60)


# articles with no source -------------------------------------------------
gs_no_source <- gs_deduped %>%
  filter(source == "" | source == "бн") %>%
  group_by(source) %>%
  tally() %>%
  arrange(desc(n))






# look at pubs in neurology -----------------------------------------------

gs_neurology <- gs_deduped %>%
  filter(source == "neurology") %>%
  filter(str_detect(title, "1619 project") == FALSE) %>%
  filter(str_detect(title, "advocacy") == FALSE) %>%
  filter(str_detect(title, "ally") == FALSE) %>%
  filter(str_detect(title, "anti-racism") == FALSE) %>%
  filter(str_detect(title, "antiracism") == FALSE) %>%
  # filter(str_detect(title, "bias") == FALSE) %>%
  filter(str_detect(title, "black lives") == FALSE) %>%
  filter(str_detect(title, "black lives matter") == FALSE) %>%
  filter(str_detect(title, "blm") == FALSE) %>%
  filter(str_detect(title, "civil right") == FALSE) %>%
  filter(str_detect(title, "critical race theory") == FALSE) %>%
  filter(str_detect(title, "culturally sensitive") == FALSE) %>%
  filter(str_detect(title, "discrimination") == FALSE) %>%
  filter(str_detect(title, "diversity") == FALSE) %>%
  filter(str_detect(title, "diverse") == FALSE) %>%
  filter(str_detect(title, "equity") == FALSE) %>%
  filter(str_detect(title, "equality") == FALSE) %>%
  # filter(str_detect(title, "gender") == FALSE) %>%
  filter(str_detect(title, "george floyd") == FALSE) %>%
  filter(str_detect(title, "inequality") == FALSE) %>%
  filter(str_detect(title, "implicit bias") == FALSE) %>%
  filter(str_detect(title, "indigenous") == FALSE) %>%
  filter(str_detect(title, "inclusion") == FALSE) %>%
  filter(str_detect(title, "inclusive") == FALSE) %>%
  filter(str_detect(title, "intersectional") == FALSE) %>%
  filter(str_detect(title, "justice") == FALSE) %>%
  filter(str_detect(title, "kendi") == FALSE) %>%
  filter(str_detect(title, "microaggression") == FALSE) %>%
  filter(str_detect(title, "multicultural") == FALSE) %>%
  filter(str_detect(title, "oppression") == FALSE) %>%
  filter(str_detect(title, "privilege") == FALSE) %>%
  # filter(str_detect(title, "race") == FALSE) %>%
  filter(str_detect(title, "racism") == FALSE) %>%
  # filter(str_detect(title, "racial") == FALSE) %>%
  filter(str_detect(title, "racist") == FALSE) %>%
  filter(str_detect(title, "reform") == FALSE) %>%
  filter(str_detect(title, "social justice") == FALSE) %>%
  filter(str_detect(title, "social change") == FALSE) %>%
  filter(str_detect(title, "systemic racism") == FALSE) %>%
  filter(str_detect(title, "transgender") == FALSE) %>%
  # filter(str_detect(title, "trans") == FALSE) %>%
  filter(str_detect(title, "underrepresented") == FALSE) %>%
  filter(str_detect(title, "white fragility") == FALSE) %>%
  filter(str_detect(title, "white supremacy") == FALSE) %>% 
  mutate(database="Google Scholar")
  




# pubmed  -----------------------------------------------------------------



pm <- read_fst(glue("{here}/out/scholarship/pubmed.fst")) %>%
  as_tibble() %>%
  mutate_all(tolower) %>%
  rename(year = publication_year, source = journal_book)



pm_dupes <- pm %>% get_dupes(title, source, year)

pm_deduped <- pm %>% distinct(title, source, year)






names(pm)
top_pm_sources <- pm_deduped %>%
  distinct(title, .keep_all = TRUE) %>%
  group_by(source) %>%
  tally() %>%
  arrange(desc(n))
top_pm_sources

pm_sources_not_stem <- pm_deduped %>%
  filter(
    (
      str_detect(source, "law") |
        str_detect(source, "race") |
        str_detect(source, "class") |
        str_detect(source, "education") |
        str_detect(source, "teacher") |
        str_detect(source, "urban") |
        str_detect(source, "sociology") |
        str_detect(source, "cultural anthropology") |
        str_detect(source, "cultural studies") |
        str_detect(source, "social") |
        str_detect(source, "gender")
    ) == TRUE
  ) %>%
  filter(str_detect(source, "psychology") == FALSE) %>%
  mutate(source = gsub("the ", "", source))
pm_sources_not_stem

top_pm_sources_not_stem <- pm_sources_not_stem %>%
  group_by(source) %>%
  tally() %>%
  arrange(desc(n))
top_pm_sources_not_stem

#
pm_nondei <- pm_deduped %>%
  filter(
    (
      str_detect(title, "breastfeeding") |
        str_detect(title, "stroke") |
        str_detect(title, " liver ") |
        str_detect(title, " lungs ") |
        str_detect(title, "gene therapy") |
        str_detect(title, "cancer") |
        str_detect(title, "hepatitis") |
        str_detect(title, "hiv") |
        str_detect(title, "covid") |
        str_detect(title, "tuberculosis") |
        str_detect(title, "blood pressure") |
        str_detect(title, "nutrition")
    ) == TRUE
  ) %>%
  filter(str_detect(title, "1619 project") == FALSE) %>%
  filter(str_detect(title, "advocacy") == FALSE) %>%
  filter(str_detect(title, "ally") == FALSE) %>%
  filter(str_detect(title, "anti-racism") == FALSE) %>%
  filter(str_detect(title, "antiracism") == FALSE) %>%
  # filter(str_detect(title, "bias") == FALSE) %>%
  filter(str_detect(title, "black lives") == FALSE) %>%
    filter(str_detect(title, "black lives matter") == FALSE) %>%
  filter(str_detect(title, "blm") == FALSE) %>%
  filter(str_detect(title, "civil right") == FALSE) %>%
  filter(str_detect(title, "critical race theory") == FALSE) %>%
  filter(str_detect(title, "culturally sensitive") == FALSE) %>%
  filter(str_detect(title, "discrimination") == FALSE) %>%
  filter(str_detect(title, "diversity") == FALSE) %>%
  filter(str_detect(title, "diverse") == FALSE) %>%
  filter(str_detect(title, "equity") == FALSE) %>%
  filter(str_detect(title, "equality") == FALSE) %>%
  # filter(str_detect(title, "gender") == FALSE) %>%
  filter(str_detect(title, "george floyd") == FALSE) %>%
  filter(str_detect(title, "inequality") == FALSE) %>%
  filter(str_detect(title, "implicit bias") == FALSE) %>%
  filter(str_detect(title, "indigenous") == FALSE) %>%
  filter(str_detect(title, "inclusion") == FALSE) %>%
  filter(str_detect(title, "inclusive") == FALSE) %>%
  filter(str_detect(title, "intersectional") == FALSE) %>%
  filter(str_detect(title, "justice") == FALSE) %>%
  filter(str_detect(title, "kendi") == FALSE) %>%
  filter(str_detect(title, "microaggression") == FALSE) %>%
  filter(str_detect(title, "multicultural") == FALSE) %>%
  filter(str_detect(title, "oppression") == FALSE) %>%
  filter(str_detect(title, "privilege") == FALSE) %>%
  # filter(str_detect(title, "race") == FALSE) %>%
  filter(str_detect(title, "racism") == FALSE) %>%
  # filter(str_detect(title, "racial") == FALSE) %>%
  filter(str_detect(title, "racist") == FALSE) %>%
  filter(str_detect(title, "reform") == FALSE) %>%
  filter(str_detect(title, "social justice") == FALSE) %>%
  filter(str_detect(title, "social change") == FALSE) %>%
    filter(str_detect(title, "systemic racism") == FALSE) %>%
  filter(str_detect(title, "transgender") == FALSE) %>%
  # filter(str_detect(title, "trans") == FALSE) %>%
  filter(str_detect(title, "underrepresented") == FALSE) %>%
  filter(str_detect(title, "white fragility") == FALSE) %>%
  filter(str_detect(title, "white supremacy") == FALSE) %>% 
  mutate(database="PubMed")
  
# Note that "bias", "race", or "racial", or "gender" not excluded because this
# could  non-DEI medical research on such topics as racial or gender 
# differences in diagnoses, statistical biases, etc.
# manual review suggests potential errors from DEI-slipping through are minimal.
# Note also there are many non-DEI contexts in which terms above can be used, 
# this list is based oN NAS data harvesting approach and not EB judgement
# of wether they are DEI-indicators.
  
# summary of duplicates --------------------------------------------------
gs_dupes_summary <- data.frame(n_deduped = nrow(gs_deduped), n_original = nrow(gs))
gs_dupes_summary$dataset <- "gs"
pm_dupes_summary <- data.frame(n_deduped = nrow(pm_deduped), n_original = nrow(pm))
pm_dupes_summary$dataset <- "pubmed"
pubs_dupes_summary <- bind_rows(gs_dupes_summary, pm_dupes_summary) %>%
  relocate(dataset, .before = 1) %>%
  mutate(perc_inflated = (n_original-n_deduped)/n_deduped * 100) %>%
  mutate(perc_inflated = round(perc_inflated, 2))


write_csv(pubs_dupes_summary, "./error_analysis_bruna/validation_output/pubs_dupes_summary.csv")
# save csv files GS-------------------------------------------------------


write_csv(gs_dupes, "./error_analysis_bruna/validation_output/gs_dupes.csv")
write_csv(top_gs_sources, "./error_analysis_bruna/validation_output/gs_sources_all_summary.csv")
write_csv(top_gs_sources_not_stem, "./error_analysis_bruna/validation_output/gs_sources_not_stem_summary.csv")
write_csv(gs_no_source, "./error_analysis_bruna/validation_output/gs_no_source_summary.csv")
write_csv(gs_neurology, "./error_analysis_bruna/validation_output/gs_neurology_examples.csv")


# save csv files PM -------------------------------------------------------
write_csv(pm_dupes, "./error_analysis_bruna/validation_output/pm_dupes.csv")
write_csv(top_pm_sources, "./error_analysis_bruna/validation_output/pm_sources_all_summary.csv")
write_csv(top_pm_sources_not_stem, "./error_analysis_bruna/validation_output/pm_sources_not_stem_summary.csv")
write_csv(pm_nondei, "./error_analysis_bruna/validation_output/pm_nondei_examples.csv")
