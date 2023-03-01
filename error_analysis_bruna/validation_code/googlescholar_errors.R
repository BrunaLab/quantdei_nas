
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
  mutate_all(tolower) %>%
  rename(year1 = year) %>%
  mutate(year1 = paste0(year1, "-01-01"))


# find duplicates ---------------------------------------------------------


gs_dupes <- gs %>% get_dupes(title, source)

nrow(gs)
nrow(gs_dupes)
nrow(gs_dupes) / nrow(gs) * 100


# review sources of these scientific articles -------------------------------

# most common sources
top_gs_sources <- gs %>%
  distinct(title, .keep_all = TRUE) %>%
  group_by(source) %>%
  tally() %>%
  arrange(desc(n))

# %>%
#   print(n=60)

# Quick check for any non-stem sources with key words
gs_sources_not_stem <- gs %>%
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
gs_no_source <- gs %>%
  filter(source == "" | source == "бн") %>%
  group_by(source) %>%
  tally() %>%
  arrange(desc(n))




# summary of duplicates --------------------------------------------------
gs_dupes_summary <- data.frame(n_gsdupes = nrow(gs_dupes), total_gs = nrow(gs))



# look at pubs in neurology -----------------------------------------------

gs_neurology <- gs %>%
  filter(source == "neurology") %>%
  filter(str_detect(title, "gender") == FALSE) %>%
  filter(str_detect(title, "inclusion") == FALSE) %>%
  filter(str_detect(title, "racism") == FALSE) %>%
  filter(str_detect(title, "racial") == FALSE) %>%
  filter(str_detect(title, "civil right") == FALSE) %>%
  filter(str_detect(title, "gender") == FALSE) %>%
  filter(str_detect(title, "reform") == FALSE) %>%
  filter(str_detect(title, "equality") == FALSE) %>%
  filter(str_detect(title, "black lives") == FALSE) %>%
  filter(str_detect(title, "social change") == FALSE) %>%
  filter(str_detect(title, "social justice") == FALSE) %>%
  filter(str_detect(title, "oppression") == FALSE)



# save csv files ----------------------------------------------------------

write_csv(gs_dupes_summary, "./error_analysis_bruna/validation_output/gs_dupes_summary.csv")
write_csv(gs_dupes, "./error_analysis_bruna/validation_output/gs_dupes.csv")
write_csv(top_gs_sources, "./error_analysis_bruna/validation_output/gs_sources_all_summary.csv")
write_csv(top_gs_sources_not_stem, "./error_analysis_bruna/validation_output/gs_sources_not_stem_summary.csv")
write_csv(gs_no_source, "./error_analysis_bruna/validation_output/gs_no_source_summary.csv")
write_csv(gs_neurology, "./error_analysis_bruna/validation_output/gs_neurology.csv")
