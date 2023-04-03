
# load libraries ----------------------------------------------------------


require(tidyverse)
require(glue)
library(janitor)
require(here)
library(fst)

# point to root -----------------------------------------------------------


here <- rprojroot::find_rstudio_root_file()
source(glue("{here}/code/setup.R"))

# load NSF grants ---------------------------------------------------------


n <- fread(paste0(here, "/out/grants/nsf_all_grants_summary_data.csv"))
n <- n %>%
  mutate_all(tolower) %>%
  as_tibble()

# load NIH grants ---------------------------------------------------------


# install.packages("fst")
nih <- read_fst(glue("{here}/out/grants/nih_parsed_all.fst"))
nih <- nih %>%
  mutate_all(tolower) %>%
  as_tibble()



# NSF Duplicates ----------------------------------------------------------
names(n)
# df of all grants that have identical title and fund_date, exclude postdoc and grf
# which have no project title (only program)
dupes_nsf_all <- n %>%
  get_dupes(title) %>%
  filter(str_detect(title, "postdoctoral research fellowship") == FALSE) %>%
  filter(str_detect(title, "postdoctoral fellowship") == FALSE) %>%
  filter(str_detect(title, "graduate research fellowship") == FALSE) %>%
  filter(str_detect(title, "graduate reserach fellowship program") == FALSE) %>%
  filter(str_detect(title, "waterman") == FALSE) %>%
  filter(str_detect(title, "summer institute") == FALSE) %>%
  as_tibble() %>% 
  mutate(agency="nsf") %>% 
  relocate(agency,.before=1)
# nrow(dupes_nsf) / nrow(n) * 100
nsf_nodupe <- anti_join(n, dupes_nsf_all) %>% as_tibble()
dupes_nsf <- dupes_nsf_all %>% distinct(title)
nsf_nodupe <- bind_rows(nsf_nodupe, dupes_nsf)
nrow(nsf_nodupe)
# # The percentage of the nsf file that is duplicated
# nrow(n)-nrow(nsf_nodupe)
# 
# 100-(nrow(nsf_nodupe) / nrow(n) * 100)


dupes_nsf_slim <- nsf_nodupe %>% distinct(title)

# look for any that might have snuck through
last_dupes <- nsf_nodupe %>%
  as_tibble() %>%
  filter(str_detect(title, "postdoctoral research fellowship") == FALSE) %>%
  filter(str_detect(title, "postdoctoral fellowship") == FALSE) %>%
  filter(str_detect(title, "graduate research fellowship") == FALSE) %>%
  filter(str_detect(title, "graduate reserach fellowship program") == FALSE) %>%
  filter(str_detect(title, "waterman") == FALSE) %>%
  filter(str_detect(title, "summer institute") == FALSE) %>%
  group_by(title) %>%
  tally() %>%
  arrange(desc(n)) %>%
  filter(n > 1)
last_dupes
# there might still be duplicates if any with identical title
# had different start date an easy way to id at least some of these
# is the "collaborative research:" prefix

# collab<-nsf_nodupe %>%
#   filter(str_detect(title,"collaborative research")==TRUE) %>%
#   group_by(title) %>% slice(1)
#
# nsf_deduped <- nsf_deduped %>%
#   filter(str_detect(title,"collaborative research")==FALSE) %>%
#   bind_rows(collab)
# #


dupes_nsf

# 
# 
# 
# nsf_perc_duplicated <- (nrow(dupes_nsf) / nrow(n)) * 100 %>% as_tibble()
# nsf_perc_duplicated <- nsf_perc_duplicated %>%
#   rename("percent_duplicated_rows" = "value") %>%
#   mutate(agency = "nsf") %>%
#   relocate(agency, .before = 1)
# 

# NIH duplicates ----------------------------------------------------------


# total duplicated records
dupes_nih_all <- nih %>%
  get_dupes(project_title, program_official_information, project_start_date) %>% 
  mutate(agency="nih") %>% 
  relocate(agency,.before=1)

names(nih)

nrow(dupes_nih_all) / nrow(nih) * 100
nih_deduped <- nih %>%
  group_by(project_title, program_official_information) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  distinct(project_title)

# nih_perc_duplicated <- (1 - (nrow(nih_deduped) / nrow(nih))) * 100 %>% as_tibble()
# nih_perc_duplicated <- nih_perc_duplicated %>%
#   rename("percent_duplicated_rows" = "value") %>%
#   mutate(agency = "nih") %>%
#   relocate(agency, .before = 1)
# nih_perc_duplicated

# bind nih and nsh dupes together ------------------------------------------

nsf_dupes_summary <- data.frame(n_deduped = nrow(nsf_nodupe), n_original = nrow(n))
nsf_dupes_summary$dataset <- "NSF"
nih_dupes_summary <- data.frame(n_deduped = nrow(nih_deduped), n_original = nrow(nih))
nih_dupes_summary$dataset <- "NIH"
grant_dupes_summary <- bind_rows(nsf_dupes_summary, nih_dupes_summary) %>%
  relocate(dataset, .before = 1) %>%
  mutate(perc_inflated = (n_original-n_deduped)/n_deduped * 100) %>%
  mutate(perc_inflated = round(perc_inflated, 2))

write_csv(grant_dupes_summary, "./error_analysis_bruna/validation_output/grant_dupes_summary.csv")



duplicated_records <- bind_rows(dupes_nsf_all, dupes_nih_all)


write_csv(duplicated_records, "./error_analysis_bruna/validation_output/grants_dupes.csv")





# Find all nsf "DIVERSITY" Grants -------------------------------------------

# all nsf grants with "diversity" in title
# TODO: filter with across or if_any to streamline
diversity_nsf <- nsf_nodupe %>%
  filter(diversity == 1) %>%
  select(-diversity) %>%
  filter(equity == 0 &
           bias == 0 &
           transgender == 0 &
           systemic == 0 &
           racism == 0 &
           discrimination == 0 &
           `social justice` == 0 &
           justice == 0 &
           racist == 0 &
           multicultural == 0 &
           privilege == 0 &
           Kendi == 0 &
           ally == 0 &
           race == 0 &
           intersectional == 0 &
           `implicit bias` == 0 &
           gender == 0) %>%
  select(title, fund_date, amount)


# nsf diversity grants that have biodiversity-related terms


# DEI terms ---------------------------------------------------------------
dei_terms <- c("1619 project","anti-racism", "antiracism",
               "bias", "black lives", "black lives matter","blm", "civil right",
               "critical race theory","culturally sensitive","discrimination",
               "equality", "gender", "george floyd", "minority",
               "inequality","implicit bias","indigenous", 
               "inclusion", "intersectional", "inclusive", 
               "kendi","microaggression","multicultural", "oppression", 
               "racism", "racial", "racist", "reform", "social justice",
               "social change", "systemic racism","transgender", 
               # "trans", # could be transfer student
               "underrepresented","white fragility", "white supremacy")

# Will need to add there to the searches below EXCEPT for the one that is the 
# subject of that specific search:
# dei_added<-c("advocacy","ally","race",
#              "equity","justice","privilege","diversity","diverse")

# "race" errors -----------------------------------------------------------

diversity_terms = c("genetic", "species", "dna","rna ", "ecology", "food web",
                    "herbivor", "mammal", "insect", "mutualis","habitat", "avian",
                    "taxonom", "wildlife", "evolution","microb","phylogen", 
                    "biogeogr", "biogeoch", "tropical", "genom", "biotic",
                    "host", "arctic", "networks", "parasite", "biodivers", 
                    "species", "insect", "mammal", "ecology", "evolutio", "evolv",
                    "bird", "snake", "frog", "lizard", "earth", "genetic", 
                    "botan", "plant")

# add the additional search terms EXCEPT race
dei_terms_diversity<-c(dei_terms,c("advocacy","ally","equity","justice",
                                   "privilege","diverse", "enhancing diversity"))


diversity_nsf_fail <- diversity_nsf %>%
  filter(str_detect(title, paste(diversity_terms, collapse = '|'))) %>%
  filter(!str_detect(title, paste(dei_terms_diversity, collapse = '|')))


diversity_terms_wide<-c(" ")
diversity_nsf_fail_wide <- diversity_nsf %>%
  filter(str_detect(title, paste(diversity_terms_wide, collapse = '|'))) %>%
  filter(!str_detect(title, paste(dei_terms_diversity, collapse = '|')))



# 
# 
# diversity_nsf_fail <- diversity_nsf %>%
#   filter(
#     (
#       str_detect(title, "genetic") |
#         str_detect(title, "species") |
#         str_detect(title, "dna") |
#         str_detect(title, "rna ") | # note space
#         str_detect(title, "ecology") |
#         str_detect(title, "food web") |
#         str_detect(title, "herbivor") |
#         str_detect(title, "mammal") |
#         str_detect(title, "insect") |
#         str_detect(title, "mutualis") |
#         str_detect(title, "habitat") |
#         str_detect(title, "avian") |
#         str_detect(title, "taxonom") |
#         str_detect(title, "wildlife") |
#         str_detect(title, "evolution") |
#         str_detect(title, "microb") |
#         str_detect(title, "phylogen") |
#         str_detect(title, "biogeograph") |
#         str_detect(title, "biogeochemist") |
#         str_detect(title, "tropical") |
#         str_detect(title, "genom") |
#         str_detect(title, "biotic") |
#         str_detect(title, "host") |
#         str_detect(title, "arctic") |
#         str_detect(title, "networks") |
#         str_detect(title, "parasite") |
#         str_detect(title, "biodiversity") |
#         str_detect(title, "species") |
#         str_detect(title, "insect") |
#         str_detect(title, "mammal") |
#         str_detect(title, "ecology") |
#         str_detect(title, "evolution") |
#         str_detect(title, "evolv") |
#         str_detect(title, "bird") |
#         str_detect(title, "snake") |
#         str_detect(title, "frog") |
#         str_detect(title, "lizard") |
#         str_detect(title, "earth") |
#         str_detect(title, "genetic") |
#         str_detect(title, "botan") |
#         str_detect(title, "biodiversity") |
#         str_detect(title, "plant")
#     ) == TRUE
#   ) %>%
#   filter(str_detect(title, "1619 project") == FALSE) %>%
#   filter(str_detect(title, "advocacy") == FALSE) %>%
#   filter(str_detect(title, "ally") == FALSE) %>%
#   filter(str_detect(title, "anti-racism") == FALSE) %>%
#   filter(str_detect(title, "antiracism") == FALSE) %>%
#   filter(str_detect(title, "bias") == FALSE) %>%
#   filter(str_detect(title, "black lives") == FALSE) %>%
#   filter(str_detect(title, "black lives matter") == FALSE) %>%
#   filter(str_detect(title, "blm") == FALSE) %>%
#   filter(str_detect(title, "civil right") == FALSE) %>%
#   filter(str_detect(title, "critical race theory") == FALSE) %>%
#   filter(str_detect(title, "culturally sensitive") == FALSE) %>%
#   filter(str_detect(title, "discrimination") == FALSE) %>%
#   filter(str_detect(title, "pathways") == FALSE) %>%
#   filter(str_detect(title, "enhancing diversity") == FALSE) %>%
#   # filter(str_detect(title, "diverse") == FALSE) %>%
#   filter(str_detect(title, "equity") == FALSE) %>%
#   filter(str_detect(title, "equality") == FALSE) %>%
#   filter(str_detect(title, "gender") == FALSE) %>%
#   filter(str_detect(title, "george floyd") == FALSE) %>%
#   filter(str_detect(title, "inequality") == FALSE) %>%
#   filter(str_detect(title, "implicit bias") == FALSE) %>%
#   filter(str_detect(title, "indigenous") == FALSE) %>%
#   filter(str_detect(title, "inclusion") == FALSE) %>%
#   filter(str_detect(title, "inclusive") == FALSE) %>%
#   filter(str_detect(title, "intersectional") == FALSE) %>%
#   filter(str_detect(title, "justice") == FALSE) %>%
#   filter(str_detect(title, "kendi") == FALSE) %>%
#   filter(str_detect(title, "microaggression") == FALSE) %>%
#   filter(str_detect(title, "multicultural") == FALSE) %>%
#   filter(str_detect(title, "oppression") == FALSE) %>%
#   filter(str_detect(title, "privilege") == FALSE) %>%
#   filter(str_detect(title, "race") == FALSE) %>%
#   filter(str_detect(title, "racism") == FALSE) %>%
#   filter(str_detect(title, "racial") == FALSE) %>%
#   filter(str_detect(title, "racist") == FALSE) %>%
#   filter(str_detect(title, "reform") == FALSE) %>%
#   filter(str_detect(title, "social justice") == FALSE) %>%
#   filter(str_detect(title, "social change") == FALSE) %>%
#   filter(str_detect(title, "systemic racism") == FALSE) %>%
#   filter(str_detect(title, "transgender") == FALSE) %>%
#   filter(str_detect(title, "minority") == FALSE) %>%
#   filter(str_detect(title, "trans") == FALSE) %>%
#   filter(str_detect(title, "underrepresented") == FALSE) %>%
#   filter(str_detect(title, "white fragility") == FALSE) %>%
#   filter(str_detect(title, "white supremacy") == FALSE) 

# Note this doesnt include any of the DIGG, post, or predoc

# calculating proportion
diversity_nsf_fail_perc <- (nrow(diversity_nsf_fail) / nrow(diversity_nsf)) * 100
diversity_nsf_fail_perc

diversity_nsf_fail_summary <- data.frame(not_dei = nrow(diversity_nsf_fail),n_diversity_deduped = nrow(diversity_nsf))
diversity_nsf_fail_summary$search <- "limited"
diversity_nsf_fail_summary_wide <- data.frame(not_dei = nrow(diversity_nsf_fail_wide),n_diversity_deduped = nrow(diversity_nsf))
diversity_nsf_fail_summary_wide$search <- "wide"

diversity_nsf_fail_summary <- bind_rows(diversity_nsf_fail_summary, diversity_nsf_fail_summary_wide) %>%
  relocate(search, .before = 1) %>%
  mutate(perc_incorrect = (not_dei)/n_diversity_deduped * 100) %>%
  mutate(perc_incorrect = round(perc_incorrect, 2))


write_csv(diversity_nsf_fail, "./error_analysis_bruna/validation_output/grants_nsf_diversity.csv")
write_csv(diversity_nsf_fail_wide, "./error_analysis_bruna/validation_output/grants_nsf_diversity_wide.csv")
write_csv(diversity_nsf_fail_summary, "./error_analysis_bruna/validation_output/diversity_nsf_fail_summary.csv")
