require(tidyverse)
require(glue)
require(here)
require(janitor)
here <- rprojroot::find_rstudio_root_file()
source(glue("{here}/code/setup.R"))

gs <- read_fst(glue("{here}/out/scholarship/google_scholar.fst")) %>% 
  as_tibble() %>%
  mutate_all(tolower) %>% 
  rename(year1 = year) %>%
  mutate(year1 = paste0(year1,"-01-01"))


gs_dupes<-gs %>% get_dupes(title,source)
nrow(gs)
nrow(gs_dupes)
nrow(gs_dupes)/nrow(gs)


gs_sources<-gs %>%
  distinct(title,.keep_all = TRUE) %>% 
  group_by(source) %>% 
  tally() %>% 
  arrange(desc(n)) %>% 
  # print(n=50) %>% 
  filter(
    (
      str_detect(source,"law")|
        str_detect(source,"cultural studies")|
        str_detect(source,"social")|
        str_detect(source,"gender")
    )==TRUE) %>% 
  filter(str_detect(source,"psychology")==FALSE) 

gs_sources
gs_dupes_summary<-data.frame(n_gsdupes=nrow(gs_dupes),total_gs=nrow(gs))
write_csv(gs_dupes_summary,"./error_analysis_bruna/gs_dupes_summary.csv")
write_csv(gs_dupes,"./error_analysis_bruna/gs_dupes.csv")
write_csv(gs_sources,"./error_analysis_bruna/gs_sources.csv")

