
# load required libraries -------------------------------------------------

require(tidyverse)
require(glue)
require(here)
library(textclean)

# point to root -----------------------------------------------------------


here <- rprojroot::find_rstudio_root_file()
source(glue("{here}/code/setup.R"))

# load "clean tweets" -----------------------------------------------------

t <- fread(glue("{here}/out/twitter/tweets_clean.csv")) 
# names(t)


# column for keyword ------------------------------------------------------


t <- t %>%
  separate(file, c("one", "account", "category"), "--", remove = FALSE) %>% # split source to isolate kw
  select(-one) %>% # delete unused portions
  relocate(category, account, .before = 1) %>%
  mutate(category = str_sub(category, 1, -5)) %>% # remove .Rds
  mutate_all(tolower) %>% 
  mutate_all(trimws) 

# The duplicate tweets are mostly retweets or repeat of theme tweets
# (eg, black history month) shared several times. Will not treat these
# as true duplicates, as argument could be made that each tweet is a unique
# repeat of DEI message

# t_deduped <- t %>%
#   distinct(text,account,category,.keep_all = TRUE)
#
# dupe_tweets_summary <-t %>% group_by(text,account,category) %>%
#   summarize(n=n()) %>%
#   arrange(desc(n)) %>%
#   filter(n>1)
# unique(t$category)

# DEI terms ---------------------------------------------------------------
dei_terms <- c("1619 project","anti-racism", "antiracism",
               "bias", "black lives", "black lives matter","blm", "civil right",
               "critical race theory","culturally sensitive","discrimination",
               "equality", "gender", "george floyd",
               "inequality","implicit bias","indigenous", 
               "inclusion", "intersectional", "inclusive", 
               "ibram",
               # "kendi",use ibram instead
               "microaggression","multicultural", "oppression", 
               "racism", "racial", "racist", "reform", "social justice",
               "social change", "systemic racism","transgender", 
               # "trans", # could be transfer student
               "underrepresented","white fragility", "white supremacy")

# Will need to add there to the searches below EXCEPT for the one that is the 
# subject of that specific search:
# dei_added<-c("advocacy","ally","race",
#              "equity","justice","privilege","diversity","diverse")

# "race" errors -----------------------------------------------------------

race_tweets <- t %>% filter(category == "race")

race_terms = c("team","compete","human race","road race","motorcycle",
               "finished","car race","driver","nascar","indy","daytona",
               "mudder","food truck","athletic","bike race","swim","golf",
               "ncaa","sailing","5k","10k","championship","cycling","compete",
               "cross country","rowing","football","triathl","baseball",
               "olympics","basketball","espn","acc ","title","space race",
               "doeracetozero","patsrun","endurance","robot","relay race",
               "amazing race","boston", "stadium","field")

# add the additional search terms EXCEPT race
dei_terms_race<-c(dei_terms,c("advocacy","ally","equity","justice",
                              "privilege","diversity","diverse"))
                    
                  
race_fail <- race_tweets %>%
  filter(str_detect(text, paste(race_terms, collapse = '|'))) %>%
  filter(!str_detect(text, paste(dei_terms_race, collapse = '|')))
# 
# race_fail <- race_tweets %>%
#   filter(
#     (
#       str_detect(text, "team") |
#         str_detect(text, "compete") |
#         str_detect(text, "human race") |
#         str_detect(text, "road race") |
#         str_detect(text, "motorcycle") |
#         str_detect(text, "finished") |
#         str_detect(text, "car race") |
#         str_detect(text, "driver") |
#         str_detect(text, "nascar") |
#         str_detect(text, "indy") |
#         str_detect(text, "daytona") |
#         str_detect(text, "mudder") |
#         str_detect(text, "food truck") |
#         str_detect(text, "athletic") |
#         str_detect(text, "bike race") |
#         str_detect(text, "swim") |
#         str_detect(text, "golf") |
#         str_detect(text, "ncaa") |
#         str_detect(text, "sailing") |
#         str_detect(text, "5k") |
#         str_detect(text, "10k") |
#         str_detect(text, "championship") |
#         str_detect(text, "cycling") |
#         str_detect(text, "compete") |
#         str_detect(text, "cross country") |
#         str_detect(text, "rowing") |
#         str_detect(text, "football") |
#         str_detect(text, "triathl") |
#         str_detect(text, "baseball") |
#         str_detect(text, "olympics") |
#         str_detect(text, "basketball") |
#         str_detect(text, "espn") |
#         str_detect(text, "acc ") |
#         str_detect(text, "title") |
#         str_detect(text, "space race") |
#         str_detect(text, "doeracetozero") |
#         str_detect(text, "patsrun") |
#         str_detect(text, "endurance") |
#         str_detect(text, "robot") |
#         str_detect(text, "relay race") |
#         str_detect(text, "amazing race") |
#         str_detect(text, "boston")
#     ) == TRUE
#   )
# 
# # this next section takes all the tweets above and eliminates any that have
# # DEI words. Other ones also have this, but it is piped for efficiency
# 
# race_fail <- race_fail %>%
#   filter(str_detect(text, "1619 project") == FALSE) %>%
#   filter(str_detect(text, "advocacy") == FALSE) %>%
#   filter(str_detect(text, "ally") == FALSE) %>%
#   filter(str_detect(text, "anti-racism") == FALSE) %>%
#   filter(str_detect(text, "antiracism") == FALSE) %>%
#   # filter(str_detect(text, "bias") == FALSE) %>%
#   filter(str_detect(text, "black lives") == FALSE) %>%
#   filter(str_detect(text, "black lives matter") == FALSE) %>%
#   filter(str_detect(text, "blm") == FALSE) %>%
#   filter(str_detect(text, "civil right") == FALSE) %>%
#   filter(str_detect(text, "critical race theory") == FALSE) %>%
#   filter(str_detect(text, "culturally sensitive") == FALSE) %>%
#   filter(str_detect(text, "discrimination") == FALSE) %>%
#   filter(str_detect(text, "diversity") == FALSE) %>%
#   filter(str_detect(text, "diverse") == FALSE) %>%
#   filter(str_detect(text, "equity") == FALSE) %>%
#   filter(str_detect(text, "equality") == FALSE) %>%
#   # filter(str_detect(text, "gender") == FALSE) %>%
#   filter(str_detect(text, "george floyd") == FALSE) %>%
#   filter(str_detect(text, "inequality") == FALSE) %>%
#   filter(str_detect(text, "implicit bias") == FALSE) %>%
#   filter(str_detect(text, "indigenous") == FALSE) %>%
#   filter(str_detect(text, "inclusion") == FALSE) %>%
#   filter(str_detect(text, "intersectional") == FALSE) %>%
#   filter(str_detect(text, "inclusive") == FALSE) %>%
#   filter(str_detect(text, "justice") == FALSE) %>%
#   filter(str_detect(text, "kendi") == FALSE) %>%
#   filter(str_detect(text, "microaggression") == FALSE) %>%
#   filter(str_detect(text, "multicultural") == FALSE) %>%
#   filter(str_detect(text, "oppression") == FALSE) %>%
#   filter(str_detect(text, "privilege") == FALSE) %>%
#   # filter(str_detect(text, "race") == FALSE) %>%
#   filter(str_detect(text, "racism") == FALSE) %>%
#   # filter(str_detect(text, "racial") == FALSE) %>%
#   filter(str_detect(text, "racist") == FALSE) %>%
#   filter(str_detect(text, "reform") == FALSE) %>%
#   filter(str_detect(text, "social justice") == FALSE) %>%
#   filter(str_detect(text, "social change") == FALSE) %>%
#   filter(str_detect(text, "systemic racism") == FALSE) %>%
#   filter(str_detect(text, "transgender") == FALSE) %>%
#   # filter(str_detect(text, "trans") == FALSE) %>%
#   filter(str_detect(text, "underrepresented") == FALSE) %>%
#   filter(str_detect(text, "white fragility") == FALSE) %>%
#   filter(str_detect(text, "white supremacy") == FALSE)


# "justice" errors --------------------------------------------------------

justice_tweets <- t %>% filter(category == "justice")


dei_terms_justice<-c(dei_terms,c("advocacy","ally","equity","race",
                              "privilege","diversity","diverse"))

justice_terms<-c("supreme court","connor", 
                 "online","commencement","criminal justice")

justice_fail <- justice_tweets %>%
  filter(str_detect(text, paste(justice_terms, collapse = '|'))) %>%
  filter(!str_detect(text, paste(dei_terms_justice, collapse = '|')))
# 
# justice_fail <- justice_tweets %>%
#   filter(
#     (
#       str_detect(text, "supreme court") |
#         str_detect(text, "connor") |
#         str_detect(text, "online") |
#         str_detect(text, "commencement") |
#         str_detect(text, "criminal justice")
#     ) == TRUE
#   ) %>%
#   filter(str_detect(text, "1619 project") == FALSE) %>%
#   filter(str_detect(text, "advocacy") == FALSE) %>%
#   filter(str_detect(text, "ally") == FALSE) %>%
#   filter(str_detect(text, "anti-racism") == FALSE) %>%
#   filter(str_detect(text, "antiracism") == FALSE) %>%
#   # filter(str_detect(text, "bias") == FALSE) %>%
#   filter(str_detect(text, "black lives") == FALSE) %>%
#   filter(str_detect(text, "black lives matter") == FALSE) %>%
#   filter(str_detect(text, "blm") == FALSE) %>%
#   filter(str_detect(text, "civil right") == FALSE) %>%
#   filter(str_detect(text, "critical race theory") == FALSE) %>%
#   filter(str_detect(text, "culturally sensitive") == FALSE) %>%
#   filter(str_detect(text, "discrimination") == FALSE) %>%
#   filter(str_detect(text, "diversity") == FALSE) %>%
#   filter(str_detect(text, "diverse") == FALSE) %>%
#   filter(str_detect(text, "equity") == FALSE) %>%
#   filter(str_detect(text, "equality") == FALSE) %>%
#   # filter(str_detect(text, "gender") == FALSE) %>%
#   filter(str_detect(text, "george floyd") == FALSE) %>%
#   filter(str_detect(text, "inequality") == FALSE) %>%
#   filter(str_detect(text, "implicit bias") == FALSE) %>%
#   filter(str_detect(text, "indigenous") == FALSE) %>%
#   filter(str_detect(text, "inclusion") == FALSE) %>%
#   filter(str_detect(text, "inclusive") == FALSE) %>%
#   filter(str_detect(text, "intersectional") == FALSE) %>%
#   filter(str_detect(text, "kendi") == FALSE) %>%
#   filter(str_detect(text, "microaggression") == FALSE) %>%
#   filter(str_detect(text, "multicultural") == FALSE) %>%
#   filter(str_detect(text, "oppression") == FALSE) %>%
#   filter(str_detect(text, "privilege") == FALSE) %>%
#   # filter(str_detect(text, "race") == FALSE) %>%
#   filter(str_detect(text, "racism") == FALSE) %>%
#   # filter(str_detect(text, "racial") == FALSE) %>%
#   filter(str_detect(text, "racist") == FALSE) %>%
#   filter(str_detect(text, "reform") == FALSE) %>%
#   filter(str_detect(text, "social justice") == FALSE) %>%
#   filter(str_detect(text, "social change") == FALSE) %>%
#   filter(str_detect(text, "systemic racism") == FALSE) %>%
#   filter(str_detect(text, "transgender") == FALSE) %>%
#   # filter(str_detect(text, "trans") == FALSE) %>%
#   filter(str_detect(text, "underrepresented") == FALSE) %>%
#   filter(str_detect(text, "white fragility") == FALSE) %>%
#   filter(str_detect(text, "white supremacy") == FALSE)
# 




# diversity errors ---------------------------------------------------------


diversity_tweets <- t %>% filter(category == "diversity")

diversity_terms<-c("species", "insect", "mammal", "ecology", "evolution", 
                   "evolv", "bird", "snake", "frog", "lizard", "earth",
                   "genetic", "plant","NSF", "nih", "botan", 
                   "biodivers", "plant")


dei_terms_diversity<-c(dei_terms,c("advocacy","ally","equity","race",
                              "justice","privilege"))


diversity_fail <- diversity_tweets %>%
  filter(str_detect(text, paste(diversity_terms, collapse = '|'))) %>%
  filter(!str_detect(text, paste(dei_terms_diversity, collapse = '|')))

# diversity_fail <- diversity_tweets %>% filter(str_detect(text,"species")==TRUE)
# 
# diversity_fail <- diversity_tweets %>%
#   filter(
#     (
#       str_detect(text, "species") |
#         str_detect(text, "insect") |
#         str_detect(text, "mammal") |
#         str_detect(text, "ecology") |
#         str_detect(text, "evolution") |
#         str_detect(text, "evolv") |
#         str_detect(text, "bird") |
#         str_detect(text, "snake") |
#         str_detect(text, "frog") |
#         str_detect(text, "lizard") |
#         str_detect(text, "earth") |
#         str_detect(text, "genetic") |
#         str_detect(text, "plant") |
#         str_detect(text, "NSF") |
#         str_detect(text, "nih") |
#         str_detect(text, "botan") |
#         str_detect(text, "biodiversity") |
#         str_detect(text, "plant")
#     ) == TRUE
#   ) %>%
#   filter(str_detect(text, "1619 project") == FALSE) %>%
#   filter(str_detect(text, "advocacy") == FALSE) %>%
#   filter(str_detect(text, "ally") == FALSE) %>%
#   filter(str_detect(text, "anti-racism") == FALSE) %>%
#   filter(str_detect(text, "antiracism") == FALSE) %>%
#   # filter(str_detect(text, "bias") == FALSE) %>%
#   filter(str_detect(text, "black lives") == FALSE) %>%
#   filter(str_detect(text, "black lives matter") == FALSE) %>%
#   filter(str_detect(text, "blm") == FALSE) %>%
#   filter(str_detect(text, "civil right") == FALSE) %>%
#   filter(str_detect(text, "critical race theory") == FALSE) %>%
#   filter(str_detect(text, "culturally sensitive") == FALSE) %>%
#   filter(str_detect(text, "discrimination") == FALSE) %>%
#   # filter(str_detect(text, "diversity") == FALSE) %>%
#   # filter(str_detect(text, "diverse") == FALSE) %>%
#   filter(str_detect(text, "equity") == FALSE) %>%
#   filter(str_detect(text, "equality") == FALSE) %>%
#   filter(str_detect(text, "gender") == FALSE) %>%
#   filter(str_detect(text, "george floyd") == FALSE) %>%
#   filter(str_detect(text, "inequality") == FALSE) %>%
#   filter(str_detect(text, "implicit bias") == FALSE) %>%
#   filter(str_detect(text, "indigenous") == FALSE) %>%
#   filter(str_detect(text, "inclusion") == FALSE) %>%
#   filter(str_detect(text, "inclusive") == FALSE) %>%
#   filter(str_detect(text, "intersectional") == FALSE) %>%
#   filter(str_detect(text, "justice") == FALSE) %>%
#   filter(str_detect(text, "kendi") == FALSE) %>%
#   filter(str_detect(text, "microaggression") == FALSE) %>%
#   filter(str_detect(text, "multicultural") == FALSE) %>%
#   filter(str_detect(text, "oppression") == FALSE) %>%
#   filter(str_detect(text, "privilege") == FALSE) %>%
#   filter(str_detect(text, "race") == FALSE) %>%
#   filter(str_detect(text, "racism") == FALSE) %>%
#   filter(str_detect(text, "racial") == FALSE) %>%
#   filter(str_detect(text, "racist") == FALSE) %>%
#   filter(str_detect(text, "reform") == FALSE) %>%
#   filter(str_detect(text, "social justice") == FALSE) %>%
#   filter(str_detect(text, "social change") == FALSE) %>%
#   filter(str_detect(text, "systemic racism") == FALSE) %>%
#   filter(str_detect(text, "transgender") == FALSE) %>%
#   # filter(str_detect(text, "trans") == FALSE) %>%
#   filter(str_detect(text, "underrepresented") == FALSE) %>%
#   filter(str_detect(text, "white fragility") == FALSE) %>%
#   filter(str_detect(text, "white supremacy") == FALSE)



# equity errors ------------------------------------------------------------

equity_tweets <- t %>% filter(category == "equity")

equity_terms<-c("private equity","finance", 
                "banking","wall street","investment","vc",
                "bank","capital","firm")

dei_terms_equity<-c(dei_terms,c("advocacy","ally","race","justice",
                                 "privilege","diversity","diverse"))


equity_fail <- equity_tweets %>%
  filter(str_detect(text, paste(equity_terms, collapse = '|'))) %>%
  filter(!str_detect(text, paste(dei_terms_equity, collapse = '|')))


# 
# equity_fail <- equity_tweets %>%
#   filter(
#     (
#       str_detect(text, "private equity") |
#         str_detect(text, "finance") |
#         str_detect(text, "banking") |
#         str_detect(text, "capital") |
#         str_detect(text, "firm")
#     ) == TRUE
#   ) %>%
#   filter(str_detect(text, "1619 project") == FALSE) %>%
#   filter(str_detect(text, "advocacy") == FALSE) %>%
#   filter(str_detect(text, "ally") == FALSE) %>%
#   filter(str_detect(text, "anti-racism") == FALSE) %>%
#   filter(str_detect(text, "antiracism") == FALSE) %>%
#   filter(str_detect(text, "bias") == FALSE) %>%
#   filter(str_detect(text, "black lives") == FALSE) %>%
#   filter(str_detect(text, "black lives matter") == FALSE) %>%
#   filter(str_detect(text, "blm") == FALSE) %>%
#   filter(str_detect(text, "civil right") == FALSE) %>%
#   filter(str_detect(text, "critical race theory") == FALSE) %>%
#   filter(str_detect(text, "culturally sensitive") == FALSE) %>%
#   filter(str_detect(text, "discrimination") == FALSE) %>%
#   filter(str_detect(text, "diversity") == FALSE) %>%
#   filter(str_detect(text, "diverse") == FALSE) %>%
#   # filter(str_detect(text, "equity") == FALSE) %>%
#   filter(str_detect(text, "equality") == FALSE) %>%
#   filter(str_detect(text, "gender") == FALSE) %>%
#   filter(str_detect(text, "george floyd") == FALSE) %>%
#   filter(str_detect(text, "inequality") == FALSE) %>%
#   filter(str_detect(text, "implicit bias") == FALSE) %>%
#   filter(str_detect(text, "indigenous") == FALSE) %>%
#   filter(str_detect(text, "inclusion") == FALSE) %>%
#   filter(str_detect(text, "intersectional") == FALSE) %>%
#   filter(str_detect(text, "inclusive") == FALSE) %>%
#   filter(str_detect(text, "justice") == FALSE) %>%
#   filter(str_detect(text, "kendi") == FALSE) %>%
#   filter(str_detect(text, "microaggression") == FALSE) %>%
#   filter(str_detect(text, "multicultural") == FALSE) %>%
#   filter(str_detect(text, "oppression") == FALSE) %>%
#   filter(str_detect(text, "privilege") == FALSE) %>%
#   filter(str_detect(text, "race") == FALSE) %>%
#   filter(str_detect(text, "racism") == FALSE) %>%
#   filter(str_detect(text, "racial") == FALSE) %>%
#   filter(str_detect(text, "racist") == FALSE) %>%
#   filter(str_detect(text, "reform") == FALSE) %>%
#   filter(str_detect(text, "social justice") == FALSE) %>%
#   filter(str_detect(text, "social change") == FALSE) %>%
#   filter(str_detect(text, "systemic racism") == FALSE) %>%
#   filter(str_detect(text, "transgender") == FALSE) %>%
#   filter(str_detect(text, "trans") == FALSE) %>%
#   filter(str_detect(text, "underrepresented") == FALSE) %>%
#   filter(str_detect(text, "white fragility") == FALSE) %>%
#   filter(str_detect(text, "white supremacy") == FALSE)


# advocacy errors ---------------------------------------------------------



advocacy_tweets <- t %>% filter(category == "advocacy")

advocacy_terms<-c("patient advocacy","patient advocate","legal","arts",
                  "law school", "appellate", "college of law", "truman",
                  "trial", "veterans", "legislature","legislative")

dei_terms_advocacy<-c(dei_terms,c("justice","ally","equity","race",
                                "privilege","diversity","diverse"))


advocacy_fail <- advocacy_tweets %>%
  filter(str_detect(text, paste(advocacy_terms, collapse = '|'))) %>%
  filter(!str_detect(text, paste(dei_terms_advocacy, collapse = '|')))


# 
# 
# advocacy_fail <- advocacy_tweets %>%
#   filter(
#     (
#       str_detect(text, "patient advocacy") |
#         str_detect(text, "arts") |
#         str_detect(text, "law school") |
#         str_detect(text, "appellate") |
#         str_detect(text, "college of law") |
#         str_detect(text, "truman") | # truman scholarship winners
#         str_detect(text, "trial") |
#         str_detect(text, "veterans") |
#         str_detect(text, "legislature")
#     ) == TRUE
#   ) %>%
#   filter(str_detect(text, "1619 project") == FALSE) %>%
#   # filter(str_detect(text, "advocacy") == FALSE) %>%
#   filter(str_detect(text, "ally") == FALSE) %>%
#   filter(str_detect(text, "anti-racism") == FALSE) %>%
#   filter(str_detect(text, "antiracism") == FALSE) %>%
#   filter(str_detect(text, "bias") == FALSE) %>%
#   filter(str_detect(text, "black lives") == FALSE) %>%
#   filter(str_detect(text, "black lives matter") == FALSE) %>%
#   filter(str_detect(text, "blm") == FALSE) %>%
#   filter(str_detect(text, "civil right") == FALSE) %>%
#   filter(str_detect(text, "critical race theory") == FALSE) %>%
#   filter(str_detect(text, "culturally sensitive") == FALSE) %>%
#   filter(str_detect(text, "discrimination") == FALSE) %>%
#   filter(str_detect(text, "diversity") == FALSE) %>%
#   filter(str_detect(text, "diverse") == FALSE) %>%
#   filter(str_detect(text, "equity") == FALSE) %>%
#   filter(str_detect(text, "equality") == FALSE) %>%
#   filter(str_detect(text, "gender") == FALSE) %>%
#   filter(str_detect(text, "george floyd") == FALSE) %>%
#   filter(str_detect(text, "inequality") == FALSE) %>%
#   filter(str_detect(text, "implicit bias") == FALSE) %>%
#   filter(str_detect(text, "indigenous") == FALSE) %>%
#   filter(str_detect(text, "inclusion") == FALSE) %>%
#   filter(str_detect(text, "inclusive") == FALSE) %>%
#   filter(str_detect(text, "intersectional") == FALSE) %>%
#   filter(str_detect(text, "justice") == FALSE) %>%
#   filter(str_detect(text, "kendi") == FALSE) %>%
#   filter(str_detect(text, "microaggression") == FALSE) %>%
#   filter(str_detect(text, "multicultural") == FALSE) %>%
#   filter(str_detect(text, "oppression") == FALSE) %>%
#   filter(str_detect(text, "privilege") == FALSE) %>%
#   filter(str_detect(text, "race") == FALSE) %>%
#   filter(str_detect(text, "racism") == FALSE) %>%
#   filter(str_detect(text, "racial") == FALSE) %>%
#   filter(str_detect(text, "racist") == FALSE) %>%
#   filter(str_detect(text, "reform") == FALSE) %>%
#   filter(str_detect(text, "social justice") == FALSE) %>%
#   filter(str_detect(text, "social change") == FALSE) %>%
#   filter(str_detect(text, "systemic racism") == FALSE) %>%
#   filter(str_detect(text, "transgender") == FALSE) %>%
#   filter(str_detect(text, "trans") == FALSE) %>%
#   filter(str_detect(text, "underrepresented") == FALSE) %>%
#   filter(str_detect(text, "white fragility") == FALSE) %>%
#   filter(str_detect(text, "white supremacy") == FALSE)
# 


# ally errors -------------------------------------------------------------


ally_tweets <- t %>% filter(category == "ally")

ally_terms<-c("@ally_","@ally","softball","tennis","gymnast","lacross",
              "hockey","golf","water polo", "waterpolo","basketball",
              "swim","pool","ncaa","soccer","champion"
              "athlet","single", "sport", "stadium","field")



# DEI terms ---------------------------------------------------------------


dei_terms_ally<-c(dei_terms,c("advocacy","justice","equity",
                                  "privilege","lgbt",
                              "diversity","diverse"))

#remove race AND ally due to racing

ally_fail <- ally_tweets %>%
  filter(str_detect(text, paste(ally_terms, collapse = '|'))) %>%
  filter(!str_detect(text, paste(dei_terms_ally, collapse = '|')))
# 
# 
# 
# ally_fail <- ally_tweets %>%
#   filter(
#     (
#       str_detect(text, "ally_") |
#         str_detect(text, "ally") |
#         str_detect(text, "softball") |
#         str_detect(text, "tennis") |
#         str_detect(text, "gymnast") |
#         str_detect(text, "lacross") |
#         str_detect(text, "water polo") |
#         str_detect(text, "waterpolo") |
#         str_detect(text, "basketball") |
#         str_detect(text, "soccer") |
#         str_detect(text, "single")
#     ) == TRUE
#   ) %>%
#   filter(str_detect(text, "1619 project") == FALSE) %>%
#   filter(str_detect(text, "advocacy") == FALSE) %>%
#   # filter(str_detect(text, "ally") == FALSE) %>%
#   filter(str_detect(text, "anti-racism") == FALSE) %>%
#   filter(str_detect(text, "antiracism") == FALSE) %>%
#   filter(str_detect(text, "bias") == FALSE) %>%
#   filter(str_detect(text, "black lives") == FALSE) %>%
#   filter(str_detect(text, "black lives matter") == FALSE) %>%
#   # filter(str_detect(text, "blm") == FALSE) %>%
#   filter(str_detect(text, "civil right") == FALSE) %>%
#   filter(str_detect(text, "critical race theory") == FALSE) %>%
#   filter(str_detect(text, "culturally sensitive") == FALSE) %>%
#   filter(str_detect(text, "discrimination") == FALSE) %>%
#   filter(str_detect(text, "diversity") == FALSE) %>%
#   filter(str_detect(text, "diverse") == FALSE) %>%
#   filter(str_detect(text, "equity") == FALSE) %>%
#   filter(str_detect(text, "equality") == FALSE) %>%
#   filter(str_detect(text, "gender") == FALSE) %>%
#   filter(str_detect(text, "george floyd") == FALSE) %>%
#   filter(str_detect(text, "inequality") == FALSE) %>%
#   filter(str_detect(text, "implicit bias") == FALSE) %>%
#   filter(str_detect(text, "indigenous") == FALSE) %>%
#   filter(str_detect(text, "inclusion") == FALSE) %>%
#   filter(str_detect(text, "inclusive") == FALSE) %>%
#   filter(str_detect(text, "intersectional") == FALSE) %>%
#   filter(str_detect(text, "justice") == FALSE) %>%
#   filter(str_detect(text, "kendi") == FALSE) %>%
#   filter(str_detect(text, "microaggression") == FALSE) %>%
#   filter(str_detect(text, "multicultural") == FALSE) %>%
#   filter(str_detect(text, "oppression") == FALSE) %>%
#   filter(str_detect(text, "privilege") == FALSE) %>%
#   filter(str_detect(text, "race") == FALSE) %>%
#   filter(str_detect(text, "racism") == FALSE) %>%
#   filter(str_detect(text, "racial") == FALSE) %>%
#   filter(str_detect(text, "racist") == FALSE) %>%
#   filter(str_detect(text, "reform") == FALSE) %>%
#   filter(str_detect(text, "social justice") == FALSE) %>%
#   filter(str_detect(text, "social change") == FALSE) %>%
#   filter(str_detect(text, "systemic racism") == FALSE) %>%
#   filter(str_detect(text, "transgender") == FALSE) %>%
#   # filter(str_detect(text, "trans") == FALSE) %>%
#   filter(str_detect(text, "underrepresented") == FALSE) %>%
#   filter(str_detect(text, "white fragility") == FALSE) %>%
#   filter(str_detect(text, "white supremacy") == FALSE)
# privilege errors -------------------------------------------------------------


privilege_tweets <- t %>% filter(category == "privilege")

privilege_terms<- c("a privilege to", "my privilege to","the privilege to",
                    "privilege to", "reporter", "privilege and honor",
                    "privilege and an", "privilege of", "the privilege of")

dei_terms_privilege<-c(dei_terms,c("advocacy","ally","equity","race",
                              "justice","diversity","diverse"))


privilege_fail <-privilege_tweets %>%
  filter(str_detect(text, paste(privilege_terms, collapse = '|'))) %>%
  filter(!str_detect(text, paste(dei_terms_privilege, collapse = '|')))

# 
# privilege_fail <- privilege_tweets %>%
#   filter(
#     (
#       str_detect(text, "a privilege to") |
#         str_detect(text, "my privilege to") |
#         str_detect(text, "the privilege to") |
#         str_detect(text, "privilege to") |
#         str_detect(text, "reporter") |
#         str_detect(text, "privilege and honor") |
#         str_detect(text, "privilege and an") |
#         str_detect(text, "privilege of") |
#         str_detect(text, "the privilege of")
#     ) == TRUE
#   ) %>%
#   filter(str_detect(text, "1619 project") == FALSE) %>%
#   filter(str_detect(text, "advocacy") == FALSE) %>%
#   filter(str_detect(text, "ally") == FALSE) %>%
#   filter(str_detect(text, "anti-racism") == FALSE) %>%
#   filter(str_detect(text, "antiracism") == FALSE) %>%
#   filter(str_detect(text, "bias") == FALSE) %>%
#   filter(str_detect(text, "black lives") == FALSE) %>%
#   filter(str_detect(text, "black lives matter") == FALSE) %>%
#   filter(str_detect(text, "blm") == FALSE) %>%
#   filter(str_detect(text, "civil right") == FALSE) %>%
#   filter(str_detect(text, "critical race theory") == FALSE) %>%
#   filter(str_detect(text, "culturally sensitive") == FALSE) %>%
#   filter(str_detect(text, "discrimination") == FALSE) %>%
#   filter(str_detect(text, "diversity") == FALSE) %>%
#   filter(str_detect(text, "diverse") == FALSE) %>%
#   filter(str_detect(text, "equity") == FALSE) %>%
#   filter(str_detect(text, "equality") == FALSE) %>%
#   filter(str_detect(text, "gender") == FALSE) %>%
#   filter(str_detect(text, "george floyd") == FALSE) %>%
#   filter(str_detect(text, "inequality") == FALSE) %>%
#   filter(str_detect(text, "implicit bias") == FALSE) %>%
#   filter(str_detect(text, "indigenous") == FALSE) %>%
#   filter(str_detect(text, "inclusion") == FALSE) %>%
#   filter(str_detect(text, "inclusive") == FALSE) %>%
#   filter(str_detect(text, "intersectional") == FALSE) %>%
#   filter(str_detect(text, "justice") == FALSE) %>%
#   filter(str_detect(text, "kendi") == FALSE) %>%
#   filter(str_detect(text, "microaggression") == FALSE) %>%
#   filter(str_detect(text, "multicultural") == FALSE) %>%
#   filter(str_detect(text, "oppression") == FALSE) %>%
#   # filter(str_detect(text, "privilege") == FALSE) %>%
#   filter(str_detect(text, "race") == FALSE) %>%
#   filter(str_detect(text, "racism") == FALSE) %>%
#   filter(str_detect(text, "racial") == FALSE) %>%
#   filter(str_detect(text, "racist") == FALSE) %>%
#   filter(str_detect(text, "reform") == FALSE) %>%
#   filter(str_detect(text, "social justice") == FALSE) %>%
#   filter(str_detect(text, "social change") == FALSE) %>%
#   filter(str_detect(text, "systemic racism") == FALSE) %>%
#   filter(str_detect(text, "transgender") == FALSE) %>%
#   filter(str_detect(text, "trans") == FALSE) %>%
#   filter(str_detect(text, "underrepresented") == FALSE) %>%
#   filter(str_detect(text, "white fragility") == FALSE) %>%
#   filter(str_detect(text, "white supremacy") == FALSE)
# 



# bind it up --------------------------------------------------------------

tweet_fail <- bind_rows(
  race_fail,
  equity_fail,
  diversity_fail,
  justice_fail,
  advocacy_fail,
  ally_fail,
  privilege_fail
)

# clean text to make it easier to present in table_-------------------------

# the following section will remove the twitter handles, urls, email addresses, 
# emoticons, etc. to streamline the presentation. The originals are still there.

# To get an idea of what issues there are run this
# check_text(t$text)

tweet_fail<-tweet_fail %>% 
  mutate(text_original=text) %>%
  mutate(text=replace_tag(text,replacement="@--")) %>%
  mutate(text=replace_url(text,replacement="[url]")) %>%
  mutate(text=replace_email(text,replacement="[email]")) %>%
  mutate(text=replace_emoticon(text)) %>%
  mutate(text=replace_emoji(text)) %>%
  mutate(text=replace_white(text)) %>%
  mutate(text=replace_html(text, symbol=TRUE))



write_csv(tweet_fail, "./error_analysis_bruna/validation_output/twitter_notdei.csv")

# oriinal tweets by kw analysed here
original_tweets <- bind_rows(
  race_tweets,
  equity_tweets,
  diversity_tweets,
  justice_tweets,
  advocacy_tweets,
  ally_tweets,
  privilege_tweets
)


# make a summary table
original_tweets_by_cat <- original_tweets %>%
  group_by(category) %>%
  summarise(tweets_in_cat = n())

tweet_fails_by_cat <- tweet_fail %>%
  group_by(category) %>%
  summarise(fail_tweets = n())



twitter_fail_summary <- tweet_fails_by_cat %>%
  left_join(original_tweets_by_cat) %>%
  mutate(perc_fail = (fail_tweets / tweets_in_cat) * 100) %>%
  mutate(total_tweets_reviewed = sum(tweets_in_cat)) %>%
  mutate(total_tweets_clean = nrow(t)) %>% 
  arrange(perc_fail)

write_csv(twitter_fail_summary, "./error_analysis_bruna/validation_output/twitter_notdei_summary.csv")
