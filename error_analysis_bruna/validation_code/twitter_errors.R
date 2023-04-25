
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

library(lubridate)
t <- t %>%
  mutate(
    created_at = ymd_hms(created_at),
    year = format_ISO8601(created_at, precision = "y") 
  )
names(t)

t<-t %>% relocate("year",.before=1)
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
               "equality", "gender", "george floyd", "minority",
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
               "amazing race","boston", "stadium","field", "training",
               "sweat")

# add the additional search terms EXCEPT race
dei_terms_race<-c(dei_terms,c("advocacy","ally","equity","justice",
                              "privilege","diversity","diverse"))
                    
                  
race_fail <- race_tweets %>%
  filter(str_detect(text, paste(race_terms, collapse = '|'))) %>%
  filter(!str_detect(text, paste(dei_terms_race, collapse = '|')))



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

# ally errors -------------------------------------------------------------


ally_tweets <- t %>% filter(category == "ally")

ally_terms<-c("@ally_","@ally","softball","tennis","gymnast","lacross",
              "hockey","golf","water polo", "waterpolo","basketball",
              "swim","pool","ncaa","soccer","champion",
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
  mutate(text=replace_url(text,replacement="")) %>%
  # mutate(text=replace_url(text,replacement="[url]")) %>%
  mutate(text=replace_email(text,replacement="[email]")) %>%
  mutate(text=replace_emoticon(text)) %>%
  mutate(text=str_remove_all(text,pattern = '[:emoji:]')) %>% 
  # mutate(text=replace_emoji(text)) %>%
  mutate(text=replace_white(text)) %>%
  mutate(text=replace_html(text, symbol=FALSE))



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



names(tweet_fail)


fail_over_time<-tweet_fail %>% 
  select(year,category,text) %>% 
  group_by(year, category) %>% 
  tally() %>% 
  arrange(category, year)


errors_over_time_plot<-ggplot(data=fail_over_time, aes(x=year, y=n, group=category)) +
  geom_line()+
  geom_point()+
  ggtitle("false positives over time (minimum)") +
  theme_classic()+
  geom_line(aes(color=category))+geom_point(aes(color=category))
errors_over_time_plot


