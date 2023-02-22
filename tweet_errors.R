require(tidyverse)
require(glue)
require(here)

here <- rprojroot::find_rstudio_root_file()
source(glue("{here}/code/setup.R"))
# load "clean tweets" -----------------------------------------------------

t <- fread(glue("{here}/out/twitter/tweets_clean.csv"))
names(t)


# "race" errors -----------------------------------------------------------

race_tweets <- t %>% 
  mutate(text=tolower(text)) %>% 
  filter(str_detect(text,"race")==TRUE) 

race_fail <- race_tweets %>% 
  filter(
    (
      str_detect(text,"team")|
        str_detect(text,"compete")|
        str_detect(text,"human race")|
        str_detect(text,"road race")|
        str_detect(text,"motorcycle")|
        str_detect(text,"finished")|
        str_detect(text,"car race")|
        str_detect(text,"driver")|
        str_detect(text,"nascar")|
        str_detect(text,"indy")|
        str_detect(text,"daytona")|
        str_detect(text,"mudder")|
        str_detect(text,"food truck")|
        str_detect(text,"athletic")|
        str_detect(text,"bike race")|
        str_detect(text,"swim")|
        str_detect(text,"golf")|
        str_detect(text,"ncaa")|
        str_detect(text,"sailing")|
        str_detect(text,"5k")|
        str_detect(text,"10k")|
        str_detect(text,"championship")|
        str_detect(text,"cycling")|
        str_detect(text,"compete")|
        str_detect(text,"cross country")|
        str_detect(text,"rowing")|
        str_detect(text,"football")|
        str_detect(text,"triathl")|
        str_detect(text,"baseball")|
        str_detect(text,"olympics")|
        str_detect(text,"basketball")|
        str_detect(text,"espn")|
        str_detect(text,"acc ")|
        str_detect(text,"title")|
        str_detect(text,"space race")|
        str_detect(text,"doeracetozero")|
        str_detect(text,"patsrun")|
        str_detect(text,"endurance")|
        str_detect(text,"robot")|
        str_detect(text,"relay race")|
        str_detect(text,"amazing race")|
        str_detect(text,"boston")
      )==TRUE) %>% 
  filter(str_detect(text,"equity")==FALSE) %>% 
  filter(str_detect(text,"racism")==FALSE) %>% 
  filter(str_detect(text,"diversity")==FALSE) %>% 
  mutate(fail_category="race")

(nrow(race_fail)/nrow(race_tweets))*100


# "justice" errors --------------------------------------------------------


justice_tweets <- t %>% 
  mutate(text=tolower(text)) %>% 
  filter(str_detect(text,"justice")==TRUE) 


justice_fail <- justice_tweets %>% 
  filter(
    (
      str_detect(text,"supreme court")|
        str_detect(text,"sandra day o¡¯connor")|
        str_detect(text,"online")|
        str_detect(text,"commencement")|
        str_detect(text,"criminal justice")
    )==TRUE) %>% 
  filter(str_detect(text,"equity")==FALSE) %>% 
  filter(str_detect(text,"race")==FALSE) %>% 
  filter(str_detect(text,"racism")==FALSE) %>% 
  filter(str_detect(text,"racial")==FALSE) %>% 
  filter(str_detect(text,"diversity")==FALSE) %>%
  filter(str_detect(text,"civil right")==FALSE) %>%
  filter(str_detect(text,"inclusion")==FALSE) %>%
  filter(str_detect(text,"reform")==FALSE) %>%
  filter(str_detect(text,"social")==FALSE) %>% 
  mutate(fail_category="justice")




# diversity errors ---------------------------------------------------------


diversity_tweets <- t %>% 
  mutate(text=tolower(text)) %>% 
  filter(str_detect(text,"diversity")==TRUE) 



# diversity_fail <- diversity_tweets %>% filter(str_detect(text,"species")==TRUE)

diversity_fail <- diversity_tweets %>% 
  filter(
    (
      str_detect(text,"species")|
        str_detect(text,"insect")|
        str_detect(text,"mammal")|
        str_detect(text,"bird")|
        str_detect(text,"botan")|
        str_detect(text,"biodiversity")|
        str_detect(text,"plant") 
    )==TRUE) %>% 
  filter(str_detect(text,"equity")==FALSE) %>% 
  filter(str_detect(text,"race")==FALSE) %>% 
  filter(str_detect(text,"inclusion")==FALSE) %>% 
  filter(str_detect(text,"racism")==FALSE) %>% 
  filter(str_detect(text,"racial")==FALSE) %>%
  filter(str_detect(text,"civil right")==FALSE) %>%
  filter(str_detect(text,"reform")==FALSE) %>%
  filter(str_detect(text,"social")==FALSE) %>% 
  mutate(fail_category="diversity")
  




# equity errors ------------------------------------------------------------



equity_tweets <- t %>% 
  mutate(text=tolower(text)) %>% 
  filter(str_detect(text,"equity")==TRUE) 



# equity_fail <- equity_tweets %>% filter(str_detect(text,"equity")==TRUE)

equity_fail <- equity_tweets %>% 
  filter(
    (
      str_detect(text,"private equity")
    )==TRUE) %>% 
  filter(str_detect(text,"race")==FALSE) %>% 
  filter(str_detect(text,"inclusion")==FALSE) %>% 
  filter(str_detect(text,"racism")==FALSE) %>% 
  filter(str_detect(text,"racial")==FALSE) %>%
  filter(str_detect(text,"civil right")==FALSE) %>%
  filter(str_detect(text,"reform")==FALSE) %>%
  filter(str_detect(text,"social")==FALSE) %>% 
  mutate(fail_category="equity")






# bind it up --------------------------------------------------------------

tweet_fail<-bind_rows(race_fail,
                       equity_fail,
                       diversity_fail,
                       justice_fail) %>% 
  relocate(fail_category,.before=1)

overall_fail_perc<-(nrow(tweet_fail)/nrow(t))*100
overall_fail_perc
race_fail_perc<-(nrow(race_fail)/nrow(race_tweets))*100
race_fail_perc
diversity_fail_perc<-(nrow(diversity_fail)/nrow(diversity_tweets))*100
diversity_fail_perc
justice_fail_perc<-(nrow(justice_fail)/nrow(justice_tweets))*100
justice_fail_perc
equity_fail_perc<-(nrow(equity_fail)/nrow(equity_tweets))*100
equity_fail_perc
