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
  filter(str_detect(text,"gender")==FALSE) %>% 
  filter(str_detect(text,"inclusion")==FALSE) %>%
  filter(str_detect(text,"racism")==FALSE) %>%
  filter(str_detect(text,"racial")==FALSE) %>%
  filter(str_detect(text,"civil right")==FALSE) %>%
  filter(str_detect(text,"gender")==FALSE) %>%
  filter(str_detect(text,"reform")==FALSE) %>%
  filter(str_detect(text,"equality")==FALSE) %>%
  filter(str_detect(text,"oppression")==FALSE) %>%
  filter(str_detect(text,"social")==FALSE) %>% 
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
        str_detect(text,"connor")|
        str_detect(text,"online")|
        str_detect(text,"commencement")|
        str_detect(text,"criminal justice")
    )==TRUE) %>% 
  filter(str_detect(text,"gender")==FALSE) %>% 
  filter(str_detect(text,"inclusion")==FALSE) %>%
  filter(str_detect(text,"racism")==FALSE) %>%
  filter(str_detect(text,"racial")==FALSE) %>%
  filter(str_detect(text,"civil right")==FALSE) %>%
  filter(str_detect(text,"gender")==FALSE) %>%
  filter(str_detect(text,"reform")==FALSE) %>%
  filter(str_detect(text,"equality")==FALSE) %>%
  filter(str_detect(text,"oppression")==FALSE) %>%
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
  filter(str_detect(text,"gender")==FALSE) %>% 
  filter(str_detect(text,"inclusion")==FALSE) %>%
  filter(str_detect(text,"racism")==FALSE) %>%
  filter(str_detect(text,"racial")==FALSE) %>%
  filter(str_detect(text,"civil right")==FALSE) %>%
  filter(str_detect(text,"gender")==FALSE) %>%
  filter(str_detect(text,"reform")==FALSE) %>%
  filter(str_detect(text,"equality")==FALSE) %>%
  filter(str_detect(text,"oppression")==FALSE) %>%
  filter(str_detect(text,"social")==FALSE) %>% 
  mutate(fail_category="diversity")
  




# equity errors ------------------------------------------------------------



equity_tweets <- t %>% 
  mutate(text=tolower(text)) %>% 
  filter(str_detect(text,"equity")==TRUE) 

equity_fail <- equity_tweets %>% 
  filter(
    (
      str_detect(text,"private equity")
    )==TRUE) %>% 
  filter(str_detect(text,"gender")==FALSE) %>% 
  filter(str_detect(text,"inclusion")==FALSE) %>%
  filter(str_detect(text,"racism")==FALSE) %>%
  filter(str_detect(text,"racial")==FALSE) %>%
  filter(str_detect(text,"civil right")==FALSE) %>%
  filter(str_detect(text,"gender")==FALSE) %>%
  filter(str_detect(text,"reform")==FALSE) %>%
  filter(str_detect(text,"equality")==FALSE) %>%
  filter(str_detect(text,"oppression")==FALSE) %>%
  filter(str_detect(text,"social")==FALSE) %>% 
  mutate(fail_category="equity")




# advocacy errors ---------------------------------------------------------



advocacy_tweets <- t %>% 
  mutate(text=tolower(text)) %>% 
  filter(str_detect(text,"advocacy")==TRUE) 

advocacy_fail <- advocacy_tweets %>% 
  filter(
    (
      str_detect(text,"patient advocacy")|
        str_detect(text,"law school")|
        str_detect(text,"appellate")|
        str_detect(text,"truman")| #truman scholarship winners
        str_detect(text,"trial")|
        str_detect(text,"veterans")|
        str_detect(text,"legislature")
    )==TRUE) %>% 
  filter(str_detect(text,"gender")==FALSE) %>% 
  filter(str_detect(text,"inclusion")==FALSE) %>%
  filter(str_detect(text,"racism")==FALSE) %>%
  filter(str_detect(text,"racial")==FALSE) %>%
  filter(str_detect(text,"civil right")==FALSE) %>%
  filter(str_detect(text,"gender")==FALSE) %>%
  filter(str_detect(text,"reform")==FALSE) %>%
  filter(str_detect(text,"equality")==FALSE) %>%
  filter(str_detect(text,"oppression")==FALSE) %>%
  filter(str_detect(text,"social")==FALSE) %>% 
  mutate(fail_category="advocacy")





# ally errors -------------------------------------------------------------


ally_tweets <- t %>% 
  mutate(text=tolower(text)) %>% 
  filter(str_detect(text,"ally")==TRUE) 

ally_fail <- ally_tweets %>% 
  filter(
    (
      str_detect(text,"@ally_")|
        str_detect(text,"@ally")|
        str_detect(text,"softball")|
        str_detect(text,"tennis")|
        str_detect(text,"gymnast")|
        str_detect(text,"lacross")|
        str_detect(text,"water polo")|
        str_detect(text,"waterpolo")|
        str_detect(text,"basketball")|
        str_detect(text,"soccer")|
        str_detect(text,"single"))==TRUE) %>% 
  filter(str_detect(text,"gender")==FALSE) %>% 
  filter(str_detect(text,"inclusion")==FALSE) %>%
  filter(str_detect(text,"racism")==FALSE) %>%
  filter(str_detect(text,"racial")==FALSE) %>%
  filter(str_detect(text,"civil right")==FALSE) %>%
  filter(str_detect(text,"gender")==FALSE) %>%
  filter(str_detect(text,"reform")==FALSE) %>%
  filter(str_detect(text,"equality")==FALSE) %>%
  filter(str_detect(text,"oppression")==FALSE) %>%
  filter(str_detect(text,"social")==FALSE) %>% 
  mutate(fail_category="ally")







# privilege errors -------------------------------------------------------------


privilege_tweets <- t %>% 
  mutate(text=tolower(text)) %>% 
  filter(str_detect(text,"privilege")==TRUE) 

privilege_fail <- privilege_tweets %>% 
  filter(
    (
      str_detect(text,"a privilege to") |
        str_detect(text,"my privilege to") |
  str_detect(text,"the privilege to ") |
    str_detect(text,"privilege to ") |
    str_detect(text,"privilege and honor ") |
    str_detect(text,"privilege and an ") |
    str_detect(text,"privilege of ") |
  str_detect(text,"the privilege of "))==TRUE) %>% 
  filter(str_detect(text,"gender")==FALSE) %>% 
  filter(str_detect(text,"inclusion")==FALSE) %>%
  filter(str_detect(text,"racism")==FALSE) %>%
  filter(str_detect(text,"racial")==FALSE) %>%
  filter(str_detect(text,"ally")==FALSE) %>%
  filter(str_detect(text,"your privilege")==FALSE) %>%
  filter(str_detect(text,"civil right")==FALSE) %>%
  filter(str_detect(text,"gender")==FALSE) %>%
  filter(str_detect(text,"reform")==FALSE) %>%
  filter(str_detect(text,"equality")==FALSE) %>%
  filter(str_detect(text,"oppression")==FALSE) %>%
  filter(str_detect(text,"social")==FALSE) %>% 
  mutate(fail_category="privilege")





# bind it up --------------------------------------------------------------

tweet_fail<-bind_rows(race_fail,
                       equity_fail,
                       diversity_fail,
                       justice_fail,
                      advocacy_fail,
                      ally_fail) %>% 
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
advocacy_fail_perc<-(nrow(advocacy_fail)/nrow(advocacy_tweets))*100
advocacy_fail_perc
ally_fail_perc<-(nrow(ally_fail)/nrow(ally_tweets))*100
ally_fail_perc
privilege_fail_perc<-(nrow(privilege_fail)/nrow(privilege_tweets))*100
privilege_fail_perc




# plots -------------------------------------------------------------------
priv_plot<-privilege_tweets %>% 
  separate(created_at, c("year","month","remainder")) %>% 
  select(-remainder) %>% 
  group_by(month,year) %>% 
  summarize(n=n()) %>% 
  mutate(year=as.integer(year)) %>% 
  mutate(date=paste(year,month,sep="-")) %>% 
  filter(year>2015)


ggplot(data=priv_plot, aes(x=date, y=n, group=1)) +
  geom_line()+
  theme_classic()
