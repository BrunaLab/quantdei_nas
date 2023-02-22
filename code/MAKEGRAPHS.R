

here <- rprojroot::find_rstudio_root_file()
library(glue)
source(glue("{here}/code/setup.R"))

files_dates <- fread(glue("{here}/out/school_websites/files_dates.csv"))
files_dates[,dates := str_remove_all(dates, "\\n")]
files_dates[,school := str_extract(short_name, "^.*?(?=_)")]
files_dates[,dei_term := map_chr(files_dates$short_name, ~str_split(.x, "_")[[1]][2])]
files_dates[,stem_term := map_chr(files_dates$short_name, ~str_split(.x, "_")[[1]][3])]
files_dates[,good_date := parse_date(dates)]
files_dates1 <- files_dates[is.na(good_date), .(dates, short_name, school, dei_term, stem_term, good_date = mdy(dates))]
files_dates2 <- files_dates[!is.na(good_date), .(dates, short_name, school, dei_term, stem_term, good_date = as_date(good_date, "ymd"))]
files_dates3 <- rbind(files_dates1, files_dates2)

# Just make sure these are good. Run this a bunch of times and eyeball it. 
# sample_n(files_dates3[,.(dates, good_date)], 100)

# files_dates4 <- as_tibble(files_dates3)

files_dates3 <- files_dates3[good_date > "1990-01-01" & good_date < "2021-10-01"]
files_dates3[, month := lubridate::floor_date(good_date, "month")]

# setorder(files_dates3[, .(count = .N), by = "short_name"], -count)[]

one_date <- files_dates3[, .(count = .N), by = "short_name"][count == 1]
files_dates4 <- files_dates3[short_name %in% one_date$short_name]

ivy_c <- c("harvard", "yale", "princeton", "columbia", "upenn", "brown", "dartmouth", "cornell")
files_dates4[,ivy :=  ifelse(school %in% ivy_c, "ivy", "non-ivy")]
files_dates4[dei_term != "anti-racism",dei_term := str_replace_all(dei_term, "-", " ")]

files_dates4[good_date > "2010-01-01" & !is.na(dei_term)] %>% 
  arrange(desc(good_date)) %>% 
  group_by(month) %>% 
  summarize(count = n()) %>% 
  # arrange(desc(count))
  ggplot(aes(x = month, y = count)) +
  geom_line() + 
  theme_classic() +
  labs(title = "Fig 1. Mentions of both STEM and DEI terms \non all university websites over time",
       x = "time", y = "count")

ggsave(glue("{here}/graphs/Fig 1.svg"))
ggsave(glue("{here}/graphs/Fig 1.png"))

files_dates4[good_date > "2010-01-01" & !is.na(dei_term)] %>% 
  arrange(desc(good_date)) %>% 
  group_by(month, dei_term) %>% 
  summarize(count = n(), .groups = "drop") %>% 
  ggplot(aes(x = month, y = count)) +
  facet_wrap(~dei_term, scales = "free", nrow = 6, ncol = 2) +
  geom_line() + 
  theme_classic() +
  labs(title = "Fig 2. Mentions of specific DEI terms alongside all STEM terms \non university websites over time",
       x = "time", y = "count")

ggsave(glue("{here}/graphs/Fig 2.svg"), height = 8,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 2.png"), height = 8,  width = 6, dpi = 300)

files_dates4[good_date > "2010-01-01" & !is.na(dei_term)] %>% 
  arrange(desc(good_date)) %>% 
  group_by(month, stem_term) %>% 
  summarize(count = n(), .groups = "drop") %>% 
  ggplot(aes(x = month, y = count)) +
  facet_wrap(~stem_term, ncol = 2, scales = "free") +
  geom_line() + 
  theme_classic() +
  labs(title = "Fig 3. Mentions of all DEI terms alongside specific STEM terms \non university websites over time",
       x = "time", y = "count")

ggsave(glue("{here}/graphs/Fig 3.svg"), height = 8,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 3.png"), height = 8,  width = 6, dpi = 300)

files_dates4[good_date > "2010-01-01" & !is.na(dei_term)] %>% 
  arrange(desc(good_date)) %>% 
  # mutate(year = floor_date(good_date, "year")) %>%
  group_by(month, ivy) %>%
  summarize(count = n(), .groups = "drop") %>% 
  ggplot(aes(x = month, y = count)) +
  facet_wrap(~ivy, ncol = 2, scales = "free") +
  geom_line() + 
  theme_classic() +
  labs(title = "Fig 4. Mentions of all DEI terms alongside all STEM terms on \nIvy League vs non-Ivy League universities over time",
       x = "time", y = "count")

ggsave(glue("{here}/graphs/Fig 4.svg"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 4.png"), height = 4,  width = 6, dpi = 300)

# IVY vs NON-IVY CALCULATIONS
# files_dates4 %>% distinct(school, .keep_all = T) %>% count(ivy)
# ratio:
# 8:92
# 92/8
# 
# in 2010 the ratio was 11:99; that is 11 hits on ivy and 99 hits on ivy
# So that is like: 99/92 (1.07 hits per non ivy) vs 11/8 (1.375 hits per ivy)
# 
# In 2021 the ratio was 409:2490, that is ivy to non-ivy. 
# So that is 2490/92 (27.065) hits per non-ivy and 409/8  (51.125) hits per ivy 
#
# files_dates4[good_date > "2010-01-01" & !is.na(dei_term)] %>% 
#   mutate(year = floor_date(good_date, "year")) %>% 
#   group_by(year, ivy) %>% 
#   summarize(count = n(), .groups = "drop") %>% 
#   arrange(year)



files_dates4[good_date > "2016-01-01" & !is.na(dei_term)] %>% 
  arrange(desc(good_date)) %>% 
  mutate(year = floor_date(month, unit = "year"), 
         dei_term = str_replace_all(dei_term, " ", "\n")) %>% 
  group_by(year, stem_term, dei_term) %>% 
  summarize(count = n(), .groups = "drop") %>% 
  ggplot(aes(x = year, y = count)) +
  geom_line()  +
  facet_grid(vars(stem_term), vars(dei_term), "free_y") +
  theme_bw(base_size = 8) +
  labs(title = "Fig 5. Matrix of DEI vs STEM terms at all universities since 2016",
       x = "year", y = "count") +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2016-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "2 years"), date_labels = "%y", 
               expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 5.svg"), height = 8,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 5.png"), height = 8,  width = 6, dpi = 300)




geolocation_data <- readRDS(glue("{here}/out/school_websites/geolocation_data.Rds"))
geolocation_data <- as.data.table(geolocation_data[,-c("geocoded")])

files_dates4 %>%
  inner_join(geolocation_data, by = "school") %>% 
  count(month, school, lon, lat) %>% 
  ggplot(aes(lon, lat)) +
  borders("state") +
  geom_point(size = 1, alpha = .15, color = "red") +
  theme_map() +
  coord_map() + 
  labs(title = "Fig 6. Geographic spread of co-occurrence \nof DEI and STEM terms at universities")

ggsave(glue("{here}/graphs/Fig 6.jpg"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 6.svg"), height = 4,  width = 6, dpi = 300)

anim <- files_dates4 %>%
  inner_join(geolocation_data, by = "school") %>% 
  count(month, school, lon, lat) %>% 
  ggplot(aes(lon, lat)) +
  borders("state", colour = "lightgray") +
  geom_point(size = 3, alpha = .15, color = "red") +
  transition_manual(month, cumulative = T) +
  labs(title = "Growth of DEI and STEM terms over time: { current_frame }") +
  theme_map() +
  coord_map()

# animate(
#   anim,
#   nframes = 100,
#   fps = 10, 
#   height = 4, 
#   width = 6, 
#   units = "in",
#   res = 150
# )

# anim_save(glue("{here}/graphs/dei_stem_growth_map.gif"))
# files_dates4 %>% sample_n(10)

t <- fread(glue("{here}/out/twitter/tweets_clean.csv"))
t[,good_date := as_date(created_at)]
t[,good_date := ymd(good_date)]
t[,short_name := str_remove(file, glue("{here}/data/_twitter/twitter_account_dei_term/"))]
t[,school := str_extract(short_name, "^.*?(?=--)")]
t[,account := map_chr(short_name, ~str_split(.x, "--")[[1]][2])]
t[,dei_term := str_remove(map_chr(short_name, ~str_split(.x, "--")[[1]][3]), "\\.Rds")]
t[,month := lubridate::floor_date(good_date, "month")]
# unique(t, by = "account")
# t %>%  distinct(dei_term)

t %>%
  group_by(school, good_date) %>%
  summarise(total = n(), .groups = 'keep') %>% 
  ggplot(., aes(x=good_date, y=total, group = 1)) + geom_line() +
  labs(title = "Fig 7. All DEI-related Tweets from all school-related accounts over time",
       x = "year", y = "count") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(glue("{here}/graphs/Fig 7.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 7.svg"), height = 4,  width = 6, dpi = 300)


t %>%
  group_by(month, dei_term) %>%
  summarise(total = n(), .groups = 'keep') %>% 
  filter(month > "2016-01-01") %>% 
  ggplot(., aes(x=month, y=total, group = 1)) + geom_line() +
  labs(title = "Fig 8. DEI-related Tweets from all school-related accounts \nby DEI term",
       x = "year", y = "count") +
  theme_classic() +
  # theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  # theme(plot.title = element_text(hjust = 0.5)) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free_y") +
  facet_wrap(~dei_term, ncol = 3) +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2015-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "1 year"), date_labels = "%y", 
               expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 8.svg"), height = 8,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 8.png"), height = 8,  width = 6, dpi = 300)


twitter_handles <-  fread(glue("{here}/out/twitter/schools_twitter_handles.csv"))
# unique_handles <- map_chr(unique(twitter_handles, by = "twitter_handle")$twitter_handle, ~str_remove(.x, "twitter\\.com/"))
school_main_handles <- unique(twitter_handles,by="file_short_name",fromLast=FALSE)
school_main_handles[,twitter_handle := map_chr(twitter_handle, ~str_remove(.x, "twitter\\.com/"))]
school_main_tweets <- t[account %in% school_main_handles$twitter_handle]
school_main_tweets[, month := lubridate::floor_date(good_date, "month")]
school_main_tweets[, year := lubridate::floor_date(good_date, "year")]

school_main_tweets[month > "2015-01-01"] %>%
  count(account, month) %>% 
  # group_by(account, month) 
  # summarise(total = n(), .groups = 'drop')
  ggplot(., aes(x=month, y=n)) + geom_col() +
  labs(title = "Fig 9. All DEI-related Tweets from university primary accounts",
       x = "year", y = "count") +
  theme_classic() +
  # theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  # theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2015-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "1 year"), date_labels = "%y", 
               expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 9.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 9.svg"), height = 4,  width = 6, dpi = 300)


dei_counts1  <-  fread(glue("{here}/out/learned_societies/dei_term_counts.csv"))

dei_counts1 %>%
  group_by(society_full_name, year) %>%
  summarise(total = sum(term_sum), .groups = 'keep') %>% 
  rename(year1 = year) %>% 
  mutate(year1 = paste0(year1,"-01-01")) %>% 
  mutate(year1 = ymd(year1)) %>% 
  # mutate(year1 = floor_date(year1, "year")) %>% 
  ggplot(., aes(x=year1, y=total, group = 1)) + geom_col() +
  labs(title = "Fig 10. DEI-related terms in academic association \npublications (absolute counts)",
       x = "year", y = "count") +
  theme_classic() +
  facet_wrap(~society_full_name, ncol = 2, scales = "free") +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2000-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "1 year"), date_labels = "%y", 
               expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 10.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 10.svg"), height = 4,  width = 6, dpi = 300)


dei_counts1 %>%
  mutate(prop = term_sum/total_chr) %>% 
  group_by(society_full_name, year) %>% 
  # summarise(total = sum(term_sum), .groups = 'keep') %>% 
  rename(year1 = year) %>% 
  mutate(year1 = paste0(year1,"-01-01")) %>% 
  mutate(year1 = ymd(year1)) %>% 
  ggplot(., aes(x=year1, y=prop, group = 1)) + 
  geom_col() +
  labs(title = "Fig 11. DEI-related terms in academic association \npublications (proportional)",
       x = "year", y = "proportion") +
  theme_classic() +
  facet_wrap(~society_full_name, ncol = 2, scales = "free") +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2000-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "1 year"), date_labels = "%y", 
               expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 11.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 11.svg"), height = 4,  width = 6, dpi = 300)


# # Diversity, equity, inclusion only ---------------------------------------
dei_counts1 %>%
  rename(year1 = year) %>%
  mutate(year1 = paste0(year1,"-01-01")) %>% 
  mutate(year1 = ymd(year1)) %>% 
  group_by(society_full_name, year1) %>%
  summarise(total = sum(dei_sum), .groups = 'drop') %>%
  ggplot(., aes(x=year1, y=total, group = 1)) + geom_col() +
  labs(title = "Fig 12. Terms 'diversity', 'equity', and 'inclusion' \nin academic association publications (absolute)",
       x = "year", y = "count") +
  theme_classic() +
  facet_wrap(~society_full_name, ncol = 2, scales = "free") + 
   scale_x_date(breaks = function(x) seq.Date(from = as.Date("2000-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "1 year"), date_labels = "%y", 
               expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 12.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 12.svg"), height = 4,  width = 6, dpi = 300)


dei_counts1 %>%
  mutate(prop = dei_sum/total_chr) %>% 
  group_by(society_full_name, year) %>% 
  # summarise(total = sum(term_sum), .groups = 'keep') %>% 
  rename(year1 = year) %>% 
  mutate(year1 = paste0(year1,"-01-01")) %>% 
  mutate(year1 = ymd(year1)) %>% 
  ggplot(., aes(x=year1, y=prop, group = 1)) + 
  geom_col() +
  labs(title = "Fig 13. Terms 'diversity', 'equity', and 'inclusion' \nin academic association publications (proportional)",
       x = "year", y = "proportion") +
  theme_classic() +
  facet_wrap(~society_full_name, ncol = 2, scales = "free") +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2000-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "1 year"), date_labels = "%y", 
               expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 13.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 13.svg"), height = 4,  width = 6, dpi = 300)


# # Racism and antiracism only ----------------------------------------------
dei_counts1 %>%
  rename(year1 = year) %>% 
  mutate(year1 = paste0(year1,"-01-01")) %>% 
  mutate(year1 = ymd(year1)) %>% 
  group_by(society_full_name, year1) %>%
  summarise(total = sum(race_sum), .groups = 'drop') %>%
  filter(total > 0)  %>% 
  ggplot(., aes(x=year1, y=total, group = 1)) + geom_col() +
  labs(title = "Fig 14. Terms 'racism' and 'antiracism' \nin academic association publications (absolute)",
       x = "year", y = "count") +
  theme_classic() +
  facet_wrap(~society_full_name, ncol = 2, scales = "free") +
    scale_x_date(breaks = function(x) seq.Date(from = as.Date("2000-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "1 year"), date_labels = "%y", 
               expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 14.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 14.svg"), height = 4,  width = 6, dpi = 300)

dei_counts1 %>%
  mutate(prop = race_sum/total_chr) %>% 
  group_by(society_full_name, year) %>% 
  # summarise(total = sum(term_sum), .groups = 'keep') %>% 
  rename(year1 = year) %>% 
  mutate(year1 = paste0(year1,"-01-01")) %>% 
  mutate(year1 = ymd(year1)) %>% 
  filter(prop > 0) %>% 
  ggplot(., aes(x=year1, y=prop, group = 1)) + 
  geom_col() +
  labs(title = "Fig 15. Terms 'racism' and 'antiracism' \nin academic association publications (proportional)",
       x = "year", y = "proportion") +
  theme_classic() +
  facet_wrap(~society_full_name, ncol = 2, scales = "free") +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2000-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "1 year"), date_labels = "%y", 
               expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 15.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 15.svg"), height = 4,  width = 6, dpi = 300)

dei_counts1 %>%
  rename(year1 = year) %>% 
  mutate(year1 = paste0(year1,"-01-01")) %>% 
  mutate(year1 = ymd(year1)) %>% 
  group_by(society_full_name, year1) %>%
  summarise(total = sum(gender_sum), .groups = 'drop') %>%
  ggplot(., aes(x=year1, y=total, group = 1)) + 
  geom_col() +
  labs(title = "Fig 16. Term 'gender' \nin academic association publications (absolute)",
       x = "year", y = "count") +
  theme_classic() +
  facet_wrap(~society_full_name, ncol = 2, scales = "free") + 
   scale_x_date(breaks = function(x) seq.Date(from = as.Date("2000-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "1 year"), date_labels = "%y", 
               expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 16.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 16.svg"), height = 4,  width = 6, dpi = 300)


dei_counts1 %>%
  mutate(prop = gender_sum/total_chr) %>% 
  group_by(society_full_name, year) %>% 
  # summarise(total = sum(term_sum), .groups = 'keep') %>% 
  rename(year1 = year) %>% 
  mutate(year1 = paste0(year1,"-01-01")) %>% 
  mutate(year1 = ymd(year1)) %>% 
  filter(prop > 0) %>% 
  ggplot(., aes(x=year1, y=prop, group = 1)) +
  geom_col() +
  labs(title = "Fig 17.  Term 'gender' \nin academic association publications (proportional)",
       x = "year", y = "proportion") +
  theme_classic() +
  facet_wrap(~society_full_name, ncol = 2, scales = "free") +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2000-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "1 year"), date_labels = "%y", 
               expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 17.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 17.svg"), height = 4,  width = 6, dpi = 300)

n <- fread(paste0(here, "/out/grants/nsf_all_grants_summary_data.csv"))
n[, term_sum := rowSums(.SD), .SDcols = 5:34]
n[, good_date := lubridate::parse_date_time(fund_date, "mdy")]
n[, day := lubridate::floor_date(good_date, "day")]
n[, month := lubridate::floor_date(good_date, "month")]
n[, year := lubridate::floor_date(good_date, "year")]

n[month > "2010-01-01" & month < "2021-10-01"] %>%  
  group_by(year) %>% 
  summarise(n = sum(term_sum)) %>% 
  ggplot(aes(x = year, y = n)) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line() + 
  theme_classic() +
  labs(title = "Fig 18. All DEI terms in all NSF grants by year",
       x = "time", y = "count")

ggsave(glue("{here}/graphs/Fig 18.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 18.svg"), height = 4,  width = 6, dpi = 300)


n[year > "2010-01-01" & year < "2021-10-01"] %>%  
  group_by(year) %>% 
  summarise(n = sum(`anti-racism`, antiracism, `Black Lives Matter`, BLM, `Critical Race Theory`, `George Floyd`, racism, racist, `white fragility`, `white supremacy`)) %>% 
  ggplot(aes(x = year, y = n)) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line() + 
  theme_classic() +
  labs(title = "Fig 19. All NSF grants with antiracist terms by year",
       x = "year", y = "count")

ggsave(glue("{here}/graphs/Fig 19.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 19.svg"), height = 4,  width = 6, dpi = 300)


n[year > "2010-01-01" & year < "2021-10-01" & term_sum > 2] %>% 
  group_by(year) %>% 
  summarise(amount_funded = sum(amount), .groups = "drop") %>%  
  ggplot(aes(x = year, y = amount_funded)) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line() +
  theme_classic() + 
  theme(legend.title=element_blank()) +
  scale_y_continuous(labels = dollar) +
  labs(title = "Fig 20. NSF grant funding for all DEI-related projects",
       x = "year", y = "grant amount", ) + 
  scale_color_brewer(type = "qual")

ggsave(glue("{here}/graphs/Fig 20.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 20.svg"), height = 4,  width = 6, dpi = 300)

n[year > "2010-01-01" & year < "2021-10-01"] %>% 
  mutate(status = ifelse(term_sum > 2, "DEI-related", "All other grants")) %>% 
  group_by(year, status) %>% 
  summarise(amount_funded = sum(amount), .groups = "drop") %>%  
  mutate(amount_funded = log(amount_funded)) %>% 
  ggplot(aes(x = year, y = amount_funded, colour = status)) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line() +
  theme_classic() + 
  theme(legend.title=element_blank()) +
  # scale_y_continuous(labels = dollar) +
  labs(title = "Fig 21. NSF grant funding for all DEI-related projects compared to other projects",
       x = "year", y = "grant amount (logged)", ) + 
  scale_color_brewer(type = "qual")

ggsave(glue("{here}/graphs/Fig 21.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 21.svg"), height = 4,  width = 6, dpi = 300)

n[year > "2010-01-01" & year < "2021-10-01"] %>% 
  mutate(antiracism_bool = ifelse(antiracism == TRUE | `anti-racism` == TRUE | `Black Lives Matter` == TRUE |BLM == TRUE |`Critical Race Theory` == TRUE |`George Floyd` == TRUE |racism == TRUE |racist == TRUE |`white fragility` == TRUE |`white supremacy` == TRUE, 1, 0)) %>% 
  group_by(year, antiracism_bool) %>% 
  summarise(amount_funded = sum(amount), .groups = "drop") %>% 
  filter(antiracism_bool == 1) %>% 
  ggplot(aes(x = year, y = amount_funded)) +
  geom_line() +
  theme_classic() + 
  theme(legend.title=element_blank()) +
  scale_y_continuous(labels = dollar) +
  labs(title = "Fig 22. NSF grant funding for all antiracism-related projects (absolute)",
       x = "year", y = "grant amount", ) + 
  scale_color_brewer(type = "qual")

ggsave(glue("{here}/graphs/Fig 22.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 22.svg"), height = 4,  width = 6, dpi = 300)


n[year > "2010-01-01" & year < "2021-10-01"] %>% 
  mutate(antiracism_bool = ifelse(antiracism == TRUE | `anti-racism` == TRUE | `Black Lives Matter` == TRUE |BLM == TRUE |`Critical Race Theory` == TRUE |`George Floyd` == TRUE |racism == TRUE |racist == TRUE |`white fragility` == TRUE |`white supremacy` == TRUE, 1, 0)) %>% 
  group_by(year, antiracism_bool) %>% 
  summarise(amount_funded = sum(amount), .groups = "drop") %>% 
  mutate(amount_funded = log(amount_funded)) %>% 
  mutate(status = ifelse(antiracism_bool, "Grants with \nantiracist language", "All other grants")) %>% 
  ggplot(aes(x = year, y = amount_funded, colour = as.factor(status))) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line() +
  theme_classic() + 
  theme(legend.title=element_blank()) +
  labs(title = "Fig 23. NSF grants for antiracist and all other topics by grant amount (logged)",
       x = "year", y = "grant amount (logged)", ) + 
  scale_color_brewer(type = "qual")

ggsave(glue("{here}/graphs/Fig 23.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 23.svg"), height = 4,  width = 6, dpi = 300)


n[year > "2010-01-01" & year < "2021-10-01"] %>%  
  group_by(year) %>% 
  summarise(n = sum(gender, transgender)) %>% 
  ggplot(aes(x = year, y = n)) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line() + 
  theme_classic() +
  labs(title = "Fig 24. All NSF grants with terms 'gender' and 'transgender' by year",
       x = "year", y = "count")

ggsave(glue("{here}/graphs/Fig 24.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 24.svg"), height = 4,  width = 6, dpi = 300)

n[year > "2010-01-01" & year < "2021-10-01"] %>% 
  mutate(gender_trans_bool = ifelse(gender == TRUE | transgender == TRUE, 1, 0)) %>% 
  group_by(year, gender_trans_bool) %>% 
  summarise(amount_funded = sum(amount), .groups = "drop") %>% 
  mutate(amount_funded = log(amount_funded)) %>% 
  mutate(status = ifelse(gender_trans_bool, "Grants with gender-related language", "All other grants")) %>% 
  ggplot(aes(x = year, y = amount_funded, colour = as.factor(status))) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line() +
  theme_classic() + 
  theme(legend.title=element_blank()) +
  labs(title = "Fig 25. NSF grants for gender-related and all other topics by grant amount (logged)",
       x = "year", y = "grant amount (logged)", ) + 
  scale_color_brewer(type = "qual")

ggsave(glue("{here}/graphs/Fig 25.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 25.svg"), height = 4,  width = 6, dpi = 300)

n1 <- fread(glue("{here}/out/grants/nsf_all_grants_summary_data_w_dei_terms.csv"))
n1[, good_date := lubridate::parse_date_time(fund_date, "mdy")]
n1[, day := lubridate::floor_date(good_date, "day")]
n1[, month := lubridate::floor_date(good_date, "month")]
n1[, year := lubridate::floor_date(good_date, "year")]

n2 <- n1[year > "2010-01-01" & year < "2021-10-01"] %>% 
  count(year, dei_term)

n2[dei_term != "" & n > 1 & dei_term %notin% c("racism", "transgender", "George Floyd", "Critical Race Theory", "oppression", "antiracism", "BLM", "ally")] %>% 
  ggplot(aes(x = year, y = n)) +
  facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line() + 
  geom_jitter() +
  theme_classic() +
  labs(title = "Fig 26. All NSF grants with DEI-related terms by year",
       x = "year", y = "count")


ggsave(glue("{here}/graphs/Fig 26.png"), height = 8,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 26.svg"), height = 8,  width = 6, dpi = 300)


nih <- read_fst(glue("{here}/out/grants/nih_parsed_all.fst"))
nih <- as.data.table(nih)
nih[search_term != "discrimination"] %>% 
  count(search_term, year = fiscal_year) %>% 
  mutate(search_term = ifelse(search_term == "anti-racism", "anti-racism", str_replace_all(search_term, "-", " "))) %>% 
  ggplot(aes(x = year, y = n)) +
  facet_wrap(~search_term, ncol = 2, scales = "free") +
  geom_line() + 
  geom_jitter() +
  theme_classic() +
  labs(title = "Fig 27. All NIH grants with DEI-related terms by year",
       x = "year", y = "count")

ggsave(glue("{here}/graphs/Fig 27.png"), height = 8,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 27.svg"), height = 8,  width = 6, dpi = 300)


stem_terms <- read_lines(glue("{here}/out/stem_terms.txt"))
dei_terms <- read_lines(glue("{here}/out/dei_terms.txt"))
more_dei_terms <- c("women", "minorities", "minority", "marginal", "marginalized")
dei_terms <- c(dei_terms, more_dei_terms)

ff <- fread(glue("{here}/out/grants/ford_foundation_grant_db_20220307.csv"), fill = T) %>% 
  clean_names()
ff <- ff[fiscal_year > "2012" & fiscal_year < "2022"]
ff[, description_lower := str_to_lower(description)]
ff[,dei_bool := ifelse(description_lower %like% str_to_lower(glue_collapse(dei_terms, sep = "|")) | subjects %like% str_to_lower(glue_collapse(dei_terms, sep = "|")), 1, 0)]
ff[,stem_bool := ifelse(description_lower %like% str_to_lower(glue_collapse(stem_terms, sep = "|")) | subjects %like% str_to_lower(glue_collapse(stem_terms, sep = "|")), 1, 0)]
ff[,stem_dei_bool := ifelse(stem_bool == 1 & dei_bool == 1, 1, 0)] 
setnames(ff, "fiscal_year", "year")

ff %>% 
  group_by(year, stem_bool, dei_bool) %>% 
  mutate(amount = parse_number(amount)) %>% 
  summarise(amount = sum(amount), .groups = "drop") %>% 
  rename(year1 = year) %>% 
  mutate(year1 = paste0(year1,"-01-01")) %>% 
  mutate(year1 = ymd(year1)) %>% 
  # mutate(amount = log(amount)) %>% 
  mutate(status = case_when(stem_bool == 1 & dei_bool == 1 ~ "STEM + DEI grants", 
                            stem_bool == 0 & dei_bool == 0 ~ "Non-STEM non-DEI grants", 
                            stem_bool == 1 ~ "STEM-related grants", 
                            dei_bool == 1 ~ "DEI-related grants")) %>% 
  ggplot(aes(x = year1, y = amount, colour = as.factor(status))) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free") +
  geom_line() +
  theme_classic() + 
  theme(legend.title=element_blank())  +
  scale_y_continuous(labels = dollar) +
  labs(title = "Fig 28. Ford Foundation grants to DEI and STEM-related projects",
       x = "year", y = "grant amount", ) + 
  scale_color_brewer(type = "div") +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2014-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "1 year"), date_labels = "%y", 
               expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 28.png"), height = 5,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 28.svg"), height = 5,  width = 6, dpi = 300)


ff[stem_dei_bool == 1 & str_to_lower(benefiting_populations) %like% "latin|black|minority|asian|female|color|indigenous|native|trans", bipoc_female := 1]
ff[is.na(bipoc_female), bipoc_female := 0]
ff[stem_dei_bool == 1 & str_to_lower(benefiting_populations) %like% "race ethnicity|West Africa|Quilombolas", other_beneficiary := 1]
ff[is.na(other_beneficiary), other_beneficiary := 0]

ff[stem_dei_bool == 1 & year < "2021-01-01"] %>% 
  group_by(year, bipoc_female) %>% 
  mutate(amount = parse_number(amount)) %>% 
  summarise(amount = sum(amount), .groups = "drop") %>% 
  rename(year1 = year) %>% 
  mutate(year1 = paste0(year1,"-01-01")) %>% 
  mutate(year1 = ymd(year1)) %>% 
  # mutate(amount = log(amount)) %>% 
  mutate(status = case_when(bipoc_female == 1 ~ "BIPOC/female/trans", 
                            bipoc_female == 0 ~ "None",
                            # other_beneficiary == 1 ~ "Other beneficiary population"
                            )) %>% 
  ggplot(aes(x = year1, y = amount, colour = as.factor(status))) + 
  geom_line() +
  theme_classic() + 
  theme(legend.title=element_blank())  +
  scale_y_continuous(labels = dollar) +
  labs(title = "Fig 29. Ford Foundation grants to DEI and STEM projects \nwith identified beneficiary populations",
       x = "year", y = "grant amount", ) + 
  scale_color_brewer(type = "qual") +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2014-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "1 year"), date_labels = "%y", 
               expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 29.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 29.svg"), height = 4,  width = 6, dpi = 300)


wos <- fread(glue("{here}/out/scholarship/march_2022_searches.csv"), sep = "|")
wos <- wos[year > 2004 & year < 2022]

wos[year > 2005 & year < 2022] %>% 
  rename(year1 = year) %>% 
  mutate(year1 = paste0(year1,"-01-01")) %>% 
  mutate(year = ymd(year1)) %>% 
  rename(count = hits) %>% 
  mutate(count = parse_number(count)) %>% 
  # filter(year > "2014-01-01") %>% 
  ggplot(aes(x = year, y = count)) +
  facet_wrap(~topic, scales = "free", ncol = 2, nrow = 5) +
  geom_line() + 
  theme_classic()  +
  labs(title = "Fig 30. Web of Science count of DEI terms over time (with control)",
       x = "time", y = "count") +
  scale_color_brewer(type = "qual") +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2000-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "5 years"), date_labels = "%y", 
               expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 30.svg"), height = 8,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 30.png"), height = 8,  width = 6, dpi = 300)


GrowthRate <- function(x) (x/lag(x)-1)*100 
MakeGrowthRateTable <- function(x){
  r1 <- GrowthRate(as.integer(rev(wos[topic==x]$hits)))
  r1 <- r1[!is.na(r1)]
  r1 <- mean(r1)
  r1 <- tibble(topic = x, average_growth_rate = r1)
  return(r1)
}
growth_rates <- map_df(unique(wos$topic), ~MakeGrowthRateTable(.x))
growth_rates <- growth_rates %>% arrange(average_growth_rate)
growth_rates$average_growth_rate <- paste0(round(growth_rates$average_growth_rate, 0),  "%")

gs <- read_fst(glue("{here}/out/scholarship/google_scholar.fst"))
gs %>%
  rename(year1 = year) %>%
  mutate(year1 = paste0(year1,"-01-01")) %>% 
  mutate(year = ymd(year1)) %>% 
  count(topic, year) %>% 
  filter(year < "2022-01-01" & year > "2000-01-01" & topic != "science") %>% 
  ggplot(., aes(x=year, y=n, group = 1)) + 
  geom_line() +
  facet_wrap(~topic, scales = "free", ncol = 2) +
  labs(title = "Fig 31. Google Scholar results for DEI and STEM terms",
       x = "year", y = "count") +
  theme_classic() +
   scale_x_date(breaks = function(x) seq.Date(from = as.Date("2000-01-01"), 
                                             to = as.Date("2021-01-01"), 
                                             by = "5 years"), date_labels = "%y", 
               expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 31.png"), height = 8,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 31.svg"), height = 8,  width = 6, dpi = 300)


arxiv <- fread(glue("{here}/out/scholarship/arxiv.csv"))

arxiv %>%
  mutate(year = floor_date(date, "year")) %>% 
  count(topic, year) %>% 
  filter(topic != "antiracism" & year > 2010) %>% 
  filter(topic != "critical race theory") %>%
  filter(topic != "systemic racism") %>% 
  ggplot(., aes(x=year, y=n, group = 1)) +
  geom_line() +
  geom_point() +
  facet_wrap(~topic, scales = "free", ncol = 2) +
  labs(title = "Fig 32. arXiv results for DEI terms",
       x = "year", y = "count") +
  theme_classic()
  # scale_y_continuous(labels = scales::comma) +
  # scale_x_date(breaks = function(x) seq.Date(from = as.Date("2000-01-01"),
  #                                            to = as.Date("2021-01-01"),
  #                                            by = "5 years"), date_labels = "%y",
  #              expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 32.png"), height = 8,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 32.svg"), height = 8,  width = 6, dpi = 300)

pubmed <-  read_fst(glue("{here}/out/scholarship/pubmed.fst"))

pubmed %>%
  mutate(create_date = ymd(create_date), 
         year = floor_date(create_date, "year")) %>% 
  count(topic, year) %>%
  filter(year > "2010-01-01" & year < "2022-01-01") %>%
  ggplot(., aes(x=year, y=n)) +
  geom_line() +
  facet_wrap(~topic, scales = "free", ncol = 2)  +
  labs(title = "Fig 33. PubMed results for DEI terms (recent)",
       x = "year", y = "count") +
  theme_classic()  +
  # scale_y_continuous(labels = scales::comma) +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2000-01-01"),
                                             to = as.Date("2021-01-01"),
                                             by = "2 years"), date_labels = "%y",
               expand = c(0,0))

ggsave(glue("{here}/graphs/Fig 33.png"), height = 8,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 33.svg"), height = 8,  width = 6, dpi = 300)


pubmed %>%
  mutate(create_date = ymd(create_date), 
         year = floor_date(create_date, "year")) %>% 
  count(topic, year) %>%
  ggplot(., aes(x=year, y=n)) +
  geom_line() +
  facet_wrap(~topic, scales = "free", ncol = 2)  +
  labs(title = "Fig 34. PubMed results for DEI terms (longitudinal)",
       x = "year", y = "count") +
  theme_classic() 

ggsave(glue("{here}/graphs/Fig 34.png"), height = 8,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 34.svg"), height = 8,  width = 6, dpi = 300)


pubmed_control <- fread(glue("{here}/out/scholarship/PubMed_total_records_by_publication_year.csv")) %>% clean_names()

pubmed_control %>%
  mutate(year = paste0(citation_year,"-01-01"),
         year = ymd(year), 
         year = floor_date(year, "year")) %>% 
  filter(year > "1950-01-01" & year < "2022-01-01") %>%
  rename(count = x2022) %>% 
  ggplot(., aes(x=year, y=count)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Fig 35. Growth of PubMed database since 1950",
       x = "year", y = "count") +
  theme_classic() 

ggsave(glue("{here}/graphs/Fig 35.png"), height = 4,  width = 6, dpi = 300)
ggsave(glue("{here}/graphs/Fig 35.svg"), height = 4,  width = 6, dpi = 300)

