
library(tidyverse)
library(data.table)
library(lubridate)
library(glue)
library(cowplot)
# library(ggpubr)
# library(egg)
# library(magick)

# library(here)
# tiff("./error_analysis_bruna/response_ms/fig1.tiff", units="in", width=6, height=10, res=600)
# tiff("./error_analysis_bruna/response_ms/fig1.pdf", units="in", width=6, height=10, res=600)
t <- fread("./out/twitter/tweets_clean.csv")
t[,good_date := as_date(created_at)]
t[,good_date := ymd(good_date)]
t[,short_name := str_remove(file, glue("./data/_twitter/twitter_account_dei_term/"))]
t[,school := str_extract(short_name, "^.*?(?=--)")]
t[,account := map_chr(short_name, ~str_split(.x, "--")[[1]][2])]
t[,dei_term := str_remove(map_chr(short_name, ~str_split(.x, "--")[[1]][3]), "\\.Rds")]
t[,month := lubridate::floor_date(good_date, "month")]
# unique(t, by = "account")
# t %>%  distinct(dei_term)

# t %>%
#   group_by(school, good_date) %>%
#   summarise(total = n(), .groups = 'keep') %>%
#   ggplot(., aes(x=good_date, y=total, group = 1)) + geom_line() +
#   labs(title = "Fig 7. All DEI-related Tweets from all school-related accounts over time",
#        x = "year", y = "count") +
#   theme_classic() +
#   theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
#   theme(plot.title = element_text(hjust = 0.5))
# #
# # ggsave(glue("{here}/graphs/Fig 7.png"), height = 4,  width = 6, dpi = 300)
# # ggsave(glue("{here}/graphs/Fig 7.svg"), height = 4,  width = 6, dpi = 300)




# terms excluded/kept
# "advocacy", "multicultural","race","anti-racism","bias","systemic","BLM",
# "intersectional","George Floyd","microaggression","oppression",
# "Critical Race Theory","implicit bias","antiracism","white supremacy"

t_for_fig<-t %>% filter(dei_term=="racism"|
                          dei_term=="diversity"|
                          dei_term=="equity"|
                          dei_term=="gender"|
                          dei_term=="inclusion"|
                          dei_term=="justice"|
                          dei_term=="privilege"|
                          dei_term=="ally"|
                          dei_term=="white fragility")



plot_nas_original<-t_for_fig %>%
  group_by(month, dei_term) %>%
  summarise(total = n(), .groups = 'keep') %>%
  filter(month > "2016-01-01") %>%
  ggplot(., aes(x=month, y=total, group = 1)) + geom_line() +
  # labs(title = "DEI-related Tweets from all school-related accounts \nby DEI term",x = "year", y = "count") +
  labs(x = "year", 
       y = "count",
       tag = "A")+
  theme_classic() +
  # theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  # theme(plot.title = element_text(hjust = 0.5)) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free_y") + # original used 3 cols
  facet_wrap(~dei_term, ncol = 1, scales = "free_y") +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2015-01-01"),
                                             to = as.Date("2021-01-01"),
                                             by = "1 year"), date_labels = "%y",
               expand = c(0,0))+
  theme(plot.margin = margin(1,1,1,1, "cm"))+
  theme(
    # panel.background = element_rect(fill = "white"),
    plot.margin = margin(t = 0.5, r = 1, b = 0.5, l = 0.0, unit = "cm"))
#
# plot_nas_redo<-t %>%
plot_nas_redo<-t_for_fig %>% # used a reduced number of terms
  group_by(month, dei_term) %>%
  summarise(total = n(), .groups = 'keep') %>%
  filter(month > "2016-01-01") %>%
  ggplot(., aes(x=month, y=total, group = 1)) + geom_line() +
  labs(x = "year", 
       y = "count",
       tag = "B")+
  theme_classic() +
  # theme(axis.text.x = element_text(angle = 55, hjust=1, size = 7)) +
  # theme(plot.title = element_text(hjust = 0.5)) +
  # facet_wrap(~dei_term, ncol = 3, scales = "free_y") +
  # facet_wrap(~dei_term, ncol = 3) +. #original used 3 cols, all terms
  facet_wrap(~dei_term, ncol = 1) +
  scale_x_date(breaks = function(x) seq.Date(from = as.Date("2015-01-01"),
                                             to = as.Date("2021-01-01"),
                                             by = "1 year"), date_labels = "%y",
               expand = c(0,0))+
  theme(
    plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 1, unit = "cm"))

# Alternatives to make and save figures in publication quality 
# fig1<-ggarrange(plot_nas_original, plot_nas_redo,ncol=2,
#           labels = c("A", "B") %>%
#             ggexport(filename = "./error_analysis_bruna/response_ms/fig1.pdf")
# fig1<-ggarrange(plot_nas_original, plot_nas_redo,ncol=2,
#                 labels = c("A", "B"))
# library(patchwork)
# fig1<-plot_nas_original+plot_nas_redo + plot_layout(ncol = 2)

# library(cowplot)
fig1<-plot_grid(plot_nas_original,plot_nas_redo, ncol = 2, align = "hv")

# grid.arrange(plot_nas_original, plot_nas_redo,ncol = 2)

ggsave("./error_analysis_bruna/response_ms/fig1.pdf", fig1,width = 6, height = 9, units = "in", dpi = 600)
# dev.off()

ggsave("./error_analysis_bruna/response_ms/fig1.tiff", fig1,width = 6, height = 9, units = "in", dpi = 600)
# dev.off()