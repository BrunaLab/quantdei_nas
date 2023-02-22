here <- rprojroot::find_rstudio_root_file()
library(glue)
source(glue("{here}/code/setup.R"))

Sys.setenv('google_key' = "")
Sys.setenv('google_cx' = "")

google_key <- Sys.getenv('google_key')
google_cx <- Sys.getenv('google_cx')

# Get all university twitter accounts -------------------------------------
# 1. Search all of them and "twitter" and pull the first result
# 2. Get a dictionary of DEI terms 
# 3. Search across them in all of the university twitter accounts

# 1. Get the twitter handles by searching the school names in google --------

json_dir <- glue("{here}/data/twitter/school_twitter_account_searches")
bearer_token1 <- ""
bearer_token <- ""

schools <- fread(glue("{here}/data/uni_websites/university_info/top_100_schools_clean.csv"))
schools[,short_name := map_chr(site, ~str_remove(.x, "\\.edu"))]

get_school_twitter_handles <- function(school_f){
  country = "us"
  # school_f <- schools$short_name[1]
  school_name_to_search <- schools[short_name == school_f]$university[1]
  # short_name <- school_f
  
  print(school_name_to_search)
  keyword <- glue('{school_name_to_search} twitter')
  keyword <- str_replace_all(keyword, " ", "%20")
  
  file_name <- paste0(school_f, "_twitter-search")
  file_name <- str_replace_all(file_name, " ", "-")
  
  json_out <- paste0(json_dir, "/", file_name, ".json" )
  
  if(file.exists(json_out)){
    print(paste0(file_name, " exists"))
    return
  }
  # country <- "us"
  url1 <- glue("https://www.googleapis.com/customsearch/v1?key={google_key}&q={keyword}&gl={country}&hl=en&cx={google_cx}&filter=0")
  print(url1)
  
  try(d2 <- read_json(url1))
  if(!exists("d2")){
    print("didnt' download properly")
    return
  }
  d3 <- map_df(d2$items, ~as.data.table(.x))
  d3_row <- nrow(d3)
  # if(d3_row == 0){
    # print(paste("page", round_page , "is empty"))
    # break
  # }
  jsonlite::write_json(d2, json_out)
  rm(d2)
  print(glue("searching on {school_name_to_search}"))
  return
  
}
## This is done. 
# map(schools$short_name, ~get_school_twitter_handles(.x))


# With those results, pull out all the twitter handles --------------------

json_dir_files <- list.files(json_dir, full.names = T)
json_dir_files_short <- list.files(json_dir, full.names = F)
json_dir_both <- as.data.table(cbind(json_dir_files, json_dir_files_short))

extract_twitter_handles <- function(file){
  # file <- json_dir_both$json_dir_files[1]
  file_short_name <- json_dir_both[json_dir_files == file]$json_dir_files_short
  full_text <- fread(file, sep = NULL, header = F)
  twitter_handles1 <- str_extract_all(full_text, 'twitter\\.com/[a-z].*?[\\?\\"/]')
  twitter_handles2 <- as.data.table(unique(map(twitter_handles1, ~str_extract(.x, '(twitter\\.com/)(\\w+)'))[[1]]))
  res <- as.data.table(cbind(file_short_name, twitter_handles2))
  names(res) <- c("file_short_name", "twitter_handle")
  return(res)
}

# This is done. Just reading it in now. 
# twitter_handles <- map_df(json_dir_both$json_dir_files, ~extract_twitter_handles(.x))
# twitter_handles %>% fwrite("./data/twitter/schools_twitter_handles.csv")

twitter_handles <-  fread(glue("{here}/data/twitter/schools_twitter_handles.csv"))


# With twitter handles, pull all tweets with DEI terms in them ------------

dei_terms <- read_lines(glue("{here}/data/dei_terms.txt"))
terms_to_drop <- c("1619", "trans", "racist", "Kendi")
dei_terms <- dei_terms[dei_terms %notin% terms_to_drop]

queries <- rbindlist(cross2(twitter_handles$twitter_handle, dei_terms))
names(queries) <- c("twitter_handle", "dei_term")
queries <- twitter_handles[queries, on = "twitter_handle", allow.cartesian = TRUE]

queries[,school := str_extract(file_short_name, "\\w+(?=_)")]
queries <- queries[twitter_handle %notlike% "embed|hashtag"]
queries[,rowid := .I]

# queries$file_short_name <- NULL

# Get DEI terms from each of the schools ----------------------------------

tweet_outpath <- glue("{here}/data/twitter/twitter_account_dei_term")

get_tweets <- function(rowid){
  # rowid <- 2
  
  school_twitter <- str_remove(queries[rowid]$twitter_handle, "twitter\\.com/")
  
  if(nchar(school_twitter) < 2){
    print(paste0(school_twitter, " bad"))
    return()
  }
  school <- queries[rowid]$school
  print(school_twitter)
  dei_term <- queries[rowid]$dei_term
  my_query <-  build_query(query = dei_term, users = school_twitter)
  print(my_query)
  file_out <- glue("{tweet_outpath}/{school}--{school_twitter}--{dei_term}.Rds")
  
  if(!file.exists(file_out)){
    get_all_tweets(query = my_query, start_tweets = "2010-01-01T00:00:00Z", end_tweets = "2021-10-11T00:00:00Z", bearer_token = bearer_token, data_path = "{tweet_outpath}", file = file_out)
    Sys.sleep(1)
    return()
  }else{
    print(paste0(file_out, " exists"))
    return()
  }
}

map(queries$rowid, ~get_tweets(.x))


# Count all their total number of tweets ----------------------------------
# 
for(school in schools){
   outfile <- glue("{here}/data/twitter/{school}_count.csv")

   if(file.exists(outfile)){
     print(paste0(outfile, " exists"))
     next
   }
#
   my_query1 <-  build_query(users = school)
#
   count <- count_all_tweets(
     query = my_query1,
     start_tweets = "2010-01-01T00:00:00Z",
     end_tweets = "2021-10-11T00:00:00Z",
     bearer_token = bearer_token,
     n = 100000,
     file = NULL,
     data_path = NULL,
     export_query = TRUE,
     bind_tweets = TRUE,
     granularity = "day",
     verbose = TRUE
   )

   fwrite(count, outfile)

 }
#

# tweets <-
#   get_all_tweets(
#     query = "#BlackLivesMatter",
#     start_tweets = "2020-01-01T00:00:00Z",
#     end_tweets = "2020-01-05T00:00:00Z",
#     file = "blmtweets.json",
#     bearer_token = bearer_token
#   )

# res <- as.data.table(bind_tweets(data_path = "./data/twitter", user = TRUE, output_format = "tidy"))
