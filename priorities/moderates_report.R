devtools::install_github("civisanalytics/civis-r")
install.packages("ggrepel")
# FINAL REPORT

library(ggplot2)
library(civis.deckR)
library(dplyr)

##############################################################################
# Gather Tweet Data for the last 7 Days
##############################################################################

# SCRIPT_ID <- 9336209
# TOPICS_FILE_ID <- 5836414
# TREND_SPLIT_DAYS <- 2
# START_DATE <- format(Sys.Date() - 7, "%Y-%m-%d", tz = "US/Eastern")
# END_DATE <- format(Sys.Date(), "%Y-%m-%d", tz = "US/Eastern")
# 
# arguments <- list(START_DATE = START_DATE,
#                   END_DATE = END_DATE,
#                   TOPICS_FILE_ID = TOPICS_FILE_ID,
#                   TREND_SPLIT_DAYS = TREND_SPLIT_DAYS)
# 
# # Let's only run the script if it hasn't yet been run today:
# 
# current_status <- civis::scripts_list_custom_runs(SCRIPT_ID)[[1]]
# last_run <- current_status$startedAt
# run_id <- current_status$id
# 
# if (as.Date(last_run) < Sys.Date()) {
#   # Update the args to the template script, kick it off and wait for completion.
#   civis::scripts_put_custom(SCRIPT_ID, arguments = arguments)
#   run_id <- civis::scripts_post_custom_runs(SCRIPT_ID)[["id"]]
#   civis::await(civis::scripts_get_custom_runs, id = SCRIPT_ID, run_id = run_id)
# }
# # Download output of the script
# outputs <- civis::scripts_list_custom_runs_outputs(SCRIPT_ID, run_id)[[1]]
# civis::download_civis(outputs[["objectId"]], "outputs.tar.gz")
# untar("outputs.tar.gz")

##############################################################################
# Data Cleaning Methods
##############################################################################

prettify_clusters <- function(df) {
  df <- df %>%
    mutate(community = case_when(
      High.activity.conservatives == 1 ~ "High Activity Conservatives",
      High.activity.progressives == 1 ~ "High Activity Progressives",
      Low.activity.moderates == 1 ~ "Low Activity Moderates",
      Medium.activity.moderates == 1 ~ "Medium Activity Moderates",
      TRUE ~ "Other")
    )
}

remove_bad_urls <- function(df) {
  df <- df %>%
    dplyr::filter(!stringr::str_detect(urls, "unfollowspy"),
                  !stringr::str_detect(urls, "fllwrs"),
                  !stringr::str_detect(urls, "whounfollowedme"),
                  !stringr::str_detect(urls, "z5concept"),
                  !stringr::str_detect(urls, "cards[.]twitter"),
                  !stringr::str_detect(urls, "twittascope"),
                  !stringr::str_detect(urls, "cloudfront"),
                  !stringr::str_detect(urls, "boingboing"),
                  !stringr::str_detect(urls, "oovoo")
    )
}

##############################################################################
# Link Plot Cleaning Methods
##############################################################################

filter_in_timeframe <- function(df, top_days) {
  df %>%
    dplyr::mutate(date = as.Date(posted_time)) %>%
    dplyr::filter(date %in% unique(rev(sort(.$date)))[1:top_days])
}

get_domain_name <- function(urls) {
  no_www <- ifelse(stringr::str_detect(urls, "www"), substring(urls, 5), substring(urls, 1) )
  split_by_slash = stringr::str_split_fixed(no_www, "/", 2)
  split_by_slash[,1]
}

clean_urls <- function(df) {
  df$urls <- gsub(pattern = "\\?[\\s\\S]+?$", replacement = "", x = df$urls, perl = TRUE)
  df$urls <- tolower(df$urls)
  df %>%
    dplyr::filter(urls != "") %>%
    dplyr::mutate(url_titles = ifelse(url_titles == "", urls, as.character(url_titles)))
}

calculate_user_share <- function(df, x, n) {
  df %>%
    dplyr::mutate(user_share = n_users/length(unique(tweets$user_id[tweets$community == x]))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(x = rep(1, n), y = rev(seq(n)))
}

##############################################################################
# Aggregation Functions
##############################################################################

get_unique_words <- function(x) {
  words <- stringr::str_split(x, " ") %>% unlist() %>% unique()
  paste(words, collapse = " ")
}

collapse_users <- function(tweets) {
  tweets %>%
    filter(!is.na(probability.conservative)) %>%
    group_by(user_id,
             community,
             politics.activity,
             probability.conservative) %>%
    summarize(uniq_words = get_unique_words(tokens_ws))
}

get_top_words <- function(users, n = 100) {
  num_users <- nrow(users)
  top_words <- users$uniq_words %>%
    stringr::str_split(pattern = " ") %>%
    unlist() %>%
    tibble::as.tibble() %>%
    group_by(word = value) %>%
    summarize(n = n(),
              percent_of_user_share = n() / num_users) %>%
    filter(word != "") %>%
    arrange(desc(n)) %>%
    .[1:n, ]
  top_words$p_conserv <- get_words_p_conservative(users, top_words)
  top_words
}

get_words_p_conservative <- function(users, top_words) {
  toks_by_user <- stringr::str_split(users$uniq_words, " ")
  p_conserv <- sapply(top_words$word, function(word) {
    contains_word <- sapply(toks_by_user, function(x) word %in% x)
    mean(users[contains_word, ]$probability.conservative)
  })
  p_conserv
}

get_top_words_by_community <- function(users, n=10) {
  top_words <- lapply(unique(users$community), function(x) {
    get_top_words(users[users$community == x, ], n)
  })
  names(top_words) <- unique(users$community)
  top_words
}

# util/formatting functions ---
minmax <- function(x) c(min(x), max(x))
mydate <- function(x) {
  dates <- minmax(x) %>% format(format = "%b %d")
  paste("From", format(min(x), format = "%b %d"), "to", dates[2])
}
dotdot <- function(y) {
  lapply(y, function(x) {
    ifelse(nchar(x) < 100, x, paste0(substr(x, 1, 100), "..."))
  }) %>% unlist
}

### Store values
n <- 10
users <- collapse_users(tweets)
top_words <- get_top_words(users, n)
top_words_by_community <- get_top_words_by_community(users, n)
top_words_by_community$Other <- NULL
top_words_by_community <- Filter(Negate(is.null), top_words_by_community)
communities <- names(top_words_by_community)

##############################################################################
# Get Top Content Function
##############################################################################

prepare_top_content_tweets <- function(tweets, x, top_days) {
  tweets <- filter_in_timeframe(tweets, top_days)
  tweets <- dplyr::filter(tweets, community == x)
  tweets <- remove_bad_urls(tweets)
  tweets <- clean_urls(tweets)
  
  tweets %>%
    # i group by user id and then again by urls to get user share
    dplyr::select(user_id, urls, url_titles, url_descriptions) %>%
    unique() %>%
    dplyr::group_by(urls, url_titles) %>%
    # how many times is each url coming up
    dplyr::summarize(n_users = n()) %>%
    dplyr::arrange(dplyr::desc(n_users)) %>%
    # get the domains here
    dplyr::mutate(domain = get_domain_name(urls))
}

get_top_content <- function(tweets, x, n, top_days = 7) {
  tweets <- prepare_top_content_tweets(tweets, x, top_days)
  tweets[1:n, ] 
}

get_top_partisan_content <- function(tweets, x, n, partisan_df, top_days = 7) {
  tweets <- prepare_top_content_tweets(tweets, x, top_days)
  
  tweets %>%
    # only include certain domains
    dplyr::inner_join(dplyr::select(partisan_df, domain = item, item)) %>%
    .[1:n, ]
}

get_top_shared_content <- function(tweets, x, n, shared_df, top_days = 7) {
  tweets <- prepare_top_content_tweets(tweets, x, top_days)
  
  tweets %>%
    # only include items that shared_df also tweeted
    dplyr::filter(url_titles %in% shared_df$url_titles) %>%
    .[1:n, ]
}

##############################################################################
# Plot Helpers
##############################################################################
color_lookup <- data.frame(
  communities = sort(communities),
  colors = sapply(c(civisreds, civisblues, civisgreens, civisyellows),
                  function(f) f(9)[7])
)

add_ideology_color <- function(tweets) {
  tweets <- mutate(tweets, ideology.color = "black")
  for (i in 1:n) {
    if (tweets$domain[i] %in% left_news$item) {
      tweets$ideology.color[i] <- "blue"
    } else if (tweets$domain[i] %in% right_news$item) {
      tweets$ideology.color[i] <- "red"
    }
  }
  tweets
}

link_plot <- function(df, x, n) {
  color <- as.character(color_lookup$colors[color_lookup$communities == x])
  df %>%
    civis.deckR::ggplot(ggplot2::aes(as.numeric(rev(reorder(x, user_share))), y)) +
    ggplot2::geom_text(aes(label = dotdot(as.character(url_titles))),
                       hjust = 0, family = "Lato", size = 4,
                       fontface = "bold") +
    geom_label(aes(label = scales::percent(round(user_share, 4)),
                   x = x - .525, fill = user_share, color = NA)) +
    geom_text(aes(label = scales::percent(round(user_share, 4)),
                  x = x - .525), family = "Lato") +
    ggplot2::geom_text(aes(label = domain,
                           y = y - .25),
                       hjust = 0, family = "Lato", size = 3.5,
                       fontface = "italic") +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   panel.border = element_blank()) +
    ggplot2::xlim(c(-.000005, n)) +
    ggplot2::ylim(c(-.005, n + .05)) +
    guides(fill = F) +
    scale_fill_gradient(high = generate_color_pal(10, color2 = color)[8],
                        low = generate_color_pal(10, color2 = color)[3])
}

tabbed_link_plot <- function(df, x, n) {
  color <- as.character(color_lookup$colors[color_lookup$communities == x])
  df %>%
    civis.deckR::ggplot(ggplot2::aes(as.numeric(rev(reorder(x, user_share))), y)) +
    ggplot2::geom_text(aes(label = dotdot(as.character(url_titles))),
                       hjust = 0, family = "Lato", size = 4,
                       fontface = "bold") +
    geom_label(aes(label = scales::percent(round(user_share, 4)),
                   x = x - .525, fill = user_share, color = NA)) +
    geom_text(aes(label = scales::percent(round(user_share, 4)),
                  x = x - .525), family = "Lato") +
    ggplot2::geom_text(aes(label = domain,
                           y = y - .25),
                       hjust = 0, family = "Lato", size = 3.5,
                       fontface = "italic", color = df$ideology.color) +
    ggplot2::theme(axis.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   panel.grid = ggplot2::element_blank(),
                   axis.title = ggplot2::element_blank(),
                   panel.border = element_blank()) +
    ggplot2::xlim(c(-.000005, n)) +
    ggplot2::ylim(c(-.005, n + .05)) +
    guides(fill = F) +
    scale_fill_gradient(high = generate_color_pal(10, color2 = color)[8],
                        low = generate_color_pal(10, color2 = color)[3])
}

##############################################################################
# Link Plots
##############################################################################

create_link_plot <- function(tweets, x, n) {
  tweets <- calculate_user_share(tweets, x, n)
  link_plot(tweets, x, n)
}

create_tabbed_link_plot <- function(tweets, x, n) {
  tweets <- calculate_user_share(tweets, x, n)
  tweets <- add_ideology_color(tweets)
  tabbed_link_plot(tweets, x, n)
}

general_link_plot <- function(tweets, x, n = 8, top_days = 7) {
  tweets <- get_top_content(tweets, x, n, top_days)
  p <- create_tabbed_link_plot(tweets, x, n)
  p + ggtitle(paste0("Content trending among the ", cleanup(x), " Community"))
}

far_left_link_plot <- function(tweets, x = "Medium Activity Moderates", n = 8, top_days = 7) {
  tweets <- get_top_partisan_content(tweets, x, n, left_news, top_days)
  p <- create_link_plot(tweets, x, n)
  p + ggtitle(paste0("Far left content trending among the ", cleanup(x), " Community"))
}

far_right_link_plot <- function(tweets, x = "Medium Activity Moderates", n = 8, top_days = 7) {
  tweets <- get_top_partisan_content(tweets, x, n, right_news, top_days)
  p <- create_link_plot(tweets, x, n)
  p + ggtitle(paste0("Far right content trending among the ", cleanup(x), " Community"))
}

shared_left_link_plot <- function(tweets, x = "Medium Activity Moderates", n = 8, top_days = 7) {
  left_tweets <- get_top_content(tweets, "High Activity Progressives", n = 50, top_days = 7)
  tweets <- get_top_shared_content(tweets, x, n, left_tweets, top_days)
  p <- create_tabbed_link_plot(tweets, x, n)
  p + ggtitle(paste0("Content left are reading trending among the ", cleanup(x), " Community"))
}

shared_right_link_plot <- function(tweets, x = "Medium Activity Moderates", n = 8, top_days = 7) {
  right_tweets <- get_top_content(tweets, "High Activity Conservatives", n = 50, top_days = 7)
  tweets <- get_top_shared_content(tweets, x, n, right_tweets, top_days)
  p <- create_tabbed_link_plot(tweets, x, n)
  p + ggtitle(paste0("Content right are reading trending among the ", cleanup(x), " Community"))
}

##############################################################################
# Read in Data
##############################################################################

# These files are from the output of that custom script
tweets_df <- read.csv("outputs/tweets_31.csv")
tweets <- prettify_clusters(tweets_df)
tweets <- remove_bad_urls(tweets)
right_news <- read.csv("partisan_domains/far_right_domains.csv")
left_news <- read.csv("partisan_domains/far_left_domains.csv")
