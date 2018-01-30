devtools::install_github("civisanalytics/civis-r")
install.packages("ggrepel")
# FINAL REPORT

library(ggplot2)
library(civis.deckR)
library(dplyr)

##############################################################################
# Gather Tweet Data for the last 7 Days
##############################################################################

SCRIPT_ID <- 9336209
TOPICS_FILE_ID <- 5836414
TREND_SPLIT_DAYS <- 2
START_DATE <- format(Sys.Date() - 7, "%Y-%m-%d", tz = "US/Eastern")
END_DATE <- format(Sys.Date(), "%Y-%m-%d", tz = "US/Eastern")

arguments <- list(START_DATE = START_DATE,
                  END_DATE = END_DATE,
                  TOPICS_FILE_ID = TOPICS_FILE_ID,
                  TREND_SPLIT_DAYS = TREND_SPLIT_DAYS)

# Let's only run the script if it hasn't yet been run today:

current_status <- civis::scripts_list_custom_runs(SCRIPT_ID)[[1]]
last_run <- current_status$startedAt
run_id <- current_status$id

if (as.Date(last_run) < Sys.Date()) {
  # Update the args to the template script, kick it off and wait for completion.
  civis::scripts_put_custom(SCRIPT_ID, arguments = arguments)
  run_id <- civis::scripts_post_custom_runs(SCRIPT_ID)[["id"]]
  civis::await(civis::scripts_get_custom_runs, id = SCRIPT_ID, run_id = run_id)
}
# Download output of the script
outputs <- civis::scripts_list_custom_runs_outputs(SCRIPT_ID, run_id)[[1]]
civis::download_civis(outputs[["objectId"]], "outputs.tar.gz")
untar("outputs.tar.gz")

##############################################################################
# Read in Data
##############################################################################

# These files are from the output of that custom script
tweets <- read.csv("outputs/tweets.csv")
tweets <- tweets %>%
  mutate(community = case_when(
    High.activity.conservatives == 1 ~ "High Activity Conservatives",
    High.activity.progressives == 1 ~ "High Activity Progressives",
    Low.activity.moderates == 1 ~ "Low Activity Moderates",
    Medium.activity.moderates == 1 ~ "Medium Activity Moderates",
    TRUE ~ "Other")) %>%
  dplyr::filter(!stringr::str_detect(urls, "unfollowspy"),
                !stringr::str_detect(urls, "fllwrs"),
                !stringr::str_detect(urls, "whounfollowedme"),
                !stringr::str_detect(urls, "z5concept"),
                !stringr::str_detect(urls, "cards[.]twitter"),
                !stringr::str_detect(urls, "twittascope"),
                !stringr::str_detect(urls, "cloudfront"),
                !stringr::str_detect(urls, "boingboing"),
                !stringr::str_detect(urls, "oovoo"))

get_top_content <- function(tweets, x, n, top_days = 7, partisan_df = NULL) {
  tweets %>%
    dplyr::mutate(date = as.Date(posted_time)) %>%
    dplyr::filter(date %in% unique(rev(sort(.$date)))[1:top_days]) %>%
    dplyr::filter(urls != "", community == x,
                  !stringr::str_detect(urls, "unfollowspy"),
                  !stringr::str_detect(urls, "fllwrs"),
                  !stringr::str_detect(urls, "whounfollowedme"),
                  !stringr::str_detect(urls, "z5concept"),
                  !stringr::str_detect(urls, "cards[.]twitter")) %>%
    # i group by user id and then again by urls to get user share
    dplyr::select(user_id, urls, url_titles, url_descriptions) %>%
    unique() %>%
    dplyr::group_by(urls, url_titles) %>%
    # how many times is each url coming up
    dplyr::summarize(n_users = n()) %>%
    dplyr::arrange(dplyr::desc(n_users)) %>%
    
    # i'm just joining to trends to get the domains here
    #dplyr::left_join(dplyr::select(trends, urls = item, domain)) %>%
    
    # get the domains here
    dplyr::mutate(domain = domain_name(urls)) %>%
    # only include certain domains
    if (!is.null(partisan_df)) dplyr::inner_join(dplyr::select(partisan_df, domain = item, item)) %>%
    .[1:n, ] 
}

