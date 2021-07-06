library(magrittr)
devtools::load_all()
#### Function call ####

getData("#user2021",
        5000,
        "data/useR_tweets.csv.gz",
        "rtweet-exploration",
        include_rts = FALSE,
        retryonratelimit = TRUE)
