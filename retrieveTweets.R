# Load required Twitter libraries
library(twitteR)
library(ROAuth)
library(streamR)

# Credentials for application 'stefano_airlines-2'

consumerKey <- "9fuIXgG6dhm4o3eibay9FfAfq"
consumerSecret <- "gqUeUPd9JjiMo23ZtzLcWAvbC0rkT0CnBOZ4tbvQtC4kELvfG3"
access_token<-"3334284022-flsjL2IwG7x9ymsnVJM3e2hlJMppJYOZJYXkMr2"
access_secret<-"leVPggxC2UHINnOBr8m6KJsUvdyyYlMVBJxW7pbeVrD60"

setup_twitter_oauth(consumerKey,
                    consumerSecret,
                    access_token,
                    access_secret)

appURL <- "https://api.twitter.com/oauth2/token"
reqURL <- "https://api.twitter.com/oauth/request_token"
authURL <- "https://api.twitter.com/oauth/authorize"
accessURL <- "https://api.twitter.com/oauth/access_token"

twitCred <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret,
                             requestURL=reqURL,
                             accessURL=accessURL,
                             authURL=authURL)

twitCred$handshake()

# Download tweets

ryanair <- searchTwitter('ryanair,Ryanair', n=10000, lang='en',
              since='2015-12-01')

easyJet <- searchTwitter('easyJet,easyjet', n=10000, lang='en',
                         since='2015-12-01')

vueling <- searchTwitter('vueling,Vueling', n=10000, lang='en',
                         since='2015-12-01')

norwegian <- searchTwitter('norwegian,Norwegian,@fly_norwegian',
                           n=10000, lang='en',
                           since='2015-12-01')

ba <- searchTwitter('@british_airways,British Airways',
                           n=10000, lang='en',
                           since='2015-12-01')

# Save tweets on disk
save(ryanair,file="ryanair.RData")
save(easyJet,file="easyJet.RData")
save(vueling,file="vueling.RData")
save(norwegian,file="norwegian.RData")
save(ba,file="ba.RData")









