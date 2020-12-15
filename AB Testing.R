library(data.table)
library(plm)
library(stargazer)
library(MatchIt)

### Reading in the data ###

df <- read.csv('company_data.csv')

### Checking data type of each variable ###

sapply(df, class)

### Converting data types to usable form ###

df$company <- as.factor(df$company)
df$sector <- as.factor(df$sector)
df$industry <- as.factor(df$industry)
df$election_year <- as.factor(df$election_year)
df$treat <- as.factor(df$treat)
df$year <- as.factor(df$year)

### Descriptive Statistics ###

summary(df)


##Retrieve data from social media (e.g. twitter or reddit or other social media sources)
# Reddit Analyze
library(RedditExtractoR)
library(sentimentr)
library(dplyr)


#############################TIME WARNING!!!(BELOW)######################################




# Create lists of securities, tickers, target subreddit.
assets_list <- c('List of Securities')

ticker <- c ('List of Securities')

target_subr <- c('politics', 'business')

# Get Reddit of the first 8th securities
mid_df <- data.frame()
RedditText_df <- data.frame()
for (x in 1:3) {
  print(assets_list[x])
  for (y in 1:2) {
    tem_df <- data.frame()
    tem_df <- get_reddit(assets_list[x], subreddit = target_subr[y], 
                         page_threshold = 400, sort_by="new")
    mid_df <- rbind(mid_df, tem_df)
  }
  name_col <- rep(ticker[x], nrow(mid_df))
  mid_df <- cbind(name_col, mid_df)
  RedditText_df <- rbind(RedditText_df, mid_df)
  mid_df <- data.frame()
}


# Change the Date Format
RedditText_df$post_date <- RedditText_df$post_date %>% as.Date('%d-%m-%y')
RedditText_df$comm_date <- RedditText_df$comm_date %>% as.Date('%d-%m-%y')

# Delete the URL details
#RedditText_rd <- RedditText_df[-c(17:19)]


# Filter Data by Date
RedditText_rd <- RedditText_df %>% subset(post_date > as.Date("2018-01-01"))
RedditText_rd <- RedditText_df %>% subset(post_date < as.Date("2020-11-22"))
head(RedditText_rd)
# Save the Raw Reddit Data
#write.csv(RedditText_rd, "RedditAnalyticsf11.csv")

library(plyr)
library(ggplot2)
library(wordcloud)
library(dplyr)
library(stringr)
list=RedditText_rd
list=list[-c(2,3,4,6,7)]
list=list[-c(11:15)]
#list<-read.csv("./RedditAnalyticsf10.csv")
head(list, n=5)
score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  scores <- laply(sentences, function(sentence, pos.words, neg.words){
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('\\d+', "", sentence)
    sentence <- tolower(sentence)
    word.list <- str_split(sentence, '\\s+')
    words <- unlist(word.list)
    pos.matches <- match(words, pos.words)
    neg.matches <- match(words, neg.words)
    pos.matches <- !is.na(pos.matches)
    neg.matches <- !is.na(neg.matches)
    score <- sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress)
  scores.df <- data.frame(score=scores, text=sentences)
  return(scores.df)
}

pos <- scan('C:/Users/palla/Desktop/Positivewords.txt', what='character', comment.char=';') #folder with positive dictionary
neg <- scan('C:/Users/palla/Desktop/Negativewords.txt', what='character', comment.char=';') #folder with negative dictionary
pos.words <- c(pos, 'upgrade')
neg.words <- c(neg, 'hack', 'breach', 'cybersecurity')
Dataset <- list
Dataset$text <- as.factor(Dataset[,9])
scores <- score.sentiment(Dataset$text, pos.words, neg.words)
scores$score
Dataset$sentiment= scores$score
write.csv(Dataset, 'Dataset13.csv')


### Difference in means approach ###

diff_in_means = mean(subset(df, treat == 1)$total_contributions)-
 mean(subset(df, treat == 0)$total_contributions)
diff_in_means

### REGRESSION MODELS ###

### Baseline Model ###

model1 <- plm(total_contributions ~  sentiment_score + sector + industry + election_year +
              to_democrats + to_republicans + annual_revenue + company + year, data = df,
              model = 'pooling', index = c('company', "year"))

stargazer(model1, title = "Baseline Regression without treatment and control group", type = "text",
          omit = c('sector', 'industry', 'company', 'election_year'), omit.yes.no = c("Yes","No"))

#Difference in Difference 
model.did<- lm(total_contributions ~ treat+ After+ treat:After, data = df)
stargazer(model.did, title = "Difference in difference without PSM", type = "text",
          omit = c('sector', 'industry', 'company', 'election_year'), omit.yes.no = c("Yes","No"))

#Difference in Difference using fixed effects 
fe.model.did<- plm(total_contributions ~ treat+ After+ treat:After, data=df, index=c("company", "year"), model="within" )
stargazer(fe.model.did, title = "Difference in difference using Fixed effects", type = "text",
          omit = c('sector', 'industry', 'company', 'election_year'), omit.yes.no = c("Yes","No"))

### Propensity Score Matching ###
set.seed(1234)
#MATCH TREATED AND CONTROL COMPANIES ON LIKELIHOOD OF TREATMENT (receiving a positive public sentiment) ***#
newdata <- subset(df, After == 0) 

match <- matchit(treat~total_contributions, method = 'nearest', data = newdata, caliper=0.001)
summary(match)

new_df <- match.data(match)
dim(new_df)

model.match <- lm(total_contributions ~  treat+ After+ treat:After, data = new_df)
stargazer(model.match, title = "Difference in difference with PSM", type = "text",
          omit = c('sector', 'industry', 'company', 'election_year'), omit.yes.no = c("Yes","No"))

#Comparison 

stargazer(model.did, model.match, title = "Effect of Public Sentiment", type = "text",
          omit = c('sector', 'industry', 'company', 'election_year'), omit.yes.no = c("Yes","No"))
