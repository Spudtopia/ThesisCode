library(purrr)
library(psych)
library(zoo)
library(ltm)
library(lubridate)
library(widyr)
library(topicmodels)
library(openxlsx)
library(twitteR)
library(NLP)
library(tidyverse)


primary_mentions <- readRDS(DATA FILE WITH PRIMARY MENTIONS)

sub_mentions <- readRDS(DATA FILE WITH SECONDARY METNIONS)


AB_institutions_twitter <- tribble(
  ~'Institution', ~'Account', ~'Classification',
  'University of Calgary','@UCalgary', 'CARI',
  'University of Alberta','@UAlberta', 'CARI',
  'University of Lethbridge','@uLethbridge', 'CARI',
  'Athabasca University','@AthabascaU', 'CARI',
  'MacEwan University','@MacEwanU', 'BASI',
  'Mount Royal University','@mountroyal4u', 'BASI',
  'Northern Alberta Institute of Tech','@NAIT','Polytech',
  'Southern Alberta Institute of Tech','@sait','Polytech',
  'Bow Valley College','@BowValley','CCI',
  'Grande Prairie Regional College','@GPRC_AB','CCI',
  'Keyano College','@keyanocollege','CCI',
  'Lakeland College','@LakelandCollege','CCI',
  'Lethbridge College','@LethCollege','CCI',
  'Medicine Hat College','MHCollege','CCI',
  'Medicine Hat College','@MHCollege','CCI',
  'NorQuest College','@NorQuest','CCI',
  'Northern Lakes College','@starthere_nlc','CCI',
  'Olds College','@OldsCollege','CCI',
  'Portage College','@PortageCollege','CCI',
  'Red Deer College','@RedDeerCollege','CCI',
  'Alberta Academy of Art and Design','@acad','SACI',
  'Ambrose University','@ambroseUC','IAI',
  'Burman University','@BurmanUniv','IAI',
  'Concordia University of Edmonton','@CUEdmonton','IAI',
  'The Kings University','@TheKingsU','IAI',
  'St Marys University','@StMarysUC','IAI')

sub_accounts <- read_csv(LIST OF SUB ACCOUNTS)
colnames(sub_accounts) <- c('Institution','SubAccount')
sub_accounts <- sub_accounts %>%
  left_join(AB_institutions_twitter %>%
              select(Institution, Classification))

total_accounts <- bind_rows(AB_institutions_twitter %>%
                              mutate(account_type ="Primary Account"),
                            sub_accounts %>%
                              mutate(account_type = "Sub Account") %>%
                              rename(Account = SubAccount)) %>%
  mutate(term = str_sub(Account,2),
         term = tolower(term)) 

### Data cleaning and preparation ----

### attach the mentioned primary mentioned accounts

matchable_pattern <- paste(AB_institutions_twitter %>% pull(Account), collapse = "|")

system.time(
  output_frame <- map_df(primary_mentions$text, ~data.frame(matched_terms = paste(unique(unlist(str_match_all(.x, matchable_pattern))), collapse = ", ")))
  )

primary_mentions_plus <- bind_cols(primary_mentions, output_frame) %>%
  mutate(account_type = "Primary Account")

### attach the mentioned secondary mentioned accounts

matchable_pattern2 <- paste(sub_accounts %>% pull(SubAccount), collapse = "|")

system.time(
  output_frame2 <- map_df(sub_mentions$text, ~data.frame(matched_terms = paste(unique(unlist(str_match_all(.x, matchable_pattern2))), collapse = ", ")))
  )
  
sub_mentions_plus <- bind_cols(sub_mentions, output_frame2) %>%
  mutate(account_type = "Sub Account")

pooled_data <- bind_rows(primary_mentions_plus, sub_mentions_plus) %>%
  mutate(trunc_date = as.Date(created),
         CARI_mention = ifelse(grepl(paste(total_accounts[total_accounts$Classification == 'CARI',] %>% pull(Account), collapse = "|"), matched_terms), 1, 0),
         BASI_mention = ifelse(grepl(paste(total_accounts[total_accounts$Classification == 'BASI',] %>% pull(Account), collapse = "|"), matched_terms), 1, 0),
         Polytech_mention = ifelse(grepl(paste(total_accounts[total_accounts$Classification == 'Polytech',] %>% pull(Account), collapse = "|"), matched_terms), 1, 0),
         CCI_mention = ifelse(grepl(paste(total_accounts[total_accounts$Classification == 'CCI',] %>% pull(Account), collapse = "|"), matched_terms), 1, 0),
         SACI_mention = ifelse(grepl(paste(total_accounts[total_accounts$Classification == 'SACI',] %>% pull(Account), collapse = "|"), matched_terms), 1, 0),
         IAI_mention = ifelse(grepl(paste(total_accounts[total_accounts$Classification == 'IAI',] %>% pull(Account), collapse = "|"), matched_terms), 1, 0),
         Mixed_mention = ifelse(CARI_mention + BASI_mention + Polytech_mention + CCI_mention + SACI_mention + IAI_mention > 1, 1, 0))
           
pooled_data2 <- pooled_data %>%
  filter(matched_terms != '' |
           replyToSN %in% total_accounts$Account) %>%
  mutate(text = case_when(grepl("RT", text) ~ str_extract(text, ".(?=RT)"),
                          !grepl("RT", text) ~ text)) %>%
  filter(!is.na(text))


### Descriptive Statistics ----

#How many tweets, how many unique tweets

pooled_data2 %>% count()

pooled_data2 %>% count(account_type)

summary_breakdown <- pooled_data2 %>% select(id, account_type, CARI_mention, BASI_mention, Polytech_mention, CCI_mention, SACI_mention, IAI_mention, Mixed_mention) %>%
  gather(institution_type, counts, CARI_mention, BASI_mention, Polytech_mention, CCI_mention, SACI_mention, IAI_mention, Mixed_mention) %>%
  filter(counts != '0') %>%
  select(-counts) %>%
  filter(institution_type != 'Mixed_mention') %>%
  count(account_type, institution_type) %>%
  spread(account_type, n) %>%
  mutate(ratio = `Primary Account`/`Sub Account`)

pooled_data2 %>% count(text) %>% count()

pooled_data2 %>% filter(screenName %in% total_accounts$Account) %>% count()

pooled_data2 %>% count(isRetweet)

pooled_data2 %>% mutate(isReply = ifelse(!is.na(replyToSN), TRUE, FALSE)) %>% count(isReply)

pooled_data2 %>% count(favoriteCount, retweetCount)

#Time path of twitter mentions

figure1 <- ggplot(pooled_data2, aes(x = trunc_date)) +
  geom_bar() +
  scale_x_date(date_breaks = "14 days") +
  ggtitle("Tweets by day") +
  xlab("Date") +
  ylab("Daily Tweets")


# Actual analysis
library(tidytext)
tweeted_words <- pooled_data2 %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>%
  filter(!word %in% total_accounts$term) %>%
  filter(!word %in% c('rt','https','t.co')) %>%
  filter(!grepl("[[:digit:]]{1,}",word))

## Content Analysis ---
words_by_institution_type <- tweeted_words %>%
  select(word, CARI_mention, BASI_mention, Polytech_mention, CCI_mention, SACI_mention, IAI_mention, Mixed_mention) %>%
  gather(institution_type, counts, CARI_mention, BASI_mention, Polytech_mention, CCI_mention, SACI_mention, IAI_mention, Mixed_mention) %>%
  filter(counts != '0') %>%
  select(-counts) %>%
  count(word, institution_type) 

words_inst_type_agg <- words_by_institution_type %>%
  count(institution_type)

words_by_account_type <- tweeted_words %>%
  count(word, account_type) 

words_account_agg <- words_by_account_type%>%
  count(account_type)

unique_words <- bind_rows(words_inst_type_agg,
                          words_account_agg %>% rename(institution_type = account_type)) %>%
  mutate(institution_type = case_when(institution_type == 'BASI_mention' ~ 'BASI',
                                      institution_type == 'CARI_mention' ~ 'CARI',
                                      institution_type == 'CCI_mention' ~ 'CCI',
                                      institution_type == 'IAI_mention' ~ 'IAI',
                                      institution_type == 'Mixed_mention' ~ 'Multiple Sectors',
                                      institution_type == 'Polytech_mention' ~ 'Polytech',
                                      institution_type == 'SACI_mention' ~ 'SACI',
                                      institution_type %in% c('Primary Account','Sub Account') ~ institution_type),
         nn = round(nn, digits = 0)) %>%
  rename(Account = institution_type, 'Unique Words' = nn)

#### Comaprison against manual classification
#AFINN style strongly positive, storngly negative classification

afinn_testing <- sentiments %>%
  filter(lexicon == 'AFINN') %>%
  select(word, score) %>%
  filter(word %in% words_by_institution_type$word) %>%
  select(word) %>%
  sample_n(200)

#write.csv(afinn_testing, FILE PATH FOR THE TEST FILE)

manual_afinn_classification <- read.csv(THE TEST FILE NOW WITH MANUAL SCORING COMPLETED) %>%
  left_join(sentiments %>% filter(lexicon == 'AFINN'))

summary(alpha(manual_afinn_classification %>% select(score, test_score)))[[1]]

summary(alpha(manual_afinn_classification %>% select(score, redundancy_score)))[[1]]


#NRC emotion mapping

#### NRC mapping ----
nrc_by_classification <- inner_join(tweeted_words, sentiments) %>%
  filter(lexicon =='nrc') %>%
  select(sentiment, CARI_mention, BASI_mention, Polytech_mention, CCI_mention, SACI_mention, IAI_mention, Mixed_mention) %>%
  gather(institution_type, counts, CARI_mention, BASI_mention, Polytech_mention, CCI_mention, SACI_mention, IAI_mention, Mixed_mention) %>%
  filter(counts != '0' & sentiment != "negative" & sentiment != "positive") %>%
  select(-counts) %>%
  count(sentiment, institution_type) %>%
  group_by(institution_type) %>%
  mutate(total_sentiment = sum(n),
         sentiment_prop = n/total_sentiment) %>%
  ungroup() %>%
  select(-n, -total_sentiment) %>%
  spread(sentiment, sentiment_prop)

nrc_classification_ranks <- nrc_by_classification %>%
  gather(variables, values, -institution_type) %>%
  group_by(institution_type) %>%
  mutate(value_rank = rank(-values, ties.method = "min")) %>%
  select(-values) %>%
  spread(variables, value_rank)

nrc_by_account_type <- inner_join(tweeted_words, sentiments) %>%
  filter(lexicon =='nrc' & !sentiment %in% c('positive','negative')) %>%
  select(sentiment, account_type) %>%
  count(sentiment, account_type) %>%
  group_by(account_type) %>%
  mutate(total_sentiment = sum(n),
         sentiment_prop = n/total_sentiment) %>%
  ungroup() %>%
  select(-n, -total_sentiment) %>%
  spread(sentiment, sentiment_prop)

nrc_account_type_ranks <- nrc_by_account_type %>%
  gather(variables, values, -account_type) %>%
  group_by(account_type) %>%
  mutate(value_rank = rank(-values, ties.method = "min")) %>%
  select(-values) %>%
  spread(variables, value_rank)

#### AFINN mapping ----
afinn_by_classification <- inner_join(tweeted_words, sentiments) %>%
  filter(lexicon =='AFINN') %>%
  select(score, CARI_mention, BASI_mention, Polytech_mention, CCI_mention, SACI_mention, IAI_mention, Mixed_mention) %>%
  gather(institution_type, counts, CARI_mention, BASI_mention, Polytech_mention, CCI_mention, SACI_mention, IAI_mention, Mixed_mention) %>%
  filter(counts != '0') %>%
  select(-counts) %>%
  count(score, institution_type) %>%
  group_by(institution_type) %>%
  mutate(total_score = sum(n),
         score_prop = n/total_score) %>%
  ungroup() %>%
  select(-n, -total_score) %>%
  spread(score, score_prop)

afinn_by_classification_rank <- afinn_by_classification %>%
  gather(variables, values, -institution_type) %>%
  group_by(institution_type) %>%
  mutate(value_rank = rank(-values, ties.method = "min")) %>%
  select(-values) %>%
  spread(variables, value_rank) %>%
  select(institution_type, `-5`, `-4`, `-3`, `-2`, `-1`, `1`,`2`,`3`,`4`,`5`)

common_afinn_contributors <- inner_join(tweeted_words, sentiments) %>%
  filter(lexicon =='AFINN') %>%
  count(word, score) %>%
  ungroup() %>%
  arrange(desc(n), score) %>%
  select(word, score, n) %>%
  mutate(word_rank = row_number()) %>%
  filter(word_rank < 31) %>%
  rename(Twitter_word = word, Twitter_count = n, Twitter_score = score)

afinn_by_account_type <- inner_join(tweeted_words, sentiments) %>%
  filter(lexicon == 'AFINN') %>%
  select(score, account_type) %>%
  count(score, account_type) %>%
  spread(score, n)

afinn_ranked <- afinn_by_account_type %>%
  gather(variables, values, -account_type) %>%
  group_by(account_type) %>%
  mutate(value_rank = rank(-values, ties.method = "min")) %>%
  select(-values) %>%
  spread(variables, value_rank) %>%
  select(account_type, `-5`, `-4`, `-3`, `-2`, `-1`, `1`,`2`,`3`,`4`,`5`)

## Topic Modeling ----
### LDA
#### Institutional Type

by_institution_type_matrix <- words_by_institution_type %>%
  cast_dtm(institution_type, word, n)

lda_matrix_institution_type <- LDA(by_institution_type_matrix, k = 6, control=list(seed = 1992))

lda_institution_type <- tidy(lda_matrix_institution_type, matrix = "beta") %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) 

#### Comparing primary vs secondary accounts

by_account_type_matrix <- words_by_account_type %>%
  cast_dtm(account_type, word, n)

lda_matrix_account_type <- LDA(by_account_type_matrix, k = 2, control=list(seed = 1992))

lda_account_type <- tidy(lda_matrix_account_type, matrix = "beta") %>%
  group_by(topic) %>%
  top_n(20, beta) %>%
  ungroup() %>%
  arrange(topic, -beta) 

### Log-Odds Ratio
#### Institutional Type
logodds_institutional_type <- words_by_institution_type %>%
  filter(n > 25) %>%
  spread(institution_type, n) %>%
  replace(., is.na(.),0) %>%
  mutate(BASI_ratio = (BASI_mention + 1)/(sum(BASI_mention, na.rm = TRUE) + 1),
         CARI_ratio = (CARI_mention + 1)/(sum(CARI_mention, na.rm = TRUE) + 1),
         CCI_ratio = (CCI_mention +1)/(sum(CCI_mention, na.rm = TRUE) + 1),
         IAI_ratio = (IAI_mention +1)/(sum(IAI_mention, na.rm = TRUE) + 1),
         Mixed_ratio = (Mixed_mention +1)/(sum(Mixed_mention, na.rm = TRUE) + 1),
         Polytech_ratio = (Polytech_mention +1)/(sum(Polytech_mention, na.rm = TRUE) + 1),
         #SACI_ratio = (SACI_mention +1)/(sum(SACI_mention, na.rm = TRUE) + 1),
         CARI_BASI_Polytech_ratio = (BASI_mention + CARI_mention + Polytech_mention + 1)/(sum(BASI_mention, CARI_mention, Polytech_mention, na.rm = TRUE) + 1),
         #CCI_SAC_IAI_ratio = (CCI_mention + IAI_mention + SACI_mention + 1)/(sum(CCI_mention, IAI_mention, SACI_mention) + 1)
         CCI_IAI_ratio = (CCI_mention + IAI_mention + 1)/(sum(CCI_mention, IAI_mention) + 1)
  )

cari_vs_basi <- logodds_institutional_type %>%
  select(word, CARI_ratio, BASI_ratio) %>%
  mutate(log_ratio = log2(CARI_ratio/BASI_ratio)) %>%
  arrange(desc(log_ratio))

cari_vs_basi <- cari_vs_basi[c(1:30,323:352),]

cari_vs_cci <- logodds_institutional_type %>%
  select(word, CARI_ratio, CCI_ratio) %>%
  mutate(log_ratio = log2(CARI_ratio/CCI_ratio)) %>%
  arrange(desc(log_ratio))

cari_vs_cci <- cari_vs_cci[c(1:30,323:352),]

cci_vs_polytech <- logodds_institutional_type %>%
  select(word, CCI_ratio, Polytech_ratio) %>%
  mutate(log_ratio = log2(CCI_ratio/Polytech_ratio)) %>%
  arrange(desc(log_ratio))

cci_vs_polytech <- cci_vs_polytech[c(1:30, 323:352),]

big_vs_small <- logodds_institutional_type %>%
  select(word, CARI_BASI_Polytech_ratio, CCI_IAI_ratio) %>%
  mutate(log_ratio = log2(CARI_BASI_Polytech_ratio/CCI_IAI_ratio)) %>%
  arrange(desc(log_ratio))

big_vs_small <- big_vs_small[c(1:30, 323:352),]

#### Specificity towards Institution
logodds_account_type <- words_by_account_type %>%
  filter(n > 30) %>%
  spread(account_type, n) %>%
  replace(., is.na(.),0) %>%
  mutate(Primary_ratio = (`Primary Account` + 1)/(sum(`Primary Account`) + 1),
         Sub_ratio = (`Sub Account` + 1) / (sum(`Sub Account`) +1),
         log_ratio = log2(Primary_ratio/Sub_ratio)) %>%
  arrange(desc(log_ratio))

logodds_account_type <- logodds_account_type[c(1:30,375:404),]

## context of word usage ---

##### Pairwise correlation

filter_vector <- words_by_institution_type %>%
  dplyr::arrange(institution_type, desc(n)) %>%
  filter(n >= 50) %>%
  group_by(institution_type) %>%
  top_n(50) %>%
  ungroup() %>%
  pull(word) %>%
  unique()

pair_wise_cors_list <- pooled_data2 %>%
  mutate(tweet_number = row_number()) %>%
  unnest_tokens(word, text) %>%
  #filter(!word %in% stop_words$word) %>%
  filter(!word %in% total_accounts$term) %>%
  filter(!word %in% c('rt','https','t.co')) %>%
  filter(!grepl("[[:digit:]]{1,}",word)) %>%
  select(word, tweet_number, CARI_mention, BASI_mention, Polytech_mention, CCI_mention, SACI_mention, IAI_mention, Mixed_mention) %>%
  gather(institution_type, counts, CARI_mention, BASI_mention, Polytech_mention, CCI_mention, SACI_mention, IAI_mention, Mixed_mention) %>%
  filter(counts != '0') %>%
  select(-counts) %>%
  group_by(institution_type, word, tweet_number) %>%
  filter(word %in% filter_vector) %>%
  pairwise_cor(word, tweet_number, sort = TRUE)

word_context <- pair_wise_cors_list %>%
  filter(item1 %in% cari_vs_basi$word |
           item1 %in% cari_vs_cci$word |
           item1 %in% cci_vs_polytech$word |
           item1 %in% big_vs_small$word) %>%
  filter(correlation >= 0.25) %>%
  arrange(desc(correlation)) %>%
  group_by(correlation) %>%
  filter(item1 == min(item1)) %>%
  ungroup()

## Facebook vs Twitter ----

facebook_posts <- read.xlsx("C:/Users/bcong/OneDrive/Documents/Thesis Code/FacebookPosts.xlsx") %>%
  mutate(Original_Post_Date = parse_date_time(Original_Post_Date, "mdy"),
         Original_Post_Date = as.Date(Original_Post_Date))

facebook_posts %>% count()

facebook_posts %>% count(Institution_Type, Institution) %>% count(Institution_Type)

facebook_posts %>% count(Institution_Type, Original_Post) %>% count(Institution_Type)

table19 <- facebook_posts %>% count(Institution_Type)

#Check how often people get tagged.
#You messed up and deliberately excluded posts which were just tagging people's name
facebook_posts %>% mutate(tag_indicator = grepl("name", Response_Post)) %>% count(tag_indicator)

figure2 <- ggplot(facebook_posts, aes(x = Original_Post_Date)) +
  geom_bar() +
  scale_x_date(date_breaks = "14 days") +
  ggtitle("Posts by day - Facebook") +
  xlab("Date") +
  ylab("Daily Posts")

facebooked_words <- facebook_posts %>%
  unnest_tokens(word, Response_Post) %>%
  filter(!word %in% stop_words$word) %>%
  left_join(sentiments) 

facebook_sentiments_nrc <- facebooked_words %>%
  filter(lexicon == "nrc" & !sentiment %in% c("positive","negative")) %>%
  count(Institution_Type, sentiment) %>%
  group_by(Institution_Type) %>%
  mutate(total_sentiment = sum(n),
         sentiment_prop = n/total_sentiment) %>%
  ungroup() %>%
  select(-n, -total_sentiment) %>%
  spread(sentiment, sentiment_prop)

facebook_nrc_rank <- facebook_sentiments_nrc %>%
  gather(variables, values, -Institution_Type) %>%
  group_by(Institution_Type) %>%
  mutate(value_rank = rank(-values, ties.method = "min")) %>%
  select(-values) %>%
  spread(variables, value_rank)

facebook_sentiments_afinn <- facebooked_words %>%
  filter(lexicon == "AFINN") %>%
  count(Institution_Type, score) %>%
  group_by(Institution_Type) %>%
  mutate(total_score = sum(n),
         score_prop = n/total_score) %>%
  ungroup() %>%
  select(-n, -total_score) %>%
  spread(score, score_prop)

facebook_afinn_rank <- facebook_sentiments_afinn %>%
  gather(variables, values, -Institution_Type) %>%
  group_by(Institution_Type) %>%
  mutate(value_rank = rank(-values, ties.method = "min")) %>%
  select(-values) %>%
  spread(variables, value_rank) %>%
  select(Institution_Type, `-4`, `-3`, `-2`, `-1`, `1`,`2`,`3`,`4`,`5`)

common_facebook_contributors <- facebooked_words %>%
  filter(lexicon =='AFINN') %>%
  count(word, score) %>%
  ungroup() %>%
  arrange(desc(n), score) %>%
  select(word, score, n) %>%
  mutate(word_rank = row_number()) %>%
  filter(word_rank < 31) %>%
  rename(Facebook_word = word, Facebook_count = n, Facebook_score = score)

afinn_intersection <- inner_join(common_afinn_contributors, common_facebook_contributors) %>%
  mutate(Commonality = case_when(Facebook_word %in% Twitter_word ~ "In Both Groups",
                                 !Facebook_word %in% Twitter_word ~ "Only One Group"))


common_graph_data <- bind_rows(pooled_data2 %>%
                                 mutate(platform = 'Twitter'),
                         facebook_posts %>%
                           rename(trunc_date = Original_Post_Date) %>%
                           mutate(platform = 'Facebook')) %>%
  count(platform, trunc_date) %>%
  spread(platform, n) %>%
  mutate(Facebook = ifelse(is.na(Facebook), 0, Facebook),
         Twitter = ifelse(is.na(Twitter), 0, Twitter))

figure3 <- ggplot(common_graph_data, aes(x = trunc_date)) +
  scale_x_date(date_breaks = "20 days") +
  geom_col(aes(y = Twitter, fill = 'Twitter')) + 
  geom_col(aes(y = Facebook*3, fill = 'Facebook')) +
  ggtitle("Activity by day") +
  xlab("Date") +
  ylab("Twitter Activity") +
  scale_y_continuous(sec.axis = sec_axis(~./3, name = "Facebook Activity")) +
  scale_fill_grey()

dumb_model <- lm(Twitter ~ Facebook, data = common_graph_data)
summary(dumb_model)

lda_model_facebook <- facebook_posts %>%
  unnest_tokens(word, Response_Post) %>%
  filter(!word %in% stop_words$word) %>%
  count(word, Institution_Type) %>%
  #Filtering more than 5 makes classifications disappear
  filter(n > 5) %>%
  spread(Institution_Type, n) %>%
  replace(., is.na(.),0) %>%
  mutate(BASI_ratio = (BASI + 1)/(sum(BASI, na.rm = TRUE) + 1),
         CARI_ratio = (CARI + 1)/(sum(CARI, na.rm = TRUE) + 1),
         CCI_ratio = (CCI +1)/(sum(CCI, na.rm = TRUE) + 1),
         IAI_ratio = (IAI +1)/(sum(IAI, na.rm = TRUE) + 1),
         Polytech_ratio = (Polytechnic +1)/(sum(Polytechnic, na.rm = TRUE) + 1),
         CARI_BASI_Polytech_ratio = (BASI + CARI + Polytechnic + 1)/(sum(BASI, CARI, Polytechnic, na.rm = TRUE) + 1),
         CCI_IAI_ratio = (CCI + IAI + 1)/(sum(CCI, IAI) + 1))

facebook_cari_cci <- lda_model_facebook %>%
  select(word, CARI_ratio, CCI_ratio) %>%
  mutate(log_ratio = log2(CARI_ratio/CCI_ratio)) %>%
  arrange(desc(log_ratio))

facebook_cari_cci <- facebook_cari_cci[c(1:30,63:92),]

facebook_big_vs_small <- lda_model_facebook %>%
  select(word, CARI_BASI_Polytech_ratio, CCI_IAI_ratio) %>%
  mutate(log_ratio = log2(CARI_BASI_Polytech_ratio/CCI_IAI_ratio)) %>%
  arrange(desc(log_ratio))

facebook_big_vs_small <- facebook_big_vs_small[c(1:30,63:92),]

# Check and see which posts are driving the most engagement.

fb_discussion_drivers <- facebook_posts %>%
  count(Institution_Type, Original_Post_Date, Original_Post) %>%
  filter(n >= 10) %>%
  unnest_tokens(word, Original_Post) %>%
  anti_join(stop_words) %>%
  group_by(Institution_Type, word) %>%
  summarise(nn = n(), posts = sum(n)) %>%
  filter(nn > 2) %>%
  arrange(Institution_Type, desc(posts))

fb_discussion_drivers %>% filter(word %in% c(big_vs_small$word))

figures_and_graphs <- list("Figure 1" = figure1, "Figure 2" = figure2, "Figure 3" = figure3,
                          "Table1" = summary_breakdown %>%
                            rename('Institution Type' = institution_type, Ratio = ratio) %>%
                            mutate(Ratio = round(Ratio, digits  = 2),
                                   Ratio = ifelse(is.na(Ratio), "-", Ratio),
                                   `Institution Type` = case_when(`Institution Type` == 'BASI_mention' ~ 'BASI',
                                                                 `Institution Type` == 'CARI_mention' ~ 'CARI',
                                                                 `Institution Type` == 'CCI_mention' ~ 'CCI',
                                                                 `Institution Type` == 'IAI_mention' ~ 'IAI',
                                                                 `Institution Type` == 'Polytech_mention' ~ 'Polytech',
                                                                 `Institution Type` == 'SACI_mention' ~ 'SACI')),
"Table 2" = unique_words,
"Table 3" = nrc_by_classification %>%
  mutate(institution_type = case_when(institution_type == 'BASI_mention' ~ 'BASI',
                                      institution_type == 'CARI_mention' ~ 'CARI',
                                      institution_type == 'CCI_mention' ~ 'CCI',
                                      institution_type == 'IAI_mention' ~ 'IAI',
                                      institution_type == 'Mixed_mention' ~ 'Multiple Sectors',
                                      institution_type == 'Polytech_mention' ~ 'Polytech',
                                      institution_type == 'SACI_mention' ~ 'SACI'),
         anger = round(anger, digits = 3),
         anticipation = round(anticipation, digits = 3),
         disgust = round(disgust, digits = 3),
         fear = round(fear, digits = 3),
         joy = round(joy, digits = 3),
         sadness = round(sadness, digits = 3),
         surprise = round(surprise, digits = 3),
         trust = round(trust, digits = 3)) %>%
  rename('Institution Type' = institution_type),
"Table 4" = nrc_classification_ranks %>%
  ungroup() %>%
  mutate(institution_type = case_when(institution_type == 'BASI_mention' ~ 'BASI',
                                      institution_type == 'CARI_mention' ~ 'CARI',
                                      institution_type == 'CCI_mention' ~ 'CCI',
                                      institution_type == 'IAI_mention' ~ 'IAI',
                                      institution_type == 'Mixed_mention' ~ 'Multiple Sectors',
                                      institution_type == 'Polytech_mention' ~ 'Polytech',
                                      institution_type == 'SACI_mention' ~ 'SACI')) %>%
  rename('Institution Type' = institution_type),
 "Table 5" = nrc_by_account_type %>%
    mutate(anger = round(anger, digits = 3),
         anticipation = round(anticipation, digits = 3),
         disgust = round(disgust, digits = 3),
         fear = round(fear, digits = 3),
         joy = round(joy, digits = 3),
         sadness = round(sadness, digits = 3),
         surprise = round(surprise, digits = 3),
         trust = round(trust, digits = 3)) %>%
    rename('Account Type' = account_type),
  "Table 6" = nrc_account_type_ranks %>%
    rename('Account Type' = account_type),
  "Table 7" = afinn_by_classification %>%
    mutate_at(2:11, funs(round(., 3))) %>%
    replace(is.na(.), "-") %>%
    mutate(institution_type = case_when(institution_type == 'BASI_mention' ~ 'BASI',
                                         institution_type == 'CARI_mention' ~ 'CARI',
                                         institution_type == 'CCI_mention' ~ 'CCI',
                                         institution_type == 'IAI_mention' ~ 'IAI',
                                         institution_type == 'Mixed_mention' ~ 'Multiple Sectors',
                                         institution_type == 'Polytech_mention' ~ 'Polytech',
                                         institution_type == 'SACI_mention' ~ 'SACI')) %>%
             rename('Institution Type' = institution_type),
   "Table 8" = afinn_by_classification_rank %>%
      ungroup() %>%
      mutate(institution_type = case_when(institution_type == 'BASI_mention' ~ 'BASI',
                                      institution_type == 'CARI_mention' ~ 'CARI',
                                      institution_type == 'CCI_mention' ~ 'CCI',
                                      institution_type == 'IAI_mention' ~ 'IAI',
                                      institution_type == 'Mixed_mention' ~ 'Multiple Sectors',
                                      institution_type == 'Polytech_mention' ~ 'Polytech',
                                      institution_type == 'SACI_mention' ~ 'SACI')) %>%
      rename('Institution Type' = institution_type),
    "Table 9" = common_afinn_contributors %>%
      select(word_rank, Twitter_word, Twitter_count, Twitter_score) %>%
      rename('Word Rank' = word_rank,
             Word = Twitter_word,
             Count = Twitter_count,
             Sentiment = Twitter_score),
    "Table 10" = afinn_ranked %>%
      rename('Account Type' = account_type),
    "Table 11" = lda_institution_type %>%
      rename('Estimated Topic' = topic, 
             Term = term,
             Beta = beta),
    "Table 12" = lda_account_type %>%
      rename('Estimated Topic' = topic,
             Term = term,
             Beta = beta),
    "Table 13" = cari_vs_basi %>%
      mutate(word = case_when(word == "mrucougars's" ~ "mrucougarss",
              word == "it's" ~ 'its',
              word != "mrucougars's" & word != "it's" ~ word)) %>%
      rename(Word = word,
             'CARI Ratio' = CARI_ratio,
             'BASI Ratio' = BASI_ratio,
             'Log-Odds Ratio' = log_ratio), 
    "Table 14" = cari_vs_cci %>%
      rename(Word = word,
              'CARI Ratio' = CARI_ratio,
              'CCI Ratio' = CCI_ratio,
              'Log-Odds Ratio' = log_ratio),
    "Table 15" = cci_vs_polytech %>%
      rename(Word = word,
          'CCI Ratio' = CCI_ratio,
          'Polytech Ratio' = Polytech_ratio,
          'Log-Odds Ratio' = log_ratio),
    "Table 16" = big_vs_small %>%
      rename(Word = word,
         'Large Schools Ratio' = CARI_BASI_Polytech_ratio,
         'Small Schools Ratio' = CCI_IAI_ratio,
         'Log-Odds Ratio' = log_ratio),
    "Table 17" = logodds_account_type %>%
      select(-`Primary Account`, -`Sub Account`) %>%
      rename(Word = word,
         'Primary Account Ratio' = Primary_ratio,
         'Secondary Account Ratio' = Sub_ratio,
         'Log-Odds Ratio' = log_ratio),
    "Table 18" = word_context %>%
      rename("Item 1" = item1,
             "Item 2" = item2,
             Correlation = correlation),
    "Table 19" = table19 %>%
      rename('Institution Type' = Institution_Type,
             Posts = n),
    "Table 20" = fb_discussion_drivers %>%
      mutate(word = case_when(word  == "wasn't" ~ "wasnt",
             word != "wasn't" ~ word)) %>%
      rename('Institution Type' = Institution_Type,
             Word = word,
             'Institution Posts' = nn,
             Replies = posts), 
    "Table 21" = facebook_sentiments_nrc %>%
         mutate(anger = round(anger, digits = 3),
         anticipation = round(anticipation, digits = 3),
         disgust = round(disgust, digits = 3),
         fear = round(fear, digits = 3),
         joy = round(joy, digits = 3),
         sadness = round(sadness, digits = 3),
         surprise = round(surprise, digits = 3),
         trust = round(trust, digits = 3)) %>%
         rename('Institution Type' = Institution_Type),
    "Table 22" = facebook_nrc_rank %>%
      ungroup() %>%
      rename('Institution Type' = Institution_Type),
    "Table 23" = facebook_sentiments_afinn %>%
      mutate_at(2:10, funs(round(., 3))) %>%
      replace(is.na(.), "-") %>%
      rename('Institution Type' = Institution_Type),
    "Table 24" = facebook_afinn_rank %>%
      rename('Institution Type' = Institution_Type),
    "Table 25" = facebook_cari_cci %>%
      rename(Word = word,
         'CARI Ratio' = CARI_ratio,
         'CCI Ratio' = CCI_ratio,
         'Log-Odds Ratio' = log_ratio),
    "Table 26" = facebook_big_vs_small %>%
      rename(Word = word,
         'Large Schools Ratio' = CARI_BASI_Polytech_ratio,
         'Small Schools Ratio' = CCI_IAI_ratio,
         'Log-Odds Ratio' = log_ratio), 
    "Table 27" = afinn_intersection %>%
      select(Twitter_word, word_rank, Facebook_word) %>%
      rename('Word Rank' = word_rank,
             'Twitter Word' = Twitter_word,
             'Facebook Word' = Facebook_word),
    "Table 28" = dumb_model)

#write_rds(figures_and_graphs, OUTPUT FILE WITH ALL THE TABLES AND CHARTS)
