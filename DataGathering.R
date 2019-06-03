library(twitteR)
library(purrr)
library(tidyverse)
library(NLP)

tconsumer_key <- CONSUMER KEY GOES HERE
tconsumer_secret <- CONSUMER SECRET GOES HERE
taccess_token <- ACCESS TOKEN GOES HERE
taccess_secret <- ACCESS SECRET GOES HERE

setup_twitter_oauth(tconsumer_key, tconsumer_secret, taccess_token, taccess_secret)


### Set up the data-frame which contains all of the institutions ----

AB_institutions_twitter <- tribble(
  ~'Institution', ~'Account',
  'University of Calgary','@UCalgary',
  'University of Alberta','@UAlberta',
  'University of Lethbridge','@uLethbridge',
  'Athabasca University','@AthabascaU',
  'MacEwan University','@MacEwanU',
  'Mount Royal University','@mountroyal4u',
  'Northern Alberta Institute of Tech','@NAIT',
  'Southern Alberta Institute of Tech','@sait',
  'Bow Valley College','@BowValley',
  'Grande Prairie Regional College','@GPRC_AB',
  'Keyano College','@keyanocollege',
  'Lakeland College','@LakelandCollege',
  'Lethbridge College','@LethCollege',
  'Medicine Hat College','@MHCollege',
  'NorQuest College','@NorQuest',
  'Northern Lakes College','@starthere_nlc',
  'Olds College','@OldsCollege',
  'Portage College','@PortageCollege',
  'Red Deer College','@RedDeerCollege',
  'Alberta Academy of Art and Design','@acad',
  'Ambrose University','@ambroseUC',
  'Burman University','@BurmanUniv',
  'Concordia University of Edmonton','@CUEdmonton',
  'The Kings University','@TheKingsU',
  'St Marys University','@StMarysUC')

sub_accounts <- read_csv(FILE WHICH INCLUDES SUB ACCOUNTS GOES HERE)
colnames(sub_accounts) <- c('Institution','SubAccount')
### Pulling the actual twitter mentions

#Main Data

system.time(primary_mentions <- map(AB_institutions_twitter$Account, ~searchTwitter(.x, n = 3000)))

primary_mentions_df <- tbl_df(map_df(unlist(primary_mentions), as.data.frame))

main_data <- readRDS('C:/Users/bcong/OneDrive/Documents/Thesis Code/PrimaryMentions.rds')

primary_mentions_df <- full_join(main_data, primary_mentions_df)

saveRDS(primary_mentions_df, 'C:/Users/bcong/OneDrive/Documents/Thesis Code/PrimaryMentions.rds')


#Secondary Accounts

system.time(sub_mentions <- map(sub_accounts$SubAccount, ~searchTwitter(.x, n = 3000)))

sub_mentions_df <- tbl_df(map_df(unlist(sub_mentions), as.data.frame))

main_subdata <- readRDS('C:/Users/bcong/OneDrive/Documents/Thesis Code/SecondaryMentions.rds')

main_subdata %>% count()

sub_mentions_df <- full_join(main_subdata, sub_mentions_df)

saveRDS(sub_mentions_df, 'C:/Users/bcong/OneDrive/Documents/Thesis Code/SecondaryMentions.rds')
