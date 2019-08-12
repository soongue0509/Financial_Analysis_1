setwd("C:/Users/Admin/Desktop/lpoint/foronerun")

# Read libraries ----------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------------------------
library(data.table)
library(tidyverse)
library(magrittr)
library(lubridate)
library(stringr)
library(tidytext)
library(tm)
library(zoo)
setwd('C:/Users/soong/Desktop/L-Point')
##########################################
####### 데이터 전처리 및 웹 크롤링########
##########################################

# Read data ---------------------------------------------------------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------------------------------------------------

product <- fread("product.csv", encoding="UTF-8") # Client ID, Session ID, HITS_SEQ, PD_Code, PD_additional_info, PD_brand, PD_price, PD_amount
search1 <- fread("search1.csv", encoding="UTF-8") # Client ID, Session ID, search word, search count per session
search2 <- fread("search2.csv", encoding="UTF-8") # Date, search word, search count
custom <- fread("custom.csv") # Client ID, Gender and Age
session <- fread("session.csv", encoding="UTF-8") # Client ID, Session ID, Session SEQ, Session Date, Total Viewed Pages, Total time spent, Device, Area
master <- fread("master.csv", encoding = "UTF-8") # About products

# Change classes
product %<>% 
  mutate(PD_BUY_AM = gsub("[[:punct:]]", "", PD_BUY_AM), PD_BUY_CT = gsub("[[:punct:]]", "", PD_BUY_CT)) %>% 
  mutate(PD_BUY_AM = as.integer(PD_BUY_AM), PD_BUY_CT = as.integer(PD_BUY_CT))

search2 %<>% 
  mutate(SEARCH_CNT = gsub("[[:punct:]]", "", SEARCH_CNT)) %>% 
  mutate(SEARCH_CNT = as.integer(SEARCH_CNT))

session %<>% 
  mutate(TOT_SESS_HR_V = gsub("[[:punct:]]", "", TOT_SESS_HR_V)) %>% 
  mutate(TOT_SESS_HR_V = as.integer(TOT_SESS_HR_V))

# Cosmetic ------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

product_session <- left_join(product, session, by=c("CLNT_ID", "SESS_ID"))

cosmetic <- left_join(product_session, master, by="PD_C") %>% 
  filter(CLAC1_NM=="화장품/뷰티케어") %>% 
  group_by(SESS_DT, CLAC3_NM) %>% 
  summarize(count=sum(PD_BUY_CT))

cosmetic_spread <- cosmetic %>% 
  spread(CLAC3_NM, count)

#write.csv(cosmetic_spread, "cosmetic_spread.csv", row.names=F)


# Search2 -------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------------------------------------
beauty_list<-list(BB_파운데이션_컴팩트류 = c('비비','파운데이션','컴팩트'),
                  립스틱_립라이너 = c('립스틱','립라이너'),
                  마스카라  = c('마스카라'),
                  메이크업브러쉬=c('메이크업브러쉬'),
                  블러셔_쉐이딩_하이라이터 = c('블러쉬', '블러셔','쉐이딩','하이라이터'),
                  아이라이너 = '아이라이너')

beauty_list_eng<-list(BB_파운데이션_컴팩트류 = c('BB','foundation','compact'),
                      립스틱_립라이너 = c('lipstick','lipliner'),
                      마스카라  = c('mascara'),
                      메이크업브러쉬=c('brush'),
                      블러셔_쉐이딩_하이라이터 = c('blush','shading','highlight','shade'),
                      아이라이너 = 'eyeliner')

M<-nrow(cosmetic_spread)
N<-length(names(beauty_list))
search_string <- matrix(0, M, N+1 )  %>% as.data.frame()
colnames(search_string) <- c( 'Date',names(beauty_list) )
search_string$Date <- cosmetic_spread$SESS_DT
search2$SEARCH_CNT<-as.numeric(search2$SEARCH_CNT)

for ( j in 1:N){
  k<-length(beauty_list[[j]])
  
  for ( i in 1:M ){
    
    search_selected <- search2 %>% filter(SESS_DT == search_string$Date[i])      
    count_kor<-NULL
    count_eng<-NULL
    
    for (kk in 1:k){
      iidx<-which( str_detect( search_selected$KWD_NM ,  beauty_list[[j]][kk]  ) ==T)
      count_kor<-sum(search_selected$SEARCH_CNT[iidx])
      
      iidx2<-which(str_detect( search_selected$KWD_NM ,  beauty_list_eng[[j]][kk]  ) ==T)
      count_eng<-sum(search_selected$SEARCH_CNT[iidx2])
      
    }
    
    count_max <- sum(count_eng, count_kor)
    search_string[i,j+1] <- count_max
  }
  
}

colnames(search_string) <- c("Date", "search_bbcream", "search_lipstick", "search_mascara", "search_brush", "search_blusher", "search_eyeliner")

# Climate -------------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

climate_rain <- fread("climate_rain.csv")
finedust <- fread("finedust.csv")

colnames(climate_rain) <- c("Location", "Date", "TempAverage", "TempLow", "TempHigh", "Rain")
colnames(finedust) <- c("Location1", "Location2", "Date", "Finedust")

climate_rain %<>% select(-Location) %>%   
  separate(Date, c("Year", "Month", "Day")) %>% 
  arrange(Year, Month, Day) %>% 
  mutate(Month=str_pad(Month, 2, "0", side="left"),
         Day=str_pad(Day, 2, "0", side="left")) %>% 
  unite(Date, Year, Month, Day, sep="")

finedust %<>% select(-Location1, -Location2) %>% 
  separate(Date, c("Year", "Month", "Day")) %>% 
  arrange(Year, Month, Day) %>% 
  mutate(Month=str_pad(Month, 2, "0", side="left"),
         Day=str_pad(Day, 2, "0", side="left")) %>% 
  unite(Date, Year, Month, Day, sep="")

weather <- left_join(climate_rain, finedust, by="Date")
weather$Date <- as.integer(weather$Date)

#write.csv(weather, "weather.csv", row.names=F)

# Day of Week ---------------------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

dayofweek <- cbind(Date = cosmetic_spread[,1], 
                   dayofweek = rep_len(c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"), length.out = 183)
)

colnames(dayofweek)[1] <- "Date"
dayofweek$dayofweek <- as.factor(dayofweek$dayofweek)

# Reviews on Online Mall ----------------------------------------------------------------------------------------------------------------------------------
# ---------------------------------------------------------------------------------------------------------------------------------------------------------

# BBcream/Foundation/Compact Review -----------------------------------------------------------------------------------------------------------------------

bbcream_eng <- fread("bbcream_review_eng.csv")

colnames(bbcream_eng) <- c("Date", "text")

bbcream_eng %<>% 
  separate(Date, c("Year", "Month", "Day"), sep="\\.") %>% 
  arrange(Year, Month, Day) %>% 
  mutate(Year=as.integer(Year), Month=as.integer(Month), Day=as.integer(Day)) %>% 
  filter(Year >= 2018 & Month >= 4 & Month <=9) %>% 
  mutate(Month=str_pad(Month, 2, "0", side="left"),
         Day=str_pad(Day, 2, "0", side="left")) %>% 
  unite(Date, Year, Month, Day, sep="") %>% 
  mutate(Date=as.integer(Date)) %>% 
  group_by(Date) %>% 
  summarize(text = toString(text))

# Corpusing

M.bbcream_eng <- Corpus(VectorSource(bbcream_eng$text))
n_bbcream_eng <- length(bbcream_eng$text)

# Parsing
## Remove special characters
for (j in 1:n_bbcream_eng) M.bbcream_eng[[j]] <- gsub("[[:punct:]]", " ", M.bbcream_eng[[j]]) 

## Lowercase
M.bbcream_eng <- tm_map(M.bbcream_eng, content_transformer(tolower))

## Remove numbers
M.bbcream_eng <- tm_map(M.bbcream_eng, removeNumbers)

## Remove blank spaces
M.bbcream_eng <- tm_map(M.bbcream_eng, stripWhitespace)

oplex <- data.frame(get_sentiments('bing'))

## Set as dataframe
sen_txt <- c(rep(NA), 161)
for(i in 1:161){
  sen_txt[i] <- as.character(M.bbcream_eng[[i]][1])
}

sen_bbcream_eng_txt <- data_frame(id=1:161, doc=sen_txt)

sen_bbcream_eng_txt_word <- sen_bbcream_eng_txt %>%
  unnest_tokens(word,doc)

## Merge data and dictionary
result.sa <- sen_bbcream_eng_txt_word %>% inner_join(get_sentiments('bing'))
result.sa <- result.sa %>% count(word,id,sentiment)
result.sa <- result.sa %>% spread(sentiment, n, fil=0)

bbcreamOplex <- summarise(group_by(result.sa, id),
                          pos.sum = sum(positive),
                          neg.sum = sum(negative),
                          pos.sent = pos.sum-neg.sum)

fordate <- cosmetic_spread
colnames(fordate)[1] <- "Date"

datedf <- cbind(fordate[,1], id=1:183)

bbcream_sentiment <- left_join(datedf, bbcreamOplex, by="id") %>% select(Date, bbcream_sentiment=pos.sent)

# write.csv(bbcream_sentiment,"bbcream_sentiment.csv", row.names=F)

# Lipstick Review ----------------------------------------------------------------------------------------------------------------------------------------

lipstick_eng <- fread("lipstick_review_eng.csv")

colnames(lipstick_eng) <- c("Date", "text")

lipstick_eng %<>% 
  separate(Date, c("Year", "Month", "Day"), sep="\\.") %>% 
  arrange(Year, Month, Day) %>% 
  mutate(Year=as.integer(Year), Month=as.integer(Month), Day=as.integer(Day)) %>% 
  filter(Year >= 2018 & Month >= 4 & Month <=9) %>% 
  mutate(Month=str_pad(Month, 2, "0", side="left"),
         Day=str_pad(Day, 2, "0", side="left")) %>% 
  unite(Date, Year, Month, Day, sep="") %>% 
  mutate(Date=as.integer(Date)) %>% 
  group_by(Date) %>% 
  summarize(text = toString(text))

# Corpusing
M.lipstick_eng <- Corpus(VectorSource(lipstick_eng$text))
n_lipstick_eng <- length(lipstick_eng$text)

# Parsing 

## Remove special characters
for (j in 1:n_lipstick_eng) M.lipstick_eng[[j]] <- gsub("[[:punct:]]", " ", M.lipstick_eng[[j]]) 

## Lowercase
M.lipstick_eng <- tm_map(M.lipstick_eng, content_transformer(tolower))

## Remove numbers
M.lipstick_eng <- tm_map(M.lipstick_eng, removeNumbers)

## Remove blank spaces
M.lipstick_eng <- tm_map(M.lipstick_eng, stripWhitespace)

oplex <- data.frame(get_sentiments('bing'))

## Set as dataframe
sen_txt <- c(rep(NA), 167)
for(i in 1:167){
  sen_txt[i] <- as.character(M.lipstick_eng[[i]][1])
}

sen_lipstick_eng_txt <- data_frame(id=1:167, doc=sen_txt)

sen_lipstick_eng_txt_word <- sen_lipstick_eng_txt %>%
  unnest_tokens(word,doc)

## Merge data and dictionary
result.sa <- sen_lipstick_eng_txt_word %>% inner_join(get_sentiments('bing'))
result.sa <- result.sa %>% count(word,id,sentiment)
result.sa <- result.sa %>% spread(sentiment, n, fil=0)

lipstickOplex <- summarise(group_by(result.sa, id),
                           pos.sum = sum(positive),
                           neg.sum = sum(negative),
                           pos.sent = pos.sum-neg.sum)

fordate <- cosmetic_spread
colnames(fordate)[1] <- "Date"

datedf <- cbind(fordate[,1], id=1:183)

lipstick_sentiment <- left_join(datedf, lipstickOplex, by="id") %>% select(Date, lipstick_sentiment=pos.sent)

# write.csv(lipstick_sentiment,"lipstick_sentiment.csv", row.names=F)

# Blusher Review ------------------------------------------------------------------------------------------------------------------------------------------

blusher_eng <- fread("blusher_review_eng.csv")

colnames(blusher_eng) <- c("Date", "text")

blusher_eng %<>% 
  separate(Date, c("Year", "Month", "Day"), sep="\\.") %>% 
  arrange(Year, Month, Day) %>% 
  mutate(Year=as.integer(Year), Month=as.integer(Month), Day=as.integer(Day)) %>% 
  filter(Year >= 2018 & Month >= 4 & Month <=9) %>% 
  mutate(Month=str_pad(Month, 2, "0", side="left"),
         Day=str_pad(Day, 2, "0", side="left")) %>% 
  unite(Date, Year, Month, Day, sep="") %>% 
  mutate(Date=as.integer(Date)) %>% 
  group_by(Date) %>% 
  summarize(text = toString(text))

# Corpusing
M.blusher_eng <- Corpus(VectorSource(blusher_eng$text))
n_blusher_eng <- length(blusher_eng$text)

# Parsing 

## Remove special characters
for (j in 1:n_blusher_eng) M.blusher_eng[[j]] <- gsub("[[:punct:]]", " ", M.blusher_eng[[j]]) 

## Lowercase
M.blusher_eng <- tm_map(M.blusher_eng, content_transformer(tolower))

## Remove numbers
M.blusher_eng <- tm_map(M.blusher_eng, removeNumbers)

## Remove blank spaces
M.blusher_eng <- tm_map(M.blusher_eng, stripWhitespace)

oplex <- data.frame(get_sentiments('bing'))

## Set as dataframe
sen_txt <- c(rep(NA), 75)
for(i in 1:75){
  sen_txt[i] <- as.character(M.blusher_eng[[i]][1])
}

sen_blusher_eng_txt <- data_frame(id=1:75, doc=sen_txt)

sen_blusher_eng_txt_word <- sen_blusher_eng_txt %>%
  unnest_tokens(word,doc)

## Merge data and dictionary
result.sa <- sen_blusher_eng_txt_word %>% inner_join(get_sentiments('bing'))
result.sa <- result.sa %>% count(word,id,sentiment)
result.sa <- result.sa %>% spread(sentiment, n, fil=0)

blusherOplex <- summarise(group_by(result.sa, id),
                          pos.sum = sum(positive),
                          neg.sum = sum(negative),
                          pos.sent = pos.sum-neg.sum)

fordate <- cosmetic_spread
colnames(fordate)[1] <- "Date"

datedf <- cbind(fordate[,1], id=1:183)

blusher_sentiment <- left_join(datedf, blusherOplex, by="id") %>% select(Date, blusher_sentiment=pos.sent)

# write.csv(blusher_sentiment,"blusher_sentiment.csv", row.names=F)

# Youtube ------------------------------------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------------------------------------------

beauty_list<-list(BB_파운데이션_컴팩트류 = c('비비','파운데이션','컴팩트류'),
                  립글로즈_틴트 = c('틴트'),
                  립스틱_립라이너 = c('립'),
                  마스카라  = c('마스카라'),
                  메이크업베이스_프라이머 = c('메이크업베이스','프라이머'),
                  메이크업브러쉬=c('메이크업브러쉬'),
                  블러셔_쉐이딩_하이라이터 = c('블러쉬', '블러셔','쉐이딩','하이라이터'),
                  선크림류 = c('선크림류'),
                  스킨_토너 = c("스킨","토너"),
                  스폰지_퍼프 = c('스폰지','퍼프'),
                  아이라이너 = '아이라이너',
                  아이섀도우 = '아이섀도우',
                  에멀젼_로션 = c('에멀젼','로션'),
                  에센스_세럼 = c("에센스","세럼"))

beauty_list_eng<-list(BB_파운데이션_컴팩트류 = c('bb','foundation','compact'),
                      립글로즈_틴트 = c('tint'),
                      립스틱_립라이너 = c('lip'),
                      마스카라  = c('mascara'),
                      메이크업베이스_프라이머 = c('base','primer'),
                      메이크업브러쉬=c('brush'),
                      블러셔_쉐이딩_하이라이터 = c('blush','shading','highlight','shade'),
                      선크림류 = c('sunscreen'),
                      스킨_토너 = c("toner",'allinone'),
                      스폰지_퍼프 = c('sponge','puff'),
                      아이라이너 = 'eyeliner',
                      아이섀도우 = 'shadow',
                      에멀젼_로션 = c('emulsion','lotion'),
                      에센스_세럼 = c("essence","serum"))

# strdetect items reviewed -------------------------------------------------------------------------------------------------------------------------------

ssinnim <- fread('ssinnim.csv') # ssinnim 
ssinnim <- ssinnim %>% as_tibble()
ssinnim$Description <- tolower(ssinnim$Description)

N<-length(names(beauty_list))
ssinnim_string <- matrix(0, nrow(ssinnim) , N+1) %>% as.data.frame()
colnames(ssinnim_string) <- c('Date',names(beauty_list) )
ssinnim_string$Date <- ssinnim$`Published Date`

for ( j in 1:N){
  k<-length(beauty_list[[j]])
  
  for ( i in 1:nrow(ssinnim) ){
    
    count_kor<-NULL
    count_eng<-NULL
    
    for (kk in 1:k){
      count_kor<-c(count_kor, 
                   str_extract_all(ssinnim$Description[i],  beauty_list[[j]][kk]  ) %>% 
                     unlist() %>% length())
      count_eng<-c(count_eng,
                   str_extract_all(ssinnim$Description[i],  beauty_list_eng[[j]][kk]  ) %>% 
                     unlist() %>% length()) 
      
      
    }
    count_max <- max(count_eng, count_kor)
    ssinnim_string[i,j+1]<-sum(count_max)
  }
}

sunny <- fread('sunny.csv') # sunny 
sunny <- sunny %>% as_tibble()
sunny$Description <- tolower(sunny$Description)

N<-length(names(beauty_list))
sunny_string <- matrix(0, nrow(sunny) , N+1) %>% as.data.frame()
colnames(sunny_string) <- c('Date',names(beauty_list) )
sunny_string$Date <- sunny$`Published Date`

for ( j in 1:N){
  k<-length(beauty_list[[j]])
  
  for ( i in 1:nrow(sunny) ){
    
    count_kor<-NULL
    count_eng<-NULL
    
    for (kk in 1:k){
      count_kor<-c(count_kor, 
                   str_extract_all(sunny$Description[i],  beauty_list[[j]][kk]  ) %>% 
                     unlist() %>% length())
      count_eng<-c(count_eng,
                   str_extract_all(sunny$Description[i],  beauty_list_eng[[j]][kk]  ) %>% 
                     unlist() %>% length()) 
      
      
    }
    count_max <- max(count_eng, count_kor)
    sunny_string[i,j+1]<-sum(count_max)
  }
}

lamuqe <- fread('lamuqe.csv') # lamuqe 
lamuqe <- lamuqe %>% as_tibble()
lamuqe$Description <- tolower(lamuqe$Description)

N<-length(names(beauty_list))
lamuqe_string <- matrix(0, nrow(lamuqe) , N+1) %>% as.data.frame()
colnames(lamuqe_string) <- c('Date',names(beauty_list) )
lamuqe_string$Date <- lamuqe$`Published Date`

for ( j in 1:N){
  k<-length(beauty_list[[j]])
  
  for ( i in 1:nrow(lamuqe) ){
    
    count_kor<-NULL
    count_eng<-NULL
    
    for (kk in 1:k){
      count_kor<-c(count_kor, 
                   str_extract_all(lamuqe$Description[i],  beauty_list[[j]][kk]  ) %>% 
                     unlist() %>% length())
      count_eng<-c(count_eng,
                   str_extract_all(lamuqe$Description[i],  beauty_list_eng[[j]][kk]  ) %>% 
                     unlist() %>% length()) 
      
      
    }
    count_max <- max(count_eng, count_kor)
    lamuqe_string[i,j+1]<-sum(count_max)
  }
}

pony <- fread('pony.csv') # pony 
pony <- pony %>% as_tibble()
pony$Description <- tolower(pony$Description)

N<-length(names(beauty_list))
pony_string <- matrix(0, nrow(pony) , N+1) %>% as.data.frame()
colnames(pony_string) <- c('Date',names(beauty_list) )
pony_string$Date <- pony$`Published Date`

for ( j in 1:N){
  k<-length(beauty_list[[j]])
  
  for ( i in 1:nrow(pony) ){
    
    count_kor<-NULL
    count_eng<-NULL
    
    for (kk in 1:k){
      count_kor<-c(count_kor, 
                   str_extract_all(pony$Description[i],  beauty_list[[j]][kk]  ) %>% 
                     unlist() %>% length())
      count_eng<-c(count_eng,
                   str_extract_all(pony$Description[i],  beauty_list_eng[[j]][kk]  ) %>% 
                     unlist() %>% length()) 
      
      
    }
    count_max <- max(count_eng, count_kor)
    pony_string[i,j+1]<-sum(count_max)
  }
}

yeondukong <- fread('yeondukong.csv') # yeondukong 
yeondukong <- yeondukong %>% as_tibble()
yeondukong$Description <- tolower(yeondukong$Description)

N<-length(names(beauty_list))
yeondukong_string <- matrix(0, nrow(yeondukong) , N+1) %>% as.data.frame()
colnames(yeondukong_string) <- c('Date',names(beauty_list) )
yeondukong_string$Date <- yeondukong$`Published Date`

for ( j in 1:N){
  k<-length(beauty_list[[j]])
  
  for ( i in 1:nrow(yeondukong) ){
    
    count_kor<-NULL
    count_eng<-NULL
    
    for (kk in 1:k){
      count_kor<-c(count_kor, 
                   str_extract_all(yeondukong$Description[i],  beauty_list[[j]][kk]  ) %>% 
                     unlist() %>% length())
      count_eng<-c(count_eng,
                   str_extract_all(yeondukong$Description[i],  beauty_list_eng[[j]][kk]  ) %>% 
                     unlist() %>% length()) 
      
      
    }
    count_max <- max(count_eng, count_kor)
    yeondukong_string[i,j+1]<-sum(count_max)
  }
}

risabae <- fread('risabae.csv') # risabae 
risabae <- risabae %>% as_tibble()
risabae$Description <- tolower(risabae$Description)

N<-length(names(beauty_list))
risabae_string <- matrix(0, nrow(risabae) , N+1) %>% as.data.frame()
colnames(risabae_string) <- c('Date',names(beauty_list) )
risabae_string$Date <- risabae$`Published Date`

for ( j in 1:N){
  k<-length(beauty_list[[j]])
  
  for ( i in 1:nrow(risabae) ){
    
    count_kor<-NULL
    count_eng<-NULL
    
    for (kk in 1:k){
      count_kor<-c(count_kor, 
                   str_extract_all(risabae$Description[i],  beauty_list[[j]][kk]  ) %>% 
                     unlist() %>% length())
      count_eng<-c(count_eng,
                   str_extract_all(risabae$Description[i],  beauty_list_eng[[j]][kk]  ) %>% 
                     unlist() %>% length()) 
      
      
    }
    count_max <- max(count_eng, count_kor)
    risabae_string[i,j+1]<-sum(count_max)
  }
}

hanbyul <- fread('hanbyul.csv') # hanbyul 
hanbyul <- hanbyul %>% as_tibble()
hanbyul$Description <- tolower(hanbyul$Description)

N<-length(names(beauty_list))
hanbyul_string <- matrix(0, nrow(hanbyul) , N+1) %>% as.data.frame()
colnames(hanbyul_string) <- c('Date',names(beauty_list) )
hanbyul_string$Date <- hanbyul$`Published Date`

for ( j in 1:N){
  k<-length(beauty_list[[j]])
  
  for ( i in 1:nrow(hanbyul) ){
    
    count_kor<-NULL
    count_eng<-NULL
    
    for (kk in 1:k){
      count_kor<-c(count_kor, 
                   str_extract_all(hanbyul$Description[i],  beauty_list[[j]][kk]  ) %>% 
                     unlist() %>% length())
      count_eng<-c(count_eng,
                   str_extract_all(hanbyul$Description[i],  beauty_list_eng[[j]][kk]  ) %>% 
                     unlist() %>% length()) 
      
      
    }
    count_max <- max(count_eng, count_kor)
    hanbyul_string[i,j+1]<-sum(count_max)
  }
}

yunjjami <- fread('yunjjami.csv') # yunjjami 
yunjjami <- yunjjami %>% as_tibble()
yunjjami$Description <- tolower(yunjjami$Description)

N<-length(names(beauty_list))
yunjjami_string <- matrix(0, nrow(yunjjami) , N+1) %>% as.data.frame()
colnames(yunjjami_string) <- c('Date',names(beauty_list) )
yunjjami_string$Date <- yunjjami$`Published Date`

for ( j in 1:N){
  k<-length(beauty_list[[j]])
  
  for ( i in 1:nrow(yunjjami) ){
    
    count_kor<-NULL
    count_eng<-NULL
    
    for (kk in 1:k){
      count_kor<-c(count_kor, 
                   str_extract_all(yunjjami$Description[i],  beauty_list[[j]][kk]  ) %>% 
                     unlist() %>% length())
      count_eng<-c(count_eng,
                   str_extract_all(yunjjami$Description[i],  beauty_list_eng[[j]][kk]  ) %>% 
                     unlist() %>% length()) 
      
      
    }
    count_max <- max(count_eng, count_kor)
    yunjjami_string[i,j+1]<-sum(count_max)
  }
}

daddoa <- fread('daddoa.csv') # daddoa 
daddoa <- daddoa %>% as_tibble()
daddoa$Description <- tolower(daddoa$Description)

N<-length(names(beauty_list))
daddoa_string <- matrix(0, nrow(daddoa) , N+1) %>% as.data.frame()
colnames(daddoa_string) <- c('Date',names(beauty_list) )
daddoa_string$Date <- daddoa$`Published Date`

for ( j in 1:N){
  k<-length(beauty_list[[j]])
  
  for ( i in 1:nrow(daddoa) ){
    
    count_kor<-NULL
    count_eng<-NULL
    
    for (kk in 1:k){
      count_kor<-c(count_kor, 
                   str_extract_all(daddoa$Description[i],  beauty_list[[j]][kk]  ) %>% 
                     unlist() %>% length())
      count_eng<-c(count_eng,
                   str_extract_all(daddoa$Description[i],  beauty_list_eng[[j]][kk]  ) %>% 
                     unlist() %>% length()) 
      
      
    }
    count_max <- max(count_eng, count_kor)
    daddoa_string[i,j+1]<-sum(count_max)
  }
}

# Leave only the dates needed in order

ssinnim_freq <- ssinnim_string %>% 
  separate(Date, c("Date", "Hour", "AMPM"), sep=" ") %>% 
  select(-Hour, -AMPM) %>% 
  separate(Date, c("Month", "Day", "Year")) %>% 
  arrange(Year, Month, Day) %>% 
  mutate(Year=as.integer(Year), Month=as.integer(Month), Day=as.integer(Day)) %>% 
  filter(Year >= 2018 & Month >= 3 & Month <=9) %>% 
  mutate(Month=str_pad(Month, 2, "0", side="left"),
         Day=str_pad(Day, 2, "0", side="left")) %>% 
  unite(Date, Year, Month, Day, sep="")

# write.csv(ssinnim_freq, "ssinnim_freq.csv", row.names=F)

sunny_freq <- sunny_string %>% 
  separate(Date, c("Date", "Hour", "AMPM"), sep=" ") %>% 
  select(-Hour, -AMPM) %>% 
  separate(Date, c("Month", "Day", "Year")) %>% 
  arrange(Year, Month, Day) %>% 
  mutate(Year=as.integer(Year), Month=as.integer(Month), Day=as.integer(Day)) %>% 
  filter(Year >= 2018 & Month >= 3 & Month <=9) %>% 
  mutate(Month=str_pad(Month, 2, "0", side="left"),
         Day=str_pad(Day, 2, "0", side="left")) %>% 
  unite(Date, Year, Month, Day, sep="")

# write.csv(sunny_freq, "sunny_freq.csv", row.names=F)

lamuqe_freq <- lamuqe_string %>% 
  separate(Date, c("Date", "Hour", "AMPM"), sep=" ") %>% 
  select(-Hour, -AMPM) %>% 
  separate(Date, c("Month", "Day", "Year")) %>% 
  arrange(Year, Month, Day) %>% 
  mutate(Year=as.integer(Year), Month=as.integer(Month), Day=as.integer(Day)) %>% 
  filter(Year >= 2018 & Month >= 3 & Month <=9) %>% 
  mutate(Month=str_pad(Month, 2, "0", side="left"),
         Day=str_pad(Day, 2, "0", side="left")) %>% 
  unite(Date, Year, Month, Day, sep="")

# write.csv(lamuqe_freq, "lamuqe_freq.csv", row.names=F)

pony_freq <- pony_string %>% 
  separate(Date, c("Date", "Hour", "AMPM"), sep=" ") %>% 
  select(-Hour, -AMPM) %>% 
  separate(Date, c("Month", "Day", "Year")) %>% 
  arrange(Year, Month, Day) %>% 
  mutate(Year=as.integer(Year), Month=as.integer(Month), Day=as.integer(Day)) %>% 
  filter(Year >= 2018 & Month >= 3 & Month <=9) %>% 
  mutate(Month=str_pad(Month, 2, "0", side="left"),
         Day=str_pad(Day, 2, "0", side="left")) %>% 
  unite(Date, Year, Month, Day, sep="")

# write.csv(pony_freq, "pony_freq.csv", row.names=F)

yeondukong_freq <- yeondukong_string %>% 
  separate(Date, c("Date", "Hour", "AMPM"), sep=" ") %>% 
  select(-Hour, -AMPM) %>% 
  separate(Date, c("Month", "Day", "Year")) %>% 
  arrange(Year, Month, Day) %>% 
  mutate(Year=as.integer(Year), Month=as.integer(Month), Day=as.integer(Day)) %>% 
  filter(Year >= 2018 & Month >= 3 & Month <=9) %>% 
  mutate(Month=str_pad(Month, 2, "0", side="left"),
         Day=str_pad(Day, 2, "0", side="left")) %>% 
  unite(Date, Year, Month, Day, sep="")

# write.csv(yeondukong_freq, "yeondukong_freq.csv", row.names=F)

risabae_freq <- risabae_string %>% 
  separate(Date, c("Date", "Hour", "AMPM"), sep=" ") %>% 
  select(-Hour, -AMPM) %>% 
  separate(Date, c("Month", "Day", "Year")) %>% 
  arrange(Year, Month, Day) %>% 
  mutate(Year=as.integer(Year), Month=as.integer(Month), Day=as.integer(Day)) %>% 
  filter(Year >= 2018 & Month >= 3 & Month <=9) %>% 
  mutate(Month=str_pad(Month, 2, "0", side="left"),
         Day=str_pad(Day, 2, "0", side="left")) %>% 
  unite(Date, Year, Month, Day, sep="")

# write.csv(risabae_freq, "risabae_freq.csv", row.names=F)

hanbyul_freq <- hanbyul_string %>% 
  separate(Date, c("Date", "Hour", "AMPM"), sep=" ") %>% 
  select(-Hour, -AMPM) %>% 
  separate(Date, c("Month", "Day", "Year")) %>% 
  arrange(Year, Month, Day) %>% 
  mutate(Year=as.integer(Year), Month=as.integer(Month), Day=as.integer(Day)) %>% 
  filter(Year >= 2018 & Month >= 3 & Month <=9) %>% 
  mutate(Month=str_pad(Month, 2, "0", side="left"),
         Day=str_pad(Day, 2, "0", side="left")) %>% 
  unite(Date, Year, Month, Day, sep="")

# write.csv(hanbyul_freq, "hanbyul_freq.csv", row.names=F)

yunjjami_freq <- yunjjami_string %>% 
  separate(Date, c("Date", "Hour", "AMPM"), sep=" ") %>% 
  select(-Hour, -AMPM) %>% 
  separate(Date, c("Month", "Day", "Year")) %>% 
  arrange(Year, Month, Day) %>% 
  mutate(Year=as.integer(Year), Month=as.integer(Month), Day=as.integer(Day)) %>% 
  filter(Year >= 2018 & Month >= 3 & Month <=9) %>% 
  mutate(Month=str_pad(Month, 2, "0", side="left"),
         Day=str_pad(Day, 2, "0", side="left")) %>% 
  unite(Date, Year, Month, Day, sep="")

# write.csv(yunjjami_freq, "yunjjami_freq.csv", row.names=F)

daddoa_freq <- daddoa_string %>% 
  separate(Date, c("Date", "Hour", "AMPM"), sep=" ") %>% 
  select(-Hour, -AMPM) %>% 
  separate(Date, c("Month", "Day", "Year")) %>% 
  arrange(Year, Month, Day) %>% 
  mutate(Year=as.integer(Year), Month=as.integer(Month), Day=as.integer(Day)) %>% 
  filter(Year >= 2018 & Month >= 3 & Month <=9) %>% 
  mutate(Month=str_pad(Month, 2, "0", side="left"),
         Day=str_pad(Day, 2, "0", side="left")) %>% 
  unite(Date, Year, Month, Day, sep="")

# write.csv(daddoa_freq, "daddoa_freq.csv", row.names=F)

# Cbind Youtubers by date ---------------------------------------------------------------------------------------------------------------------------------

for (i in 2:ncol(ssinnim_freq)) {
  colnames(ssinnim_freq)[i] <- paste("ssinnim_", colnames(ssinnim_freq)[i], sep="")
}

for (i in 2:ncol(sunny_freq)) {
  colnames(sunny_freq)[i] <- paste("sunny_", colnames(sunny_freq)[i], sep="")
}

for (i in 2:ncol(lamuqe_freq)) {
  colnames(lamuqe_freq)[i] <- paste("lamuqe_", colnames(lamuqe_freq)[i], sep="")
}

for (i in 2:ncol(pony_freq)) {
  colnames(pony_freq)[i] <- paste("pony_", colnames(pony_freq)[i], sep="")
}

for (i in 2:ncol(yeondukong_freq)) {
  colnames(yeondukong_freq)[i] <- paste("yeondukong_", colnames(yeondukong_freq)[i], sep="")
}

for (i in 2:ncol(risabae_freq)) {
  colnames(risabae_freq)[i] <- paste("risabae_", colnames(risabae_freq)[i], sep="")
}

for (i in 2:ncol(hanbyul_freq)) {
  colnames(hanbyul_freq)[i] <- paste("hanbyul_", colnames(hanbyul_freq)[i], sep="")
}

for (i in 2:ncol(yunjjami_freq)) {
  colnames(yunjjami_freq)[i] <- paste("yunjjami_", colnames(yunjjami_freq)[i], sep="")
}

for (i in 2:ncol(daddoa_freq)) {
  colnames(daddoa_freq)[i] <- paste("daddoa_", colnames(daddoa_freq)[i], sep="")
}

# BB/Foundation/Compact

beforeDate <- data.frame(Date = seq(20180319,20180331,by=1))

bbcream <- left_join(rbind(beforeDate, fordate[,1]), ssinnim_freq[,c(1,2)] %>% mutate(Date=as.numeric(Date)), by="Date") %>% 
  left_join(., sunny_freq[,c(1,2)] %>% mutate(Date=as.numeric(Date)), by="Date") %>% 
  left_join(., lamuqe_freq[,c(1,2)] %>% mutate(Date=as.numeric(Date)), by="Date") %>% 
  left_join(., pony_freq[,c(1,2)] %>% mutate(Date=as.numeric(Date)), by="Date") %>% 
  left_join(., yeondukong_freq[,c(1,2)] %>% mutate(Date=as.numeric(Date)), by="Date") %>% 
  left_join(., risabae_freq[,c(1,2)] %>% mutate(Date=as.numeric(Date)), by="Date") %>% 
  left_join(., hanbyul_freq[,c(1,2)] %>% mutate(Date=as.numeric(Date)), by="Date") %>% 
  left_join(., yunjjami_freq[,c(1,2)] %>% mutate(Date=as.numeric(Date)), by="Date") %>% 
  left_join(., daddoa_freq[,c(1,2)] %>% mutate(Date=as.numeric(Date)), by="Date") %>% 
  replace(., is.na(.), 0) %>%
  group_by(Date) %>% 
  summarize_all(funs(sum))

# write.csv(bbcream, "bbcream.csv", row.names=F)

# Lipstick

lipstick <- left_join(rbind(beforeDate, fordate[,1]), ssinnim_freq[,c(1,4)] %>% mutate(Date=as.numeric(Date)), by="Date") %>% 
  left_join(., sunny_freq[,c(1,4)] %>% mutate(Date=as.numeric(Date)), by="Date") %>% 
  left_join(., lamuqe_freq[,c(1,4)] %>% mutate(Date=as.numeric(Date)), by="Date") %>% 
  left_join(., pony_freq[,c(1,4)] %>% mutate(Date=as.numeric(Date)), by="Date") %>% 
  left_join(., yeondukong_freq[,c(1,4)] %>% mutate(Date=as.numeric(Date)), by="Date") %>% 
  left_join(., risabae_freq[,c(1,4)] %>% mutate(Date=as.numeric(Date)), by="Date") %>% 
  left_join(., hanbyul_freq[,c(1,4)] %>% mutate(Date=as.numeric(Date)), by="Date") %>% 
  left_join(., yunjjami_freq[,c(1,4)] %>% mutate(Date=as.numeric(Date)), by="Date") %>% 
  left_join(., daddoa_freq[,c(1,4)] %>% mutate(Date=as.numeric(Date)), by="Date") %>% 
  replace(., is.na(.), 0) %>%
  group_by(Date) %>% 
  summarize_all(funs(sum))

# write.csv(lipstick, "lipstick.csv", row.names=F)

# Blusher

blusher <- left_join(rbind(beforeDate, fordate[,1]), ssinnim_freq[,c(1,8)] %>% mutate(Date=as.numeric(Date)), by="Date") %>% 
  left_join(., sunny_freq[,c(1,8)] %>% mutate(Date=as.numeric(Date)), by="Date") %>% 
  left_join(., lamuqe_freq[,c(1,8)] %>% mutate(Date=as.numeric(Date)), by="Date") %>% 
  left_join(., pony_freq[,c(1,8)] %>% mutate(Date=as.numeric(Date)), by="Date") %>% 
  left_join(., yeondukong_freq[,c(1,8)] %>% mutate(Date=as.numeric(Date)), by="Date") %>% 
  left_join(., risabae_freq[,c(1,8)] %>% mutate(Date=as.numeric(Date)), by="Date") %>% 
  left_join(., hanbyul_freq[,c(1,8)] %>% mutate(Date=as.numeric(Date)), by="Date") %>% 
  left_join(., yunjjami_freq[,c(1,8)] %>% mutate(Date=as.numeric(Date)), by="Date") %>% 
  left_join(., daddoa_freq[,c(1,8)] %>% mutate(Date=as.numeric(Date)), by="Date") %>% 
  replace(., is.na(.), 0) %>%
  group_by(Date) %>% 
  summarize_all(funs(sum))

# write.csv(blusher, "blusher.csv", row.names=F)

# Rolling Reviews ---------------------------------------------------------------------------------------------------------------------------------------

bbcream_rolling <- bbcream

bbcream <- as.data.frame(bbcream)

for (j in 2:10){
  for ( i in 14:183){
    bbcream_rolling[i,j]<-bbcream[i,j]+
      bbcream[i-1,j]+bbcream[i-2,j]+bbcream[i-3,j]+
      bbcream[i-4,j]+bbcream[i-5,j]+bbcream[i-6,j]+
      bbcream[i-7,j]+bbcream[i-8,j]+bbcream[i-9,j]+
      bbcream[i-10,j]+bbcream[i-11,j]+bbcream[i-12,j]+
      bbcream[i-13,j]
  }
}

bbcream_overview <- bbcream_rolling %>% mutate(bbcream_overview = rowSums(.[2:10])) %>% select(Date, bbcream_overview)

lipstick_rolling<-lipstick

lipstick <- as.data.frame(lipstick)

for (j in 2:10){
  for ( i in 14:183){
    lipstick_rolling[i,j]<-lipstick[i,j]+
      lipstick[i-1,j]+lipstick[i-2,j]+lipstick[i-3,j]+
      lipstick[i-4,j]+lipstick[i-5,j]+lipstick[i-6,j]+
      lipstick[i-7,j]+lipstick[i-8,j]+lipstick[i-9,j]+
      lipstick[i-10,j]+lipstick[i-11,j]+lipstick[i-12,j]+
      lipstick[i-13,j]
  }
}

lipstick_overview <- lipstick_rolling %>% mutate(lipstick_overview = rowSums(.[2:10])) %>% select(Date, lipstick_overview)

blusher_rolling<-blusher

blusher <- as.data.frame(blusher)

for (j in 2:10){
  for ( i in 14:183){
    blusher_rolling[i,j]<-blusher[i,j]+
      blusher[i-1,j]+blusher[i-2,j]+blusher[i-3,j]+
      blusher[i-4,j]+blusher[i-5,j]+blusher[i-6,j]+
      blusher[i-7,j]+blusher[i-8,j]+blusher[i-9,j]+
      blusher[i-10,j]+blusher[i-11,j]+blusher[i-12,j]+
      blusher[i-13,j]
  }
}

blusher_overview <- blusher_rolling %>% mutate(blusher_overview = rowSums(.[2:10])) %>% select(Date, blusher_overview)

# Make Weighted Views -----------------------------------------------------------------------------------------------------------------------------------

views <- fread("youtube_views.csv")

logrollingfun <- function(views, review) {
  views * log(2+review)
}

bbcream_join <- left_join(views, bbcream_rolling, by ='Date')
bbcream_join <- bbcream_join %>% mutate(weighted_lamuqe.view = logrollingfun(Daily.lamuqe.view, lamuqe_BB_파운데이션_컴팩트류),
                                        weighted_PONY.view = logrollingfun(Daily.PONY.view, pony_BB_파운데이션_컴팩트류),
                                        weighted_Daddoa.view = logrollingfun(Daily.Daddoa.view, daddoa_BB_파운데이션_컴팩트류),
                                        weighted_Sunny.view = logrollingfun(Daily.Sunny.view, sunny_BB_파운데이션_컴팩트류),
                                        weighted_Ssinnim.view = logrollingfun(Daily.Ssinnim.view, ssinnim_BB_파운데이션_컴팩트류),
                                        weighted_Yeondookong.view = logrollingfun(Daily.Yeondookong.view, yeondukong_BB_파운데이션_컴팩트류),
                                        weighted_Yunjjami.view = logrollingfun(Daily.Yunjjami.view, yunjjami_BB_파운데이션_컴팩트류),
                                        weighted_Hanbyul.view = logrollingfun(Daily.Hanbyul.view, hanbyul_BB_파운데이션_컴팩트류),
                                        weighted_RISABAE.view = logrollingfun(Daily.RISABAE.view, risabae_BB_파운데이션_컴팩트류))

bbcream_weighted_view <- bbcream_join %>% 
  select(c(1,22:30)) %>% 
  mutate(bbcream_weighted_view = rowSums(.[2:10])) %>%
  mutate(bbcream_weighted_view = bbcream_weighted_view/9) %>% 
  select(Date, bbcream_weighted_view)

lipstick_join <- left_join(views, lipstick_rolling, by ='Date')
lipstick_join <- lipstick_join %>% mutate(weighted_lamuqe.view = logrollingfun(Daily.lamuqe.view, lamuqe_립스틱_립라이너),
                                          weighted_PONY.view = logrollingfun(Daily.PONY.view, pony_립스틱_립라이너),
                                          weighted_Daddoa.view = logrollingfun(Daily.Daddoa.view, daddoa_립스틱_립라이너),
                                          weighted_Sunny.view = logrollingfun(Daily.Sunny.view, sunny_립스틱_립라이너),
                                          weighted_Ssinnim.view = logrollingfun(Daily.Ssinnim.view, ssinnim_립스틱_립라이너),
                                          weighted_Yeondookong.view = logrollingfun(Daily.Yeondookong.view, yeondukong_립스틱_립라이너),
                                          weighted_Yunjjami.view = logrollingfun(Daily.Yunjjami.view, yunjjami_립스틱_립라이너),
                                          weighted_Hanbyul.view = logrollingfun(Daily.Hanbyul.view, hanbyul_립스틱_립라이너),
                                          weighted_RISABAE.view = logrollingfun(Daily.RISABAE.view, risabae_립스틱_립라이너))

lipstick_weighted_view <- lipstick_join %>% 
  select(c(1,22:30)) %>% 
  mutate(lipstick_weighted_view = rowSums(.[2:10])) %>%
  mutate(lipstick_weighted_view = lipstick_weighted_view/9) %>% 
  select(Date, lipstick_weighted_view)

blusher_join <- left_join(views, blusher_rolling, by ='Date')
blusher_join <- blusher_join %>% mutate(weighted_lamuqe.view = logrollingfun(Daily.lamuqe.view, lamuqe_블러셔_쉐이딩_하이라이터),
                                        weighted_PONY.view = logrollingfun(Daily.PONY.view, pony_블러셔_쉐이딩_하이라이터),
                                        weighted_Daddoa.view = logrollingfun(Daily.Daddoa.view, daddoa_블러셔_쉐이딩_하이라이터),
                                        weighted_Sunny.view = logrollingfun(Daily.Sunny.view, sunny_블러셔_쉐이딩_하이라이터),
                                        weighted_Ssinnim.view = logrollingfun(Daily.Ssinnim.view, ssinnim_블러셔_쉐이딩_하이라이터),
                                        weighted_Yeondookong.view = logrollingfun(Daily.Yeondookong.view, yeondukong_블러셔_쉐이딩_하이라이터),
                                        weighted_Yunjjami.view = logrollingfun(Daily.Yunjjami.view, yunjjami_블러셔_쉐이딩_하이라이터),
                                        weighted_Hanbyul.view = logrollingfun(Daily.Hanbyul.view, hanbyul_블러셔_쉐이딩_하이라이터),
                                        weighted_RISABAE.view = logrollingfun(Daily.RISABAE.view, risabae_블러셔_쉐이딩_하이라이터))

blusher_weighted_view <- blusher_join %>% 
  select(c(1,22:30)) %>% 
  mutate(blusher_weighted_view = rowSums(.[2:10])) %>%
  mutate(blusher_weighted_view = blusher_weighted_view/9) %>% 
  select(Date, blusher_weighted_view)

# Making Final Data --------------------------------------------------------------------------------------------------------------------------------------
# --------------------------------------------------------------------------------------------------------------------------------------------------------

# Target Variable
# cosmetic_spread <- fread("cosmetic_spread.csv")
colnames(cosmetic_spread)[1] <- "Date"

# Weather Data
# weather <- fread("weather.csv")

# Sentiment Data
# bbcream_sentiment <- fread("bbcream_sentiment.csv")
# lipstick_sentiment <- fread("lipstick_sentiment.csv")
# blusher_sentiment <- fread("blusher_sentiment.csv")

# Youtube Data
views <- fread("youtube_views.csv")
subs <- fread("youtube_subs.csv")

# Weighted Views Data
# weightedviews <- fread("")

# Search2 Data
# search2_carpentered <- fread("")

# Making BBcream/Foundation/Compact Dataset ---------------------------------------------------------------------------------------------------------------
bbcream_final <- left_join(cosmetic_spread[,c(1,2)], weather, by="Date") %>%
  left_join(., dayofweek, by="Date") %>% 
  left_join(., search_string[,c(1,2)], by="Date") %>% 
  left_join(., bbcream_sentiment, by="Date") %>% 
  left_join(., views[,c(1,12)], by="Date") %>%
  left_join(., subs[,c(1,12)], by="Date") %>% 
  left_join(., bbcream_overview, by="Date") %>% 
  left_join(., bbcream_weighted_view, by="Date") %>% 
  rename(bbcream = `BB/파운데이션/컴팩트류`)

# Making Lipstick Dataset ---------------------------------------------------------------------------------------------------------------------------------
lipstick_final <- left_join(cosmetic_spread[,c(1,17)], weather, by="Date") %>% 
  left_join(., dayofweek, by="Date") %>% 
  left_join(., search_string[,c(1,3)], by="Date") %>% 
  left_join(., lipstick_sentiment, by="Date") %>% 
  left_join(., views[,c(1,12)], by="Date") %>%
  left_join(., subs[,c(1,12)], by="Date") %>% 
  left_join(., lipstick_overview, by="Date") %>% 
  left_join(., lipstick_weighted_view, by="Date") %>% 
  rename(lipstick = `립스틱/립라이너`)

# Making Blusher Dataset ----------------------------------------------------------------------------------------------------------------------------------
blusher_final <- left_join(cosmetic_spread[,c(1,27)], weather, by="Date") %>% 
  left_join(., dayofweek, by="Date") %>% 
  left_join(., search_string[,c(1,6)], by="Date") %>% 
  left_join(., blusher_sentiment, by="Date") %>% 
  left_join(., views[,c(1,12)], by="Date") %>%
  left_join(., subs[,c(1,12)], by="Date") %>% 
  left_join(., blusher_overview, by="Date") %>% 
  left_join(., blusher_weighted_view, by="Date") %>% 
  rename(blusher = `블러셔/쉐이딩/하이라이터`)

# Imputate NAs --------------------------------------------------------------------------------------------------------------------------------------------
for (i in 2:ncol(bbcream_final)){
  print(c(colnames(bbcream_final)[i],sum(is.na(bbcream_final[,i]))))
}

bbcream_final$Rain[is.na(bbcream_final$Rain)] <- 0 # 강수량은 NA를 0으로
bbcream_final$Finedust <- na.spline(bbcream_final$Finedust) # 미세먼지는 spline으로 
bbcream_final$bbcream_sentiment[is.na(bbcream_final$bbcream_sentiment)] <- 0 # 감성분석은 0으로

for (i in 2:ncol(lipstick_final)){
  print(c(colnames(lipstick_final)[i],sum(is.na(lipstick_final[,i]))))
}

lipstick_final$Rain[is.na(lipstick_final$Rain)] <- 0 # 강수량은 NA를 0으로
lipstick_final$Finedust <- na.spline(lipstick_final$Finedust) # 미세먼지는 spline으로 
lipstick_final$lipstick_sentiment[is.na(lipstick_final$lipstick_sentiment)] <- 0 # 감성분석은 0으로

for (i in 2:ncol(blusher_final)){
  print(c(colnames(blusher_final)[i],sum(is.na(blusher_final[,i]))))
}

blusher_final$Rain[is.na(blusher_final$Rain)] <- 0 # 강수량은 NA를 0으로
blusher_final$Finedust <- na.spline(blusher_final$Finedust) # 미세먼지는 spline으로 
blusher_final$blusher_sentiment[is.na(blusher_final$blusher_sentiment)] <- 0 # 감성분석은 0으로

write.csv(bbcream_final, "bbcream_final.csv", row.names=F)
write.csv(lipstick_final, "lipstick_final.csv", row.names=F)
write.csv(blusher_final, "blusher_final.csv", row.names=F)







###########################################
####### 수요트렌드 예측 - Modeling ########
###########################################



########################
# Random Forest VarImp #
########################

bbcreamrf <- randomForest(bbcream ~., data=bbcream_final %>% select(-Date), ntree=1000)
varImpPlot(bbcreamrf)

lipstickrf <- randomForest(lipstick ~., data=lipstick_final %>% select(-Date), ntree=1000)
varImpPlot(lipstickrf)

blusherrf <- randomForest(blusher~., data=blusher_final %>% select(-Date), ntree=1000)
varImpPlot(blusherrf)

######################
# ARIMAX forecasting #
######################

bbcream_final <- fread("bbcream_final.csv")

n <- nrow(bbcream_final)        

y <- bbcream_final[,2] %>% as.matrix
x_step1 <- cbind(bbcream_final[,c(3:7)], model.matrix(~as.factor(bbcream_final$dayofweek))[,-1]) %>% as.matrix
x_step2 <- cbind(bbcream_final[,c(3:7,9:10)], model.matrix(~as.factor(bbcream_final$dayofweek))[,-1]) %>% as.matrix
x_step3 <- cbind(bbcream_final[,c(3:7,9:14)], model.matrix(~as.factor(bbcream_final$dayofweek))[,-1]) %>% as.matrix

# Optimal model choice
arimax_step1 <- auto.arima(y[1:122], xreg=x_step1[1:122,], stepwise=F)
arimax_step1     #arima(1,1,1)

arimax_step2 <- auto.arima(y[1:122], xreg=x_step2[1:122,], stepwise=F)
arimax_step2     #arima(1,1,1)

arimax_step3 <- auto.arima(y[1:122], xreg=x_step3[1:122,], stepwise=F)
arimax_step3     #arima(1,0,0)

# Rolling Window

nf = 56 # number of forecasting step

rw <- c(1, 15, 29, 43)

# Step1 : Weather, Dayofweek
ARIMAX_pred1=matrix(0,nf,1)

for(i in rw){ 
  y_rolling = y[i:(121+i),]
  x_rolling_step1 = x_step1[i:(121+i),]
  
  fit1 = arima(y_rolling,order = c(1,0,0),xreg = x_rolling_step1, method = "ML")
  ARIMAX_pred1[i:(i+13)] = predict(fit1, newxreg = matrix(x_step1[(122+i):(135+i),], nrow=14))$pred
}

# Step2 : Weather, Dayofweek + Search, Sentiment
ARIMAX_pred2=matrix(0,nf,1)

for(i in rw){ 
  y_rolling = y[i:(121+i),]
  x_rolling_step2 = x_step2[i:(121+i),]
  
  fit2 = arima(y_rolling,order = c(1,0,0),xreg = x_rolling_step2, method = "ML")
  ARIMAX_pred2[i:(i+13)] = predict(fit2, newxreg = matrix(x_step2[(122+i):(135+i),], nrow=14))$pred
}


# Step3 : Weather, Dayofweek + Search, Sentiment + Youtube variables
ARIMAX_pred3=matrix(0,nf,1)

for(i in rw){ 
  y_rolling = y[i:(121+i),]
  x_rolling_step3 = x_step3[i:(121+i),]
  
  fit3 = arima(y_rolling,order = c(1,0,0),xreg = x_rolling_step3, method = "ML")
  ARIMAX_pred3[i:(i+13)] = predict(fit3, newxreg = matrix(x_step3[(122+i):(135+i),], nrow=14))$pred
}

# Comparison
comp = cbind(y[123:178],ARIMAX_pred1,ARIMAX_pred2,ARIMAX_pred3)

yf   = y[123:178]
se1  = (yf-ARIMAX_pred1)^2 %>% sqrt()
se2  = (yf-ARIMAX_pred2)^2  %>% sqrt()
se3  = (yf-ARIMAX_pred3)^2  %>% sqrt()

RMSE_by_Variable  = cbind(mean(se1),mean(se2),mean(se3))
colnames(RMSE_by_Variable) <- c("Variable_Set1","Variable_Set2","Variable_Set3")
RMSE_by_Variable

RMSE_by_Window = cbind(mean(se3[1:14]), mean(se3[15:28]), mean(se3[29:42]), mean(se3[43:56]))
colnames(RMSE_by_Window) <- c("Window1","Window2","Window3", "Window4")
RMSE_by_Window

###########################
# FORECASTXGB forecasting #
###########################

bbcream_final <- fread("bbcream_final.csv")

n <- nrow(bbcream_final)        

y <- bbcream_final[,2] %>% as.matrix
x_step1 <- cbind(bbcream_final[,c(3:7)], model.matrix(~as.factor(bbcream_final$dayofweek))[,-1]) %>% as.matrix
x_step2 <- cbind(bbcream_final[,c(3:7,9:10)], model.matrix(~as.factor(bbcream_final$dayofweek))[,-1]) %>% as.matrix
x_step3 <- cbind(bbcream_final[,c(3:7,9:14)], model.matrix(~as.factor(bbcream_final$dayofweek))[,-1]) %>% as.matrix

# Parameter Tuning
# Season & Trend

rmse_vec_seas_trend <- list()
k=1

for (i in c("differencing", "none")) {
  for (j in c("dummies", "none", "fourier")) {
    fitxg <- xgbar(ts(y[1:122,]), xreg = x_step1[1:122,], 
                   trend_method = i, seas_method = j,
                   nrounds_method = "cv", nfold = 4)
    xg_pred <- forecast(fitxg, xreg=x_step1[123:183,])
    rmse_xg <- (y[123:183,] - xg_pred$mean)^2 %>% mean() %>% sqrt()
    rmse_vec_seas_trend[[k]] <- list(trend=i, season=j, rmse=rmse_xg)
    k <- k+1
  }
}  # trend=none, season=dummies

# Lambda

rmse_vec1 <- NULL

for (i in seq(0.01, 1, by=0.01)) {
  fitxg <- xgbar(ts(y[1:122,]), xreg = x_step1[1:122,], lambda = i,
                 trend_method = "none", seas_method = "dummies",
                 nrounds_method = "cv", nfold = 4)
  xg_pred <- forecast(fitxg, xreg=x_step1[123:183,])
  rmse_xg1 <- (y[123:183,] - xg_pred$mean)^2 %>% mean() %>% sqrt()
  rmse_vec1 <- c(rmse_vec1, rmse_xg1)
} # lmabda = 0.87

# Rolling Window

nf = 56 # number of forecasting step

rw <- c(1, 15, 29, 43)

# Step1 : Weather, Dayofweek
XGBAR_pred1=matrix(0,nf,1)

for(i in rw){ 
  y_rolling = y[i:(121+i),]
  x_rolling_step1 = x_step1[i:(121+i),]
  
  fit1 = xgbar(ts(y_rolling) ,xreg = x_rolling_step1,
               trend_method = "none", seas_method = "dummies",
               lambda = 0.87,
               nrounds_method = "cv", nfold = 4)
  XGBAR_pred1[i:(i+13)] = forecast(fit1, xreg = x_step1[(122+i):(135+i),])$mean
}

# Step2 : Weather, Dayofweek + Search, Sentiment
XGBAR_pred2=matrix(0,nf,1)

for(i in rw){ 
  y_rolling = y[i:(121+i),]
  x_rolling_step2 = x_step2[i:(121+i),]
  
  fit2 = xgbar(ts(y_rolling),xreg = x_rolling_step2,
               trend_method = "none", seas_method = "dummies",
               lambda = 0.87,
               nrounds_method = "cv", nfold = 4)
  XGBAR_pred2[i:(i+13)] = forecast(fit2, xreg = x_step2[(122+i):(135+i),])$mean
}


# Step3 : Weather, Dayofweek + Search, Sentiment + Youtube variables
XGBAR_pred3=matrix(0,nf,1)

for(i in rw){ 
  y_rolling = y[i:(121+i),]
  x_rolling_step3 = x_step3[i:(121+i),]
  
  fit3 = xgbar(ts(y_rolling),xreg = x_rolling_step3,
               trend_method = "none", seas_method = "dummies",
               lambda = 0.87,
               nrounds_method = "cv", nfold = 4)
  XGBAR_pred3[i:(i+13)] = forecast(fit3, xreg = x_step3[(122+i):(135+i),])$mean
}



# Comparison
comp = cbind(y[123:178],XGBAR_pred1,XGBAR_pred2,XGBAR_pred3)

yf   = y[123:178]
se1  = (yf-XGBAR_pred1)^2 %>% sqrt()
se2  = (yf-XGBAR_pred2)^2  %>% sqrt()
se3  = (yf-XGBAR_pred3)^2  %>% sqrt()

RMSE_by_Variable  = cbind(mean(se1),mean(se2),mean(se3))
colnames(RMSE_by_Variable) <- c("Variable_Set1","Variable_Set2","Variable_Set3")
RMSE_by_Variable

RMSE_by_Window = cbind(mean(se3[1:14]), mean(se3[15:28]), mean(se3[29:42]), mean(se3[43:56]))
colnames(RMSE_by_Window) <- c("Window1","Window2","Window3", "Window4")
RMSE_by_Window

##############
## XGBOOST ###
##############

library(dummies)
library(xgboost)
library(randomForest)
library(dplyr)
library(ggplot2)
# Modeling 

bbcream_final<-fread('bbcream_final.csv')
blusher_final<-fread('blusher_final.csv')
lipstick_final<-fread('lipstick_final.csv')

### XGbosot start

data_dummy<-dummy.data.frame(bbcream_final)   
# blusher_final, lipstick_final 로 하고싶으면 각각 넣어주면 됨. 이하 코딩은 동일

t1_y<-c(0,data_dummy$bbcream[1:(nrow(data_dummy)-1) ])

bbcream_final<-data_dummy %>% mutate(t1_y = t1_y)

bb_train <- bbcream_final  %>% filter(Date <= 20180801) %>% 
  select(-'Date') %>% mutate_if(is.character, as.factor)

bb_train <- bb_train[-1,]

train_label<-bb_train[,1]

## test set
bb_test <- bbcream_final  %>% filter(Date > 20180801) %>% 
  select(-'Date') %>% mutate_if(is.character, as.factor)


## grid_search_fun

grid_list <- expand.grid(subsample = seq(0.5,1,0.1), 
                         colsample_bytree = seq(0.5,1,0.1),
                         max_depth = seq(3,10,1),
                         min_child = seq(1,10,1), 
                         eta = 0.05
)

xg_param_tune <- function(xgtrain, grid_list){
  RMSE_list<- apply(grid_list, 1, function(List){
    
    SubsampleRate <- List[["subsample"]]
    ColsampleRate <- List[["colsample_bytree"]]
    Depth <- List[["max_depth"]]
    Eta <- List[["eta"]]
    MinChild <- List[["min_child"]]
    ModelCV <- xgb.cv(data =  xgtrain, nrounds = 2000, nfold = 3, showsd = TRUE, 
                      metrics = "rmse", verbose = F, "eval_metric" = "rmse",
                      "objective" = "reg:linear", "max.depth" = Depth, "eta" = Eta,                               "subsample" = SubsampleRate, "colsample_bytree" = ColsampleRate
                      , "min_child_weight" = MinChild, objective = "reg:linear",
                      early_stopping_rounds = 60,nthread=2)
    
    validationScores <- as.data.frame(ModelCV$evaluation_log)
    
    rmse <- tail(validationScores$test_rmse_mean, 1)
    trmse <- tail(validationScores$train_rmse_mean,1)
    output <- c(rmse, trmse, SubsampleRate, ColsampleRate, Depth, Eta, MinChild)
    
  })
  output_final <- as.data.frame(t(RMSE_list))
  colnames(output_final) <-c("TestRMSE", "TrainRMSE", "SubSampRate", "ColSampRate", "Depth", "eta", "MinChild")
  #write.csv(output, "xgb_grid_lpoint.csv",row.names = F)
  output_sort<-output_final %>% arrange(TestRMSE)
  return(output_sort)
}


## 3 step fun

xg_step <- function(xgtrain_step, xgtest_step){
  cnum_step<-dim(xgtrain_step)[2]
  
  # parameter 
  output_sort <-xg_param_tune(xgtrain_step,grid_list)
  best_param<-output_sort[1,]
  
  # nround
  xgb_params <- list(objective = "reg:linear",
                     max.depth = best_param$Depth, 
                     min_child_weight = best_param$MinChild,
                     subsample = best_param$SubSampRate, 
                     colsample_bytree = best_param$ColSampRate,
                     eta = 0.05,
                     eval_metric = "rmse")
  
  cv_model <- xgb.cv(params = xgb_params,
                     data = xgtrain_step, 
                     nrounds = 5000,
                     nfold = 3,
                     verbose = T,
                     prediction = TRUE,
                     early_stopping_rounds = 50,
                     nthread=2)
  
  best_nround=cv_model$best_iteration
  
  # Xgbosot Rolling Window
  
  nf = 60 # number of forecasting step
  
  rw <- c(1, 16, 31, 46)
  
  xgboost_pred=rep(0,nf)
  
  for ( i in rw){
    
    xgtrain_window <- xgb.DMatrix(as.matrix(bbcream_final[i:(121+i) , c(3:(cnum_step+1) ,21)  ]), ###
                                  label = bbcream_final$bbcream[i:(121+i)])
    
    
    xg_model_window <- xgb.train(params = xgb_params,
                                 data = xgtrain_window,
                                 nrounds = best_nround)
    
    for ( j in 1:15 ){
      test_point <-xgtest_step[i+j-1,]
      if (j == 1){ test_point[cnum_step] <- bbcream_final$t1_y[122+i]  }
      else { test_point[cnum_step] <-  old_tp }
      names(test_point)[ cnum_step ] <- 't1_y'
      test_point <- t(as.matrix(test_point))
      
      tp<-predict(xg_model_window,test_point,type='class')
      old_tp <-tp
      xgboost_pred[i+j-1] <- tp
    }
    
  }
  
  return(list(param_grid = output_sort,best_nround,xgboost_pred))
}


# Imprtance Matrix
#imp_matrix <- xgb.importance(model = xg_model_window)
# plot 
#impplot = xgb.ggplot.importance(importance_matrix = imp_matrix, top_n = 30)
#impplot


## Maek 3 step 
# Step1 : Weather, Dayofweek  # 13
xgtrain_step1 <- xgb.DMatrix(as.matrix(bb_train[,c(2:13,20) ]),
                             label = train_label)

xgtest_step1 <- as.matrix(bb_test[,c(2:13,20)])


# Step2 : Weather, Dayofweek + Search, Sentiment   # 15
xgtrain_step2 <- xgb.DMatrix(as.matrix(bb_train[,c(2:15,20) ]),
                             label = train_label)

xgtest_step2 <- as.matrix(bb_test[,c(2:15,20)])


# Step3 : Weather, Dayofweek + Search, Sentiment + Youtube variables  #  19
xgtrain_step3 <- xgb.DMatrix(as.matrix(bb_train[,-1]),
                             label = train_label)

xgtest_step3 <- as.matrix(bb_test[,-1])


### Forecasting

bbcream_xg_pred1 <- xg_step(xgtrain_step1,xgtest_step1)
bbcream_xg_pred2 <- xg_step(xgtrain_step2,xgtest_step2)
bbcream_xg_pred3 <- xg_step(xgtrain_step3,xgtest_step3)

RMSE(pred1[[3]],bb_test$bbcream)

# 마찬가지로 blusher , lipstick 도 실시 
# dummy_data <-dummy.data.frame(blusher_final)
# dummy_data <-dummy.data.frame(lipstich_final) 로 첫 코딩만 바꿔서 실행하면 됨.






######### grid_visual ### 

output_vis<-output %>% mutate( 
  Depth= paste('Depth',Depth,sep = ':') %>% as.factor,
  MinChild= paste('MinChild',MinChild,sep = ':') %>% as.factor)

output_vis2 <- output_vis %>%  filter(MinChild != 'MinChild:10')
ggplot(output_vis2,aes(x=SubSampRate,y=ColSampRate ))+ 
  geom_tile(aes(fill=TestRMSE),colour = "white") + 
  scale_fill_gradient(low = "lightyellow",high = "black")+
  facet_grid(Depth ~ MinChild)+
  labs(x="SubSampRate",
       y="ColSampRate",
       title = "XGboost Grid Search", 
       subtitle="Maxdepth, Minchild, Colsample, Subsample")


#####################
### Random Forest ###
#####################

bbcream_final<-fread('bbcream_final.csv')
blusher_final<-fread('blusher_final.csv')
lipstick_final<-fread('lipstick_final.csv')

RF_data<-bbcream_final
# blusher_final, lipstick_final 로 하고싶으면 각각 넣어주면 됨. 이하 코딩은 동일

t1_y<-c(0,RF_data$bbcream[1:(nrow(RF_data)-1) ])

bbcream_final<- RF_data %>% mutate(t1_y = t1_y)
bbcream_final<-bbcream_final %>% mutate_if(is.character, as.factor)
bbcream_final <- bbcream_final[-1,]

# train set
bb_train <- bbcream_final  %>% filter(Date <= 20180801) %>% 
  select(-'Date') %>% mutate_if(is.character, as.factor)



## test set
bb_test <- bbcream_final  %>% filter(Date > 20180801) %>% 
  select(-'Date') %>% mutate_if(is.character, as.factor)


# RF rolling window

nf = 60 # number of forecasting step

rw <- c(1, 16, 31, 46)

RF_pred=rep(0,nf)
rf_imp <- vector(length(rw),mode = "list")

rf_step <- function(rftrain_step, rftest_step){
  cnum_step<-dim(rftrain_step)[2]
  
  ## grid_search
  
  control <- trainControl(method='cv', 
                          number=3, 
                          search='grid')
  
  tunegrid <- expand.grid(.mtry = (1:cnum_step))
  
  rf_gridsearch <- train(bbcream ~ ., 
                         data = rftrain_step,
                         method = 'rf',
                         metric = 'RMSE',
                         tuneGrid = tunegrid,
                         trControl = control)
  
  # find best mtry
  best_mtry<- as.numeric(rf_gridsearch$bestTune) 
  
  k<-0
  for ( i in rw){
    k<-k+1
    rftrain_window <- bbcream_final[i:(121+i) , c(2:(cnum_step) ,15) ]
    
    # modeling
    rf_model_window = randomForest(bbcream ~ ., ntree=300, mtry = best_mtry,
                                   data = rftrain_window)
    # feature importance
    rf_imp[[k]] <- rf_model_window$importance[order(rf_model_window$importance,decreasing = T),] 
    
    for ( j in 1:15 ){
      test_point <-rftest_step[i+j-1,]
      if (j == 1){ test_point[cnum_step] <- bbcream_final$t1_y[122+i]  }
      else { test_point[cnum_step] <-  old_tp }
      names(test_point)[ cnum_step ] <- 't1_y'
      
      tp<-predict(rf_model_window,test_point,type='class')
      old_tp <-tp
      RF_pred[i+j-1] <- tp
    }
    
  }
  return(list(rf_imp = rf_imp, rf_pred = RF_pred))
}






## Maek 3 step 
# Step1 : Weather, Dayofweek  # 13
rftrain_step1 <- bb_train[, c(1:7,14) ]

rftest_step1 <- bb_test[,c(1:7,14) ]

bbcream_rf_pred1<-rf_step(rftrain_step1,rftest_step1)

#write.csv(bbcream_rf_pred1[[2]], "rf_pred1.csv",row.names = F)
RMSE(bbcream_rf_pred1[[2]],bb_test$bbcream)



# Step2 : Weather, Dayofweek + Search, Sentiment   # 15
rftrain_step2 <- bb_train[, c(1:9,14) ]

rftest_step2 <- bb_test[,c(1:9,14) ]

bbcream_rf_pred2<-rf_step(rftrain_step2,rftest_step2)


#write.csv(bbcream_rf_pred2[[2]], "rf_pred2.csv",row.names = F)
RMSE(bbcream_rf_pred2[[2]],bb_test$bbcream)



# Step3 : Weather, Dayofweek + Search, Sentiment + Youtube variables  #  19
rftrain_step3 <- bb_train

rftest_step3 <- bb_test

bbcream_rf_pred3<-rf_step(rftrain_step3,rftest_step3)


#write.csv(bbcream_rf_pred3[[2]], "rf_pred3.csv",row.names = F)
RMSE(bbcream_rf_pred3[[2]],bb_test$bbcream)



##########################################
############ 온라인 선호지수##############
##########################################

#four_data : Product, Session, Custom, Master 네 개 파일을 CLNT_ID 기준으로 Merge한 파일

getwd()
setwd()

four_data <- fread("")



four_data <- left_join(product,custom) %>% left_join(session) %>% left_join(master) %>% arrange(CLNT_ID)
rm(custom,master,pd_cu,product,session)
rm(search1,search2)
four_data %<>% 
  mutate(PD_BUY_AM = gsub("[[:punct:]]", "", PD_BUY_AM), PD_BUY_CT = gsub("[[:punct:]]", "", PD_BUY_CT)) %>% 
  mutate(PD_BUY_AM = as.integer(PD_BUY_AM), PD_BUY_CT = as.integer(PD_BUY_CT))

four_data %<>% 
  mutate(TOT_SESS_HR_V = gsub("[[:punct:]]", "", TOT_SESS_HR_V)) %>% 
  mutate(TOT_SESS_HR_V = as.integer(TOT_SESS_HR_V))

four_data <-
  four_data %>% 
  arrange(CLNT_ID, SESS_ID, HITS_SEQ, SESS_SEQ, SESS_DT) %>% 
  select(CLNT_ID, SESS_ID, PD_NM, PD_BRA_NM, PD_BUY_AM, PD_BUY_CT, PD_ADD_NM,
         SESS_SEQ, HITS_SEQ, TOT_PAG_VIEW_CT, TOT_SESS_HR_V,
         everything())

four_data <- 
  four_data %>% 
  select(CLNT_ID, SESS_ID, PD_NM, PD_BRA_NM, PD_BUY_AM, PD_BUY_CT, PD_ADD_NM, SESS_DT,
         SESS_SEQ, HITS_SEQ, TOT_PAG_VIEW_CT, TOT_SESS_HR_V,
         everything())


#####  유튜브 변수가 '있는' 업종들의 온라인 선호지수 만들기 --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



dat <- four_data %>% filter(CLAC3_NM == "립스틱/립라이너")
dat_youtube <- fread("lipstick.csv") %>% as_tibble

dat <- four_data %>% filter(CLAC3_NM == "BB/파운데이션/컴팩트류")
dat_youtube <- fread("bbcream.csv") %>% as_tibble

dat <- four_data %>% filter(CLAC3_NM == "블러셔/쉐이딩/하이라이터")
dat_youtube <- fread("blusher.csv") %>% as_tibble



names(dat_youtube) <- c("Date",                  "sales",              
                        "search_product",        "product_sentiment",    
                        "DailyViewAverage",      "DailysubsAverage",     
                        "product_overview",      "product_weighted_view")

# 성별변수   - 비율로 만든다

gender <- dat %>% group_by(SESS_DT) %>% summarise(Female = table(CLNT_GENDER)[1], Male = table(CLNT_GENDER)[2])
gender[is.na(gender)]<-0
gender <- gender %>% mutate(FEMALE_RATIO = Female/(Female+Male)) %>% select(SESS_DT,FEMALE_RATIO)

# 최종
dat <- dat %>% group_by(SESS_DT) %>% summarise(HITS_SEQ = sum(HITS_SEQ, na.rm = T),
                                               TOT_PAG_VIEW_CT = sum(TOT_PAG_VIEW_CT, na.rm = T),
                                               TOT_SESS_HR_V = sum(TOT_SESS_HR_V, na.rm = T),
                                               AGE_DISTINCT = n_distinct(CLNT_AGE),
                                               PROVINCE_DISTINCT = n_distinct(ZON_NM),
                                               CITY_DISTINCT = n_distinct(CITY_NM)
)

dat <- inner_join(dat,gender)
rm(gender)

final <-
  inner_join(dat, dat_youtube, by = c("SESS_DT" = "Date")) %>%
  select(-product_overview, -product_weighted_view) %>% 
  select(SESS_DT, sales, everything()) 
rm(dat,dat_youtube)




### ----------------------------------------------------------------------------------------------
# PCA 진행하기
library(psych)
#
pca_none<-principal(final[,-1],
                    nfactors = 12,
                    rotate = "none") 

pca_none 
pca_none$scores


#
pca_varimax<-principal(final[,-1],
                       nfactor = 12,
                       rotate = "varimax") 

pca_varimax
pca_varimax$scores 


# RC1 과 RC2의 평균값 산출

cor( ((pca_varimax$scores[,1])*(pca_varimax$scores[,2])/2),
     final$sales) 

cbind(pca_none$scores[,1], pca_varimax$scores[,1]) # score 값들이 거의 비슷한 것을 확인가능. 
cor(pca_none$scores[,1], pca_varimax$scores[,1]) # 상관계수도 거의 1에 가깝다

pca_varimax$Vaccounted %>% dim
pca_varimax$Vaccounted[4,1]+pca_varimax$Vaccounted[4,2]

#
libstick <- cbind(final, 
                  (pca_varimax$scores[,1]*(pca_varimax$Vaccounted[4,1]/(pca_varimax$Vaccounted[4,1]+pca_varimax$Vaccounted[4,2]))) + 
                    (pca_varimax$scores[,2]*(pca_varimax$Vaccounted[4,2]/(pca_varimax$Vaccounted[4,1]+pca_varimax$Vaccounted[4,2])))) %>%
  as_tibble %>% rename('PREF_INDEX' = "(pca_varimax$scores[, 1] * (pca_varimax$Vaccounted[4, 1]/(pca_varimax$Vaccounted[4, ") 

libstick$SESS_DT<- paste(str_sub(libstick$SESS_DT,1,4),
                         str_sub(libstick$SESS_DT,5,6),
                         str_sub(libstick$SESS_DT,7,8),sep='-')
libstick$SESS_DT <- as.Date(libstick$SESS_DT)



#
bbcream <- cbind(final, 
                  (pca_varimax$scores[,1]*(pca_varimax$Vaccounted[4,1]/(pca_varimax$Vaccounted[4,1]+pca_varimax$Vaccounted[4,2]))) + 
                    (pca_varimax$scores[,2]*(pca_varimax$Vaccounted[4,2]/(pca_varimax$Vaccounted[4,1]+pca_varimax$Vaccounted[4,2])))) %>%
  as_tibble %>% rename('PREF_INDEX' = "(pca_varimax$scores[, 1] * (pca_varimax$Vaccounted[4, 1]/(pca_varimax$Vaccounted[4, ") 

bbcream$SESS_DT<- paste(str_sub(bbcream$SESS_DT,1,4),
                         str_sub(bbcream$SESS_DT,5,6),
                         str_sub(bbcream$SESS_DT,7,8),sep='-')
bbcream$SESS_DT <- as.Date(bbcream$SESS_DT)

#
blusher <- cbind(final, 
                  (pca_varimax$scores[,1]*(pca_varimax$Vaccounted[4,1]/(pca_varimax$Vaccounted[4,1]+pca_varimax$Vaccounted[4,2]))) + 
                    (pca_varimax$scores[,2]*(pca_varimax$Vaccounted[4,2]/(pca_varimax$Vaccounted[4,1]+pca_varimax$Vaccounted[4,2])))) %>%
  as_tibble %>% rename('PREF_INDEX' = "(pca_varimax$scores[, 1] * (pca_varimax$Vaccounted[4, 1]/(pca_varimax$Vaccounted[4, ") 

blusher$SESS_DT<- paste(str_sub(blusher$SESS_DT,1,4),
                         str_sub(blusher$SESS_DT,5,6),
                         str_sub(blusher$SESS_DT,7,8),sep='-')
blusher$SESS_DT <- as.Date(blusher$SESS_DT)

libstick %>% select(1,2,14) %>% View


# 각 상품군 PREF_INDEX 시각화


ggplot(data = libstick, aes(x = SESS_DT, y = PREF_INDEX, group = 1)) +
  geom_line(color="brown1")
ggplot(data = bbcream, aes(x = SESS_DT, y = PREF_INDEX, group = 1)) +
  geom_line(color="darksalmon")
ggplot(data = blusher, aes(x = SESS_DT, y = PREF_INDEX, group = 1)) +
  geom_line(color="darkturquoise")




cor(libstick$PREF_INDEX, libstick$sales)


ggplot(libstick, aes(SESS_DT)) + 
  geom_line(aes(y = scale(sales), colour = "SALES")) + 
  geom_line(aes(y = scale(PREF_INDEX), colour = "PREF_INDEX"))

ggplot(bbcream, aes(SESS_DT)) + 
  geom_line(aes(y = scale(sales), colour = "SALES")) + 
  geom_line(aes(y = scale(PREF_INDEX), colour = "PREF_INDEX"))

ggplot(blusher, aes(SESS_DT)) + 
  geom_line(aes(y = scale(sales), colour = "SALES")) + 
  geom_line(aes(y = scale(PREF_INDEX), colour = "PREF_INDEX"))




# PREF_INDEX가 바로 최종 '온라인 선호지수' 이다.
View(libstick)




cor(libstick$sales, libstick$PREF_INDEX)
cor(libstick$DailysubsAverage, libstick$PREF_INDEX)


#선호지수 시각화
library(ggplot2)
library(lubridate)


# Basic line plot with points
libstick$SESS_DT %>% as.Date()
ggplot(data = libstick, aes(x = SESS_DT, y = PREF_INDEX, group = 1)) +
  geom_line()+
  geom_point()

# Change the line type
ggplot(data = libstick, aes(x = SESS_DT, y = PREF_INDEX, group = 1)) +
  geom_line(linetype = "dashed")+
  geom_point()

# Change the color
ggplot(data = libstick, aes(x = SESS_DT, y = PREF_INDEX, group = 1)) +
  geom_line(color="red")
ggplot(data = libstick, aes(x = SESS_DT, y = sales, group = 1)) +
  geom_line(color="blue")

ggplot(libstick, aes(SESS_DT)) + 
  geom_line(aes(y = scale(sales), colour = "SALES")) + 
  geom_line(aes(y = scale(PREF_INDEX), colour = "PREF_INDEX"))

scale(libstick$sales) %>% head

#####  유튜브 변수가 '없는' 업종들 --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
### Step1)  업종별로 서치데이터를 만들어준다  (네이버 연관검색어도 사용)


product_session <- left_join(product, session, by=c("CLNT_ID", "SESS_ID"))

# Category1_spread에는  첫 카태고리 구매 횟수가 담겨있다.
Category1_spread <- left_join(product_session, master, by="PD_C") %>% 
  filter(CLAC1_NM=="패션잡화") %>% 
  group_by(SESS_DT, CLAC3_NM) %>% 
  summarize(count=sum(PD_BUY_CT)) %>% 
  spread(CLAC3_NM, count)

names(Category1_spread)[-1]


product_list<-list(
  기타모자 = c('연예인모자','모자종류','여자모자','모자메이커','남자모자','야구모자','테니스모자','여성벙거지모자','여성모자','챙모자'),
  기타여행용가방 = c('천여행가방','여행용보스턴백','보스턴백','여행가방캐리어','캐리어','여행가방추천','여행크로스백','소형여행가방','백팩','여행가방브랜드','명품여행가방','여행용백팩'),
  여성토트백 = c('여성가방','여성숄더백','여성미니토트백','여자 토트백', '여성 쇼퍼백', '여성 토드백'),
  남성백팩 = c('남성백팩','남성 백팩','백팩','백팩 브랜드','백팩브랜드','메신저백','메신저 백','남성가방','남성 가방','남성 백팩 추천','남성백팩추천','배낭여행 백팩','남자 슬링백','남자슬링백','슬링백'),
  여성스니커즈 = c('여성 스니커즈','여성','스니커즈','여성스니커즈','명품운동화','명품 운동화','여자스니커즈브랜드','여자 스니커즈','여자 스니커즈 브랜드','정려원 신발','여성슬립온','여성 명품운동화','여성명품운동화','여자명품운동화','여성운동화','여성 운동화','정려원 스니커즈','여성 스니커즈 추천','여자 스니커즈 추천','여성 키높이 스니커즈','여성키높이스니커즈'),
  유아동부츠 = c('아동부츠','아동 부츠','엠엘비키즈부츠','엠엘비키즈 부츠','노스페이스 아동부츠','노스페이스 아동 부츠','유아 겨울신발','유아 여름신발','유아 가을신발','유아 스키부츠','유아 어그','뉴발란스 아동부츠','아동 방한부츠','키즈겨울부츠','키즈 겨울 부츠','mlb 방한부츠','부츠'),
  목걸이 = c('목걸이','학생목걸이','학생 목걸이','목걸이팬던트','목걸이 팬던트','목걸이종류','목걸이 종류','심플목걸이','심플 목걸이','애견 목걸이','여성 목걸이','여성목걸이','남성목걸이','남성 목걸이','반지','은목걸이','금목걸이','은 목걸이','금 목걸이','목걸이줄','탄생석목걸이','탄생석 목걸이')
)


M<-nrow(Category1_spread)
N<-length(names(product_list))
search_string <- matrix(0, M, N+1 )  %>% as.data.frame()
colnames(search_string) <- c( 'Date',names(product_list) )
search_string$Date <- Category1_spread$SESS_DT
search2$SEARCH_CNT<-as.numeric(search2$SEARCH_CNT)

for ( j in 1:N){
  k<-length(product_list[[j]])
  
  for ( i in 1:M ){
    
    search_selected <- search2 %>% filter(SESS_DT == search_string$Date[i])      
    count_kor<-NULL
    
    for (kk in 1:k){
      iidx<-which( str_detect( search_selected$KWD_NM ,  product_list[[j]][kk]  ) ==T)
      count_kor<-sum(search_selected$SEARCH_CNT[iidx])
      
    }
    
    count_max <- sum(count_kor)
    search_string[i,j+1] <- count_max
  }
  
}



### Step2) 업종별로 다시 최종 데이터를 결합한다. 
# 각 업종별로 데이터 불러오기
names(search_string)[7]
dat <- four_data %>% filter(CLAC3_NM == names(search_string)[7])


# 성별변수   - 비율로 만든다

gender <- dat %>% group_by(SESS_DT) %>% summarise(Female = table(CLNT_GENDER)[1], Male = table(CLNT_GENDER)[2])
gender[is.na(gender)]<-0
gender <- gender %>% mutate(FEMALE_RATIO = Female/(Female+Male)) %>% select(SESS_DT,FEMALE_RATIO)

# 최종
dat <- dat %>% group_by(SESS_DT) %>% summarise(sales = sum(PD_BUY_CT),
                                               SUM_HITS_SEQ = sum(HITS_SEQ, na.rm = T),
                                               SUM_TOT_PAG_VIEW_CT = sum(TOT_PAG_VIEW_CT, na.rm = T),
                                               SUM_TOT_SESS_HR_V = sum(TOT_SESS_HR_V, na.rm = T)/3600,
                                               AGE_DISTINCT = n_distinct(CLNT_AGE),
                                               PROVINCE_DISTINCT = n_distinct(ZON_NM),
                                               CITY_DISTINCT = n_distinct(CITY_NM)
)
# Gender와 기존변수들 결합
final <- inner_join(dat,gender) 


# Search 데이터 추출
names(search_string)[c(1,7)]
search <- search_string[,names(search_string)[c(1,7)]] 
names(search) = c("SESS_DT", "SEARCH_TOT_CNT")


# 서치데이터와 기존 데이터 최종 결합
final <- inner_join(final,search)




# PCA 진행하기
library(psych)
pca <-principal(final[,-1],
                nfactors = 9,
                rotate = "varimax") 

pca$loadings
final_with_index <- cbind(final, pca$scores[,1]) %>% as_tibble
names(final_with_index)[11] <- 'PREF_INDEX' #PREF_INDEX가 최종 선호지수









##########################################################
############ 서비스 제안 2)  - 고객 군집화  ##############
##########################################################



### --------------------------------------------------------------------------
# 군집분석 시행 이전 전처리 과정 

library(tidyr)
library(cluster)
library(fpc)
library(factoextra)


### 군집분석에 필요한 파일 불러오기 ( four_data & search1)
four_data <- fread("four_data.csv") %>% as_tibble()
search1 <- fread("2_Search1.csv", encoding = "UTF-8") %>% as_tibble



### CLNT_ID별로 전처리하기
CLNT_rest <- 
  four_data %>% group_by(CLNT_ID) %>% 
  summarise(MEAN_PD_BUY_AM = mean(PD_BUY_AM),
            SUM_PD_BUY_CT = sum(PD_BUY_CT),
            FIRST_SESS_SEQ = last(SESS_SEQ),
            MEAN_HITS_SEQ = mean(HITS_SEQ),
            MEAN_TOT_PAG_VIEW_CT = mean(TOT_PAG_VIEW_CT),
            MEAN_TOT_SESS_HR_V = mean(TOT_SESS_HR_V))



# 각 고객의 평균적인 SESS_SEQ의 차이(difference)값 생성하기 
CLNT_DIFF_SESS_SEQ <- 
  four_data %>% arrange(CLNT_ID, SESS_SEQ) %>% 
  distinct(CLNT_ID, SESS_SEQ, .keep_all = T) %>% 
  select(CLNT_ID, SESS_SEQ) %>% 
  group_by(CLNT_ID) %>%
  summarise(DIFF_SESS_SEQ   = mean(SESS_SEQ - lag(SESS_SEQ), na.rm = T))
CLNT_DIFF_SESS_SEQ[is.na(CLNT_DIFF_SESS_SEQ)] <- 0 

CLNT_rest <- left_join(CLNT_rest,CLNT_DIFF_SESS_SEQ)
rm(CLNT_DIFF_SESS_SEQ)



### 각 고객별로 어떤 대분류 상품을 샀는지를 Encoding하기 
CLNT_CLAC1 <-
  four_data %>% select(1,6,14) %>% 
  mutate(i = row_number()) %>% 
  spread(CLAC1_NM, PD_BUY_CT ) %>% select(-i) 
CLNT_CLAC1[is.na(CLNT_CLAC1)] <- 0 #NA를 0으로 대체
CLNT_CLAC1 <- CLNT_CLAC1 %>% group_by_at(1) %>% summarise_all(sum)

# 두 파일 merge
CLNT <- inner_join(CLNT_rest,CLNT_CLAC1) 
rm(CLNT_rest, CLNT_CLAC1)

# 각 클라이언트 별로 서치 카운트 세기
CLNT_search1 <- 
  search1 %>% arrange(CLNT_ID) %>%
  group_by(CLNT_ID) %>%
  summarise(TOT_SEARCH_CNT = sum(SEARCH_CNT))

rm(search1)
# 최종 결합
CLNT_final <- left_join(CLNT,CLNT_search1)
CLNT_final[is.na(CLNT_final)] <- 0
rm(CLNT)


# 데모그래픽 정보 추가하기 
CLNT_DEMO<-
  four_data %>% group_by(CLNT_ID) %>% summarise(CLNT_GENDER = first(CLNT_GENDER),
                                                CLNT_AGE = first(CLNT_AGE),
                                                DVC_CTG_NM = first(DVC_CTG_NM),
                                                CITY_NM = first(ZON_NM)) 

# 최종 merge
CLNT_final <- left_join(CLNT_final, CLNT_DEMO)
rm(CLNT_DEMO, CLNT_search1)


CLNT_final <- CLNT_final %>% select(1,2,3,4,8,everything())



### 팩터변수 팩터처리 해주기

CLNT_final %>% is.na %>% sum

#기
CLNT_final$CLNT_GENDER <- as.factor(CLNT_final$CLNT_GENDER)
CLNT_final$CLNT_GENDER <- as.factor(gsub("F", 1, gsub("M", 2, CLNT_final$CLNT_GENDER)))

#
CLNT_final$DVC_CTG_NM <- as.factor(CLNT_final$DVC_CTG_NM)
CLNT_final$DVC_CTG_NM <- as.factor(gsub("mobile", 1, gsub("tablet", 2, gsub('desktop', 3, CLNT_final$DVC_CTG_NM) )))

#
CLNT_final$CLNT_AGE <- as.factor(CLNT_final$CLNT_AGE)

#
CLNT_final$CITY_NM <- as.factor(CLNT_final$CITY_NM)
CLNT_final$CITY_NM <- as.factor(gsub("Seoul", 1, 
                                     gsub("Busan", 2, 
                                          gsub('Chungcheongnam-do', 3,
                                               gsub('Gyeongsangnam-do',4, 
                                                    gsub('Jeollanam-do',5, 
                                                         gsub('Gyeongsangbuk-do',6,
                                                              gsub('Gyeonggi-do' ,7,
                                                                   gsub('Gangwon-do',8,
                                                                        gsub('Daejeon', 9,
                                                                             gsub('Daegu',10, 
                                                                                  gsub('Jeollabuk-do', 11,
                                                                                       gsub('Incheon', 12, 
                                                                                            gsub('Chungcheongbuk-do', 13,
                                                                                                 gsub('Gwangju', 14,
                                                                                                      gsub('Ulsan',15, 
                                                                                                           gsub('Jeju-do',16, CLNT_final$CITY_NM)))))))))))))))))



#write.csv(CLNT_final, "클라이언트별_변수49개펼치기.csv", row.names = F)

# 화장품 많이 구입하는 사람들 위주로 필터링 하기 
CLNT_final <- fread("클라이언트별_변수49개펼치기.csv") %>% as_tibble
CLNT_final <- CLNT_final %>% filter(`화장품/뷰티케어` > 10) # 10번 이상 구매한 obs는 약 5543명



# 클러스터링을 진행하기 전  팩터 변수를 제외하고 스케일링 해주기

CLNT_norm <- scale(CLNT_final[,-c(1,47,48,49,50)]) %>% as_tibble

CLNT_norm$CLNT_ID <- CLNT_final$CLNT_ID
CLNT_norm$CLNT_GENDER <- CLNT_final$CLNT_GENDER
CLNT_norm$CLNT_AGE <- CLNT_final$CLNT_AGE
CLNT_norm$DVC_CTG_NM <- CLNT_final$DVC_CTG_NM
CLNT_norm$CITY_NM <- CLNT_final$CITY_NM
CLNT_norm <- CLNT_norm %>% select(CLNT_ID,everything())




# Dissimilarity Matrix 구하기


gower_dist <- daisy(CLNT_norm[,-c(1)],
                    metric = "gower",
                    stand = FALSE,
                    type = list())

summary(gower_dist)

gower_mat <- as.matrix(gower_dist)

# 이 Gower Matrix를 통해 가장 유사한 두명과 비유사한 두 명을 뽑아보자
CLNT_norm[
  which(gower_mat == min(gower_mat[gower_mat != min(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
CLNT_final %>% filter(CLNT_ID == 2411712 | CLNT_ID == 2222925) %>% View


CLNT_norm[
  which(gower_mat == max(gower_mat[gower_mat != max(gower_mat)]),
        arr.ind = TRUE)[1, ], ]
CLNT_final %>% filter(CLNT_ID == 3483525 | CLNT_ID == 1662720) %>% View


### 최적의 K 찾기  -> 실루엣을 비교해보자

sil_width <- c(NA)

for(i in 2:10){
  
  pam_fit <- pam(gower_dist,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}


# Plot sihouette width - 높을수록 좋다

plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)


# Nbclust 함수를 통한 최적의 군집수 K 찾기 
library(NbClust)
nb_result_pam <- NbClust(data = CLNT_norm[,-1],
                         distance = NULL,
                         diss = gower_dist,
                         min.nc = 2, max.nc = 10, 
                         method = 'complete')


### PAM clustering 진행
pam_fit <- pam(gower_dist, diss = TRUE, k = 3)
CLNT_final$cluster <- pam_fit$clustering


### 각 군집별 통계량을 비교하여 군집별 유형 정의해주기
CLNT_final[CLNT_final$cluster == 1,] %>% summary %>% View
CLNT_final[CLNT_final$cluster == 2,] %>% summary %>% View
CLNT_final[CLNT_final$cluster == 3,] %>% summary %>% View


CLNT_final[CLNT_final$cluster == 1,] %>% select(2,3,4,5,6,7,14,23,24,26,28,45,46,48) %>% summary
CLNT_final[CLNT_final$cluster == 2,] %>% select(2,3,4,5,6,7,14,23,24,26,28,45,46,48) %>% summary
CLNT_final[CLNT_final$cluster == 3,] %>% select(2,3,4,5,6,7,14,23,24,26,28,45,46,48) %>% summary


pam_results <- 
  sample_data %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary

#중심점들에 있는 obs들을 보여준다.
CLNT_final[pam_fit$medoids, ] %>% View



