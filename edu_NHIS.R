rm(list = ls())
setwd("/Volumes/Samsung_T5/NIA_PublicData/NHIS/NHIS_OPEN_T20")


# install.packages("data.table")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("DataExplorer")
# install.packages("extrafont")


library(data.table)
library(dplyr)
library(ggplot2)
library(DataExplorer)
library(extrafont)

library(randomForest)
library(reprtree)
library(caTools)
library(caret)


font_import()
# combine csv files

par(family="NanumGothic")


filenames <- list.files("/Volumes/Samsung_T5/NIA_PublicData/NHIS/NHIS_OPEN_T20")


var.names <- c("기준년도",	"가입자일련번호",	"진료내역일련번호",	"성별코드", "연령대코드",	
               "시도코드",	"요양개시일자",	"서식코드",	"진료과목코드",	"주상병코드",	
               "부상병코드",	"요양일수",	"입내원일수", "심결가산율",	"심결요양급여비용총액",	
               "심결본인부담금",	"심결보험자부담금",	"총처방일수",	"데이터기준일자")

df.list <- list()

for (i in 1:length(filenames)){
  df.list[[i]] <- fread(filenames[i], header = TRUE, encoding = 'UTF-8')
  colnames(df.list[[i]]) <- var.names
}



plot_str(df.list[1:3])

str(df.list)



# 2016 data 

dataframe_2016 <- df.list[[15]]

plot_str(dataframe_2016)

plot_intro(dataframe_2016)

introduce(dataframe_2016)


str(dataframe_2016)

# plot_missing(dataframe_2016)

dataframe_2016$`연령대코드` <- as.factor(dataframe_2016$`연령대코드`)
dataframe_2016$시도코드 <- as.factor(dataframe_2016$시도코드)



par(mfrow = c(4,2))

for (i in 1:15){
  raw <- df.list[[i]]
  counts <- table(raw$성별코드)
  barplot(counts, col = c("blue", "red"), main = unique(df.list[[i]]$기준년도))
}

par(mfrow = c(3,1))

for (i in 1:15) {
  raw <- df.list[[i]]
  counts <- table(raw$요양일수)
  barplot(counts, main = paste0(unique(df.list[[i]]$기준년도), "년 요양일수"))
}


for (i in 1:15) {
  raw <- df.list[[i]]
  counts <- table(raw$요양일수)
  counts <- counts[counts !=max(counts)]
  barplot(counts, main = paste0(unique(df.list[[i]]$기준년도), "년 요양일수"))
}


for (i in 1:15) {
  raw <- df.list[[i]]
  counts <- table(raw$요양일수)
  barplot(log(counts), main = paste0(unique(df.list[[i]]$기준년도), "년 요양일수"))
}



for (i in 1:15) {
  raw <- df.list[[i]]
  density <- density(raw$요양일수)
  plot(density, main = paste0(unique(df.list[[i]]$기준년도), "년 요양일수 밀도"))
}

# 참고 
x <- seq(0, 10, length = 1000)
plot(x, dgamma(x, 2, 1), type = 'l', ylim = c(0, 0.6), main = 'gamma distribution')

plot(x, dnorm(x, 3), type = 'l', ylim = c(0, 0.6), main = 'normal distribution')
plot(x, dunif(x, 2, 6), type = 'l', ylim = c(0, 0.6), main = 'uniform distribution')
plot(x, dexp(x, 0.5), type = 'l', ylim = c(0, 0.6), main = 'exponential distribution')


########################################################################################

library(ggplot2)
library(dplyr)

# sessionInfo()
# library(extrafont)
# Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8")
# theme_set(theme_gray(base_family="AppleGothic"))
# par(family = "AppleGothic")


raw <- dataframe_2016 # 12,540,347 obs. of 19 variables

set.seed(1234)
raw <- raw[sample(10000),]

raw$성별코드 <- as.factor(raw$성별코드)
raw$연령대코드 <- as.factor(raw$연령대코드)
raw$시도코드 <- as.factor(raw$시도코드)


# library(extrafont) # 맥에서 ggplot2 한글 깨짐 해결방법
# font_import()

ggplot(raw, aes(연령대코드, 요양일수)) +
  geom_point(aes(colour= 성별코드)) + ggtitle("연령대별 요양일") +
  theme_set(theme_gray(base_family = 'NanumGothic'))


##########

df <- select(raw, -c(기준년도, 가입자일련번호, 진료내역일련번호, 서식코드, 요양개시일자, 
                         데이터기준일자, 주상병코드, 부상병코드))


# categorization 항목 변경
levels(df$시도코드) <- c("서울", "부산", "대구", "인천", "광주", "대전", "울산", "세종",
                     "경기도", "강원도", "충북", "충남", "전북", "전남", "경북", "경남", "제주")
levels(df$연령대코드) <- c("4", "9", "14", "19", "24",
                              "29", "34", "39", "44", "49", 
                              "54", "59", "64", "69", "74", 
                              "78", "84", "85")
levels(df$성별코드) <- c("남자", "여자")

df$진료과목코드 <- as.factor(df$진료과목코드)

user_manual <- data.frame(code_num = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9,
                                  10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 
                                  20, 21, 22, 23, 24, 25, 26, 
                                  50, 51, 52, 53, 54, 55, 56, 57, 58, 58, 
                                  80, 81, 82, 83, 84, 85, 86, 87, 88, "ZZ", "-"))

user_manual$code_name <-  c("일반의", "내과", "신경과", "정신과", "외과", "정형외과", 
                            "신경외과", "흉부외과", "성형외과", "마취통증의학과", "산부인과", 
                            "소아청소년과", "안과", "이비인후과", "피부과", "비뇨기과", 
                            "영상의학과", "방사선종야학과", "병리과", "진단검사의학과", "결핵과",
                            "재활의학과", "핵의학과", "가정의학과", "응급의학과", "산업의학과",
                            "예방의학과", "구강악안면외과", "치과보철과", "치과교정과", "소아치과", "치주과",
                            "치과보존과", "구강내과", "구강악안면방사선과", "구강병리과", "예방치과", "한방내과",
                            "한방부인과", "한방소아과", "한방안과,이비인", "한방신경정신과", "침구과", 
                            "한방재활의학과","사상체질과", "한방응급", "결측", "정상")



levels(df$진료과목코드) <- user_manual$code_name[user_manual$code_num %in% levels(df$진료과목코드)]

head(user_manual)
tail(user_manual)


par(mfrow = c(2,1))
hist(df$심결보험자부담금, main = '심결보험자부담금 히스토그램', xlab = '', col = 'blue')
plot(density(df$심결보험자부담금), main = '심결보험자부담금 Probability density function', col = "blue", xlim = c(0, 1000000))



# log transformation
df$심결요양급여비용총액 <- log(df$심결요양급여비용총액+1)
df$심결본인부담금 <- log(df$심결본인부담금+1)
df$심결보험자부담금 <- log(df$심결보험자부담금+1)

hist(df$심결보험자부담금,
     main = '로그변환 심결보험자부담금 히스토그램', xlab = '' ,
     col = 'red')
plot(density(df$심결보험자부담금), 
     main = '로그변환 심결보험자부담금 Probability density function',
     col = "red")




df.male <- df %>% filter(성별코드 == "남자") %>% select(심결보험자부담금) %>% unlist()
df.female <-  df %>% filter(성별코드 == "여자") %>% select(심결보험자부담금) %>% unlist()


par(mfrow = c(2,1))
hist(df.male, xlim = c(0, 18), ylim = c(0, 40000), 
     main = "(성별) 심결보험자부담금 히스토그램",
     col = rgb(0.1, 0.1, 0.9, 0.5))
hist(df.female, add = T, col = rgb(0.9, 0.1, 0.1, 0.5))

plot(density(df.male), col = 'blue', ylim = c(0, 3),
     main = '(성별) 심결보험자부담금 확률밀도함수')
lines(density(df.female), col = 'red')


par(mfrow = c(1,1))
df.Seoul <- df %>% filter(시도코드 == "서울") %>% select(심결보험자부담금) %>% unlist()
plot(density(df.Seoul), col = 1, ylim = c(0, 2.5), main = '(시도별) 심결보험자부담금 확률밀도함수')

for (i in 2:length(levels(df$시도코드))) {
  df %>% 
    filter(시도코드 == levels(df$시도코드)[i]) %>% 
    select(심결보험자부담금) %>% 
    unlist() %>%
    density() %>%
    lines(col = i)
}

plot(density(df.Seoul), col = 1, ylim = c(0, 2.5), main = '(시도별) 심결보험자부담금 확률밀도함수')
df %>% 
  filter(시도코드 == levels(df$시도코드)[7]) %>% 
  select(심결보험자부담금) %>% 
  unlist() %>% 
  density() %>% 
  lines(col="red")


# rm(list = ls()[!ls() %in% "df"])




set.seed(1234)
samples <- sample.split(df$시도코드, SplitRatio = .70)
df.train <- subset(df, samples == TRUE)
df.test <- subset(df, samples == FALSE)


m <- randomForest(시도코드 ~ 성별코드 + 연령대코드 + 심결보험자부담금 +  진료과목코드 + 
                        요양일수 + 입내원일수 +  총처방일수, 
                      data = df.train, importance = TRUE, 
                      ntree = 500, mtry = 2, do.trace = 100)
# ntree = 500 : 500 trees will be trained
# mtry=2: 2 features is chosen for each iteration

prediction <- predict(m, newdata = df.test)
res <- confusionMatrix(prediction, df.test$시도코드)
res$table

sum(diag(res$table))/sum(res$table)

imp <- m$importance
varImpPlot(m)








