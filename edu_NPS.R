rm(list = ls())

setwd("/Volumes/Samsung_T5/NIA_PublicData")

install.packages("dplyr")
install.packages("ggplot2")
install.packages("extrafont")

library(dplyr)
library(ggplot2)
library(ggrepel)
library(extrafont)

# 맥에서 ggplot에서 한글깨질 경우
font_import()


raw <- read.table("NPS/국민연금 가입 사업장 내역 2019년 4월.csv",
                  fileEncoding = "euc-kr", header = T, sep = ",")

View(raw)
head(raw)
tail(raw)

raw.names <- raw %>% colnames()
raw.names <- gsub('[[:digit:]]+', '', raw.names)
raw.names <- gsub('[a-zA-Z._]', '', raw.names)

colnames(raw) <- raw.names

str(raw)


df <- filter(raw, raw$사업장가입상태코드등록탈퇴 == 1)


# raw.names
# c("자료생성년월", "사업장명", "사업자등록번호", "사업장가입상태코드등록탈퇴", "우편번호", 
#   "사업장지번상세주소", "사업장도로명상세주소", "고객법정동주소코드", "고객행정동주소코드", 
#   "법정동주소광역시도코드", "법정동주소광역시시군구코드", 
#   "법정동주소광역시시군구읍면동코드","사업장형태구분코드법인개인", "사업장업종코드국세청업종코드참조", 
#   "사업장업종코드명", "적용일자", "재등록일자", "탈퇴일자", "가입자수", 
#   "당월고지금액", "신규취득자수", "상실가입자수")

head(raw)
tail(raw)

c("사업장명", 
  "사업장지번상세주소", "사업장도로명상세주소", 
  "법정동주소광역시도코드", "법정동주소광역시시군구코드", "법정동주소광역시시군구읍면동코드",
  "사업장업종코드명", "적용일자", "재등록일자", "탈퇴일자", "가입자수", 
  "당월고지금액", "신규취득자수", "상실가입자수")



df <- select(df, raw.names[raw.names %in% c("사업장명", "사업장지번상세주소",
                                            "사업장업종코드명", "가입자수", 
                                            "당월고지금액", "신규취득자수", "상실가입자수")])


head(df)
tail(df)

# unique(df$사업장지번상세주소)

지번주소 <- "성동구"

df.subset <- df[grep(지번주소, df$사업장지번상세주소), ] 
df.split.list <- df.subset$사업장지번상세주소 %>% as.character() %>% strsplit(' ')
df.split <- do.call(rbind, df.split.list)
df.split <- as.data.frame(df.split) 
colnames(df.split) <- c("시", "구", "동")

df.subset <- cbind(df.split, df.subset[,colnames(df.subset) != "사업장지번상세주소"])

head(df.subset)

# 변수 생성

df.subset$net <- df.subset$신규취득자수 - df.subset$상실가입자수

df.subset$구간 <- ifelse(df.subset$가입자수 < 5, "5미만", 
                       ifelse(df.subset$가입자수 < 10, "10미만", 
                              ifelse(df.subset$가입자수 < 50, "50미만", 
                                     ifelse(df.subset$가입자수 < 100, "100미만",
                                            ifelse(df.subset$가입자수 < 1000, "1000미만","1000이상")))))


# df.subset$구간 <- ifelse(df.subset$가입자수 < 5, "10미만", 
#                        ifelse(df.subset$가입자수 < 100, "100미만",
#                                             ifelse(df.subset$가입자수 < 1000, "1000미만","1000이상")))



head(df.subset)




library(ggplot2)

ggplot(data = df.subset, aes(x = 동, y=net )) +
  geom_boxplot(alpha = 0) + 
  geom_jitter(alpha = 0.6, color = "tomato") + 
  ggtitle("순보험가입자수")

head(df.subset)


ggplot(data = df.subset, aes(x= 동, y = net, fill = factor(구간))) +
  geom_boxplot() + 
  ggtitle("가입규모별 순보험가입자수")


ggplot(data = df.subset, aes(x=동, y=net, size = 가입자수, color = 동)) +
  geom_point(alpha = 0.6) + 
  ggtitle("가입규모별 순보험가입자수") + 
  xlab("") + ylab("순보험자가입수")
  


ggplot(data = df.subset, aes(x=동, y=net, size = 가입자수, label = 가입자수)) +
  geom_point(alpha = 0.6) + 
  geom_point(data = df.subset[df.subset$동 != "성수동2가",], color = 'grey') +
  geom_point(data = df.subset[df.subset$동 == "성수동2가",], color = 'red') +
  geom_point(data = df.subset[df.subset$동 == "성수동1가",], color = 'pink') +
  ggtitle("가입규모별 순보험가입자수") + 
  xlab("") + ylab("순보험자가입수") +
  theme(legend.position = "bottom") + 
  geom_text_repel(
    data = subset(df.subset, net < -20),
    nudge_y = -45 - subset(df.subset, net < -20)$net,
    segment.size = 0.5,
    segment.color = 'grey50',
    direction = 'x'
  )  + 
  geom_text_repel(
    data = subset(df.subset, net > 1),
    nudge_y = 20 - subset(df.subset, net > 1)$net,
    segment.size = 0.5,
    segment.color = 'grey50',
    direction = 'x'
  )










