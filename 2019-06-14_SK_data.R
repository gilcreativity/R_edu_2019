rm(list = ls())
setwd("~/Documents/R_work")

library(data.table)

raw <- fread("201603_SKT_Sales.csv")
colnames(raw)

# Description
###############################
# ▶STD_YM : 기준년월  <- 201603
# ▶SIDO_NM : 시도명 <- 서울특별시
# ▶SGNG_NM : 시군구명 <- 종로구
# ▶ADONG_NM : 행정동명 <- 17
# ▶BLOCK_CD : 소지역코드 <- 3
# ▶SCLS_NM : 소분류업종명
# ▶MCLS_NM : 중분류업종명
# ▶LCLS_NM : 대분류업종명
# ▶SALE_AMT_00TMST : 00시 카드매출
# ▶APV_CNT_00TMST : 00시 승인건수
# ▶PRICE_00TMST : 00시 매출단가
# ▶DATA_LOAD_DT : 데이터적재일시 <- 2016-05-04 오후 6:20:55
###############################


head(raw)
summary(raw)

par(family = "AppleMyungjo")

library(DataExplorer)
plot_str(raw)
plot_intro(raw)
plot_missing(raw)
plot_bar(raw)

colnames(raw)
raw <- raw[,-c("STD_YM", "SI_DO_NM","SGNG_NM","DATA_LOAD_DT")]
raw$ADONG_NM <- as.factor(raw$ADONG_NM)
raw$BLOCK_CD <- as.factor(raw$BLOCK_CD)


plot_str(raw)
plot_intro(raw)
plot_missing(raw)
plot_bar(raw$ADONG_NM, title = "Frequency of Purchasing",
         ggtheme = theme_grey(base_family = "AppleMyungjo"))


# do.call(paste0, raw[,c("X_COORD", "Y_COORD")])
library(tidyr)

raw <- raw %>% 
  unite(X_Y, X_COORD, Y_COORD, sep = "_", remove = TRUE)

df <- melt(raw, id=c("ADONG_NM", "BLOCK_CD", "X_Y"))
df$X_Y <- as.factor(df$X_Y)

plot_str(df)
plot_intro(df)
plot_missing(df)
plot_bar(df, ggtheme = theme_grey(base_family = "AppleMyungjo"))


df_group <- df %>%
  group_by(ADONG_NM) %>% summarize(total =sum(value))

addr_list <- unique(df$ADONG_NM)

barplot(total ~ ADONG_NM, 
        data=df_group,
        xlab = "", ylab = "",
        col=rainbow(length(addr_list)),
        axes=T, beside=T, las = 2)

df_block <- df %>% group_by(BLOCK_CD) %>% summarize(total =sum(value))

block_list <- unique(df$BLOCK_CD)
barplot(total ~ BLOCK_CD, 
        data=df_block,
        xlab = "", ylab = "",
        col=rainbow(length(block_list)),
        axes=T, beside=T, las = 2)


unique(filter(df, BLOCK_CD == block_list[1])[,1])
unique(filter(df, BLOCK_CD == block_list[2])[,1])
unique(filter(df, BLOCK_CD == block_list[3])[,1])


#################


load(file = "./kormaps2014-master/data/korpop3.rda")
load(file = "./kormaps2014-master/data/kormap3.rda")

tmp <- kormap3[grep(11010, kormap3$adm_dr_cd ), ]
tmp_pop <- korpop3[grep(11010, as.numeric(korpop3$code)),]
addr_pop <- data.frame(addr_list = as.character(tmp_pop$행정구역별_읍면동),pop =  as.numeric(tmp_pop$총인구_명))
addr_pop <- addr_pop[order(addr_pop$pop, decreasing = TRUE),]



ggplot(tmp_pop,aes(map_id=code,fill=총인구_명))+
  geom_map(map=tmp,colour="black",size=0.1)+
  expand_limits(x=tmp$long,y=tmp$lat)+
  scale_fill_gradientn(colours=c('white','orange','red'))+
  ggtitle("2015년도 서울시 종로구 읍면동별 인구분포도")+
  coord_map()


barplot(total ~ ADONG_NM, 
        data=df_group,
        xlab = "", ylab = "",
        col=rainbow(length(addr_list)),
        axes=T, beside=T, las = 2)

barplot(pop ~ addr_list,
        data = addr_pop,
        xlab = "", ylab = "",
        col= rainbow(length(addr_list)),
        axes=T, beside=T, las = 2)



