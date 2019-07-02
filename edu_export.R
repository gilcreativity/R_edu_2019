rm(list = ls())
setwd("/Volumes/Samsung_T5/NIA_PublicData")


install.packages("ggplot2")
install.packages("dplyr")
install.packages("zoo")
install.packages("corrplot")
install.packages("tseries")
install.packages("forecast")

library(ggplot2)
library(dplyr)
library(zoo)
library(corrplot)
library(tseries)
library(forecast)



# Import Data


raw <- read.table("KOTRA/대한무역투자진흥공사 수출선행지수 추이(2009년 4분기~2019년2분기).csv",
                  fileEncoding = "euc-kr", header = T, sep = ",")

View(raw)

head(raw)

tail(raw)

str(raw)


# Handling data

# Convert character to date

library(dplyr)
library(zoo)

qt <- do.call(paste, c(raw[c("년도", "분기")], sep = "-"))
qt <- as.yearqtr(qt, format = "%Y-%q")
df <- cbind(qt, select(raw, -c("번호", "년도", "분기")))

str(df)
tail(df)



colSums(is.na(df))


# replace missing values with column mean
for (i in 1: ncol(df)){
  df[is.na(df[,i]), i] <- round(mean(df[,i], na.rm = TRUE), digits = 2)
}




  

  
# Correlogram :Visualing the correlation matrix 
# Seven different visualization methods can be used : “circle”, “square”, “ellipse”, “number”, “shade”, “color”, “pie”.

par(family="NanumGothic")  # 맥의 경우 corrplot에서 글짜깨지는 경우

library(corrplot)

df.cor <- cor(select(df, -qt))

corrplot(df.cor, method = "circle", type = 'upper', order = "hclust", tl.col = "black", main = "")




library(extrafont) # 맥에서 ggplot에서 한글깨질 경우
font_import(pattern = 'NanumGothic')

sort(df.cor[,"수출선행지수"], decreasing = TRUE)

theme_update(text=element_text(family="NanumGothic")) # ggplot2가 만든 그래프의 폰트 변경
ggplot() + 
  geom_line(data = df, aes(x = qt, y = 수출선행지수), color = 'red') + 
  geom_line(data = df, aes(x = qt, y =중국선행지수), color = 'blue') +
  ggtitle("무역투자진흥공사 지수자료")
 

# 국내 수출 선행지수가 중국선행지수와 상관관계가 가장 높음 
# : 실제 국내수출중 대중수출이 차지하는 비율이 가장 높음
# 참고) 한국무역협회 : http://stat.kita.net, 한국무역 주요 국가

p <- ggplot(data = df, aes(x = qt, y = 수출선행지수)) + 
  geom_line(color = "#00AFBB", size = 1) +
  ggtitle("무역투자진흥공사 지수자료")

# Add trend smoothed line
p + stat_smooth(
  color = "#FC4E07", fill = "#FC4E07", 
  method = "loess"
)


library(tseries)

df.ts <- ts(df[nrow(df):1,], start=c(2009, 4), end=c(2019, 2), frequency = 4)

export <- df.ts[,colnames(df.ts) == "수출선행지수"] 
export %>% decompose() %>% plot()


# stationary check for time-series analysis
adf.test(export) # p-value = 0.4095 : non stationary

# making stationary time series
diff_export <- diff(log(export))
plot(diff_export, main = "수출선행지수 변화율(분기)", col = "blue", xlab = "", ylab = "")
acf(diff_export)
pacf(diff_export)
ar(diff_export, method = "ols", 1)


# check stationary 
adf.test(diff_export) # p-value = 0.09808 : stationary

# simple example of time series : Arima 
library(forecast)

fit <- auto.arima(export)
summary(fit)
plot(forecast(fit, h=5), main = "수출선행지수 5분기 예측")

par(mfrow = c(1,1))

class(export)


