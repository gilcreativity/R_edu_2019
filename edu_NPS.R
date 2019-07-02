rm(list = ls())

setwd("/Volumes/Samsung_T5/NIA_PublicData")

install.packages("dplyr")
install.packages("ggplot2")
install.packages("extrafont")

library(dplyr)
library(ggplot2)
library(ggrepel)
library(extrafont)

# �ƿ��� ggplot���� �ѱ۱��� ���
font_import()


raw <- read.table("NPS/���ο��� ���� ����� ���� 2019�� 4��.csv",
                  fileEncoding = "euc-kr", header = T, sep = ",")

View(raw)
head(raw)
tail(raw)

raw.names <- raw %>% colnames()
raw.names <- gsub('[[:digit:]]+', '', raw.names)
raw.names <- gsub('[a-zA-Z._]', '', raw.names)

colnames(raw) <- raw.names

str(raw)


df <- filter(raw, raw$����尡�Ի����ڵ���Ż�� == 1)


# raw.names
# c("�ڷ�������", "������", "����ڵ�Ϲ�ȣ", "����尡�Ի����ڵ���Ż��", "������ȣ", 
#   "������������ּ�", "����嵵�θ����ּ�", "�����������ּ��ڵ�", "�����������ּ��ڵ�", 
#   "�������ּұ����õ��ڵ�", "�������ּұ����ýñ����ڵ�", 
#   "�������ּұ����ýñ������鵿�ڵ�","��������±����ڵ���ΰ���", "���������ڵ屹��û�����ڵ�����", 
#   "���������ڵ��", "��������", "��������", "Ż������", "�����ڼ�", 
#   "��������ݾ�", "�ű�����ڼ�", "��ǰ����ڼ�")

head(raw)
tail(raw)

c("������", 
  "������������ּ�", "����嵵�θ����ּ�", 
  "�������ּұ����õ��ڵ�", "�������ּұ����ýñ����ڵ�", "�������ּұ����ýñ������鵿�ڵ�",
  "���������ڵ��", "��������", "��������", "Ż������", "�����ڼ�", 
  "��������ݾ�", "�ű�����ڼ�", "��ǰ����ڼ�")



df <- select(df, raw.names[raw.names %in% c("������", "������������ּ�",
                                            "���������ڵ��", "�����ڼ�", 
                                            "��������ݾ�", "�ű�����ڼ�", "��ǰ����ڼ�")])


head(df)
tail(df)

# unique(df$������������ּ�)

�����ּ� <- "������"

df.subset <- df[grep(�����ּ�, df$������������ּ�), ] 
df.split.list <- df.subset$������������ּ� %>% as.character() %>% strsplit(' ')
df.split <- do.call(rbind, df.split.list)
df.split <- as.data.frame(df.split) 
colnames(df.split) <- c("��", "��", "��")

df.subset <- cbind(df.split, df.subset[,colnames(df.subset) != "������������ּ�"])

head(df.subset)

# ���� ����

df.subset$net <- df.subset$�ű�����ڼ� - df.subset$��ǰ����ڼ�

df.subset$���� <- ifelse(df.subset$�����ڼ� < 5, "5�̸�", 
                       ifelse(df.subset$�����ڼ� < 10, "10�̸�", 
                              ifelse(df.subset$�����ڼ� < 50, "50�̸�", 
                                     ifelse(df.subset$�����ڼ� < 100, "100�̸�",
                                            ifelse(df.subset$�����ڼ� < 1000, "1000�̸�","1000�̻�")))))


# df.subset$���� <- ifelse(df.subset$�����ڼ� < 5, "10�̸�", 
#                        ifelse(df.subset$�����ڼ� < 100, "100�̸�",
#                                             ifelse(df.subset$�����ڼ� < 1000, "1000�̸�","1000�̻�")))



head(df.subset)




library(ggplot2)

ggplot(data = df.subset, aes(x = ��, y=net )) +
  geom_boxplot(alpha = 0) + 
  geom_jitter(alpha = 0.6, color = "tomato") + 
  ggtitle("�����谡���ڼ�")

head(df.subset)


ggplot(data = df.subset, aes(x= ��, y = net, fill = factor(����))) +
  geom_boxplot() + 
  ggtitle("���ԱԸ� �����谡���ڼ�")


ggplot(data = df.subset, aes(x=��, y=net, size = �����ڼ�, color = ��)) +
  geom_point(alpha = 0.6) + 
  ggtitle("���ԱԸ� �����谡���ڼ�") + 
  xlab("") + ylab("�������ڰ��Լ�")
  


ggplot(data = df.subset, aes(x=��, y=net, size = �����ڼ�, label = �����ڼ�)) +
  geom_point(alpha = 0.6) + 
  geom_point(data = df.subset[df.subset$�� != "������2��",], color = 'grey') +
  geom_point(data = df.subset[df.subset$�� == "������2��",], color = 'red') +
  geom_point(data = df.subset[df.subset$�� == "������1��",], color = 'pink') +
  ggtitle("���ԱԸ� �����谡���ڼ�") + 
  xlab("") + ylab("�������ڰ��Լ�") +
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









