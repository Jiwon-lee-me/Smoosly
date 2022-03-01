setwd('C:\\Users\\jiwon.DESKTOP-J6K6P2J\\Desktop\\스무슬리')
BR_NUM_TB = read.csv('기존 브라 최종 수치_노션.csv')
BR_NUM_INFO = read.csv('기존 브라 최종 수치_실측.csv')
View(BR_NUM_TB)
View(BR_NUM_INFO)
# View(BR_NUM_TB)
# View(BR_NUM_INFO)
# View(BR_NUM_TB_PUR)
attach(BR_NUM_TB_PUR)

library(dplyr)
# attach(BR_NUM_TB)

# CATEGORY별 재고 확인
BR_NUM_TB %>% 
  group_by(CATEGORY) %>% 
  summarise(n = n())

BR_NUM_TB_PUR %>% 
  group_by(분류) %>% 
  summarise(n = n())


# install.packages('sqldf')
library(sqldf)

BR_NUM_TOT = sqldf('SELECT A.PK_ITEM, A.OLD_KEY, A.CATEGORY, 
                   B.BT_LEN, B.LOWER_LEN, B.INNER_LEN, B.OUTER_LEN 
                   FROM BR_NUM_TB A
                   JOIN BR_NUM_INFO B
                   ON A.PK_ITEM = B.PK_ITEM AND A.OLD_KEY = B.OLD_KEY')

a = list()
a = append(a, 'a')

for (num in 1:49) {
  if (BR_NUM_TB[num, 2] = BR_NUM_INFO[num, 2]) {
    a = append(a, as.character(BR_NUM_TB[num, 2]))
  }
}

# View(BR_NUM_TOT)
nrow(BR_NUM_INFO)
nrow(unique(BR_NUM_INFO))

BR_NUM_TOT_4 = sqldf('SELECT A.PK_ITEM, A.OLD_KEY, A.CATEGORY, 
                   B.BT_LEN, B.LOWER_LEN, B.INNER_LEN, B.OUTER_LEN 
                   FROM BR_NUM_INFO A 
                   JOIN BR_NUM_TB B 
                   ON A.PK_ITEM = B.PK_ITEM AND A.OLD_KEY = B.OLD_KEY 
                     WHERE CATEGORY = 4')
# View(BR_NUM_TOT_4)

BR_NUM_TB_PUR = read.csv('BR_NUM_TB_NEW.csv')
# View(BR_NUM_TB_PUR)
attach(BR_NUM_TB_PUR)
#===================
#if (사이즈 ='75A' | 사이즈 = '70B' | 사이즈 = '65C'){
#  분류 = 2
#}
#  else if (사이즈 = '80A' | 사이즈 = '75B' | 사이즈 = '70C'){
#    분류 = 3
#  }
#    else if (사이즈 = '85A' | 사이즈 = '80B' | 사이즈 ='75C' | 사이즈 = '70D'){
#      분류 = 4
#    }
#      else if (사이즈 = '90A' | 사이즈 = '85B' | 사이즈 = '80C' | 사이즈 = '75D' | 사이즈 = '70E'){
#        분류 = 5
#      }
#        else if (사이즈 = '95A' | 사이즈 = '90B' | 사이즈 = '85C' | 사이즈 = '80D' | 사이즈 = '75E'){
#          분류 = 6
#}
#          else{
#            분류 = 0
#        }
#=======================

# install.packages('caret')
library(caret)
set.seed(1712)
# split
inTrain = createDataPartition(y = c(2, 3, 4, 5),
                              p = 0.7,
                              list = F)
test = BR_NUM_INFO[inTrain,]
train = BR_NUM_INFO[-inTrain,]

nrow(train)
nrow(test)
#...

# standardize
train.data = scale(train[4:7])
summary(train.data)
head(train.data)
# modeling, k = 3
BR_NUM_INFO.kmeans = kmeans(train.data[,-4], centers = 3, iter.max = 1000)
BR_NUM_INFO.kmeans$centers

train$cluster = as.factor(BR_NUM_INFO.kmeans$cluster)
qplot(BT_LEN, LOWER_LEN, colour = cluster, data = train)
table(train$CATEGORY, train$cluster)


inTrain = createDataPartition(y = BR_NUM_TOT$CATEGORY,
                              p = 0.7,
                              list = F)
train = BR_NUM_TOT[inTrain,]
test = BR_NUM_TOT[-inTrain,]

nrow(train)
nrow(test)
#...

# standardize
train.data = scale(train[4:7])
summary(train.data)
head(train.data)
# modeling, k = 3
BR_NUM_TOT.kmeans = kmeans(train.data[,-5], centers = 3, iter.max = 1000)
BR_NUM_TOT.kmeans$centers

train$cluster = as.factor(BR_NUM_TOT.kmeans$cluster)
qplot(BT_LEN, LOWER_LEN, colour = cluster, data = train)
table(train$CATEGORY, train$cluster)

BR_NUM_TB = read.csv('BR_NUM_TB.csv')
BR_NUM_TB_OD = order(BR_NUM_TB$PK_ITEM)
View(BR_NUM_TB_OD)

bra = read.csv('bra_data.csv')
View(bra)
