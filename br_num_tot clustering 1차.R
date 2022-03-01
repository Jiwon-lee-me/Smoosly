setwd('C:\\Users\\jiwon.DESKTOP-J6K6P2J\\Desktop\\스무슬리')
BR_NUM_TB = read.csv('기존 브라 수치_노션.csv')
BR_NUM_INFO = read.csv('BR_NUM_INFO.csv')
BR_NUM_PUR = read.csv('1차 새로 구매한 브라 리스트.csv')
# View(BR_NUM_TB)
# View(BR_NUM_INFO)
# View(BR_NUM_PUR)
# View(BR_NUM_TB)
# View(BR_NUM_INFO)
# View(BR_NUM_TB_PUR)
View(BR_NUM_TOT)
attach(BR_NUM_TB_PUR)

library(dplyr)
# attach(BR_NUM_TB)

# CATEGORY별 재고 확인
# ======
TB_OF_NUM = BR_NUM_TB %>% 
  group_by(CATEGORY) %>% 
  summarise(n = n())

TB_OF_PUR = BR_NUM_PUR %>% 
  group_by(분류) %>% 
  summarise(n = n())
#==========


# install.packages('sqldf')
library(sqldf)

BR_NUM_TOT = sqldf('SELECT A.PK_ITEM, A.OLD_KEY, A.CATEGORY, B.BT_LEN, B.LOWER_LEN, B.INNER_LEN, B.OUTER_LEN 
                   FROM BR_NUM_TB A
                   JOIN BR_NUM_INFO B
                   ON A.PK_ITEM = B.PK_ITEM AND A.OLD_KEY = B.OLD_KEY')

#===============OLD_KEY 다른지 체크하는 코드=============
a = list()
# a = append(a, 'a')

for (num in 1:95) {
  if (sort(BR_NUM_INFO[num, 2]) != sort(BR_FEATURE_ONEHOT[num, 2])) {
    a = append(a, as.character(BR_NUM_INFO[num, 2]))
  }
}

b = list()

for (num in 1:95) {
  if (sort(BR_NUM_INFO[num, 1]) != sort(BR_FEATURE_ONEHOT[num, 1])) {
    b = append(b, as.character(BR_NUM_INFO[num, 2]))
  }
}

BR_FEATURE_ONEHOT[23, 2] = 'FS'
View(BR_FEATURE_ONEHOT)
a = as.matrix(a)
class(a)
b = as.data.frame(b)
PRAUD = cbind(a, b)
View(a)
#===============
a
# View(BR_NUM_TOT)
View(BR_NUM_TOT)
nrow(BR_NUM_TOT)
nrow(unique(BR_NUM_TOT))


attach(BR_NUM_TOT)
lm1 = lm(log(CATEGORY) ~ log(BT_LEN) + log(OUTER_LEN) + log(INNER_LEN))
summary(lm1)

plot(OUTER_LEN, BT_LEN)
# View(BR_NUM_TOT_4)
# View(BR_NUM_TB_PUR)
attach(BR_NUM_PUR)

class(사이즈)
SIZE = 사이즈
#=================== 새로 구매한 브라 분류체계 할당하는 코드=============
for (i in 1:35){
  if (BR_NUM_PUR[i, 3] == '75A' | BR_NUM_PUR[i, 3] == '70B' | BR_NUM_PUR[i, 3] == '65C'){
    BR_NUM_PUR[i, 4] = 2
  }else if (BR_NUM_PUR[i, 3] == '80A' | BR_NUM_PUR[i, 3] == '75B' | BR_NUM_PUR[i, 3] == '70C') {
    BR_NUM_PUR[i, 4] = 3
  }else if (BR_NUM_PUR[i, 3] == '85A' | BR_NUM_PUR[i, 3] == '80B' | BR_NUM_PUR[i, 3] == '75C' | BR_NUM_PUR[i, 3] == '70D'){
    BR_NUM_PUR[i, 4] = 4
  }else if (BR_NUM_PUR[i, 3] == '90A' | BR_NUM_PUR[i, 3] == '85B' | BR_NUM_PUR[i, 3] == '80C' | BR_NUM_PUR[i, 3] == '75D' | BR_NUM_PUR[i, 3] == '70E'){
    BR_NUM_PUR[i, 4] = 5
  }else {
    BR_NUM_PUR[i, 4] = 0
  }
}
#=======================


# install.packages('caret')
library(caret)
set.seed(1712)
# split
inTrain = createDataPartition(y = BR_NUM_TOT$CATEGORY,
                              p = 0.8,
                              list = F)
train = BR_NUM_INFO[inTrain,]
test = BR_NUM_INFO[-inTrain,]

nrow(train)
nrow(test)
#...

# standardize
train.data = scale(train[3:6])
summary(train.data)
head(train.data)
# modeling, k = 3
BR_NUM_TOT.kmeans = kmeans(train.data[,-5], centers = 3, iter.max = 1000000)
BR_NUM_TOT.kmeans$centers

train$cluster = as.factor(BR_NUM_TOT.kmeans$cluster)
qplot(OUTER_LEN, INNER_LEN, colour = cluster, data = train)
table(train$CATEGORY, train$cluster)
#...
head(train)


# write tb
write.csv(BR_NUM_TOT,"BR_NUM_INFO.csv", row.names = FALSE)
