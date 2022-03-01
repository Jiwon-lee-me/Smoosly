setwd('C:\\Users\\jiwon.DESKTOP-J6K6P2J\\Desktop\\스무슬리')
BR_NUM_TB = read.csv('BR_NUM_TB.csv')
BR_NUM_INFO = read.csv('BR_NUM_INFO.csv')
BR_NUM_TB_PUR = read.csv('BR_NUM_TB_PUR.csv')
# View(BR_NUM_TB)
# View(BR_NUM_INFO)
View(BR_NUM_TB_PUR)
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

install.packages('sqldf')
library(sqldf)

BR_NUM_TOT = sqldf('SELECT A.PK_ITEM, A.OLD_KEY, A.CATEGORY, 
                   B.BT_LEN, B.LOWER_LEN, B.INNER_LEN, B.OUTER_LEN 
                   FROM BR_NUM_INFO A 
                   JOIN BR_NUM_TB B 
                   ON A.PK_ITEM = B.PK_ITEM AND A.OLD_KEY = B.OLD_KEY')
# View(BR_NUM_TOT)
nrow(BR_NUM_TOT)
nrow(unique(BR_NUM_TOT))

BR_NUM_TOT_4 = sqldf('SELECT A.PK_ITEM, A.OLD_KEY, A.CATEGORY, 
                   B.BT_LEN, B.LOWER_LEN, B.INNER_LEN, B.OUTER_LEN 
                   FROM BR_NUM_INFO A 
                   JOIN BR_NUM_TB B 
                   ON A.PK_ITEM = B.PK_ITEM AND A.OLD_KEY = B.OLD_KEY 
                     WHERE CATEGORY = 4')
View(BR_NUM_TOT_4)

BR_NUM_TB_PUR = read.csv('BR_NUM_TB_NEW.csv')
View(BR_NUM_TB_PUR)
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
