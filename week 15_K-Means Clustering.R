
# 교수님의 잠재계층분석 논문을 K-클러스터링 해본 실습

# Package for cluster validity
install.packages("clValid")
install.packages("plotrix")
library(dplyr)
library(clValid)
library(plotrix)

# Part 1: K-Means Clustering ----------------------------------------------
# Load the powerline dataset
powerline <- read.csv("powerline.csv")
# x1 ~ x10 말씀해주셨던 변수 (설문조사)

# Remove the class label
powerline_class <- powerline[,1]
powerline_x <- powerline[,-c(1,12,13,14)]
# 4개의 변수를 제거하고 클러스할 때 쓸 변수 10개만 남겨놓음

# data scaling
# powerline_x_scaled <- scale(powerline_x, center = TRUE, scale = TRUE)
powerline_x_scaled <- powerline_x 
# 원래대로하면 정규화를 해주어야함 (소득, 나이와 같은 변수였다면)

# Evaluating the cluster validity measures
# 2~10개까지 클러스터링
powerline_clValid <- clValid(powerline_x_scaled, 2:10, clMethods = "kmeans", 
                           validation = c("internal", "stability"))
summary(powerline_clValid) # 타당성 지표들 확인 됨
#실루엣 = 2, 던 = 8일때 가장 큼 -> 맨 밑에 집계됨
# 엇갈린 결과가 나오는데 어떡해야하????
# 앞에 나오는 타당성 지표를 보면 2가 가장 빈도수가 높다.
# 따라서 2개의 그룹으로 클러스터링 실시

# Perform K-Means Clustering with the best K determined by Silhouette
# 2개로 클러스팅하라는 함수 
powerline_kmc <- kmeans(powerline_x_scaled,2)

str(powerline_kmc)
powerline_kmc$centers
powerline_kmc$size
powerline_kmc$cluster # 1번부터 720까지 몇번 클러스터에 포함 되었는지

# Assign the cluster info
powerline$class <- powerline_kmc$cluster # 클러스터링 분류 값 파생변수 추가

# Class 별 나이와 가구소득 평균

powerline %>% 
  group_by(class) %>% 
  summarise(age_mean = mean(age),
            income_mean = mean(income_h))
# 첫번째 집단은 문제인식 등 별로 크게 느끼지 않는다
# 두번째 집단은 문제인식 등이 다 높다.

# class 별 성별 비율 1=남성, 2= 여성
powerline %>% 
  group_by(gender,class) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>%
  mutate(pct=n/tot_group*100)

## 마지막으로 t검정으로 집단 간 평균비교 가능 (통계분석실무)
