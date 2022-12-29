# 라이브러리 설치 및 데이터 불러오기 -------------
library(foreign)
library(dplyr)
library(ggplot2)

raw_welfare <- read.spss("data_spss_Koweps2014.sav", to.data.frame = T)
welfare <- raw_welfare

# 데이터 탐색 -----------------------------------
dim(welfare)
str(welfare)
options(max.print = 100000) # 데이터가 너무 많아 출력이 안 될 때 옵션 설정
head(welfare)
summary(welfare)

# 데이터가 많으므로, 필요한 데이터만 추출 ------
## 변수명 변경 (기존 변수명은 코딩북 참고) -----
welfare <- rename (welfare, sex = h0901_4,
                  birth = h0901_5,
                  income = h09_din)


# 성별에 따른 소득 ------------------------------
class(welfare$sex) # 변수타입 확인
summary(welfare$sex) #카테고리컬 데이터는 의미없는 행위
as.factor(welfare$sex) # 따라서 class 변환
table(welfare$sex)

## 9는 결측치 이므로, NA로 변환
welfare$sex <- ifelse(welfare$sex == 9, NA, welfare$sex) 
table(is.na(welfare$sex)) # True -> 결측치 ㅇ

welfare$sex <- ifelse(welfare$sex == 1, "male", "female") 
qplot(welfare$sex)

## 소득
class(welfare$income)
summary(welfare$income)
qplot(welfare$income)+xlim(0, 10000)
table(is.na(welfare$income))

# 개인조사가 아닌 가구조사이기 때문에, 성별에 따른 격차가 큼
# 그래서, 도메인 지식이 중요
welfare %>% group_by(sex) %>%  summarise(mean_income = mean(income))
ggplot(data=income_mean, aes(x=sex, y=mean_income))+geom.col()
# 나이에 따른 소득 -----------------------------

class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)

welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth) 
table(is.na(welfare$bitrh)) # True -> 결측치 ㅇ

# 연령에 따른 소득 -----------------------------
welfare$age <- 2014-welfare$birth + 1
summary(welfare$age)
qplot(welfare$age)

age_income <- welfare %>% group_by(age) %>% summarise((mean_income = mean(income)))
ggplot(data=age_income, aes(x=age, y=mean_income))+geom_col()
ggplot(data=age_income, aes(x=age, y=mean_income))+geom_point()


wlefare <- welfare %>% mutate(ageg = ifelse(age < 30, "young",
                                           ifelse(age <= 59, "middle", "old")))
table(welfare$ageg)
qlot(welfare%ageg)

welfare_mean <- welfare %>% filter(ageg != "young") 
                        %>% group_by(ageg) %>% summarise(mean_income = mean(income))

ggplot(data=welfare_mean, aes(x=ageg, y=mean_income))+geom_col()

# 연령대 및 성별에 따른 소득 -------------------

welfare_mean <- welfare %>% filter(ageg != "young") %>% 
  group_by(ageg, sex) %>% summarise((mean_income = mean(income)))

# fill -> 그래프에 색 지정
# position defalt value = stack
ggplot(data=welfare_mean, aes(x=ageg, y=mean_income, fill = sex)) + geom_col(position = "dodge")
