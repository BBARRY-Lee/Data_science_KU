library(dbplyr)
library(ggplot2)

######## 추론 단계 ------------------------------------------------------------------------------------------------
# 로지스틱 회귀 분석 (추론과 예측을 위해 사용)
# s 그래프를 띄는 것은 선형회귀분석 x -> 로지스틱 함수!!
# 로지스틱 함수식에서 x가 무한대로 커져도 최대값은 1, 무한대로 작아져도 최소값은 0
# 그래서 시그모이드 함수가 0 아니면 1인 것임!!!
# 그래서 이진변수, 범주형 변수를 모델링하는 데 쓰이는 함수임 -> s 자 형태의 그래프가 유지됨 
# 로지스틱 펑션 (계량경제학 등) == 시그모이드 펑션 (머신러닝, 딥러닝)

### 승산 (Odds)
# 성공확률 : P / 실패확률 : 1-P
ott <- read.csv("midterm_OTT.csv")
ott$OTT_usage <- ifelse(ott$OTT_usage == 2, 0, ott$OTT_usage) # 0 or 1 로 범주형으로 인식시킴
table(ott$OTT_usage)
result <- glm(OTT_usage ~ factor(gender) + age + factor(marriage), data = ott, family = binomial(link = "logit"))
summary(result)

######## 예측 단계 ------------------------------------------------------------------------------------------------
ploan <- read.csv("Personal Loan.csv")
str(ploan)
summary(ploan)

ploan <-  ploan[, -c(1,5)] # 1번째 변수, 5번째 변수 제외시킴 (ID, Zip code)
str(ploan)
summary(ploan)

# train, test data set (70%, 30%)
trn_idx <- sample(1:nrow(ploan),round(0.7*nrow(ploan))) #1~2500 -> 70%만 샘플링
set.seed(1) # 같은 랜덤만 뽑히게 설정
ploan_train <- ploan[trn_idx, ]
ploan_test <- ploan[-trn_idx,]

# 비율이 잘 반영되었는지 검증
prop.table(table(ploan_train$Personal.Loan)) # 검증
prop.table(table(ploan_test$Personal.Loan)) # 검증

# ~. 퍼스널론 빼고 나머지 변수 모두
result <- glm(Personal.Loan ~., data = ploan_train, family = binomial(link = "logit")) 
options((scipen  = 5)) # 지수표기법 해제
summary(result)
# R에서는 이벤트 발생을 큰 수로 봄 (ex : 1, 0이 있으면 1이 이벤트 발생한 경우)

round(exp(coef(result)), 3) # 통계적으로 유의한 변수들에 대해서, 1보다 크면, 그만큼 증가할 때마다 대출 승인이 난다는 것이고, 0보다 작은 수는 그만 큼 줄어듦을 의미


##### 찐 예측을 해보자 -------------------------

ploan_pred <- predict(result, newdata = ploan_test, type = "response")
head(ploan_pred)

# 파생변수 (0.5를 기준으로 yes, no 출력)
ploan_pred_factor <- factor(ifelse(ploan_pred<0.5, 0, 1), levels = c(0, 1), labels = c("No", "Yes"))
head(ploan_pred_factor) # 왜 다 no로 나오지...는 신기하게도 그것만 뽑힘

# 정답과 예측값 비교
ploan_test$Personal.Loan <- factor(ploan_test$Personal.Loan, levels = c(0, 1), labels = c("No", "Yes"))
ploan_test$Personal.Loan
table(ploan_test$Personal.Loan, ploan_pred_factor, dnn = c("Actual", "Prediction"))
# 정분류율, 오분류율, 1750/673+44 , 1750/7+26





