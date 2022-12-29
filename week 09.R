# 선형회귀분석 (supervised learning : 지도학습)
# 마케팅에서 비지도학습을 주로 사용 (STP)
# 유의수준 : 오류의 가능성을 묶어둠
# 검정통계량 : 표준 오차/ 표본평균-귀무가설에서 설정한 값 
# 표본적 자료 -> 모집단에 대한 통계적 추론

library(tidyverse)  # ggplot2 + dplyr
mpg <- mpg

#lm (linear model) : 회귀분석모델 -> lm(종속변수~독립변수, data = )
result <- lm(cty~hwy, data=mpg)
summary(result) # cty = 0.84 + hwy * 0.6
ggplot(data=mpg, aes(x=hwy, y=cty))+geom_point(alpha = 0.3) + geom_smooth(method="lm")


result2 <- lm(cty~hwy+displ, data=mpg) # 변수가 여러개일 때 '+'로 연결
summary(result2) # 베타1 : 0.59, 베타2 : -0.52 
# 해석 : 자동차배기량(displ)을 통제했을 때, 고속도로 연비가 좋아질 때마다 시내 연비가 좋아진다.
# 다중회구선형분석을 더 신뢰함 

mydata <- read.csv("ceosal.csv")
view(mydata)
# roe : 자기자본수익률
lm.roe <- lm(salary ~ roe, data=mydata)
summary(lm.roe)
ggplot(data=mydata, aes(x=roe, y=salary))+geom_point(alpha = 0.3) + geom_smooth(method="lm")
hist(mydata$salary) # 왼쪽으로 치우친 모습
# 해석 : 유의수준 10% 내에서 통계적으로 유의하다. 5% 내 유의수준을 잡으면 유의하지 않는다.
# roe가 1% 증가할 때마다 salary가 18,000불 증가한다.

#log를 취함 -> 정규분포 (하지만 좋은 것이 아님) -> 왜??
mydata$salary_log <- log(mydata$salary)
hist(mydata$salary_log)
result3 <- lm(log(salary)~roe, data = mydata)
summary(result3)
# 퍼센트 변화로 해석 : roe가 1% 증가할 때 ceo 연봉이 1.38% 증가한다


