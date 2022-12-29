library(ggplot2)
library(dplyr)

Advertising <- read.csv("Advertising.csv")
apt_data <- read.csv("apt_data.csv", fileEncoding = "euc-kr")
Credit <- read.csv("Credit.csv")
examscore <- read.csv("examscore.csv")

### 본 강의의 핵심은 왜 써야하는지 개념을 확실히 짚고 갈 것
# 로지스틱 회귀분석, 의사결정 나무 (종속변수가 연속변수(양적변수)가 아닌, 범주형 변수일 때)
# 주성분분석,  클러스터링 (비지도학습 (unsupervised), y값이 없을 때)
# 데이터사이언스 관점, 추론이나 검정이 매우 중요, 예측만 하면 무단이나 다름없다.

# 선형회귀분석
lm.fit <- lm(sales ~ tv, data = Advertising)
summary(lm.fit)
# tv 1cost가 증가할 때 0.04 증가한다.

#중회기분석
lm.fit <- lm(sales ~ tv + radio + newspaper, data = Advertising)
summary(lm.fit)

# 라디오와 뉴스페이퍼를 각각 선형회귀분석하면 잘 되지만,
# 같이 다중회귀분석을 하면 뉴스페이퍼가 잘 나오지 않는다.
# 이는 라디오가 sales를 더 잘 설명하는 변수이기 때문이다.
# 상관계수 분석을 대강 하면 알 수 있다.

# 연봉이 높은 사람은 혈압이 높다. 단순히 연봉만 두고 보면 이 모형은 맞다고 볼 수 있다.
# 그러나 숨겨진 변수가 무엇일까? 여기에는 나이라는 것이 숨어져 있다.
# 혈압 <- 나이, 연봉으로 하면 연봉은 그렇게 큰 유의적 변수가 아님을 알 수 있다.
# 그러면 위으 advertising의 숨겨진 변수는 무엇일까? 인구 수 등이 될 수 있을 것이다.

# 자동차의 출시연도가 연비에 미치는 영향은?
# 숨겨진 변수는 배기량! 새로나온 차 일수록, 배기량이 높게 제작된다 (연비 하락)
# 즉, 동일한 배기량의 차 기준으로 연도가 최신인 차가 연비가 좋다.
mpg <-  mpg

lm.mpg <- lm(formula = cty ~ year+displ, data = mpg)
summary(lm.mpg)


### 부동산 값 회귀분석 ---------------------
### 다중회귀분석이 중요한 이유를 설명할 수 있는 예시

# 왜 오래될수록 부동산값이 비싸게 나올까?
# 오래된 아파트일수록, 단지수가 많기 때문 
# 최신 아파트일수록, 단지규모가 작음

lm.apt <- lm(price ~ size + age, data = apt_data)
summary(lm.apt)

# 똑같은 사이즈와 단지수인 아파트일 때, 연식이 많으면 아파트 가격이 하락한다.
# 내생성을 보려면 실험을 해야하는데, 사회경제로는 실험할 수 없다.
# 따라서, 관측자료에서 age의 순수한 효과를 보기가 어렵다.
# 이는 계량경제학에서 다루는 분야
lm.apt <- lm(price ~ size + age + complex_size, data = apt_data)
summary(lm.apt)

lm.apt <- lm(price ~ size + age + complex_size + subway + brand, data = apt_data)
summary(lm.apt)

##### 역세권 + 브랜드명은 양적 변수가 아니다. 단순 선형회귀분석 불가능
#### 구러면!? 신용카드 예제로 살펴보자.
#### onehot벡터? 더미변수 

lm.credit <- lm(balance ~ gender, data = Credit)
summary(lm.credit) # female = 1로 자동으로 됨 # 남성대비 여성
# 위성별에 따른 결과는통계적으로 유의하지 않음, 그러면?

lm.credit <- lm(balance ~ ethnicity, data = Credit) # 흑인대비 아시안, 흑인대비 백인
summary(lm.credit)
# 이것도 통계적으로 유의하지 않음 (P밸류가 0.05보다 적지 않기 때문)



### 변수 간 교호작용의 분석 ------------------ -> 추가로 공부하자! <- ------------------------------------------

lm.adv <-  lm(sales ~ tv * radio, data = Advertising)
summary(lm.adv)
# 결과값에서 tv : radio = tv와 radio를 동시에 했을 때, 시너지 효과가 있다를 설명함
# tv * radio = tv + radio +tv:radio -> 곱하기만 하고 싶으면 tv:radio

lm.crd <- lm(formula = balance ~ income + student + income:student, data = Credit)
summary(lm.crd)
# income:studentYes  -1.9992 : income이 학생여부 만큼 영향을 끼친다.

### 변수에 로그를 취하는 경우 -> 정규분포화
# 로그를 취하는 이유는 탄력성의 개념 때문










