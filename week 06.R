install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)

# 배경, 산점도, 그림을 그릴 때, '+'로 연결, lim = limit
ggplot(data = mpg, aes(x = displ, y = hwy)) + geom_point() + xlim(3, 6) + ylim(10, 30)

# 실습 1
ggplot(data = mpg, aes(x = cty, y = hwy)) + geom_point()

options(scipen = 99) # 지수표기 -> 정수표기

midwest_new <- midwest
ggplot(data = midwest_new, aes(x = poptotal, y = popasian)) + geom_point() + xlim(0, 500000) + ylim(0, 10000)


# 드라이브 타입별(drv) 고속도로 연비(hwy) 평균 -> 막대 그래프 표현 : geom_col() (연비 내림차순 : reorder)
mydata <- mpg %>% group_by(drv) %>%  summarise(mean_hwy = mean(hwy))                                                                                             
ggplot(data = mydata, aes(x = reorder(drv, -mean_hwy), y = mean_hwy)) + geom_col()

# 빈도 측정 시 사용 geom_bar()
# -> 질적변수(카테고리컬 변수)가 있을 때, x로 더 쉽게 그릴 수 있음 (연속변수도 그릴 수 있음)
ggplot(data = mpg, aes(x = hwy)) + geom_bar() 

# 실습 2
mpg
mydata2 <- mpg %>% filter(class == 'suv') %>% group_by(manufacturer) %>% summarise(mean_cty = mean(cty)) %>% arrange(-mean_cty) %>% head(5)
mydata2

ggplot(data=mydata2, aes(x=manufacturer, y = mean_cty)) + geom_col()

ggplot(data=mpg, aes(x=class)) + geom_bar()


# geom_line() -> 시계열 그래프 그릴 때 사용

# economics -> 미국거시경제데이터 -> pce = 민간소비지수. psavert = 민간저축지수, uemped = 실업률, unemploy = 실업자 수
# 시계열 데이터는 꼭 날짜 시간 등의 변수가 있어야 함
economics <- economics
head(economics)

ggplot(data = economics, aes(x = date, y = psavert)) + geom_line()

# geom_boxplot()
ggplot(data = mpg, aes(x = drv, y = hwy)) + geom_boxplot()

# 실습 3
mydata3 <- mpg %>%filter(class %in% c('compact', 'subcompact', 'suv')) 
mydata3

ggplot(data = mydata3, aes(x = class, y = cty)) + geom_boxplot()
# 박스플롯에서는 평균을 알 수 없음

# 결측치 (null 등 데이터 정제 및 제거)

df <- data.frame(sex = c("M", "F", NA, "M", "F"), score = c(5, 4, 3, 4, NA))
df

is.na(df$sex) #결측여부 체크, R에서 'is' -> ~ 입니까?
table(is.na(df$sex)) # -> 카운트

mean(df$score) # 결측치가 있으면, 연산 안 됨

# 결측치 제거 -> score에 결측치가 있는 것을 나타냄
df %>% filter(is.na(score))

# !를 is 앞에 붙이면, na 제외 나머지 값 출력
mydata4 <- df %>% filter(!is.na(score) & !is.na(sex))

mean(mydata4$score)

# 결측치 제거 간단하게 na.omit() -> 근데 실무에서는 거의 안 씀 -> 데이터가 많이 삭제 돼 버리기 때문
na.omit(df)

# na.rm -> na를 remove한 연산
mean(df$score, na.rm = T)

# 결측치 채워넣기도 가능 -> 나머지 데이터의 평균 등으로 채워넣음































