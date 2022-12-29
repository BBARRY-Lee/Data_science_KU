# 변수 지정하기
# art+(-)를 누르면 자동으로 " <- " 삽입
a <- 1
b <- 2
c <- 3
d <- 4.5
1+2
a+b
a+b+c
4/b
5*b

# 변수에 여러 데이터 삽 : combine의 약어
a <- c(1, 2, 3, 4, 5)

# Q. var1은 왜 num, var2,3은 왜 int?
var1 <- c(1, 2, 5, 7, 8) # 숫자 5개로 구성된 var1
var2 <- c(1:5)
var3 <- seq(1, 5) #sequence의 약어
var4 <- seq(1, 10, by = 2) # 1~10까지 2간격
var5 <- seq(1, 10, by = 3) # 1~10까지 3간격
var3 + var4
var1 + var2
var1
var1 + 2 # var1=5, 2=1과 같이 크기가 다르나, 2를 5개 복제하여 연산함
var2
var1+var2

# 문자로 된 변수 삽입
str1 <- "a"
str2 <- "text"
str3 <- "Hello World!" 

# 연속 문자 변수
str4 <- c("a", "b", "c")
str5 <- c("Hello!", "World", "is", "good!")

str1 + 3 # 문자 숫자 연산 불가능

# 함수적용
A <- c(1,2,3)
mean(A)
max(A)
min(A)
# 문자를 다루는 함수
paste(str5, collapse =",") #하나의 원소로 합쳐줌
paste(str5, collapse =" ")

# 함수결과로 새 변수 만들기
A_mean <- mean(A)
str5_new <- paste(str5, collapse =" ")


# 패키지 설치
install.packages("ggplot2")
library(ggplot2)

# 그래프 표현
x <-  c("a", "a", "b", "c")
qplot(x) # 빈도 그래프
mpg <-  mpg # ggplot2 설치할 때 같이있는 자동차 데이터
qplot(data = mpg, x = hwy) # mpg 데이터프레임에서 hwy변수를 가지고 막대 그래프
qplot(data = mpg, x = cty)
qplot(data = mpg, x = drv, y=hwy) # 구동방식(drv)대비 고속도로 연비(hwy) 표현
qplot(data = mpg, x = drv, y = hwy, geom = "line")
qplot(data = mpg, x = drv, y = hwy, geom = "boxplot", color = "drv")
?qplot

student <- c(80, 60, 70, 50, 90)
mean(student)
student_mean <- mean(student)
student_mean
