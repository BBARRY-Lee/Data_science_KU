# 패키지 불러오기 --------------------------------------

library(tidyverse)
library(readxl)


# 데이터 불러오기 --------------------------------------

birth_df <-  read_excel("birth.xlsx")

# 데이터 탐색하기 --------------------------------------

birth_df %>% dim()
dim(birth_df)
birth_df %>% View()
head(birth_df)
str(birth_df)

colSums(is.na(birth_df)) #colSums 함수는 변수별(가로)로 숫자 더하기

is.na(birth_df$시점) %>% head()
!is.na(birth_df$시점)

# 원하는 행과 열 선택하기 --------------------------------------

birth_df %>% 
  filter(!is.na(시점)) %>% # filter(시점 != NA)
  select(시점,전국) %>% 
  head()


# 칼럼의 정보를 나눠주는 separate ------------------------------

birth_df <-  birth_df %>% 
             filter(!is.na(시점)) %>% 
             select(시점,전국) %>% 
             separate(시점,into=c("년도","월")) #separate 함수 기능: column을 특정패턴에 따라 두개로 쪼개기

# 월별로 구분하여 정보 요약하기 ------------------------------
# 중간고사 출제유형 1

birth_df %>%
  group_by(월) %>%
  summarise(평균출생수 = mean(전국))

# 데이터를 순서대로 정렬하는 arrange  -----------------------

birth_df %>%
  group_by(월) %>%
  summarise(평균출생수 = mean(전국)) %>% 
  arrange(desc(평균출생수))

#ggplot2을 이용한 시각화

birth_df %>%
  group_by(월) %>%
  summarise(평균출생수 = mean(전국)) %>% 
  ggplot(data=.,aes(x=월,y=평균출생수)) + 
  geom_col() + 
  labs(title="월별 신생아 출생 평균",subtitle="1997년1월-2020년12월") +
  theme_bw(base_size = 15)

