# ���̺귯�� ��ġ �� ������ �ҷ����� -------------
library(foreign)
library(dplyr)
library(ggplot2)

raw_welfare <- read.spss("data_spss_Koweps2014.sav", to.data.frame = T)
welfare <- raw_welfare

# ������ Ž�� -----------------------------------
dim(welfare)
str(welfare)
options(max.print = 100000) # �����Ͱ� �ʹ� ���� ����� �� �� �� �ɼ� ����
head(welfare)
summary(welfare)

# �����Ͱ� �����Ƿ�, �ʿ��� �����͸� ���� ------
## ������ ���� (���� �������� �ڵ��� ����) -----
welfare <- rename (welfare, sex = h0901_4,
                  birth = h0901_5,
                  income = h09_din)


# ������ ���� �ҵ� ------------------------------
class(welfare$sex) # ����Ÿ�� Ȯ��
summary(welfare$sex) #ī�װ����� �����ʹ� �ǹ̾��� ����
as.factor(welfare$sex) # ���� class ��ȯ
table(welfare$sex)

## 9�� ����ġ �̹Ƿ�, NA�� ��ȯ
welfare$sex <- ifelse(welfare$sex == 9, NA, welfare$sex) 
table(is.na(welfare$sex)) # True -> ����ġ ��

welfare$sex <- ifelse(welfare$sex == 1, "male", "female") 
qplot(welfare$sex)

## �ҵ�
class(welfare$income)
summary(welfare$income)
qplot(welfare$income)+xlim(0, 10000)
table(is.na(welfare$income))

# �������簡 �ƴ� ���������̱� ������, ������ ���� ������ ŭ
# �׷���, ������ ������ �߿�
welfare %>% group_by(sex) %>%  summarise(mean_income = mean(income))
ggplot(data=income_mean, aes(x=sex, y=mean_income))+geom.col()
# ���̿� ���� �ҵ� -----------------------------

class(welfare$birth)
summary(welfare$birth)
qplot(welfare$birth)

welfare$birth <- ifelse(welfare$birth == 9999, NA, welfare$birth) 
table(is.na(welfare$bitrh)) # True -> ����ġ ��

# ���ɿ� ���� �ҵ� -----------------------------
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

# ���ɴ� �� ������ ���� �ҵ� -------------------

welfare_mean <- welfare %>% filter(ageg != "young") %>% 
  group_by(ageg, sex) %>% summarise((mean_income = mean(income)))

# fill -> �׷����� �� ����
# position defalt value = stack
ggplot(data=welfare_mean, aes(x=ageg, y=mean_income, fill = sex)) + geom_col(position = "dodge")