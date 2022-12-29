# ����ȸ�ͺм� (supervised learning : �����н�)
# �����ÿ��� �������н��� �ַ� ��� (STP)
# ���Ǽ��� : ������ ���ɼ��� �����
# ������跮 : ǥ�� ����/ ǥ�����-�͹��������� ������ �� 
# ǥ���� �ڷ� -> �����ܿ� ���� ����� �߷�

library(tidyverse)  # ggplot2 + dplyr
mpg <- mpg

#lm (linear model) : ȸ�ͺм��� -> lm(���Ӻ���~��������, data = )
result <- lm(cty~hwy, data=mpg)
summary(result) # cty = 0.84 + hwy * 0.6
ggplot(data=mpg, aes(x=hwy, y=cty))+geom_point(alpha = 0.3) + geom_smooth(method="lm")


result2 <- lm(cty~hwy+displ, data=mpg) # ������ �������� �� '+'�� ����
summary(result2) # ��Ÿ1 : 0.59, ��Ÿ2 : -0.52 
# �ؼ� : �ڵ�����ⷮ(displ)�� �������� ��, ���ӵ��� ���� ������ ������ �ó� ���� ��������.
# ����ȸ�������м��� �� �ŷ��� 

mydata <- read.csv("ceosal.csv")
view(mydata)
# roe : �ڱ��ں����ͷ�
lm.roe <- lm(salary ~ roe, data=mydata)
summary(lm.roe)
ggplot(data=mydata, aes(x=roe, y=salary))+geom_point(alpha = 0.3) + geom_smooth(method="lm")
hist(mydata$salary) # �������� ġ��ģ ���
# �ؼ� : ���Ǽ��� 10% ������ ��������� �����ϴ�. 5% �� ���Ǽ����� ������ �������� �ʴ´�.
# roe�� 1% ������ ������ salary�� 18,000�� �����Ѵ�.

#log�� ���� -> ���Ժ��� (������ ���� ���� �ƴ�) -> ��??
mydata$salary_log <- log(mydata$salary)
hist(mydata$salary_log)
result3 <- lm(log(salary)~roe, data = mydata)
summary(result3)
# �ۼ�Ʈ ��ȭ�� �ؼ� : roe�� 1% ������ �� ceo ������ 1.38% �����Ѵ�

