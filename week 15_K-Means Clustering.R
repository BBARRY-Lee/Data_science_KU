
# �������� ��������м� ������ K-Ŭ�����͸� �غ� �ǽ�

# Package for cluster validity
install.packages("clValid")
install.packages("plotrix")
library(dplyr)
library(clValid)
library(plotrix)

# Part 1: K-Means Clustering ----------------------------------------------
# Load the powerline dataset
powerline <- read.csv("powerline.csv")
# x1 ~ x10 �������̴ּ� ���� (��������)

# Remove the class label
powerline_class <- powerline[,1]
powerline_x <- powerline[,-c(1,12,13,14)]
# 4���� ������ �����ϰ� Ŭ������ �� �� ���� 10���� ���ܳ���

# data scaling
# powerline_x_scaled <- scale(powerline_x, center = TRUE, scale = TRUE)
powerline_x_scaled <- powerline_x 
# ��������ϸ� ����ȭ�� ���־���� (�ҵ�, ���̿� ���� �������ٸ�)

# Evaluating the cluster validity measures
# 2~10������ Ŭ�����͸�
powerline_clValid <- clValid(powerline_x_scaled, 2:10, clMethods = "kmeans", 
                           validation = c("internal", "stability"))
summary(powerline_clValid) # Ÿ�缺 ��ǥ�� Ȯ�� ��
#�Ƿ翧 = 2, �� = 8�϶� ���� ŭ -> �� �ؿ� �����
# ������ ����� �����µ� ��ؾ���????
# �տ� ������ Ÿ�缺 ��ǥ�� ���� 2�� ���� �󵵼��� ����.
# ���� 2���� �׷����� Ŭ�����͸� �ǽ�

# Perform K-Means Clustering with the best K determined by Silhouette
# 2���� Ŭ�������϶�� �Լ� 
powerline_kmc <- kmeans(powerline_x_scaled,2)

str(powerline_kmc)
powerline_kmc$centers
powerline_kmc$size
powerline_kmc$cluster # 1������ 720���� ��� Ŭ�����Ϳ� ���� �Ǿ�����

# Assign the cluster info
powerline$class <- powerline_kmc$cluster # Ŭ�����͸� �з� �� �Ļ����� �߰�

# Class �� ���̿� �����ҵ� ���

powerline %>% 
  group_by(class) %>% 
  summarise(age_mean = mean(age),
            income_mean = mean(income_h))
# ù��° ������ �����ν� �� ���� ũ�� ������ �ʴ´�
# �ι�° ������ �����ν� ���� �� ����.

# class �� ���� ���� 1=����, 2= ����
powerline %>% 
  group_by(gender,class) %>% 
  summarise(n = n()) %>% 
  mutate(tot_group = sum(n)) %>%
  mutate(pct=n/tot_group*100)

## ���������� t�������� ���� �� ��պ� ���� (���м��ǹ�)