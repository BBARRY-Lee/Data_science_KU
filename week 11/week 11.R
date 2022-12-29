library(dbplyr)
library(ggplot2)

######## �߷� �ܰ� ------------------------------------------------------------------------------------------------
# ������ƽ ȸ�� �м� (�߷а� ������ ���� ���)
# s �׷����� ��� ���� ����ȸ�ͺм� x -> ������ƽ �Լ�!!
# ������ƽ �Լ��Ŀ��� x�� ���Ѵ�� Ŀ���� �ִ밪�� 1, ���Ѵ�� �۾����� �ּҰ��� 0
# �׷��� �ñ׸��̵� �Լ��� 0 �ƴϸ� 1�� ����!!!
# �׷��� ��������, ������ ������ �𵨸��ϴ� �� ���̴� �Լ��� -> s �� ������ �׷����� ������ 
# ������ƽ ��� (�跮������ ��) == �ñ׸��̵� ��� (�ӽŷ���, ������)

### �»� (Odds)
# ����Ȯ�� : P / ����Ȯ�� : 1-P
ott <- read.csv("midterm_OTT.csv")
ott$OTT_usage <- ifelse(ott$OTT_usage == 2, 0, ott$OTT_usage) # 0 or 1 �� ���������� �νĽ�Ŵ
table(ott$OTT_usage)
result <- glm(OTT_usage ~ factor(gender) + age + factor(marriage), data = ott, family = binomial(link = "logit"))
summary(result)

######## ���� �ܰ� ------------------------------------------------------------------------------------------------
ploan <- read.csv("Personal Loan.csv")
str(ploan)
summary(ploan)

ploan <-  ploan[, -c(1,5)] # 1��° ����, 5��° ���� ���ܽ�Ŵ (ID, Zip code)
str(ploan)
summary(ploan)

# train, test data set (70%, 30%)
trn_idx <- sample(1:nrow(ploan),round(0.7*nrow(ploan))) #1~2500 -> 70%�� ���ø�
set.seed(1) # ���� ������ ������ ����
ploan_train <- ploan[trn_idx, ]
ploan_test <- ploan[-trn_idx,]

# ������ �� �ݿ��Ǿ����� ����
prop.table(table(ploan_train$Personal.Loan)) # ����
prop.table(table(ploan_test$Personal.Loan)) # ����

# ~. �۽��η� ���� ������ ���� ���
result <- glm(Personal.Loan ~., data = ploan_train, family = binomial(link = "logit")) 
options((scipen  = 5)) # ����ǥ��� ����
summary(result)
# R������ �̺�Ʈ �߻��� ū ���� �� (ex : 1, 0�� ������ 1�� �̺�Ʈ �߻��� ���)

round(exp(coef(result)), 3) # ��������� ������ �����鿡 ���ؼ�, 1���� ũ��, �׸�ŭ ������ ������ ���� ������ ���ٴ� ���̰�, 0���� ���� ���� �׸� ŭ �پ���� �ǹ�


##### �� ������ �غ��� -------------------------

ploan_pred <- predict(result, newdata = ploan_test, type = "response")
head(ploan_pred)

# �Ļ����� (0.5�� �������� yes, no ���)
ploan_pred_factor <- factor(ifelse(ploan_pred<0.5, 0, 1), levels = c(0, 1), labels = c("No", "Yes"))
head(ploan_pred_factor) # �� �� no�� ������...�� �ű��ϰԵ� �װ͸� ����

# ����� ������ ��
ploan_test$Personal.Loan <- factor(ploan_test$Personal.Loan, levels = c(0, 1), labels = c("No", "Yes"))
ploan_test$Personal.Loan
table(ploan_test$Personal.Loan, ploan_pred_factor, dnn = c("Actual", "Prediction"))
# ���з���, ���з���, 1750/673+44 , 1750/7+26




