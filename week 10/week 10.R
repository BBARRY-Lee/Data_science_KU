library(ggplot2)
library(dplyr)

Advertising <- read.csv("Advertising.csv")
apt_data <- read.csv("apt_data.csv", fileEncoding = "euc-kr")
Credit <- read.csv("Credit.csv")
examscore <- read.csv("examscore.csv")

### �� ������ �ٽ��� �� ����ϴ��� ������ Ȯ���� ¤�� �� ��
# ������ƽ ȸ�ͺм�, �ǻ���� ���� (���Ӻ����� ���Ӻ���(��������)�� �ƴ�, ������ ������ ��)
# �ּ��км�,  Ŭ�����͸� (�������н� (unsupervised), y���� ���� ��)
# �����ͻ��̾� ����, �߷��̳� ������ �ſ� �߿�, ������ �ϸ� �����̳� �ٸ�����.

# ����ȸ�ͺм�
lm.fit <- lm(sales ~ tv, data = Advertising)
summary(lm.fit)
# tv 1cost�� ������ �� 0.04 �����Ѵ�.

#��ȸ��м�
lm.fit <- lm(sales ~ tv + radio + newspaper, data = Advertising)
summary(lm.fit)

# ������ ���������۸� ���� ����ȸ�ͺм��ϸ� �� ������,
# ���� ����ȸ�ͺм��� �ϸ� ���������۰� �� ������ �ʴ´�.
# �̴� ������ sales�� �� �� �����ϴ� �����̱� �����̴�.
# ������ �м��� �밭 �ϸ� �� �� �ִ�.

# ������ ���� ����� ������ ����. �ܼ��� ������ �ΰ� ���� �� ������ �´ٰ� �� �� �ִ�.
# �׷��� ������ ������ �����ϱ�? ���⿡�� ���̶�� ���� ������ �ִ�.
# ���� <- ����, �������� �ϸ� ������ �׷��� ū ������ ������ �ƴ��� �� �� �ִ�.
# �׷��� ���� advertising�� ������ ������ �����ϱ�? �α� �� ���� �� �� ���� ���̴�.

# �ڵ����� ��ÿ����� ���� ��ġ�� ������?
# ������ ������ ��ⷮ! ���γ��� �� �ϼ���, ��ⷮ�� ���� ���۵ȴ� (���� �϶�)
# ��, ������ ��ⷮ�� �� �������� ������ �ֽ��� ���� ���� ����.
mpg <-  mpg

lm.mpg <- lm(formula = cty ~ year+displ, data = mpg)
summary(lm.mpg)


### �ε��� �� ȸ�ͺм� ---------------------
### ����ȸ�ͺм��� �߿��� ������ ������ �� �ִ� ����

# �� �����ɼ��� �ε��갪�� ��ΰ� ���ñ�?
# ������ ����Ʈ�ϼ���, �������� ���� ���� 
# �ֽ� ����Ʈ�ϼ���, �����Ը� ����

lm.apt <- lm(price ~ size + age, data = apt_data)
summary(lm.apt)

# �Ȱ��� ������� �������� ����Ʈ�� ��, ������ ������ ����Ʈ ������ �϶��Ѵ�.
# �������� ������ ������ �ؾ��ϴµ�, ��ȸ�����δ� ������ �� ����.
# ����, �����ڷῡ�� age�� ������ ȿ���� ���Ⱑ ��ƴ�.
# �̴� �跮�����п��� �ٷ�� �о�
lm.apt <- lm(price ~ size + age + complex_size, data = apt_data)
summary(lm.apt)

lm.apt <- lm(price ~ size + age + complex_size + subway + brand, data = apt_data)
summary(lm.apt)

##### ������ + �귣����� ���� ������ �ƴϴ�. �ܼ� ����ȸ�ͺм� �Ұ���
#### ������!? �ſ�ī�� ������ ���캸��.
#### onehot����? ���̺��� 

lm.credit <- lm(balance ~ gender, data = Credit)
summary(lm.credit) # female = 1�� �ڵ����� �� # ������� ����
# �������� ���� �������������� �������� ����, �׷���?

lm.credit <- lm(balance ~ ethnicity, data = Credit) # ���δ�� �ƽþ�, ���δ�� ����
summary(lm.credit)
# �̰͵� ��������� �������� ���� (P����� 0.05���� ���� �ʱ� ����)



### ���� �� ��ȣ�ۿ��� �м� ------------------ -> �߰��� ��������! <- ------------------------------------------

lm.adv <-  lm(sales ~ tv * radio, data = Advertising)
summary(lm.adv)
# ��������� tv : radio = tv�� radio�� ���ÿ� ���� ��, �ó��� ȿ���� �ִٸ� ������
# tv * radio = tv + radio +tv:radio -> ���ϱ⸸ �ϰ� ������ tv:radio

lm.crd <- lm(formula = balance ~ income + student + income:student, data = Credit)
summary(lm.crd)
# income:studentYes  -1.9992 : income�� �л����� ��ŭ ������ ��ģ��.

### ������ �α׸� ���ϴ� ��� -> ���Ժ���ȭ
# �α׸� ���ϴ� ������ ź�¼��� ���� ����









