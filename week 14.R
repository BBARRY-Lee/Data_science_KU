Data <- USArrests # �̱��� �� (50�� ��)�� ���¹��� �ڷ�
str(Data)
head(Data)

# ���� ���
apply(Data, 2, mean) # 1 = �� ����, 2 = �� ����
apply(Data, 2, sd)
summary(Data)

# �ּ��� �м�
# PCA ��� 5���� ����Ʈ�� �̷��� ���� (sdev, rotation, center, scale, x)
pr.out = prcomp(Data, center = TRUE, scale = TRUE)
str(pr.out) 

pr.out$center # �м� �� ���(center)�� ǥ������(scale)
pr.out$rotation # PC1 ~ PC4 : �ּ��е� (����)

biplot(pr.out, scale = 0)
# ������ : ����, ������ : data
# �������� �ּ��п� ���� ������ �⿩�ϴ� �� (������)
# ������ 50�� �ַ� ��ȯ�� �����͸� ���� �࿡ ��Ÿ�� ��
# > head(Data)�� ���� ����� ����

# ������ ���ϱ�
pr.out$sdev
pr.var = pr.out$sdev^2 # �����Ͽ� �ּ��� �л� ���
pve = pr.var/sum(pr.var)
pve # �ּ����� ���� ����

# scree plot �׸���
plot(pve, xlab="Principal Componant", ylab = "Proportion of Variance Explained", ylim = c(0,1), type = "line")