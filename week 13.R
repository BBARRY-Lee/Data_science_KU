# ��ȸ�������� �м��� �� ��, ���ݱ��� ����� ����ȸ��, ������ƽ ���� ���� ����Ѵ�.
# ��ȸ������ �߷��� �� �����̱� ����
# �����ͻ��̾�, �м��� ������ �� ����
# ��������� �� ��ġ�� ������ �� ���� n���� �Ǿ�� ��
# ������� (Confusion Matrix) �����ǥ �⸻, �������� ���� ���� (�ΰ����� Ư�̵�)

### ���Ī ���з� ���
# ��ġ������ �м��� �ص�, �ᱹ �����ڿ��� ������ ��ġ�� ��� ������ ���̳�?
# �ҷ��ϳ��� �������� �߸� ���� ���� ��, ������ �ս��� -10�� ���� �߱��Ѵ�. ��

### ���з���, �ΰ���, Ư�̵�, BCR, F1score �ǽ��غ��� 
Ploan <- read.csv("Personal Loan.csv")
Ploan
str(Ploan)
summary(Ploan)

Ploan <- Ploan[, -c(1,5)] # 1��° ����, 5��° ���� ���ܽ�Ŵ (ID, Zip code)

# train, test data set (70%, 30%)
trn_idx <- sample(1:nrow(ploan),round(0.7*nrow(ploan))) #1~2500 -> 70%�� ���ø�
trn_idx <-  sample(1:nrow(Ploan), round(0.7*nrow(Ploan)))
set.seed(1) # ���� ������ ������ ����
Ploan_train <- Ploan[trn_idx, ]
Ploan_test <- Ploan[-trn_idx,]

# ������ �� �ݿ��Ǿ����� ����
prop.table(table(Ploan_train$Personal.Loan)) # ����
prop.table(table(Ploan_test$Personal.Loan)) # ����

### ������ƽ ȸ�ͺм� ���� -----
# ~. �۽��η� ���� ������ ���� ���
result <- glm(Personal.Loan ~., data = Ploan_train, family = binomial(link = "logit")) 
options((scipen  = 5)) # ����ǥ��� ����
summary(result)
# R������ �̺�Ʈ �߻��� ū ���� �� (ex : 1, 0�� ������ 1�� �̺�Ʈ �߻��� ���)

round(exp(coef(result)), 3) # ��������� ������ �����鿡 ���ؼ�, 1���� ũ��, �׸�ŭ ������ ������ ���� ������ ���ٴ� ���̰�, 0���� ���� ���� �׸� ŭ �پ���� �ǹ�

Ploan_pred <- predict(result, newdata = Ploan_test, type = "response")
head(Ploan_pred)

# �Ļ����� (0.5�� �������� yes, no ���)
Ploan_pred_factor <- factor(ifelse(Ploan_pred<0.5, 0, 1), levels = c(0, 1), labels = c("No", "Yes"))
head(Ploan_pred_factor)

# ����� ������ �� (�������)---------------------------
Ploan_test$Personal.Loan <- factor(Ploan_test$Personal.Loan, levels = c(0, 1), labels = c("No", "Yes"))
Ploan_test$Personal.Loan
# ���з���, ���з���, 1750/667+27, 1750/5+51
table(Ploan_test$Personal.Loan, Ploan_pred_factor, dnn = c("Actual", "Prediction"))

### logistic regression (Ȯ���� ���� 0.5)�� decision Tree (post-pruning)�� ������ ��

# CART with Post-Pruning -------------------------------
install.packages("tree")
library(tree)

# Training the tree
CART_post <- tree(Personal.Loan ~ ., ploan_train)
summary(CART_post)

# Plot the tree
plot(CART_post)
text(CART_post)

# Find the best tree
set.seed(123)
CART_post_cv <- cv.tree(CART_post, FUN = prune.misclass)

# Plot the pruning result
plot(CART_post_cv$size, CART_post_cv$dev, type = "b") #size = # of terminal node
CART_post_cv

# Select the final model
CART_post_pruned <- prune.misclass(CART_post, best = 6) # best means size of leaf nodes
plot(CART_post_pruned)
text(CART_post_pruned)

# Prediction
CART_post_prey <- predict(CART_post_pruned, ploan_test, type = "class")
CART_post_cm <- table(ploan_test$Personal.Loan, CART_post_prey)
CART_post_cm


# CART with Pre-Pruning -------------------------------
# For CART
install.packages("matrixStats")
install.packages("party")
library(party)


# Divide the dataset into training/validation/test datasets
train_idx <- 1:1000
val_idx   <- 1001:1500
test_idx  <- 1501:2500

ploan_train <- ploan[train_idx,]
ploan_val   <- ploan[val_idx,]
ploan_test  <- ploan[test_idx,]


# party ��Ű�� ��, ctree function
# ctree_control : ���� ���� (���� 90%�� ������) 
ploan_control = ctree_control(mincriterion = 0.90, minsplit = 10, maxdepth = 10)
ploan_pretree <- ctree(Personal.Loan ~ ., data = ploan_train, controls = ploan_control)
plot(ploan_pretree)

# Use the training and validation dataset to train the best tree
ploan_train <- rbind(ploan_train, ploan_val)
CART_pre    <- ctree(Personal.Loan ~ ., data = ploan_train, controls = ploan_control)

plot(CART_pre)
plot(CART_pre, type="simple")

CART_pre_prediction <- predict(CART_pre, newdata = ploan_test)

# Performance of the best tree
CART_pre_cm <- table(ploan_test$Personal.Loan, CART_pre_prediction)
CART_pre_cm
table(Ploan_test$Personal.Loan, Ploan_pred_factor, dnn = c("Actual", "Prediction"))
#���з��������� ������ �󸶳� ������ �� �ߴ��� �Ǵ��Ѵ� -> ������ ������ �ִ�!?

# ���������� ���
