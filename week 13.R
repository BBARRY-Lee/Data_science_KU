# 사회과학적인 분석을 할 때, 지금까지 배웠던 선형회귀, 로지스틱 등을 많이 사용한다.
# 사회과학은 추론이 주 목적이기 때문
# 데이터사이언스, 분석은 예측이 주 목적
# 정오행렬을 다 합치면 데이터 총 개수 n개가 되어야 함
# 정오행렬 (Confusion Matrix) 기반지표 기말, 졸업시험 출제 예정 (민감도와 특이도)

### 비대칭 오분류 비용
# 수치적으로 분석을 해도, 결국 상위자에게 경제적 가치를 어떻게 설명할 것이냐?
# 불량하나가 정상으로 잘못 판정 됐을 때, 경제적 손실은 -10만 불을 야기한다. 등

### 정분류율, 민감도, 특이도, BCR, F1score 실습해보기 
Ploan <- read.csv("Personal Loan.csv")
Ploan
str(Ploan)
summary(Ploan)

Ploan <- Ploan[, -c(1,5)] # 1번째 변수, 5번째 변수 제외시킴 (ID, Zip code)

# train, test data set (70%, 30%)
trn_idx <- sample(1:nrow(ploan),round(0.7*nrow(ploan))) #1~2500 -> 70%만 샘플링
trn_idx <-  sample(1:nrow(Ploan), round(0.7*nrow(Ploan)))
set.seed(1) # 같은 랜덤만 뽑히게 설정
Ploan_train <- Ploan[trn_idx, ]
Ploan_test <- Ploan[-trn_idx,]

# 비율이 잘 반영되었는지 검증
prop.table(table(Ploan_train$Personal.Loan)) # 검증
prop.table(table(Ploan_test$Personal.Loan)) # 검증

### 로지스틱 회귀분석 기준 -----
# ~. 퍼스널론 빼고 나머지 변수 모두
result <- glm(Personal.Loan ~., data = Ploan_train, family = binomial(link = "logit")) 
options((scipen  = 5)) # 지수표기법 해제
summary(result)
# R에서는 이벤트 발생을 큰 수로 봄 (ex : 1, 0이 있으면 1이 이벤트 발생한 경우)

round(exp(coef(result)), 3) # 통계적으로 유의한 변수들에 대해서, 1보다 크면, 그만큼 증가할 때마다 대출 승인이 난다는 것이고, 0보다 작은 수는 그만 큼 줄어듦을 의미

Ploan_pred <- predict(result, newdata = Ploan_test, type = "response")
head(Ploan_pred)

# 파생변수 (0.5를 기준으로 yes, no 출력)
Ploan_pred_factor <- factor(ifelse(Ploan_pred<0.5, 0, 1), levels = c(0, 1), labels = c("No", "Yes"))
head(Ploan_pred_factor)

# 정답과 예측값 비교 (정오행렬)---------------------------
Ploan_test$Personal.Loan <- factor(Ploan_test$Personal.Loan, levels = c(0, 1), labels = c("No", "Yes"))
Ploan_test$Personal.Loan
# 정분류율, 오분류율, 1750/667+27, 1750/5+51
table(Ploan_test$Personal.Loan, Ploan_pred_factor, dnn = c("Actual", "Prediction"))

### logistic regression (확률값 기준 0.5)과 decision Tree (post-pruning)의 성능평가 비교

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


# party 패키지 내, ctree function
# ctree_control : 기준 설정 (순도 90%면 만족해) 
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
#정분류율가지고 모형이 얼마나 예측을 잘 했는지 판단한다 -> 하지만 문제가 있다!?

# 마지막으로 계산

