ploan <- read.csv("Personal Loan.csv")
str(ploan)
summary(ploan)

ploan <- ploan[,-c(1,5)]
ploan$Personal.Loan<- as.factor(ploan$Personal.Loan)
str(ploan)
summary(ploan)


trn_idx<- 1:1500 # trn <- training data
ploan_train <- ploan[trn_idx,]
ploan_test  <- ploan[-trn_idx,]


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

# Construct single tree and evaluation
# tree parameter settings
# min_criterion = c(0.9, 0.95, 0.99) # ë¶?ë¥? ????????? ê¸°ì??
# min_split = c(10, 30, 50, 100) # ë§?ì§?ë§? node?????? ê´?ì¸¡ë?? ìµ???? ê´?ì¸? ê°? ???, ???ê²? ??¤ì????? ??? ë¡? ??¸ë??ê°? ë§????ì§? (ê³¼ì????? ê°???¥ì?? ì¦?ê°?) 
# max_depth = c(0, 10, 5) # ??¸ë¦¬??? ìµ????ê¹????

# party ÆĞÅ°Áö ³», ctree function
# ctree_control : ±âÁØ ¼³Á¤ (¼øµµ 90%¸é ¸¸Á·ÇØ) 
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

#Á¤ºĞ·ùÀ²°¡Áö°í ¸ğÇüÀÌ ¾ó¸¶³ª ¿¹ÃøÀ» Àß Çß´ÂÁö ÆÇ´ÜÇÑ´Ù -> ÇÏÁö¸¸ ¹®Á¦°¡ ÀÖ´Ù!?
