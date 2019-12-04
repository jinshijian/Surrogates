
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
data(ptitanic)

str(ptitanic)

# Step1: Begin with a small cp. 
set.seed(123)
tree <- rpart(survived ~ ., data = ptitanic, control = rpart.control(cp = 0.0001))

# Step2: Pick the tree size that minimizes misclassification rate (i.e. prediction error).
# Prediction error rate in training data = Root node error * rel error * 100%
# Prediction error rate in cross-validation = Root node error * xerror * 100%
# Hence we want the cp value (with a simpler tree) that minimizes the xerror. 
printcp(tree)

# Step3: Prune the tree using the best cp.
tree.pruned <- prune(tree, cp = 'bestcp')

# confusion matrix (training data)
conf.matrix <- table(ptitanic$survived, predict(tree.pruned,type="class"))
rownames(conf.matrix) <- paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) <- paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

plot(tree)
text(tree, cex = 0.8, use.n = TRUE, xpd = TRUE)

binary.model <- rpart(survived~., data = ptitanic, cp = .02)
rpart.plot(binary.model)
