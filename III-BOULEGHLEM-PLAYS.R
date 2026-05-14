rm(list = objects())

getwd()
# setwd(chemin)

df_music <- read.table("./Music_2026.txt", header = T, sep = ";")

n = nrow(df_music)
m = ncol(df_music)
col_names = names(df_music)



##### III. Classification multinomiale nominale

library(nnet)
library(ROCR)



set.seed(103)
train = sample(c(TRUE,FALSE),n,rep=TRUE,prob=c(2/3,1/3))
test = !train # ! et pas - car le faux passe au vrai

train_multi <- df_music[train, ]
test_multi <- df_music[test, ]

train_multi$Y <- as.factor(train_multi$GENRE)
test_multi$Y <- factor(test_multi$GENRE, levels = levels(train_multi$Y))

ref_genre <- levels(train_multi$Y)[length(levels(train_multi$Y))]

train_multi$Y <- relevel(train_multi$Y, ref = ref_genre)
test_multi$Y <- relevel(test_multi$Y, ref = ref_genre)

idx <- which(names(train_multi) == "GENRE")

train_multi <- train_multi[, -idx]
test_multi <- test_multi[, -idx]

relevant_idx_multi <- c(seq(1, 147), seq(168, ncol(train_multi) - 1))

formulaModMulti <- as.formula(
  paste("Y ~", paste(names(train_multi)[relevant_idx_multi], collapse = " + "))
)



### 4. Régression logistique multinomiale
library(nnet)

ModMulti <- multinom(
  formulaModMulti,
  data = train_multi,
  trace = FALSE
)

summary(ModMulti)

pred_multi_train <- predict(ModMulti, newdata = train_multi, type = "class")
pred_multi_test <- predict(ModMulti, newdata = test_multi, type = "class")

err_multi_train <- mean(pred_multi_train != train_multi$Y)
err_multi_test <- mean(pred_multi_test != test_multi$Y)

cat("Erreur apprentissage multinom :", err_multi_train, "\n")
cat("Erreur test multinom :", err_multi_test, "\n")

conf_train_multi <- table(Observe = train_multi$Y, Pred = pred_multi_train)
conf_test_multi <- table(Observe = test_multi$Y, Pred = pred_multi_test)

print(conf_train_multi)
print(conf_test_multi)



### 5. Représentation du réseau de neurones 



x_train_nn <- model.matrix(formulaModMulti, data = train_multi)[, -1]
x_test_nn <- model.matrix(formulaModMulti, data = test_multi)[, -1]

classes <- levels(train_multi$Y)

# Construction propre de la matrice indicatrice des classes
y_train_nn <- matrix(
  0,
  nrow = nrow(train_multi),
  ncol = length(classes)
)

colnames(y_train_nn) <- classes

y_train_nn[
  cbind(seq_len(nrow(train_multi)), match(train_multi$Y, classes))
] <- 1

set.seed(2026)

ModNN <- nnet(
  x = x_train_nn,
  y = y_train_nn,
  size = 0,
  skip = TRUE, # on autorise le skipping sinon ne fonctionne pas car pas de poids dans la partie cachée du modèle
  softmax = TRUE,
  rang = 0,
  trace = FALSE
)

library(NeuralNetTools)

plotnet(ModNN)

cat("Code convergence nnet :", ModNN$convergence, "\n")

probas_nn_train <- predict(ModNN, x_train_nn, type = "raw")
probas_nn_test <- predict(ModNN, x_test_nn, type = "raw")

colnames(probas_nn_train) <- classes
colnames(probas_nn_test) <- classes

pred_nn_train <- classes[max.col(probas_nn_train)]
pred_nn_test <- classes[max.col(probas_nn_test)]

pred_nn_train <- factor(pred_nn_train, levels = classes)
pred_nn_test <- factor(pred_nn_test, levels = classes)

err_nn_train <- mean(pred_nn_train != train_multi$Y)
err_nn_test <- mean(pred_nn_test != test_multi$Y)

cat("Erreur apprentissage nnet :", err_nn_train, "\n")
cat("Erreur test nnet :", err_nn_test, "\n")

cat("Erreur apprentissage multinom :", err_multi_train, "\n")
cat("Erreur test multinom :", err_multi_test, "\n")

table(Observe = test_multi$Y, Pred_nnet = pred_nn_test)



### 6. Courbes ROC one vs. all

probas_multi_test <- predict(ModMulti, newdata = test_multi, type = "probs")

probas_multi_test <- as.matrix(probas_multi_test)

classes <- levels(test_multi$Y)

calc_auc <- function(pred_obj) {
  perf <- performance(pred_obj, "auc")
  return(perf@y.values[[1]])
}

perf_ova <- list()
auc_ova <- numeric(length(classes))
names(auc_ova) <- classes

for (i in seq_along(classes)) {
  cl <- classes[i]
  
  y_bin <- as.numeric(test_multi$Y == cl)
  
  pred_obj <- prediction(probas_multi_test[, cl], y_bin)
  perf_ova[[cl]] <- performance(pred_obj, "tpr", "fpr")
  auc_ova[cl] <- calc_auc(pred_obj)
}

print(round(auc_ova, 4))

plot(
  perf_ova[[1]],
  col = 1,
  lwd = 2,
  main = "Courbes ROC un contre tous - Modèle multinomial",
  xlab = "Taux de faux positifs",
  ylab = "Taux de vrais positifs"
)

if (length(classes) >= 2) {
  for (i in 2:length(classes)) {
    plot(perf_ova[[i]], add = TRUE, col = i, lwd = 2)
  }
}

# Règle parfaite et règle aléatoire
lines(c(0, 0, 1), c(0, 1, 1), col = "black", lty = 1, lwd = 2)
abline(0, 1, col = "grey", lty = 2)

legend(
  "bottomright",
  legend = c(
    paste(classes, "(AUC =", round(auc_ova, 3), ")"),
    "Parfaite",
    "Aléatoire"
  ),
  col = c(seq_along(classes), "black", "grey"),
  lty = c(rep(1, length(classes)), 1, 2),
  lwd = c(rep(2, length(classes)), 2, 1)
)

macro_auc <- mean(auc_ova)

cat("AUC macro moyenne :", macro_auc, "\n")
