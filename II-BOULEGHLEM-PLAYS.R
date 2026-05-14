rm(list = objects())

getwd()
# setwd(chemin)

df_music <- read.table("./Music_2026.txt", header = T, sep = ";")

n = nrow(df_music)
m = ncol(df_music)
col_names = names(df_music)



set.seed(103)
train = sample(c(TRUE,FALSE),n,rep=TRUE,prob=c(2/3,1/3))
test = !train # ! et pas - car le faux passe au vrai
train_set = df_music[train & df_music$GENRE %in% c("Classical", "Jazz"), ]
nrow(train_set)

test_set = df_music[test & df_music$GENRE %in% c("Classical", "Jazz"), ]
nrow(test_set)

train_set$Y <- as.factor(train_set[, ncol(train_set)])
test_set$Y <- as.factor(test_set[, ncol(test_set)])

idx <- which(names(train_set) == "GENRE")

train_set <- train_set[, -idx]
test_set <- test_set[, -idx]



##### II. Classification binaire

### 1.

relevant_idx <- c(seq(1, 147), seq(168, ncol(train_set) - 1))
formulaModT <- as.formula(paste("Y ~ " , paste(names(train_set)[relevant_idx], collapse = " + ")))

ModT <- glm(formulaModT, family = binomial, data = train_set)

vars_Mod1 <- names(which(summary(ModT)$coefficients[, 4] < 0.05))[-1] #on enlève l'intercept

formulaMod1 <- as.formula(paste("Y ~", paste(vars_Mod1, collapse = " + ")))

Mod1 <- glm(formulaMod1, family = binomial, data = train_set)


vars_Mod2 <- names(which(summary(ModT)$coefficients[, 4] < 0.2))[-1] #on enlève l'intercept

formulaMod2 <- as.formula(paste("Y ~", paste(vars_Mod2, collapse = " + ")))

Mod2 <- glm(formulaMod2, family = binomial, data = train_set)

library(MASS)

cache_file <- "ModAIC_Music_2026.rds"

if (file.exists(cache_file)) {
  ModAIC <- readRDS(cache_file)
  cat("ModAIC chargé depuis le cache :", cache_file, "\n")
} else {
  ModAIC <- stepAIC(ModT, trace = FALSE)
  saveRDS(ModAIC, cache_file)
  cat("ModAIC calculé puis sauvegardé dans :", cache_file, "\n")
}

formulaAIC <- formula(ModAIC)
# Voici la formule du modèle optimisé selon le critère AIC 
#Y ~ PAR_TC + PAR_ASE1 + PAR_ASE3 + PAR_ASE5 + PAR_ASE6 + PAR_ASE7 + 
#     PAR_ASE8 + PAR_ASE9 + PAR_ASE10 + PAR_ASE11 + PAR_ASE12 + 
#     PAR_ASE13 + PAR_ASE14 + PAR_ASE15 + PAR_ASE16 + PAR_ASE17 + 
#     PAR_ASE18 + PAR_ASE19 + PAR_ASE20 + PAR_ASE21 + PAR_ASE22 + 
#     PAR_ASE23 + PAR_ASE24 + PAR_ASE25 + PAR_ASE26 + PAR_ASE27 + 
#     PAR_ASE28 + PAR_ASE29 + PAR_ASE31 + PAR_ASE32 + PAR_ASE33 + 
#     PAR_ASE34 + PAR_ASE_M + PAR_ASEV1 + PAR_ASEV2 + PAR_ASEV3 + 
#     PAR_ASEV4 + PAR_ASEV5 + PAR_ASEV6 + PAR_ASEV7 + PAR_ASEV8 + 
#     PAR_ASEV9 + PAR_ASEV10 + PAR_ASEV11 + PAR_ASEV12 + PAR_ASEV13 + 
#     PAR_ASEV14 + PAR_ASEV15 + PAR_ASEV16 + PAR_ASEV17 + PAR_ASEV18 + 
#     PAR_ASEV19 + PAR_ASEV20 + PAR_ASEV21 + PAR_ASEV22 + PAR_ASEV23 + 
#     PAR_ASEV24 + PAR_ASEV25 + PAR_ASEV26 + PAR_ASEV27 + PAR_ASEV28 + 
#     PAR_ASEV29 + PAR_ASEV30 + PAR_ASEV31 + PAR_ASEV32 + PAR_ASEV34 + 
#     PAR_ASE_MV + PAR_ASC + PAR_ASS + PAR_ASS_V + PAR_SFM1 + PAR_SFM2 + 
#     PAR_SFM3 + PAR_SFM4 + PAR_SFM5 + PAR_SFM6 + PAR_SFM7 + PAR_SFM8 + 
#     PAR_SFM9 + PAR_SFM10 + PAR_SFM11 + PAR_SFM12 + PAR_SFM13 + 
#     PAR_SFM14 + PAR_SFM15 + PAR_SFM16 + PAR_SFM17 + PAR_SFM18 + 
#     PAR_SFM19 + PAR_SFM20 + PAR_SFM21 + PAR_SFM22 + PAR_SFM23 + 
#     PAR_SFM_M + PAR_SFMV1 + PAR_SFMV2 + PAR_SFMV3 + PAR_SFMV4 + 
#     PAR_SFMV5 + PAR_SFMV6 + PAR_SFMV7 + PAR_SFMV8 + PAR_SFMV9 + 
#     PAR_SFMV10 + PAR_SFMV11 + PAR_SFMV12 + PAR_SFMV13 + PAR_SFMV14 + 
#     PAR_SFMV15 + PAR_SFMV16 + PAR_SFMV17 + PAR_SFMV18 + PAR_SFMV19 + 
#     PAR_SFMV20 + PAR_SFMV21 + PAR_SFMV22 + PAR_SFMV23 + PAR_SFMV24 + 
#     PAR_SFM_MV + PAR_MFCC3 + PAR_MFCC4 + PAR_MFCC6 + PAR_MFCC8 + 
#     PAR_MFCC9 + PAR_MFCC10 + PAR_MFCC11 + PAR_MFCC12 + PAR_MFCC15 + 
#     PAR_MFCC16 + PAR_MFCC17 + PAR_MFCC19 + PAR_MFCC20 + PAR_THR_2RMS_TOT + 
#     PAR_THR_3RMS_TOT + PAR_THR_1RMS_10FR_MEAN + PAR_THR_1RMS_10FR_VAR + 
#     PAR_THR_2RMS_10FR_VAR + PAR_THR_3RMS_10FR_MEAN + PAR_THR_3RMS_10FR_VAR + 
#     PAR_PEAK_RMS10FR_MEAN + PAR_PEAK_RMS10FR_VAR + PAR_1RMS_TCD + 
#     PAR_2RMS_TCD + PAR_3RMS_TCD + PAR_1RMS_TCD_10FR_MEAN + PAR_1RMS_TCD_10FR_VAR + 
#     PAR_2RMS_TCD_10FR_MEAN
### 2. Courbes ROC et AUC

library(ROCR)
calc_auc <- function(pred_obj) {
  perf <- performance(pred_obj, "auc")
  return(perf@y.values[[1]])
}

pred_ModT_train <- predict(ModT, type = "response")
pred_ModT_test <- predict(ModT, test_set, type = "response")

pred_obj_ModT_train <- prediction(pred_ModT_train, train_set$Y)
pred_obj_ModT_test <- prediction(pred_ModT_test, test_set$Y)

perf_ModT_train <- performance(pred_obj_ModT_train, "tpr", "fpr")
perf_ModT_test <- performance(pred_obj_ModT_test, "tpr", "fpr")
auc_ModT_train <- calc_auc(pred_obj_ModT_train)
auc_ModT_test <- calc_auc(pred_obj_ModT_test)

pred_Mod1_test <- predict(Mod1, newdata = test_set, type = "response")
pred_Mod2_test <- predict(Mod2, newdata = test_set, type = "response")
pred_ModAIC_test <- predict(ModAIC, newdata = test_set, type = "response")


pred_obj_Mod1_test <- prediction(pred_Mod1_test, test_set$Y)
pred_obj_Mod2_test <- prediction(pred_Mod2_test, test_set$Y)
pred_obj_ModAIC_test <- prediction(pred_ModAIC_test, test_set$Y)

auc_Mod1_test <- calc_auc(pred_obj_Mod1_test)
auc_Mod2_test <- calc_auc(pred_obj_Mod2_test)
auc_ModAIC_test <- calc_auc(pred_obj_ModAIC_test)

perf_Mod1_test <- performance(pred_obj_Mod1_test, "tpr", "fpr")
perf_Mod2_test <- performance(pred_obj_Mod2_test, "tpr", "fpr")
perf_ModAIC_test <- performance(pred_obj_ModAIC_test, "tpr", "fpr")

plot(perf_ModT_train, col = "blue", lty = 2, main = "Courbes ROC")
plot(perf_ModT_test, add = TRUE, col = "red", lty = 2)
plot(perf_Mod1_test, add = TRUE, col = "green")
plot(perf_Mod2_test, add = TRUE, col = "orange")
plot(perf_ModAIC_test, add = TRUE, col = "purple", lwd = 2)

lines(c(0, 0, 1), c(0, 1, 1), col = "black", lty = 1, lwd = 2)
abline(0, 1, col = "grey", lty = 2)

legend("bottomright", 
       legend = c(paste("ModT Train (AUC=", round(auc_ModT_train, 3), ")"),
                  paste("ModT Test (AUC=", round(auc_ModT_test, 3), ")"),
                  paste("Mod1 Test (AUC=", round(auc_Mod1_test, 3), ")"),
                  paste("Mod2 Test (AUC=", round(auc_Mod2_test, 3), ")"),
                  paste("ModAIC Test (AUC=", round(auc_ModAIC_test, 3), ")"),
                  "Parfaite", "Aléatoire"),
       col = c("blue", "red", "green", "orange", "purple", "black", "grey"),
       lty = c(2, 2, 1, 1, 1, 1, 2), lwd = c(1, 1, 1, 1, 2, 2, 1))


### 3. Régression Ridge (+ Lasso en bonus)

library(glmnet)


x_train <- model.matrix(formulaModT, train_set)[, -1]
y_train <- train_set$Y

x_test <- model.matrix(formulaModT, test_set)[, -1]
y_test <- test_set$Y

lambda_seq <- 10^seq(10, -2, length = 100)


ridge_mod <- glmnet(x_train, y_train, family = "binomial", alpha = 0, lambda = lambda_seq)
lasso_mod <- glmnet(x_train, y_train, family = "binomial", alpha = 1, lambda = lambda_seq)

plot(ridge_mod, xvar = "lambda")
plot(lasso_mod, xvar = "lambda")


### 4. Validation croisée pour le paramètre de régularisation

set.seed(2026)

#on minimise l'erreur de classification
cv_ridge <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 0, nfolds = 10, lambda = lambda_seq, type.measure = "class")
cv_lasso <- cv.glmnet(x_train, y_train, family = "binomial", alpha = 1, nfolds = 10, lambda = lambda_seq, type.measure = "class")
plot(cv_ridge)
plot(cv_lasso)
best_lambda <- cv_ridge$lambda.1se
cat("Lambda optimal (1se) :", cv_ridge$lambda.1se, "\n")
cat("Lambda minimal (min) :", cv_ridge$lambda.min, "\n")
best_lambda_lasso <- cv_lasso$lambda.1se 
cat("Lambda optimal (1se) :", cv_lasso$lambda.1se, "\n")
cat("Lambda minimal (min) :", cv_lasso$lambda.min, "\n")

pred_ridge_test <- predict(ridge_mod, s = best_lambda, newx = x_test, type = "response")
pred_lasso_test <- predict(lasso_mod, s = best_lambda_lasso, newx = x_test, type = "response")


pred_obj_ridge_test <- prediction(pred_ridge_test, test_set$Y)
auc_ridge_test <- calc_auc(pred_obj_ridge_test)
perf_ridge_test <- performance(pred_obj_ridge_test, "tpr", "fpr")

pred_obj_lasso_test <- prediction(pred_lasso_test, test_set$Y)
auc_lasso_test <- calc_auc(pred_obj_lasso_test)
perf_lasso_test <- performance(pred_obj_lasso_test, "tpr", "fpr")

cat("AUC Ridge (Test) :", auc_ridge_test, "\n")
cat("AUC Lasso (Test) :", auc_lasso_test, "\n")

### 5. Compléter la figure de la question 2 avec Ridge (+ Lasso en bonus)

plot(perf_ModT_train, col = "blue", lty = 2, main = "Courbes ROC - Tous les modèles")
plot(perf_ModT_test, add = TRUE, col = "red", lty = 2)
plot(perf_Mod1_test, add = TRUE, col = "green")
plot(perf_Mod2_test, add = TRUE, col = "orange")
plot(perf_ModAIC_test, add = TRUE, col = "purple", lwd = 2)

plot(perf_ridge_test, add = TRUE, col = "darkred", lwd = 2, lty = 5)
plot(perf_lasso_test, add = TRUE, col = "darkgreen", lwd = 2, lty = 5)

lines(c(0, 0, 1), c(0, 1, 1), col = "black", lty = 1, lwd = 2)
abline(0, 1, col = "grey", lty = 2)

legend("bottomright", 
       legend = c(paste("ModT Train (AUC=", round(auc_ModT_train, 3), ")"),
                  paste("ModT Test (AUC=", round(auc_ModT_test, 3), ")"),
                  paste("Mod1 Test (AUC=", round(auc_Mod1_test, 3), ")"),
                  paste("Mod2 Test (AUC=", round(auc_Mod2_test, 3), ")"),
                  paste("ModAIC Test (AUC=", round(auc_ModAIC_test, 3), ")"),
                  paste("Ridge Test (AUC=", round(auc_ridge_test, 3), ")"),
                  paste("Lasso Test (AUC=", round(auc_lasso_test, 3), ")"),
                  "Parfaite", "Aléatoire"),
       col = c("blue", "red", "green", "orange", "purple", "darkred", "darkgreen", "black", "grey"),
       lty = c(2, 2, 1, 1, 1, 5, 5, 1, 2), lwd = c(1, 1, 1, 1, 2, 2, 2, 2, 1))


### 6. Récapitulatif et prédictions sur le jeu de test final

auc_Mod1_train <- calc_auc(prediction(predict(Mod1, type="response"), train_set$Y))
auc_Mod2_train <- calc_auc(prediction(predict(Mod2, type="response"), train_set$Y))
auc_ModAIC_train <- calc_auc(prediction(predict(ModAIC, type="response"), train_set$Y))
pred_ridge_train <- predict(ridge_mod, s = best_lambda, newx = x_train, type = "response")
auc_ridge_train <- calc_auc(prediction(pred_ridge_train, train_set$Y))
pred_lasso_train <- predict(lasso_mod, s = best_lambda_lasso, newx = x_train, type = "response")
auc_lasso_train <- calc_auc(prediction(pred_lasso_train, train_set$Y))


recap <- data.frame(
  Modele = c("ModT", "Mod1", "Mod2", "ModAIC", "Ridge", "Lasso"),
  AUC_Train = round(c(auc_ModT_train, auc_Mod1_train, auc_Mod2_train, auc_ModAIC_train, auc_ridge_train, auc_lasso_train), 4),
  AUC_Test = round(c(auc_ModT_test, auc_Mod1_test, auc_Mod2_test, auc_ModAIC_test, auc_ridge_test, auc_lasso_test), 4)
)
print(recap)


erreur_classif <- function(proba, y_true) {
  proba <- as.vector(proba)
  pred <- ifelse(proba > 0.5, "Jazz", "Classical")
  mean(pred != as.character(y_true))
}

# Probabilités sur apprentissage
p_ModT_train <- predict(ModT, newdata = train_set, type = "response")
p_Mod1_train <- predict(Mod1, newdata = train_set, type = "response")
p_Mod2_train <- predict(Mod2, newdata = train_set, type = "response")
p_ModAIC_train <- predict(ModAIC, newdata = train_set, type = "response")

p_Ridge_train <- predict(ridge_mod, s = best_lambda, newx = x_train, type = "response")
p_Lasso_train <- predict(lasso_mod, s = best_lambda_lasso, newx = x_train, type = "response")

# Probabilités sur test
p_ModT_test <- predict(ModT, newdata = test_set, type = "response")
p_Mod1_test <- predict(Mod1, newdata = test_set, type = "response")
p_Mod2_test <- predict(Mod2, newdata = test_set, type = "response")
p_ModAIC_test <- predict(ModAIC, newdata = test_set, type = "response")

p_Ridge_test <- predict(ridge_mod, s = best_lambda, newx = x_test, type = "response")
p_Lasso_test <- predict(lasso_mod, s = best_lambda_lasso, newx = x_test, type = "response")

# Tableau des erreurs
recap_err <- data.frame(
  Modele = c("ModT", "Mod1", "Mod2", "ModAIC", "Ridge", "Lasso"),
  
  Erreur_Train = round(c(
    erreur_classif(p_ModT_train, train_set$Y),
    erreur_classif(p_Mod1_train, train_set$Y),
    erreur_classif(p_Mod2_train, train_set$Y),
    erreur_classif(p_ModAIC_train, train_set$Y),
    erreur_classif(p_Ridge_train, train_set$Y),
    erreur_classif(p_Lasso_train, train_set$Y)
  ), 4),
  
  Erreur_Test = round(c(
    erreur_classif(p_ModT_test, test_set$Y),
    erreur_classif(p_Mod1_test, test_set$Y),
    erreur_classif(p_Mod2_test, test_set$Y),
    erreur_classif(p_ModAIC_test, test_set$Y),
    erreur_classif(p_Ridge_test, test_set$Y),
    erreur_classif(p_Lasso_test, test_set$Y)
  ), 4)
)

print(recap_err)

# predictions sur la base de donnée sans labels
df_music_test <- read.table("./Music_test_2026.txt", header = T, sep = ";")

probas_modAIC <- predict(ModAIC, newdata = df_music_test, type = "response")
predictions_final <- ifelse(probas_modAIC > 0.5, "Jazz", "Classical")

write.table(predictions_final, file = "BOULEGHLEM-PLAYS_test.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)

