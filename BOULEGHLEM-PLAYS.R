
##### Partie 1

rm(list=objects());graphics.off()
# setwd("C:/Users/plays/Documents/ENSTA/2A STIC/STA03/Projet")
library(ggplot2)
library(corrplot)
library(cluster)
# Question 1


df = read.table("Music_2026.txt",header=TRUE, sep = ";") 

dim(df)
n = nrow(df)
p = ncol(df)
summary(df$PAR_SC_V) #valeur max grande devant la moyenne -> on passe au log
summary(df$PAR_ASC_V)

#Répartition des données
tab_genre <- table(df$GENRE)
prop.table(tab_genre)

#Justification du log
ggplot(df, aes(x = PAR_SC_V)) + geom_histogram(bins = 30, fill = "steelblue")
ggplot(df, aes(x = log(PAR_SC_V))) + geom_histogram(bins = 30, fill = "steelblue")


#PAR_SC_V
ggplot(df, aes(y = PAR_SC_V)) +  #beaucoup de valeurs aberantes
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribution univariée de PAR_SC_V")

ggplot(df, aes(y = log(PAR_SC_V))) +  #mieux réparti avec le log, pratique pour utiliser plus tard l'ACP
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribution univariée de PAR_SC_V")

#PAR_ASC_V
ggplot(df, aes(y = PAR_ASC_V)) + 
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribution univariée de PAR_ASC_V")

ggplot(df, aes(y = log(PAR_ASC_V))) + #Pareil
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribution univariée de PAR_ASC_V")

#Analyse bivariée
mat_cor <- cor(df[, -p])
corrplot(mat_cor[1:10, 1:10], method = "circle")
#On cherche les variable très corrélée (corrélation > 0.99)
p <- ncol(mat_cor)
redondances <- list()

for (i in 1:p) {
  for (j in 1:i) {
    if (i != j && abs(mat_cor[i, j]) > 0.99) {
      redondances[[length(redondances) + 1]] <- c(i,j, mat_cor[i, j])
    }
  }
}
cor(df$PAR_ASE_M, df$PAR_ASE_MV) 

cor(df$PAR_SFM_M, df$PAR_SFM_MV)

print(redondances) #On remarque que les lignes 128:147 et 148:167 sont égales ont peut donc les enlever
#à part ça il reste deux couple de variables très corrélé je sais pas pk et je sais pas quoi en faire

#Question 2 - ACP
library(FactoMineR)
library(factoextra)
df$PAR_SC_V <- log(df$PAR_SC_V)
df$PAR_ASC_V <- log(df$PAR_ASC_V)
df<- df[, -(148:167)]
p = ncol(df)
n = nrow(df)

#variables centrée réduite
X = scale(df[,-p],center=TRUE,scale=TRUE)/sqrt((n-1)/n)

res = PCA(X[,-p])
res 
#valeurs propres et vecteurs propres

#round(res$eig,4) 
sum(res$eig[,1])

barplot(res$eig[,2],main="% inertie",names=paste("Dim",1:nrow(res$eig)))
abline(h=100/171,lty=2)

ggplot()+ aes(x=1:length(res$eig[,2]),y=res$eig[,2]) + geom_col() + 
  geom_hline(yintercept=100/171, lty=2) +
  ggtitle("% inertie") + xlab("") + ylab("")


fviz_eig(res, addlabels = TRUE, 
         main = "Eboulis des valeurs propres") +
  geom_hline(yintercept=100/171)

# variables et cercle des corrélations



fviz_pca_var(res, 
             select.var = list(contrib = 15), 
             col.var = "contrib", 
             labelsize = 2,   
             repel = TRUE)  

fviz_pca_var(res, 
             axes = c(2, 3),                  
             select.var = list(contrib = 15), 
             col.var = "contrib", 
             labelsize = 2,   
             repel = TRUE)


#individus et étude simultanée individus/variables dans le premier plan

plt1 = plot(res,axes = c(1,2), choix = "ind",label="none", col.ind = adjustcolor("black", alpha.f = 0.2))
plt2 = plot(res,axes = c(1,2), choix = "var")
cowplot::plot_grid(plt1, plt2, ncol = 2, nrow = 1)

Ind = res$ind


# contributions
apply(Ind$contrib,2,which.max)   # le plus contributif sur chaque axe

head(sort(Ind$contrib[, 1],decreasing=TRUE)) # les premiers contibutifs du premier axe

# cosinus carré : qualité de représentration
which.max(Ind$cos2[,2])


#plan (2,3)


plt3 = plot(res,axes = c(2,3), choix = "ind",label="none",col.ind = adjustcolor("black", alpha.f = 0.2))
plt4 = plot(res,axes = c(2,3), choix = "var")
cowplot::plot_grid(plt3,plt4)

#contributif et mal représenté sur l'axe 3
which(res$ind$cos2[,3]<0.15 & res$ind$contrib[,3]>1)

c(contrib=Ind$contrib[89,3],cos2= Ind$cos2[89,3])
 

# globalement sur le plan (2,3)
mean(res$ind$coord[,2]^2+res$ind$coord[,3]^2)  
mean(res$ind$cos2[,2]+res$ind$cos2[,3])       

cowplot::plot_grid(plt1, plt2, plt3, plt4, ncol = 2, nrow = 2)

#Question 3 :CAH

library(mclust)
library(cowplot)

# matrice de distance
d_music <- dist(X, method = "euclidean")

#méthode de Ward
res_ward <- hclust(d_music, method = "ward.D2")

#Visualisation du Dendrogramme
 
plot(res_ward, labels = FALSE, main = "Dendrogramme (Méthode de Ward)", xlab = "", sub = "")
abline(h = 60, col = "red", lty = 2)

clusters_ward <- cutree(res_ward, k = 6)

#silhouette
sil_ward <- silhouette(clusters_ward, d_music)
p_sil1 <- fviz_silhouette(sil_ward, main = "Silhouette : Groupes Ward (Automatique)")

# Silhouette pour les genres réels
genres_num <- as.numeric(as.factor(df$GENRE))
sil_genre <- silhouette(genres_num, d_music)
p_sil2 <- fviz_silhouette(sil_genre, main = "Silhouette : Genres Réels (Humain)")

# Affichage comparatif
plot_grid(p_sil1, p_sil2, ncol = 1)

#Comparaison statistique
# Indice de Rand Ajusté (ARI) : mesure la concordance entre Ward et le Genre
ari <- adjustedRandIndex(clusters_ward, df$GENRE)
cat("--------------------------------------------\n")
cat("Indice de Rand Ajusté (ARI) :", round(ari, 4), "\n")
cat("--------------------------------------------\n")

# Visualisation des clusters sur l'ACP
fviz_cluster(list(data = X, cluster = clusters_ward),
             geom = "point", 
             ellipse.type = "convex",
             palette = "jco",
             alpha.ind = 0.1,
             main = "Clusters Ward projetés sur l'ACP (Dim 1 & 2)")



##### Partie 2


rm(list = objects())

getwd()
# setwd(chemin)

df_music <- read.table("./Music_2026.txt", header = T, sep = ";")
df_music$PAR_SC_V <- log(df_music$PAR_SC_V)
df_music$PAR_ASC_V <- log(df_music$PAR_ASC_V)
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
#on enregistre en fichier rds puisque le calcul est (très) long
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
# Y ~ PAR_TC + PAR_ASE1 + PAR_ASE3 + PAR_ASE5 + PAR_ASE6 + PAR_ASE7 + 
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
#     PAR_ASE_MV + PAR_ASC + PAR_ASC_V + PAR_ASS + PAR_ASS_V + 
#     PAR_SFM1 + PAR_SFM2 + PAR_SFM3 + PAR_SFM4 + PAR_SFM5 + PAR_SFM6 + 
#     PAR_SFM7 + PAR_SFM8 + PAR_SFM9 + PAR_SFM10 + PAR_SFM11 + 
#     PAR_SFM12 + PAR_SFM13 + PAR_SFM14 + PAR_SFM15 + PAR_SFM16 + 
#     PAR_SFM17 + PAR_SFM18 + PAR_SFM19 + PAR_SFM20 + PAR_SFM21 + 
#     PAR_SFM22 + PAR_SFM23 + PAR_SFM_M + PAR_SFMV1 + PAR_SFMV2 + 
#     PAR_SFMV3 + PAR_SFMV4 + PAR_SFMV5 + PAR_SFMV6 + PAR_SFMV7 + 
#     PAR_SFMV8 + PAR_SFMV9 + PAR_SFMV10 + PAR_SFMV11 + PAR_SFMV12 + 
#     PAR_SFMV13 + PAR_SFMV14 + PAR_SFMV15 + PAR_SFMV16 + PAR_SFMV17 + 
#     PAR_SFMV18 + PAR_SFMV19 + PAR_SFMV20 + PAR_SFMV21 + PAR_SFMV22 + 
#     PAR_SFMV23 + PAR_SFMV24 + PAR_SFM_MV + PAR_MFCC3 + PAR_MFCC4 + 
#     PAR_MFCC6 + PAR_MFCC8 + PAR_MFCC9 + PAR_MFCC10 + PAR_MFCC11 + 
#     PAR_MFCC12 + PAR_MFCC15 + PAR_MFCC16 + PAR_MFCC17 + PAR_MFCC19 + 
#     PAR_MFCC20 + PAR_THR_2RMS_TOT + PAR_THR_3RMS_TOT + PAR_THR_1RMS_10FR_MEAN + 
#     PAR_THR_1RMS_10FR_VAR + PAR_THR_2RMS_10FR_VAR + PAR_THR_3RMS_10FR_MEAN + 
#     PAR_THR_3RMS_10FR_VAR + PAR_PEAK_RMS10FR_MEAN + PAR_PEAK_RMS10FR_VAR + 
#     PAR_1RMS_TCD + PAR_2RMS_TCD + PAR_3RMS_TCD + PAR_1RMS_TCD_10FR_MEAN + 
#     PAR_1RMS_TCD_10FR_VAR + PAR_2RMS_TCD_10FR_MEAN
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






##### Partie 3


rm(list = objects())

getwd()
# setwd(chemin)

df_music <- read.table("./Music_2026.txt", header = T, sep = ";")
df_music$PAR_SC_V <- log(df_music$PAR_SC_V)
df_music$PAR_ASC_V <- log(df_music$PAR_ASC_V)
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

# matrice indicatrice des classes
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

