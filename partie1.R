
rm(list=objects());graphics.off()
setwd("C:/Users/plays/Documents/ENSTA/2A STIC/STA03/Projet")
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
#On cherche les variable trop corrélée (corrélation > 0.99)
p <- ncol(mat_cor)
redondances <- list()

for (i in 1:p) {
  for (j in 1:i) {
    if (i != j && abs(mat_cor[i, j]) > 0.99) {
      redondances[[length(redondances) + 1]] <- c(i,j, mat_cor[i, j])
    }
  }
}
cor(df$PAR_ASE_M, df$PAR_ASE_MV) #sur celles la jsp quoi dire

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