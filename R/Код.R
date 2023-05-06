library(readxl)
library(openxlsx)
library(dplyr)
library(tidyr)
library(hablar)
library(ggplot2)
library(stargazer)
library(factoextra)
library(FactoMineR)
library(corrplot)
library(deaR)
library(Benchmarking)
library(SensoMineR)
library(kohonen)
library(gtsummary)
library(gt)
library(RColorBrewer)
library(car)
library(MASS)
library(skedastic)
library(lmtest)
library(plm)
library(randomForest)
library(randomForestExplainer)
library(stats)
library(flexclust)
library(cluster)
library(AER)
library(plotrix)

data <- read_excel("Data.xlsx")
data <- data[-1 , ] #удалим строку с единицами измерения
data <- data %>% retype()
data <- as.data.frame(data)
data <- data[, c(1,2,3,4,5,6,7,14,
                 8,9,10,11,12,
                 16,17,18,19,43,44,
                 15,
                 24,25,26,27,28,
                 29,33,34,36,
                 31,32,
                 37,38,39,40,41,48,
                 20,21,22,23,
                 35,
                 45,46,
                 30)]

###Описательные статистики###
data_sum <- dplyr::select(data, -Year, -`1519`, -`2029`, -`3039`, -`4049`, -`5059`)
stargazer(data_sum, summary = TRUE, title = 'Описательные статистики', type = "text", out = 'statistics1.html', digits = 1)
data <- mutate(data, `1539` = `1519`+`2029`+`3039`)
data <- mutate(data, `4059` = `4049`+`5059`)
data <- dplyr::select(data, -`1519`, -`2029`, -`3039`, -`4049`, -`5059`)
data_sum <- dplyr::select(data, -Year)
stargazer(data_sum, summary = TRUE, title = 'Описательные статистики', type = "text", out = 'statistics2.html', digits = 0)

data_corr <- na.omit(dplyr::select(data, - Region, -Year))
corrplot(cor(data_corr), tl.cex = 0.6, tl.col = 'black')

####Стандартизация####
min_max_norm <- function (x) {
  (x - min(x)) / (max(x) - min(x))
}

data_norm <- as.data.frame(apply(data[-c(1:80),-c(1,2)], MARGIN = 2, min_max_norm)) #по столбцам
data_norm$Year <- data$Year[-c(1:80)]
data_norm$Region <- data$Region[-c(1:80)]
data_norm <- data_norm %>% dplyr::select(Year, everything() )
data_norm <- data_norm %>% dplyr::select(Region, everything() )

####Группировка####
#возраст: 41-42
#население: 3
#отсутствие высшего образования: 5-8
#высшее образование: 4, 9-14
#занятость: 15
#занятость RD: 16-20
#компании: 21-24
#компьютеры: 25-26
#затраты RD: 27-32
#патенты: 33-36
#продукты: 37
#ВРП: 38-39
#преступность: 40

data_group <- as.data.frame(dplyr::select(data_norm, Region, Year, `1539`, `4059`, population))
data_group$nohigheduc <- pull(1/4*(data_norm[5]+data_norm[6]+data_norm[7]+data_norm[8]))
data_group$higheduc <- pull(1/7*(data_norm[4]+data_norm[9]+data_norm[10]+data_norm[11]+data_norm[12]+data_norm[13]+data_norm[14]))
data_group$employment <- data_norm$employment
data_group$employRD <- pull(1/5*(data_norm[16]+data_norm[17]+data_norm[18]+data_norm[19]+data_norm[20]))
data_group$company <- pull(1/4*(data_norm[21]+data_norm[22]+data_norm[23]+data_norm[24]))
data_group$computer <- pull(1/2*(data_norm[25]+data_norm[26]))
data_group$expendRD <- pull(1/6*(data_norm[27]+data_norm[28]+data_norm[29]+data_norm[30]+data_norm[31]+data_norm[32]))
data_group$patent <- pull(1/4*(data_norm[33]+data_norm[34]+data_norm[35]+data_norm[36]))
data_group$product <- data_norm$innovationproduct
data_group$GRP <- pull(1/2*(data_norm[38]+data_norm[39]))
data_group$crime <- data_norm$crime

####k-cluster####
data_k <- data_group %>% 
  filter(Region != 'г.Москва') %>% 
  filter(Region != 'г.Санкт0Петербург')
data_kk <- data_k[,-c(1,2,5,8,15,16)]

fviz_nbclust(data_kk, kmeans, method = "wss")
k <- kmeans(data_kk, centers = 7)
fviz_cluster(k, data = data_kk, palette = "Paired")
k_data <- dplyr::select(data_k, Region, Year)
k_data$cluster <- k$cluster
#write.xlsx(k_data, 'k_new.xlsx')

fviz_nbclust(data_kk, pam, method = "wss")
kk <- pam(data_kk, k = 9, metric = "euclidean", stand = FALSE)
fviz_cluster(kk, data = data_kk, palette = "Paired")
kk_data <- dplyr::select(data_k, Region, Year)
kk_data$cluster <- kk$cluster
#write.xlsx(kk_data, 'kk_new.xlsx')

####МИИРИС####
miiris <- read_xlsx("МИИРИС.xlsx", sheet = 2)
miiris$regul <- as.factor(miiris$regul)
miiris$infra <- as.factor(miiris$infra)
data_new <- data_group
data_new$regul <- miiris$regul
data_new$infra <- miiris$infra

####DEA####
data_dea <- data_group %>% unite('Region_Year', 1:2, remove = TRUE)
dea <- read_data(datadea = data_dea,
                   inputs = c(2,3,5,6,8,9,10,11),
                   outputs = c(12,13))
mod <- model_basic(dea) 
plot(mod)
eff <- deaR::efficiencies(mod)
data_dea$eff <- eff
data_dea[eff == 1, "Region_Year"]

#####
data19 <-  filter(data_new, Year == 2019)
data18 <-  filter(data_new, Year == 2018)
data17 <-  filter(data_new, Year == 2017)
data16 <-  filter(data_new, Year == 2016)
data15 <-  filter(data_new, Year == 2015)
data14 <-  filter(data_new, Year == 2014)
data13 <-  filter(data_new, Year == 2013)
data12 <-  filter(data_new, Year == 2012)
data11 <-  filter(data_new, Year == 2011)
data10 <-  filter(data_new, Year == 2010)
data09 <-  filter(data_new, Year == 2009)
data08 <-  filter(data_new, Year == 2008)
data07 <-  filter(data_new, Year == 2007)
data06 <-  filter(data_new, Year == 2006)
data05 <-  filter(data_new, Year == 2005)
data04 <-  filter(data_new, Year == 2004)
data03 <-  filter(data_new, Year == 2003)
data02 <-  filter(data_new, Year == 2002)
data01 <-  filter(data_new, Year == 2001)

#####
A <- function(x) {
  ifelse(x > quantile(x, probs = 7/8), 1, 0)
}

####DEA2019####
data_dea19 <- dplyr::select(data19, -Year)
dea19 <- read_data(datadea = data_dea19,
                   inputs = c(2,3,5,6,8,9,10,11),
                   outputs = c(12,13))
mod <- model_basic(dea19) 
# summary(mod)
# plot(mod)
# масштаб
# rts(mod)
# eff <- deaR::efficiencies(mod)
# plot(eff)
mod19 <- model_multiplier(dea19)
M19 <- as.data.frame(multipliers(mod19))

#Кластеризация
m19 <- apply(M19, MARGIN = 2, A) #по столбцам
# dist <- dist(m19)
# mod_clust <- hclust(dist, method = 'average')
# plot(mod_clust, ann = FALSE, labels = FALSE, hang = -1)
# #кластеры
# rect.hclust(mod_clust, k = 2)
# #номер кластера
# N <- cutree(mod_clust, k = 2)
# table(N)
# 
# m19 <- as.data.frame(m19)
# m19$N <- N
# colMeans(m19[N==2,])

#PCA
#без Чукотского АО, Еврейской АО
# pca <- PCA(M19[-c(13,79),-c(9,10)], graph = FALSE)
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M19$multiplier_output.patent[-c(13,79)])
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M19$multiplier_output.product[-c(13,79)])
# mod_cl <- HCPC(pca, nb.clust = 4, graph = FALSE)
# fviz_cluster(mod_cl, palette = "Paired", ggtheme = theme_classic(), repel = TRUE)
# data_cl <- mod_cl$data.clust
# data_cl <- dplyr::select(data_cl, clust)

####DEA2018####
data_dea18 <- dplyr::select(data18, -Year)
dea18 <- read_data(datadea = data_dea18,
                   inputs = c(2,3,5,6,8,9,10,11),
                   outputs = c(12,13))
mod <- model_basic(dea18)
# summary(mod)
# plot(mod)
# масштаб
# rts(mod)
# eff <- deaR::efficiencies(mod)
# plot(eff)
mod18 <- model_multiplier(dea18)
M18 <- as.data.frame(multipliers(mod18))

#Кластеризация
m18 <- apply(M18, MARGIN = 2, A) #по столбцам
# dist <- dist(m18)
# mod_clust <- hclust(dist, method = 'average')
# plot(mod_clust, ann = FALSE, labels = FALSE, hang = -1)
# #кластеры
# rect.hclust(mod_clust, k = 2)
# #номер кластера
# N <- cutree(mod_clust, k = 2)
# table(N)
# 
# m18 <- as.data.frame(m18)
# m18$N <- N
# colMeans(m18[N==2,])

#PCA
#без Чукотского АО, Еврейской АО
# pca <- PCA(M18[-c(13,79),-c(9,10)], graph = FALSE)
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M18$multiplier_output.patent[-c(13,79)])
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M18$multiplier_output.product[-c(13,79)])
# mod_cl <- HCPC(pca, nb.clust = 4, graph = FALSE)
# fviz_cluster(mod_cl, palette = "Paired", ggtheme = theme_minimal(), show.clust.cent = FALSE, repel = TRUE)
# data_cl <- mod_cl$data.clust
# data_cl <- dplyr::select(data_cl, clust)

####DEA2017####
data_dea17 <- dplyr::select(data17, -Year)
dea17 <- read_data(datadea = data_dea17,
                   inputs = c(2,3,5,6,8,9,10,11),
                   outputs = c(12,13))
mod <- model_basic(dea17)
# summary(mod)
# plot(mod)
# масштаб
# rts(mod)
# eff <- deaR::efficiencies(mod)
# plot(eff)
mod17 <- model_multiplier(dea17)
M17 <- as.data.frame(multipliers(mod17))

#Кластеризация
m17 <- apply(M17, MARGIN = 2, A) #по столбцам
# dist <- dist(m17)
# mod_clust <- hclust(dist, method = 'average')
# plot(mod_clust, ann = FALSE, labels = FALSE, hang = -1)
# #кластеры
# rect.hclust(mod_clust, k = 3)
# #номер кластера
# N <- cutree(mod_clust, k = 3)
# table(N)
# 
# m17 <- as.data.frame(m17)
# m17$N <- N
# colMeans(m17[N==2,])
# colMeans(m17[N==3,])

#PCA
#без Чукотского АО, Еврейской АО
# pca <- PCA(M17[-c(13,79),-c(9,10)], graph = FALSE)
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M17$multiplier_output.patent[-c(13,79)])
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M17$multiplier_output.product[-c(13,79)])
# mod_cl <- HCPC(pca, nb.clust = 4, graph = FALSE)
# fviz_cluster(mod_cl, palette = "Paired", ggtheme = theme_classic(), repel = TRUE)
# data_cl <- mod_cl$data.clust
# data_cl <- dplyr::select(data_cl, clust)

####DEA2016####
data_dea16 <- dplyr::select(data16, -Year)
dea16 <- read_data(datadea = data_dea16,
                   inputs = c(2,3,5,6,8,9,10,11),
                   outputs = c(12,13))
mod <- model_basic(dea16)
# summary(mod)
#plot(mod)
#масштаб
#rts(mod)
# eff <- deaR::efficiencies(mod)
# plot(eff)
mod16 <- model_multiplier(dea16)
M16 <- as.data.frame(multipliers(mod16))

#Кластеризация
m16 <- apply(M16, MARGIN = 2, A) #по столбцам
# dist <- dist(m16)
# mod_clust <- hclust(dist, method = 'average')
# plot(mod_clust, ann = FALSE, labels = FALSE, hang = -1)
# #кластеры
# rect.hclust(mod_clust, k = 2)
# #номер кластера
# N <- cutree(mod_clust, k = 2)
# table(N)
# 
# m16 <- as.data.frame(m16)
# m16$N <- N
# colMeans(m16[N==2,])

#PCA
#без Чукотского АО, Еврейской АО
# pca <- PCA(M16[-c(13,79),-c(9,10)], graph = FALSE)
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M16$multiplier_output.patent[-c(13,79)])
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M16$multiplier_output.product[-c(13,79)])
# mod_cl <- HCPC(pca, nb.clust = 4, graph = FALSE)
# fviz_cluster(mod_cl, palette = "Paired", ggtheme = theme_classic(), repel = TRUE)
# data_cl <- mod_cl$data.clust
# data_cl <- dplyr::select(data_cl, clust)

####DEA2015####
data_dea15 <- dplyr::select(data15, -Year)
dea15 <- read_data(datadea = data_dea15,
                   inputs = c(2,3,5,6,8,9,10,11),
                   outputs = c(12,13))
mod <- model_basic(dea15)
# summary(mod)
#plot(mod)
#масштаб
#rts(mod)
# eff <- deaR::efficiencies(mod)
# plot(eff)
mod15 <- model_multiplier(dea15)
M15 <- as.data.frame(multipliers(mod15))

#Кластеризация
m15 <- apply(M15, MARGIN = 2, A) #по столбцам
# dist <- dist(m15)
# mod_clust <- hclust(dist, method = 'average')
# plot(mod_clust, ann = FALSE, labels = FALSE, hang = -1)
# #кластеры
# rect.hclust(mod_clust, k = 2)
# #номер кластера
# N <- cutree(mod_clust, k = 2)
# table(N)
# 
# m15 <- as.data.frame(m15)
# m15$N <- N
# colMeans(m15[N==2,])

#PCA
#без Чукотского АО, Еврейской АО, Костромской О
# pca <- PCA(M15[-c(13,24,79),-c(9,10)], graph = FALSE)
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M15$multiplier_output.patent[-c(13,24,79)])
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M15$multiplier_output.product[-c(13,24,79)])
# mod_cl <- HCPC(pca, nb.clust = 4, graph = FALSE)
# fviz_cluster(mod_cl, palette = "Paired", ggtheme = theme_classic(), repel = TRUE)
# data_cl <- mod_cl$data.clust
# data_cl <- dplyr::select(data_cl, clust)

####DEA2014####
data_dea14 <- dplyr::select(data14, -Year)
dea14 <- read_data(datadea = data_dea14,
                   inputs = c(2,3,5,6,8,9,10,11),
                   outputs = c(12,13))
mod <- model_basic(dea14)
# summary(mod)
#plot(mod)
#масштаб
#rts(mod)
# eff <- deaR::efficiencies(mod)
# plot(eff)
mod14 <- model_multiplier(dea14)
M14 <- as.data.frame(multipliers(mod14))

#Кластеризация
m14 <- apply(M14, MARGIN = 2, A) #по столбцам
# dist <- dist(m14)
# mod_clust <- hclust(dist, method = 'average')
# plot(mod_clust, ann = FALSE, labels = FALSE, hang = -1)
# #кластеры
# rect.hclust(mod_clust, k = 2)
# #номер кластера
# N <- cutree(mod_clust, k = 2)
# table(N)
# 
# m14 <- as.data.frame(m14)
# m14$N <- N
# colMeans(m14[N==2,])

#PCA
#без Чукотского АО, Еврейской АО
# pca <- PCA(M14[-c(13,79),-c(9,10)], graph = FALSE)
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M14$multiplier_output.patent[-c(13,79)])
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M14$multiplier_output.product[-c(13,79)])
# mod_cl <- HCPC(pca, nb.clust = 4, graph = FALSE)
# fviz_cluster(mod_cl, palette = "Paired", ggtheme = theme_classic(), repel = TRUE)
# data_cl <- mod_cl$data.clust
# data_cl <- dplyr::select(data_cl, clust)

####DEA2013####
data_dea13 <- dplyr::select(data13, -Year)
dea13 <- read_data(datadea = data_dea13,
                   inputs = c(2,3,5,6,8,9,10,11),
                   outputs = c(12,13))
mod <- model_basic(dea13)
# summary(mod)
#plot(mod)
#масштаб
#rts(mod)
# eff <- deaR::efficiencies(mod)
# plot(eff)
mod13 <- model_multiplier(dea13)
M13 <- as.data.frame(multipliers(mod13))

#Кластеризация
m13 <- apply(M13, MARGIN = 2, A) #по столбцам
# dist <- dist(m13)
# mod_clust <- hclust(dist, method = 'average')
# plot(mod_clust, ann = FALSE, labels = FALSE, hang = -1)
# #кластеры
# rect.hclust(mod_clust, k = 2)
# #номер кластера
# N <- cutree(mod_clust, k = 2)
# table(N)

# m13 <- as.data.frame(m13)
# m13$N <- N
# colMeans(m13[N==2,])

#PCA
#без Чукотского АО, Еврейской АО
# pca <- PCA(M13[-c(13,79),-c(9,10)], graph = FALSE)
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M13$multiplier_output.patent[-c(13,79)])
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M13$multiplier_output.product[-c(13,79)])
# mod_cl <- HCPC(pca, nb.clust = 4, graph = FALSE)
# fviz_cluster(mod_cl, palette = "Paired", ggtheme = theme_classic(), repel = TRUE)
# data_cl <- mod_cl$data.clust
# data_cl <- dplyr::select(data_cl, clust)

####DEA2012####
data_dea12 <- dplyr::select(data12, -Year)
dea12 <- read_data(datadea = data_dea12,
                   inputs = c(2,3,5,6,8,9,10,11),
                   outputs = c(12,13))
mod <- model_basic(dea12)
# summary(mod)
#plot(mod)
#масштаб
#rts(mod)
# eff <- deaR::efficiencies(mod)
# plot(eff)
mod12 <- model_multiplier(dea12)
M12 <- as.data.frame(multipliers(mod12))

#Кластеризация
m12 <- apply(M12, MARGIN = 2, A) #по столбцам
# dist <- dist(m12)
# mod_clust <- hclust(dist, method = 'average')
# plot(mod_clust, ann = FALSE, labels = FALSE, hang = -1)
# #кластеры
# rect.hclust(mod_clust, k = 2)
# #номер кластера
# N <- cutree(mod_clust, k = 2)
# table(N)
# 
# m12 <- as.data.frame(m12)
# m12$N <- N
# colMeans(m12[N==2,])

#PCA
#без Чукотского АО
pca <- PCA(M12[-c(79),-c(9,10)], graph = FALSE)
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M12$multiplier_output.patent[-c(79)])
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M12$multiplier_output.product[-c(79)])
# mod_cl <- HCPC(pca, nb.clust = 4, graph = FALSE)
# fviz_cluster(mod_cl, palette = "Paired", ggtheme = theme_classic(), repel = TRUE)
# data_cl <- mod_cl$data.clust
# data_cl <- dplyr::select(data_cl, clust)

####DEA2011####
data_dea11 <- dplyr::select(data11, -Year)
dea11 <- read_data(datadea = data_dea11,
                   inputs = c(2,3,5,6,8,9,10,11),
                   outputs = c(12,13))
mod <- model_basic(dea11)
# summary(mod)
#plot(mod)
#масштаб
#rts(mod)
# eff <- deaR::efficiencies(mod)
# plot(eff)
mod11 <- model_multiplier(dea11)
M11 <- as.data.frame(multipliers(mod11))

#Кластеризация
m11 <- apply(M11, MARGIN = 2, A) #по столбцам
# dist <- dist(m11)
# mod_clust <- hclust(dist, method = 'average')
# plot(mod_clust, ann = FALSE, labels = FALSE, hang = -1)
# #кластеры
# rect.hclust(mod_clust, k = 2)
# #номер кластера
# N <- cutree(mod_clust, k = 2)
# table(N)
# 
# m11 <- as.data.frame(m11)
# m11$N <- N
# colMeans(m11[N==2,])

#PCA
#без Чукотского АО, Липецкой О, Вологодской О
# pca <- PCA(M11[-c(9,30,79),-c(9,10)], graph = FALSE)
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M11$multiplier_output.patent[-c(9,30,79)])
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M11$multiplier_output.product[-c(9,30,79)])
# mod_cl <- HCPC(pca, nb.clust = 4, graph = FALSE)
# fviz_cluster(mod_cl, palette = "Paired", ggtheme = theme_classic(), repel = TRUE)
# data_cl <- mod_cl$data.clust
# data_cl <- dplyr::select(data_cl, clust)

####DEA2010####
data_dea10 <- dplyr::select(data10, -Year)
dea10 <- read_data(datadea = data_dea10,
                   inputs = c(2,3,5,6,8,9,10,11),
                   outputs = c(12,13))
mod <- model_basic(dea10)
# summary(mod)
#plot(mod)
#масштаб
#rts(mod)
# eff <- deaR::efficiencies(mod)
# plot(eff)
mod10 <- model_multiplier(dea10)
M10 <- as.data.frame(multipliers(mod10))

#Кластеризация
m10 <- apply(M10, MARGIN = 2, A) #по столбцам
# dist <- dist(m10)
# mod_clust <- hclust(dist, method = 'average')
# plot(mod_clust, ann = FALSE, labels = FALSE, hang = -1)
# #кластеры
# rect.hclust(mod_clust, k = 3)
# #номер кластера
# N <- cutree(mod_clust, k = 3)
# table(N)
# 
# m10 <- as.data.frame(m10)
# m10$N <- N
# colMeans(m10[N==2,])
# colMeans(m10[N==3,])

#PCA
#без Чукотского АО, Еврейской АО, Костромской О
# pca <- PCA(M10[-c(13,24,79),-c(9,10)], graph = FALSE)
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M10$multiplier_output.patent[-c(13,24,79)])
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M10$multiplier_output.product[-c(13,24,79)])
# mod_cl <- HCPC(pca, nb.clust = 4, graph = FALSE)
# fviz_cluster(mod_cl, palette = "Paired", ggtheme = theme_classic(), repel = TRUE)
# data_cl <- mod_cl$data.clust
# data_cl <- dplyr::select(data_cl, clust)

####DEA2009####
data_dea09 <- dplyr::select(data09, -Year)
dea09 <- read_data(datadea = data_dea09,
                   inputs = c(2,3,5,6,8,9,10,11),
                   outputs = c(12,13))
mod <- model_basic(dea09)
# summary(mod)
#plot(mod)
#масштаб
#rts(mod)
# eff <- deaR::efficiencies(mod)
# plot(eff)
mod09 <- model_multiplier(dea09)
M09 <- as.data.frame(multipliers(mod09))

#Кластеризация
m09 <- apply(M09, MARGIN = 2, A) #по столбцам
# dist <- dist(m09)
# mod_clust <- hclust(dist, method = 'average')
# plot(mod_clust, ann = FALSE, labels = FALSE, hang = -1)
# #кластеры
# rect.hclust(mod_clust, k = 2)
# #номер кластера
# N <- cutree(mod_clust, k = 2)
# table(N)
# 
# m09 <- as.data.frame(m09)
# m09$N <- N
# colMeans(m09[N==2,])

#PCA
#без Чукотского АО, Еврейской АО, Р Ингушетия, Костромской О
# pca <- PCA(M09[-c(13,24,49,79),-c(9,10)], graph = FALSE)
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M09$multiplier_output.patent[-c(13,24,49,79)])
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M09$multiplier_output.product[-c(13,24,49,79)])
# mod_cl <- HCPC(pca, nb.clust = 4, graph = FALSE)
# fviz_cluster(mod_cl, palette = "Paired", ggtheme = theme_classic(), repel = TRUE)
# data_cl <- mod_cl$data.clust
# data_cl <- dplyr::select(data_cl, clust)

####DEA2008####
data_dea08 <- dplyr::select(data08, -Year)
dea08 <- read_data(datadea = data_dea08,
                   inputs = c(2,3,5,6,8,9,10,11),
                   outputs = c(12,13))
mod <- model_basic(dea08)
# summary(mod)
#plot(mod)
#масштаб
#rts(mod)
# eff <- deaR::efficiencies(mod)
# plot(eff)
mod08 <- model_multiplier(dea08)
M08 <- as.data.frame(multipliers(mod08))

#Кластеризация
m08 <- apply(M08, MARGIN = 2, A) #по столбцам
# dist <- dist(m08)
# mod_clust <- hclust(dist, method = 'average')
# plot(mod_clust, ann = FALSE, labels = FALSE, hang = -1)
# #кластеры
# rect.hclust(mod_clust, k = 3)
# #номер кластера
# N <- cutree(mod_clust, k = 3)
# table(N)
# 
# m08 <- as.data.frame(m08)
# m08$N <- N
# colMeans(m08[N==2,])
# colMeans(m08[N==3,])

#PCA
#без Чукотского АО, Р Ингушетия, Костромской О, Р Марий Эл   
pca <- PCA(M08[-c(24,49,53,79),-c(9,10)], graph = FALSE)
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M08$multiplier_output.patent[-c(24,49,53,79)])
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M08$multiplier_output.product[-c(24,49,53,79)])
# mod_cl <- HCPC(pca, nb.clust = 4, graph = FALSE)
# fviz_cluster(mod_cl, palette = "Paired", ggtheme = theme_classic(), repel = TRUE)
# data_cl <- mod_cl$data.clust
# data_cl <- dplyr::select(data_cl, clust)

####DEA2007####
data_dea07 <- dplyr::select(data07, -Year)
dea07 <- read_data(datadea = data_dea07,
                   inputs = c(2,3,5,6,8,9,10,11),
                   outputs = c(12,13))
mod <- model_basic(dea07)
# summary(mod)
#plot(mod)
#масштаб
#rts(mod)
# eff <- deaR::efficiencies(mod)
# plot(eff)
mod07 <- model_multiplier(dea07)
M07 <- as.data.frame(multipliers(mod07))

#Кластеризация
m07 <- apply(M07, MARGIN = 2, A) #по столбцам
# dist <- dist(m07)
# mod_clust <- hclust(dist, method = 'average')
# plot(mod_clust, ann = FALSE, labels = FALSE, hang = -1)
# #кластеры
# rect.hclust(mod_clust, k = 2)
# #номер кластера
# N <- cutree(mod_clust, k = 2)
# table(N)
# 
# m07 <- as.data.frame(m07)
# m07$N <- N
# colMeans(m07[N==2,])

#PCA
#без Чукотского АО, Р Ингушетия, Костромской О
# pca <- PCA(M07[-c(24,49,79),-c(9,10)], graph = FALSE)
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M07$multiplier_output.patent[-c(24,49,79)])
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M07$multiplier_output.product[-c(24,49,79)])
# mod_cl <- HCPC(pca, nb.clust = 4, graph = FALSE)
# fviz_cluster(mod_cl, palette = "Paired", ggtheme = theme_classic(), repel = TRUE)
# data_cl <- mod_cl$data.clust
# data_cl <- dplyr::select(data_cl, clust)

####DEA2006####
data_dea06 <- dplyr::select(data06, -Year)
dea06 <- read_data(datadea = data_dea06,
                   inputs = c(2,3,5,6,8,9,10,11),
                   outputs = c(12,13))
mod <- model_basic(dea06)
# summary(mod)
#plot(mod)
#масштаб
#rts(mod)
# eff <- deaR::efficiencies(mod)
# plot(eff)
mod06 <- model_multiplier(dea06)
M06 <- as.data.frame(multipliers(mod06))

#Кластеризация
m06 <- apply(M06, MARGIN = 2, A) #по столбцам
# dist <- dist(m06)
# mod_clust <- hclust(dist, method = 'average')
# plot(mod_clust, ann = FALSE, labels = FALSE, hang = -1)
# #кластеры
# rect.hclust(mod_clust, k = 2)
# #номер кластера
# N <- cutree(mod_clust, k = 2)
# table(N)
# 
# m06 <- as.data.frame(m06)
# m06$N <- N
# colMeans(m06[N==2,])

#PCA
#без Чукотского АО, Еврейской АО, Р Ингушетия, Костромской О
# pca <- PCA(M06[-c(13,24,49,79),-c(9,10)], graph = FALSE)
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M06$multiplier_output.patent[-c(13,24,49,79)])
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M06$multiplier_output.product[-c(13,24,49,79)])
# mod_cl <- HCPC(pca, nb.clust = 4, graph = FALSE)
# fviz_cluster(mod_cl, palette = "Paired", ggtheme = theme_classic(), repel = TRUE)
# data_cl <- mod_cl$data.clust
# data_cl <- dplyr::select(data_cl, clust)

####DEA2005####
data_dea05 <- dplyr::select(data05, -Year)
dea05 <- read_data(datadea = data_dea05,
                   inputs = c(2,3,5,6,8,9,10,11),
                   outputs = c(12,13))
mod <- model_basic(dea05)
# summary(mod)
#plot(mod)
#масштаб
#rts(mod)
# eff <- deaR::efficiencies(mod)
# plot(eff)
mod05 <- model_multiplier(dea05)
M05 <- as.data.frame(multipliers(mod05))

#Кластеризация
m05 <- apply(M05, MARGIN = 2, A) #по столбцам
# dist <- dist(m05)
# mod_clust <- hclust(dist, method = 'average')
# plot(mod_clust, ann = FALSE, labels = FALSE, hang = -1)
# #кластеры
# rect.hclust(mod_clust, k = 4)
# #номер кластера
# N <- cutree(mod_clust, k = 4)
# table(N)
# 
# m05 <- as.data.frame(m05)
# m05$N <- N
# colMeans(m05[N==2,])
# colMeans(m05[N==3,])
# colMeans(m05[N==4,])

#PCA
#без Чеченской Р, Р Ингушетия
# pca <- PCA(M05[-c(49,77),-c(9,10)], graph = FALSE)
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M05$multiplier_output.patent[-c(49,77)])
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M05$multiplier_output.product[-c(49,77)])
# mod_cl <- HCPC(pca, nb.clust = 4, graph = FALSE)
# fviz_cluster(mod_cl, palette = "Paired", ggtheme = theme_classic(), repel = TRUE)
# data_cl <- mod_cl$data.clust
# data_cl <- dplyr::select(data_cl, clust)

####DEA2004####
data_dea04 <- dplyr::select(data04, -Year)
dea04 <- read_data(datadea = data_dea04,
                   inputs = c(2,3,5,6,8,9,10,11),
                   outputs = c(12,13))
mod <- model_basic(dea04)
# summary(mod)
#plot(mod)
#масштаб
#rts(mod)
# eff <- deaR::efficiencies(mod)
# plot(eff)
mod04 <- model_multiplier(dea04)
M04 <- as.data.frame(multipliers(mod04))

#Кластеризация
m04 <- apply(M04, MARGIN = 2, A) #по столбцам
# dist <- dist(m04)
# mod_clust <- hclust(dist, method = 'average')
# plot(mod_clust, ann = FALSE, labels = FALSE, hang = -1)
# #кластеры
# rect.hclust(mod_clust, k = 2)
# #номер кластера
# N <- cutree(mod_clust, k = 2)
# table(N)
# 
# m04 <- as.data.frame(m04)
# m04$N <- N
# colMeans(m04[N==2,])

#PCA
#без Чеченской Р, Р Ингушетия, Р Адыгея, Чукотского АО, Костромской О
# pca <- PCA(M04[-c(24,44,49,77,79),-c(9,10)], graph = FALSE)
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M04$multiplier_output.patent[-c(24,44,49,77,79)])
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M04$multiplier_output.product[-c(24,44,49,77,79)])
# mod_cl <- HCPC(pca, nb.clust = 4, graph = FALSE)
# fviz_cluster(mod_cl, palette = "Paired", ggtheme = theme_classic(), repel = TRUE)
# data_cl <- mod_cl$data.clust
# data_cl <- dplyr::select(data_cl, clust)

####DEA2003####
data_dea03 <- dplyr::select(data03, -Year)
dea03 <- read_data(datadea = data_dea03,
                   inputs = c(2,3,5,6,8,9,10,11),
                   outputs = c(12,13))
mod <- model_basic(dea03)
# summary(mod)
#plot(mod)
#масштаб
#rts(mod)
# eff <- deaR::efficiencies(mod)
# plot(eff)
mod03 <- model_multiplier(dea03)
M03 <- as.data.frame(multipliers(mod03))

#Кластеризация
m03 <- apply(M03, MARGIN = 2, A) #по столбцам
# dist <- dist(m03)
# mod_clust <- hclust(dist, method = 'average')
# plot(mod_clust, ann = FALSE, labels = FALSE, hang = -1)
# #кластеры
# rect.hclust(mod_clust, k = 2)
# #номер кластера
# N <- cutree(mod_clust, k = 2)
# table(N)
# 
# m03 <- as.data.frame(m03)
# m03$N <- N
# colMeans(m03[N==2,])

#PCA
#без Чеченской Р, Р Ингушетия, Чукотского АО
# pca <- PCA(M03[-c(49,77,79),-c(9,10)], graph = FALSE)
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M03$multiplier_output.patent[-c(49,77,79)])
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M03$multiplier_output.product[-c(49,77,79)])
# mod_cl <- HCPC(pca, nb.clust = 4, graph = FALSE)
# fviz_cluster(mod_cl, palette = "Paired", ggtheme = theme_classic(), repel = TRUE)
# data_cl <- mod_cl$data.clust
# data_cl <- dplyr::select(data_cl, clust)

####DEA2002####
data_dea02 <- dplyr::select(data02, -Year)
dea02 <- read_data(datadea = data_dea02,
                   inputs = c(2,3,5,6,8,9,10,11),
                   outputs = c(12,13))
mod <- model_basic(dea02)
# summary(mod)
#plot(mod)
#масштаб
#rts(mod)
# eff <- deaR::efficiencies(mod)
# plot(eff)
mod02 <- model_multiplier(dea02)
M02 <- as.data.frame(multipliers(mod02))

#Кластеризация
m02 <- apply(M02, MARGIN = 2, A) #по столбцам
# dist <- dist(m02)
# mod_clust <- hclust(dist, method = 'average')
# plot(mod_clust, ann = FALSE, labels = FALSE, hang = -1)
# #кластеры
# rect.hclust(mod_clust, k = 2)
# #номер кластера
# N <- cutree(mod_clust, k = 2)
# table(N)
# 
# m02 <- as.data.frame(m02)
# m02$N <- N
# colMeans(m02[N==2,])

#PCA
#без Еврейской АО, Костромской О, Вологодской О
# pca <- PCA(M02[-c(9,13,24),-c(9,10)], graph = FALSE)
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M02$multiplier_output.patent[-c(9,13,24)])
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M02$multiplier_output.product[-c(9,13,24)])
# mod_cl <- HCPC(pca, nb.clust = 4, graph = FALSE)
# fviz_cluster(mod_cl, palette = "Paired", ggtheme = theme_classic(), repel = TRUE)
# data_cl <- mod_cl$data.clust
# data_cl <- dplyr::select(data_cl, clust)

####DEA2001####
data_dea01 <- dplyr::select(data01, -Year)
dea01 <- read_data(datadea = data_dea01,
                   inputs = c(2,3,5,6,8,9,10,11),
                   outputs = c(12,13))
mod <- model_basic(dea01)
# summary(mod)
#plot(mod)
#масштаб
#rts(mod)
# eff <- deaR::efficiencies(mod)
# plot(eff)
mod01 <- model_multiplier(dea01)
M01 <- as.data.frame(multipliers(mod01))

#Кластеризация
m01 <- apply(M01, MARGIN = 2, A) #по столбцам
# dist <- dist(m01)
# mod_clust <- hclust(dist, method = 'average')
# plot(mod_clust, ann = FALSE, labels = FALSE, hang = -1)
# #кластеры
# rect.hclust(mod_clust, k = 2)
# #номер кластера
# N <- cutree(mod_clust, k = 2)
# table(N)

# m01 <- as.data.frame(m01)
# m01$N <- N
# colMeans(m01[N==2,])

#PCA
#без Еврейской АО
# pca <- PCA(M01[-c(13),-c(9,10)], graph = FALSE)
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M01$multiplier_output.patent[-c(13)])
# fviz_pca_ind(pca, repel = TRUE, pointshape = 21,
#              pointsize = M01$multiplier_output.product[-c(13)])
# mod_cl <- HCPC(pca, nb.clust = 4, graph = FALSE)
# fviz_cluster(mod_cl, palette = "Paired", ggtheme = theme_classic(), repel = TRUE)
# data_cl <- mod_cl$data.clust
# data_cl <- dplyr::select(data_cl, clust)



####Стратегии####
M01$Region <- data01$Region
m01 <- M01 %>% filter(Region %in% c("г.Москва", "РеспубликаТатарстан", "Самарскаяобласть","Ивановскаяобласть","Московскаяобласть","Вологодскаяобласть","Липецкаяобласть","РеспубликаМарийЭл","Ульяновскаяобласть"))
M02$Region <- data02$Region
m02 <- M02 %>% filter(Region %in% c("г.Москва", "РеспубликаТатарстан", "Самарскаяобласть","Ивановскаяобласть","Московскаяобласть","Вологодскаяобласть","Липецкаяобласть","РеспубликаМарийЭл","Ульяновскаяобласть"))
M03$Region <- data03$Region
m03 <- M03 %>% filter(Region %in% c("г.Москва", "РеспубликаТатарстан", "Самарскаяобласть","Ивановскаяобласть","Московскаяобласть","Вологодскаяобласть","Липецкаяобласть","РеспубликаМарийЭл","Ульяновскаяобласть"))
M04$Region <- data04$Region
m04 <- M04 %>% filter(Region %in% c("г.Москва", "РеспубликаТатарстан", "Самарскаяобласть","Ивановскаяобласть","Московскаяобласть","Вологодскаяобласть","Липецкаяобласть","РеспубликаМарийЭл","Ульяновскаяобласть"))
M05$Region <- data05$Region
m05 <- M05 %>% filter(Region %in% c("г.Москва", "РеспубликаТатарстан", "Самарскаяобласть","Ивановскаяобласть","Московскаяобласть","Вологодскаяобласть","Липецкаяобласть","РеспубликаМарийЭл","Ульяновскаяобласть"))
M06$Region <- data01$Region
m06 <- M06 %>% filter(Region %in% c("г.Москва", "РеспубликаТатарстан", "Самарскаяобласть","Ивановскаяобласть","Московскаяобласть","Вологодскаяобласть","Липецкаяобласть","РеспубликаМарийЭл","Ульяновскаяобласть"))
M07$Region <- data07$Region
m07 <- M07 %>% filter(Region %in% c("г.Москва", "РеспубликаТатарстан", "Самарскаяобласть","Ивановскаяобласть","Московскаяобласть","Вологодскаяобласть","Липецкаяобласть","РеспубликаМарийЭл","Ульяновскаяобласть"))
M08$Region <- data08$Region
m08 <- M08 %>% filter(Region %in% c("г.Москва", "РеспубликаТатарстан", "Самарскаяобласть","Ивановскаяобласть","Московскаяобласть","Вологодскаяобласть","Липецкаяобласть","РеспубликаМарийЭл","Ульяновскаяобласть"))
M09$Region <- data09$Region
m09 <- M09 %>% filter(Region %in% c("г.Москва", "РеспубликаТатарстан", "Самарскаяобласть","Ивановскаяобласть","Московскаяобласть","Вологодскаяобласть","Липецкаяобласть","РеспубликаМарийЭл","Ульяновскаяобласть"))
M10$Region <- data10$Region
m10 <- M10 %>% filter(Region %in% c("г.Москва", "РеспубликаТатарстан", "Самарскаяобласть","Ивановскаяобласть","Московскаяобласть","Вологодскаяобласть","Липецкаяобласть","РеспубликаМарийЭл","Ульяновскаяобласть"))
M11$Region <- data11$Region
m11 <- M11 %>% filter(Region %in% c("г.Москва", "РеспубликаТатарстан", "Самарскаяобласть","Ивановскаяобласть","Московскаяобласть","Вологодскаяобласть","Липецкаяобласть","РеспубликаМарийЭл","Ульяновскаяобласть"))
M12$Region <- data12$Region
m12 <- M12 %>% filter(Region %in% c("г.Москва", "РеспубликаТатарстан", "Самарскаяобласть","Ивановскаяобласть","Московскаяобласть","Вологодскаяобласть","Липецкаяобласть","РеспубликаМарийЭл","Ульяновскаяобласть"))
M13$Region <- data13$Region
m13 <- M13 %>% filter(Region %in% c("г.Москва", "РеспубликаТатарстан", "Самарскаяобласть","Ивановскаяобласть","Московскаяобласть","Вологодскаяобласть","Липецкаяобласть","РеспубликаМарийЭл","Ульяновскаяобласть"))
M14$Region <- data14$Region
m14 <- M14 %>% filter(Region %in% c("г.Москва", "РеспубликаТатарстан", "Самарскаяобласть","Ивановскаяобласть","Московскаяобласть","Вологодскаяобласть","Липецкаяобласть","РеспубликаМарийЭл","Ульяновскаяобласть"))
M15$Region <- data15$Region
m15 <- M15 %>% filter(Region %in% c("г.Москва", "РеспубликаТатарстан", "Самарскаяобласть","Ивановскаяобласть","Московскаяобласть","Вологодскаяобласть","Липецкаяобласть","РеспубликаМарийЭл","Ульяновскаяобласть"))
M16$Region <- data16$Region
m16 <- M16 %>% filter(Region %in% c("г.Москва", "РеспубликаТатарстан", "Самарскаяобласть","Ивановскаяобласть","Московскаяобласть","Вологодскаяобласть","Липецкаяобласть","РеспубликаМарийЭл","Ульяновскаяобласть"))
M17$Region <- data17$Region
m17 <- M17 %>% filter(Region %in% c("г.Москва", "РеспубликаТатарстан", "Самарскаяобласть","Ивановскаяобласть","Московскаяобласть","Вологодскаяобласть","Липецкаяобласть","РеспубликаМарийЭл","Ульяновскаяобласть"))
M18$Region <- data18$Region
m18 <- M18 %>% filter(Region %in% c("г.Москва", "РеспубликаТатарстан", "Самарскаяобласть","Ивановскаяобласть","Московскаяобласть","Вологодскаяобласть","Липецкаяобласть","РеспубликаМарийЭл","Ульяновскаяобласть"))
M19$Region <- data19$Region
m19 <- M19 %>% filter(Region %in% c("г.Москва", "РеспубликаТатарстан", "Самарскаяобласть","Ивановскаяобласть","Московскаяобласть","Вологодскаяобласть","Липецкаяобласть","РеспубликаМарийЭл","Ульяновскаяобласть"))
# write.xlsx(m01, 'm01.xlsx')
# write.xlsx(m02, 'm02.xlsx')
# write.xlsx(m03, 'm03.xlsx')
# write.xlsx(m04, 'm04.xlsx')
# write.xlsx(m05, 'm05.xlsx')
# write.xlsx(m06, 'm06.xlsx')
# write.xlsx(m07, 'm07.xlsx')
# write.xlsx(m08, 'm08.xlsx')
# write.xlsx(m09, 'm09.xlsx')
# write.xlsx(m10, 'm10.xlsx')
# write.xlsx(m11, 'm11.xlsx')
# write.xlsx(m12, 'm12.xlsx')
# write.xlsx(m13, 'm13.xlsx')
# write.xlsx(m14, 'm14.xlsx')
# write.xlsx(m15, 'm15.xlsx')
# write.xlsx(m16, 'm16.xlsx')
# write.xlsx(m17, 'm17.xlsx')
# write.xlsx(m18, 'm18.xlsx')
# write.xlsx(m19, 'm19.xlsx')

####Сравнение распределений####
F19 <- deaR::efficiencies(model_basic(dea19))
F18 <- deaR::efficiencies(model_basic(dea18))
F17 <- deaR::efficiencies(model_basic(dea17))
F16 <- deaR::efficiencies(model_basic(dea16))
F15 <- deaR::efficiencies(model_basic(dea15))
F14 <- deaR::efficiencies(model_basic(dea14))
F13 <- deaR::efficiencies(model_basic(dea13))
F12 <- deaR::efficiencies(model_basic(dea12))
F11 <- deaR::efficiencies(model_basic(dea11))
F10 <- deaR::efficiencies(model_basic(dea10))
F09 <- deaR::efficiencies(model_basic(dea09))
F08 <- deaR::efficiencies(model_basic(dea08))
F07 <- deaR::efficiencies(model_basic(dea07))
F06 <- deaR::efficiencies(model_basic(dea06))
F05 <- deaR::efficiencies(model_basic(dea05))
F04 <- deaR::efficiencies(model_basic(dea04))
F03 <- deaR::efficiencies(model_basic(dea03))
F02 <- deaR::efficiencies(model_basic(dea02))
F01 <- deaR::efficiencies(model_basic(dea01))

plot(F01,F02, xlim=range(F01,F02), ylim=range(F01,F02))
abline(0,1) 
plot(F02,F03, xlim=range(F02,F03), ylim=range(F02,F03))
abline(0,1) 
plot(F03,F04, xlim=range(F03,F04), ylim=range(F03,F04))
abline(0,1) 
plot(F04,F05, xlim=range(F04,F05), ylim=range(F04,F05))
abline(0,1) 
plot(F05,F06, xlim=range(F05,F06), ylim=range(F05,F06))
abline(0,1) 
plot(F06,F07, xlim=range(F06,F07), ylim=range(F06,F07))
abline(0,1) 
plot(F07,F08, xlim=range(F07,F08), ylim=range(F07,F08), )
abline(0,1) 
plot(F08,F09, xlim=range(F08,F09), ylim=range(F08,F09))
abline(0,1) 
plot(F09,F10, xlim=range(F09,F10), ylim=range(F09,F10))
abline(0,1) 
plot(F10,F11, xlim=range(F10,F11), ylim=range(F10,F11))
abline(0,1) 
plot(F11,F12, xlim=range(F11,F12), ylim=range(F11,F12))
abline(0,1) 
plot(F12,F13, xlim=range(F12,F13), ylim=range(F12,F13))
abline(0,1) 
plot(F13,F14, xlim=range(F13,F14), ylim=range(F13,F14))
abline(0,1) 
plot(F14,F15, xlim=range(F14,F15), ylim=range(F14,F15))
abline(0,1) 
plot(F15,F16, xlim=range(F15,F16), ylim=range(F15,F16))
abline(0,1) 
plot(F16,F17, xlim=range(F16,F17), ylim=range(F16,F17))
abline(0,1) 
plot(F17,F18, xlim=range(F17,F18), ylim=range(F17,F18))
abline(0,1) 
plot(F18,F19, xlim=range(F18,F19), ylim=range(F18,F19))
abline(0,1) 

#тест
TT <- sum(F02-1)/length(F02)/(sum(F03-1)/length(F03))
#p-value
1-pf(TT, 2*length(F02), 2*length(F03))

F_data <- read_xlsx('eff.xlsx', sheet = 1)
F_data$line <- c(rep(0, 40), rep(1, 40))
ggplot(data = F_data, aes(x = F07, y = F08)) + geom_point() +
  geom_line(data = F_data, aes(x = line, y = line)) + theme_bw() +
  xlab('Эффективность 2007') + ylab('Эффективность 2008')
ggplot(data = F_data, aes(x = F10, y = F11)) + geom_point() +
  geom_line(data = F_data, aes(x = line, y = line)) + theme_bw() +
  xlab('Эффективность 2010') + ylab('Эффективность 2011')
ggplot(data = F_data, aes(x = F13, y = F14)) + geom_point() +
  geom_line(data = F_data, aes(x = line, y = line)) + theme_bw() +
  xlab('Эффективность 2013') + ylab('Эффективность 2014')
ggplot(data = F_data, aes(x = F16, y = F17)) + geom_point() +
  geom_line(data = F_data, aes(x = line, y = line)) + theme_bw() +
  xlab('Эффективность 2016') + ylab('Эффективность 2017')

####Tobit####
data_tobit <- read_xlsx("tobit.xlsx", sheet = 2)
data_tobit19 <-  filter(data_tobit, Year == 2019)
miiris19 <- filter(miiris, Year == 2019)
data_tobit <- as.data.frame(apply(data_tobit19[,-c(1,2)], MARGIN = 2, min_max_norm)) 
data_tobit$regul <- miiris19$regul
data_tobit$infra <- miiris19$infra
data_tobit$Region <- data_tobit19$Region
data_tobit <- data_tobit %>% dplyr::select(Region, everything() )
data_tobit$assets <- replace(data_tobit$assets, data_tobit$assets==0, 0.000000001)
data_tobit$population <- replace(data_tobit$population, data_tobit$population==0, 0.000000001)
data_tobit$employment <- replace(data_tobit$employment, data_tobit$employment==0, 0.000000001)
data_tobit$productivity <- replace(data_tobit$productivity, data_tobit$productivity==0, 0.000000001)
data_tobit$GRP <- replace(data_tobit$GRP, data_tobit$GRP==0, 0.000000001)
mod <- lm(data = data_tobit[-c(25,49,70,77,79),], eff ~ log(assets) + log(population) + log(employment) + log(productivity) + log(GRP) + regul + infra)
summary(mod)
crPlots(mod)

C <- cooks.distance(mod) #влиятельные точки (Cook Distance)
plot(C, type = 'h')
which(C > 0.1)
H <- hatvalues(mod) #точки высокой напряженности (Hat Values)
plot(H, type = 'h')
which(H > 0.2)
influencePlot(mod) #выбросы

tobit19 <- tobit(data = data_tobit[-c(25,49,70,77,79),], eff ~ log(assets) + log(population) + log(employment) + log(productivity) + log(GRP) + regul + infra, left = 0, right = 1)
summary(tobit19)
stargazer(tobit19, summary = TRUE, title = 'Tobit-регрессия', type = "text", out = 'tobit.html', digits = 3)

####Стандартные отклонения####
EFF <- read.xlsx('eff.xlsx')
EFF <- t(EFF)
EFF_std <- std.error(EFF)
EFF_std <- as.data.frame(EFF_std)
EFF_std$Region <- data_dea19$Region
EFF_std <- subset(EFF_std, select=c(2,1))
write.xlsx(EFF_std, file = 'EFF_std.xlsx')

####DEA с лагами 5 лет####
data_lad <- read_xlsx('data_new.xlsx')
datalag14 <-  filter(data_lad, Year == 2014) 
datalag13 <-  filter(data_lad, Year == 2013)
datalag12 <-  filter(data_lad, Year == 2012)
datalag11 <-  filter(data_lad, Year == 2011)
datalag10 <-  filter(data_lad, Year == 2010)
datalag09 <-  filter(data_lad, Year == 2009)
datalag08 <-  filter(data_lad, Year == 2008)
datalag07 <-  filter(data_lad, Year == 2007)
datalag06 <-  filter(data_lad, Year == 2006)
datalag05 <-  filter(data_lad, Year == 2005)
datalag04 <-  filter(data_lad, Year == 2004)
datalag03 <-  filter(data_lad, Year == 2003)
datalag02 <-  filter(data_lad, Year == 2002)
datalag01 <-  filter(data_lad, Year == 2001)
datalag14 <-  dplyr::select(datalag14, -Year)
datalag13 <-  dplyr::select(datalag13, -Year)
datalag12 <-  dplyr::select(datalag12, -Year)
datalag11 <-  dplyr::select(datalag11, -Year)
datalag10 <-  dplyr::select(datalag10, -Year)
datalag09 <-  dplyr::select(datalag09, -Year)
datalag08 <-  dplyr::select(datalag08, -Year)
datalag07 <-  dplyr::select(datalag07, -Year)
datalag06 <-  dplyr::select(datalag06, -Year)
datalag05 <-  dplyr::select(datalag05, -Year)
datalag04 <-  dplyr::select(datalag04, -Year)
datalag03 <-  dplyr::select(datalag03, -Year)
datalag02 <-  dplyr::select(datalag02, -Year)
datalag01 <-  dplyr::select(datalag01, -Year)

####DEA####
dealag <- read_data(datadea = datalag05,
                    inputs = c(2,3,5,6,8,9,10,11),
                    outputs = c(22,23))
mod <- model_basic(dealag)
f <- deaR::efficiencies(mod)
#тест
TT <- sum(F05-1)/length(F05)/(sum(f-1)/length(f))
1-pf(TT, 2*length(F05), 2*length(f))


####Слои эффективности####
deaeff <- read_data(datadea = data_dea12,      #
                    inputs = c(2,3,5,6,8,9,10,11),
                    outputs = c(12,13))
mod <- model_basic(deaeff)
plot(mod)
eff <- deaR::efficiencies(mod)
data_dea12$eff <- eff                          #
data_dea12$Region[eff == 1]                    #
data_dea12 <- data_dea12 %>% filter(eff != 1)  #
data_dea12 <- dplyr::select(data_dea12, -eff)  #

####РСА для выбора весов####
###Компоненты###
#возраст: 41-42
#население: 3
#отсутствие высшего образования: 5-8
#высшее образование: 4, 9-14
#занятость: 15
#занятость RD: 16-20
#компании: 21-24
#компьютеры: 25-26
#затраты RD: 27-32
#патенты: 33-36
#продукты: 37
#ВРП: 38-39
#преступность: 40
M <- function (x) {
  (x + abs(min(x)) + 1)
}


data1 <- data_norm[, c(4,5,6,7,8,9,10,11,12,13,14)]
data1 <- na.omit(data1)
pca1 <- PCA(data1, graph = FALSE)
corrplot(pca1$var$coord, is.corr = FALSE)
PCA1 <- as.data.frame(pca1$ind$coord)
PCA1 <- as.data.frame(apply(PCA1, MARGIN = 2, M)) #по столбцам

data2 <- data_norm[, c(16,17,18,19,20)]
data2 <- na.omit(data2)
pca2 <- PCA(data2, graph = FALSE)
corrplot(pca2$var$coord, is.corr = FALSE)
PCA2 <- as.data.frame(pca2$ind$coord)
PCA2 <- as.data.frame(apply(PCA2, MARGIN = 2, M)) #по столбцам

data3 <- data_norm[, c(21,22,23,24)]
data3 <- na.omit(data3)
pca3 <- PCA(data3, graph = FALSE)
corrplot(pca3$var$coord, is.corr = FALSE)
PCA3 <- as.data.frame(pca3$ind$coord)
PCA3 <- as.data.frame(apply(PCA3, MARGIN = 2, M)) #по столбцам

data4 <- data_norm[, c(25,26)]
data4 <- na.omit(data4)
pca4 <- PCA(data4, graph = FALSE)
corrplot(pca4$var$coord, is.corr = FALSE)
PCA4 <- as.data.frame(pca4$ind$coord)
PCA4 <- as.data.frame(apply(PCA4, MARGIN = 2, M)) #по столбцам

data5 <- data_norm[, c(27,28,29,30,31,32)]
data5 <- na.omit(data5)
pca5 <- PCA(data5, graph = FALSE)
corrplot(pca5$var$coord, is.corr = FALSE)
PCA5 <- as.data.frame(pca5$ind$coord)
PCA5 <- as.data.frame(apply(PCA5, MARGIN = 2, M)) #по столбцам

data6 <- data_norm[, c(33,34,35,36)]
data6 <- na.omit(data6)
pca6 <- PCA(data6, graph = FALSE)
corrplot(pca6$var$coord, is.corr = FALSE)
PCA6 <- as.data.frame(pca6$ind$coord)
PCA6 <- as.data.frame(apply(PCA6, MARGIN = 2, M)) #по столбцам

data7 <- data_norm[, c(38,39)]
data7 <- na.omit(data7)
pca7 <- PCA(data7, graph = FALSE)
corrplot(pca7$var$coord, is.corr = FALSE)
PCA7 <- as.data.frame(pca7$ind$coord)
PCA7 <- as.data.frame(apply(PCA7, MARGIN = 2, M)) #по столбцам

# write.xlsx(PCA1, 'PCA1.xlsx')
# write.xlsx(PCA2, 'PCA2.xlsx')
# write.xlsx(PCA3, 'PCA3.xlsx')
# write.xlsx(PCA4, 'PCA4.xlsx')
# write.xlsx(PCA5, 'PCA5.xlsx')
# write.xlsx(PCA6, 'PCA6.xlsx')
# write.xlsx(PCA7, 'PCA7.xlsx')

PCA <- read_excel("PCA.xlsx")
data_norm$dim1 <- PCA$Dim.2
data_norm$dim2 <- PCA$Dim.1
data_norm$dim3 <- PCA$Dim.3
data_norm$dim4 <- PCA$Dim.4
data_norm$dim5 <- PCA$Dim.5
data_norm$dim6 <- PCA$Dim.6
data_norm$dim7 <- PCA$Dim.7
data_norm$dim8 <- PCA$Dim.8

#####
Data <- data_norm[, c(1,2,41,42,3,43,44,15,45,46,47,48,49,37,50,40)]
data_sum <- dplyr::select(Data, -Year)
stargazer(data_sum, summary = TRUE, title = 'Описательные статистики', type = "text", out = 'statistics3.html', digits = 1)

####k-cluster####
data_k <- Data %>% 
  filter(Region != 'г.Москва') %>% 
  filter(Region != 'г.Санкт0Петербург') %>% 
  filter(Region != 'Московскаяобласть')
data_kk <- data_k[,-c(1,2,5,8,15,16)]

fviz_nbclust(data_kk, kmeans, method = "wss")
k <- kmeans(data_kk, centers = 7)
fviz_cluster(k, data = data_kk, palette = "Paired")
k_data <- dplyr::select(data_k, Region, Year)
k_data$cluster <- k$cluster
#write.xlsx(k_data, 'k.xlsx')

fviz_nbclust(data_kk, pam, method = "wss")
kk <- pam(data_kk, k = 7, metric = "euclidean", stand = FALSE)
fviz_cluster(kk, data = data_kk, palette = "Paired")
kk_data <- dplyr::select(data_k, Region, Year)
kk_data$cluster <- kk$cluster
#write.xlsx(kk_data, 'kk.xlsx')

####ggplot####
library(gridExtra)
library(utils)
library(ggridges)
library(viridis)

FF_data <- read_xlsx('eff_new.xlsx', sheet = 1)
FF_data <- filter(FF_data, Year != 2001)
FF_data$Year <- as.factor(FF_data$Year)
ggplot(data = FF_data, aes(x = Eff, y = Year, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  ylab("") + xlab("") +
  theme(legend.position="none", panel.spacing = unit(0.1, "lines"),
        strip.text.x = element_text(size = 10)) + scale_fill_viridis(name = "", option = "G") + theme_bw()



rts <- read_excel("rts.xlsx")
names(rts)
rts1 <- rts[,c(1:19)]
names(rts1)
colnames(rts1) <- c("Region", 2002:2019)
rts2 <- rts1 %>% dplyr::filter(Region %in% c("Еврейскаяавтономнаяобласть"))
rts3 <- as.data.frame(rts2 %>% pivot_longer(c = "2002":"2019", names_to = "Year", values_to = "Efficiency"))
rts3$Region <- as.factor(rts3$Region)
rts3$Year <- as.numeric(rts3$Year)
ggplot(rts3, aes(x = Year, y = Efficiency, fill = Region)) + ylab('') + xlab('') +
  geom_col() + theme_classic() + facet_wrap(~Region) + scale_fill_manual(values = c("#0868AC"))
