# # loading packages
library(tidyverse)
library(corrplot)
library(rpart)
library(rpart.plot)
# library(scatterplot3d)
# library(rgl)
library(caret)
library(randomForest)
library(MASS)
library(gridExtra)
library(ggplot2)

#  read in datasets
set14 <- readRDS("set14.rds")

# consider some correlations
jpeg('corTypePlot2014.jpeg')
corrplot(cor(set14[,c("newspapers","magazines","radio", "tv", "internet")]),
         method = "pie",
         order = "hclust",
         hclust.method = "complete",
         tl.col = 'black',
         mar = c(1,1,1,1),
         addCoefasPercent = TRUE,
         tl.pos = TRUE)
dev.off()

## consider kmeans
wss <- vector()
set.seed(123)
for(k in c(1,2,3,4,5,6)) {
        temp <- kmeans(set14[,c("newspapers","magazines","radio", "tv", "internet", "all")],
                       centers = k,
                       nstart = 5,
                       iter.max = 30)
        wss <- append(wss,temp$tot.withinss)
}

jpeg('kmeansTypePlot2014.jpeg')
plot(c(1,2,3,4,5,6), wss, type = "b", xlab = "k-values", ylab = "total within sum of squares" )
dev.off()

set.seed(123)
kmeans14 <- kmeans(set14[,c("newspapers","magazines","radio", "tv", "internet","all")],
                   centers = 4,
                   nstart = 5,
                   iter.max = 100)

table(kmeans14$cluster)

# align with interpretation of 2012....
# green to green:  2 to 2
# lilac to red: 4 to 1
# blue to blue: 3 to 3
# red to lilac: 1 to 4
kmeans14$cluster <- ifelse(kmeans14$cluster == 1, 9, kmeans14$cluster)
kmeans14$cluster <- ifelse(kmeans14$cluster == 2, 7, kmeans14$cluster)
kmeans14$cluster <- ifelse(kmeans14$cluster == 3, 8, kmeans14$cluster)
kmeans14$cluster <- ifelse(kmeans14$cluster == 4, 6, kmeans14$cluster)
kmeans14$cluster <- kmeans14$cluster - 5

# add cluster labels to the dataset
set14c <- set14 %>%
        mutate(cluster = factor(kmeans14$cluster)) %>%
        dplyr::select(qn, pwgt, cluster, everything())

# save them
saveRDS(set14c, "set14c.rds")
# read back
set14c <- readRDS("set14c.rds")

## some plots for simple version to use in longitudinal stuff later...
# boxplots of clusters and media types

boxplot <- function(set,type) {
        ggplot(set, aes_string("cluster", type, fill = "cluster")) +
                geom_boxplot() +
                guides(fill = FALSE) +
                labs(title = type)
}

jpeg('typeBoxPlots_14.jpeg', quality = 100, type = "cairo")
grid.arrange(boxplot(set14c, type = "all"),
             boxplot(set14c, type = "newspapers"),
             boxplot(set14c, type = "magazines"),
             boxplot(set14c, type = "radio"),
             boxplot(set14c, type = "tv"),
             boxplot(set14c, type = "internet"),
             ncol=3, nrow = 2)
dev.off()

# try to make sense of demographics

# size of each cluster
ggplot(data = set14c, aes(x = cluster, fill = cluster)) +
        geom_bar(stat = "count") +
        guides(fill = FALSE)

# demographics by cluster

bars_by_cluster <- function(set, category) { # category:one of race, edu, age, lsm, sex, hh_inc
        if(category == "race") {
                level = c("black", "coloured", "indian", "white")
                title = "Population Group 2014"
        }
        if(category == "edu") {
                level = c(c("<matric", "matric",">matric"))
                title = "Education Level 2014"
        }
        if(category == "age") {
                level = c(c("15-24","25-44", "45-54","55+"))
                title = "Age Group 2014"
        }
        if(category == "lsm") {
                level = c("1-2", "3-4", "5-6", "7-8", "9-10")
                title = "LSM 2014"
        }
        if(category == "sex") {
                level = c("male", "female")
                title = "Gender 2014"
        }
        if(category == "hh_inc") {
                level = c("<5000","5000-10999","11000-19999",">=20000")
                title = "Household Income 2014"
        }
        
        ggplot(data = set14c, aes_string(x = "cluster", fill = category)) +
                geom_bar(stat = "count", position = position_dodge()) +
                scale_fill_discrete(labels=level) +
                labs(title = title) +
                guides(fill=guide_legend(title=NULL)) 
}

jpeg('typeDemogPlots_14.jpeg', quality = 100, type = "cairo")
grid.arrange(bars_by_cluster(set14c, "sex"),
             bars_by_cluster(set14c, "age"),
             bars_by_cluster(set14c, "race"),
             bars_by_cluster(set14c, "edu"),
             bars_by_cluster(set14c, "hh_inc"),
             bars_by_cluster(set14c, "lsm"),
             ncol=2, nrow = 3)
dev.off()

# consider multidimensional scaling and self organising maps on the clusters :

# 1st create a subset to ensure easier running
set.seed(56)
sub14 <- set14c[sample(nrow(set14c), size = 1000),]

# distance matrix and MDS
sub14_dist <- dist(sub14[,c("newspapers","magazines","radio", "tv", "internet", "all")])
mds14 <- cmdscale(sub14_dist)
plot(mds14, col = as.numeric(sub14$cluster) + 1, pch = 19, ylab = "", xlab = "")

# 3D scaling
mds3 <- cmdscale(dist(sub14[,c("newspapers", "magazines", "radio", "tv", "internet", "all")]), k = 3)
mds3 <- as.data.frame(mds3)

# 2D Scatterplots of 4 cente

# setting colours
cols <- as.numeric(sub14$cluster) + 1
cols <- ifelse(cols == 5, 6, cols)

jpeg('kmeans2DPlot2014.jpeg')
plot(mds14, col = cols, ylab = "", xlab = "", pch = 19)
dev.off()
# 

# consider for some predictions:
# create training and test sets:

set.seed(56)
ind_train <- createDataPartition(set14c$cluster, p = 0.7, list = FALSE)
training <- set14c[ind_train,]
testing <- set14c[-ind_train,]

# # using random forest:
forest14_type <- randomForest(cluster ~ newspapers
                              + tv
                              + radio
                              + magazines
                              + internet,
                              data = training )

pred_forest14_type <- predict(forest14_type, newdata = testing)

confusionMatrix(pred_forest14_type, testing$cluster) 

# with lda. Although given accuracy of forest,  no real need.
set.seed(56)
lda14 <- lda(cluster ~ newspapers
             + tv
             + radio
             + magazines
             + internet,
             data = training)
summary(lda14)

pred_lda14 <- predict(lda14, newdata = testing)
confusionMatrix(pred_lda14$class, testing$cluster) # collinearity meant took out 

# using only demographic information
forest14_demogr <- randomForest(cluster ~ age
                                + sex
                                + edu
                                + hh_inc
                                + race
                                + lsm,
                                data = training)

pred_forest14_demogr <- predict(forest14_demogr, newdata = testing)

confusionMatrix(pred_forest14_demogr, testing$cluster)

# with lda
set.seed(56)
lda14_demogr <- lda(cluster ~ age
                    + sex
                    + edu
                    + hh_inc
                    + race
                    + lsm,
                    data = training)

pred_lda14_demogr <- predict(lda14_demogr, newdata = testing)
confusionMatrix(pred_lda14_demogr$class, testing$cluster)

##  some qualitative consideration of the four types:

# consider a single tree partitioning to try to add meaning to the four clusters
control <- rpart.control(maxdepth = 3, cp = 0.001)
tree14 <- rpart(cluster ~ newspapers + tv + radio + magazines + internet, 
                data = set14c,
                control = control) # weights = pwgt
par(mfrow = c(1,1))
plot(tree14, uniform = TRUE, margin = 0.2)
text(tree14, pretty = 0, cex = 0.8)

# for more detail
rpart.plot(tree14, type = 4, extra = 1, cex = 0.5)