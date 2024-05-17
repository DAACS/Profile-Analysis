library(readxl)
library(ggplot2)
library(GGally)
library(magrittr)
library(reshape2)
library(psych)
library(fpc)
library(cluster)
library(flexclust)

load('data/DAACS-EC.rda') # WGU data

srl_domains <- c('srl_managing_time', 'srl_help_seeking', 'srl_managing_environment', 'srl_understanding',
				 'srl_anxiety', 'srl_mastery_orientation', 'srl_mindset', 'srl_self_efficacy',
				 'srl_monitoring', 'srl_evaluation', 'srl_planning')
cluster_cols <- c('srl_motivation', 'srl_metacognition', 'srl_strategies')
				  # , 'FeedbackViews')

# ec_srl1 <- daacs.ec[daacs.ec$Treat, srl_domains]
ec_srl1_all <- daacs.ec[daacs.ec$Treat,]

ec_srl1_all <- ec_srl1_all[complete.cases(ec_srl1_all[,cluster_cols]),]
ec_srl1 <- ec_srl1_all[,cluster_cols]


scale_this <- function(x){
	(x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

hist(ec_srl1$FeedbackViews)
ec_srl1$FeedbackViews <- log(ec_srl1$FeedbackViews + 1)

ec_srl1 <- ec_srl1 |> dplyr::mutate_if(is.numeric, scale_this)



##### Determine number of clusters
wss <- (nrow(ec_srl1) - 1) * sum(apply(ec_srl1, 2, var))
for(i in 2:10) { # Calculate within sum of quares for up to 10 clusters
	wss[i] <- sum(kmeans(ec_srl1, centers = i)$withinss)
}
plot(1:10, wss, type = 'b', xlab = 'Number of Clusters', ylab = 'Within group sum of squares')

##### K-means cluster analysis
nClusters <- 4
fit <- kmeans(ec_srl1, nClusters)
# aggregate(srl1, by = list(fit$cluster), FUN = mean)
ec_srl1$cluster <- factor(fit$cluster, labels = letters[1:nClusters])
ec_srl1_all$cluster <- factor(fit$cluster, labels = letters[1:nClusters])

##### Cluster Plots

ec_srl1.melted <- melt(ec_srl1, id.vars = 'cluster')
tab <- describeBy(ec_srl1.melted$value, group = list(ec_srl1.melted$cluster, ec_srl1.melted$variable), mat = TRUE)
tab <- tab[,c('group1', 'group2', 'n', 'mean', 'se', 'sd', 'median')]
names(tab)[1:2] <- c('Cluster', 'Factor')

ggplot(tab[order(tab$Factor),],
	   aes(x = Factor, y = mean, color = Cluster, group = Cluster)) +
	geom_path(alpha = 0.5) +
	geom_point() +
	geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se), width = 0.25, alpha = 0.5) +
	geom_text(aes(label = round(mean, digits = 2)), hjust = -0.5, size = 3) +
	# ylim (c(1,4)) +
	xlab('') + ylab('z-score') +
	theme_minimal()
