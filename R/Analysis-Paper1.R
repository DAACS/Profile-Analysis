library(readxl)
library(ggplot2)
library(GGally)
library(magrittr)
library(reshape2)
library(psych)
library(fpc)
library(cluster)
library(flexclust)

source('R/cluster_functions.R')
source('R/profile_plot.R')

set.seed(2112)

load('data/DAACS-WGU-SRL.rda')

cluster_cols <- c('srl_motivation', 'srl_metacognition', 'srl_strategies', 'mathTotal', 'readTotal', 'writeTotal')
srl_labels <- c(#'FeedbackViews' = 'Feedback Views',
	'srl_metacognition' = 'Metacognition',
	'srl_motivation' = 'Motivation',
	'srl_strategies' = 'Strategies',
	'readTotal' = 'Reading',
	'mathTotal' = 'Mathematics',
	'writeTotal' = 'Writing')



##### Data Prep
wgu.srl1_all <- daacs.wgu[complete.cases(daacs.wgu[,cluster_cols]),]
wgu.srl1 <- wgu.srl1_all[,cluster_cols]

wgu.srl1 <- wgu.srl1 |> dplyr::mutate_if(is.numeric, scale_this)

hist(wgu.srl1_all$FeedbackViews)
wgu.srl1_all$LogFeedbackViews <- log(wgu.srl1_all$FeedbackViews + 1)
hist(wgu.srl1_all$LogFeedbackViews)

##### Validation
# https://cran.r-project.org/web/packages/clValid/vignettes/clValid.pdf
library(clValid)
intern <- clValid::clValid(wgu.srl1,
						   nClust = 2:9,
						   maxitems = nrow(wgu.srl1),
						   clMethods = c('hierarchical', 'kmeans', 'pam'),
						   validation = c('internal', 'stability'),
						   verbose = interactive())
summary(intern)
clValid::measures(intern)
measures <- array2DF(tmp) |> reshape2::dcast(Var3 + Var1 ~ Var2, value.var = 'Value')
names(measures)[1:2] <- c('Method', 'Measure')
measures$optimal <- NA
measure_funs <- list('APN' = min,
					 'AD' = min,
					 'ADM' = min,
					 'FOM' = min,
					 'Connectivity' = min,
					 'Dunn' = max,
					 'Silhouette' = max)
for(i in 1:nrow(measures)) {
	measures$optimal[i] <- names(measures)[2 + which(measures[i,3:10] == measure_funs[[measures$Measure[i]]](measures[i,3:10]))]
}
write.csv(measures, 'tables/measures.csv', row.names = FALSE)
plot(intern)
optimalScores(intern)

##### Variable centric analysis

lm(LogFeedbackViews ~ srl_motivation + srl_metacognition + srl_strategies + mathTotal + readTotal + writeTotal,
   data = wgu.srl1_all) |> summary()

glm(OnTime_Term1 ~ srl_motivation + srl_metacognition + srl_strategies + mathTotal + readTotal + writeTotal,
   data = wgu.srl1_all,
   family = binomial(link = 'logit')) |> summary()

##### How many clusters?

wgu_gap <- cluster::clusGap(wgu.srl1, kmeans, K.max = 9, B = 60, verbose = interactive()) # NOTE: This takes a while to run

cluster_metrics <- data.frame(
	k = 1:9,
	wss = wss(wgu.srl1),
	silhoutte = silhouette_score(wgu.srl1),
	gap = wgu_gap$Tab[1:9,'gap'],
	calinski_harabasz = calinski_harabasz(wgu.srl1),
	davies_bouldin = davies_bouldin(wgu.srl1),
	stringsAsFactors = FALSE
)

p_davies_bouldin <- ggplot(cluster_metrics, aes(x = k, y = davies_bouldin)) +
	geom_path() +
	geom_point() +
	scale_x_continuous(breaks = 1:9) +
	xlab('Number of Clusters') +
	ylab("Davies-Bouldin's Index") +
	theme_minimal()
p_davies_bouldin

p_calinski <- ggplot(cluster_metrics, aes(x = k, y = calinski_harabasz)) +
	geom_path() +
	geom_point() +
	scale_x_continuous(breaks = 1:9) +
	xlab('Number of Clusters') +
	ylab('Calinski-Harabasz statistic') +
	theme_minimal()
p_calinski

p_wss <- ggplot(cluster_metrics, aes(x = k, y = wss)) +
	geom_hline(aes(yintercept = wss), linetype = 2, color = 'grey50') +
	geom_path() +
	geom_point() +
	scale_x_continuous(breaks = 1:9) +
	xlab('Number of Clusters') +
	ylab('Within Group Sum of Squares') +
	theme_minimal()
p_wss

p_ss <- ggplot(cluster_metrics, aes(x = k, y = silhoutte)) +
	geom_path() +
	geom_point() +
	scale_x_continuous(breaks = 1:9) +
	xlab('Number of Clusters') +
	ylab('Silhouette Score') +
	theme_minimal()
p_ss

p_gap <- ggplot(cluster_metrics, aes(x = k, y = gap)) +
	geom_path() +
	geom_point() +
	scale_x_continuous(breaks = 1:9) +
	xlab('Number of Clusters') +
	ylab('Gap Statistic') +
	theme_minimal()

cowplot::plot_grid(p_wss + xlab('') + ylab('') + ggtitle('Within Group Sum of Squares', subtitle = 'Elbow method'),
				   p_ss + xlab('') + ylab('') + ggtitle('Silhouette Score', subtitle = 'Higher values desired'),
				   p_gap + xlab('') + ylab('') + ggtitle('Gap Statistic', subtitle = 'Higher values desired'),
				   p_davies_bouldin + xlab('') + ylab('') + ggtitle("Davies-Bouldin's Index", subtitle = 'Lower values desired'),
				   p_calinski + ylab('') + ggtitle('Calinski-Harabasz Statistic', subtitle = 'Higher values desired'),
				   ncol = 1)
ggsave('figures/WGU_N_Cluster_Plots.png', width = 13, height = 3, bg = 'white')

# https://r-tastic.co.uk/post/optimal-number-of-clusters/
srl_dist <- dist(as.matrix(wgu.srl1))   # find distance matrix
hc <- hclust(srl_dist)
k_try <- 6
memb <- cutree(hc, k = k_try)
cent <- NULL
for(k in 1:k_try) {
	cent <- rbind(cent, colMeans(wgu.srl1[memb == k, , drop = FALSE]))
}
hc1 <- hclust(dist(cent)^2, members = table(memb))
opar <- par(mfrow = c(1, 2))
plot(hc,  labels = FALSE, hang = -1, main = "Original Tree")
plot(hc1, labels = FALSE, hang = -1, main = paste0("Re-start from ", k_try, " clusters"))
par(opar)

plot(hc, labels = FALSE, hang = -1)

# library(vegan)
# set.seed(2112)
# cal_fit2 <- cascadeKM(srl_dist, 2, 9, iter = 10)
# plot(cal_fit2, sortg = TRUE, grpmts.plot = TRUE)
# calinski.best2 <- as.numeric(which.max(cal_fit2$results[2,]) + 1)
# cat("Calinski criterion optimal number of clusters:", calinski.best2, "\n")

# library(mclust)
# set.seed(2112)
# d_clust2 <- Mclust(as.matrix(srl_dist), G = 2:9)
# m.best2 <- dim(d_clust2$z)[2]
# cat("model-based optimal number of clusters:", m.best2, "\n")


##### cluster analysis #########################################################
nClusters <- 5

# train <- sample(nrow(wgu.srl1), .5 * nrow(wgu.srl1))
# pred_train <- kmeans(wgu.srl1[train,], nClusters)
# kcca_train <- flexclust::as.kcca(pred_train, data = wgu.srl1[train,])
# pred_test <- predict(kcca_train, newdata = wgu.srl1[-train,])
# image(kcca_train)
# points(wgu.srl1[train,1:2], col = pred_train, pch = 19, cex = 0.3)
# points(wgu.srl1[-train, 1:2], col = pred_test, pch = 22) #, bg = "orange")

set.seed(2112)
# kmeans
wgu.fit <- kmeans(wgu.srl1, nClusters)
wgu.srl1_all$cluster <- factor(wgu.fit$cluster, labels = letters[1:nClusters])

if(nClusters == 3) {
	cluster_labels <- c('A: High SRL / High Academic',
						'B: Low SRL / High Academic',
						'C: Low SRL / Low Academic')
} else if(nClusters == 4) {
	cluster_labels <- c('A: Low SRL / Average Academic',
						'B: Mixed SRL / Low Academic',
						'C: High SRL / Average Academic',
						'D: Average SRL / High Acadamic')
} else if(nClusters == 5) {
	cluster_labels = c("A: Low SRL / Low Academic",
					   "B: Average SRL / Low Academic",
					   "C: Low SRL / Average Aacademic",
					   "D: Average SRL / High Academic",
					   "E: High SRL / Average Academic")
} else if(nClusters == 6) {
	cluster_labels = c("A: Low SRL / High Academic",
					   "B: Average SRL / Low Academic",
					   "C: Low SRL / Low Aacademic",
					   "D: Average SRL / Mixed Academic",
					   "E: Low SRL / Average Academic",
					   "F: Average SRL / High Academic")

} else  {
	cluster_labels <- LETTERS[1:nClusters]
}

profile_plot(
	df = wgu.srl1,
	clusters = wgu.srl1_all$cluster,
	df_dep = wgu.srl1_all[,c('LogFeedbackViews', 'OnTime_Term1')],
	ylab = 'Mean Standard Scale Score',
	cluster_labels = cluster_labels
)



ggplot(wgu.srl1_all, aes(x = mathTotal, srl_motivation, color = cluster)) +
	# geom_point(alpha = 0.5) +
	geom_density2d()


library(factoextra)
k2 <- kmeans(wgu.srl1, centers = 2, nstart = 25)
k3 <- kmeans(wgu.srl1, centers = 3, nstart = 25)
k4 <- kmeans(wgu.srl1, centers = 4, nstart = 25)
k5 <- kmeans(wgu.srl1, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = wgu.srl1) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = wgu.srl1) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = wgu.srl1) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = wgu.srl1) + ggtitle("k = 5")

gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)


wgu.srl1 %>%
	dplyr::mutate(Cluster = wgu.fit$cluster) %>%
	dplyr::group_by(Cluster) %>%
	dplyr::summarise_all("mean")



# hierarchical
# wgu.fit <- hclust(dist(wgu.srl1))
# wgu.srl1_all$cluster <- cutree(wgu.fit, k = nClusters)

# clusplot(wgu.srl1, wgu.fit$cluster, stand = TRUE, color=TRUE, shade=FALSE, labels=0, lines=0)
# plotcluster(wgu.srl1, wgu.fit$cluster)

table(wgu.srl1_all$cluster)

##### Cluster Plot
wgu.srl1.melted <- melt(cbind(wgu.srl1, cluster = as.character(wgu.srl1_all$cluster)), id.vars = 'cluster')
wgu.tab <- describeBy(wgu.srl1.melted$value, group = list(wgu.srl1.melted$cluster, wgu.srl1.melted$variable), mat = TRUE)
wgu.tab <- wgu.tab[,c('group1', 'group2', 'n', 'mean', 'se', 'sd', 'median')]
names(wgu.tab)[1:2] <- c('Cluster', 'Factor')

wgu.tab$Factor <- factor(wgu.tab$Factor, levels = names(srl_labels), ordered = TRUE)



wgu.tab$Cluster <- factor(wgu.tab$Cluster,
						  levels = letters[1:nClusters],
						  labels = cluster_labels)

text_size <- 2
hjust <- -0.1 #-0.5
point_size <- 2
se_factor <- 1.96
color_palette <- 2

p_cluster_wgu <- ggplot(wgu.tab[order(wgu.tab$Factor),],
						aes(x = Factor, y = mean, color = Cluster, group = Cluster)) +
	geom_hline(yintercept = 0, color = 'grey70') +
	geom_path(alpha = 0.5) +
	geom_point(size = point_size) +
	geom_errorbar(aes(ymin = mean - se_factor * se, ymax = mean + se_factor * se), width = 0.25, alpha = 0.5) +
	geom_label(aes(label = round(mean, digits = 2)), fill = 'white', hjust = hjust, size = text_size, show.legend = FALSE) +
	# geom_label(data = wgu.tab[wgu.tab$Factor == 'srl_metacognition',],
	# 		   aes(x = Factor, y = mean, label = substr(Cluster, 1, 1)), hjust =2) +
	xlab('') + ylab('Mean Standard Scale Score') +
	scale_x_discrete(labels = srl_labels) +
	scale_color_brewer(type = 'qual', palette = color_palette) +
	theme_minimal() +
	theme(legend.position = 'bottom', legend.key.size = unit(0, 'lines')) +
	ggtitle('Cluster Profiles', subtitle = paste0('k = ', nClusters))
p_cluster_wgu


lm(LogFeedbackViews ~ cluster, data = wgu.srl1_all) |> summary()
wgu.logfeedbackviews <- aov(LogFeedbackViews ~ cluster, data = wgu.srl1_all) |> summary()
wgu.logfeedbackviews

f_str <- paste0("$F_{", paste0(wgu.logfeedbackviews[[1]][1:2,1], collapse = ", "), "}$ = ",
	   round(wgu.logfeedbackviews[[1]][1,4], digits = 2), ", ",
	   print_p_value(wgu.logfeedbackviews[[1]][1,5]))


glm(OnTime_Term1 ~ cluster, data = wgu.srl1_all, family = binomial(link = 'logit')) |> summary()
wgu.success.chisq <- chisq.test(wgu.srl1_all$cluster, wgu.srl1_all$OnTime_Term1)
wgu.success.chisq

wgu.feedback.tab <- describeBy(wgu.srl1_all$LogFeedbackViews, group = list(wgu.srl1_all$cluster), mat = TRUE)

wgu.feedback.tab$group1 <- factor(wgu.feedback.tab$group1,
								  levels = letters[1:nClusters],
								  labels = cluster_labels)

p_wgu_feedback <- ggplot(wgu.feedback.tab, aes(x = group1, y = mean, color = group1)) +
	geom_errorbar(aes(ymin = mean - se_factor * se, ymax = mean + se_factor * se, color = group1), width = 0.5) +
	geom_point(size = point_size) +
	geom_label(aes(label = paste0(round(mean, digits = 2))), fill = 'white', hjust = hjust, size = text_size) +
	xlab('Cluster') + ylab('Feedback Views (log scale)') +
	ggtitle("Feedback Views",
			subtitle = latex2exp::TeX(f_str)) +
	theme_minimal() +
	scale_color_brewer(type = 'qual', palette = color_palette) +
	theme(legend.position = 'none')
p_wgu_feedback

wgu.success.tab <- describeBy(wgu.srl1_all$OnTime_Term1, group = list(wgu.srl1_all$cluster), mat = TRUE)

wgu.success.tab$group1 <- factor(wgu.success.tab$group1,
								 levels = letters[1:nClusters],
								 labels = cluster_labels)


p_wgu_success <- ggplot(wgu.success.tab, aes(x = group1, y = mean, color = group1)) +
	geom_errorbar(aes(ymin = mean - se_factor * se, ymax = mean + se_factor * se, color = group1), width = 0.5) +
	geom_point(size = point_size) +
	geom_label(aes(label = paste0(round(100 * mean, digits = 1), '%')), fill = 'white', hjust = hjust, size = text_size) +
	xlab('Cluster') + ylab('On-Time Progress') +
	ggtitle(label = 'On-Time Progress',
			subtitle = latex2exp::TeX(paste0("$\\chi^2$ = ", round(wgu.success.chisq$statistic, digits = 2), ", ",
											 print_p_value(wgu.success.chisq$p.value)))) +
	theme_minimal() +
	scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
	# scale_x_discrete(expand = c(0,2)) +
	scale_color_brewer(type = 'qual', palette = color_palette) +
	theme(legend.position = 'none')
p_wgu_success


grobs <- ggplotGrob(p_cluster_wgu)$grobs
legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]

cowplot::plot_grid(
	cowplot::plot_grid(p_cluster_wgu + theme(legend.position = 'none'),
					   p_wgu_feedback + scale_x_discrete(labels = LETTERS[1:nClusters]),
					   p_wgu_success + scale_x_discrete(labels = LETTERS[1:nClusters]),
					   nrow = 1,
					   rel_widths = c(3,1, 1)),
	legend,
	ncol = 1,
	rel_heights = c(10, 1))

ggsave(paste0('figures/WGU_Cluster_Plot_', nClusters, '.png'), bg = 'white', width = 13, height = 5)





ggpairs(wgu.srl1,
		columns = names(wgu.srl1),
		axisLabels = 'show',
		mapping = ggplot2::aes_string(color='cluster'),
		# upper='blank',
		upper = list(continuous="cor"),
		diag = list(continuous="densityDiag", discrete='barDiag',
					aes_string(group='cluster', fill='cluster')),
		lower = list(continuous="density", combo="box", alpha = 0.2))



##### Test
wgu.srl1_all$cluster

lm(LogFeedbackViews ~ cluster,
   data = wgu.srl1_all) |> summary()

glm(OnTime_Term1 ~ cluster,
   data = wgu.srl1_all, family = binomial(link = 'logit')) |> summary()

feedback_lm_outs <- list()
ontime_lm_outs <- list()
for(i in unique(wgu.srl1_all$cluster)) {
	feedback_lm_outs[[i]] <- lm(LogFeedbackViews ~ Motivation + Metacognition + Strategies + Mathematics + Reading + Writing,
					   data = wgu.srl1_all[wgu.srl1_all$cluster == i,])
	ontime_lm_outs[[i]] <- glm(OnTime_Term1 ~ Motivation + Metacognition + Strategies + Mathematics + Reading + Writing,
							   data = wgu.srl1_all[wgu.srl1_all$cluster == i,],
							   family = binomial(link = 'logit'))
}

huxreg('Cluster A' = feedback_lm_outs[['a']],
	   'Cluster B' = feedback_lm_outs[['b']],
	   'Cluster C' = feedback_lm_outs[['c']],
	   'Cluster D' = feedback_lm_outs[['d']],
	   'Cluster E' = feedback_lm_outs[['e']],
	   error_pos = "right",
	   bold_signif = 0.05)

huxreg('Cluster A' = ontime_lm_outs[['a']],
	   'Cluster B' = ontime_lm_outs[['b']],
	   'Cluster C' = ontime_lm_outs[['c']],
	   'Cluster D' = ontime_lm_outs[['d']],
	   'Cluster E' = ontime_lm_outs[['e']],
	   error_pos = "right",
	   bold_signif = 0.05)



##### Alpha's
names(daacs.wgu)
psych::alpha(daacs.wgu[,c('math_word_problems', 'math_lines_and_functions', "math_variables_and_equations",
						  'math_number_and_calculation', 'math_statistics')])

psych::alpha(daacs.wgu[,c("read_structure", "read_inference", "read_language", "read_purpose", "read_ideas")])

writing <- daacs.wgu[,c("write_scoringType", "write_summary", "write_suggestions",
						"write_structure", "write_transitions", "write_ideas",
						"write_cohesion", "write_correct", "write_complexity", "conventions")]
writing <- writing[complete.cases(writing),]
psych::alpha(writing[,-1])

# names(math.items.wgu)
# math_wide <- reshape2::dcast(math.items.wgu[,c('DAACS_ID', 'question', 'score')],
# 							 formula = DAACS_ID ~ question,
# 							 fun.aggregate = mean,
# 							 value.var = 'score')
# names(math_wide)[2:ncol(math_wide)] <- paste0('item', 1:(ncol(math_wide)-1))
# psych::alpha(math_wide[,-1])
