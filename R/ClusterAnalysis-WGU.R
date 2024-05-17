library(readxl)
library(ggplot2)
library(GGally)
library(magrittr)
library(reshape2)
library(psych)
library(fpc)
library(cluster)
library(flexclust)

load('data/DAACS-WGU.rda') # WGU data
load('data/DAACS-EC.rda')
load("data/DAACS-UA_2023.rda")


dashboard.pages <- c(
	"/dashboard", "/",
	"/assessments/mathematics","/assessments/mathematics/start","/assessments/mathematics/take",
	"/assessments/college_skills","/assessments/college_skills/start","/assessments/college_skills/take",
	"/assessments/writing","/assessments/writing/start","/assessments/take",
	"/assessments/reading","/assessments/reading/start","/assessments/reading/take"
)


events.students.ec$FeedbackPage <- !events.students.ec$url.base %in% dashboard.pages
events.students.ec.pageviews <- as.data.frame(table(events.students.ec$DAACS_ID))
names(events.students.ec.pageviews) <- c('DAACS_ID', 'PageViews')
events.students.ec.feedbackviews <- as.data.frame(table(
	events.students.ec[events.students.ec$FeedbackPage,]$DAACS_ID))
names(events.students.ec.feedbackviews) <- c('DAACS_ID', 'FeedbackViews')
daacs.ec <- merge(daacs.ec, events.students.ec.pageviews, by = 'DAACS_ID', all.x = TRUE)
daacs.ec <- merge(daacs.ec, events.students.ec.feedbackviews, by = 'DAACS_ID', all.x = TRUE)
daacs.ec[is.na(daacs.ec$PageViews),]$PageViews <- 0
daacs.ec[is.na(daacs.ec$FeedbackViews),]$FeedbackViews <- 0

# events.students.ec$FeedbackPage <- !events.students.ec$url.base %in% dashboard.pages
# events.students.ec.pageviews <- as.data.frame(table(events.students.ec$DAACS_ID))
# names(events.students.ec.pageviews) <- c('DAACS_ID', 'PageViews')
# events.students.ec.feedbackviews <- as.data.frame(table(
# 	events.students.ec[events.students.ec$FeedbackPage,]$DAACS_ID))
# names(events.students.ec.feedbackviews) <- c('DAACS_ID', 'FeedbackViews')
# daacs.ec <- merge(daacs.ec, events.students.ec.pageviews, by = 'DAACS_ID', all.x = TRUE)
# daacs.ec <- merge(daacs.ec, events.students.ec.feedbackviews, by = 'DAACS_ID', all.x = TRUE)
# daacs.ec[is.na(daacs.ec$PageViews),]$PageViews <- 0
# daacs.ec[is.na(daacs.ec$FeedbackViews),]$FeedbackViews <- 0


##### Subset the SRL data (pick one!)
# All subdomains
# srl_cols <- c('srl_managing_time', 'srl_help_seeking', 'srl_managing_environment', 'srl_understanding',
# 			  'srl_anxiety', 'srl_mastery_orientation', 'srl_mindset', 'srl_self_efficacy',
# 			  'srl_monitoring', 'srl_evaluation', 'srl_planning')
# wgu.srl1 <- daacs.wgu[daacs.wgu$Treat, srl_cols]
# ec.srl1 <- daacs.ec[daacs.ec$Treat, srl_cols]
# ua.srl1 <- daacs.ualbany[daacs.ualbany$srl_attempt > 0, srl_cols]


# 3 first order domains
cluster_cols <- c('srl_motivation', 'srl_metacognition', 'srl_strategies', #'FeedbackViews',
				  'mathTotal', 'readTotal', 'writeTotal')

wgu.srl1_all <- daacs.wgu[daacs.wgu$Treat,]
wgu.srl1_all <- wgu.srl1_all[complete.cases(wgu.srl1_all[,cluster_cols]),]
wgu.srl1 <- wgu.srl1_all[,cluster_cols]

ec.srl1_all <- daacs.ec[daacs.ec$Treat,]
ec.srl1_all <- ec.srl1_all[complete.cases(ec.srl1_all[,cluster_cols]),]
ec.srl1 <- ec.srl1_all[,cluster_cols]

daacs.ua$Treat <- daacs.ua$Group == 'Treatment'
daacs.ua$srl_motivation <- daacs.ua$motivation
daacs.ua$srl_metacognition <- daacs.ua$metacognition
daacs.ua$srl_strategies <- daacs.ua$strategies
cbind(cluster_cols, cluster_cols %in% names(daacs.ua))
ua.srl1_all <- daacs.ua[daacs.ua$Treat,]
ua.srl1_all <- ua.srl1_all[complete.cases(ua.srl1_all[,cluster_cols]),]
ua.srl1 <- ua.srl1_all[,cluster_cols]

ua.srl1$mathTotal <- as.numeric(ua.srl1$mathTotal)
ua.srl1$readTotal <- as.numeric(ua.srl1$readTotal)
ua.srl1$writeTotal <- as.numeric(ua.srl1$writeTotal)

# ua.srl1_all <- daacs.ualbany[daacs.ualbany$srl_attempt == 1,]
# ua.srl1_all <- ua.srl1_all[complete.cases(ua.srl1_all[,cluster_cols])]

scale_this <- function(x) {
	(x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

hist(wgu.srl1$FeedbackViews)
wgu.srl1$FeedbackViews <- log(wgu.srl1$FeedbackViews + 1)

hist(ec.srl1$FeedbackViews)
ec.srl1$FeedbackViews <- log(ec.srl1$FeedbackViews + 1)

wgu.srl1 <- wgu.srl1 |> dplyr::mutate_if(is.numeric, scale_this)
ec.srl1 <- ec.srl1 |> dplyr::mutate_if(is.numeric, scale_this)
ua.srl1 <- ua.srl1 |> dplyr::mutate_if(is.numeric, scale_this)

table(complete.cases(wgu.srl1))
table(complete.cases(ec.srl1))
table(complete.cases(ua.srl1))

##### Determine number of clusters
wss <- function(df) {
	wss <- (nrow(df) - 1) * sum(apply(df, 2, var))
	for(i in 2:10) { # Calculate within sum of quares for up to 10 clusters
		wss[i] <- sum(kmeans(df, centers = i)$withinss)
	}
	return(wss)
}

wgu.wss <- wss(wgu.srl1)
ec.wss <- wss(ec.srl1)
ua.wss <- wss(ua.srl1)

wss_all <- datua.srl1wss_all <- data.frame(
	Institution = c(rep('WGU', length(wgu.wss)),
					rep('EC', length(ec.wss)),
					rep('UA', length(ua.wss))),
	nClusters = c(1:length(wgu.wss), 1:length(ec.wss), 1:length(ua.wss)),
	wss = c(wgu.wss, ec.wss, ua.wss)
)
ggplot(wss_all, aes(x = nClusters, y = wss)) +
	geom_path() +
	geom_point() +
	scale_x_continuous(breaks = 1:length(wgu.wss)) +
	xlab('Number of Clusters') +
	ylab('Within group sum of squares') +
	facet_wrap(~ Institution, nrow = 1, scales = 'free_y') +
	theme_minimal()
ggsave('figures/sum_of_squares.png', width = 6.5, height = 3)

##### K-means cluster analysis
nClusters <- 4

wgu.fit <- kmeans(wgu.srl1, nClusters)
wgu.srl1$cluster <- factor(wgu.fit$cluster, labels = letters[1:nClusters])
wgu.srl1_all$cluster <- factor(wgu.fit$cluster, labels = letters[1:nClusters])

ec.fit <- kmeans(ec.srl1, nClusters)
ec.srl1$cluster <- factor(ec.fit$cluster, labels = letters[1:nClusters])
ec.srl1_all$cluster <- factor(ec.fit$cluster, labels = letters[1:nClusters])

ua.fit <- kmeans(ua.srl1, nClusters)
ua.srl1$cluster <- factor(ua.fit$cluster, labels = letters[1:nClusters])
ua.srl1_all$cluster <- factor(ua.fit$cluster, labels = letters[1:nClusters])

# Cluster sizes
table(wgu.srl1$cluster) %>% print() %>% prop.table()

# ggplot(wgu.srl1, aes(x = cluster,
# 				 label = scales::percent(prop.table(stat(count))))) +
# 	geom_bar() +
# 	geom_text(stat = 'count',
# 			  position = position_dodge(.9),
# 			  vjust = -0.5,
# 			  size = 4)

##### Cluster Plots
wgu.srl1.melted <- melt(wgu.srl1, id.vars = 'cluster')
wgu.tab <- describeBy(wgu.srl1.melted$value, group = list(wgu.srl1.melted$cluster, wgu.srl1.melted$variable), mat = TRUE)
wgu.tab <- wgu.tab[,c('group1', 'group2', 'n', 'mean', 'se', 'sd', 'median')]
names(wgu.tab)[1:2] <- c('Cluster', 'Factor')

ec.srl1.melted <- melt(ec.srl1, id.vars = 'cluster')
ec.tab <- describeBy(ec.srl1.melted$value, group = list(ec.srl1.melted$cluster, ec.srl1.melted$variable), mat = TRUE)
ec.tab <- ec.tab[,c('group1', 'group2', 'n', 'mean', 'se', 'sd', 'median')]
names(ec.tab)[1:2] <- c('Cluster', 'Factor')

ua.srl1.melted <- melt(ua.srl1, id.vars = 'cluster')
ua.tab <- describeBy(ua.srl1.melted$value, group = list(ua.srl1.melted$cluster, ua.srl1.melted$variable), mat = TRUE)
ua.tab <- ua.tab[,c('group1', 'group2', 'n', 'mean', 'se', 'sd', 'median')]
names(ua.tab)[1:2] <- c('Cluster', 'Factor')

# fv_tab <- describeBy(wgu.srl1$FeedbackViews, group = wgu.srl1$cluster, mat = TRUE)
# ggplot(wgu.srl1, aes(x = cluster, y = FeedbackViews)) +
# 	geom_segment(data = fv_tab, aes(x = group1, y = mean - 2 + sd, yend = mean + 2 + sd), color = 'green', size = 3) +
# 	geom_segment(data = fv_tab, aes(x = group1, y = mean - sd, yend = mean + sd), color = 'green', size = 12) +
# 	geom_boxplot(width = 0.5, alpha = 0.2) +
# 	geom_point(data = fv_tab, aes(x = group1, y = mean), color = 'blue', size = 3) +
# 	theme_minimal()


srl_labels <- c(#'FeedbackViews' = 'Feedback Views',
				'srl_metacognition' = 'Metacognition',
				'srl_motivation' = 'Motivation',
				'srl_strategies' = 'Strategies',
				'readTotal' = 'Reading',
				'mathTotal' = 'Mathematics',
				'writeTotal' = 'Writing')
wgu.tab$Factor <- factor(wgu.tab$Factor, levels = names(srl_labels), ordered = TRUE)
ec.tab$Factor <- factor(ec.tab$Factor, levels = names(srl_labels), ordered = TRUE)
ua.tab$Factor <- factor(ua.tab$Factor, levels = names(srl_labels), ordered = TRUE)

p_cluster_wgu <- ggplot(wgu.tab[order(wgu.tab$Factor),],
	   aes(x = Factor, y = mean, color = Cluster, group = Cluster)) +
	geom_hline(yintercept = 0, color = 'grey70') +
	geom_path(alpha = 0.5) +
	geom_point() +
	geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se), width = 0.25, alpha = 0.5) +
	geom_text(aes(label = round(mean, digits = 2)), hjust = -0.5, size = 3, show.legend = FALSE) +
	xlab('') + ylab('Mean Scale Score') +
	scale_x_discrete(labels = srl_labels) +
	scale_color_brewer(type = 'qual', palette = 2) +
	ggtitle('WGU') +
	theme_minimal()
p_cluster_wgu
ggsave('figures/WGU-ClusterScores.pdf', width = 6, height = 4)


p_cluster_ec <- ggplot(ec.tab[order(wgu.tab$Factor),],
	   aes(x = Factor, y = mean, color = Cluster, group = Cluster)) +
	geom_hline(yintercept = 0, color = 'grey70') +
	geom_path(alpha = 0.5) +
	geom_point() +
	geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se), width = 0.25, alpha = 0.5) +
	geom_text(aes(label = round(mean, digits = 2)), hjust = -0.5, size = 3, show.legend = FALSE) +
	xlab('') + ylab('Mean Scale Score') +
	scale_x_discrete(labels = srl_labels) +
	scale_color_brewer(type = 'qual', palette = 2) +
	ggtitle('EC') +
	theme_minimal()
p_cluster_ec
ggsave('figures/EC-ClusterScores.pdf', width = 6, height = 4)

p_cluster_ua <- ggplot(ua.tab[order(ua.tab$Factor),],
					   aes(x = Factor, y = mean, color = Cluster, group = Cluster)) +
	geom_hline(yintercept = 0, color = 'grey70') +
	geom_path(alpha = 0.5) +
	geom_point() +
	geom_errorbar(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se), width = 0.25, alpha = 0.5) +
	geom_text(aes(label = round(mean, digits = 2)), hjust = -0.5, size = 3, show.legend = FALSE) +
	xlab('') + ylab('Mean Scale Score') +
	scale_x_discrete(labels = srl_labels) +
	scale_color_brewer(type = 'qual', palette = 2) +
	ggtitle('UA') +
	theme_minimal()
p_cluster_ua
ggsave('figures/UA-ClusterScores.pdf', width = 6, height = 4)

# legend <- cowplot::get_legend(
# 	# create some space to the left of the legend
# 	p_cluster_wgu + theme(legend.box.margin = margin(0, 0, 0, 12))
# )

y_limits <- c(-2.6, 1.2)

cowplot::plot_grid(p_cluster_wgu + theme(legend.position="bottom") + ylim(y_limits),
				   # p_cluster_ec + theme(legend.position="bottom") + ylim(y_limits),
				   p_cluster_ua + theme(legend.position = "bottom") + ylim(y_limits),
				   # legend = legend,
				   # rel_widths = c(1,1, 0.3),
				   nrow = 1)




ggpairs(wgu.srl1,
		columns = names(wgu.srl1),
		axisLabels = 'show',
		mapping = ggplot2::aes_string(color='cluster'),
		# upper='blank',
		upper = list(continuous="cor"),
		diag = list(continuous="densityDiag", discrete='barDiag',
				  aes_string(group='cluster', fill='cluster')),
		lower = list(continuous="density", combo="box", alpha = 0.2))
ggsave('figures/WGU-Cluster-Pairs.pdf', width = 15, height = 15)


ggpairs(ec.srl1,
		columns = names(ec.srl1)[c(1:4)],
		axisLabels = 'show',
		mapping = ggplot2::aes_string(color='cluster'),
		# upper='blank',
		upper = list(continuous="cor"),
		diag = list(continuous="densityDiag", discrete='barDiag',
					aes_string(group='cluster', fill='cluster')),
		lower = list(continuous="density", combo="box", alpha = 0.2))
ggsave('figures/EC-Cluster-Pairs.pdf', width = 15, height = 15)


##### Inference

levels(wgu.srl1_all$cluster)
wgu.srl1_all$cluster <- relevel(wgu.srl1_all$cluster, ref = 'b')
levels(ec.srl1_all$cluster)
ec.srl1_all$cluster <- relevel(ec.srl1_all$cluster, ref = 'b')


hist(wgu.srl1_all$FeedbackViews)
wgu.srl1_all$LogFeedbackViews <- log(wgu.srl1_all$FeedbackViews + 1)

lm(LogFeedbackViews ~ cluster, data = wgu.srl1_all) |> summary()
aov(LogFeedbackViews ~ cluster, data = wgu.srl1_all) |> summary()
wgu.feedback.tab <- describeBy(wgu.srl1_all$LogFeedbackViews, group = list(wgu.srl1_all$cluster), mat = TRUE)
p_wgu_feedback <- ggplot(wgu.feedback.tab, aes(x = group1, y = mean)) +
	geom_errorbar(aes(ymin = mean - se, ymax = mean + se), color = 'black', width = 0.5) +
	geom_point(size = 3) +
	xlab('Cluster') + ylab('Log of Feedback Views') +
	# ggtitle(latex2exp::TeX(paste0("$\\chi$ = ", round(wgu.chisq$statistic, digits = 2), ", p < 0.01"))) +
	theme_minimal()
p_wgu_feedback
cowplot::plot_grid(p_cluster_wgu + theme(legend.position = 'bottom'), p_wgu_feedback, rel_widths = c(3,1), nrow = 1)


# lm(CreditRatio_Term1 ~ cluster, data = wgu.srl1_all) |> summary()

aov(OnTime_Term1 ~ cluster, data = wgu.srl1_all) |> summary()

aov(SuccessTerm1 ~ cluster, data = ec.srl1_all) |> summary()

table(wgu.srl1_all$cluster, wgu.srl1_all$OnTime_Term1, useNA = 'ifany')
wgu.chisq <- chisq.test(x = wgu.srl1_all$cluster, y = wgu.srl1_all$OnTime_Term1)
wgu.chisq

table(ec.srl1_all$cluster, ec.srl1_all$SuccessTerm1, useNA = 'ifany')
ec.chisq <- chisq.test(x = ec.srl1_all$cluster, ec.srl1_all$SuccessTerm1)
ec.chisq

wgu.success.tab <- describeBy(wgu.srl1_all$OnTime_Term1, group = list(wgu.srl1_all$cluster), mat = TRUE)
ggplot(wgu.success.tab, aes(x = group1, y = mean)) +
	geom_errorbar(aes(ymin = mean - se, ymax = mean + se), color = 'black', width = 0.5) +
	geom_point(size = 3) +
	xlab('Cluster') + ylab('Percent Success Term 1') +
	ggtitle(latex2exp::TeX(paste0("$\\chi$ = ", round(wgu.chisq$statistic, digits = 2), ", p < 0.01"))) +
	theme_minimal()
ggsave('figures/WGU-OnTime-by-Cluster.png', width = 6.5, height = 3)

ec.success.tab <- describeBy(ec.srl1_all$SuccessTerm1, group = list(ec.srl1_all$cluster), mat = TRUE)
ggplot(ec.success.tab, aes(x = group1, y = mean)) +
	geom_errorbar(aes(ymin = mean - se, ymax = mean + se), color = 'black', width = 0.5) +
	geom_point(size = 3) +
	xlab('Cluster') + ylab('Percent Success Term 1') +
	ggtitle(latex2exp::TeX(paste0("$\\chi$ = ", round(ec.chisq$statistic, digits = 2), ", p = ", round(ec.chisq$p.value, digits = 2)))) +
	theme_minimal()
ggsave('figures/EC-OnTime-by-Cluster.png', width = 6.5, height = 3)




glm(OnTime_Term1 ~ cluster,
	data = wgu.srl1_all,
	family = binomial(link = 'logit')) |>
	summary()


glm(SuccessTerm1 ~ cluster,
	data = ec.srl1_all,
	family = binomial(link = 'logit')) |>
	summary()




glm(OnTime_Term1 ~ srl_metacognition + srl_motivation + srl_strategies + srl_metacognition * srl_motivation,
	data = srl1_all,
	family = binomial(link = 'logit')) |>
	summary()

glm(FeedbackViews ~ srl_metacognition + srl_motivation + srl_strategies + srl_metacognition * srl_motivation,
	data = srl1_all,
	# family = binomial(link = 'logit')
	family = gaussian()
) |>
	summary()

glm(FeedbackViews ~ srl_metacognition + srl_motivation + srl_strategies,
	data = srl1_all[srl1_all$cluster %in% c('a', 'b'),],
	# family = binomial(link = 'logit')
	family = gaussian()
) |>
	summary()

glm(FeedbackViews ~ srl_metacognition + srl_motivation + srl_strategies,
	data = srl1_all[srl1_all$cluster %in% c('c', 'd'),],
	# family = binomial(link = 'logit')
	family = gaussian()
) |>
	summary()

# glm(OnTime_Term1 ~ srl_metacognition + srl_motivation + srl_strategies,
# 	data = srl1_all[srl1_all$cluster %in% c('c', 'd'),],
# 	family = binomial(link = 'logit')
# 	# family = gaussian()
# ) |>
# 	summary()

cor(srl1_all[,cluster_cols])




# cluster::clusplot(wgu.srl1, wgu.srl1$cluster, color=TRUE, shade=TRUE, labels=2, lines=0, main = 'Bivariate Cluster Plot')
# fpc::plotcluster(wgu.srl1[,names(wgu.srl1) != 'cluster'], wgu.srl1$cluster, main = 'Discriminant projection plot')


##### Validate cluster solutions

nClusters <- 4
set.seed(2112); rows <- sample(nrow(srl1), nrow(srl1) / 2)
train <- srl1[rows, names(srl1) != 'cluster']
valid <- srl1[-rows, names(srl1) != 'cluster']

fit1 <- kmeans(train, nClusters)
fit2 <- kmeans(valid, nClusters)
fpc::cluster.stats(fit1$cluster, fit2$cluster)

cl1 <- flexclust::kcca(train, k = nClusters, kccaFamily("kmeans"))
cl2 <- flexclust::kcca(valid, k = nClusters, kccaFamily("kmeans"))

image(cl1)
points(train, col = predict(cl1), pch = 19, cex = 0.3)
points(valid, col = predict(cl1, newdata = valid), pch = 22)#, bg = "orange")

tab <- table(predict(cl2), predict(cl1, newdata = valid))
tab
randIndex(tab)


################################################################################
##### Descriptives #############################################################

##### For WGU (outcome and demographic data)

names(daacs.wgu)

table(daacs.wgu$ETHNICITY2, useNA = 'ifany')
table(daacs.wgu$EMPLOYMENT_STATUS, useNA = 'ifany')
table(daacs.wgu$FIRST_GEN_STUDENT, useNA = 'ifany')
table(daacs.wgu$GENDER, useNA = 'ifany')
table(daacs.wgu$HOUSEHOLD_INCOME, useNA = 'ifany')
table(daacs.wgu$Military, useNA = 'ifany')
describe(daacs.wgu$TRANSFER_CREDITS)
describe(daacs.wgu$Age)

# Cronbach's alpha
psych::alpha(daacs.wgu[,c('srl_metacognition', 'srl_motivation', 'srl_strategies')])

