#' Profile plot for cluster analysis.
#'
#' @param df data.frame with the columns used for the cluster analysis.
#' @param clusters vector indicating what cluster each row of `df` belongs to.
#' @param df_dep a data.frame with any dependent variables to include in the plot (optional).
#' @param normalize if TRUE values in `df` will be converted to z-scores.
#'
#' @import ggplot2
#' @importFrom dplyr mutate_if all_of
#' @importFrom reshape2 melt
#' @importFrom psych describeBy
#' @importFrom latex2exp TeX
profile_plot = function(
		df,
		clusters,
		df_dep,
		normalize = TRUE,
		text_size = 2,
		hjust = -0.1,
		point_size = 2,
		se_factor = 1.96,
		color_palette = 2,
		cluster_labels,
		cluster_order,
		ylab = ifelse(normalize, 'Mean Standard Score', 'Mean Score'),
		title = 'Cluster Profiles'
) {
	if(length(clusters) != nrow(df)) {
		stop('length of clusters is not the same as the number of rows in df.')
	}

	if(!is.factor(clusters)) {
		clusters <- as.factor(clusters)
	}

	n_clusters <- clusters |> levels() |> length()

	if(missing(cluster_labels)) {
		cluster_labels <- levels(clusters)
	}

	cluster_labels <- paste0(LETTERS[1:n_clusters], ': ', cluster_labels)

	print_p_value <- function(p_value) {
		if(p_value < 0.01) {
			"p < 0.01"
		} else {
			paste0("p = ", round(p_value, digits = 2))
		}
	}

	scale_this <- function(x) {
		(x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
	}

	if(normalize) {
		df <- df |> dplyr::mutate_if(is.numeric, scale_this)
	}

	plots <- list()

	nClusters <- clusters |> unique() |> length()
	df.melted <- reshape2::melt(cbind(df, cluster = as.character(clusters)), id.vars = 'cluster')
	tab <- psych::describeBy(df.melted$value, group = list(df.melted$cluster, df.melted$variable), mat = TRUE)
	tab <- tab[,c('group1', 'group2', 'n', 'mean', 'se', 'sd', 'median')]
	names(tab)[1:2] <- c('Cluster', 'Factor')

	if(!missing(cluster_order)) {
		tab$Factor <- factor(tab$Factor,
							 levels = cluster_order,
							 ordered = TRUE)
	}

	plots[[length(plots)+1]] <- ggplot(tab[order(tab$Factor),],
						aes(x = Factor, y = mean, color = Cluster, group = Cluster)) +
		geom_hline(yintercept = 0, color = 'grey70') +
		geom_path(alpha = 0.5) +
		geom_point(size = point_size) +
		geom_errorbar(aes(ymin = mean - se_factor * se, ymax = mean + se_factor * se), width = 0.25, alpha = 0.5) +
		geom_label(aes(label = round(mean, digits = 2)),
				   fill = 'white', hjust = hjust, size = text_size, show.legend = FALSE) +
		xlab('') + ylab(ylab) +
		scale_color_brewer(type = 'qual', palette = color_palette, labels = cluster_labels) +
		theme_minimal() +
		theme(legend.position = 'bottom', legend.key.size = unit(0, 'lines')) +
		ggtitle(title, subtitle = paste0('k = ', nClusters))

	if(missing(df_dep)) {
		return(plots[[1]])
	} else {
		if(is.vector(df_dep)) {
			df_dep <- data.frame(y = df_dep)
		}

		for(i in 1:ncol(df_dep)) {
			dep_tab <- psych::describeBy(df_dep[,i,drop=TRUE],
										 group = list(clusters), mat = TRUE)
			dep_tab$group1 <- factor(dep_tab$group1,
									 levels = levels(clusters),
									 labels = cluster_labels)

			plots[[length(plots)+1]] <- ggplot(dep_tab, aes(x = group1, y = mean, color = group1)) +
				geom_errorbar(aes(ymin = mean - se_factor * se, ymax = mean + se_factor * se, color = group1), width = 0.5) +
				geom_point(size = point_size) +
				geom_label(aes(label = paste0(round(mean, digits = 2))), fill = 'white', hjust = hjust, size = text_size) +
				xlab('Cluster') + ylab('') +
				theme_minimal() +
				scale_color_brewer(type = 'qual', palette = color_palette) +
				scale_x_discrete('', labels = levels(clusters)) +
				theme(legend.position = 'none')

			col <- df_dep[,i,drop=TRUE]
			if(is.factor(col) | is.character(col) | is.logical(col)) { # chi-squared
				chisq_out <- chisq.test(clusters, col)
				x2_str <- paste0("$\\chi^2$ = ", round(chisq_out$statistic, digits = 2), ", ",
									  print_p_value(chisq_out$p.value))
				plots[[length(plots)]] <- plots[[length(plots)]] +
					ggtitle(names(df_dep)[i], subtitle = latex2exp::TeX(x2_str))
			} else { # ANOVA
				aov_out <- aov(col ~ clusters) |> summary()
				f_str <- paste0("$F_{", paste0(aov_out[[1]][1:2,1], collapse = ", "), "}$ = ",
								round(aov_out[[1]][1,4], digits = 2), ", ",
								print_p_value(aov_out[[1]][1,5]))
				plots[[length(plots)]] <- plots[[length(plots)]] +
					ggtitle(names(df_dep)[i], subtitle = latex2exp::TeX(f_str))
			}
		}

		grobs <- ggplotGrob(plots[[1]])$grobs
		legend <- grobs[[which(sapply(grobs, function(x) x$name) == "guide-box")]]

		plots[[1]] <- plots[[1]] + theme(legend.position = 'none')
		plots$nrow <- 1
		plots$rel_widths <- c(3, rep(1, ncol(df_dep)))

		cowplot::plot_grid(
			do.call(
				cowplot::plot_grid,
				args = plots
			),
			legend,
			ncol = 1,
			rel_heights = c(10, 1))
	}
}

if(FALSE) {
	library(palmerpenguins)
	library(ggplot2)

	data(penguins)
	n_clusters <- 3
	cluster_cols <- c('bill_depth_mm', 'bill_length_mm', 'body_mass_g', 'flipper_length_mm')
	# cluster_cols <- c('bill_depth_mm', 'body_mass_g' )
	penguins <- penguins |>
		tidyr::drop_na(dplyr::all_of(cluster_cols))
	kmeans_fit <- kmeans(penguins[,cluster_cols], n_clusters)
	clusters <- factor(kmeans_fit$cluster, labels = LETTERS[1:n_clusters])

	profile_plot(df = penguins[,cluster_cols],
				 clusters = clusters,
				 df_dep = penguins[,'body_mass_g',drop=FALSE])

	ggplot(penguins, aes(x = bill_depth_mm, y = bill_length_mm, color = clusters)) +
		geom_point()

	table(clusters, penguins$species)
}
