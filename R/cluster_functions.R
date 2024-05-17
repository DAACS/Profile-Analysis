scale_this <- function(x) {
	(x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

print_p_value <- function(p_value) {
	if(p_value < 0.01) {
		"p < 0.01"
	} else {
		paste0("p = ", round(p_value, digits = 2))
	}
}

wss <- function(df, k = 9) {
	wss <- (nrow(df) - 1) * sum(apply(df, 2, var))
	for(i in 1:k) { # Calculate within sum of quares for up to 10 clusters
		wss[i] <- sum(kmeans(df, centers = i)$withinss)
	}
	return(wss)
}

silhouette_score <- function(df, k = 9, ...) {
	ssall <- numeric(length(k))
	for(i in 1:k) {
		km <- kmeans(df, centers = i, nstart = 25)
		ss <- cluster::silhouette(km$cluster, dist(df), ...)
		ssall[i] <- ifelse(is.na(ss), NA, mean(ss[, 3]))
	}
	return(ssall)
}

calinski_harabasz <- function(df, k = 9, ...) {
	cal <- numeric(length(k))
	for(i in 1:k) {
		km <- kmeans(df, i)
		cal[i] <- fpc::calinhara(df, km$cluster, ...)
	}
	return(cal)
}

davies_bouldin <- function(df, k = 9, ...) {
	davies <- numeric(length(k))
	for(i in 1:k) {
		km <- kmeans(df, i)
		db <- clusterSim::index.DB(df, km$cluster, ...)
		davies[i] <- db$DB
	}
	return(davies)
}
