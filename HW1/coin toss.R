source("flip_streak_dist.R")
source("rescale.R")

res <- flip_streak_dist(10000)	#return vector of streaks (values sum to n)
res$counts[length(res$breaks)] <- 0

res.matrix <-  cbind(res$breaks,res$counts)
res.matrix <- data.frame(res.matrix)

m <- ggplot(res.matrix, aes(x = X1, y=X2)) +
  geom_histogram(binwidth = 1, stat="identity",
	colour="darkgreen", fill="white")
m + scale_y_log10
