#given n_tosses, this will return a 
flip_streak_dist <- function(n_tosses) {

#Generate a sequence of n_tosses random tosses of a coin.
flip_set_random <- sample(0:1,n_tosses,replace=TRUE)

#VERIFY sample set
#go through each toss and check if it is 1 or 0
#Report the number of heads and tails (set 1 = heads, 0 = tails)
#head_count <- 0
#tail_count <- 0
#for (i in 1:n_tosses) {
#	if (flip_set_random[i]==1) head_count <- head_count+1
#	if (flip_set_random[i]==0) tail_count <- tail_count+1
#what's the percentage heads or tails?
#percent_heads = head_count/n_tosses
#percent_tails = tail_count/n_tosses
###turn this into a vector and plot. verify it’s 50-50.
#flip_distribution = c(percent_heads,percent_tails)
#barplot(flip_distribution, xlab="Heads Tails", ylab="Percent of flips") 

#count the length of run sequences in this list of heads and tails. A run is an unbroken #sequence of heads or tails. For example, the sequence TTHTHHHHTT has two heads runs of #lengths 1 and 4, and three tails runs, of lengths 2, 1,2. 
findstreak <- function(x) {
	n <- length(x)	#set n equal to number of random tosses
	flip_streak <- NULL	#flip_streak will hold the length for each of j streaks
	count <- 1
	j <- 1
	for (k in 2:n){
		if (x[k] != x[k-1]) {
			flip_streak[j] <- count
			count <- 1
			j <- j+1
			if(k==n) flip_streak[j] <- count   #this is a kludge, but need last flip
			} else {
			count <- count+1
			if(k==n) flip_streak[j] <- count
	}
}
return(flip_streak)	
}

#call the findstreak() function with random set
res <- findstreak(flip_set_random)
return(res)
}
