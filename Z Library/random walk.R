#The rle function is named for the acronym of "run length encoding". What does the term "run length" mean? Imagine you flip a coin 10 times and record the outcome as "H" if the coin lands showing heads, and "T" if the coin lands showing tails. You want to know what the longest streak of heads is. You also want to know the longest streak of tails. The run length is the length of consecutive types of a flip. If the outcome of our experiment was "H T T H H H H H T H", the longest run length of heads would be 5, since there are 5 consecutive heads starting at position 4, and the longest run length for tails would be 2, since there are two consecutive heads starting at position 2. If you just have 10 flips, it is pretty easy to simply eyeball the answer. But if you had 100 flips, or 100,000, it would not be easy at all. However, it is very easy with the rle function in R! That function will encode the entire result into its run lengths. Using the example above, we start with 1 H, then 2 Ts, 5 Hs, 1 T, and finally 1 H. That is exactly what the rle function computes, as you will see below in the example.

#SAMPLE = sample(c("HEADS", "TAILS"), 100000, replace = TRUE)
#table(SAMPLE)

#sample.rle <- rle(SAMPLE)
#str(sample.rle)
#sort(sample.rle$lengths, decreasing = TRUE)
#hist(sample.rle$lengths, freq = TRUE, breaks = "Sturges")


p <- 0.5


p_matrix <- matrix(c(1-p,p,p,1-p),nrow=2)
p_matrix