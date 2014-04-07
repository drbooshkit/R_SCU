# assume pop = 300000
people <- seq(1,300000)
people_aids <- sample(people,1500)	#samples from set without replacement
people_noaids <- setdiff(people,people_aids)	#setdiff is difference between sets

people_aids_pos <- sample(people_aids, 1500*.99) #of the set that has aids, 99% positive test
people_aids_neg <- setdiff(people_aids, people_aids_pos) #left over 1% of people who have it but don't have positive test

people_noaids_neg <- sample(people_noaids, 298500*0.95) #sub-sample of conifrmation accuracy
people_noaids_pos <- setdiff(people_noaids, people_noaids_neg)

# compute conditional probabilities
# use constructed popullation sub-sets
pr_aids_given_pos <- (length(people_aids_pos))/(length(people_aids_pos) + length(people_noaids_pos))

