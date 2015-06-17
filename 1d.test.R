rm(list=ls())
smp <- c(1,3,3,3,3,3,3,3,1,3,2,3,4,3,3,4,2)

mean(smp)

t.test(smp, mu=3, alternative="greater")

hist(smp)

str(chickwts)
w <- chickwts$weight

mean(w)

t.test(w, mu=270, alternative="two.sided")
