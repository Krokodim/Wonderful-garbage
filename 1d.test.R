pop.size <- 1000  # population size
pop.mean <- 5     # population mean
pop.sd   <- .5     # population sd
smp.size <- 10    # sample sze

# populate the population
dt.pop <- rnorm(pop.size, pop.mean, pop.sd)

# subset a sample
dt.smp <- dt.pop[smp.idx <- sample(1:pop.size, smp.size)]

# do a test that sample mean equals the population mean
#test <- t.test(dt.smp,  mu=pop.mean, alternative="two.sided")

test <- wilcox.test(dt.smp, mu=pop.mean, alternative="two.sided", conf.int = T)

print(test)

# draw the population
plot(dt.pop, pch=20, col="grey", main="Sample mean T-test")
# and it's mean
abline (h=mean(dt.pop), col="red", lwd=2)

# draw the sample
points(smp.idx, dt.smp, pch=1, col="blue")
grid()
# and it's mean
abline (h=mean(dt.smp), col="blue", lwd=1, lty=2)

# draw the confidence interval for the sample mean
rect(0,
     y1 <- max(c(test$conf.int[1], min(dt.pop)-pop.sd)),
     pop.size, 
     y2 <- min(c(test$conf.int[2], max(dt.pop)+pop.sd)),
     border="steelblue", col="steelblue", lty=3, density=20)



legend(
  "topleft",
  legend=c("population","sample"),
  pch=c(20,1), col=c("grey","blue"), ncol=2,
  cex=1, bg="snow"
)

r2 <- legend(
  "topright",
  legend=c(
    sprintf("population mean = %.4f",mean(dt.pop)), 
    sprintf("sample mean = %.4f",mean(dt.smp)),
    "sample mean confidence interval"
  ),
  col=c("red","blue"),lty=c(1,2,NA), 
  lwd=c(2,1,NA), 
  cex=.6, bg="snow"
)

rect(
  r2$text$x[3]-strwidth("w", cex=.6)*4,
  r2$text$y[3]- r2$rect$h/6,
  r2$text$x[3]-strwidth("w", cex=.6),
  r2$text$y[3]+ r2$rect$h/11,
  border="steelblue", col="steelblue", lty=3, density=20)


r3 <- legend(
  "bottomright",cex=.5, bg="snow",
  legend=c(
    sprintf("p-value: %f",test$p.value),
    sprintf("df=%.0f",test$parameter),
    sprintf("t-statistic=%.3f",test$statistic),
    sprintf("conf.int. =[%.2f, %.2f]",test$conf.int[1],test$conf.int[2])
  )
)

# a piece of alchemistry
p.bar.bottom <- r3$rect$top + r3$rect$h/6
p.bar.top    <- p.bar.bottom + r3$rect$h/5
p.bar.left  <-  r3$rect$left
p.bar.right  <- r3$rect$left + r3$rect$w
p.bar.pos    <- p.bar.left + (p.bar.right - p.bar.left) * test$p.value

# draw the p-gauge
rect(p.bar.left, p.bar.bottom, p.bar.pos, p.bar.top, border="grey", col="grey")
rect(p.bar.left, p.bar.bottom, p.bar.right,  p.bar.top)

