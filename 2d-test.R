smp.size  <- 300
smp1.mean <- 71
smp1.sd   <- 14

smp2.mean <- 69
smp2.sd   <- 11

dt.smp1 <- rnorm(smp.size, smp1.mean, smp1.sd)

dt.smp2 <- rnorm(smp.size, smp2.mean, smp2.sd)


plot(c(dt.smp1, dt.smp2), type="n")
grid()

points(dt.smp1, col="steelblue")
points( c(rep(NA,300),dt.smp2), col="salmon")
abline(h=mean(dt.smp1), col="steelblue")
abline(h=mean(dt.smp2), col="salmon")

legend(
  "topleft",
  legend=c(
    "sample #2 = N(7,11)",
    "sample #2 mean"
  ),
  pch=c(1,NA),
  lty=c(NA,1),
  col="steelblue",
  bg="snow",
  cex=.6,
)

legend(
  "topright",
  legend=c(
    "sample #1 = N(61,11)",
    "sample #1 mean"
  ),
  pch=c(1,NA),
  lty=c(NA,1),
  col="salmon",
  bg="snow",
  cex=.6,
)

test <- t.test(dt.smp1, dt.smp2, paired=FALSE, var.equal=FALSE )

legend("bottom",
       legend = c(
         sprintf("p-value=%f",test$p.value)
         ),
       cex=.8, bg="snow"
       )

print (test)
