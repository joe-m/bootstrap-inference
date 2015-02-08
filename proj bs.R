require(boot)
# generate bs estimates for mean
pop <- gravity$g
mu <- mean(pop)
sam.size <- 10
sam <- sample(pop, sam.size, replace = T)
xbar <- mean(sam)
nsims <- 1000
bs <- numeric(nsims)
for(i in 1:nsims)  {   #bs sims
        bs.sam.ind <- sample(1:sam.size, sam.size, replace = T)
        bs[i] <- mean(sam[bs.sam.ind])}
xbarstar <- mean(bs)
#mean(bs); mean(sam) 
var(bs); var(sam)

# 95% bs confidence interval for mu
bs.ci <- xbar + c(-1,1)*quantile(abs(bs - xbar), 0.95) 

# 95% z-based CI for mu
z.ci <- xbar + c(-1,1)*1.96*sqrt(var(sam)) / sqrt(sam.size)