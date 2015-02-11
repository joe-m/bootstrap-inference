require(boot)
project=function(data,ss,z){ #data=vector of population data ss=sample size
        #pop and sample information
        N=length(data) #popuation size
        mu=mean(data)
        print("mu"); print(mu)
        sam=sample(data,ss) #takes a sample of size ss
        print("Sample"); print(sam)
        
        #HT estimation
        Pi=ss/N
        Tau_ht=sum(sam/Pi) #divides every sampled observation by Pi
        mu_ht=Tau_ht/N
        print("HT Estimate of Population Total"); print(Tau_ht); 
        print("mu_ht"); print(mu_ht)
        Pij=(ss/N)*((ss-1)/(N-1))*
                matrix(rep(1,ss*ss),ss)+(ss/N)*((N-ss)/(N-1))*diag(rep(1,ss)) #Pij matrix
        Pii=diag(Pij) #takes Pii's out of the Pij matrix
        PiPj=Pii%o%Pii
        yiyj=sam%o%sam
        SighatHT=sqrt((sum((Pij-PiPj)*yiyj/(Pij*PiPj)))/(N*N)) #HT est of variance of estimated mean
        print("SighatHT"); print(SighatHT)
        zCI=c(mu_ht+c(-1,1)*z*SighatHT) #zCI confidence interval
        print("95% CI");  print(zCI)
        
        #BS estimation
        xbar <- mean(sam)
        print("xbar"); print(xbar)
        nsims <- 1000
        bs <- numeric(nsims)
        for(i in 1:nsims)  {   #bs sims
                bs.sam.ind <- sample(1:ss, ss, replace = T)
                bs[i] <- mean(sam[bs.sam.ind])}
        xbarstar <- mean(bs)
        print("xbarstarBS"); print(xbarstar)
        #mean(bs); mean(sam)
        var(bs); var(sam)
        # 95% bs confidence interval for mu
        bs.ci <- xbar + c(-1,1)*quantile(abs(bs - xbar), 0.95)
        print("BS 95% CI"); print(bs.ci)
        
        # 95% z-based CI for mu (not BS)
        z.ci <- xbar + c(-1,1)*1.96*sqrt(var(sam)) / sqrt(ss)
        print("z-based CI for BS estimate"); print(z.ci)}

data=gravity$g
project(data,ss,1.96)
