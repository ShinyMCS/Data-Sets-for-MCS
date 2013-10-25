#R Program 81 - LUIZ
S1=c(100,58	,95	,55	,79	,95	,60	,88	,68	,94	,60	,64	,88	,57	,
66	,67	,76	,95	,85	,105	,80	,85	,82	,102	,100	,75	,40	,70	,63	,103	,95	,
80	,72	,68	,48	,70	,90	,60	,80	,96	,54	,80	,88	,70	,90	,79	,
100	,85	,108	,53	,58	,49	)


S2=c(97	,77	,74	,59	,79	,85	,78	,78	,68	,96	,74	,64	,76	,60	,78	,71	,67	,103	,95	,78	,70	,80	,78	,102	,102	,77	,
45	,60	,50	,94	,91	,66	,63	,65	,58	,75	,105	,65	,80	,90	,58	,75	,
83	,78	,85	,65	,90	,76	,100	,65	,40	,53	)



#############################################################

X=S1
Y=S2

n=length(X)

As=(Y+X)/2
Ds=(X-Y)
plot(As,Ds)
abline(h=0)
abline(h=mean(Ds), lty=3)
Cs=As-mean(As)
SLR=lm(Ds~Cs)
summary(SLR)


#            Estimate Std. Error t value Pr(>|t|)  
#(Intercept) -0.23333    0.97874  -0.238   0.8133  
#Cs           0.09250    0.04991   1.853   0.0744 .


to=c(0,0)
t1=c(0,0)
summary(SLR)$coefficients[,3]
dist = sum(summary(SLR)$coefficients[,3]^2)


dist = sum(summary(SLR)$coefficients[,3]^2)
to[2]=summary(SLR)$coefficients[5]
t1[2]=summary(SLR)$coefficients[6]
r =SQRT(2*qf(0.95,2,n-2))
tcrit=qt(0.95,n-1)

as.numeric(abs(to[2])<=tcrit)
as.numeric(abs(t1[2])<=tcrit)
as.numeric(dist<=r)
#############################################################



plot(x=to,y=t1, xlim=c(-6,6),ylim=c(-6,6))
abline(h=0,col=2,lty=2)
abline(v=0,col=2,lty=2)



zone=qt(0.95,n-1)
abline(h=zone,col=3,lty=2)
abline(h=-zone,col=3,lty=2)
abline(v=zone,col=3,lty=2)

abline(v=-zone,col=3,lty=2)
