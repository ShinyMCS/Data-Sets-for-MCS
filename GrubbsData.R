#R Program 81 - Grubbs Gun Data

Fo=c(793.8,793.1,792.4,794.0,791.4,792.4,791.7,792.3,789.6,794.4,790.9,793.5)
Co=c(794.6,793.9,793.2,794.0,792.2,793.1,792.4,792.8,790.2,795.0,791.6,793.8)
Te=c(793.2,793.3,792.6,793.8,791.6,791.6,791.6,792.4,788.5,794.7,791.3,793.5)
#############################################################

X =Fo
Y =Co

plot(X,Y,pch=17)
abline(a=0,b=1)




n=length(X)

As=(Y+X)/2
Ds=(X-Y)
plot(As,Ds)
abline(h=0)
abline(h=mean(Ds), lty=3)
Cs=As-mean(As)





############################################################




SLR=lm(Ds~As)
summary(SLR)

to=c(0,0)
t1=c(0,0)
summary(SLR)$coefficients[,3]
dist = sum(summary(SLR)$coefficients[,3]^2)


dist = sqrt(sum(summary(SLR)$coefficients[,3]^2))
dist
to[2]=summary(SLR)$coefficients[5]
t1[2]=summary(SLR)$coefficients[6]
r =sqrt(2*qf(0.95,2,n-2))
#############################################################

plot.new()
plot.window(xlim=c(-4,4),ylim=c(-4,4))
sig.level=0.05
axis(1,pos=0,at=c(-3.3,1,2,3),labels=rep("",4),tcl=0.3,cex.axis=0.8)
axis(2,pos=0,at=c(-3.3,1,2,3),labels=rep("",4),tcl=0.3,cex.axis=0.8,las=1)
radius=sqrt(2*qf(p=sig.level,df1=2,df2=(n-2),lower.tail=FALSE))
abline(h=c(-1,1)*radius,lty=2)
abline(v=c(-1,1)*radius,lty=2)
lines(ellipse(x=0,t=radius),type="l",col="red")
points(to,t1)



as.numeric(abs(to[2])<=radius)
as.numeric(abs(t1[2])<=radius)
as.numeric(dist<=radius)
to[2]
t1[2]
tcrit
dist
radius
