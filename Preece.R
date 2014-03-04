options(digits = 5, show.signif.stars = FALSE)


Straight = c(24,19.5,8.2,12.1,8,8.2,10.1,5.5,10.1,7.2,5.6)
Oscillating = c(15,6.6,1.9,1.5,1.1,2.5,0.6,0.5,0.5,3.1,2.1,1.6)

Ds = Straight - Oscillating
As = (Straight + Oscillating)/2
Cs = As - mean(As)
summary( lm( Ds ~ Cs ) )


sum((Ds-mean(Ds))^2)-((sum((Ds-mean(Ds))*Cs))^2)/sum(Cs^2)




#
