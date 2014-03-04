
# Data used in Bartko (1994)

X = c(52,53,59,60,59,59,57,53,54)
Y = c(58,55,56,54,59,60,59,58,52)

Ds = (X - Y)
Ms = (X + Y)/2

plot( Ms , Ds , xlab = "Average of two methods", 
    ylab = "Difference between two methods" , xlim = c(50,63),
    ylim = c(-10,10), main = "Eye tracking data in milliseconds")
avgDs = mean(Ds); sdDs = sd(Ds)
abline( h=c(avgDs - 2*sdDs, avgDs + 2*sdDs), lty = 2, col = 3)
abline( h = avgDs , lty = 2, col = "grey")




# Student's paired t-test
# t = -0.4373, df = 8, p-value = 0.6735    

t.test(Ds)




# Morgan-Pitman
# t = 0.47, df = 7, p-value = 0.6526

cor.test(Ds,Ms, method = "pearson")





# BBB
# slope has t* = 0.47, p-value = 0.65
# F* = 0.19675, p-value = 0.82579

FitFull = lm(Ds ~ Ms)  #;  summary(FitFull)
                    
SSE = anova(FitFull)[[2]][2]
Fstar = ((sum(Ds^2)-SSE)/2)/(SSE/(9-2))
Fstar
pf(Fstar, df1=2, df2=(9-2), lower.tail = FALSE)



Zs = Ms - mean(Ms)
SLR = lm(Ds ~ Zs)
summary(SLR)






FitNull = lm( Ds ~ -1 )
anova(FitFull,FitNull)

#   The "new" test:

Zs = Ms - mean(Ms)

SLR = lm(Ds ~ Zs)


summary(SLR)

anova(lm(Ds ~ Ms))
anova(lm(Ds ~ Zs))



112.667  16.095

((sum(Ds^2)-112.667)/2)/16.095

(sum(Ds^2)-112.667)/(2*16.095)

(-0.415)^2 +  0.470^2


    # Coefficients:
    #             Estimate Std. Error t value Pr(>|t|)
    # (Intercept)   -0.556      1.337   -0.42     0.69
    # Zs             0.296      0.630    0.47     0.65











points( x = Ms[c(1,18)] , y = Ds[c(1,18)] , pch = 16, col =2)






#
