# Red cell volume measured simultaneously
# in 19 patients using radioactive (Cr) and
# non radioactive (biotin) cell labels.

rcv = data.frame(Cr = c(1267, 1710, 1882, 1914, 1940, 1976, 2033, 2039, 2077, 2087, 2102, 2139, 2184, 2192, 2393, 2425, 2554, 2600, 3420), Biotin = c(1954, 1651, 1887, 2043, 2054, 2075, 1976, 2120, 2061, 2152, 1894, 1982, 2153, 2288, 2628, 2495, 2463, 3186, 3488) )


options(digits = 5, show.signif.stars = FALSE)

# Red cell volume measured simultaneously in 19 patients 
# using radioactive (Cr) and non radioactive (biotin) 
# cell labels. (Cavill et al., 1988).

Cavill = read.table("Cavill.txt", header = TRUE)
Cr = Cavill$Cr; Biotin = Cavill$Biotin

#xtable(cbind(Cavill[1:10,],rbind(Cavill[11:19,],c(999,999))))
#plot(Cr,Biotin,xlim=c(1250,3500),ylim=c(1250,3500))

Ds <- (Biotin - Cr)
Ms <- (Biotin + Cr)/2
plot( Ms , Ds , xlab = "Average of two methods", 
    ylab = "Difference between two methods" , xlim = c(1200,3600),
    ylim = c(-670,670), main = "Red cell volume")
avgDs <- mean(Ds); sdDs <- sd(Ds)
abline( h=c(avgDs - 2*sdDs, avgDs + 2*sdDs), lty = 2, col = 3)
abline( h = avgDs , lty = 2, col = "grey")
points( x = Ms[c(1,18)] , y = Ds[c(1,18)] , pch = 16, col =2)

cor.test(Ds,Ms, method = "pearson")



getG(Ds)
g.crit(n=19)
which(abs(Ds) == max(abs(Ds)))

lines(ellipse(cor(Ms,Ds), centre = c( mean(Ms),mean(Ds) ) , 
    scale = c( sd(Ms) , sd(Ds) ) ) , col = 2)

lines(ellipse(0, centre = c( mean(Ms),mean(Ds) ) , 
    scale = c( sd(Ms) , sd(Ds) ) ) , lty = 2, col = 4)
    

