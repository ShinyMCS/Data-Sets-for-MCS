
options(digits = 5, show.signif.stars = FALSE)



# make a data frame containing J and S groups only:
dat = subset(blood, subset = method != "R")
plot(dat)



# next plot
subject = c(dat$subject)
plot(subject, dat$BP, axes = FALSE, type = "n", ylab = "BP", xlim = c(3,83)) 
abline(v = seq(85), col = "grey")
box() ; axis(2); axis(4)
axis(1, at = seq(85), labels = seq(85), cex.axis = 0.6, las = 3, lwd.ticks = 0.5) 
points(subject, dat$BP, pch = c(rep(4,255),rep(1,255)), col = c(rep("deepskyblue",255),rep("magenta",255)))
legend(200, 10, legend = c("J","S"), bg = "white", pch = c(4,1),  col = c("deepskyblue","magenta"), horiz = TRUE)

# next plot
dat.J = subset(dat, subset = method == "J") 
dat.S = subset(dat, subset = method == "S") 
mean.J = c(with(dat.J, by(BP, subject, mean))) 
mean.S = c(with(dat.S, by(BP, subject, mean))) 
means = (mean.J + mean.S) / 2 
diffs = mean.S - mean.J 
dat$means = rep(means, 6) 
dat$resids = rep(diffs, 6) 
plot(dat$means, dat$BP, axes = FALSE, type = "n", xlab = "overall means (by subject)", ylab = "BP") 
abline(v = means, col = "grey", lwd = 0.5)
box() ; axis(1); axis(2); axis(4, labels = FALSE) 
points(dat$means, dat$BP, pch = c(rep(4,255),rep(1,255)), col = c(rep("deepskyblue",255),rep("magenta",255)))
legend(90, 230, legend = c("J","S"), bg = "white", pch = c(4,1),  col = c("deepskyblue","magenta"), horiz = TRUE)

#next plot
dat$BPdiffs = dat$BP - rep(mean.J, 6)
plot(dat$means, dat$BPdiffs, axes = FALSE, type = "n", xlab = "overall means (by subject)", ylab = "BP") 
abline(v = means, col = "grey", lwd = 0.5)
box() ; axis(1); axis(2); axis(4, labels = FALSE) 
points(dat$means, dat$BPdiffs, pch = c(rep(4,255),rep(1,255)), col = c(rep("deepskyblue",255),rep("magenta",255)))
legend(90, 100, legend = c("J","S"), bg = "white", pch = c(4,1),  col = c("deepskyblue","magenta"), horiz = TRUE) 
points(means, diffs, pch = 16, cex = 1, col = "black")
