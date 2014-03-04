options(digits = 5, show.signif.stars = FALSE)

# GROWTH OF BONE MARROW OF TUBERCULAR GUINEA 
# PIGS CULTURED WITH/WITHOUT TUBERCULIN 
# Table 11.1, Simple Experimental Designs, page 282 of 
# Bliss, C.I., Statistics in Biology, Volume 1, McGraw-Hill, 1967
# 	y1 = "without" and y2 = "with"

y1 = c(39.2, 14.5, 27.5, 14.5, 32.5, 43.6, 77.5, 55, 55, 7.25, 23, 39.25, 24.75, 26.5, 16.6)
y2 = c(17.25, 7.8, 14.75, 9.75, 14, 11, 14, 14.75, 23, 6.4, 12, 22.25, 14, 13.75, 5.6)

plot(y1, y2, xlim = c(0,80), ylim = c(0,80), axes = FALSE, xlab = "Without", ylab = "With")
axis(1); axis(2); abline(c(0,1), col = "red", lty = 2)

n = length(y1); xx = c(rep(1,n),rep(2,n)); yy = c(y1,y2)
plot(xx, yy, xlim = c(0.75,2.25), ylim = c(0,80), axes = FALSE, xlab = "", ylab = "", type = "n")
axis(2) ;  axis(1, at = c(1,2), labels = c("Without","With"))
title(main = "Growth of bone marrow of tubercular guinea\n pigs cultured with/without tuberculin")
segments(rep(1,n), y1, rep(2,n), y2, lty=2)
points(xx, yy, pch = 16)


s = y1 + y2
d = y1 - y2

t.test(d)



plot(x,y)
plot(s,d)

slr = lm(d ~ I(s-mean(s)))
summary(slr)










