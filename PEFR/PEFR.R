# The PEFR data describes the comparison of two measurements of peak expiratory flow rate (PEFR). 
# One of these measurements uses a``Large" meter and the other a ``Mini" meter.
# Two measurements were made with a Wright peak flow meter and two with a mini Wright meter, in random order.  
# All measurements were taken by the same observer, using the same two instruments. 
# (These data were collected to demonstrate the statistical method and provide no evidence on the comparability of
# these two instruments.)
#####################################################################

library(nlme)

WrightOne <- c(494, 395, 516, 434, 476, 557, 413, 442, 650,  433, 417, 656, 267, 478, 178, 423, 427)
WrightTwo <- c(490, 397, 512, 401, 470, 611, 415, 431, 638,  429, 420, 633, 275, 492, 165, 372, 421)
MiniOne <- c(512, 430, 520, 428, 500, 600, 364, 380, 658,  445, 432, 626, 260, 477, 259, 350, 451)
MiniTwo <- c(525, 415, 508, 444, 500, 625, 460, 390, 642,  432, 420, 605, 227, 467, 268, 370, 443)

######################################################################
PEFR <- c(MiniOne,MiniTwo,WrightOne,WrightTwo)
Subject <- rep(1:17,4)
repl <- c(rep(1,17),rep(2,17),rep(1,17),rep(2,17))
meth <- c(rep("M",34),rep("W",34))


Flow <- groupedData( resp ~ method | subject ,
        data = data.frame( resp = PEFR, subject = Subject,
        method = meth,
        obs = repl),
    labels = list(resp = "PEFR", method = "Measurement Device"),
    order.groups = FALSE )

######################################################################
