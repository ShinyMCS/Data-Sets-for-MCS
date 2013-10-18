# Diabetes patients attending an outpatient clinic in Denmark have their HbA1c levels
# routinely measured at every visit.Venous and Capillary blood samples were obtained
# from all patients appearing at the clinic over two days. Samples were measured on four
# consecutive days on each machines, hence there are ve analysis days.
#
# Carstensen et al. (2008) notes that every machine was calibrated every day to the
# manufacturers guidelines. Measurements are classied by method, individual and repli-
# cate. In this case the replicates are clearly not exchangeable, neither within patients
# nor simulataneously for all patients.
#
###########################################################################################

install.packages("MethComp")
library(MethComp)

data(HbA1c)

summary(HbA1c)
