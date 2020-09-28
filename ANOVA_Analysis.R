##################################################################################################
# ANOVA Analysis Different Data Sets
# Author: Burcu Ozek
################################################################################################

library(readxl)
X14_17 <- read_excel("Desktop/14.17.xlsx")

X14_17$A<- as.factor(X14_17$A)
X14_17$B<- as.factor(X14_17$B)
X14_17$C<- as.factor(X14_17$C)

model = glm(measurements ~ C + A + B + A*B + A*C + B*C + A*B*C,
           data = X14_17)

Anova(model,
      type = "II")

aov(model)
summary(aov(model))


##################################################################################################

library(readxl)
X14_18 <- read_excel("Desktop/14.18.xlsx")
#View(X14_18)

X14_18$A<- as.factor(X14_18$A)
X14_18$B<- as.factor(X14_18$B)
X14_18$C<- as.factor(X14_18$C)

model_2 = glm(measurements ~ C + A + B + A*B + A*C + B*C + A*B*C,
           data = X14_18)

Anova(model_2,
      type = "II")

aov(model_2)
summary(aov(model_2))
model_2


##################################################################################################

library(readxl)
X14_33 <- read_excel("Desktop/14.33.xlsx")
View(X14_33)
X14_33$Floor<- as.factor(X14_33$Floor)
X14_33$Sweet<- as.factor(X14_33$Sweet)

model_3 = glm(measurements ~ Floor + Sweet + Floor* Sweet,
              data = X14_33)

aov(model_3)
summary(aov(model_3))
anova(model_3)
summary(anova(model_3))
