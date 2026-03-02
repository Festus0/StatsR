Herbicides <- read.csv("Herbicides.csv")
Herbicides$Treatment

Herbicides$Treatment <- relevel(Herbicides$Treatment , ref="None")
boxplot(Yield ~ Treatment, data = Herbicides)

model1 <- aov(Yield ~ factor(Treatment), data = Herbicides)
summary.aov(model1)

model.tables(model1, "means", se = TRUE)


library(lsmeans)
lsm <- lsmeans(model1, ~Treatment)
pairs(lsm, reverse = TRUE, adjust = "none")
#TukeyHSD(model1)
contr <- list("None vs others" = c(1, -0.25, -0.25, -0.25, -0.25),
              "WeedAway - None" = c(1, 0, 0, -1, 0))
contrast(lsm, contr)


lm_model1 <- lm(Yield ~ Treatment, data = Herbicides)
summary(lm_model1)



### Fertilizer ###################


Fertilizer <- read.csv("Fertilizer.csv")
names(Fertilizer)[2] <- "Treatment"
Fertilizer$Treatment <- as.factor(Fertilizer$Treatment)
Fertilizer$Block <- as.factor(Fertilizer$Block)
View(Fertilizer)

boxplot(Yield ~ Treatment, data=Fertilizer, 
        main = "Yield by Treatment")
boxplot(Yield ~ Block, data=Fertilizer, 
        main = "Yield by Block")

anova_fert <- aov(Yield ~ Block + Treatment, data=Fertilizer)
anova(anova_fert)
modtab <- model.tables(anova_fert, "means", se = TRUE)

mean_trt0<-as.numeric(modtab$tables$Treatment[1])
mean_trt200<-as.numeric(modtab$tables$Treatment[5])
sed <- as.numeric(modtab$se$Treatment)

T_obs <- (mean_trt200 - mean_trt0)/sed
qt(0.95, df = 12) ###reject the null hypothesis of equality between means

