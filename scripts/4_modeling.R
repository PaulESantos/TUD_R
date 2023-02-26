################################################################################
#                                                                              #
#                     "Reproducible Data Analysis with R"                      #
#                     Cédric Scherer | Feb 27-Mar 3, 2023                      #
#                                                                              #
#                              --- Session 4 ---                               #
#                        "Data Analysis and Modeling"                          #
#                                                                              #
################################################################################


## PREPARATION #################################################################

## This session makes use of the following packages:
##  - readr
##  - dplyr 
##  - forcats 
##  - ggplot2  
##  - lmtest 
##  - lme4 
##  - BSDA
##  - ggforce

## Please install the missing packages by running the following:
pkgs <- c("readr", "dplyr", "forcats", "ggplot2", "lmtest", "lme4", 
          "BSDA", "ggforce")
unavailable <- setdiff(pkgs, rownames(installed.packages()))
install.packages(unavailable)


## That's it---let's start!




## -----------------------------------------------------------------------------
birds <- readr::read_csv("./data/birds.csv")


## -----------------------------------------------------------------------------
birds


## -----------------------------------------------------------------------------
library(dplyr)
birds <- mutate(birds, across(c("family", "diet", "passerine", "aquatic"), factor))


## -----------------------------------------------------------------------------
birds


## -----------------------------------------------------------------------------
birds <- readr::read_csv("./data/birds.csv", col_types = "fdddfff")


## -----------------------------------------------------------------------------
birds


## -----------------------------------------------------------------------------
mean(birds$avg_abund)


## -----------------------------------------------------------------------------
mean(birds$mass)


## -----------------------------------------------------------------------------
median(birds$avg_abund)


## -----------------------------------------------------------------------------
median(birds$mass)


## -----------------------------------------------------------------------------
var(birds$avg_abund)


## -----------------------------------------------------------------------------
var(birds$mass)


## -----------------------------------------------------------------------------
sd(birds$avg_abund)


## -----------------------------------------------------------------------------
sd(birds$mass)


## -----------------------------------------------------------------------------
quantile(birds$avg_abund)


## -----------------------------------------------------------------------------
diff(as.numeric(quantile(birds$avg_abund, c(0.25, 0.75))))
## also works without `as.numeric()`


## -----------------------------------------------------------------------------
IQR(birds$avg_abund)  
# diff(quantile(as.numeric(birds$avg_abund), c(0.25, 0.75)))


## -----------------------------------------------------------------------------
range(birds$avg_abund)


## -----------------------------------------------------------------------------
diff(range(birds$avg_abund))  
# max(birds$avg_abund) - min(birds$avg_abund)


## -----------------------------------------------------------------------------
shapiro.test(birds$avg_abund)


## -----------------------------------------------------------------------------
ks.test(birds$avg_abund, "pnorm")


## -----------------------------------------------------------------------------
chisq.test(birds$avg_abund, p = rep(1 / nrow(birds), nrow(birds))) 
## uniform distribution


## -----------------------------------------------------------------------------
rep(1 / nrow(birds), nrow(birds))


## -----------------------------------------------------------------------------
t.test(birds$avg_abund, mu = 0, alternative = "two.sided")


## -----------------------------------------------------------------------------
t.test(birds$avg_abund, mu = 5, alternative = "two.sided")  ## true mean: 5.7


## -----------------------------------------------------------------------------
t.test(birds$avg_abund, mu = 0, alternative = "two.sided")  
## true mean: 5.7


## -----------------------------------------------------------------------------
t.test(birds$avg_abund, mu = 0, alternative = "greater")  
## true mean: 5.7


## -----------------------------------------------------------------------------
shapiro.test(birds$avg_abund)


## -----------------------------------------------------------------------------
shapiro.test(log10(birds$avg_abund))


## -----------------------------------------------------------------------------
t.test(log10(birds$avg_abund), mu = 0, alternative = "two.sided")  
## true mean: .5


## -----------------------------------------------------------------------------
t.test(log10(birds$avg_abund), mu = .5, alternative = "two.sided")  
## true mean: .5


## -----------------------------------------------------------------------------
set.seed(1)
dist <- rnorm(n = 1000, mean = 500, sd = 100)


## -----------------------------------------------------------------------------
# install.packages("BSDA")
BSDA::z.test(dist, mu = 500, sigma.x = 100)


## -----------------------------------------------------------------------------
count(birds, aquatic)


## -----------------------------------------------------------------------------
var.test(avg_abund ~ aquatic, data = birds)


## -----------------------------------------------------------------------------
t.test(avg_abund ~ aquatic, data = birds, 
       alternative = "two.sided", var.equal = TRUE)


## -----------------------------------------------------------------------------
## mean of level 1 is "greater" than that of level 2
t.test(avg_abund ~ aquatic, data = birds, 
       alternative = "greater", var.equal = TRUE)


## -----------------------------------------------------------------------------
library(ggplot2)

ggplot(birds, aes(x = aquatic, y = avg_abund)) +
  geom_boxplot(notch = TRUE) +
  scale_x_discrete(labels = c("Terrestrial families", "Aquatic families")) +
  labs(x = NULL, y = "Average abundance")


## -----------------------------------------------------------------------------
count(birds, passerine)


## -----------------------------------------------------------------------------
var.test(mass ~ passerine, data = birds)


## -----------------------------------------------------------------------------
count(birds, passerine)


## -----------------------------------------------------------------------------
var.test(log10(mass) ~ passerine, data = birds)


## -----------------------------------------------------------------------------
t.test(log10(mass) ~ passerine, data = birds, 
       alternative = "two.sided", var.equal = FALSE)


## -----------------------------------------------------------------------------
## log10(mass) is greater in 0 compared to 1 => passerine less
t.test(log10(mass) ~ passerine, data = birds, 
       alternative = "greater", var.equal = FALSE)


## -----------------------------------------------------------------------------
plot(birds$passerine, birds$mass)


## -----------------------------------------------------------------------------
ggplot(birds, aes(x = forcats::fct_rev(passerine), y = mass)) +
  geom_boxplot(notch = TRUE) +
  scale_x_discrete(labels = c("Passerine families", "Non-passerine families")) +
  labs(x = NULL, y = "Body mass (g)")


## -----------------------------------------------------------------------------
ggplot(birds, aes(x = forcats::fct_rev(passerine), y = log10(mass))) +
  geom_boxplot(notch = TRUE) +
  scale_x_discrete(labels = c("Passerine families", "Non-passerine families")) +
  labs(x = NULL, y = "log10(body mass)")


## -----------------------------------------------------------------------------
wilcox.test(birds$mass[birds$aquatic == 0], birds$mass[birds$aquatic == 1])


## -----------------------------------------------------------------------------
plot(birds$mass, birds$max_abund)


## -----------------------------------------------------------------------------
(g <- 
   ggplot(birds, aes(x = mass, y = max_abund)) +
   geom_point(size = 3, alpha = .3) + 
   labs(x = "Body mass (g)", y = "Maximum abundance"))


## -----------------------------------------------------------------------------
lm_birds <- lm(max_abund ~ mass, data = birds)


## -----------------------------------------------------------------------------
lm_birds


## -----------------------------------------------------------------------------
summary(lm_birds)


## -----------------------------------------------------------------------------
anova(lm_birds)


## -----------------------------------------------------------------------------
plot(lm_birds)


## -----------------------------------------------------------------------------
shapiro.test(residuals(lm_birds))


## -----------------------------------------------------------------------------
# install.packages("lmtest")
lmtest::bptest(lm_birds)


## -----------------------------------------------------------------------------
hist(residuals(lm_birds))


## -----------------------------------------------------------------------------
ggplot(tibble(residuals = residuals(lm_birds)), aes(x = residuals)) + 
  geom_histogram(bins = 20)


## -----------------------------------------------------------------------------
g + stat_smooth(method = "lm")


## -----------------------------------------------------------------------------
g + 
  scale_x_log10(name = "log10(body mass)")


## -----------------------------------------------------------------------------
g + 
  scale_x_log10(name = "log10(body mass)") + 
  scale_y_log10(name = "log10(maximum abundance)")


## -----------------------------------------------------------------------------
g + 
  scale_x_log10(name = "log10(body mass)") + 
  scale_y_log10(name = "log10(maximum abundance)") +
  stat_smooth(method = "lm")


## -----------------------------------------------------------------------------
g + coord_trans(x = "log10", y = "log10")


## -----------------------------------------------------------------------------
g + coord_trans(x = "log10", y = "log10") + stat_smooth(method = "lm")


## -----------------------------------------------------------------------------
birds <- birds %>% 
  mutate(across(c(max_abund, mass), log10, .names = "{col}_log"))


## -----------------------------------------------------------------------------
birds


## -----------------------------------------------------------------------------
# birds <- birds %>%
#   mutate(log_max_abund = log10(max_abund), log_mass = log10(mass))


## -----------------------------------------------------------------------------
# birds$log_max_abund <- log10(birds$max_abund)
# birds$log_mass      <- log10(birds$mass)


## -----------------------------------------------------------------------------
lm_log <- lm(max_abund_log ~ mass_log, data = birds)


## -----------------------------------------------------------------------------
plot(lm_log)


## -----------------------------------------------------------------------------
ggplot(tibble(residuals = residuals(lm_log)), aes(x = residuals)) + 
  geom_histogram(bins = 20)


## -----------------------------------------------------------------------------
shapiro.test(residuals(lm_log))


## -----------------------------------------------------------------------------
summary(lm_log)


## -----------------------------------------------------------------------------
anova(lm_log)


## -----------------------------------------------------------------------------
summary(lm_log)$coefficients


## -----------------------------------------------------------------------------
summary(lm_log)$adj.r.squared


## -----------------------------------------------------------------------------
birds_terrestrial <- filter(birds, aquatic == "0")


## -----------------------------------------------------------------------------
birds_terrestrial


## -----------------------------------------------------------------------------
lm_terr <- lm(max_abund_log ~ mass_log, data = birds_terrestrial)


## -----------------------------------------------------------------------------
# lm_terr <- lm(max_abund_log ~ mass_log, data = birds, 
#               subset = birds$aquatic == "0")


## -----------------------------------------------------------------------------
plot(lm_terr)


## -----------------------------------------------------------------------------
ggplot(tibble(residuals = residuals(lm_terr)), aes(x = residuals)) + 
  geom_histogram(bins = 20)


## -----------------------------------------------------------------------------
shapiro.test(residuals(lm_terr))


## -----------------------------------------------------------------------------
summary(lm_terr)


## -----------------------------------------------------------------------------
anova(lm_terr)


## -----------------------------------------------------------------------------
summary(lm_terr)$coefficients


## -----------------------------------------------------------------------------
summary(lm_terr)$adj.r.squared


## -----------------------------------------------------------------------------
ggplot(birds, aes(x = mass, y = max_abund, 
                  color = forcats::fct_rev(passerine))) +
  stat_smooth(method = "lm") +
  geom_point(size = 3, alpha = .5) + 
  scale_color_manual(values = c("#663399", "#EFAC00"), 
                     labels = c("Passerine families", "Other families")) +
  labs(x = "Body mass (g)", y = "Maximum abundance", color = NULL)


## -----------------------------------------------------------------------------
lm_mult <- lm(max_abund_log ~ mass_log + passerine, data = birds)


## -----------------------------------------------------------------------------
plot(lm_mult)


## -----------------------------------------------------------------------------
summary(lm_mult)


## -----------------------------------------------------------------------------
lm_ttest <- lm(mass_log ~ aquatic, data = birds)


## -----------------------------------------------------------------------------
ttest_lm <- t.test(mass_log ~ aquatic, data = birds, var.equal = TRUE)


## -----------------------------------------------------------------------------
anova(lm_ttest)


## -----------------------------------------------------------------------------
ttest_lm$statistic^2


## -----------------------------------------------------------------------------
aov_diet <- aov(max_abund_log ~ diet, data = birds)


## -----------------------------------------------------------------------------
plot(aov_diet)


## -----------------------------------------------------------------------------
shapiro.test(resid(aov_diet))


## -----------------------------------------------------------------------------
bartlett.test(max_abund_log ~ diet, data = birds)


## -----------------------------------------------------------------------------
summary(aov_diet)


## -----------------------------------------------------------------------------
anova(lm(max_abund_log ~ diet, data = birds))


## -----------------------------------------------------------------------------
TukeyHSD(aov_diet, ordered = TRUE)


## -----------------------------------------------------------------------------
TukeyHSD(aov_diet, ordered = TRUE)$diet[7, , drop = FALSE]


## -----------------------------------------------------------------------------
ggplot(birds, aes(x = mass, y = max_abund, color = diet)) +
  geom_point(size = 3, alpha = .3) + 
  stat_smooth(method = "lm", se = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  labs(x = "Body mass (g)", y = "Maximum abundance", color = "Diet type:")


## -----------------------------------------------------------------------------
birds_diet <- 
  birds %>% 
  add_count(diet) %>% 
  filter(n > 5) %>% 
  mutate(diet = factor(as.character(diet)))


## -----------------------------------------------------------------------------
ggplot(birds_diet, aes(x = diet, y = mass)) +
  geom_boxplot(color = "grey40", fill = "grey80", lwd = .9, outlier.shape = NA) +
  ggforce::geom_sina(maxwidth = .5, alpha = .5, size = 3, color = "#28A87D") +
  labs(x = "Diet type", y = "Body mass (g)")


## -----------------------------------------------------------------------------
# install.packages("lme4")
lm_mixed1 <- lme4::lmer(max_abund_log ~ mass_log + (1 | diet), data = birds_diet)


## -----------------------------------------------------------------------------
coef(lm_mixed1)$diet


## -----------------------------------------------------------------------------
# install.packages("lme4")
lm_mixed2 <- lme4::lmer(max_abund_log ~ mass_log + (0 + mass_log | diet), 
                        data = birds_diet)


## -----------------------------------------------------------------------------
coef(lm_mixed2)$diet


## -----------------------------------------------------------------------------
lm_mixed3 <- lme4::lmer(max_abund_log ~ mass_log + (1 + mass_log | diet), 
                        data = birds_diet)


## -----------------------------------------------------------------------------
coef(lm_mixed3)$diet


## -----------------------------------------------------------------------------
plot(lm_mixed3)


## -----------------------------------------------------------------------------
summary(lm_mixed3)


## -----------------------------------------------------------------------------
anova(lm_mixed3)


## -----------------------------------------------------------------------------
anova(lm_log, lm_mult)


## -----------------------------------------------------------------------------
anova(lm_mixed1, lm_mixed3)
