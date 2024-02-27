### Deception Data Analysis - Pilot Test ###
library(readr)
library(tidyverse)
#install.packages("plotly")
library(plotly)
library(broom)
library(skimr)
library(knitr)
#install.packages("rcompanion")
library(rcompanion)
#install.packages('afex')
library(afex)
#install.packages("performance")
library(performance)
#install.packages("effectsize")
library(effectsize)
#install.packages("mice")
library(mice)
#install.packages("see")
library(see)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("ltm")
library(ltm)
library(lmtest)
library(sandwich)
library(apaTables)
??run_csv

## Import Dataset ##
data_complete <- read_csv("C:/Users/13057/Desktop/GMU PhD/Classes/Fourth Year/Experiments/Deception/Deception Survey November 1st Results/Hidden State POV Analysis/Quantitative Analysis with Hidden 3rd Person POV - Full (n = 498).csv")
View(data_complete)
summary(data_complete)
skim(data_complete)


data_complete %>% 
  group_by(gender) %>%
  summarize(n=n()) %>%
  mutate(percent = (n/sum(n)) * 100) %>%
  arrange(desc(n))

data_complete %>% 
  group_by(degree) %>%
  summarize(n=n()) %>%
  mutate(percent = (n/sum(n)) * 100) %>%
  arrange(desc(n))

data_complete %>% 
  group_by(english_first_lang) %>%
  summarize(n=n()) %>%
  mutate(percent = (n/sum(n)) * 100) %>%
  arrange(desc(n))


glimpse(data_complete)
str(data_complete)
sum(is.na(data_complete))
which(is.na(data_complete$approval), arr.ind=TRUE)

### Creation of datasets for each condition ###
External <- data_complete %>% filter(deception_type == "External")
view(External)

Hidden <- data_complete %>% filter(deception_type == "Hidden")
view(Hidden)
Superficial <- data_complete %>% filter(deception_type == "Superficial")
view(Superficial)

### Obtain the descriptive statistics for Quant Variables - Full Dataset ###
skim(data_complete)
summary(data_complete)

### Obtain the descriptive statistics for Quant Variables - Each Condition ###

skim(External)
summary(External)

skim(Hidden)
summary(Hidden)

skim(Superficial)
summary(Superficial)

### Analysis 1 - Chi Square Test on Categorical Deception measure ###
rq1 <- data_complete %>% dplyr::select(deception_type,deceptive_cat) %>% group_by(deception_type,deceptive_cat)%>%
  summarize(n=n())%>%
  kable()

rq1_table <- matrix(c(38,32,38), byrow = T, nrow = 3)

rownames(rq1_table) <- c("External","Hidden","Superficial")
colnames(rq1_table) <- c("Not Sure")

rq1_table

model <- tidy(chisq.test(rq1_table,correct = FALSE))

pairwiseNominalIndependence(rq1_table,
                            fisher = FALSE,
                            gtest  = FALSE,
                            chisq  = TRUE,
                            cramer = TRUE,
                            stats = TRUE,
                            method = "fdr")


### One-Way Between Subjects ANOVA on Approval across experimental conditions
rq3 <- data_complete
skim(rq3)

### ANOVA for Approval Ratings ###
rq3.aov <- aov(approval ~ deception_type, data = rq3)
rq3.aov.table <- apa.aov.table(rq3.aov)
print(rq3.aov.table)
apa.save(filename = "Deception Ratings ANOVA.doc",rq3.aov.table)
file.exists("Deception Ratings ANOVA.doc")
apa.knit.table.for.pdf(rq3.aov.table)
# Checking Assumptions of ANOVA for Analysis
plot(rq3.aov)
check_heteroscedasticity(rq3.aov) # Heteroscedasticity - NOT MET
check_outliers(rq3.aov) # Outliers - MET
check_normality(cor1) # Normality - NOT MET, may not be important because of the large sample size (N = 505)
### Output of anova model ###
summary(rq3.aov)
tukey.rq3.aov <- tidy(TukeyHSD(rq3.aov))

#### Applying Robust Standard Errors to Analysis due to violation in Homoscedasticity ####
library(ez)
rq3.aov.hc3 <- ezANOVA(data = rq3,
                       wid = .(participant),
                       dv = .(approval),
                       between = .(deception_type),
                       type = 2,
                       white.adjust = T,
                       detailed = T,
                       return_aov = T)

pairwise.t.test(rq3$approval, rq3$deception_type, paired = F, p.adjust.method = 'bonferroni')

### Calculating Summary Statistics for ANOVA and adding clds for pairwise comparisons ###
library(emmeans)
library(multcomp)
emmeans.rq3.aov <- emmeans(rq3.aov, specs = "deception_type")
emmeans.rq3.aov

#### Adding CLDS for multiple comparsions  - Method 1 ####
emmeans.rq3.aov.cld <- cld(emmeans.rq3.aov, Letters = letters)
emmeans.rq3.aov.cld$fill <- ifelse(emmeans.rq3.aov.cld$emmean < 0, "#00BFC4","#F8766D")


library("ggpubr")

aov_test <- tibble::tribble(
  ~group1, ~group2,   ~p.adj.signif,
  "Hidden", "External", "***",
  "Superficial","External", "***",
  "Superficial","Hidden", "***"
)

aov_plot <- ggplot(emmeans.rq3.aov.cld, aes(deception_type, emmean, fill = emmeans.rq3.aov.cld$fill <- ifelse(emmeans.rq3.aov.cld$emmean < 0, "#00BFC4","#F8766D"))) + 
  geom_col(alpha = 0.5, show.legend = FALSE, color = "black") + 
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.1, linewidth = 0.7) +
  theme_classic() +
  xlab("Deception Type") + ylab("Approval Rating") + ggtitle("Approval Rating by Deception Type") +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_pvalue_manual(
    aov_test, 
    y.position = 35, step.increase = 0.1,
    label = "p.adj.signif"
  )
  


#### Adding CLDS for multiple comparsions  - Method 2 ####
library(multcompView)
cld <- multcompLetters4(rq3.aov, tukey.rq3.aov)
print(cld)

#### Welch ANOVA -> Check it out and triple check finding ###
bartlett.test(approval ~ deception_type, data = rq3)
welch.aov <- oneway.test(approval ~ deception_type, data = rq3, var.equal = FALSE)
TukeyHSD(welch.aov)
#install.packages("ez")
library(ez)

### ANOVA for Continuous Deception Ratings ###
rq3.decep.cont <- aov(deceptive_con ~ deception_type, data = rq3)
summary(rq3.decep.cont)
tukey.rq3.decep.cont <- tidy(TukeyHSD(rq3.decep.cont))
rq3.decep.cont.table <- apa.aov.table(rq3.decep.cont, filename = "Deception Ratings ANOVA Continuous.doc")
apa.save(filename = "Deception Ratings ANOVA Continuous.doc",rq3.decep.cont.table)
file.exists("Deception Ratings ANOVA Continuous.doc")
print(rq3.decep.cont.table)
apa.knit.table.for.pdf(rq3.decep.cont.table)



### Calculating Summary Statistics for ANOVA and adding clds for pairwise comparisons ###
emmeans.rq3.decep.cont <- emmeans(rq3.decep.cont, specs = "deception_type")
emmeans.rq3.decep.cont

#### Adding CLDS for multiple comparsions  - Method 1 ####
emmeans.rq3.decep.cont.cld <- cld(emmeans.rq3.decep.cont, Letters = letters)

aov_test_2 <- tibble::tribble(
  ~group1, ~group2,   ~p.adj.signif,
  "Hidden", "External", "***",
  "Superficial","External", "",
  "Superficial","Hidden", "***"
)

aov_plot_decep_cont <- ggplot(emmeans.rq3.decep.cont.cld, aes(deception_type, emmean)) + 
  geom_col(alpha = 0.5, show.legend = FALSE, color = "black", fill = "#00BFC4") + 
  geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE), width = 0.1, linewidth = 0.7) +
  theme_classic() +
  xlab("Deception Type") + ylab("Deception Rating") + ggtitle("Deceptiveness Rating by Deception Type") +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_pvalue_manual(
    aov_test_2, 
    y.position = 85, step.increase = 0.2,
    label = "p.adj.signif"
  )


#### Applying Robust Standard Errors to Analysis due to violation in Homoscedasticity ####
rq3.decep.cont.hc3 <- ezANOVA(data = rq3,
                              wid = .(participant),
                              dv = .(deceptive_con),
                              between = .(deception_type),
                              type = 2,
                              white.adjust = T,
                              detailed = T,
                              return_aov = T)
print(rq3.aov.hc3)
print(rq3.decep.cont.hc3)



### Multiple Regression with Demographic Information
rq4 <- data_complete
glimpse(rq4)

# Model 2: Experience followed by Knowledge - USE THIS MODEL FOR FINAL ANALYSIS #
model_lm_2 <- lm(deceptive_con ~ age + robot_exp + robot_knowledge , data = rq4)
r2(model_lm_2)
print(model_lm_2)
tidy(model_lm_2)
# Checking assumptions of Regression for model_lm_2 #
check_collinearity(model_lm_2) # Low Correlation
check_heteroscedasticity(model_lm_2) # Homoscedasticity -> NOT MET
check_normality(model_lm_2) # Normality -> NOT MET
check_outliers(model_lm_2) # Outliers -> None Detected
check_model(model_lm_2)

#### Applying Robust Standard Errors to Analysis due to violation in Homoscedasticity ####
model_lm2_hc3 <- coeftest(model_lm_2, vcov. = vcovHC(model_lm_2, type = "HC3"))
print(model_lm2_hc3)
tidy(model_lm2_hc3)

### Regression Model - External State Deception ###
model_lm_external <- lm(deceptive_con ~ age + robot_exp + robot_knowledge , data = External)
r2(model_lm_external)
tidy(model_lm_external)

apa.external.table <- apa.reg.table(model_lm_external)
apa.save("External State Deception Regression.doc",apa.external.table)
apa.knit.table.for.pdf(apa.external.table)

e_model <- aov(deceptive_con ~ age + robot_exp + robot_knowledge , data = External)
apa.aov.table(e_model)

#### HC3 Correction ####
model_e_hc3 <- coeftest(model_lm_external, vcov. = vcovHC(model_lm_external, type = "HC3"))
print(model_e_hc3)

### Regression Model - Hidden State Deception ###
model_lm_hidden <- lm(deceptive_con ~ age + robot_exp + robot_knowledge , data = Hidden)
r2(model_lm_hidden)
tidy(model_lm_hidden)

apa.hidden.table <- apa.reg.table(model_lm_hidden)

h_model <- aov(deceptive_con ~ age + robot_exp + robot_knowledge , data = Hidden)
apa.aov.table(h_model)

apa.hidden.table <- apa.reg.table(model_lm_external)
#### HC3 Correction ####
model_h_hc3 <- coeftest(model_lm_hidden, vcov. = vcovHC(model_lm_hidden, type = "HC3"))
print(model_h_hc3)

### Regression Model - Superficial State Deception ###
model_lm_sup <- lm(deceptive_con ~ age + robot_exp + robot_knowledge , data = Superficial)
r2(model_lm_sup)
tidy(model_lm_sup)

apa.sup.table <- apa.reg.table(model_lm_sup)

s_model <- aov(deceptive_con ~ age + robot_exp + robot_knowledge , data = Superficial)
apa.aov.table(s_model)

#### HC3 Correction ####
model_s_hc3 <- coeftest(model_lm_sup, vcov. = vcovHC(model_lm_sup, type = "HC3"))
print(model_s_hc3)
