# ICU Outcome regression analysis


# Load Packages --------------------------------------------------------

require(HH)
require(tidyverse)
require(vcd)

# Load Data ------------------------------------------------------------

data("icu")

## Subset to Variables of Interest
icu <- icu %>% select(STA, AGE, SEX, SER, CPR, HRA)

## Fix Service Factor
icu <- icu %>% mutate(SER = factor(SER, labels = c("Medical", "Surgical")))

## Standard summary for examination
summary(icu)

# Part 1: Visualize Data -------------------------------------------------------

icu %>% ggplot(aes(x = AGE, y = HRA, color = STA)) +
  geom_jitter(size = 2, alpha = 0.8) +
  facet_grid(SER ~ SEX + CPR, 
             labeller = labeller(
               CPR = c("No" = "CPR\u2013No", "Yes" = "CPR\u2013Yes"))
  ) +
  scale_color_manual(values = c("Lived" = "#27647B", "Died" = "#CA3542"),
                     guide = guide_legend(title = "Status")) +
  labs(x = "Age (Years)", y = "Heart Rate (bpm)")

## Alternate Colors: Colorblind Safe

icu %>% ggplot(aes(x = AGE, y = HRA, color = STA)) +
  geom_jitter(size = 1.5, alpha = 0.8) +
  facet_grid(SER ~ SEX + CPR, 
             labeller = labeller(
               CPR = c("No" = "CPR\u2013No", "Yes" = "CPR\u2013Yes"))
  ) +
  scale_color_manual(values = c("Lived" = "#ffde40", "Died" = "#200772"),
                     guide = guide_legend(title = "Status")) +
  labs(x = "Age (Years)", y = "Heart Rate (bpm)")

## Mosaic plot to support first impressions
mosaicplot(table(icu$SER,icu$STA),color= c("#ffde40", "#200772"),main = "Survival Status by First ICU Service Type ")

"Lived" = "#ffde40", "Died" = "#200772"

# Part 2: Modeling -------------------------------------------------------------

## Full Model 
icu_model_1 <- glm(STA ~  AGE + SEX + SER + CPR + HRA, data = icu, 
                   family = binomial)        

summary(icu_model_1)  

## Drop Sex: Least Significant Predictor
icu_model_2 <- glm(STA ~  AGE + SER + CPR + HRA, data = icu, 
                   family = binomial)        

summary(icu_model_2) 

## Drop Heart Rate: Least Significant Predictor
icu_model_3 <- glm(STA ~  AGE + SER + CPR, data = icu, family = binomial)        

summary(icu_model_3) 

# Final Model Details -------------------------------------------------

anova(icu_model_3, test = "Chi")  

coef(summary(icu_model_3)) 

# Part 2: Predictions ----------------------------------------------------------

## predicted probabilities, logit and odds for original dataset
p_hat <- predict.glm(icu_model_3, type="response") 
logit_p_hat <- logit(p_hat)
odds_hat <- p_hat/(1 - p_hat)

# Part 2: Vizualizations  ------------------------------------------------------

icu_pred <- tibble(AGE = rep(16:92, 4), 
                   SER = factor(c(rep("Medical", 154), rep("Surgical", 154))),
                   CPR = factor(rep(c(rep("No", 77), rep("Yes", 77)), 2))) 

icu_pred$STA_HAT <- predict.glm(icu_model_3, type = "response", 
                                newdata = icu_pred)

icu_pred %>% ggplot(aes(x = AGE, y = STA_HAT, color = CPR)) +
  geom_line(lwd = 1.2) +
  facet_grid(SER ~ CPR, 
             labeller = labeller(CPR = c("No" = "CPR\u2013No", "Yes" = "CPR\u2013Yes"))) +
  ylim(0, 1) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("#27647B", "#CA3542")) +
  labs(x = "Age (Years)", y = "Predicted Probability of Status\u2013Dead") +
  geom_hline(mapping = aes(yintercept = 0.5), linetype = 2, color = "#e084a3") +
  theme(legend.position = "none")

## Alternate Colors: Colorblind Safe

icu_pred %>% ggplot(aes(x = AGE, y = STA_HAT, color = CPR)) +
  geom_line(lwd = 1.2) +
  facet_grid(SER ~ CPR, 
             labeller = labeller(CPR = c("No" = "CPR\u2013No", "Yes" = "CPR\u2013Yes"))) +
  ylim(0, 1) +
  scale_y_continuous(labels = scales::percent) +
  scale_color_manual(values = c("#ffde40",  "#200772")) +
  labs(x = "Age (Years)", y = "Predicted Probability of Status\u2013Dead") +
  geom_hline(mapping = aes(yintercept = 0.5), linetype = 2, color = "#e084a3") +
  theme(legend.position = "none")


# Part 3: Predictions ----------------------------------------------------------

new_obs <- tbl_df(read.table(header = TRUE, text = "
                             AGE SEX SER CPR HRA
                             91 1 0 1 89
                             91 1 0 0 79
                             51 1 1 0 98
                             96 0 0 0 79
                             89 1 0 0 80")) %>% 
  as.tibble() %>% mutate(SEX = factor(SEX, labels = c("Female", "Male")),
                         SER = factor(SER, labels = c("Medical", "Surgical")),
                         CPR = factor(CPR, labels = c("No", "Yes"))) 

predict.glm(icu_model_3, type = "response", newdata = new_obs) %>% as.tibble() %>% 
  mutate(STA = if_else(value > 0.5, "Died", "Lived"))
