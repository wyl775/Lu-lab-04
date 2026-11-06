install.packages(c("glmmTMB", "broom.mixed", "DHARMa", "ggeffects", "titanic"))

# Part 1 Project Configuration ####

library(glmmTMB)      
library(dplyr)        
library(tidyr)       
library(ggplot2)      
library(broom.mixed) 
library(performance)  
library(gt)           
library(readr)       
library(DHARMa)       
library(ggeffects)    
library(titanic)      

titanic <- titanic_train                     

titanic <- titanic |>                        
  select(Survived, Sex, Age, Pclass) |>      
  drop_na() |>                              
  mutate(
    Sex.Binary = case_when(                  
      Sex == "male" ~ 0,                     
      Sex == "female" ~ 1)) |>               
  mutate(
    Survived.Char = case_when(               
      Survived == 0 ~ "Died",
      Survived == 1 ~ "Survived")) |>
  mutate(
    Pclass.Char = case_when(                 
      Pclass == "1" ~ "First Class",
      Pclass == "2" ~ "Second Class",
      Pclass == "3" ~ "Third Class")) |>
  mutate(Pclass = factor(Pclass, levels = c(1, 2, 3))) |>  
  select(Survived, Survived.Char, Sex, Sex.Binary, Pclass, Pclass.Char, Age)  

n <- nrow(titanic)                          
n                                            

# B. Console exploration (1.4) 

head(titanic, 10)                 
glimpse(titanic)                  

summary(titanic$Age)              
sd(titanic$Age)                   

table(titanic$Survived.Char)     
table(titanic$Sex)                
table(titanic$Pclass.Char)       

prop.table(table(titanic$Sex))    

table(titanic$Sex, titanic$Survived.Char)          
prop.table(table(titanic$Sex, titanic$Survived.Char),
           margin = 1)                             

table(titanic$Pclass.Char, titanic$Survived.Char)  
prop.table(table(titanic$Pclass.Char, titanic$Survived.Char),
           margin = 1)                             

table(titanic$Sex, titanic$Pclass.Char)           

ggplot(titanic, aes(x = Age, fill = Survived.Char)) +  
  geom_histogram(binwidth = 5, position = "dodge")      

# C. Report tables (1.4) 

# Table 1: descriptive statistics for Age
age_desc <- titanic |>                 
  summarise(                           
    Variable = "Age (years)",          
    Mean = mean(Age),                  
    SD   = sd(Age),                   
    Min  = min(Age),                  
    Max  = max(Age)                    
  )

age_desc |>
  gt() |>                                                  
  tab_header(title = "Descriptive Statistics for Age") |>  
  fmt_number(columns = c(Mean, SD, Min, Max), decimals = 2)  

# Table 2: cell counts for the categorical variables
cell_counts <- titanic |>
  select(Survived = Survived.Char,               
         Sex,                                   
         `Ticket class` = Pclass.Char) |>
  pivot_longer(everything(),                     
               names_to  = "Variable",           
               values_to = "Category") |>       
  count(Variable, Category) |>                  
  group_by(Variable) |>                         
  mutate(Percent = n / sum(n) * 100) |>          
  ungroup()                                      

cell_counts |>
  gt(groupname_col = "Variable") |>             
  tab_header(title = "Cell Counts for Categorical Variables") |>
  cols_label(n = "n", Percent = "%") |>          
  fmt_number(columns = Percent, decimals = 1)    

# Table 3: survival rates by sex and by ticket class
surv_sex <- titanic |>
  group_by(Group = Sex) |>                       
  summarise(n = n(),                             
            `n survived` = sum(Survived),        
            `Survival rate (%)` = mean(Survived) * 100) |>  
  mutate(Variable = "Sex")                       

surv_class <- titanic |>
  group_by(Group = Pclass.Char) |>              
  summarise(n = n(),
            `n survived` = sum(Survived),
            `Survival rate (%)` = mean(Survived) * 100) |>
  mutate(Variable = "Ticket class")

bind_rows(surv_sex, surv_class) |>               
  gt(groupname_col = "Variable") |>
  tab_header(title = "Survival by Sex and Ticket Class") |>
  fmt_number(columns = `Survival rate (%)`, decimals = 1)

# Part 2 Models ####

mod0 <- glmmTMB(Survived ~ 1, data = titanic,       
                family = binomial(link = "logit"))                

mod1 <- glmmTMB(
  Survived ~ Sex,                                   
  data = titanic,                                   
  family = binomial(link = "logit"))

mod2 <- glmmTMB(
  Survived ~ Sex + Pclass,                         
  data = titanic,
  family = binomial(link = "logit"))

mod3 <- glmmTMB(
  Survived ~ Sex + Pclass + Age,                    
  data = titanic,
  family = binomial(link = "logit"))


summary(mod0)    # intercept = log-odds of survival for all passengers
summary(mod1)    # intercept = females (reference group); Sexmale = male vs female
summary(mod2)    # intercept = females in first class
summary(mod3)    # intercept = females in first class aged 0

confint(mod3)    # 95% confidence intervals for mod3's coefficients

# Part 3 Compare model performance ####
# 3.1 Nested-model chi-square tests 
tests <- anova(mod0, mod1, mod2, mod3)   

tests                                   

tests <- tests |>
  select(Chisq, `Chi Df`, `Pr(>Chisq)`) |>      
  rename(
    Nested.df = `Chi Df`,                       
    Nested.Chisq.stat = Chisq,                 
    Nested.p.value = `Pr(>Chisq)`)             

# 3.2 Extract model-level performance 
glance0 <- broom.mixed::glance(mod0)   
glance1 <- broom.mixed::glance(mod1)
glance2 <- broom.mixed::glance(mod2)
glance3 <- broom.mixed::glance(mod3)

#  combine model metrics
mods <- c(                             
  "mod0: Null",
  "mod1: Sex",
  "mod2: + Pclass",
  "mod3: + Age")

glances <- rbind(                      
  glance0,
  glance1,
  glance2,
  glance3)

# 3.3 Combine nested tests and model performance 
comparison_table <- cbind(             
  Model = mods,                       
  glances,                             
  tests)                              

# model comparison table
comparison_table |>
  select(                              
    Model,
    AIC,
    BIC,
    logLik,
    deviance,
    df.residual,
    Nested.df,
    Nested.Chisq.stat,
    Nested.p.value
  ) |>
  gt() |>                              
  cols_label(                         
    Model = "Model",
    AIC = "AIC",
    BIC = "BIC",
    logLik = "Log-likelihood",
    deviance = "Deviance",
    df.residual = "Residual df",
    Nested.df = "Nested df",
    Nested.Chisq.stat = "Nested χ²",
    Nested.p.value = "Nested p"
  ) |>
  fmt_number(                          
    columns = c(
      AIC,
      BIC,
      logLik,
      deviance,
      Nested.Chisq.stat
    ),
    decimals = 3
  ) |>
  fmt(                                 
    columns = Nested.p.value,
    fns = function(x) {
      ifelse(
        is.na(x),                      
        "—",
        ifelse(x < .001, "< .001", sprintf("%.3f", x))  
      )
    }
  ) |>
  sub_missing(                         
    columns = c(Nested.df, Nested.Chisq.stat),
    missing_text = "—"
  )

# Additional performance comparison 
performance_comparison <- compare_performance( 
  mod0,
  mod1,
  mod2,
  mod3,
  rank = TRUE)                                  

# Part 4 Evaluate binary logistic regression diagnostics ####
# chunk: select model
selected_row <- comparison_table |>
  filter(Nested.p.value < .05) |>       
  slice_tail(n = 1)                     

#creating an object with our selected model's name for dynamic reporting
selected_name <- selected_row$Model     

#case matching for dynamic reporting
selected_model <- switch(               
  selected_name,
  "mod0: Null" = mod0,
  "mod1: Sex" = mod1,
  "mod2: + Pclass" = mod2,
  "mod3: + Age" = mod3)

round(AIC(selected_model), 3)           
round(BIC(selected_model), 3)           

# 4.1a dharma sim
set.seed(9041)                                    
res_sel <- simulateResiduals(selected_model,       
                             n = 500,              
                             plot = FALSE)         

#  dharma plot
plot(res_sel)    # left: Q-Q plot (uniformity); right: residuals vs predicted

# 4.1b vif, vif plot
check_collinearity(selected_model)               
vif_plot <- plot(check_collinearity(selected_model))  
vif_plot                                        

# 4.1c Complete separation

summary(selected_model)                          
table(titanic$Sex, titanic$Survived.Char)        
table(titanic$Pclass.Char, titanic$Survived.Char)
range(predict(selected_model, type = "response"))  

# 4.1d Influential observations outliers)
testOutliers(res_sel)      

# 4.1e Events per predictor 
n_event <- sum(titanic$Survived == 1)               
n_nonevent <- sum(titanic$Survived == 0)            
n_coef <- length(fixef(selected_model)$cond)     

n_event; n_nonevent; n_coef                         
n_event / n_coef                                    

# Part 5 selecting a model ####

# 5.2 Extract coefficients with tidy() 
coefficient_results <- broom.mixed::tidy(
  selected_model,
  effects = "fixed",         
  component = "cond",        
  conf.int = TRUE,            
  conf.level = 0.95) |>       
  mutate(
    odds.ratio = exp(estimate),   
    or.low     = exp(conf.low),   
    or.high    = exp(conf.high))  

coefficient_results           

# Coefficient table
coefficient_results |>
  select(
    term,
    estimate,
    std.error,
    conf.low,
    conf.high,
    p.value
  ) |>
  gt() |>
  cols_label(
    term = "Predictor",
    estimate = "Estimate",
    std.error = "SE",
    conf.low = "95% CI lower",
    conf.high = "95% CI upper",
    p.value = "p"
  ) |>
  fmt_number(
    columns = c(
      estimate,
      std.error,
      conf.low,
      conf.high
    ),
    decimals = 3
  ) |>
  fmt(
    columns = p.value,
    fns = function(x) {
      ifelse(
        x < .001,
        "< .001",
        sprintf("%.3f", x)
      )
    }
  )

# Log-odds coefficients
b_male <- coefficient_results |> filter(term == "Sexmale") |> pull(estimate)  
b_p2   <- coefficient_results |> filter(term == "Pclass2") |> pull(estimate)  
b_p3   <- coefficient_results |> filter(term == "Pclass3") |> pull(estimate)  
b_age  <- coefficient_results |> filter(term == "Age")     |> pull(estimate)  

# Odds ratios and their 95% CIs
or_male  <- coefficient_results |> filter(term == "Sexmale") |> pull(odds.ratio)
or_male_l <- coefficient_results |> filter(term == "Sexmale") |> pull(or.low)
or_male_h <- coefficient_results |> filter(term == "Sexmale") |> pull(or.high)

or_p2   <- coefficient_results |> filter(term == "Pclass2") |> pull(odds.ratio)
or_p2_l <- coefficient_results |> filter(term == "Pclass2") |> pull(or.low)
or_p2_h <- coefficient_results |> filter(term == "Pclass2") |> pull(or.high)

or_p3   <- coefficient_results |> filter(term == "Pclass3") |> pull(odds.ratio)
or_p3_l <- coefficient_results |> filter(term == "Pclass3") |> pull(or.low)
or_p3_h <- coefficient_results |> filter(term == "Pclass3") |> pull(or.high)

or_age   <- coefficient_results |> filter(term == "Age") |> pull(odds.ratio)
or_age_l <- coefficient_results |> filter(term == "Age") |> pull(or.low)
or_age_h <- coefficient_results |> filter(term == "Age") |> pull(or.high)


fixef(selected_model)                                # coefficients on the log-odds scale
exp(fixef(selected_model)$cond)                      # coefficients as odds ratios
exp(confint(selected_model, parm = "beta_"))         # odds-ratio CIs (should match or.low / or.high)

# 5.4 selected model performance
selected_row |>
  select(
    Model,
    AIC,
    BIC,
    logLik,
    deviance,
    df.residual,
    Nested.df,
    Nested.Chisq.stat,
    Nested.p.value
  ) |>
  gt() |>
  cols_label(
    Model = "Model",
    AIC = "AIC",
    BIC = "BIC",
    logLik = "Log-likelihood",
    deviance = "Deviance",
    df.residual = "Residual df",
    Nested.df = "Nested df",
    Nested.Chisq.stat = "Nested χ²",
    Nested.p.value = "Nested p"
  ) |>
  fmt_number(
    columns = where(is.numeric),
    decimals = 2
  ) |>
  fmt(                                   
    columns = Nested.p.value,
    fns = function(x) ifelse(x < .001, "< .001", sprintf("%.3f", x))
  )

# 5.6 Visualize one predictor with ggpredict() 
pred_age <- ggpredict(selected_model, terms = "Age [0:80 by=1]") 
# other predictors held at reference/typical values
ggplot(pred_age, aes(x, predicted)) +                         
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.18) +  #
  geom_line(linewidth = 1.2) +                                
  labs(
    x = "Age (years)",
    y = "Predicted Probability of Survival"
  ) +
  theme_classic()                                             

# Part 6: Discuss your findings and limitations ####
