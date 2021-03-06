# Empirical application for "A Bayesian framework for estimating human capabilities"

# Set up
rm(list = ls())

library(rstan) # for fitting Bayesian model
library(ggplot2) # for visualization
library(dplyr) # for data summaries

options(mc.cores = parallel::detectCores()) # Sets default number of cores
rstan_options(auto_write = TRUE)
options(warn=-1)

# Compile model
compiled.BSFA.model <- stan_model(file = "./bmodel_yop.stan")

# Import data
df <- read.csv("./yop_child_data.csv")

# View data
View(df)

# Prepare data for BSFA
N <- nrow(df)                         #N = number of rows in data
NF <- length(unique(df$partid))       #NF= number of families / sampling units
id <- as.numeric(as.factor(df$partid))#id= unique sampling unit identifier
f <- df$child_edu_index               #f = observed functioning vector

## Factual X matrix
drop <- c("partid", "child_edu_index")
Xf <- df[, !(names(df) %in% drop)]    #Xf= actual covariate matrix
Xf$male_assigned <- Xf$assigned*Xf$male 

View(Xf)

## Counterfactual X matrix
Xc <- Xf                              #Xc= counterfactual covariate matrix
Xc$assigned <- abs(Xc$assigned - 1)   #flip treatment indicator on Xc
Xc$male_assigned <- Xc$assigned*Xc$male
K <- ncol(Xf)                         #number of columns in X matrix

# Compile prepared data
data.list <- list(N = N, 
                  NF = NF, 
                  K = K, 
                  id = id, 
                  f = f, 
                  Xf = Xf, 
                  Xc = Xc)

# Fit BSFA model
sampled.BSFA.model <- sampling(compiled.BSFA.model, 
                          data = data.list,
                          warmup = 2500,
                          iter = 5000, #total number of iterations
                          chains = 4)


sampled.BSFA.model <- readRDS("sampled.BSFA.model.RDS")

# Diagnostics
traceplot(sampled.BSFA.model, pars=c("mu_a", "beta[1]", "beta[5]", "beta[53]","sigma_a2", "sigma_v2", "rho"))
summary(sampled.BSFA.model, pars=c("mu_a", "beta", "sigma_a2", "sigma_v2", "rho"))$summary[,c("mean", "2.5%", "50%", "97.5%", "n_eff", "Rhat")]


# Save results

# extract samples from sampled.BSFA.model
trtid <- which(colnames(Xf)%in% c("assigned", "male_assigned"))
coefficients <- data.frame(extract(sampled.BSFA.model, pars = paste("beta[", trtid, "]", sep = "")))
functioning_factual <- data.frame(extract(sampled.BSFA.model, pars = c("f_pred_f")))
functioning_counterfactual <- data.frame(extract(sampled.BSFA.model, pars = c("f_pred_c")))


# save csv files of samples
write.csv(coefficients, "coefficients.csv")
write.csv(functioning_factual, "functioning_factual.csv") 
write.csv(functioning_counterfactual, "functioning_counterfactual.csv")

# Plot impact estimates
coefficients <- read.csv(file = "coefficients.csv", header = TRUE, sep = ",")

ggplot(data = coefficients) +
  geom_density(aes(x = beta.1.+beta.53.,fill = "grey90"), alpha = I(.4)) +
  geom_density(aes(x = beta.1.,fill = "grey20"),alpha = I(.4)) +
  xlab("Impact Estimate") +
  ylab("Density") +
   scale_fill_manual(name = "",
                       values = c("grey20" = "grey20",
                                "grey90"="grey90"),
                       labels = c("female", "male")) +
  theme_bw()

# posterior means
coefficients %>% 
  mutate(male_treatment_effect = beta.1.+beta.53.,
         female_treatment_effect = beta.1.) %>%
  summarise(male_prob_positive = mean(male_treatment_effect),
            female_prob_positive=mean(female_treatment_effect))

# 90% credible intervals
coefficients %>% 
  mutate(male_treatment_effect = beta.1.+beta.53.,
         female_treatment_effect = beta.1.) %>%
  summarise(male_prob_positive = quantile(male_treatment_effect, c(.05, .95)),
            female_prob_positive=quantile(female_treatment_effect, c(.05, .95))) %>%t()

# What is the probability of a positive treatment effect on capabilities?
coefficients %>% 
  mutate(male_treatment_effect = beta.1.+beta.53.,
         female_treatment_effect = beta.1.) %>%
  summarise(male_prob_positive = mean(male_treatment_effect > 0),
            female_prob_positive=mean(female_treatment_effect > 0))
# 94% posterior probability of a positive effect on educational capabilities for children of female beneficiaries


# Plot all capability sets

# Capability set defined as 95% interval of sampled functionings
# factuals
df$f_pred_f_lb <- apply(functioning_factual, 2, "quantile", c(0.025))
df$f_pred_f_ub <- apply(functioning_factual, 2, "quantile", c(0.975))
# counterfactuals
df$f_pred_c_lb <- apply(functioning_counterfactual, 2, "quantile", c(0.025))
df$f_pred_c_ub <- apply(functioning_counterfactual, 2, "quantile", c(0.975))
# prep data for plot
data.treated <- df[df$assigned == 1,]
data.treated <- data.treated[order(data.treated$f_pred_c_ub),]
data.treated$id <- 1:nrow(data.treated)
data.treated$gender <- factor(data.treated$male, levels = c(0,1), labels = c("female", "male"))

ggplot(data.treated) +
  geom_errorbar(aes(x = id, ymin = f_pred_f_lb, ymax = f_pred_f_ub, colour = "grey40"), width = 0) +
  geom_errorbar(aes(x = id, ymin = f_pred_c_lb, ymax = f_pred_c_ub, colour = "grey60"), width = 0) +
  xlab("Child") +
  ylab("Education Level") +
  facet_wrap(~gender) +
  scale_y_continuous(limits = c(-5, 5))+
  scale_colour_manual(name = "",
                      values = c("grey40"="grey40",
                                 "grey60" = "grey60"),
                      labels = c("treated","untreated")) +
  theme_bw()


