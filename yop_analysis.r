# Empirical application for "A Bayesian framework for estimating human capabilities"

# Set up
rm(list = ls())
library(rstan)
library(ggplot2)
library(gridExtra)
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
sampled.model <- sampling(compiled.BSFA.model, 
                          data = data.list,
                          warmup = 2500,
                          iter = 5000)



# Diagnostics
traceplot(sampled.model, pars=c("mu_a", "beta", "gamma1", "gamma2", "sigma_a2", "sigma_v2", "rho"))
summary(sampled.model, pars=c("mu_a", "beta", "gamma1", "gamma2", "sigma_a2", "sigma_v2", "rho"),cache=FALSE)$summary


# Save results

# extract samples from sampled.model
trtid <- which(colnames(Xf)%in% c("assigned", "male", "male_assigned"))
coefficients <- data.frame(extract(sampled.model, pars = paste("beta[", trtid, "]", sep = "")))
functioning_factual <- data.frame(extract(sampled.model, pars = c("f_pred_f")))
functioning_counterfactual <- data.frame(extract(sampled.model, pars = c("f_pred_c")))

# compute mean potentials
potentials <- data.frame(get_posterior_mean(sampled.model, pars = c("c_f"))[, 5])

# save csv files of samples
write.csv(coefficients, "coefficients.csv")
write.csv(potentials, "potentials.csv")
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
                       values = c("grey90" = "grey90",
                                "grey20"="grey20"),
                       labels = c("female", "male")) +
  theme_bw()

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
  geom_errorbar(aes(x = id, ymin = f_pred_f_lb, ymax = f_pred_f_ub, colour = "grey60"), width = 0) +
  geom_errorbar(aes(x = id, ymin = f_pred_c_lb, ymax = f_pred_c_ub, colour = "grey40"), width = 0) +
  #geom_line(aes(x = id, y = f_pred_c_ub)) +
  #geom_line(aes(x = id, y = f_pred_c_lb)) +
  xlab("Child") +
  ylab("Education Level") +
  facet_wrap(~gender) +
  scale_y_continuous(limits = c(-5, 5))+
  scale_colour_manual(name = "",
                      values = c("grey60" = "grey60",
                                 "grey40"="grey40"),
                      labels = c("treated", "untreated")) +
  theme_bw()


