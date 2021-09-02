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
compiled.model <- stan_model(file = "./bmodel_yop.stan")

# Import data
df <- read.csv("./yop_child_data.csv")


# Prepare data for BSFA
N <- nrow(df)                         #N = number of rows in data
NF <- length(unique(df$partid))       #NF = number of families / sampling units
id <- as.numeric(as.factor(df$partid))#id= unique samping unit identifier
f <- df$child_edu_index               #f = observed functioning vector

drop <- c("partid", "child_edu_index")
Xf <- df[, !(names(df) %in% drop)]    #Xf= actual covariate matrix
Xf$male_assigned <- Xf$assigned*Xf$male 

Xc <- Xf                              #Xc = counterfactual covariate matrix
Xc$assigned <- abs(Xc$assigned - 1)   #flip treatment indicator on Xc
Xc$male_assigned <- Xc$assigned*Xc$male
K <- ncol(Xf)                         #number of colums in X matrix

# Compile prepared data
data.list <- list(N = N, 
                  NF = NF, 
                  K = K, 
                  id = id, 
                  f = f, 
                  Xf = Xf, 
                  Xc = Xc)

# Fit BSFA model
sampled.model <- sampling(compiled.model, 
                          data = data.list,
                          warmup = 2500,
                          iter = 5000)

# Diagnostics
traceplot(sampled.model, pars=c("mu_a", "beta", "gamma1", "gamma2", "sigma_a2", "sigma_v2", "rho"))
summary(sampled.model, pars=c("mu_a", "beta", "gamma1", "gamma2", "sigma_a2", "sigma_v2", "rho"))$summary
check_divergences(sampled.model)

# Save results
trtid <- which(colnames(Xf)%in% c("assigned", "male", "male_assigned"))
coefficients <- data.frame(extract(sampled.model, pars = paste("beta[", trtid, "]", sep = "")))
potentials <- data.frame(get_posterior_mean(sampled.model, pars = c("c_f"))[, 5])
functioning_factual <- data.frame(extract(sampled.model, pars = c("f_pred_f")))
functioning_counterfactual <- data.frame(extract(sampled.model, pars = c("f_pred_c")))
write.csv(data, "data_sample.csv")
write.csv(coefficients, "coefficients.csv")
write.csv(potentials, "potentials.csv")
write.csv(functioning_factual, "functioning_factual.csv")
write.csv(functioning_counterfactual, "functioning_counterfactual.csv")

# Plot impact estimates
coefficients <- read.csv(file = "coefficients.csv", header = TRUE, sep = ",")
 ggplot(data = coefficients) +
  stat_density(aes(x = beta.1.+beta.53.), colour = "grey60", geom = "line") +
  stat_density(aes(x = beta.1.), colour = "blue", geom = "line") +
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Impact Estimate") +
  ylab("Density") #+
   #scale_colour_manual(name = "",
   #                    values = c("grey60" = "grey60",
    #                              "grey30"="grey30"),
     #                  labels = c("females", "males"))

# Plot potentials
data <- read.csv(file = "data_sample.csv", header = TRUE, sep = ",")
data$potentials <- read.csv(file = "potentials.csv", header = TRUE, sep = ",")[, 2]
data.treated <- data[data$assigned == 1,]
data.control <- data[data$assigned == 0,]
potentials <- ggplot() + 
  stat_ecdf(data = data.control, aes(x = potentials, colour = "c1"), geom = "line") +
  stat_ecdf(data = data.treated, aes(x = potentials, colour = "c2"), geom = "line") +
  ggtitle("(b)") + theme(plot.title = element_text(hjust = 0.5),  
  legend.position = c(0.2, 0.8), legend.title = element_blank()) +
  scale_color_manual(values = c("c1" = "grey", "c2" = "black"), 
  labels = c("Control", "Treated")) +
  xlab("Potential") +
  ylab("Cumulative Probability")

# Combine
plot <- grid.arrange(coeffs, potentials, nrow = 1)
ggsave("yop1.pdf", plot, width = 8, height = 4, units = c("in"))

# Plot capability set for individual
f_pred_f <- read.csv(file = "functioning_factual.csv", header = TRUE, sep = ",")[, -1]
f_pred_c <- read.csv(file = "functioning_counterfactual.csv", header = TRUE, sep = ",")[, -1]
sets1 <- ggplot() + 
  stat_ecdf(data = f_pred_f, aes(x = f_pred_c[, 1], colour = "c1"), geom = "line") +
  stat_ecdf(data = f_pred_f, aes(x = f_pred_f[, 1], colour = "c2"), geom = "line") +
  ggtitle("(a)") + theme(plot.title = element_text(hjust = 0.5),  
  legend.position = c(0.2, 0.8), legend.title = element_blank()) +
  scale_color_manual(values = c("c1" = "grey", "c2" = "black"), 
  labels = c("Control", "Treated")) +
  xlab("Education Level") +
  ylab("Cumulative Probability")

# Plot all capability sets
data$f_pred_f_lb <- apply(f_pred_f, 2, "quantile", c(0.025))
data$f_pred_f_ub <- apply(f_pred_f, 2, "quantile", c(0.975))
data$f_pred_c_lb <- apply(f_pred_c, 2, "quantile", c(0.025))
data$f_pred_c_ub <- apply(f_pred_c, 2, "quantile", c(0.975))
data.treated <- data[data$assigned == 1,]
data.treated <- data.treated[order(data.treated$f_pred_c_ub),]
data.treated$id <- 1:nrow(data.treated)
sets2 <- ggplot(data.treated) +
  geom_errorbar(aes(x = id, ymin = f_pred_f_lb, ymax = f_pred_f_ub), width = 0, color = "gray") +
  geom_line(aes(x = id, y = f_pred_c_ub)) +
  geom_line(aes(x = id, y = f_pred_c_lb)) +
  ggtitle("(b)") + theme(plot.title = element_text(hjust = 0.5)) +
  xlab("Child") +
  ylab("Education Level")

# Combine
plot <- grid.arrange(sets1, sets2, nrow = 1)
ggsave("yop2.pdf", plot, width = 8, height = 4, units = c("in"))

