#Loading necessary library
library(rstan)
library(ggplot2)

# Creating data frame of Pulli Blackbirds
years <- c(1964:1983)
captured <- c(2488, 3583, 4518, 4315, 4347, 4517, 3448, 3461, 3745, 3139, 2811, 3166, 3141, 3535, 3646, 3918, 3403, 3510, 2927, 4150)
recaptured <- c(52, 74, 78, 67, 101, 81, 58, 55, 67, 54, 42, 57, 58, 64, 60, 62, 54, 57, 41, 70)

# Preparing data for Stan
data_list <- list(
  N_years = length(years),
  captures = captured,
  recaptures = recaptured
)

# Creating Bayesian Hierarchical model
stan_code <- "
data {
  int<lower=1> N_years;        // Number of years
  int captures[N_years];     // Number of captures
  int recaptures[N_years];   // Number of recaptures
}

parameters {
  real<lower=0> alpha;  // Prior hyperparameter
  real<lower=0> beta;   // Prior hyperparameter
  real<lower=0, upper=1> gamma[N_years];  // Recapture rate for each year
  real<lower=0> lambda[N_years];  // Capture rate for each year
}

model {
  // Priors
  alpha ~ normal(0, 1000);
  beta ~ normal(0, 1000);

  // Likelihood
  for (i in 1:N_years) {
    captures[i] ~ poisson(lambda[i]);
    recaptures[i] ~ binomial(captures[i], gamma[i]);
  }

  // Hierarchical model for capture and recapture rates
  for (i in 1:N_years) {
    lambda[i] ~ gamma(alpha, beta);
    gamma[i] ~ beta(2, 2);
  }
}
"

# Compiling the model
stan_model <- stan_model(model_code = stan_code)

# Fitting the model
fit <- sampling(stan_model, data = data_list, iter = 2000, chains = 4)

# Printing summary
print(fit)

# Extracting posterior samples
posterior_samples <- extract(fit)




# Computing posterior mean of gamma as estimated recapture probability
gamma_mean <- colMeans(posterior_samples$gamma)

# Computing 95%  credible interval of gamma
gamma_ci <- apply(posterior_samples$gamma, 2, quantile, probs = c(0.025, 0.975))

# Creating a data frame to plot
plot_data <- data.frame(
  year = years,
  gamma_mean = gamma_mean,
  lower = gamma_ci[1, ],
  upper = gamma_ci[2, ]
)

#Plotting the graph
ggplot(plot_data, aes(x = year, y = gamma_mean)) +
  stat_smooth(color = "blue", size = 1) +
  geom_point(color = "black", size =2 ) +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "black")) +
  theme(plot.background = element_rect(fill = "#FFFFFF", color = "black")) + theme_classic() +
  xlab('Year') +
  ylab('Recapture Probability') +
  ggtitle('Estimated Recapture Probability of Pulli Blackbirds (BHM)') +
  theme(text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title.x = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 16, color = "black"))

