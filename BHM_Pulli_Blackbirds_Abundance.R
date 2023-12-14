# Loading necessary library
library(rstan)
library(ggplot2)

# Creating a data frame of Pulli Blackbirds
years <- c(1964:1983)
captured <- c(2488, 3583, 4518, 4315, 4347, 4517, 3448, 3461,3745, 3139, 2811, 3166, 3141, 3535,3646, 3918, 3403, 3510, 2927, 4150)

# Preparing the data for Stan
stan_data <- list(
  N = length(captured),
  captured = captured
)

# Creating a single Bayesian Hierarchical model
stan_model <- "
data {
  int<lower=0> N;
  int<lower=0> captured[N];
}

parameters {
  real<lower=0, upper=10000> mu;
  real<lower=0, upper=10000> sigma;
  real<lower=0, upper=10000> lambda[N];
}

model {
  mu ~ normal(5000, 1000);     // Non-uniform prior for mu
  sigma ~ cauchy(0, 5);        // Non-uniform prior for sigma
  for (n in 1:N) {
    captured[n] ~ poisson(lambda[n]);
  }
}
"

# Fitting the model
fit <- stan(model_code = stan_model, data = stan_data)

# Extracting the samples lambda as estimated abundance of population
samples <- extract(fit)$lambda

# Calculating the mean and 95% credible interval of lambda
mean_lambda <- apply(samples, 2, mean)
ci <- apply(samples, 2, quantile, probs = c(0.025, 0.975))

# Creating a data frame to plot
plot_data <- data.frame(
  year = years,
  mean = mean_lambda,
  lower = ci[1, ],
  upper = ci[2, ]
)

# Plotting a graph
ggplot(plot_data, aes(x = year, y = mean)) +
  stat_smooth(color = "blue", size = 1) +
  geom_point(color = "black", size =2 ) +
  theme(panel.background = element_rect(fill = "#FFFFFF", color = "black")) +
  theme(plot.background = element_rect(fill = "#FFFFFF", color = "black")) + theme_classic() +
  xlab('Year') +
  ylab('Relative Population') +
  ggtitle('Estimated Population Abundance of Pulli Blackbirds (BHM)') +
  theme(text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        axis.title.x = element_text(size = 16, color = "black"),
        axis.title.y = element_text(size = 16, color = "black"))


