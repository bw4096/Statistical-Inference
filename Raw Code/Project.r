library(tidyverse)
set.seed(1968)
n <- 40
lambda <- 0.2
theoretical_mean <- 1/lambda
sd <- 1/lambda
variance <- (1/lambda)^2
sem <- sd / sqrt(n)

# take a sample of rexp variables
sample <- rexp(1000, lambda)

# report measured mean and variance
sample_mean <- mean(sample)
sample_var <- var(sample)

# histogram of 1000 randow exp variables
data <- data.frame(sample)
data %>% ggplot(aes(sample)) +
        geom_histogram(alpha = 0.5, col = "Black", fill = "lightblue") +
        geom_vline(size = 2,xintercept = theoretical_mean, colour = "Red", lty = 1) +
        geom_vline(size = 2,xintercept = sample_mean, colour = "Green", lty = 2) +
        geom_vline(size = 2,xintercept = variance, colour = "Purple", lty = 1) +
        geom_vline(size = 2,xintercept = sample_var, colour = "Yellow", lty = 2) +
        ggtitle("Figure 1: Histogram of 1000 rexp variables (lambda=0.2)") 

# Histogram of means of 1000 40 sample exp variables
lambda <- 0.2   # lambda

n <- 40         # number of exponentials

sims <- 1000    # number of simulations

#Run simulations
sim_exp <- replicate(sims, rexp(n, lambda))

#Calc the means of the exponential simulations
mu_xbar <- apply(sim_exp, 2, mean)

data1 <- data.frame(mu_xbar)
data1 %>% ggplot(aes(mu_xbar)) +
        geom_histogram(alpha = 0.5, col = "Black", fill = "lightblue") +
        geom_vline(size = 2,xintercept = theoretical_mean, colour = "Red", lty = 1) +
        labs(x = "Simulation Means") +
        geom_vline(size = 2,xintercept = mean(mu_xbar), colour = "Green", lty = 2) +
        geom_vline(size = 2,xintercept = theoretical_mean + c(-1,1) * sem, colour = "Purple", lty = 1) +
        geom_vline(size = 2,xintercept = mean(mu_xbar) + c(-1,1) * sd(mu_xbar), colour = "Yellow", lty = 2) +
        ggtitle("Figure 2: Histogram of 1000 averages of 40 rexp variables (lambda=0.2)") 

#Histogram with Normal Distribution Curve
data1 %>% ggplot(aes(mu_xbar)) +
        geom_histogram(alpha = 0.5, col = "Black", fill = "lightblue", aes(y = ..density..)) +
        stat_function(fun = dnorm, args = list(mean = theoretical_mean, sd = sem)) +
        labs(x = "Simulation Means") +
        ggtitle("Figure 3: Histogram of 1000 averages of 40 rexp variables (lambda=0.2)") 