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
        ggtitle("Figure 2: Histogram of 1000 rexp variables (lambda=0.2)") 

# Histogram of means of 1000 40 sample exp variables

