devtools::install_github("mullerchristophe21/misaem", force=T)


library(MASS)
d <- 4
n <- 300
cov_rate <- 0.65

beta <- rnorm(d, 0, 1)
mu <- 1:d * 0
sigma <- toeplitz( cov_rate^(0:(d-1)) )

set.seed(123)  # Set seed for reproducibility
X <- mvrnorm(n, mu, sigma)
y <- X %*% beta
y <- exp(y) / (1 + exp(y))
y <- rbinom(n, 1, y)

###

M <- (rnorm(n*d, 0, 1) < 0.)
M <- matrix(M, nrow=n, ncol=d)

X[M] <- NA

###

data <- data.frame(y=y, X)

fit <- misaem::miss.glm("y ~ .", data=data)
fit$trace
mat <- do.call(rbind, fit$trace$beta)

library(ggplot2)
library(dplyr)
library(tidyr)

# Convert matrix to data frame
df <- as.data.frame(mat)
df$Iteration <- 1:nrow(df)  # Add iteration column

# Reshape data for ggplot (long format)
df_long <- df %>%
  pivot_longer(cols = -Iteration, names_to = "Beta", values_to = "Value") %>%
  mutate(Beta = factor(Beta, labels = paste0("Beta ", 1:ncol(mat)))) # Rename factors

# Plot with ggplot
ggplot(df_long, aes(x = Iteration, y = Value, color = Beta)) +
  geom_line(size = 1.2) +  # Thicker lines
  labs(title = "Convergence of Beta Coefficients",
       x = "Iteration",
       y = "Beta values",
       color = "Coefficients") +
  theme_minimal() +  # Clean theme
  scale_color_manual(values = c("red", "blue", "green", "purple", "orange")) +  # Custom colors
  theme(legend.position = "top")  # Move legend to top


