## Example- 5.1: Hotelling T-Square evaluating ##

# Formula:
#   T^2 = n * (x̄ - μ0)' * S^(-1) * (x̄ - μ0)
#
# where:
#   x̄   = sample mean vector
#   μ0   = hypothesized mean vector
#   S    = sample covariance matrix
#   n    = sample size
#   p    = dimension (number of variables)

# Step 1: Define data matrix X (each row = observation)
X <- matrix(c(6,10,8,   # First column = X1
              9, 6, 3), # Second column = X2
            ncol=2)

# Step 2: Define hypothesized mean vector μ0
mu0 <- c(9,5)

# Step 3: Get sample size (n) and dimension (p)
n <- nrow(X)   # number of observations
p <- ncol(X)   # number of variables

# Step 4: Compute sample mean vector
# Formula: x̄ = (1/n) * Σ Xi
xbar <- colMeans(X)

# Step 5: Compute sample covariance matrix S
# Formula: S = (1/(n-1)) * (X - x̄)' * (X - x̄)
X_centered <- sweep(X, 2, xbar)   # subtract mean from each column
S <- (t(X_centered) %*% X_centered) / (n - 1)

# Step 6: Compute inverse of covariance matrix S
S_inv <- solve(S)

# Step 7: Compute Hotelling’s T^2 statistic
# Formula: T^2 = n * (x̄ - μ0)' * S^(-1) * (x̄ - μ0)
diff <- xbar - mu0
T2 <- n * t(diff) %*% S_inv %*% diff

# Step 8: Sampling distribution of T^2
# Formula: T^2 ~ ((n-1)p / (n-p)) * F_{p, n-p}
df1 <- p
df2 <- n - p
scaling <- ((n - 1) * p) / (n - p)
F_value <- T2 / scaling

# Step 9: Print results
cat("Sample mean vector (x̄):\n")
print(xbar)

cat("\nSample covariance matrix (S):\n")
print(S)

cat("\nInverse covariance matrix (S^-1):\n")
print(S_inv)

cat("\nHotelling's T^2 statistic:\n")
print(T2)

cat("\nEquivalent F-statistic:\n")
cat("F =", F_value, "with df =", df1, "and", df2, "\n")
