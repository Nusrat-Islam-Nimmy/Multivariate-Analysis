# Construct matrix and mean vector, covariance matrix, and correlation matrix from the matrix.

# Create data frame
df <- data.frame(
  x1 = c(9, 2, 6, 5, 8),
  x2 = c(12, 8, 6, 4, 10),
  x3 = c(3, 4, 0, 2, 1)
)

# Print data frame
print("Data frame:")
print(df)

# 1. Convert to matrix
X <- as.matrix(df)
print("Data Matrix:")
print(X)

# 2. Mean vector
# Built-in
mean_vec <- colMeans(X)
print("Mean Vector (built-in):")
print(mean_vec)

# Manual calculation
n <- nrow(X)
mean_manual <- apply(X, 2, function(col) sum(col)/n)
print("Mean Vector (manual):")
print(mean_manual)

# 3. Covariance matrix
# Built-in
cov_mat <- cov(X)
print("Covariance Matrix (built-in):")
print(cov_mat)

# Manual formula: (X - mean)^T (X - mean) / (n-1)
X_centered <- scale(X, center = TRUE, scale = FALSE)
cov_manual <- t(X_centered) %*% X_centered / (n - 1)
print("Covariance Matrix (manual):")
print(cov_manual)

# 4. Correlation matrix
# Built-in
cor_mat <- cor(X)
print("Correlation Matrix (built-in):")
print(cor_mat)

# Manual correlation = cov / (sd_i * sd_j)
sd_vec <- apply(X, 2, sd)
cor_manual <- cov_manual / (sd_vec %o% sd_vec)
print("Correlation Matrix (manual):")
print(cor_manual)

# 5. Visualization (simple heatmaps)
par(mfrow=c(1,2)) # two plots side by side

image(1:ncol(X), 1:ncol(X), cov_mat,
      main="Covariance Matrix Heatmap", 
      xlab="", ylab="", axes=FALSE, col=heat.colors(12))
axis(1, at=1:ncol(X), labels=colnames(df))
axis(2, at=1:ncol(X), labels=colnames(df))

image(1:ncol(X), 1:ncol(X), cor_mat,
      main="Correlation Matrix Heatmap", 
      xlab="", ylab="", axes=FALSE, col=terrain.colors(12))
axis(1, at=1:ncol(X), labels=colnames(df))
axis(2, at=1:ncol(X), labels=colnames(df))
