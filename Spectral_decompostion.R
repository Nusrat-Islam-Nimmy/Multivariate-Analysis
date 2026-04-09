#Example 2.10: Spectral decomposition for the given data

# Define matrix S
S <- matrix(c(13, -4, 2,
              -4, 13, -2,
              2, -2,  10), 
            nrow=3, byrow=TRUE)

print("Matrix S:")
print(S)

# 1. Eigen decomposition
eig <- eigen(S)

# Eigenvalues
lambda <- eig$values
print("Eigenvalues:")
print(lambda)

# Eigenvectors
Q <- eig$vectors
print("Eigenvectors (columns):")
print(Q)

# 2. Verify spectral decomposition: S ≈ Q * diag(lambda) * t(Q)
reconstructed <- Q %*% diag(lambda) %*% t(Q)
print("Reconstructed Matrix (Q * Lambda * Q^T):")
print(reconstructed)

# 3. Visualization
par(mfrow=c(1,2))

# Barplot of eigenvalues
barplot(lambda, 
        main="Eigenvalues", 
        col="skyblue", 
        ylab="Value", 
        names.arg=paste0("λ", 1:length(lambda)))

# Plot eigenvectors (as arrows from origin)
plot(c(-1,1), c(-1,1), type="n", 
     main="Eigenvectors", xlab="X1", ylab="X2",
     xlim=c(-1,1), ylim=c(-1,1))
arrows(0, 0, Q[1,1], Q[2,1], col="red", lwd=2, length=0.1)
arrows(0, 0, Q[1,2], Q[2,2], col="blue", lwd=2, length=0.1)
arrows(0, 0, Q[1,3], Q[2,3], col="green", lwd=2, length=0.1)
legend("topright", legend=c("Eigenvector 1","Eigenvector 2","Eigenvector 3"),
       col=c("red","blue","green"), lwd=2)
