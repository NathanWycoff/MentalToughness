#!/usr/bin/Rscript
#  test_int_meth.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 03.25.2018

##Test the method we're using for esimating interactions
require(lavaan)
require(Matrix)

## Simulate some data
set.seed(123)
n <- 180 #Number of observations
ipf <- 3#Indicators per factor
r <- 3#Number of Factors

#Generate each factor
betas <- c(0.5,0.6,-0.3)
sigmas_z <- rep(1,r)
Z <- matrix(rnorm(n*r), nrow = n, ncol = r)
Z[,1] <- rnorm(n, 0, sigmas_z[1])
Z[,2] <- rnorm(n, 0, sigmas_z[2])
Z[,3] <- rnorm(n, 0, sigmas_z[3]) + betas[1] * Z[,1] + betas[2] * Z[,2] + betas[3] * Z[,1] * Z[,2]

#Generate the coordinates of the indicators in factor space
LAMBDA <- as.matrix(bdiag(lapply(1:r, function(i) t(c(1, rgamma(ipf-1, 5, 5))))))

#Generate the error matrix
sigmas_x <- rep(1,ipf*r)
E <- t(do.call(rbind, lapply(1:(ipf*r), function(i) rnorm(n, 0, sigmas_x[i]))))

MU <- Z %*% LAMBDA
X <- MU + E

X <- apply(X, 2, function(i) i - mean(i))

#### ANALYZE USING LAVAAN
df <- data.frame(X)

noint_model <- "
    #Measurement Model
    Z1 =~ X1 + X2 + X3
    Z2 =~ X4 + X5 + X6
    Z3 =~ X7 + X8 + X9

    #Regressions
    Z3 ~ Z1 + Z2 
"

fit <- sem(noint_model, data = df)

Z1_coefs <- c(1, coef(fit)[1:2])
Z1_coefs <- Z1_coefs  / sum(Z1_coefs)
Z2_coefs <- c(1, coef(fit)[3:4])
Z2_coefs <- Z2_coefs  / sum(Z2_coefs)

Z1_hat <- rowMeans(X[,1:3] %*% diag(Z1_coefs))
Z2_hat <- rowMeans(X[,4:6] %*% diag(Z2_coefs))

Z1_hat <- Z1_hat - mean(Z1_hat)
Z2_hat <- Z2_hat - mean(Z2_hat)

par(mfrow=c(1,2))
plot(Z1_hat, Z[,1])
plot(Z2_hat, Z[,2])

plot(Z1_hat * Z2_hat, Z[,1] * Z[,2])

df$INT <- Z1_hat * Z2_hat

int_model <- "
    #Measurement Model
    Z1 =~ X1 + X2 + X3
    Z2 =~ X4 + X5 + X6
    Z3 =~ X7 + X8 + X9

    #Regressions
    Z3 ~ Z1 + Z2 + INT
"

fit <- sem(int_model, data = df)
summary(fit)
