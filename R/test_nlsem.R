#!/usr/bin/Rscript
#  test_int_meth.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 03.25.2018

##Test the method we're using for esimating interactions
require(lavaan)
require(nlsem)
require(Matrix)

## Simulate some data
set.seed(123)
n <- 1000 #Number of observations
ipf <- 3#Indicators per factor
r <- 3#Number of Factors

#Generate each factor
betas <- c(0.5,0.4,0.3)
sigmas_z <- rgamma(r, 1, 1)
Z <- matrix(rnorm(n*r), nrow = n, ncol = r)
Z[,1] <- rnorm(n, 0, sigmas_z[1])
Z[,2] <- rnorm(n, 0, sigmas_z[2])
Z[,3] <- rnorm(n, 0, sigmas_z[3]) + betas[1] * Z[,1] + betas[2] * Z[,2] + betas[3] * Z[,1] * Z[,2]

#Generate the coordinates of the indicators in factor space
LAMBDA <- as.matrix(bdiag(lapply(1:r, function(i) t(c(1, rgamma(ipf-1, 1, 1))))))

#Generate the error matrix
sigmas_x <- rgamma(ipf*r, 1, 10)
E <- t(do.call(rbind, lapply(1:(ipf*r), function(i) rnorm(n, 0, sigmas_x[i]))))

MU <- Z %*% LAMBDA
X <- MU + E

X <- apply(X, 2, function(i) i - mean(i))

#### ANALYZE USING LAVAAN
#First pass
df <- data.frame(X)

lav_model <- "
    #Measurement Model
    Z1 =~ X1 + X2 + X3
    Z2 =~ X4 + X5 + X6
    Z3 =~ X7 + X8 + X9

    #Regressions
    Z3 ~ Z1 + Z2 + Z1:Z2
"

nlsem_model <- specify_sem(num.x = 6, num.y = 3, num.xi = 2, num.eta = 1,
    xi = "xi=x1-x3,x4-x6", eta = "x7-x9", rel.lat = "eta1~xi1,eta2~xi1,eta2~eta1",
    num.classes = 2, constraints = "direct1")

start <- runif(count_free_parameters(nlsem_model))
fit <- em(nlsem_model, data = df, start = start, verbose = TRUE, qml = TRUE, max.mstep=100, max.iter = 1000)

fit <- sem(noint_model, data = df)
