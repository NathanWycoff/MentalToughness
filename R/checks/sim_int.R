#!/usr/bin/Rscript
#  sim_int.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 03.21.2018

## Try to capture a latent interaction effect.
## Example taken from http://rstudio-pubs-static.s3.amazonaws.com/16521_c51516aca5a1430cabf9f6dec4b7ddb5.html

## Define params
reg_coeff = c(0.4, 0.4, 0.2)
ploadings = c(1, 0.96, 0.78)
aloadings = c(1, 0.57, 0.71, 1.01)
gloadings = c(1, 1.1, 0.77)
covpbcatt = 0.31  #between first-order factors

# residual variances:
varp1 = 0.37
varp2 = 0.67
varp3 = 1.39

vara1 = 0.38
vara2 = 0.72
vara3 = 3.01
vara4 = 2.35

varg1 = 1.29
varg2 = 0.54
varg3 = 1.24

varpbc = 2.71
varatt = 1.79
vargc = 2.85

### Sim Data
set.seed(1234)  # so that we can replicate
samplesize = 500
PBC <- rnorm(samplesize, 0, sqrt(varpbc))
ATT <- covpbcatt/varpbc * PBC + sqrt(varatt - covpbcatt/varpbc^2) * rnorm(samplesize)
INT = PBC * ATT
GC <- reg_coeff[1] * PBC + reg_coeff[2] * ATT + reg_coeff[3] * INT + rnorm(samplesize,
    sd = sqrt(vargc))

p1 <- ploadings[1] * PBC + rnorm(samplesize, sd = sqrt(varp1))
p2 <- ploadings[2] * PBC + rnorm(samplesize, sd = sqrt(varp2))
p3 <- ploadings[3] * PBC + rnorm(samplesize, sd = sqrt(varp3))

a1 <- aloadings[1] * ATT + rnorm(samplesize, sd = sqrt(vara1))
a2 <- aloadings[2] * ATT + rnorm(samplesize, sd = sqrt(vara2))
a3 <- aloadings[3] * ATT + rnorm(samplesize, sd = sqrt(vara3))
a4 <- aloadings[4] * ATT + rnorm(samplesize, sd = sqrt(vara4))

g1 <- gloadings[1] * GC + rnorm(samplesize, sd = sqrt(varg1))
g2 <- gloadings[2] * GC + rnorm(samplesize, sd = sqrt(varg2))
g3 <- gloadings[3] * GC + rnorm(samplesize, sd = sqrt(varg3))

obs = cbind(p1, p2, p3, a1, a2, a3, a4, g1, g2, g3)

cn = colnames(obs)
obs = obs - data.frame(matrix(rep(colMeans(obs), nrow(obs)), ncol = ncol(obs),
    byrow = T))  #mean centering
colnames(obs) = cn  # awkward way to retain variable names
round(colMeans(obs), 3)

## Fit measurement model
library(lavaan)  # first install with install.packages('lavaan')
meas.model = "GC =~ g1+g2+g3; PBC =~ p1+p2+p3; ATT  =~ a1+a2+a3+a4"
meas.f = cfa(meas.model, data = obs)
summary(meas.f)
fitmeasures(meas.f)

### Model w no interaction
sem.model = "GC =~ g1+g2+g3;PBC =~ p1+p2+p3; ATT  =~ a1+a2+a3+a4; GC ~ PBC + ATT"
sem.f = sem(sem.model, data = obs)
summary(sem.f)

### Define product indicators
# create product indicator P11 from p1 and a1, and so on
prod = obs$p1 * obs$a1
obs$prod11 = prod - mean(prod)
prod = obs$p1 * obs$a2
obs$prod12 = prod - mean(prod)
prod = obs$p1 * obs$a3
obs$prod13 = prod - mean(prod)
prod = obs$p1 * obs$a4
obs$prod14 = prod - mean(prod)
prod = obs$p2 * obs$a1
obs$prod21 = prod - mean(prod)
prod = obs$p2 * obs$a2
obs$prod22 = prod - mean(prod)
prod = obs$p2 * obs$a3
obs$prod23 = prod - mean(prod)
prod = obs$p2 * obs$a4
obs$prod24 = prod - mean(prod)
prod = obs$p3 * obs$a1
obs$prod31 = prod - mean(prod)
prod = obs$p3 * obs$a2
obs$prod32 = prod - mean(prod)
prod = obs$p3 * obs$a3
obs$prod33 = prod - mean(prod)
prod = obs$p3 * obs$a4
obs$prod34 = prod - mean(prod)
head(obs, 2)

interaction.model.1 = "GC =~ g1+g2+g3; PBC =~ p1+p2+p3; ATT  =~ a1+a2+a3+a4; INT =~ prod11+prod12+prod23+prod34; GC ~ PBC + ATT+INT; prod11 ~~prod12"
int1.f = sem(interaction.model.1, data = obs)
summary(int1.f)
