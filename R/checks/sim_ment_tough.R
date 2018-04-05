#!/usr/bin/Rscript
#  sim_ment_tough.R Author "Nathan Wycoff <nathanbrwycoff@gmail.com>" Date 02.15.2018

## Simulate our mental toughness model for studies 1 and 2.

#The subgraph with only cog_end, lead_groups, and the effect of cog_end on lead_group
require(lavaan)
require(blavaan)

n <- 100000

## Define some coefficients
# Loadings for cognitive endurance
bfi_to_cogn <- 1
grt_to_cogn <- 0.2
sc.hw_to_cogn <- 1.3
leadChal_to_cogn <- 1.4

# Loadings for leading groups
auth_to_lead <- 1
tfl_to_lead <- 1.2
ili_to_lead <- 1.7

# Loadings for response to discomfort
uh_to_resp <- 1
vmi.uh_to_resp <- 2.1
dis_to_resp <- -1.2
neur_to_resp <- -0.3

# Relations among latent variables
cogn_to_lead <- 1.2
resp_to_cogn <- 0.8
resp_to_group <- 0.2

# Parameters associated with special variables.
ffmq_to_resp <- 0.5
ffmq_int_resp_to_cogn <- 0.2

# Measurement error for each factor, equal within a factor for simplicity
sig_cogn_meas <- 1.2
sig_lead_meas <- 0.3
sig_resp_meas <- 2.1

## Sample some things
# Sample some special indicators
ffmq <- rnorm(n, 0, sig_resp_meas)

#Sample the latent variables
resp_disc <- ffmq_to_resp * ffmq + rnorm(n)
cogn_end <- resp_to_cogn * resp_disc + 
        ffmq_int_resp_to_cogn * resp_disc * ffmq + rnorm(n)
lead_group <- resp_to_group * resp_disc + cogn_to_lead * cogn_end + rnorm(n)

#Sample the indicators
# for cogn_end
bfi_consc <- bfi_to_cogn * cogn_end + rnorm(n, 0, sig_cogn_meas)
grt <- grt_to_cogn * cogn_end + rnorm(n, 0, sig_cogn_meas)
sc.hw <- sc.hw_to_cogn * cogn_end + rnorm(n, 0, sig_cogn_meas)
leadChal <- leadChal_to_cogn * cogn_end + rnorm(n, 0, sig_cogn_meas)

# for lead_group
auth <- auth_to_lead * lead_group + rnorm(n, 0, sig_lead_meas)
tfl <- tfl_to_lead * lead_group + rnorm(n, 0, sig_lead_meas)
ili <- ili_to_lead * lead_group + rnorm(n, 0, sig_lead_meas)

# for resp_disc
uh <- uh_to_resp * resp_disc + rnorm(n, 0, sig_resp_meas)
vmi.uh <- vmi.uh_to_resp * resp_disc + rnorm(n, 0, sig_resp_meas)
dis <- dis_to_resp * resp_disc + rnorm(n, 0, sig_resp_meas)
neur <- neur_to_resp * resp_disc + rnorm(n, 0, sig_resp_meas)

#Confirm with SEM that we get what we expect
fint <- ffmq * resp_disc
df <- data.frame(bfi_consc, grt, sc.hw, auth, tfl, ili, uh, vmi.uh, dis, neur, ffmq, fint, leadChal)
model <- '  #Measurement Model:
            cogn_end =~ bfi_consc + grt + sc.hw + leadChal
            lead_group =~ auth + tfl + ili
            resp_disc =~ uh + vmi.uh + dis + neur
            #Latent Structure
            resp_disc ~ ffmq
            cogn_end ~ resp_disc + fint
            lead_group ~ cogn_end + resp_disc
            '
fit <- sem(model, data = df)
summary(fit)
