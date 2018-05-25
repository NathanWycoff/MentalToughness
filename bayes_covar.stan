data {
    int<lower=1> P; // Number of measurements
    int<lower=1> M; // Number of groups.
    int<lower=1> N; // Sample size for each group

    real<lower=0> nu; //Prior degrees of freedom

    matrix[N, P] X; // Observed data
    int groups[N]; // Which group each datum belongs to; in 1:M

    vector[P] mu_0;//Prior mean vector
    matrix[P, P] SIGMA_mu_0; // Prior covariance for sample mean vector.
    vector[P] b;//
    vector[P] xi;//
}
parameters {
    cov_matrix[P] Q;// A correlation type matrix
    real<lower=0> delta[P]; // Encodes the standard deviations.
    vector[P] mu[M];// Means for each group.
}
transformed parameters {
    cov_matrix[P] SIGMA;
    for (p1 in 1:P) {
        for (p2 in 1:P) {
            SIGMA[p1,p2] = delta[p1] * delta[p2] * Q[p1, p2];
        }
    }
}
model {
    // -- Prior --
    // Hierarchical Inverse Wishart as dicsussed in:
    // https://arxiv.org/pdf/1408.4050.pdf
    // On correlations
    Q ~ inv_wishart(nu, LAMBDA);
    // On standard deviations
    for (p in 1:P) {
        delta[p] ~ lognormal(b[p], xi[p]);
    }

    // On mean
    for (m in 1:M) {
        mu[m] ~ multi_normal(mu_0, SIGMA_mu_0);
    }

    // -- Likelihood -- 
    for (i in 1:N) {
        X[i,] ~ multi_normal(mu, SIGMA);
    }
}
