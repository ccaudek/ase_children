data {
  int<lower=0> J;        // number of subjects
	int<lower=0> N;        // number of trials
	vector<lower=0>[N] rt; // vector of response times in seconds
  int<lower=0> id[N];    // array of subjects' id (1,2,3,...)
}
parameters {
	real<lower=0> mu[J];
	real<lower=0> sigma[J];
	real<lower=0> lambda[J];
  real<lower=0> mu_m;
  real<lower=0> sigma_m;
  real<lower=0> mu_s;
  real<lower=0> sigma_s;
  real<lower=0> mu_l;
  real<lower=0> sigma_l;
}
transformed parameters {
	real<lower=0> tau;
	tau = 1/mu_l; // transform hyperparameter mu_lambda
}
model {
   mu     ~ normal(mu_m , sigma_m);
   sigma  ~ normal(mu_s , sigma_s);
   lambda ~ normal(mu_l , sigma_l);
   for (n in 1:N)
     rt[n] ~ exp_mod_normal(mu[id[n]], sigma[id[n]], lambda[id[n]]);
}

