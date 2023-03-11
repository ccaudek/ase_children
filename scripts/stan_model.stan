// Hierarchical SDT With Parameter Expansion
// k = number of subjects
// h = number of hits
// f = number of FA
// s = number of signal trials
// n = number of noise trials

data {
  int<lower=1> k;
  int<lower=0> h[k];
  int<lower=0> f[k];
  int<lower=0> s[k]; 
  int<lower=0> n[k]; 
}
parameters {
  real muc;
  real mud;
  real<lower=0> lambdac;
  real<lower=0> lambdad;
  real<lower=0, upper=1> xic;
  real<lower=0, upper=1> xid;
  vector[k] deltac;
  vector[k] deltad;
}
transformed parameters {
  vector[k] d;
  vector[k] c;
  vector<lower=0, upper=1>[k] thetah;
  vector<lower=0, upper=1>[k] thetaf;
  real<lower=0> sigmacnew;
  real<lower=0> sigmadnew;
  real<lower=0> sigmac;
  real<lower=0> sigmad;

  sigmacnew = inv_sqrt(lambdac);
  sigmadnew = inv_sqrt(lambdad);
  sigmac = fabs(xic) * sigmacnew;
  sigmad = fabs(xid) * sigmadnew;

  // Discriminability and Bias
  c = muc + xic * deltac;
  d = mud + xid * deltad;

  // Reparameterization Using Equal-Variance Gaussian SDT
  for(i in 1:k) {
    thetah[i] = Phi(d[i] / 2 - c[i]);
    thetaf[i] = Phi(-d[i] / 2 - c[i]);
  }
}
model {
  // Priors
  muc ~ normal(0, inv_sqrt(.001));
  mud ~ normal(0, inv_sqrt(.001));
  xic ~ beta(1, 1);  // can be removed
  xid ~ beta(1, 1);  // can be removed
  lambdac ~ gamma(.1, .1);
  lambdad ~ gamma(.1, .1);

  deltac ~ normal(0, sigmacnew);
  deltad ~ normal(0, sigmadnew);

  // Observed counts
  h ~ binomial(s, thetah);
  f ~ binomial(n, thetaf);
} // The posterior predictive distribution
