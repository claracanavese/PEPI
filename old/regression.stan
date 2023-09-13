functions{
  real zmin_ode(real t, 
                vector z0,
                real lambda_minus,
                real lambda_plus,
                real omega_minus,
                real omega_plus) { 
    real delta = (lambda_minus - lambda_plus)^2 + 4*omega_minus*omega_plus;
    real c1 = ((lambda_minus - lambda_plus + sqrt(delta))*z0[1] + 2*omega_minus*z0[2])/((lambda_minus - lambda_plus + sqrt(delta))^2 + 4*omega_minus*omega_plus);
    real c2 = (2*omega_plus*z0[1] - (lambda_minus - lambda_plus + sqrt(delta))*z0[2])/((lambda_minus - lambda_plus + sqrt(delta))^2 + 4*omega_minus*omega_plus);
    real zmin = exp((lambda_minus + lambda_plus)*t/2.)*(c1*(lambda_minus - lambda_plus + sqrt(delta))*exp(sqrt(delta)*t/2.) + c2*2*omega_minus*exp(-sqrt(delta)*t/2.));
    
    return zmin;
  }
  
  real zplus_ode(real t, 
                vector z0,
                real lambda_minus,
                real lambda_plus,
                real omega_minus,
                real omega_plus) { 
    real delta = (lambda_minus - lambda_plus)^2 + 4*omega_minus*omega_plus;
    real c1 = ((lambda_minus - lambda_plus + sqrt(delta))*z0[1] + 2*omega_minus*z0[2])/((lambda_minus - lambda_plus + sqrt(delta))^2 + 4*omega_minus*omega_plus);
    real c2 = (2*omega_plus*z0[1] - (lambda_minus - lambda_plus + sqrt(delta))*z0[2])/((lambda_minus - lambda_plus + sqrt(delta))^2 + 4*omega_minus*omega_plus);
    real zplus = exp((lambda_minus + lambda_plus)*t/2.)*(c1*2*omega_plus*exp(sqrt(delta)*t/2.) - c2*(lambda_minus - lambda_plus + sqrt(delta))*exp(-sqrt(delta)*t/2.));
    return zplus;
  }
}

data {
  int<lower=1> n_times; 
  int zminus[n_times]; 
  int zplus[n_times]; 
  real t[n_times]; 
  vector[2] z0;
}

parameters {
  real<lower=0> lambda_minus;
  real<lower=0> lambda_plus;
  real<lower=0> omega_minus;
  real<lower=0> omega_plus;
}

// transformed parameters {
//   real<lower=0> omega_minus = effomega_minus*lambda_plus;
//   real<lower=0> omega_plus = effomega_plus*lambda_minus;
// }

model {
  // Priors
  target += gamma_lpdf(lambda_minus | 2., 1.);
  target += gamma_lpdf(lambda_plus | 2., 1.);
  target += gamma_lpdf(omega_minus | 1.5, 280);
  target += gamma_lpdf(omega_plus | 1.5, 280);
  
  // Likelihood
  for (i in 1:n_times){
    target += poisson_lpmf(zminus[i] | zmin_ode(t[i],z0,lambda_minus,lambda_plus,omega_minus,omega_plus));
    target += poisson_lpmf(zplus[i] | zplus_ode(t[i],z0,lambda_minus,lambda_plus,omega_minus,omega_plus));
  }
}

generated quantities {
  real pred_minus[n_times];
  real pred_plus[n_times];
  
  for (i in 1:n_times){
    pred_minus[i] = zmin_ode(t[i],z0,lambda_minus,lambda_plus,omega_minus,omega_plus);
    pred_plus[i] = zplus_ode(t[i],z0,lambda_minus,lambda_plus,omega_minus,omega_plus);
  }
}
