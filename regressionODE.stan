functions {
  real[] switching_process(real t,
                          real[] z,
                          real[] theta, //lambda_minus,lambda_plus, omega_minus, omega_plus
                          real[] x_r,
                          int[] x_i
                          ) {
    real dzdt[4];
    real lambda_minus = theta[1];
    real lambda_plus = theta[2];
    real omega_minus = theta[3];
    real omega_plus = theta[4];
    
    dzdt[1] = lambda_minus*z[1] + omega_minus*z[2];
    dzdt[2] = lambda_plus*z[2] + omega_plus*z[1];
    dzdt[3] = lambda_minus*z[1] + omega_minus*z[2];
    dzdt[4] = lambda_plus*z[2] + omega_plus*z[1];
    // dzdt[3] = 2*lambda_minus*z[3] + lambda_minus*z[1] + 2*omega_minus*z[5] + omega_minus*z[2];
    // dzdt[4] = 2*lambda_plus*z[4] + lambda_plus*z[2] + 2*omega_plus*z[5] + omega_plus*z[1];
    // dzdt[5] = (lambda_minus + lambda_plus)*z[5] + omega_plus*z[3] + omega_minus*z[4];
    
    return dzdt;
  } 
}

data {
  int<lower=1> n_times; 
  real z0[4];
  real t0;
  real zminus[n_times]; 
  real zplus[n_times]; 
  real t[n_times]; 
}

transformed data {
  real x_r[0];
  int x_i[0];
}

parameters {
  real<lower=0> lambda_minus;
  real<lower=0> lambda_plus;
  real<lower=0> omega_minus;
  real<lower=0> omega_plus;
}

transformed parameters {
  real theta[4];
  theta[1] = lambda_minus;
  theta[2] = lambda_plus;
  theta[3] = omega_minus;
  theta[4] = omega_plus;
}

model {
  real z_hat[n_times, 4];
  
  target += gamma_lpdf(lambda_minus | 2., 1.);
  target += gamma_lpdf(lambda_plus | 2., 1.);
  target += gamma_lpdf(omega_minus | 1.5, 280);
  target += gamma_lpdf(omega_plus | 1.5, 280);
  
  // lambda_minus ~ gamma(2.,1.);
  // lambda_plus ~ gamma(2.,1.);
  // omega_minus ~ gamma(1.5, 280);
  // omega_plus ~ gamma(1.5, 280);
  
  z_hat = integrate_ode_rk45(switching_process, z0, t0, t, theta, x_r, x_i);
  
  for (i in 1:n_times) {
    // zminus[i] ~ normal(z_hat[i,1], sqrt(z_hat[i,3]));
    // zplus[i] ~ normal(z_hat[i,2], sqrt(z_hat[i,4]));
    
    // target += poisson_lpmf(zminus[i] | z_hat[i,1]);
    // target += poisson_lpmf(zplus[i] | z_hat[i,2]);
    
    target += normal_lpdf(zminus[i] | z_hat[i,1], sqrt(z_hat[i,3]));
    target += normal_lpdf(zplus[i] | z_hat[i,2], sqrt(z_hat[i,4]));
  }
  
}

generated quantities {
  real pred_minus[n_times];
  real pred_plus[n_times];
  real var_minus[n_times];
  real var_plus[n_times];
  real pred[n_times, 4] = integrate_ode_rk45(switching_process, z0, t0, t, theta, x_r, x_i);
  // real y_prior[n_times];

  for (i in 1:n_times){
    pred_minus[i] = pred[i,1];
    pred_plus[i] = pred[i,2];
    var_minus[i] = pred[i,3];
    var_plus[i] = pred[i,4];
  }

  // for (i in 1:n_times){
  //   pred_minus[i] = zmin_ode(t[i],z0,theta[1],theta[2],theta[3],theta[4]);
  //   pred_plus[i] = zplus_ode(t[i],z0,theta[1],theta[2],theta[3],theta[4]);
  // }
}
