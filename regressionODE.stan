functions {
  vector switching_process(real t,
                          vector z,
                          real[] theta //lambda_minus,lambda_plus, omega_minus, omega_plus
                          ) {
    vector[5] dzdt;
    real lambda_minus = theta[1];
    real lambda_plus = theta[2];
    // real lambda_minus = 1.2;
    // real lambda_plus = 1.2;
    real omega_minus = theta[3];
    real omega_plus = theta[4];

    dzdt[1] = lambda_minus*z[1] + omega_minus*z[2]; // mean -
    dzdt[2] = lambda_plus*z[2] + omega_plus*z[1]; // mean +
    dzdt[3] = lambda_minus*z[1] + omega_minus*z[2] + 2*lambda_minus*z[3] + 2*omega_minus*z[5]; // var -
    dzdt[4] = lambda_plus*z[2] + omega_plus*z[1] + 2*lambda_plus*z[4] + 2*omega_plus*z[5]; // var +
    dzdt[5] = (lambda_minus + lambda_plus)*z[5] + omega_plus*z[3] + omega_minus*z[4]; // covariance

    // dzdt[3] = 2*lambda_minus*z[3] + lambda_minus*z[1] + 2*omega_minus*z[5] + omega_minus*z[2];
    // dzdt[4] = 2*lambda_plus*z[4] + lambda_plus*z[2] + 2*omega_plus*z[5] + omega_plus*z[1];
    // dzdt[5] = (lambda_minus + lambda_plus)*z[5] + omega_plus*z[3] + omega_minus*z[4];

    return dzdt;
  }
}

data {
  int<lower=1> n_times; 
  vector[5] z0;
  real t0;
  vector[n_times] zminus;
  vector[n_times] zplus;
  array[n_times] real t;
}

// transformed data {
//   real x_r[0];
//   int x_i[0];
// }

parameters {
  real<lower=0> lambda_minus;
  real<lower=0> lambda_plus;
  real<lower=0, upper=0.02> rate_minus; // omega_minus / lambda_plus
  real<lower=0, upper=0.02> rate_plus; // omega_plus / lambda_minus
}

transformed parameters {
  real theta[4];
  theta[1] = lambda_minus;
  theta[2] = lambda_plus;
  theta[3] = rate_minus*lambda_plus;
  theta[4] = rate_plus*lambda_minus;
}

model {
  // real z_hat[n_times,5];
  array[n_times] vector[5] z_hat = ode_rk45(switching_process, z0, t0, t, theta);
  
  target += gamma_lpdf(lambda_minus | 2., 1.5);
  target += gamma_lpdf(lambda_plus | 2., 1.5);
  target += gamma_lpdf(rate_minus | 1, 1000);
  target += gamma_lpdf(rate_plus | 3, 500);

  // z_hat = integrate_ode_rk45(switching_process, z0, t0, t, theta, x_r, x_i);
  
  for (i in 1:n_times) {
    target += normal_lpdf(zminus[i] | z_hat[i,1], sqrt(z_hat[i,3]));
    target += normal_lpdf(zplus[i] | z_hat[i,2], sqrt(z_hat[i,4]));
  }
  
}

generated quantities {
  real pred_minus[n_times];
  real pred_plus[n_times];
  // real var_minus[n_times];
  // real var_plus[n_times];
  // real pred[n_times, 5] = integrate_ode_rk45(switching_process, z0, t0, t, theta, x_r, x_i);
  array[n_times] vector[5] pred = ode_rk45(switching_process, z0, t0, t, theta);

  for (i in 1:n_times){
    pred_minus[i] = pred[i,1];
    pred_plus[i] = pred[i,2];
    // var_minus[i] = pred[i,3];
    // var_plus[i] = pred[i,4];
  }
}
