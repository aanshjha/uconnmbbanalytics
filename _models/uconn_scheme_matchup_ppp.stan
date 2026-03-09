// Hierarchical PPP model for action-by-coverage matchup effects.
data {
  int<lower=1> N;
  int<lower=1> A;
  int<lower=1> C;
  int<lower=1> AC;
  int<lower=1> T_off;
  int<lower=1> T_def;
  int<lower=1> L;

  array[N] int<lower=1, upper=A> action_id;
  array[N] int<lower=1, upper=C> coverage_id;
  array[N] int<lower=1, upper=AC> action_coverage_id;
  array[N] int<lower=1, upper=T_off> off_team_id;
  array[N] int<lower=1, upper=T_def> def_team_id;
  array[N] int<lower=1, upper=L> lineup_id;

  vector[N] transition_flag;
  vector[N] shot_quality_z;
  vector[N] y;
}

parameters {
  real alpha;

  vector[A] beta_action_raw;
  vector[C] gamma_coverage_raw;
  vector[AC] delta_action_coverage_raw;
  vector[T_off] u_off_raw;
  vector[T_def] v_def_raw;
  vector[L] w_lineup_raw;

  real<lower=0> tau_action;
  real<lower=0> tau_coverage;
  real<lower=0> tau_action_coverage;
  real<lower=0> tau_off_team;
  real<lower=0> tau_def_team;
  real<lower=0> tau_lineup;

  real b_transition;
  real b_shot_quality;

  real<lower=0> sigma;
}

transformed parameters {
  vector[A] beta_action = tau_action * beta_action_raw;
  vector[C] gamma_coverage = tau_coverage * gamma_coverage_raw;
  vector[AC] delta_action_coverage = tau_action_coverage * delta_action_coverage_raw;
  vector[T_off] u_off = tau_off_team * u_off_raw;
  vector[T_def] v_def = tau_def_team * v_def_raw;
  vector[L] w_lineup = tau_lineup * w_lineup_raw;
}

model {
  alpha ~ normal(1.00, 0.35);

  beta_action_raw ~ normal(0, 1);
  gamma_coverage_raw ~ normal(0, 1);
  delta_action_coverage_raw ~ normal(0, 1);
  u_off_raw ~ normal(0, 1);
  v_def_raw ~ normal(0, 1);
  w_lineup_raw ~ normal(0, 1);

  tau_action ~ normal(0, 0.20);
  tau_coverage ~ normal(0, 0.20);
  tau_action_coverage ~ normal(0, 0.10);
  tau_off_team ~ normal(0, 0.20);
  tau_def_team ~ normal(0, 0.20);
  tau_lineup ~ normal(0, 0.15);

  b_transition ~ normal(0, 0.20);
  b_shot_quality ~ normal(0, 0.50);

  sigma ~ normal(0, 0.35);

  for (n in 1:N) {
    real mu = alpha
      + beta_action[action_id[n]]
      + gamma_coverage[coverage_id[n]]
      + delta_action_coverage[action_coverage_id[n]]
      + u_off[off_team_id[n]]
      + v_def[def_team_id[n]]
      + w_lineup[lineup_id[n]]
      + b_transition * transition_flag[n]
      + b_shot_quality * shot_quality_z[n];

    y[n] ~ normal(mu, sigma);
  }
}

generated quantities {
  vector[N] y_rep;
  vector[N] log_lik;

  for (n in 1:N) {
    real mu = alpha
      + beta_action[action_id[n]]
      + gamma_coverage[coverage_id[n]]
      + delta_action_coverage[action_coverage_id[n]]
      + u_off[off_team_id[n]]
      + v_def[def_team_id[n]]
      + w_lineup[lineup_id[n]]
      + b_transition * transition_flag[n]
      + b_shot_quality * shot_quality_z[n];

    y_rep[n] = normal_rng(mu, sigma);
    log_lik[n] = normal_lpdf(y[n] | mu, sigma);
  }
}
