data {
  int<lower=1> N;                 // segments
  int<lower=1> P;                 // players
  int<lower=1> L;                 // lineups
  int<lower=1> G;                 // games
  int<lower=1,upper=P> uconn5[N,5];
  int<lower=1,upper=L> lineup_id[N];
  int<lower=1,upper=G> game_id[N];

  vector[N] y_def;                // points_against / poss_est
  vector<lower=0>[N] w;           // poss_est weights

  vector[G] opp_adjO_z;           // standardized opponent AdjO at game-level
  vector[G] site_home;            // 1 home, 0 away_or_neutral

  vector[N] score_margin_start_z; // standardized UConn score margin at stint start
  vector[N] elapsed_game_sec_z;   // standardized elapsed game seconds at stint start
}

parameters {
  real intercept_def;

  vector[P] alpha_def_raw;
  real<lower=0> tau_def;

  vector[L] u_def_raw;
  real<lower=0> tau_u_def;

  real b_oppO;
  real b_home;
  real b_score_margin;
  real b_elapsed_game;

  real<lower=0> sigma;
}

transformed parameters {
  // Sum-to-zero constraints remove soft intercept/effect tradeoffs.
  vector[P] alpha_def = tau_def * (alpha_def_raw - mean(alpha_def_raw));
  vector[L] u_def = tau_u_def * (u_def_raw - mean(u_def_raw));
}

model {
  // Priors
  intercept_def ~ normal(0, 0.5);

  alpha_def_raw ~ normal(0, 1);
  tau_def ~ normal(0, 0.2);

  u_def_raw ~ normal(0, 1);
  tau_u_def ~ normal(0, 0.2);

  b_oppO ~ normal(0, 0.2);
  b_home ~ normal(0, 0.2);
  b_score_margin ~ normal(0, 0.2);
  b_elapsed_game ~ normal(0, 0.2);

  sigma ~ normal(0, 1);

  // Likelihood (weighted by possessions)
  for (i in 1:N) {
    real mu =
      intercept_def
      + alpha_def[uconn5[i,1]] + alpha_def[uconn5[i,2]] + alpha_def[uconn5[i,3]]
      + alpha_def[uconn5[i,4]] + alpha_def[uconn5[i,5]]
      + u_def[lineup_id[i]]
      + b_oppO * opp_adjO_z[game_id[i]]
      + b_home * site_home[game_id[i]]
      + b_score_margin * score_margin_start_z[i]
      + b_elapsed_game * elapsed_game_sec_z[i];

    y_def[i] ~ normal(mu, sigma / sqrt(w[i] + 1e-6));
  }
}

generated quantities {
  // Probability lineup is worse than baseline defense (u_def > 0)
  vector[L] pr_leak;
  for (l in 1:L) {
    pr_leak[l] = u_def[l] > 0;
  }
}
