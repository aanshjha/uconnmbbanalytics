data {
  int<lower=1> N;                       // stints
  int<lower=1> P;                       // UConn players
  int<lower=1> L;                       // unique UConn 5-man lineups
  array[N,5] int<lower=1,upper=P> uconn5;      // UConn players on court
  array[N] int<lower=1,upper=L> lineup_id;     // lineup id
  array[N] int<lower=1> game_id;               // game index (1..G)
  int<lower=1> G;                       // games

  vector[N] y;                          // net points per possession in stint
  vector<lower=1e-6>[N] w;              // possession weight (estimated)

  // Game-level opponent controls, standardized in R
  vector[G] opp_adjO_z;
  vector[G] opp_adjD_z;
  vector[G] site_home;                  // 1 if UConn home, 0 if away_or_neutral

  // Stint-level game state controls, standardized in R
  vector[N] score_margin_start_z;       // UConn score margin at stint start
  vector[N] elapsed_game_sec_z;         // elapsed game seconds at stint start
}

parameters {
  real intercept;

  // Player net impact term (identifiable from net PPP outcome)
  vector[P] alpha_net_raw;
  real<lower=0> tau_net;

  // Lineup synergy term (deviation beyond player sum)
  vector[L] u_raw;
  real<lower=0> tau_u;

  // Opponent and site effects
  real b_oppO;
  real b_oppD;
  real b_home;
  real b_score_margin;
  real b_elapsed_game;

  real<lower=0> sigma;
}

transformed parameters {
  // Sum-to-zero constraints remove soft intercept/effect tradeoffs.
  vector[P] alpha_net = tau_net * (alpha_net_raw - mean(alpha_net_raw));
  vector[L] u = tau_u * (u_raw - mean(u_raw));
}

model {
  // Priors tuned for net points/possession scale
  intercept ~ normal(0, 0.05);

  alpha_net_raw ~ normal(0, 1);
  tau_net ~ normal(0, 0.05);

  u_raw ~ normal(0, 1);
  tau_u ~ normal(0, 0.03);

  b_oppO ~ normal(0, 0.05);
  b_oppD ~ normal(0, 0.05);
  b_home ~ normal(0, 0.05);
  b_score_margin ~ normal(0, 0.05);
  b_elapsed_game ~ normal(0, 0.05);

  sigma ~ normal(0, 0.10);

  for (n in 1:N) {
    real mu = intercept
              + u[lineup_id[n]]
              + b_oppO * opp_adjO_z[game_id[n]]
              + b_oppD * opp_adjD_z[game_id[n]]
              + b_home * site_home[game_id[n]]
              + b_score_margin * score_margin_start_z[n]
              + b_elapsed_game * elapsed_game_sec_z[n];

    // Net player contribution is the identifiable player term under net PPP outcome.
    for (k in 1:5) {
      mu += alpha_net[uconn5[n,k]];
    }

    y[n] ~ normal(mu, sigma / sqrt(w[n]));
  }
}

generated quantities {
  vector[N] y_rep;
  for (n in 1:N) {
    real mu = intercept
              + u[lineup_id[n]]
              + b_oppO * opp_adjO_z[game_id[n]]
              + b_oppD * opp_adjD_z[game_id[n]]
              + b_home * site_home[game_id[n]]
              + b_score_margin * score_margin_start_z[n]
              + b_elapsed_game * elapsed_game_sec_z[n];

    for (k in 1:5) {
      mu += alpha_net[uconn5[n,k]];
    }

    y_rep[n] = normal_rng(mu, sigma / sqrt(w[n]));
  }
}
