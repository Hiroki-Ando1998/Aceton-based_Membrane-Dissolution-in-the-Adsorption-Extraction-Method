
data {
  int<lower=1> N;          // サンプル数
  vector[N] y;             // 観測データ（log10変換後のRatioなど）
}

parameters {
  real mu;                 // 平均
  real<lower=0> sigma;     // 標準偏差（正の値に制約）
}

model {
  // 弱情報事前分布（データのスケールに合わせて適宜調整可能）
  mu ~ normal(0, 5);
  sigma ~ cauchy(0, 2.5);  // または normal(0, 2.5)

  // 尤度（正規分布の当てはめ）
  y ~ student_t(3, mu, sigma);
}

generated quantities {
  
  // 事後予測チェック（PPC）用の予測値生成
  vector[N] y_rep;
  for (n in 1:N) {
    y_rep[n] = normal_rng(mu, sigma);
  }
}
