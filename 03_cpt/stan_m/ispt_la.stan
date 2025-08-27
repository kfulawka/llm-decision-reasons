functions {
  
  real pwf(real p, real gam) {
    
    real wp;
    
    if (p > 0) { wp = exp(-pow(-log(p), gam)); } else { wp = 0; }
    
    return wp;
    
  }
  
  real vf(real x, real s, real alpha, real beta, int mixed) {
    
    real v;
    real L;

    if (mixed == 0) { L = 1; } else { L = beta; }

    v = L * s * pow(fabs(x), alpha); 

    return v;
  
  }

}

data {
  
  // n - no. of data points; N - no. of subjects
  int<lower = 0> n;
  int<lower = 0> N;
  

  // subject ind vector
  int<lower = 1> sub[n];
  
  // XP data
  matrix[n, 3] PA;
  matrix[n, 3] PB;
  
  matrix[n, 3] XA;
  matrix[n, 3] XB;
  
  matrix[n, 3] XAs;
  matrix[n, 3] XBs;
  
  int mixed[n];
  
  int<lower = 0, upper = 1> choice[n];
  
}

parameters {
  
  // SPT pop-lvl parameters
  real pop_lvl_mu[4]; 
  
  // individual-lvl z-scale displacements
  matrix[4, N] id_z;
  
  // ind-dist variances
  vector<lower = 0>[4] id_sig;

}


transformed parameters {
  
  // spt sv cpmutpation
  matrix[n, 3] pi_pa;
  matrix[n, 3] pi_pb;
  matrix[n, 3] v_xa;
  matrix[n, 3] v_xb;
  matrix[n, 2] sv_ab;
  
  real lin_pred[n];
  
  // id-lvl model parameters on model scale
  vector[N] ialpha;
  vector[N] ibeta;
  vector[N] igam;
  vector[N] iphi;

  // id-lvl parameters on the model scale
  for(s in 1:N) {
    
    ialpha[s] = Phi(pop_lvl_mu[1] + id_sig[1] * id_z[1, s]);
    ibeta[s] = Phi(pop_lvl_mu[2] + id_sig[2] * id_z[2, s]) * 5;
    igam[s] = Phi(pop_lvl_mu[3] + id_sig[3] * id_z[3, s]) * 2;
    iphi[s] = Phi(pop_lvl_mu[4] + id_sig[4] * id_z[4, s]) * 5;
    
  }

  // for each data point
  for(i in 1:n) {
    
    // for each outcome branch
    for(j in 1:3) {
      
      // option A
      v_xa[i,j] =  vf(XA[i,j], XAs[i,j], ialpha[sub[i]], ibeta[sub[i]], mixed[i]);
      pi_pa[i,j] = pwf(PA[i,j], igam[ sub[i] ]);

      // option B
      v_xb[i,j] =  vf(XB[i,j], XBs[i,j], ialpha[sub[i]], ibeta[sub[i]], mixed[i]);
      pi_pb[i,j] = pwf(PB[i,j], igam[ sub[i] ]);

    }
    
    // subjective values
    sv_ab[i,1] = dot_product(v_xa[i,], pi_pa[i,]);
    sv_ab[i,2] = dot_product(v_xb[i,], pi_pb[i,]);
    
    // P(choose a)
    lin_pred[i] = iphi[sub[i]] * (sv_ab[i,1] - sv_ab[i,2]);
    
  }
  
}

model {
  
  // PT parameters
  pop_lvl_mu ~ std_normal();

  // ind-lvl z-scale displacemenets
  to_vector(id_z) ~ std_normal();
  // prior for id standard deviations
  // keeps the sd < 1 so that no bimiodal dists occur after applyin Phi()
  id_sig ~ normal(.5, .15); // id vars

  // likelihood
  choice ~ bernoulli_logit( lin_pred );
  
}

// 
generated quantities {
  
  // pop-lvl pars to monitor
  real <lower = 0> alpha;
  real <lower = 0> beta;
  real <lower = 0> gam;
  real <lower = 0> phi;

  //logliks for loo
  real log_lik[n];

  // pop-lvl transformations
  alpha = Phi(pop_lvl_mu[1]);
  beta = Phi(pop_lvl_mu[2]) * 5;
  gam = Phi(pop_lvl_mu[3]) * 2;
  phi = Phi(pop_lvl_mu[4]) * 5;
  
  // logliks
  for(i in 1:n) {
    
    log_lik[i] = bernoulli_logit_lpmf( choice[i] | lin_pred[i] );
    
  }

}