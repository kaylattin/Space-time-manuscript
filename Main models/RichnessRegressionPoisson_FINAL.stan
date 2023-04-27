

data {


// MAIN MODEL
  // groups and counts
  int<lower=1> ncounts;                        // Number of observations
  int<lower=1> nreg;                           // number of regions
  int<lower=1> nobs;                           // number of unique observers
  int<lower=1> nst;
  int<lower=1> nfirstobs;
  int<lower=1> ndata;

  int<lower=1> spacetime[ncounts];
  int<lower=0> richness[ncounts];   // Species richness as a count/integer
  int<lower=0> space[ncounts];  // 0-1 indicator for space
  int<lower=0> time[ncounts];  // 0-1 indicator for time
  int<lower=1> reg[ncounts];  // Regions

  int<lower=1> obs[ncounts];   // observers
  int<lower=1> firstobs[ncounts];
  real pforest[ncounts];  // Percent forest cover
  vector[11] xrep;
  

}



parameters {
// MAIN MODEL
  matrix[nreg, nst] a;

  vector[nreg] b_space;
  vector[nreg] b_time;

  matrix[nobs, nst] observer_raw;
  vector[nfirstobs] first_raw;
  real<lower=0> sdobs;
  real<lower=0> sdfirst;
  

  
  real<lower=0> sdnoise;
  vector[ncounts] noise_raw;
  

}

transformed parameters{
  
 vector[ncounts] lambda;
 matrix[nobs, nst] observer;
 vector[nfirstobs] first;

  observer = sdobs * observer_raw; 
  first = sdobs * first_raw;

  for(i in 1:ncounts){

  real noise = sdnoise*noise_raw[i];
  
  lambda[i] = a[reg[i], spacetime[i]] + b_time[reg[i]] * time[i] * pforest[i] + b_space[reg[i]] * space[i] * pforest[i] + observer[obs[i], spacetime[i]] + first[firstobs[i]] + noise;
  
  }
  
    // likelihood
}



model {

 
// MAIN MODEL

 to_vector(a) ~ std_normal();

 b_space ~ std_normal();
 b_time ~ std_normal();

 to_vector(observer_raw) ~ std_normal();
 first_raw ~ std_normal();
 
 sdobs ~ std_normal();
 sdfirst ~ std_normal();
 
 sdnoise ~ std_normal();
 noise_raw ~ std_normal();

richness ~ poisson_log(lambda);         
   
}

generated quantities{
    int y_rep[ncounts];
    int sim_space[11];
    int sim_time[11];
  //vector[ncounts] log_lik;
  vector[nreg]  b_dif_rg;
  real avg_b_space = mean(b_space);
  real avg_b_time = mean(b_time);
  real avg_a_space = mean(a[,2]);
  real avg_a_time = mean(a[,1]);
  real avg_obs_space = mean(observer[,2]);
  real avg_obs_time = mean(observer[,1]);
  real avg_first_obs = mean(first);
  real b_dif;

  real<lower=0> retrans_noise;
  retrans_noise = 0.5*(sdnoise^2);
  
  
     for(g in 1:nreg){
         b_dif_rg[g] = b_time[g]-b_space[g];
     }
     
     b_dif = avg_b_time - avg_b_space;
     
    for(i in 1:11){  
    real noise = sdnoise*noise_raw[i];
    
    sim_space[i] = poisson_log_rng(avg_a_space + avg_b_space * xrep[i] + avg_obs_space + avg_first_obs + noise); 
    sim_time[i] = poisson_log_rng(avg_a_time + avg_b_time * xrep[i] +  avg_obs_time + avg_first_obs + noise);
    
    }
      
  // Y_rep for prior predictive check
  for(k in 1:ncounts){
 y_rep[k] = poisson_log_rng(lambda[k]);
  }

  
  // for(n in 1:ncounts){
    // log_lik[n] = poisson_lcdf(richness[n] | a[reg[n], spacetime[n]] + b_time[reg[n]] * time[n] * pforest[n] +  b_space[reg[n]] * space[n] * pforest[n] + observer[obs[n]] + log(stops[n]) + first[firstobs[n]] + sdnoise[n]);
    
  //

}
