

data {


// MAIN MODEL
  // groups and counts
  int<lower=1> ncounts;                        // Number of observations
  int<lower=1> nreg;                           // number of regions
  int<lower=1> nst;

  int<lower=1> spacetime[ncounts];
  int<lower=0> richness[ncounts];   // Species richness as a count/integer
  int<lower=0> space[ncounts];  // 0-1 indicator for space
  int<lower=0> time[ncounts];  // 0-1 indicator for time
  int<lower=1> reg[ncounts];  // Regions

  real pforest[ncounts];  // Percent forest cover

}



parameters {
// MAIN MODEL
  matrix[nreg, nst] a;

  vector[nreg] b_space;
  vector[nreg] b_time;
  
  real<lower=0> sdnoise;
  vector[ncounts] noise_raw;


}

transformed parameters{
  
 vector[ncounts] lambda;

  for(i in 1:ncounts){

  real noise = sdnoise*noise_raw[i];
  
  lambda[i] = a[reg[i], spacetime[i]] + b_time[reg[i]] * time[i] * pforest[i] + b_space[reg[i]] * space[i] * pforest[i] + noise;
  
  }
  
    // likelihood
}



model {

  
 
// MAIN MODEL

 to_vector(a) ~ std_normal();

 b_space ~ std_normal();
 b_time ~ std_normal();
 
 
 sdnoise ~ std_normal();
 noise_raw ~ std_normal();

richness ~ poisson_log(lambda);         
   
}

generated quantities{
    int y_rep[ncounts];
    int y_space;
    int y_time;
  //vector[ncounts] log_lik;
  vector[nreg]  b_dif_rg;
  real avg_space;
  real avg_time;
  real b_dif;
  real<lower=0> retrans_noise;
  retrans_noise = 0.5*(sdnoise^2);
  

     for(g in 1:nreg){
         b_dif_rg[g] = b_time[g]-b_space[g];
     }
     
     avg_space = mean(b_space);
     avg_time = mean(b_time);
     
     b_dif = avg_time - avg_space;
     
     y_space = 
     
     
  // Y_rep for prior predictive check
  for(k in 1:ncounts){
  y_rep[k] = poisson_log_rng(lambda[k]);
  }
  
  
  // for(n in 1:ncounts){
    // log_lik[n] = poisson_lcdf(richness[n] | a[reg[n], spacetime[n]] + b_time[reg[n]] * time[n] * pforest[n] +  b_space[reg[n]] * space[n] * pforest[n] + observer[obs[n]] + log(stops[n]) + first[firstobs[n]] + sdnoise[n]);
    
  // }
  
  
  
  
}
