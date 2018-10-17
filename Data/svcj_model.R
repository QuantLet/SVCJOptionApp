
svcj_model <- function(P, N = 5000, n = 1000) {

# Generate returns Y from initial prices P
Y <- diff(P)/P[-length(P)]*sqrt(250)

one <- rep(1, length = length(Y))

# Prior distribution of hyperparameter values
a=0
A=25 # prior for mu
b = matrix(c(0,0), nrow = 2, ncol = 1) # prior for alpha and beta
B = diag(2) # prior for alpha and beta
c = 2.5 # prior for sigma2_v
C = 0.1 # prior for sigma2_v
d = 10 # prior for mu_v
D = 20 # prior for mu_v
e = 0 
E = 100 # prior for mu_y
f = 100 # prior for sigma2_y
F = 40 # prior for sigma2_y
g = 0
G = 4
k = 2 # prior for lambda
K = 30 # prior for lambda
tmp = vector()

# Starting values
# m=mu
m=a 
msum=0 
m2sum=0

# kappa=-alpha/beta, the theta in equation 1
kappa = 0 
kappasum = 0 
kappa2sum = 0     

# alpha in equation 3
alpha = b[1]
alphasum = 0 
alpha2sum = 0

# beta in eq.3
beta = b[2] 
betasum = 0 
beta2sum = 0

# sigma_v in eq.3
s2V = C/(c - 2)
s2Vsum = 0
s2V2sum = 0

# the relation between w1 ad w2
rho = 0 
rhosum = 0 
rho2sum = 0

# mu_v, the param in expoential distr. of Z_v
# (jump size in variance)
mV = D/(d-2) 
mVsum = 0
mV2sum = 0         

# mu_y, the mean of jump size in price Z_y
mJ = e 
mJsum = 0 
mJ2sum = 0

# sigma_Y, the variance of jump size in price Z_y 
s2J = F/(f - 2) 
s2Jsum = 0 
s2J2sum = 0

# rho param in the jump size of price
rhoJ = g 
rhoJsum = 0 
rhoJ2sum = 0

# jump intensity
lambda = 0
lambdasum = 0
lambda2sum = 0

# Initial values for variance_t
V = 0.1*(Y - mean(Y))^2 + 0.9*var(Y)
Vsum = 0
Vsum2 = 0

# J = data(2:end,3);
J = abs(Y) - mean(Y) > 2 * sd(Y); 
Jsum = 0

# the jump size in volatility, Z_t^y
XV = rexp(length(Y), rate = 1/mV)
XVsum = 0

# the jump size in price
X = rnorm(n = length(Y), mean = (mJ+XV*rhoJ), sd = s2J^0.5)
Xsum = 0
stdevrho = 0.01
dfrho = 6.5
stdevV = 0.9
dfV = 4.5
acceptsumV = rep(0,length(V))
acceptsumrho = 0
acceptsums2V = 0
Z = rep(1, length(Y))

# matrix for params
test = matrix(0, nrow = N, ncol = 10)

for (i in 1:N){
  
  print(paste0("Iteration ", i, " of ", N))
  Rho = 1/(1-rho^2)
  V0 = V[1]
  
  # Draw m(i+1)
  Q=(Y - X*J - rho/s2V^0.5*(V-append(V0, V[1:length(V)-1])*(1+beta)-alpha-J*XV))/(append(V0, V[1:length(V)-1]))^0.5
  W = (1/append(V0, V[1:length(V)-1]))^0.5
  
  As = solve(solve(A) + 1/(1-rho^2)*(t(W)%*%W))
  as = As*(solve(A)*a+1/(1-rho^2)*(t(W)%*%Q))
  
  m = rnorm(n = 1,mean = as, sd = As^0.5)
  
  if (i>n){
    msum = msum + m
    m2sum = m2sum + m^2
  }
  
  # (alpha, beta)
  # Expected return
  eY = Y - Z*m - X*J
  # Expected variance
  eV = V - append(V0, V[1:length(V)-1]) - XV*J
  Q = (eV - rho*sqrt(s2V)*eY)/append(V0, V[1:length(V)-1])^0.5
  W = cbind(1/append(V0, V[1:length(V)-1])^0.5, append(V0, V[1:length(V)-1])^0.5)
  Bs = solve(solve(B) + as.vector(Rho/s2V)*t(W)%*%W)
  bs = Bs%*%(solve(B)%*%b + as.vector(Rho/s2V)*(t(W)%*%Q))
  library(MASS)
  temp = MASS::mvrnorm(n=1, mu = bs, Sigma = Bs)  
  alpha = temp[1]
  beta = temp[2]
  kappa = -alpha/beta
  
  if (i>n){
    alphasum = alphasum + alpha
    alpha2sum = alpha2sum + alpha^2
    betasum = betasum + beta
    beta2sum = beta2sum + beta^2
    kappasum = kappasum + kappa
    kappa2sum = kappa2sum + kappa^2
  }
  
  # s2V
  cs = c + length(Y)
  Cs = C + sum(((V-append(V0, V[1:length(V)-1])-alpha-beta*append(V0, V[1:length(V)-1])-XV*J)^2)/append(V0, V[1:length(V)-1]))
  #install.packages("MCMCpack")
  library(MCMCpack)
  s2Vprop = MCMCpack::riwish(cs, Cs)
  # iwishrnd(Tau,df) general formula in Matlab
  q = exp(-0.5*sum((V-append(V0, V[1:length(V)-1])*(1+beta) - alpha - J*XV)^2/(as.vector(s2Vprop)*append(V0, V[1:length(V)-1])) -
                     (V - append(V0, V[1:length(V)-1])*(1+beta) - alpha - J*XV)^2/(s2V*append(V0, V[1:length(V)-1]))))
  p = exp(-0.5*sum((V-append(V0, V[1:length(V)-1])*(1+beta) - alpha - J*XV - rho*as.vector(s2Vprop)^0.5*(Y-Z*m-J*X))^2/
                     ((1-rho^2)*as.vector(s2Vprop)*append(V0, V[1:length(V)-1])) -
                     (V-append(V0, V[1:length(V)-1])*(1+beta) - alpha - J*XV - rho*s2V^0.5*(Y-Z*m-J*X))^2/
                     ((1-rho^2)*s2V*append(V0, V[1:length(V)-1]))))
  x = ifelse(is.nan(min(p/q,1)),1,min(p/q,1))
  u = runif(1)
  
  if (x>u){
    s2V = s2Vprop
    if (i>n){
      acceptsums2V = acceptsums2V + 1
    }
  }    
  
  if (i>n){
    s2Vsum = s2Vsum + s2V
    s2V2sum = s2V2sum + s2V^2
  } 
  
  # rho
  # Draw a candidate for rho(i+1)
  # draw rhoc from a t distribution with 8 df and std of 0.2666
  rhoprop = rho + stdevrho*rt(n = length(rho), df = dfrho)
  if (abs(rhoprop)<1){
    p = (sqrt( 1 - rho^2 )/ sqrt( 1 - rhoprop^2 ))^T * exp( sum( - 1 / ( 2 * ( 1 - rhoprop^2 ) ) *
                                                                   ( Y - Z*m - J*X - as.vector(rhoprop / s2V^0.5) * ( V - alpha - append(V0, V[1:length(V)-1]) * ( 1 + beta ) - J*XV ) )^2/
                                                                   append(V0, V[1:length(V)-1]) + 1 / ( 2 * ( 1 - rho^2 ) ) *
                                                                   ( Y - Z*m - J*X - as.vector(rho / s2V^0.5) * ( V[1:length(V)] - alpha - append(V0, V[1:length(V)-1]) * ( 1 + beta ) - J*XV ) )^2/
                                                                   append(V0, V[1:length(V)-1]) ) )
    
    u = runif(1)
    x = min(p,1)
    if (x>u){
      rho = rhoprop
      if (i>n){
        acceptsumrho = acceptsumrho + 1
      } 
    } 
  }
  
  if (i>n){
    rhosum = rhosum + rho
    rho2sum = rho2sum + rho^2
  }
  
  # mV
  ds= d + 2*length(Y)
  Ds = D + 2*sum(XV)
  library(MCMCpack)
  mV = MCMCpack::riwish(ds, Ds)
  
  if (i>n){
    mVsum = mVsum + mV
    mV2sum = mV2sum + mV^2
  }
  
  # mJ
  Es = 1/(length(Y)/s2J + 1/E)
  es = Es * (sum((X-XV*rhoJ)/s2J)+ e/E)
  mJ = rnorm(n=1, mean= es, sd = Es^0.5)
  if (i>n){
    mJsum = mJsum + mJ
    mJ2sum = mJ2sum + mJ^2
  } 
  
  # s2Y
  fs = f + length(Y)
  Fs = F + sum((X-mJ-rhoJ*XV)^2)
  s2J = MCMCpack::riwish(fs,Fs)
  if (i>n){
    s2Jsum = s2Jsum + s2J
    s2J2sum = s2J2sum + s2J^2
  }
  
  # rhoJ
  Gs = solve(sum(XV^2)/s2J + 1/G)
  gs = Gs * (sum((X - mJ)*XV)/s2J + g/G);
  rhoJ = rnorm(n=1,mean = gs,sd=Gs^0.5); 
  if (i > n)  {
    rhoJsum = rhoJsum + rhoJ 
    rhoJ2sum = rhoJ2sum + rhoJ^2
  }
  
  
  # lambda
  ks = k + sum(J)
  Ks = K + length(Y) - sum(J)
  lambda = rbeta(n = 1,ks, Ks)
  
  if (i>n){
    lambdasum = lambdasum + lambda
    lambda2sum = lambda2sum + lambda^2
  }
  
  # J
  eY1 = Y - Z*m - X
  eY2 = Y - Z*m
  eV1 = V - append(V0, V[1:length(V)-1]) - alpha - beta*append(V0, V[1:length(V)-1]) - XV
  eV2 = V - append(V0, V[1:length(V)-1]) - alpha - beta*append(V0, V[1:length(V)-1])
  p1 = lambda*exp( -0.5 * ( ((eY1 - (rho/sqrt(s2V))*eV1)^2)/((1-rho^2)*append(V0, V[1:length(V)-1])) + (eV1^2)/(s2V*append(V0, V[1:length(V)-1])) ) )
  p2 = (1 - lambda) * exp( -0.5 * ( ((eY2 - (rho/sqrt(s2V))*eV2)^2)/((1-rho^2)*append(V0, V[1:length(V)-1])) + (eV2^2)/(s2V*append(V0, V[1:length(V)-1])) ) )
  p = p1/(p1 + p2)
  tmp= cbind(tmp, p1, p2, p)
  
  u = runif(n = length(Y))
  J = as.double(u < p)
  
  if (i>n){
    Jsum = Jsum + J
  }
  
  Jindex = which(J == 1)
  
  # XV
  XV[as.logical(!J)] = rexp(n = (length(Y) - sum(J)), rate = 1/mV)
  if (length(Jindex) != 0){
    if (Jindex[1] == 1) {
      t = 1
      eV = V[1] - V0 - alpha - beta*V0;
      eY = Y[1] - Z[1]*m - X[1]
      H = solve( 1 /((1 - rho^2)*s2V*V0) + rhoJ^2/s2J )
      h = H * ((eV-rho*sqrt(s2V)*eY)/((1 - rho^2)*s2V*V0) + rhoJ*(X[1] - mJ)/s2J - 1/mV)
      if (h+5*sqrt(H) > 0) { 
        XV[1] = truncnorm::rtruncnorm(n = 1, mean = h, sd = sqrt(H), a=0, b=h+5*sqrt(H))
      } else {
        XV[1] = 0
      }
      if (is.infinite(XV[1]) | is.nan(XV[1])) { 
        XV[1] = 0 
      }
    } else {  
      t = Jindex[1]
      eV = V[t] - V[t-1] - alpha - beta*V[t-1]
      eY = Y[t] - Z[t]*m - X[t]
      H = solve( 1 /((1 - rho^2)*s2V*V[t-1]) + rhoJ^2/s2J )
      h = H * ((eV-rho*sqrt(s2V)*eY)/((1 - rho^2)*s2V*V[t-1]) + rhoJ*(X[t] - mJ)/s2J - 1/mV)
      if (h+5*sqrt(H) > 0) {
        XV[t] = truncnorm::rtruncnorm(n = 1, mean = h, sd = sqrt(H), a=0, b=h+5*sqrt(H))
      } else { 
        XV[t] = 0
      }
      if (is.infinite(XV[t]) |is.nan(XV[t])) {
        XV[t] = 0
      }
    }
    if (length(Jindex) > 1) {
      for (t in Jindex[2:length(Jindex)]) {
        eV = V[t] - V[t-1] - alpha - beta*V[t-1]
        eY = Y[t] - Z[t]*m - X[t]
        H = solve( 1 /((1 - rho^2)*s2V*V[t-1]) + rhoJ^2/s2J )
        h = H * ((eV-rho*sqrt(s2V)*eY)/((1 - rho^2)*s2V*V[t-1]) + rhoJ*(X[t] - mJ)/s2J - 1/mV)
        if (h+5*sqrt(H) > 0) { 
          XV[t] = truncnorm::rtruncnorm(n = 1, mean = h, sd = sqrt(H), a=0, b=h+5*sqrt(H))
        } else { 
          XV[t] = 0
        }
        if (is.infinite(XV[t]) | is.nan(XV[t])) {
          XV[t] = 0
        }
      }
    }
  }
  
  if (i > n) {
    XVsum = XVsum + XV
  }
  
  
  # X
  X[as.logical(!J)] = rnorm(mJ + rhoJ*XV[as.logical(!J)], sd = sqrt(s2J))
  if (length(Jindex) != 0){
    if (Jindex[1] == 1) {
      t = 1
      eV = V[1] - V0 - alpha - beta*V0 - XV[1]
      eY = Y[1] - Z[1]*m
      L = solve(1/((1 - rho^2)*V0) + 1/s2J)
      l = L * ( (eY - (rho/sqrt(s2V))*eV)/((1 - rho^2)*V0) + (mJ + rhoJ*XV[1])/s2J )
      X[1] = rnorm(n=1,l,sqrt(L))
    } else {
      t = Jindex[1]
      eV = V[t] - V[t-1] - alpha - beta*V[t-1] - XV[t]
      eY = Y[t] - Z[t]*m
      L = solve(1/((1 - rho^2)*V[t-1]) + 1/s2J)
      l = L * ( (eY - (rho/sqrt(s2V))*eV)/((1 - rho^2)*V[t-1]) + (mJ + rhoJ*XV[t])/s2J )
      X[t] = rnorm(n=1,l,sqrt(L))
    }
    if (length(Jindex) > 1) {
      for (t in Jindex[2:length(Jindex)]) {
        eV = V[t] - V[t-1] - alpha - beta*V[t-1] - XV[t]
        eY = Y[t] - Z[t]*m
        L = solve(1/((1 - rho^2)*V[t-1]) + 1/s2J)
        l = L * ( (eY - (rho/sqrt(s2V))*eV)/((1 - rho^2)*V[t-1]) + (mJ + rhoJ*XV[t])/s2J )
        X[t] = rnorm(n=1,l,sqrt(L))
      }
    }
  }
  if (i > n) {
    Xsum = Xsum + X
  }
  
  
  # Draw V
  epsilon = rt(n=length(Y), dfV) 
  mv = 0 # mean of t distribution with parameter dfV
  v = dfV/(dfV-2) # # variance of t distribution with parameter dfV
  epsilon = (stdevV/sqrt(v)) * epsilon
  if (i == floor(n / 2)){
    Vindex1 = which(Vsum2 > quantile(Vsum2, 0.925))
    Vindex2 = which(Vsum2 > quantile(Vsum2, 0.75) & Vsum2 < quantile(Vsum2, 0.925))
    Vindex3 = which(Vsum2 < quantile(Vsum2, 0.25) & Vsum2 > quantile(Vsum2, 0.025))
    Vindex4 = which(Vsum2 < quantile(Vsum2, 0.025))
  }
  if (i > floor(n / 2) - 1) {
    epsilon[Vindex1] = 1.35 * epsilon[Vindex1]
    epsilon[Vindex2] = 1.25 * epsilon[Vindex2]
    epsilon[Vindex3] = 0.75 * epsilon[Vindex3]
    epsilon[Vindex4] = 0.65 * epsilon[Vindex4]
  }
  j = 1
  Vprop = V + epsilon
  p1 = max(0,exp( -0.5 * ( ( Y[j+1] - Z[j+1]*m[1]  - J[j+1]*X[j+1] - rho / s2V^0.5 *(V[j+1] - Vprop[j] - alpha - Vprop[j] * beta - J[j+1]*XV[j+1] ) )^2/( (1 - rho^2) * Vprop[j] ) +
                             ( Y[j] - Z[j]*m[1]  - J[j]*X[j] - rho / s2V^0.5 *(Vprop[j] - V0 - alpha - V0 * beta - J[j]*XV[j]))^2/( (1 - rho^2) * V0 ) +
                             ( V[j+1] - Vprop[j] - alpha - Vprop[j] * beta - J[j+1]*XV[j+1] )^2/( s2V * Vprop[j] ) +
                             ( Vprop[j] - V0 - alpha - V0 * beta - J[j]*XV[j])^2/( s2V * V0 ) ) ) / Vprop[j])
  p2 = max(0,exp( -0.5 * ( ( Y[j+1] - Z[j+1]*m[1]  - J[j+1]*X[j+1] - rho / s2V^0.5 *(V[j+1] - V[j] - alpha - V[j] * beta - J[j+1]*XV[j+1] ) )^2/( (1 - rho^2) * V[j] ) +
                             ( Y[j] - Z[j]*m[1]  - J[j]*X[j] -rho / s2V^0.5 *(V[j] - V0 - alpha - V0 * beta - J[j]*XV[j]) )^2/( (1 - rho^2) * V0 ) +
                             ( V[j+1] - V[j] - alpha - V[j] * beta - J[j+1]*XV[j+1])^2/( s2V * V[j] ) +
                             ( V[j] - V0 - alpha - V0 * beta - J[j]*XV[j])^2/( s2V * V0 ) ) ) / V[j])
  if (p2 != 0) {
    acceptV = min(p1/p2, 1)
  } else if( p1 > 0){ 
    acceptV = 1 
  } else { 
    acceptV = 0
  }
  
  u = runif(length(Y))
  if (u[j] < acceptV) {
    V[j] = Vprop[j]
    if (i > n) {
      acceptsumV[j] = acceptsumV[j] + 1
    }
  }
  
  for (j in 2:length(Y)-1) {
    p1 = max(0,exp( -0.5 * ( ( Y[j+1] - Z[j+1]*m[1]  - J[j+1]*X[j+1] - rho / s2V^0.5 *(V[j+1] - Vprop[j] - alpha - Vprop[j] * beta - J[j+1]*XV[j+1]) )^2/( (1 - rho^2) * Vprop[j] ) +
                               ( Y[j] - Z[j]*m[1]  - J[j]*X[j] - rho / s2V^0.5 *(Vprop[j] - V[j-1] - alpha - V[j-1] * beta - J[j]*XV[j] ) )^2/( (1 - rho^2) * V[j-1] ) +
                               ( V[j+1] - Vprop[j] - alpha - Vprop[j] * beta - J[j+1]*XV[j+1])^2/( s2V * Vprop[j] ) +
                               ( Vprop[j] - V[j-1] - alpha - V[j-1] * beta - J[j]*XV[j])^2/( s2V * V[j-1] ) ) ) / Vprop[j])
    p2 = max(0,exp( -0.5 * ( ( Y[j+1] - Z[j+1]*m[1]  - J[j+1]*X[j+1] - rho / s2V^0.5 *(V[j+1] - V[j] - alpha - V[j] * beta - J[j+1]*XV[j+1]) )^2/( (1 - rho^2) * V[j] ) +
                               ( Y[j] - Z[j]*m[1] - J[j]*X[j] - rho / s2V^0.5 *(V[j] - V[j-1] - alpha - J[j]*XV[j] - V[j-1] * beta ) )^2/( (1 - rho^2) * V[j-1] ) +
                               ( V[j+1] - V[j] - alpha - V[j] * beta - J[j+1]*XV[j+1])^2/( s2V * V[j] ) +
                               ( V[j] - V[j-1] - alpha - V[j-1] * beta - J[j]*XV[j])^2/( s2V * V[j-1] ) ) ) / V[j])
    if (p2 != 0){
      acceptV = min(p1/p2, 1)
    } else if (p1 > 0){
      acceptV = 1 
    } else {
      acceptV = 0
    }
    
    if (u[j] < acceptV) { 
      V[j] = Vprop[j]
      if (i > n) {
        acceptsumV[j] = acceptsumV[j] + 1
      }
    }
  }
  
  j = length(Y)
  p1 = max(0,exp( -0.5 * ( ( Y[j] - Z[j]*m[1]  - J[j]*X[j] - rho / s2V^0.5 *(Vprop[j] - V[j-1] - alpha - V[j-1] * beta - J[j]*XV[j]) )^2/( (1 - rho^2) * V[j-1] ) +
                             ( Vprop[j] - V[j-1] - alpha - V[j-1] * beta - J[j]*XV[j])^2/( s2V * V[j-1] ) ) ) / Vprop[j]^0.5)
  p2 = max(0,exp( -0.5 * ( ( Y[j] - Z[j]*m[1]  - J[j]*X[j] - rho / s2V^0.5 *(V[j] - V[j-1] - alpha - V[j-1] * beta - J[j]*XV[j]) )^2/( (1 - rho^2) * V[j-1] ) +
                             ( V[j] - V[j-1] - alpha - V[j-1] * beta  - J[j]*XV[j])^2/( s2V * V[j-1] ) ) ) / V[j]^0.5)
  
  if (ifelse(is.nan(p2),TRUE,p2 != 0)) {
    acceptV = ifelse(is.nan(min(p1/p2, 1)),0,min(p1/p2, 1))
  } else if (p1 > 0){
    acceptV = 1
  } else {
    acceptV = 0
  }
  
  if (u[j] < acceptV) {
    V[j] = Vprop[j]
    if (i > n) {
      acceptsumV[j] = acceptsumV[j] + 1
    }
  }
  
  if (i > n) {
    Vsum = Vsum + V
  }
  if (i > floor(n / 2) - 100 | i < floor(n / 2)) { 
    Vsum2 = Vsum2 + V
  }
  
  test[i,] = c(m, mJ, s2J, lambda, alpha, beta,  rho, s2V, rhoJ, mV)
  
}

parameters <- data.frame(parameter = as.character(c("mu", "mu_y", "sigma_y", "lambda", "alpha", "beta", "rho", "sigma_v", "rho_j", "mu_v")), 
                               mean = round(c(msum/(N-n), mJsum/(N-n), s2Jsum/(N-n),lambdasum/(N-n),alphasum/(N-n),betasum/(N-n),rhosum/(N-n),s2Vsum/(N-n),rhoJsum/(N-n),mVsum/(N-n)),3),
                               sd = round(c((m2sum/(N-n)-(msum/(N-n))^2)^0.5,
                                      (mJ2sum/(N-n)-(mJsum/(N-n))^2)^0.5,
                                      (s2J2sum/(N-n)-(s2Jsum/(N-n))^2)^0.5,
                                      (lambda2sum/(N-n)-(lambdasum/(N-n))^2)^0.5,
                                      (alpha2sum/(N-n)-(alphasum/(N-n))^2)^0.5,
                                      (beta2sum/(N-n)-(betasum/(N-n))^2)^0.5,
                                      (rho2sum/(N-n)-(rhosum/(N-n))^2)^0.5,
                                      (s2V2sum/(N-n)-(s2Vsum/(N-n))^2)^0.5,
                                      (rhoJ2sum/(N-n)-(rhoJsum/(N-n))^2)^0.5,
                                      (mV2sum/(N-n)-(mVsum/(N-n))^2)^0.5),3)
                         )

test <- as.data.frame(test)
colnames(test) <- c("mu", "mu_y", "sigma_y", "lambda", "alpha", "beta", "rho", "sigma_v", "rho_j", "mu_v")

                               
jump_vol = XVsum/(N-n)*round(Jsum/(N-n))
jump_price = Xsum/(N-n)*round(Jsum/(N-n))
vol = Vsum/(N-n)
sig = vol^0.5
resid = (Y[2:length(Y)] - msum/(N-n) - jump_price[2:length(jump_price)])/sig[1:length(sig)-1]

svcj_results <- setNames(list(parameters, test, jump_vol, jump_price, vol, resid), 
                         c("parameters", "param_evolution" ,"jumps_volatility", "jumps_price", "volatility", "residuals"))
return(svcj_results)
 
print(svcj_results$parameters)

}
