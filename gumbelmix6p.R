gumbelmix6p_stanvars <- "
   real gumbelmin(real x, real mu, real disc){
     //return 1- exp(-exp(-(-x-mu)/disc));
     //return exp(gumbel_lccdf(-x | mu,disc));
     return 1 - gumbel_cdf(-x|mu,disc);
   }
   real gumbelmix6p_lpmf(int y, real mu, 
                   real crc, real crlm, real crll, real crhm, real crhh, 
                   real mix,
                   int itemtype) {
     int nthres = 5;
     real p;
     real p2;
     real disc = 1;
     vector[5] thres;
     if (y == 1) {
       thres[1] = crc - (exp(crlm) + exp(crll));
       p = gumbelmin(thres[1], mu, disc);
     } else if (y == nthres + 1) {
     thres[nthres] = crc + (exp(crhm) + exp(crhh));
       p = 1 - gumbelmin(thres[nthres], mu, disc);
     } else {
       if (y == 2) {
         thres[1] = crc - (exp(crlm) + exp(crll));
         thres[2] = crc - (exp(crlm));
       } else if (y == 3) {
         thres[2] = crc - (exp(crlm));
         thres[3] = crc;
       } else if (y == 4) {
         thres[3] = crc;
         thres[4] = crc + (exp(crhm));
       } else if (y == 5) {
         thres[4] = crc + (exp(crhm));
         thres[5] = crc + (exp(crhm) + exp(crhh));
       }
       p = gumbelmin(thres[y], mu, disc) - gumbelmin(thres[y-1], mu, disc);
     }
     if (itemtype == 1) {
     if (y == 1) {
         p2 = gumbelmin(thres[1], 0, disc);
       } else if (y == nthres + 1) {
         p2 = 1 - gumbelmin(thres[nthres], 0, disc);
       } else {
         p2 = gumbelmin(thres[y], 0, disc) - gumbelmin(thres[y-1], 0, disc);
       }
       return(log_mix(mix, log(p), log(p2)));
     } else {
       return log(p);
     }
     
   }
"

gumbelmix6p_family <- custom_family(
  name = "gumbelmix6p", 
  dpars = c("mu", "crc", "crlm", "crll", "crhm", "crhh", "mix"), 
  links = c("identity", rep("identity", 5), "probit"), 
  lb = c(NA, rep(NA, 5), 0), ub = c(rep(NA, 6), 1),
  type = "int", vars = "vint1[n]"
)
sv_gumbelmix6p <- stanvar(scode = gumbelmix6p_stanvars, block = "functions")

log_lik_gumbelmix6p <- function(i, prep) {
  mu <- brms::get_dpar(prep, "mu", i = i)
  #discsignal <- brms::get_dpar(prep, "discsignal", i = i)
  crc <- brms::get_dpar(prep, "crc", i = i)
  crlm <- brms::get_dpar(prep, "crlm", i = i)
  crll <- brms::get_dpar(prep, "crll", i = i)
  crhm <- brms::get_dpar(prep, "crhm", i = i)
  crhh <- brms::get_dpar(prep, "crhh", i = i)
  mix <- brms::get_dpar(prep, "mix", i = i)
  
  itemtype <- prep$data$vint1[i]
  y <- prep$data$Y[i]
  gumbelmix6p_lpmf(y, mu, crc, crlm, crll, crhm, crhh, mix, itemtype)
}
posterior_predict_gumbelmix6p <- function(i, prep, ...) {
  mu <- brms::get_dpar(prep, "mu", i = i)
  #discsignal <- brms::get_dpar(prep, "discsignal", i = i)
  crc <- brms::get_dpar(prep, "crc", i = i)
  crlm <- brms::get_dpar(prep, "crlm", i = i)
  crll <- brms::get_dpar(prep, "crll", i = i)
  crhm <- brms::get_dpar(prep, "crhm", i = i)
  crhh <- brms::get_dpar(prep, "crhh", i = i)
  mix <- brms::get_dpar(prep, "mix", i = i)
  itemtype <- prep$data$vint1[i]
  nsamples <- length(mu)
  nthres <- 5
  thres <- matrix(NA_real_, nrow = nsamples, ncol = nthres)
  p <- matrix(NA_real_, nrow = nsamples, ncol = nthres+1)
  p2 <- matrix(NA_real_, nrow = nsamples, ncol = nthres+1)
  disc = rep(1, nsamples)
  thres[,1] = crc - (exp(crlm) + exp(crll));
  thres[,2] = crc - (exp(crlm));
  thres[,3] = crc;
  thres[,4] = crc + (exp(crhm));
  thres[,5] = crc + (exp(crhm) + exp(crhh));
  for (y in 1:(nthres+1)) {
    if (y == 1) {
      p[,y] = ordinal::pgumbel(thres[,1], mu, disc, max = FALSE)
    } else if (y == nthres + 1) {
      p[,y] = 1 - ordinal::pgumbel(thres[,nthres], mu, disc, max = FALSE)
    } else {
      p[,y] = ordinal::pgumbel(thres[,y], mu, disc, max = FALSE) -
          ordinal::pgumbel(thres[,y-1], mu, disc, max = FALSE)
    }
  }
  if (itemtype == 1) {
     for (y in 1:(nthres+1)) {
       if (y == 1) {
         p2[,y] = ordinal::pgumbel(thres[,1], 0, disc, max = FALSE)
       } else if (y == nthres + 1) {
         p2[,y] = 1 - ordinal::pgumbel(thres[,nthres], 0, disc, max = FALSE)
       } else {
         p2[,y] = ordinal::pgumbel(thres[,y], 0, disc, max = FALSE) -
           ordinal::pgumbel(thres[,y-1], 0, disc, max = FALSE)
       }
     }
    out <- mix * p + (1-mix) * p2
    return(apply(out, 1, extraDistr::rcat, n = 1))
  } else {
    return(apply(p, 1, extraDistr::rcat, n = 1))
  }
  
}