uvsdt6p_stanvars <- "
   real uvsdt6p_lpmf(int y, real mu, real discsignal, 
                   real crc, real crlm, real crll, real crhm, real crhh, 
                   int itemtype) {
     int nthres = 5;
     real p;
     real disc;
     vector[5] thres;
     if (itemtype == 0) {
       disc = 1;
     } else {
       disc = discsignal;
     }
     if (y == 1) {
       thres[1] = crc - (exp(crlm) + exp(crll));
       p = Phi(disc * (thres[1] - mu));
     } else if (y == nthres + 1) {
     thres[nthres] = crc + (exp(crhm) + exp(crhh));
       p = 1 - Phi(disc * (thres[nthres] - mu));
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
       p = Phi(disc * (thres[y] - mu)) -
           Phi(disc * (thres[y - 1] - mu));
     }
     return log(p);
   }
"

uvsdt6p_family <- custom_family(
  name = "uvsdt6p", 
  dpars = c("mu", "discsignal", "crc", "crlm", "crll", "crhm", "crhh"), 
  links = c("identity", "log", rep("identity", 5)), lb = c(NA, 0, rep(NA, 5)),
  type = "int", vars = "vint1[n]"
)
stanvars <- stanvar(scode = uvsdt6p_stanvars, block = "functions")

log_lik_uvsdt6p <- function(i, prep) {
  mu <- brms::get_dpar(prep, "mu", i = i)
  discsignal <- brms::get_dpar(prep, "discsignal", i = i)
  crc <- brms::get_dpar(prep, "crc", i = i)
  crlm <- brms::get_dpar(prep, "crlm", i = i)
  crll <- brms::get_dpar(prep, "crll", i = i)
  crhm <- brms::get_dpar(prep, "crhm", i = i)
  crhh <- brms::get_dpar(prep, "crhh", i = i)
  
  itemtype <- prep$data$vint1[i]
  y <- prep$data$Y[i]
  uvsdt6p_lpmf(y, mu, discsignal, crc, crlm, crll, crhm, crhh, itemtype)
}
posterior_predict_uvsdt6p <- function(i, prep, ...) {
  mu <- brms::get_dpar(prep, "mu", i = i)
  discsignal <- brms::get_dpar(prep, "discsignal", i = i)
  crc <- brms::get_dpar(prep, "crc", i = i)
  crlm <- brms::get_dpar(prep, "crlm", i = i)
  crll <- brms::get_dpar(prep, "crll", i = i)
  crhm <- brms::get_dpar(prep, "crhm", i = i)
  crhh <- brms::get_dpar(prep, "crhh", i = i)
  itemtype <- prep$data$vint1[i]
  nsamples <- length(mu)
  nthres <- 5
  thres <- matrix(NA_real_, nrow = nsamples, ncol = nthres)
  p <- matrix(NA_real_, nrow = nsamples, ncol = nthres+1) 
  if (itemtype == 0) {
    disc = rep(1, nsamples)
  } else {
    disc = discsignal
  }
  thres[,1] = crc - (exp(crlm) + exp(crll));
  thres[,2] = crc - (exp(crlm));
  thres[,3] = crc;
  thres[,4] = crc + (exp(crhm));
  thres[,5] = crc + (exp(crhm) + exp(crhh));
  for (y in 1:(nthres+1)) {
    if (y == 1) {
      p[,y] = pnorm(disc * (thres[,1] - mu));
    } else if (y == nthres + 1) {
      p[,y] = 1 - pnorm(disc * (thres[,nthres] - mu));
    } else {
      p[,y] = pnorm(disc * (thres[,y] - mu)) -
        pnorm(disc * (thres[,y - 1] - mu));
    }
  }
  apply(p, 1, extraDistr::rcat, n = 1)
}