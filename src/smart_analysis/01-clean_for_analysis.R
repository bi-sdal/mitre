library(here)
library(purrr)

source('./R/smart_analysis.R')

#smart_coefficients <- readRDS(here('data/mitre/final/logistic_regressions/coefficients.RDS'))
#smart_deviances <- readRDS(here('data/mitre/final/logistic_regressions/deviance.RDS'))

y_smart_fits <- readRDS(here('data/mitre/final/logistic_regressions/fit_smart.RDS'))
n_smart_fits <- readRDS(here('data/mitre/final/logistic_regressions/fit_notSmart.RDS'))

#fit0 <- readRDS(here('./data/mitre/working/logistic_regressions/example_fit0.RDS'))
#fit1 <- readRDS(here('./data/mitre/working/logistic_regressions/example_fit1.RDS'))

# make sure all the coefficients are in the same order
coef_names_smart <- lapply(y_smart_fits, function(x) names(x$coefficients))
stopifnot(all(unlist(lapply(coef_names_smart, all.equal, coef_names_smart[[1]]))))

coef_names_notSmart <- lapply(n_smart_fits, function(x) names(x$coefficients))
stopifnot(all(unlist(lapply(coef_names_notSmart, all.equal, coef_names_notSmart[[1]]))))

smart_dat    <- purrr::map2(y_smart_fits, 1:length(y_smart_fits), .GlobalEnv$coefs_sim_num)
basel_dat <- purrr::map2(n_smart_fits, 1:length(n_smart_fits), .GlobalEnv$coefs_sim_num)


## scale data -----

smart_var_sd <- lapply(y_smart_fits, function(x){lapply(x$data, sd)})
basel_var_sd <- lapply(n_smart_fits, function(x){lapply(x$data, sd)})

smart_dat <- purrr::map2_df(smart_dat, smart_var_sd, .GlobalEnv$scale_vars_all)
basel_dat <- purrr::map2_df(basel_dat, basel_var_sd, .GlobalEnv$scale_vars_all)

smart_dat %>% head(11)
basel_dat %>% head(5)

saveRDS(smart_dat, './data/mitre/final/logistic_regressions/smart_data_coefs.RDS')
saveRDS(basel_dat, './data/mitre/final/logistic_regressions/basel_data_coefs.RDS')
