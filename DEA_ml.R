# DEA ML

remove(list = objects())
setwd('~/research/africa/DEA_Stunting')
library(tidyverse)
library(glmnet)
library(sandwich)
library(neuralnet)


df <- read.csv('regdata2.csv')

# df <- df %>% mutate(jan = ifelse(month == 1, 1, 0),
#                     feb = ifelse(month == 2, 1, 0),
#                     mar = ifelse(month == 3, 1, 0),
#                     apr = ifelse(month == 4, 1, 0),
#                     jun = ifelse(month == 6, 1, 0),
#                     jul = ifelse(month == 7, 1, 0),
#                     aug = ifelse(month == 8, 1, 0),
#                     sep = ifelse(month == 9, 1, 0),
#                     oct = ifelse(month == 10, 1, 0),
#                     nov = ifelse(month == 11, 1, 0),
#                     dec = ifelse(month == 12, 1, 0),
#                     colledu = ifelse(maxedu == 4, 1, 0),
#                     otheredu = ifelse(maxedu == 5, 1, 0),
#                     secsch = ifelse(maxedu == 3, 1, 0),
#                     primsch = ifelse(fac_maxedu == 'PrimSch', 1, 0),
#                     ind_latrine = ifelse(fac_latrine == 'Indoor', 1, 0),
#                     out_latrine = ifelse(fac_latrine == 'Outdoor', 1, 0),
#                     dlatrine = ifelse(latrine == 4, 0, 1),
#                     no_latrine = ifelse(fac_latrine == 'None', 1, 0),
#                     quadhhm = totalhhmembers^2,
#                     dgen = ifelse(gen == 'M', 1, 0),
#                     dchil_u5 = ifelse(chunder5 > 0, 1, 0),
#                     dchil_o5 = ifelse(malesunder5 + fem5to10 > 0, 1, 0),
#                     w_comdamdeprw = ifelse(fac_watersource == 'ComDamDepRW', 1, 0),
#                     w_lake = ifelse(fac_watersource == 'Lake', 1, 0),
#                     w_privbore = ifelse(fac_watersource == 'PrivBorehole', 1, 0),
#                     w_tap = ifelse(watersource == 5, 1, 0),
#                     w_rwseasstr = ifelse(fac_watersource == 'RWSeasStr', 1, 0),
#                     w_run = ifelse(df$watersource == 1 | df$watersource == 2 |
#                                      df$watersource == 3 | df$watersource ==4 |
#                                      df$watersource == 7, 1, 0),
#                     livinc_l = ifelse(totallivinc < liv_inc_l, 1, 0),
#                     livinc_m = ifelse(totallivinc > liv_inc_l &
#                                         totallivinc < liv_inc_m, 1, 0),
#                     offinc_l = ifelse(offfarmnetinc < off_inc_l, 1, 0),
#                     offinc_m = ifelse(offfarmnetinc > off_inc_l &
#                                         offfarmnetinc < off_inc_m, 1, 0))


# interactions
df <- df %>% mutate(d_chover5 = ifelse(males5to10 + femunder5 > 0, 1, 0),
                    acre_livincl = totalacres*d_livinc_l,
                    acre_livincm = totalacres*d_livinc_m,
                    acre_offincl = totalacres*d_offinc_l,
                    acre_offincm = totalacres*d_offinc_m,
                    acre_hhmem = totalacres*totalhhmembers,
                    acre_hhmem2 = totalacres*quadhhm,
                    hhmem_livincl = totalhhmembers*d_livinc_l,
                    hhmem_livincm = totalhhmembers*d_livinc_m,
                    hhmem_offincl = totalhhmembers*d_offinc_l,
                    hhmem_offincm = totalhhmembers*d_offinc_m,
                    dchu5_livincl = d_chunder5*d_livinc_l,
                    dchu5_livincm = d_chunder5*d_livinc_m,
                    dcho5_livincl = d_chover5*d_livinc_l,
                    dcho5_livincm = d_chover5*d_livinc_m,
                    dchu5_offincl = d_chunder5*d_offinc_l,
                    dchu5_offincm = d_chunder5*d_offinc_m,
                    dcho5_offincl = d_chover5*d_offinc_l,
                    dcho5_offincm = d_chover5*d_offinc_m,
                    chu5_livincl = chunder5*d_livinc_l,
                    chu5_livincm = chunder5*d_livinc_m,
                    cho5_livincl = ch5to10*d_livinc_l,
                    cho5_livincm = ch5to10*d_livinc_m,
                    chu5_offincl = chunder5*d_offinc_l,
                    chu5_offincm = chunder5*d_offinc_m,
                    cho5_offincl = ch5to10*d_offinc_l,
                    cho5_offincm = ch5to10*d_offinc_m)

# months
df <- df %>% mutate(jan = ifelse(month == 1, 1, 0),
                    feb = ifelse(month == 2, 1, 0),
                    mar = ifelse(month == 3, 1, 0),
                    apr = ifelse(month == 4, 1, 0),
                    jun = ifelse(month == 6, 1, 0),
                    jul = ifelse(month == 7, 1, 0),
                    aug = ifelse(month == 8, 1, 0),
                    sep = ifelse(month == 9, 1, 0),
                    oct = ifelse(month == 10, 1, 0),
                    nov = ifelse(month == 11, 1, 0),
                    dec = ifelse(month == 12, 1, 0))

fe_modvars <- c('eff', 'totalhhmembers', 'quadhhm', 'd_gen', 'totalacres', 'd_chunder5',
                'd_livinc_l', 'd_livinc_m', 'd_offinc_l', 'd_offinc_m', 'electricity',
                'sec_sch', 'coll_edu', 'other_edu', 'd_tapwtr', 'd_runwtr', 'd_latrine',
                'm_n_', 'acre_offincm')


mod_vars <- c('eff', 'totalhhmembers', 'quadhhm', 'd_gen', 'totalacres', 'd_chunder5',
              'd_livinc_l', 'd_livinc_m', 'd_offinc_l', 'd_offinc_m', 'electricity',
              'sec_sch', 'coll_edu', 'other_edu', 'd_tapwtr', 'd_runwtr', 'd_latrine',
              'jan', 'feb', 'mar', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov',
              'dec', 'acre_livincl', 'acre_livincm', 'acre_offincl', 'acre_offincm',
              'acre_hhmem', 'acre_hhmem2', 'hhmem_livincl', 'hhmem_livincm', 'hhmem_offincl',
              'hhmem_offincm', 'chu5_livincl', 'chu5_livincm', 'cho5_livincl',
              'cho5_livincm', 'chu5_offincl', 'chu5_offincm', 'cho5_offincl',
              'cho5_offincm')


# 'dchu5_livincl', 'dchu5_livincm', 'dcho5_livincl',
# 'dcho5_livincm', 'dchu5_offincl', 'dchu5_offincm', 'dcho5_offincl',
# 'dcho5_offincm'

# FE summary
fe_mod_df <- df[, fe_modvars]
fe_mod <- lm(eff ~ ., data = fe_mod_df)
fe_mod_pred <- sum((df$eff -
                      predict(fe_mod,
                              fe_mod_df[!names(fe_mod_df) %in% 'eff']))^2)/
  nrow(fe_mod_df)

# Lasso eval
mod_df <- df[, mod_vars]
X <- mod_df %>% select(-'eff') %>% as.matrix() 
y <- mod_df$eff

lambda_seq <- 10^seq(3, -3, length.out = 500)
cv_out <- cv.glmnet(X, y, alpha = 1, lambda = lambda_seq)
cv_lambda <- cv_out$lambda.min 
# saving cross validation lambda value
cv_lambda <- 0.003573602

lasso_mod <- glmnet(X, y, lambda = cv_lambda)
lasso_coef <- coef(lasso_mod)

idx_lasscoef <- which(lasso_coef != 0)
lasso_vars <- row.names(lasso_coef)[idx_lasscoef[idx_lasscoef > 1]]
b_vars <- c('d_gen', 'd_chunder5', 'electricity', 'sec_sch', 'coll_edu', 'other_edu',
            'jan', 'feb', 'mar', 'jul', 'sep', 'oct', 'nov', 'dec')

lasso_mod_df <- cbind(df['eff'], 1, df[lasso_vars])
colnames(lasso_mod_df)[2] <- 'int'
lasso_mod <- lm(eff ~ . -1, data = lasso_mod_df)
lasso_hacse <- sqrt(diag(vcovHAC(lasso_mod))) 
lasso_bt <- coef(lasso_mod) / lasso_hacse
lasso_p <- 2*(1-pt(abs(lasso_bt), nrow(lasso_mod_df) - ncol(lasso_mod_df) - 1))

lasso_mod_coefs <- cbind(summary(lasso_mod)$coefficients, lasso_hacse, lasso_p) %>%
  data.frame() %>% 
  select(c('Estimate', 'lasso_hacse', 'lasso_p')) %>%
  round(4)

lasso_mod_pred <- sum((df$eff -
                         predict(lasso_mod,
                                 lasso_mod_df[!names(lasso_mod_df) %in% 'eff']))^2)/
  nrow(lasso_mod_df)

# neural network for pred
nn <- neuralnet(eff ~ ., hidden = c(10), data = lasso_mod_df)
X <- lasso_mod_df %>% select(-'eff')
nn_mod_pred <- sum((df$eff - predict(nn, X))^2)/nrow(lasso_mod_df)

# neural network counterfactual 
muX <- colMeans(X) %>% as.matrix()
muX[which(rownames(muX) %in% b_vars)] <- 0

nn_mean_pred <- predict(nn, t(muX))
mean_eff <- mean(df$eff)

cfX <- function(var, val) {
  
  X <- muX
  X[which(rownames(X) %in% var)] <- val
  
  return(t(X))
  
}


# having no formal education
# low off farm income
# increasing total acres by 1 sd
cf_acre_offincl <- muX['acre_offincl', 1] + sd(df$totalacres)
# increasing acre effect on hhmem
cf_acre_quadhhm <- muX['acre_hhmem2', 1] + sd(df$totalacres)
(predict(nn, cfX(c('other_edu', 'acre_offincl', 'acre_hhmem2'),
                 c(1, cf_acre_offincl, cf_acre_quadhhm))) - nn_mean_pred)/nn_mean_pred


# impact of having primary school edu
# medium off farm inc
# decreasing acres held by 1 sd
cf_acre_offincm <- muX['acre_offincm', 1] - sd(df$totalacres)
cf_acre_quadhhm <- muX['acre_hhmem2', 1] - sd(df$totalacres)
(predict(nn, cfX(c('electricity', 'other_edu', 'acre_offincm', 'acre_hhmem2'),
                 c(0, 0, cf_acre_offincm, cf_acre_quadhhm))) - nn_mean_pred)/nn_mean_pred





