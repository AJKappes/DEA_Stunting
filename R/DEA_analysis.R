library(ggplot2)
library(GGally)
library(dplyr)
library(lme4)
library(lmtest)
library(car)


agg_data <- read.csv('reg_data_r.csv')

fac_cols <- c('maxedu', 'watersource', 'month', 'latrine', 'fac_latrine')
agg_data[fac_cols] <- lapply(agg_data[fac_cols], factor)
agg_data <- mutate(agg_data, quadhhm = totalhhmembers^2)

#### fixed effect model construction ####

baseline_mod <- lm(eff ~ totalhhmembers + quadhhm + chunder5 + ch5to10 + totallivinc +
                  offfarmnetinc + electricity + totalacres + maxedu + watersource + month, 
                  data = agg_data)
summary(baseline_mod)                     

agg_data <- mutate(agg_data, 
                   lntotallivinc = log(totallivinc),
                   lnofffarmnetinc = log(offfarmnetinc))

agg_data$lntotallivinc <- ifelse(agg_data$lntotallivinc == -Inf, 1e-10, agg_data$lntotallivinc)
agg_data$lnofffarmnetinc <- ifelse(agg_data$lnofffarmnetinc == -Inf, 1e-10, agg_data$lnofffarmnetinc)

mod2 <- lm(eff ~ totalhhmembers + quadhhm + chunder5 + ch5to10 + lntotallivinc +
           lnofffarmnetinc + electricity + totalacres + maxedu + watersource + month, 
           data = agg_data)
summary(mod2)

resmod2 <- data.frame(residuals(mod2))

resmod2_hist <- ggplot() +
  geom_histogram(aes(resmod2), bins = 10) +
  labs(title = "Residual Histogram", x = "Residuals", y = "Frequency") +
  theme(plot.title = element_text(face = 'bold', size = '15', hjust = .5))





#### random effects model construnction ####
varplot <- agg_data[c('chunder5', 'lntotallivinc', 'lnofffarmnetinc', 'maxedu', 'month', 'watersource')]

pairs(varplot)

re_mod1 <- lmer(eff ~ totalhhmembers + quadhhm + chunder5 + ch5to10 + lntotallivinc +
                  lnofffarmnetinc + electricity + totalacres + maxedu + watersource +
                  (1 | month), data = agg_data)
summary(re_mod1)


#### LM & Hausman test statistic, testing for RE ####

D <- data.frame(model.matrix(mod2)) %>%
  select(electricity, maxedu3, maxedu4, maxedu5,
         watersource2, watersource3, watersource4, watersource5, watersource6, watersource7,
         month2, month3, month4, month6, month7, month8, month9, month10, month11, month12) %>%
  as.matrix()

e <- as.matrix(resmod2)
T <- 11
n <- nrow(agg_data)
d <- D%*%t(D)

LM <- n*T/(2*(T-1))*(t(e)%*%d%*%e/t(e)%*%e - 1)^2
LM > qchisq(.95, 1)

# hausman
coefficients(re_mod1)
bfe <- as.matrix(coefficients(mod2)[2:18])
Bre <- coefficients(re_mod1)$month[1, ] 
Bre <- Bre[, c(2:18)] 
Bre <- as.matrix(Bre) %>% t() 

Vfe <- vcov(mod2)[2:18, 2:18]
Vre <- vcov(re_mod1)[2:18, 2:18]

H <- t(bfe - Bre)%*%solve(Vfe - Vre)%*%(bfe - Bre)

k <- nrow(bfe)
H > qchisq(.95, k-1)


#### variable re-specification ####

agg_data <- mutate(agg_data, d_chunder5 = ifelse(agg_data$chunder5 > 0, 1, 0))

liv_inc <- sort(agg_data$totallivinc)
liv_inc <- data.frame(ifelse(liv_inc == 0, NA, liv_inc)) %>% na.omit()
rownames(liv_inc) <- NULL
idx_liv <- nrow(liv_inc)/3
liv_inc_l <- liv_inc[idx_liv, ] + 1
liv_inc_m <- liv_inc[idx_liv*2, ] + 1

off_inc <- sort(agg_data$offfarmnetinc)
off_inc <- data.frame(ifelse(off_inc == 0, NA, off_inc)) %>% na.omit()
rownames(off_inc) <- NULL
idx_off <- nrow(off_inc)/3
off_inc_l <- off_inc[idx_off, ] + 1
off_inc_m <- off_inc[idx_off*2, ] + 1

m_n_ <- matrix(0, nrow = nrow(agg_data), 1)

edu <- data.frame('sec_sch' = rep(0, nrow(agg_data)),
                  'coll_edu' = rep(0, nrow(agg_data)),
                  'other_edu' = rep(0, nrow(agg_data)))
                                   
for (i in 1:nrow(agg_data)){
  
  if (agg_data$month[i] == 6){
    m_n_[i,] = 'june'
  } else if (agg_data$month[i] == 7){
    m_n_[i,] = 'july'
  } else if (agg_data$month[i] == 8){
    m_n_[i,] = 'aug'
  } else if (agg_data$month[i] == 9){
    m_n_[i,] = 'sep'
  } else if (agg_data$month[i] == 10){
    m_n_[i,] = 'oct'
  } else if (agg_data$month[i] == 11){
    m_n_[i,] = 'nov'
  } else if (agg_data$month[i] == 12){
    m_n_[i,] = 'dec'
  } else if (agg_data$month[i] == 1){
    m_n_[i,] = 'jan'
  } else if (agg_data$month[i] == 2){
    m_n_[i,] = 'feb'
  } else if (agg_data$month[i] == 3){
    m_n_[i,] = 'mar'
  } else {
    m_n_[i,] = 'apr'
  } 
  
  if (agg_data$maxedu[i] == 3){
    edu[i, 'sec_sch'] = 1
  }
  if (agg_data$maxedu[i] == 4){
    edu[i, 'coll_edu'] = 1
  }
  if (agg_data$maxedu[i] == 5){
    edu[i, 'other_edu'] = 1
  }
}

agg_data <- cbind(agg_data, m_n_, edu)

agg_data <- mutate(agg_data,
                   d_livinc_l = ifelse(agg_data$totallivinc < liv_inc_l, 1, 0),
                   d_livinc_m = ifelse(agg_data$totallivinc > liv_inc_l & agg_data$totallivinc < liv_inc_m, 1, 0),
                   d_offinc_l = ifelse(agg_data$offfarmnetinc < off_inc_l, 1, 0),
                   d_offinc_m = ifelse(agg_data$offfarmnetinc > off_inc_l & agg_data$offfarmnetinc < off_inc_m, 1, 0),
                   d_tapwtr = ifelse(agg_data$watersource == 5, 1, 0),
                   d_runwtr = ifelse(agg_data$watersource == 1 | agg_data$watersource == 2 | agg_data$watersource == 3 |
                                     agg_data$watersource ==4 | agg_data$watersource == 7, 1, 0),
                   d_latrine = ifelse(agg_data$latrine == 4, 0, 1),
                   d_gen = ifelse(agg_data$gen == 'M', 1, 0),
                   d_highed = ifelse(agg_data$maxedu == 3 | agg_data$maxedu == 4, 1, 0))

# subset to include vaccination data, removing all observations with "no response"

agg_data$child5vacc[agg_data$child5vacc == 2] <- 0
idx <- which(agg_data$child5vacc == 3)
dat2 <- agg_data[-idx, ]

#### Descriptive stats ####
modvars <- c('Dep: DEA Efficiencies', 'totalhhmembers', 'quadhhm', 'd_gen', 'totalacres', 'd_chunder5',
             'd_livinc_l', 'd_livinc_m', 'd_offinc_l', 'd_offinc_m', 'electricity', 'sec_sch', 'coll_edu', 'other_edu',
             'd_tapwtr', 'd_runwtr', 'd_latrine', 'm_n_*abreviated month*', 'totalacres:d_offinc_m')

text_def <- c('efficiency measure of using protein, carbs, vegetables, corn, fruit, and fat food inputs for growth in height by month relative to other individual food input frequencies in the sample', 
              'total members in the household', 'total household members squared', 'binary, 1 if male, 0 if female', 'total household owned and rented acres',
              'binary, 1 if household has children under 5, 0 otherwise', 'binary, lower 3rd income earners from livestock (<= 501 Kenyan schillings)',
              'binary, middle 3rd income earners from livestock (501 < x <= 1001 Kenyan schillings)', 'binary, lower 3rd off-farm income earners (<= 9001 Kenyan schillings)',
              'binary, middle 3rd off-farm income earners (9001 < x <= 25201 Kenyan schillings)', 'binary, 1 if house has electricity, 0 otherwise',
              'binary, highest level of education in household is secondary school', 'binary, highest level of education in household is college graduate',
              'binary, highest level of education in household is other (no primary, secondary, or college education)',
              'binary, 1 if the household sources water from a public or private tap, 0 otherwise', 'binary, 1 if the household sources water from a running water source, 0 otherwise (3rd source is standing water, e.g. lake)',
              'binary, 1 if the household has access to a latrine (indoor or outdoor), 0 otherwise', 'categorical, seasonality effects based on month',
              'interaction term between household total acres and binary mid-level off-farm income earners')

attach(agg_data)
ones <- c('-', '-', '-', length(which(d_gen == 1)), '-', length(which(d_chunder5 == 1)), length(which(d_livinc_l == 1)),
          length(which(d_livinc_m == 1)), length(which(d_offinc_l == 1)), length(which(d_offinc_m == 1)), 
          length(which(electricity == 1)), length(which(sec_sch == 1)), length(which(coll_edu == 1)), length(which(other_edu == 1)),
          length(which(d_tapwtr == 1)), length(which(d_runwtr == 1)), length(which(d_latrine == 1)), '-', '-')

zeros <- c('-', '-', '-', length(which(d_gen == 0)), '-', length(which(d_chunder5 == 0)), length(which(d_livinc_l == 0)),
          length(which(d_livinc_m == 0)), length(which(d_offinc_l == 0)), length(which(d_offinc_m == 0)), 
          length(which(electricity == 0)), length(which(sec_sch == 0)), length(which(coll_edu == 0)), length(which(other_edu == 0)),
          length(which(d_tapwtr == 0)), length(which(d_runwtr == 0)), length(which(d_latrine == 0)), '-', '-')

mins <- c(round(min(eff), 3), min(totalhhmembers), '-', '-', min(totalacres), '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-',
         '-', min(totalacres * d_offinc_m))

maxs <- c(round(max(eff), 3), max(totalhhmembers), '-', '-', max(totalacres), '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-',
          '-', max(totalacres * d_offinc_m))

means <- c(round(mean(eff), 3), round(mean(totalhhmembers), 3), '-', '-', round(mean(totalacres), 3), '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-',
          '-', round(mean(totalacres * d_offinc_m), 3))

sds <- c(round(sd(eff), 3), round(sd(totalhhmembers), 3), '-', '-', round(sd(totalacres), 3), '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-', '-',
          '-', round(sd(totalacres * d_offinc_m), 3))
detach(agg_data)


variable_stats <- data.frame('Variables' = modvars,
                             'Definitions' = text_def,
                             'Ones' = ones,
                             'Zeros' = zeros,
                             'Min' = mins,
                             'Max' = maxs,
                             'Mean' = means,
                             'Std_Dev' = sds) 

# macronutrient intake frequency across months

freq <- read.csv('nutrient_freq.csv')

month <- c(6:12, 1:4)
month_list <- c('June', 'July', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr')

# protein

protein_stats <- data.frame('month' = month,
                            'min' = rep(0, length(month)),
                            'max' = rep(0, length(month)),
                            'protein_mean' = rep(0, length(month)),
                            'protein_sd' = rep(0, length(month)))

for (i in 1:nrow(protein_stats)){
  
  print(i)
  
  protein_stats[i, c('min')] <- min(freq[freq[, c('month')] == protein_stats$month[i], c('protein')])
  protein_stats[i, c('max')] <- max(freq[freq[, c('month')] == protein_stats$month[i], c('protein')])
  protein_stats[i, c('protein_mean')] <- mean(freq[freq[, c('month')] == protein_stats$month[i], c('protein')])
  protein_stats[i, c('protein_sd')] <- sd(freq[freq[, c('month')] == protein_stats$month[i], c('protein')])
  
}

# carbs 

carbs_stats <- data.frame('month' = month,
                          'min' = rep(0, length(month)),
                          'max' = rep(0, length(month)),
                          'carbs_mean' = rep(0, length(month)),
                          'carbs_sd' = rep(0, length(month)))

for (i in 1:nrow(carbs_stats)){
  
  print(i)
  
  carbs_stats[i, c('min')] <- min(freq[freq[, c('month')] == carbs_stats$month[i], c('carbs')])
  carbs_stats[i, c('max')] <- max(freq[freq[, c('month')] == carbs_stats$month[i], c('carbs')])
  carbs_stats[i, c('carbs_mean')] <- mean(freq[freq[, c('month')] == carbs_stats$month[i], c('carbs')])
  carbs_stats[i, c('carbs_sd')] <- sd(freq[freq[, c('month')] == carbs_stats$month[i], c('carbs')])
  
}

# veg

veg_stats <- data.frame('month' = month,
                        'min' = rep(0, length(month)),
                        'max' = rep(0, length(month)),
                        'veg_mean' = rep(0, length(month)),
                        'veg_sd' = rep(0, length(month)))

for (i in 1:nrow(veg_stats)){
  
  print(i)
  
  veg_stats[i, c('min')] <- min(freq[freq[, c('month')] == veg_stats$month[i], c('veg')])
  veg_stats[i, c('max')] <- max(freq[freq[, c('month')] == veg_stats$month[i], c('veg')])
  veg_stats[i, c('veg_mean')] <- mean(freq[freq[, c('month')] == veg_stats$month[i], c('veg')])
  veg_stats[i, c('veg_sd')] <- sd(freq[freq[, c('month')] == veg_stats$month[i], c('veg')])
  
}

# corn

corn_stats <- data.frame('month' = month,
                         'min' = rep(0, length(month)),
                         'max' = rep(0, length(month)),
                         'corn_mean' = rep(0, length(month)),
                         'corn_sd' = rep(0, length(month)))

for (i in 1:nrow(corn_stats)){
  
  print(i)
  
  corn_stats[i, c('min')] <- min(freq[freq[, c('month')] == corn_stats$month[i], c('corn')])
  corn_stats[i, c('max')] <- max(freq[freq[, c('month')] == corn_stats$month[i], c('corn')])
  corn_stats[i, c('corn_mean')] <- mean(freq[freq[, c('month')] == corn_stats$month[i], c('corn')])
  corn_stats[i, c('corn_sd')] <- sd(freq[freq[, c('month')] == corn_stats$month[i], c('corn')])
  
}


# fruit

fruit_stats <- data.frame('month' = month,
                          'min' = rep(0, length(month)),
                          'max' = rep(0, length(month)),
                          'fruit_mean' = rep(0, length(month)),
                          'fruit_sd' = rep(0, length(month)))

for (i in 1:nrow(fruit_stats)){
  
  print(i)
  
  fruit_stats[i, c('min')] <- min(freq[freq[, c('month')] == fruit_stats$month[i], c('fruit')])
  fruit_stats[i, c('max')] <- max(freq[freq[, c('month')] == fruit_stats$month[i], c('fruit')])
  fruit_stats[i, c('fruit_mean')] <- mean(freq[freq[, c('month')] == fruit_stats$month[i], c('fruit')])
  fruit_stats[i, c('fruit_sd')] <- sd(freq[freq[, c('month')] == fruit_stats$month[i], c('fruit')])
  
}

#fat

fat_stats <- data.frame('month' = month,
                        'min' = rep(0, length(month)),
                        'max' = rep(0, length(month)),
                        'fat_mean' = rep(0, length(month)),
                        'fat_sd' = rep(0, length(month)))

for (i in 1:nrow(fat_stats)){
  
  print(i)
  
  fat_stats[i, c('min')] <- min(freq[freq[, c('month')] == fat_stats$month[i], c('fat')])
  fat_stats[i, c('max')] <- max(freq[freq[, c('month')] == fat_stats$month[i], c('fat')])
  fat_stats[i, c('fat_mean')] <- mean(freq[freq[, c('month')] == fat_stats$month[i], c('fat')])
  fat_stats[i, c('fat_sd')] <- sd(freq[freq[, c('month')] == fat_stats$month[i], c('fat')])
  
}

freq_stats <- cbind(protein_stats, carbs_stats[, -c(1)], veg_stats[, -c(1)], corn_stats[, -c(1)], 
                    fruit_stats[, -c(1)], fat_stats[, -c(1)])

reduced_freq_stats <- freq_stats[, c('month', 'protein_mean', 'protein_sd', 'carbs_mean', 'carbs_sd',
                                     'veg_mean', 'veg_sd', 'corn_mean', 'corn_sd', 'fruit_mean', 'fruit_sd',
                                     'fat_mean', 'fat_sd')] %>%
  mutate(month = month_list)

reduced_freq_stats <- reduced_freq_stats %>% mutate_at(vars(protein_mean:fat_sd), funs(round(., 3)))

colnames(reduced_freq_stats)[2:13] <- rep(c('mean', 'sd'), 6)

# village average efficiency across months

vil_eff <- read.csv('monthly_village_av_eff.csv') %>%
  select(c('v10', 'v13', 'v2', 'v28', 'v35', 'v49', 'v53', 'v55', 'v67', 'v68'))

vil <- c(10, 13, 2, 28, 35, 49, 53, 55, 67, 68)

vil_stats <- data.frame('village_id' = vil,
                        'min' = rep(0, length(vil)),
                        'max' = rep(0, length(vil)),
                        'mean' = rep(0, length(vil)),
                        'sd' = rep(0, length(vil))) 
  

for (i in 1:ncol(vil_eff)){
  
  print(i)
  
  vil_stats[i, c('min')] <- min(vil_eff[, i])
  vil_stats[i, c('max')] <- max(vil_eff[, i])
  vil_stats[i, c('mean')] <- mean(vil_eff[, i])
  vil_stats[i, c('sd')] <- sd(vil_eff[, i])
  
}

vil_stats <- round(vil_stats, 3)

vil_month_stats <- data.frame(as.matrix(t(vil_eff))) %>% round(3)
colnames(vil_month_stats) <- month_list
rownames(vil_month_stats) <- c('vil_10', 'vil_13', 'vil_2', 'vil_28', 'vil_35', 'vil_49', 'vil_53', 
                               'vil_55', 'vil_67', 'vil_68')
vil_month_stats <- cbind(vil_month_stats, vil_stats[ , c('mean', 'sd')])

# aggregate desc stats for all villages and all months

month_stats <- data.frame('month' = month,
                          'min' = rep(0, length(month)),
                          'max' = rep(0, length(month)),
                          'mean' = rep(0, length(month)),
                          'sd' = rep(0, length(month)))

for (i in 1:nrow(month_stats)){
 
  print(i)
  
  month_stats[i, c('min')] <- min(agg_data[agg_data[, c('month')] == month_stats$month[i], c('eff')])
  month_stats[i, c('max')] <- max(agg_data[agg_data[, c('month')] == month_stats$month[i], c('eff')])
  month_stats[i, c('mean')] <- mean(agg_data[agg_data[, c('month')] == month_stats$month[i], c('eff')])
  month_stats[i, c('sd')] <- sd(agg_data[agg_data[, c('month')] == month_stats$month[i], c('eff')])

}

month_stats <- round(month_stats, 3) %>% mutate(month = month_list)  

# model variables desc stats



#### FE and RE with re-specified variables ####

# FE models, no interaction and interaction specification

fe_mod <- lm(eff ~ totalhhmembers + quadhhm + d_gen + totalacres + d_chunder5 + d_livinc_l + d_livinc_m +
                d_offinc_l + d_offinc_m + electricity + maxedu + d_tapwtr + d_runwtr + d_latrine + m_n,
              data = agg_data)

fe_int_mod <- lm(eff ~ totalhhmembers + quadhhm + d_gen + totalacres + d_chunder5 + d_livinc_l + d_livinc_m +
                   d_offinc_l + d_offinc_m + electricity + sec_sch + coll_edu + other_edu + d_tapwtr + d_runwtr +
                   d_latrine + m_n_ + totalacres:d_offinc_m,
                 data = agg_data)
# interaction model: reject homoskedastic and no auto nulls, chi square stat 18.282 with chi crit 16.151, DW stat 1.85

# Newey-West robust cov mat

n <- nrow(agg_data)
x <- model.matrix(fe_int_mod)
k <- ncol(x)
e <- residuals(fe_int_mod)

S <- matrix(0, k, k)

i <- 1
repeat{
  
  S = S + n^(-1) * (e[i]^2 * matrix(x[i, ], nrow = k, ncol = 1) %*% x[i, ])
  i = i + 1
  
  if (i > n) break 
}

W <- matrix(0, k, k)

L <- floor(n^(1/4))
l <- 1
i <- l + 1
repeat{
  
  W = W + n^(-1) * (((L + 1 - l) / (L + 1)) * e[i] * e[i - l] *
                      (matrix(x[i, ], nrow = k, ncol = 1) %*% x[i - l, ] + 
                         matrix(x[i - l, ], nrow = k, ncol = 1) %*% x[i, ]))
  i = i + 1
  
  if (i > n){
    
    l = l + 1
    i = l + 1
  }
  
  if (l > L) break
}

Q <- S + W
NW_cov <- n * solve(t(x) %*% x) %*% Q %*% solve(t(x) %*% x)
NW_se <- sqrt(diag(NW_cov))
NW_t <- coef(fe_int_mod) / NW_se
NW_p <- 2*(1-pt(abs(NW_t), n-k))

sig <- rep('', length(NW_p))
for (i in 1:length(NW_p)){
  
  if (NW_p[i] > .05 & NW_p[i] <= .1){
    
    sig[i] = '.'
  }
  
  else if (NW_p[i] > .01 & NW_p[i] <= .05){
    
    sig[i] = '*'
  }
  
  else if (NW_p[i] > .001 & NW_p[i] <= .01){
    
    sig[i] = '**'
  }
  
  else if (NW_p[i] <= .001){
    
    sig[i] = '***'
  }
  
  else {
    
    sig[i] == ''
  }
}

mod_sum <- cbind(coef(summary(fe_int_mod))[, 1:2], NW_se,
                 coef(summary(fe_int_mod))[, 3], NW_t,
                 coef(summary(fe_int_mod))[, 4], NW_p) %>% round(4)
colnames(mod_sum)[c(2, 4, 6)] <- c('FE_se', 'FE_t', 'FE_p')

mod_sum <- cbind(mod_sum, data.frame(sig))
colnames(mod_sum)[8] <- ''

max_hhmembers <- coef(fe_int_mod)[2]/(abs(2*coef(fe_int_mod)[3]))

# reduced observation model to include vaccine observations

reduced_mod <- lm(eff ~ totalhhmembers + quadhhm + d_gen + totalacres + d_chunder5 + d_livinc_l + d_livinc_m +
                    d_offinc_l + d_offinc_m + electricity + maxedu + d_tapwtr + d_runwtr + d_latrine + m_n +
                    child5vacc + totalhhmembers:d_offinc_m + totalacres:d_offinc_m,
                  data = dat2)
# RE model
re_mod <- lmer(eff ~ totalhhmembers + quadhhm + d_gen + totalacres + d_chunder5 + d_livinc_l + d_livinc_m +
                 d_offinc_l + d_offinc_m + electricity + maxedu + d_tapwtr + d_runwtr + d_latrine + 
                 totalacres:d_offinc_m + (1 | m_n),
               data = agg_data)


