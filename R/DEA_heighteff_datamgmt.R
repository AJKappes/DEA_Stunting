library(Benchmarking)
library(lpSolve)
library(ggplot2)
library(dplyr)
library(magrittr)

df <- data.frame()

dea <- function(df){
  
  
  inputs <- df[, 5:10]
  output <- data.frame(df[, 4])
  colnames(output) <- c('height')
  
  n <- nrow(df)
  s <- ncol(inputs)
  m <- ncol(output)
  
  for (i in 1:n){
    
    obj.f <- c(as.numeric(output[i,]), rep(0,s), 1, -1)
    cons_lhs_1 <- cbind(output, -1*inputs, 1, -1)
    cons.f <- rbind(cons_lhs_1, c(rep(0,m), as.numeric(inputs[i,]), 0, 0))
    sdirxn <- c(rep("<=",n), "=")
    rhs <- c(rep(0,n), 1)
    res <- lp("max", obj.f, cons.f, sdirxn, rhs, scale = 1)
    
    if (i==1){
      
      eff <- res$objval
      vals <- res$solution
    }
    else{
      
      eff <- rbind(eff, res$objval)
      vals <- rbind(vals, res$solution)
    }
  }
  
  eff <- data.frame(eff[,])
  vals <- data.frame(vals[,])
  
  #return(eff)
  return(vals)

}

#### data to apply to function ####
data <- read.csv('subheightdata.csv')
data <- data[-c(32, 52, 90, 111, 115, 126, 192), c(-1)]

data1 <- read.csv('dat1.csv')
data1 <- data1[-c(32, 52, 90, 111, 115, 126, 192), c(-1)]

data2 <- read.csv('dat2.csv')
data2 <- data2[-c(32, 52, 90, 111, 115, 126, 192), c(-1)]

data3 <- read.csv('dat3.csv')
data3 <- data3[-c(32, 52, 90, 111, 115, 126, 192), c(-1)]

data4 <- read.csv('dat4.csv')
data4 <- data4[-c(32, 52, 90, 111, 115, 126, 192), c(-1)]

data5 <- read.csv('dat5.csv')
data5 <- data5[-c(32, 52, 90, 111, 115, 126, 192), c(-1)]

data6 <- read.csv('dat6.csv')
data6 <- data6[-c(32, 52, 90, 111, 115, 126, 192), c(-1)]

data7 <- read.csv('dat7.csv')
data7 <- data7[-c(32, 52, 90, 111, 115, 126, 192), c(-1)]

data8 <- read.csv('dat8.csv')
data8 <- data8[-c(32, 52, 90, 111, 115, 126, 192), c(-1)]

data9 <- read.csv('dat9.csv')
data9 <- data9[-c(32, 52, 90, 111, 115, 126, 192), c(-1)]

data10 <- read.csv('dat10.csv')
data10 <- data10[-c(32, 52, 90, 111, 115, 126, 192), c(-1)]

data11 <- read.csv('dat11.csv')
data11 <- data11[-c(32, 52, 90, 111, 115, 126, 192), c(-1)]

eff1 <- dea(data1)
eff2 <- dea(data2)
eff3 <- dea(data3)
eff4 <- dea(data4)
eff5 <- dea(data5)
eff6 <- dea(data6)
eff7 <- dea(data7)
eff8 <- dea(data8)
eff9 <- dea(data9)
eff10 <- dea(data10)
eff11 <- dea(data11)

# comment out below if objective fx values are used, not input coefficient contribution
nutr_ins <- c('protein', 'carbs', 'veg', 'corn', 'fruit', 'fat')

val1 <- eff1[, 2:7] %>% set_colnames(nutr_ins)
val2 <- eff2[, 2:7] %>% set_colnames(nutr_ins)
val3 <- eff3[, 2:7] %>% set_colnames(nutr_ins)
val4 <- eff4[, 2:7] %>% set_colnames(nutr_ins)
val5 <- eff5[, 2:7] %>% set_colnames(nutr_ins)
val6 <- eff6[, 2:7] %>% set_colnames(nutr_ins)
val7 <- eff7[, 2:7] %>% set_colnames(nutr_ins)
val8 <- eff8[, 2:7] %>% set_colnames(nutr_ins)
val9 <- eff9[, 2:7] %>% set_colnames(nutr_ins)
val10 <- eff10[, 2:7] %>% set_colnames(nutr_ins)
val11 <- eff11[, 2:7] %>% set_colnames(nutr_ins)

means_val1 <- cbind(mean(val1$protein), mean(val1$carbs), mean(val1$veg),
                    mean(val1$corn), mean(val1$fruit), mean(val1$fat))

stndv_val1 <- cbind(sd(val1$protein), sd(val1$carbs), sd(val1$veg),
                    sd(val1$corn), sd(val1$fruit), sd(val1$fat))

sum_stat_val1 <- rbind(means_val1, stndv_val1) %>% set_colnames(nutr_ins) %>% set_rownames(c('mean', 'sd'))

means_val2 <- cbind(mean(val2$protein), mean(val2$carbs), mean(val2$veg),
                    mean(val2$corn), mean(val2$fruit), mean(val2$fat))

stndv_val2 <- cbind(sd(val2$protein), sd(val2$carbs), sd(val2$veg),
                    sd(val2$corn), sd(val2$fruit), sd(val2$fat))

sum_stat_val2 <- rbind(means_val2, stndv_val2) %>% set_colnames(nutr_ins) %>% set_rownames(c('mean', 'sd'))

means_val3 <- cbind(mean(val3$protein), mean(val3$carbs), mean(val3$veg),
                    mean(val3$corn), mean(val3$fruit), mean(val3$fat))

stndv_val3 <- cbind(sd(val3$protein), sd(val3$carbs), sd(val3$veg),
                    sd(val3$corn), sd(val3$fruit), sd(val3$fat))

sum_stat_val3 <- rbind(means_val3, stndv_val3) %>% set_colnames(nutr_ins) %>% set_rownames(c('mean', 'sd'))

means_val4 <- cbind(mean(val4$protein), mean(val4$carbs), mean(val4$veg),
                    mean(val4$corn), mean(val4$fruit), mean(val4$fat))

stndv_val4 <- cbind(sd(val4$protein), sd(val4$carbs), sd(val4$veg),
                    sd(val4$corn), sd(val4$fruit), sd(val4$fat))

sum_stat_val4 <- rbind(means_val4, stndv_val4) %>% set_colnames(nutr_ins) %>% set_rownames(c('mean', 'sd'))

means_val5 <- cbind(mean(val5$protein), mean(val5$carbs), mean(val5$veg),
                    mean(val5$corn), mean(val5$fruit), mean(val5$fat))

stndv_val5 <- cbind(sd(val5$protein), sd(val5$carbs), sd(val5$veg),
                    sd(val5$corn), sd(val5$fruit), sd(val5$fat))

sum_stat_val5 <- rbind(means_val5, stndv_val5) %>% set_colnames(nutr_ins) %>% set_rownames(c('mean', 'sd'))

means_val6 <- cbind(mean(val6$protein), mean(val6$carbs), mean(val6$veg),
                    mean(val6$corn), mean(val6$fruit), mean(val6$fat))

stndv_val6 <- cbind(sd(val6$protein), sd(val6$carbs), sd(val6$veg),
                    sd(val6$corn), sd(val6$fruit), sd(val6$fat))

sum_stat_val6 <- rbind(means_val6, stndv_val6) %>% set_colnames(nutr_ins) %>% set_rownames(c('mean', 'sd'))

means_val7 <- cbind(mean(val7$protein), mean(val7$carbs), mean(val7$veg),
                    mean(val7$corn), mean(val7$fruit), mean(val7$fat))

stndv_val7 <- cbind(sd(val7$protein), sd(val7$carbs), sd(val7$veg),
                    sd(val7$corn), sd(val7$fruit), sd(val7$fat))

sum_stat_val7 <- rbind(means_val7, stndv_val7) %>% set_colnames(nutr_ins) %>% set_rownames(c('mean', 'sd'))

means_val8 <- cbind(mean(val8$protein), mean(val8$carbs), mean(val8$veg),
                    mean(val8$corn), mean(val8$fruit), mean(val8$fat))

stndv_val8 <- cbind(sd(val8$protein), sd(val8$carbs), sd(val8$veg),
                    sd(val8$corn), sd(val8$fruit), sd(val8$fat))

sum_stat_val8 <- rbind(means_val8, stndv_val8) %>% set_colnames(nutr_ins) %>% set_rownames(c('mean', 'sd'))

means_val9 <- cbind(mean(val9$protein), mean(val9$carbs), mean(val9$veg),
                    mean(val9$corn), mean(val9$fruit), mean(val9$fat))

stndv_val9 <- cbind(sd(val9$protein), sd(val9$carbs), sd(val9$veg),
                    sd(val9$corn), sd(val9$fruit), sd(val9$fat))

sum_stat_val9 <- rbind(means_val9, stndv_val9) %>% set_colnames(nutr_ins) %>% set_rownames(c('mean', 'sd'))

means_val10 <- cbind(mean(val10$protein), mean(val10$carbs), mean(val10$veg),
                    mean(val10$corn), mean(val10$fruit), mean(val10$fat))

stndv_val10 <- cbind(sd(val10$protein), sd(val10$carbs), sd(val10$veg),
                    sd(val10$corn), sd(val10$fruit), sd(val10$fat))

sum_stat_val10 <- rbind(means_val10, stndv_val10) %>% set_colnames(nutr_ins) %>% set_rownames(c('mean', 'sd'))

means_val11 <- cbind(mean(val11$protein), mean(val11$carbs), mean(val11$veg),
                    mean(val11$corn), mean(val11$fruit), mean(val11$fat))

stndv_val11 <- cbind(sd(val11$protein), sd(val11$carbs), sd(val11$veg),
                    sd(val11$corn), sd(val11$fruit), sd(val11$fat))

sum_stat_val11 <- rbind(means_val11, stndv_val11) %>% set_colnames(nutr_ins) %>% set_rownames(c('mean', 'sd'))


desc <- data.frame(rep(c('mean', 'sd'), 11)) %>% set_colnames('statistic')
desc_row <- data.frame(c('Jun', 'Jun', 'Jul', 'Jul', 'Aug', 'Aug', 'Sep', 'Sep', 'Oct', 'Oct',
                         'Nov', 'Nov', 'Dec', 'Dec', 'Jan', 'Jan', 'Feb', 'Feb', 'Mar', 'Mar',
                         'Apr', 'Apr')) %>% set_colnames('month')

agg_sum_val_stat <- rbind(sum_stat_val1, sum_stat_val2, sum_stat_val3, sum_stat_val4,
                          sum_stat_val5, sum_stat_val6, sum_stat_val7, sum_stat_val8,
                          sum_stat_val9, sum_stat_val10, sum_stat_val11) %>% set_rownames(1:nrow(desc_row))

agg_sum_val_stat <- cbind(desc_row, desc, agg_sum_val_stat)
mean_val_stat <- cbind(subset(agg_sum_val_stat, agg_sum_val_stat$statistic == 'mean'), 1:11)
colnames(mean_val_stat)[3:9] <- c('Food_Group1', 'Food_Group2', 'Food_Group3', 'Food_Group4', 'Food_Group5',
                                  'Food_Group6','x')

input_map <- ggplot(mean_val_stat, aes(x)) +
  geom_line(aes(y = Food_Group1), alpha = 0.5) +
  geom_point(aes(y = Food_Group1, shape = 'Food_Goup1'), size = 4) +
  geom_line(aes(y = Food_Group2), alpha = 0.5) +
  geom_point(aes(y = Food_Group2, shape = 'Food_Group2'), size = 4) +
  geom_line(aes(y = Food_Group3), alpha = 0.5) +
  geom_point(aes(y = Food_Group3, shape = 'Food_Group3'), size = 4) +
  geom_line(aes(y = Food_Group4), alpha = 0.5) +
  geom_point(aes(y = Food_Group4, shape = 'Food_Group4'), size = 4) +
  geom_line(aes(y = Food_Group5), alpha = 0.5) +
  geom_point(aes(y = Food_Group5, shape = 'Food_Group5'), size = 4) +
  geom_line(aes(y = Food_Group6), alpha = 0.5) +
  geom_point(aes(y = Food_Group6, shape = 'Food_Group6'), size = 4) +
  scale_shape_manual(name = 'Food Groups', 
                     labels = c('Chicken, fish, eggs, other meat items',
                                'Rice, potatoes, wheat',
                                'Greens, cabbage, tomatoes, avocados, other vegetables',
                                'Ugali and porridge',
                                'Mangos, oranges, bananas, guavas',
                                'Breast milk'),
                     values = c(0, 1, 2, 3, 4, 5)) +
  scale_x_discrete(limits = c('June-14', 'July', 'Aug', 'Sep', 'Oct','Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr')) +
  scale_y_continuous(breaks = seq(from = 0, to = 0.25, by = .025), limits = c(0, 0.25)) +
  labs(title = "Food Group % Impact on Growth Efficiency", x = '', y = "% Impact") +
  theme(plot.title = element_text(size = 20, face = 'bold', hjust = .5),
        axis.title = element_text(size = 15, face = 'bold'))

eff <- cbind(eff1, eff2, eff3, eff4, eff5, eff6, eff7, eff8, eff9, eff10, eff11)
colnames(eff) <- c('June', 'July', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr')


#### package function DEA to confirm equivalent values ####
inputss <- data11[, 5:10]
outputt <- data11[, 4]

eff_deav1 <- dea(inputss, outputt, RTS = 'vrs', SLACK = TRUE)
eff_deac1 <- dea(inputss, outputt, RTS = 'crs', SLACK = TRUE)
m1 <- mean(eff_deav1$objval - eff_deac1$objval)

eff_deav2 <- dea(inputss, outputt, RTS = 'vrs', SLACK = TRUE)
eff_deac2 <- dea(inputss, outputt, RTS = 'crs', SLACK = TRUE)
m2 <- mean(eff_deav2$objval - eff_deac2$objval)

eff_deav3 <- dea(inputss, outputt, RTS = 'vrs', SLACK = TRUE)
eff_deac3 <- dea(inputss, outputt, RTS = 'crs', SLACK = TRUE)
m3 <- mean(eff_deav3$objval - eff_deac3$objval)

eff_deav4 <- dea(inputss, outputt, RTS = 'vrs', SLACK = TRUE)
eff_deac4 <- dea(inputss, outputt, RTS = 'crs', SLACK = TRUE)
m4 <- mean(eff_deav4$objval - eff_deac4$objval)

eff_deav5 <- dea(inputss, outputt, RTS = 'vrs', SLACK = TRUE)
eff_deac5 <- dea(inputss, outputt, RTS = 'crs', SLACK = TRUE)
m5 <- mean(eff_deav5$objval - eff_deac5$objval)

eff_deav6 <- dea(inputss, outputt, RTS = 'vrs', SLACK = TRUE)
eff_deac6 <- dea(inputss, outputt, RTS = 'crs', SLACK = TRUE)
m6 <- mean(eff_deav6$objval - eff_deac6$objval)

eff_deav7 <- dea(inputss, outputt, RTS = 'vrs', SLACK = TRUE)
eff_deac7 <- dea(inputss, outputt, RTS = 'crs', SLACK = TRUE)
m7 <- mean(eff_deav7$objval - eff_deac7$objval)

eff_deav8 <- dea(inputss, outputt, RTS = 'vrs', SLACK = TRUE)
eff_deac8 <- dea(inputss, outputt, RTS = 'crs', SLACK = TRUE)
m8 <- mean(eff_deav8$objval - eff_deac8$objval)

eff_deav9 <- dea(inputss, outputt, RTS = 'vrs', SLACK = TRUE)
eff_deac9 <- dea(inputss, outputt, RTS = 'crs', SLACK = TRUE)
m9 <- mean(eff_deav9$objval - eff_deac9$objval)

eff_deav10 <- dea(inputss, outputt, RTS = 'vrs', SLACK = TRUE)
eff_deac10 <- dea(inputss, outputt, RTS = 'crs', SLACK = TRUE)
m10 <- mean(eff_deav10$objval - eff_deac10$objval)

eff_deav11 <- dea(inputss, outputt, RTS = 'vrs', SLACK = TRUE)
eff_deac11 <- dea(inputss, outputt, RTS = 'crs', SLACK = TRUE)
m11 <- mean(eff_deav11$objval - eff_deac11$objval)

rbind(m1, m2, m3, m4, m5, m6, m7,m8, m9, m10, m11) %>% round(3)

### aggregating efficiencies and food frequencies ##################################

agg_eff <- cbind(data1[, 1:3], eff1, eff2, eff3, eff4, eff5, eff6, eff7, eff8, eff9, eff10, eff11)
names(agg_eff)[4:14] <- c('m1', 'm2', 'm3', 'm4', 'm5', 'm6', 'm7', 'm8', 'm9', 'm10', 'm11')

macronutrients <- c('protein', 'carbs', 'veg', 'corn', 'fruit', 'fat', 'gen')

freq1 <- cbind(data1[, macronutrients], rep(6, nrow(data1)))
colnames(freq1)[8] <- 'month'

freq2 <- cbind(data2[, macronutrients], rep(7, nrow(data2)))
colnames(freq2)[8] <- 'month'

freq3 <- cbind(data3[, macronutrients], rep(8, nrow(data3)))
colnames(freq3)[8] <- 'month'

freq4 <- cbind(data4[, macronutrients], rep(9, nrow(data4)))
colnames(freq4)[8] <- 'month'

freq5 <- cbind(data5[, macronutrients], rep(10, nrow(data5)))
colnames(freq5)[8] <- 'month'

freq6 <- cbind(data6[, macronutrients], rep(11, nrow(data6)))
colnames(freq6)[8] <- 'month'

freq7 <- cbind(data7[, macronutrients], rep(12, nrow(data7)))
colnames(freq7)[8] <- 'month'

freq8 <- cbind(data8[, macronutrients], rep(1, nrow(data8)))
colnames(freq8)[8] <- 'month'

freq9 <- cbind(data9[, macronutrients], rep(2, nrow(data9)))
colnames(freq9)[8] <- 'month'

freq10 <- cbind(data10[, macronutrients], rep(3, nrow(data10)))
colnames(freq10)[8] <- 'month'

freq11 <- cbind(data11[, macronutrients], rep(4, nrow(data11)))
colnames(freq11)[8] <- 'month'

nutrient_freq <- rbind(freq1, freq2, freq3, freq4, freq5, freq6, freq7, freq8, freq9, freq10, freq11)

#write.csv(nutrient_freq, file = 'nutrient_freq.csv', row.names = FALSE)

### month 1 subsetting high efficiency ########################

m1_list_top <- which(agg_eff$m1 >= .8)
m1_eff_high <- cbind(data1[c(m1_list_top), 2:10], agg_eff[c(m1_list_top), 'm1'])

m1_eff_m <- subset(m1_eff_high, m1_eff_high$gen=='M', select = c(1:10))
m1_eff_f <- subset(m1_eff_high, m1_eff_high$gen=='F', select = c(1:10))

attach(m1_eff_m)
m1_m_summary_high <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                           mean(fruit), mean(fat), nrow(m1_eff_m))
detach(m1_eff_m)

attach(m1_eff_f)
m1_f_summary_high <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                           mean(fruit), mean(fat), nrow(m1_eff_f))
detach(m1_eff_f)

m1_summary_high <- round(rbind(m1_m_summary_high, m1_f_summary_high), 3)
colnames(m1_summary_high) <- c('protien', 'carbs', 'veg', 'corn', 'fruit', 'fat', 'obs')
rownames(m1_summary_high) <- c('Month 1: male >= .8 eff', 'female >= .8 eff')

### month 1 subsetting low efficiency ########################

m1_list_bot <- which(agg_eff$m1 < .8)
m1_eff_low <- cbind(data1[c(m1_list_bot), 2:10], agg_eff[c(m1_list_bot), 'm1'])

m1_eff_m <- subset(m1_eff_low, m1_eff_low$gen=='M', select = c(1:10))
m1_eff_f <- subset(m1_eff_low, m1_eff_low$gen=='F', select = c(1:10))

attach(m1_eff_m)
m1_m_summary_low <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                           mean(fruit), mean(fat), nrow(m1_eff_m))
detach(m1_eff_m)

attach(m1_eff_f)
m1_f_summary_low <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                           mean(fruit), mean(fat), nrow(m1_eff_f))
detach(m1_eff_f)

m1_summary_low <- round(rbind(m1_m_summary_low, m1_f_summary_low), 3)
colnames(m1_summary_low) <- c('protien', 'carbs', 'veg', 'corn', 'fruit', 'fat', 'obs')
rownames(m1_summary_low) <- c('month 1: male < .8 eff', 'female < .8 eff')

m1_summary <- rbind(m1_summary_high, m1_summary_low)



### month 2 subsetting high efficiency ########################

m2_list_top <- which(agg_eff$m2 >= .8)
m2_eff_high <- cbind(data2[c(m2_list_top), 2:10], agg_eff[c(m2_list_top), 'm2'])

m2_eff_m <- subset(m2_eff_high, m2_eff_high$gen=='M', select = c(1:10))
m2_eff_f <- subset(m2_eff_high, m2_eff_high$gen=='F', select = c(1:10))

attach(m2_eff_m)
m2_m_summary_high <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                           mean(fruit), mean(fat), nrow(m2_eff_m))
detach(m2_eff_m)

attach(m2_eff_f)
m2_f_summary_high <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                           mean(fruit), mean(fat), nrow(m2_eff_f))
detach(m2_eff_f)

m2_summary_high <- round(rbind(m2_m_summary_high, m2_f_summary_high), 3)
colnames(m2_summary_high) <- c('protien', 'carbs', 'veg', 'corn', 'fruit', 'fat', 'obs')
rownames(m2_summary_high) <- c('Month 2: male >= .8 eff', 'female >= .8 eff')

### month 2 subsetting low efficiency ########################

m2_list_bot <- which(agg_eff$m2 < .8)
m2_eff_low <- cbind(data2[c(m2_list_bot), 2:10], agg_eff[c(m2_list_bot), 'm2'])

m2_eff_m <- subset(m2_eff_low, m2_eff_low$gen=='M', select = c(1:10))
m2_eff_f <- subset(m2_eff_low, m2_eff_low$gen=='F', select = c(1:10))

attach(m2_eff_m)
m2_m_summary_low <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                          mean(fruit), mean(fat), nrow(m2_eff_m))
detach(m2_eff_m)

attach(m2_eff_f)
m2_f_summary_low <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                          mean(fruit), mean(fat), nrow(m2_eff_f))
detach(m2_eff_f)

m2_summary_low <- round(rbind(m2_m_summary_low, m2_f_summary_low), 3)
colnames(m2_summary_low) <- c('protien', 'carbs', 'veg', 'corn', 'fruit', 'fat', 'obs')
rownames(m2_summary_low) <- c('month 2: male < .8 eff', 'female < .8 eff')

m2_summary <- rbind(m2_summary_high, m2_summary_low)





### month 3 subsetting high efficiency ########################

m3_list_top <- which(agg_eff$m3 >= .8)
m3_eff_high <- cbind(data3[c(m3_list_top), 2:10], agg_eff[c(m3_list_top), 'm3'])

m3_eff_m <- subset(m3_eff_high, m3_eff_high$gen=='M', select = c(1:10))
m3_eff_f <- subset(m3_eff_high, m3_eff_high$gen=='F', select = c(1:10))

attach(m3_eff_m)
m3_m_summary_high <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                           mean(fruit), mean(fat), nrow(m3_eff_m))
detach(m3_eff_m)

attach(m3_eff_f)
m3_f_summary_high <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                           mean(fruit), mean(fat), nrow(m3_eff_f))
detach(m3_eff_f)

m3_summary_high <- round(rbind(m3_m_summary_high, m3_f_summary_high), 3)
colnames(m3_summary_high) <- c('protien', 'carbs', 'veg', 'corn', 'fruit', 'fat', 'obs')
rownames(m3_summary_high) <- c('Month 3: male >= .8 eff', 'female >= .8 eff')

### month 3 subsetting low efficiency ########################

m3_list_bot <- which(agg_eff$m3 < .8)
m3_eff_low <- cbind(data3[c(m3_list_bot), 2:10], agg_eff[c(m3_list_bot), 'm3'])

m3_eff_m <- subset(m3_eff_low, m3_eff_low$gen=='M', select = c(1:10))
m3_eff_f <- subset(m3_eff_low, m3_eff_low$gen=='F', select = c(1:10))

attach(m3_eff_m)
m3_m_summary_low <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                          mean(fruit), mean(fat), nrow(m3_eff_m))
detach(m3_eff_m)

attach(m3_eff_f)
m3_f_summary_low <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                          mean(fruit), mean(fat), nrow(m3_eff_f))
detach(m3_eff_f)

m3_summary_low <- round(rbind(m3_m_summary_low, m3_f_summary_low), 3)
colnames(m3_summary_low) <- c('protien', 'carbs', 'veg', 'corn', 'fruit', 'fat', 'obs')
rownames(m3_summary_low) <- c('month 3: male < .8 eff', 'female < .8 eff')

m3_summary <- rbind(m3_summary_high, m3_summary_low)

### month 4 subsetting high efficiency ########################

m4_list_top <- which(agg_eff$m4 >= .8)
m4_eff_high <- cbind(data4[c(m4_list_top), 2:10], agg_eff[c(m4_list_top), 'm4'])

m4_eff_m <- subset(m4_eff_high, m4_eff_high$gen=='M', select = c(1:10))
m4_eff_f <- subset(m4_eff_high, m4_eff_high$gen=='F', select = c(1:10))

attach(m4_eff_m)
m4_m_summary_high <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                           mean(fruit), mean(fat), nrow(m4_eff_m))
detach(m4_eff_m)

attach(m4_eff_f)
m4_f_summary_high <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                           mean(fruit), mean(fat), nrow(m4_eff_f))
detach(m4_eff_f)

m4_summary_high <- round(rbind(m4_m_summary_high, m4_f_summary_high), 3)
colnames(m4_summary_high) <- c('protien', 'carbs', 'veg', 'corn', 'fruit', 'fat', 'obs')
rownames(m4_summary_high) <- c('Month 4: male >= .8 eff', 'female >= .8 eff')

### month 4 subsetting low efficiency ########################

m4_list_bot <- which(agg_eff$m4 < .8)
m4_eff_low <- cbind(data4[c(m4_list_bot), 2:10], agg_eff[c(m4_list_bot), 'm4'])

m4_eff_m <- subset(m4_eff_low, m4_eff_low$gen=='M', select = c(1:10))
m4_eff_f <- subset(m4_eff_low, m4_eff_low$gen=='F', select = c(1:10))

attach(m4_eff_m)
m4_m_summary_low <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                          mean(fruit), mean(fat), nrow(m4_eff_m))
detach(m4_eff_m)

attach(m4_eff_f)
m4_f_summary_low <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                          mean(fruit), mean(fat), nrow(m4_eff_f))
detach(m4_eff_f)

m4_summary_low <- round(rbind(m4_m_summary_low, m4_f_summary_low), 3)
colnames(m4_summary_low) <- c('protien', 'carbs', 'veg', 'corn', 'fruit', 'fat', 'obs')
rownames(m4_summary_low) <- c('month 4: male < .8 eff', 'female < .8 eff')

m4_summary <- rbind(m4_summary_high, m4_summary_low)

### month 5 subsetting high efficiency ########################

m5_list_top <- which(agg_eff$m5 >= .8)
m5_eff_high <- cbind(data5[c(m5_list_top), 2:10], agg_eff[c(m5_list_top), 'm5'])

m5_eff_m <- subset(m5_eff_high, m5_eff_high$gen=='M', select = c(1:10))
m5_eff_f <- subset(m5_eff_high, m5_eff_high$gen=='F', select = c(1:10))

attach(m5_eff_m)
m5_m_summary_high <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                           mean(fruit), mean(fat), nrow(m5_eff_m))
detach(m5_eff_m)

attach(m5_eff_f)
m5_f_summary_high <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                           mean(fruit), mean(fat), nrow(m5_eff_f))
detach(m5_eff_f)

m5_summary_high <- round(rbind(m5_m_summary_high, m5_f_summary_high), 3)
colnames(m5_summary_high) <- c('protien', 'carbs', 'veg', 'corn', 'fruit', 'fat', 'obs')
rownames(m5_summary_high) <- c('Month 5: male >= .8 eff', 'female >= .8 eff')

### month 5 subsetting low efficiency ########################

m5_list_bot <- which(agg_eff$m5 < .8)
m5_eff_low <- cbind(data5[c(m5_list_bot), 2:10], agg_eff[c(m5_list_bot), 'm5'])

m5_eff_m <- subset(m5_eff_low, m5_eff_low$gen=='M', select = c(1:10))
m5_eff_f <- subset(m5_eff_low, m5_eff_low$gen=='F', select = c(1:10))

attach(m5_eff_m)
m5_m_summary_low <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                          mean(fruit), mean(fat), nrow(m5_eff_m))
detach(m5_eff_m)

attach(m5_eff_f)
m5_f_summary_low <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                          mean(fruit), mean(fat), nrow(m5_eff_f))
detach(m5_eff_f)

m5_summary_low <- round(rbind(m5_m_summary_low, m5_f_summary_low), 3)
colnames(m5_summary_low) <- c('protien', 'carbs', 'veg', 'corn', 'fruit', 'fat', 'obs')
rownames(m5_summary_low) <- c('month 5: male < .8 eff', 'female < .8 eff')

m5_summary <- rbind(m5_summary_high, m5_summary_low)

### month 6 subsetting high efficiency ########################

m6_list_top <- which(agg_eff$m6 >= .8)
m6_eff_high <- cbind(data6[c(m6_list_top), 2:10], agg_eff[c(m6_list_top), 'm6'])

m6_eff_m <- subset(m6_eff_high, m6_eff_high$gen=='M', select = c(1:10))
m6_eff_f <- subset(m6_eff_high, m6_eff_high$gen=='F', select = c(1:10))

attach(m6_eff_m)
m6_m_summary_high <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                           mean(fruit), mean(fat), nrow(m6_eff_m))
detach(m6_eff_m)

attach(m6_eff_f)
m6_f_summary_high <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                           mean(fruit), mean(fat), nrow(m6_eff_f))
detach(m6_eff_f)

m6_summary_high <- round(rbind(m6_m_summary_high, m6_f_summary_high), 3)
colnames(m6_summary_high) <- c('protien', 'carbs', 'veg', 'corn', 'fruit', 'fat', 'obs')
rownames(m6_summary_high) <- c('Month 6: male >= .8 eff', 'female >= .8 eff')

### month 6 subsetting low efficiency ########################

m6_list_bot <- which(agg_eff$m6 < .8)
m6_eff_low <- cbind(data6[c(m6_list_bot), 2:10], agg_eff[c(m6_list_bot), 'm6'])

m6_eff_m <- subset(m6_eff_low, m6_eff_low$gen=='M', select = c(1:10))
m6_eff_f <- subset(m6_eff_low, m6_eff_low$gen=='F', select = c(1:10))

attach(m6_eff_m)
m6_m_summary_low <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                          mean(fruit), mean(fat), nrow(m6_eff_m))
detach(m6_eff_m)

attach(m6_eff_f)
m6_f_summary_low <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                          mean(fruit), mean(fat), nrow(m6_eff_f))
detach(m6_eff_f)

m6_summary_low <- round(rbind(m6_m_summary_low, m6_f_summary_low), 3)
colnames(m6_summary_low) <- c('protien', 'carbs', 'veg', 'corn', 'fruit', 'fat', 'obs')
rownames(m6_summary_low) <- c('month 6: male < .8 eff', 'female < .8 eff')

m6_summary <- rbind(m6_summary_high, m6_summary_low)

### month 7 subsetting high efficiency ########################

m7_list_top <- which(agg_eff$m7 >= .8)
m7_eff_high <- cbind(data7[c(m7_list_top), 2:10], agg_eff[c(m7_list_top), 'm7'])

m7_eff_m <- subset(m7_eff_high, m7_eff_high$gen=='M', select = c(1:10))
m7_eff_f <- subset(m7_eff_high, m7_eff_high$gen=='F', select = c(1:10))

attach(m7_eff_m)
m7_m_summary_high <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                           mean(fruit), mean(fat), nrow(m7_eff_m))
detach(m7_eff_m)

attach(m7_eff_f)
m7_f_summary_high <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                           mean(fruit), mean(fat), nrow(m7_eff_f))
detach(m7_eff_f)

m7_summary_high <- round(rbind(m7_m_summary_high, m7_f_summary_high), 3)
colnames(m7_summary_high) <- c('protien', 'carbs', 'veg', 'corn', 'fruit', 'fat', 'obs')
rownames(m7_summary_high) <- c('Month 7: male >= .8 eff', 'female >= .8 eff')

### month 7 subsetting low efficiency ########################

m7_list_bot <- which(agg_eff$m7 < .8)
m7_eff_low <- cbind(data7[c(m7_list_bot), 2:10], agg_eff[c(m7_list_bot), 'm7'])

m7_eff_m <- subset(m7_eff_low, m7_eff_low$gen=='M', select = c(1:10))
m7_eff_f <- subset(m7_eff_low, m7_eff_low$gen=='F', select = c(1:10))

attach(m7_eff_m)
m7_m_summary_low <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                          mean(fruit), mean(fat), nrow(m7_eff_m))
detach(m7_eff_m)

attach(m7_eff_f)
m7_f_summary_low <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                          mean(fruit), mean(fat), nrow(m7_eff_f))
detach(m7_eff_f)

m7_summary_low <- round(rbind(m7_m_summary_low, m7_f_summary_low), 3)
colnames(m7_summary_low) <- c('protien', 'carbs', 'veg', 'corn', 'fruit', 'fat', 'obs')
rownames(m7_summary_low) <- c('month 7: male < .8 eff', 'female < .8 eff')

m7_summary <- rbind(m7_summary_high, m7_summary_low)

### month 8 subsetting high efficiency ########################

m8_list_top <- which(agg_eff$m8 >= .8)
m8_eff_high <- cbind(data8[c(m8_list_top), 2:10], agg_eff[c(m8_list_top), 'm8'])

m8_eff_m <- subset(m8_eff_high, m8_eff_high$gen=='M', select = c(1:10))
m8_eff_f <- subset(m8_eff_high, m8_eff_high$gen=='F', select = c(1:10))

attach(m8_eff_m)
m8_m_summary_high <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                           mean(fruit), mean(fat), nrow(m8_eff_m))
detach(m8_eff_m)

attach(m8_eff_f)
m8_f_summary_high <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                           mean(fruit), mean(fat), nrow(m8_eff_f))
detach(m8_eff_f)

m8_summary_high <- round(rbind(m8_m_summary_high, m8_f_summary_high), 3)
colnames(m8_summary_high) <- c('protien', 'carbs', 'veg', 'corn', 'fruit', 'fat', 'obs')
rownames(m8_summary_high) <- c('Month 8: male >= .8 eff', 'female >= .8 eff')

### month 8 subsetting low efficiency ########################

m8_list_bot <- which(agg_eff$m8 < .8)
m8_eff_low <- cbind(data8[c(m8_list_bot), 2:10], agg_eff[c(m8_list_bot), 'm8'])

m8_eff_m <- subset(m8_eff_low, m8_eff_low$gen=='M', select = c(1:10))
m8_eff_f <- subset(m8_eff_low, m8_eff_low$gen=='F', select = c(1:10))

attach(m8_eff_m)
m8_m_summary_low <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                          mean(fruit), mean(fat), nrow(m8_eff_m))
detach(m8_eff_m)

attach(m8_eff_f)
m8_f_summary_low <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                          mean(fruit), mean(fat), nrow(m8_eff_f))
detach(m8_eff_f)

m8_summary_low <- round(rbind(m8_m_summary_low, m8_f_summary_low), 3)
colnames(m8_summary_low) <- c('protien', 'carbs', 'veg', 'corn', 'fruit', 'fat', 'obs')
rownames(m8_summary_low) <- c('month 8: male < .8 eff', 'female < .8 eff')

m8_summary <- rbind(m8_summary_high, m8_summary_low)

### month 9 subsetting high efficiency ########################

m9_list_top <- which(agg_eff$m9 >= .8)
m9_eff_high <- cbind(data9[c(m9_list_top), 2:10], agg_eff[c(m9_list_top), 'm9'])

m9_eff_m <- subset(m9_eff_high, m9_eff_high$gen=='M', select = c(1:10))
m9_eff_f <- subset(m9_eff_high, m9_eff_high$gen=='F', select = c(1:10))

attach(m9_eff_m)
m9_m_summary_high <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                           mean(fruit), mean(fat), nrow(m9_eff_m))
detach(m9_eff_m)

attach(m9_eff_f)
m9_f_summary_high <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                           mean(fruit), mean(fat), nrow(m9_eff_f))
detach(m9_eff_f)

m9_summary_high <- round(rbind(m9_m_summary_high, m9_f_summary_high), 3)
colnames(m9_summary_high) <- c('protien', 'carbs', 'veg', 'corn', 'fruit', 'fat', 'obs')
rownames(m9_summary_high) <- c('Month 9: male >= .8 eff', 'female >= .8 eff')

### month 9 subsetting low efficiency ########################

m9_list_bot <- which(agg_eff$m9 < .8)
m9_eff_low <- cbind(data9[c(m9_list_bot), 2:10], agg_eff[c(m9_list_bot), 'm9'])

m9_eff_m <- subset(m9_eff_low, m9_eff_low$gen=='M', select = c(1:10))
m9_eff_f <- subset(m9_eff_low, m9_eff_low$gen=='F', select = c(1:10))

attach(m9_eff_m)
m9_m_summary_low <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                          mean(fruit), mean(fat), nrow(m9_eff_m))
detach(m9_eff_m)

attach(m9_eff_f)
m9_f_summary_low <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                          mean(fruit), mean(fat), nrow(m9_eff_f))
detach(m9_eff_f)

m9_summary_low <- round(rbind(m9_m_summary_low, m9_f_summary_low), 3)
colnames(m9_summary_low) <- c('protien', 'carbs', 'veg', 'corn', 'fruit', 'fat', 'obs')
rownames(m9_summary_low) <- c('month 9: male < .8 eff', 'female < .8 eff')

m9_summary <- rbind(m9_summary_high, m9_summary_low)

### month 10 subsetting high efficiency ########################

m10_list_top <- which(agg_eff$m10 >= .8)
m10_eff_high <- cbind(data10[c(m10_list_top), 2:10], agg_eff[c(m10_list_top), 'm10'])

m10_eff_m <- subset(m10_eff_high, m10_eff_high$gen=='M', select = c(1:10))
m10_eff_f <- subset(m10_eff_high, m10_eff_high$gen=='F', select = c(1:10))

attach(m10_eff_m)
m10_m_summary_high <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                           mean(fruit), mean(fat), nrow(m10_eff_m))
detach(m10_eff_m)

attach(m10_eff_f)
m10_f_summary_high <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                           mean(fruit), mean(fat), nrow(m10_eff_f))
detach(m10_eff_f)

m10_summary_high <- round(rbind(m10_m_summary_high, m10_f_summary_high), 3)
colnames(m10_summary_high) <- c('protien', 'carbs', 'veg', 'corn', 'fruit', 'fat', 'obs')
rownames(m10_summary_high) <- c('Month 10: male >= .8 eff', 'female >= .8 eff')

### month 10 subsetting low efficiency ########################

m10_list_bot <- which(agg_eff$m10 < .8)
m10_eff_low <- cbind(data10[c(m10_list_bot), 2:10], agg_eff[c(m10_list_bot), 'm10'])

m10_eff_m <- subset(m10_eff_low, m10_eff_low$gen=='M', select = c(1:10))
m10_eff_f <- subset(m10_eff_low, m10_eff_low$gen=='F', select = c(1:10))

attach(m10_eff_m)
m10_m_summary_low <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                          mean(fruit), mean(fat), nrow(m10_eff_m))
detach(m10_eff_m)

attach(m10_eff_f)
m10_f_summary_low <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                          mean(fruit), mean(fat), nrow(m10_eff_f))
detach(m10_eff_f)

m10_summary_low <- round(rbind(m10_m_summary_low, m10_f_summary_low), 3)
colnames(m10_summary_low) <- c('protien', 'carbs', 'veg', 'corn', 'fruit', 'fat', 'obs')
rownames(m10_summary_low) <- c('month 10: male < .8 eff', 'female < .8 eff')

m10_summary <- rbind(m10_summary_high, m10_summary_low)

### month 11 subsetting high efficiency ########################

m11_list_top <- which(agg_eff$m11 >= .8)
m11_eff_high <- cbind(data11[c(m11_list_top), 2:10], agg_eff[c(m11_list_top), 'm11'])

m11_eff_m <- subset(m11_eff_high, m11_eff_high$gen=='M', select = c(1:10))
m11_eff_f <- subset(m11_eff_high, m11_eff_high$gen=='F', select = c(1:10))

attach(m11_eff_m)
m11_m_summary_high <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                            mean(fruit), mean(fat), nrow(m11_eff_m))
detach(m11_eff_m)

attach(m11_eff_f)
m11_f_summary_high <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                            mean(fruit), mean(fat), nrow(m11_eff_f))
detach(m11_eff_f)

m11_summary_high <- round(rbind(m11_m_summary_high, m11_f_summary_high), 3)
colnames(m11_summary_high) <- c('protien', 'carbs', 'veg', 'corn', 'fruit', 'fat', 'obs')
rownames(m11_summary_high) <- c('Month 11: male >= .8 eff', 'female >= .8 eff')

### month 11 subsetting low efficiency ########################

m11_list_bot <- which(agg_eff$m11 < .8)
m11_eff_low <- cbind(data11[c(m11_list_bot), 2:10], agg_eff[c(m11_list_bot), 'm11'])

m11_eff_m <- subset(m11_eff_low, m11_eff_low$gen=='M', select = c(1:10))
m11_eff_f <- subset(m11_eff_low, m11_eff_low$gen=='F', select = c(1:10))

attach(m11_eff_m)
m11_m_summary_low <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                           mean(fruit), mean(fat), nrow(m11_eff_m))
detach(m11_eff_m)

attach(m11_eff_f)
m11_f_summary_low <- cbind(mean(protein), mean(carbs), mean(veg), mean(corn),
                           mean(fruit), mean(fat), nrow(m11_eff_f))
detach(m11_eff_f)

m11_summary_low <- round(rbind(m11_m_summary_low, m11_f_summary_low), 3)
colnames(m11_summary_low) <- c('protien', 'carbs', 'veg', 'corn', 'fruit', 'fat', 'obs')
rownames(m11_summary_low) <- c('month 11: male < .8 eff', 'female < .8 eff')

m11_summary <- rbind(m11_summary_high, m11_summary_low)

freq_summary_low <- rbind(m1_summary_low, m2_summary_low, m3_summary_low, m4_summary_low, m5_summary_low,
                          m6_summary_low, m7_summary_low, m8_summary_low, m9_summary_low, m10_summary_low,
                          m11_summary_low)

freq_summary_high <- rbind(m1_summary_high, m2_summary_high, m3_summary_high, m4_summary_high, m5_summary_high,
                          m6_summary_high, m7_summary_high, m8_summary_high, m9_summary_high, m10_summary_high,
                          m11_summary_high)


### village effeciency summary #################################

rownames(m1_eff_high) <- NULL
m1c10 <- nrow(m1_eff_high[1, ])
m1c13 <- nrow(m1_eff_high[2, ])
m1c2 <- nrow(m1_eff_high[3, ])
m1c28 <- 0
m1c35 <- nrow(m1_eff_high[4:6, ])
m1c49 <- nrow(m1_eff_high[7:17, ])
m1c53 <- nrow(m1_eff_high[18:28, ])
m1c55 <- nrow(m1_eff_high[29:36, ])
m1c67 <- 0
m1c68 <- nrow(m1_eff_high[37, ])
comp_m1 <- c(m1c10, m1c13, m1c2, m1c28, m1c35, m1c49, m1c53, m1c55, m1c67, m1c68)

rownames(m2_eff_high) <- NULL
m2c10 <- nrow(m2_eff_high[1, ])
m2c13 <- nrow(m2_eff_high[2, ])
m2c2 <- 0
m2c28 <- nrow(m2_eff_high[3:5, ])
m2c35 <- nrow(m2_eff_high[6:8, ])
m2c49 <- nrow(m2_eff_high[9:16, ])
m2c53 <- nrow(m2_eff_high[17:25, ])
m2c55 <- nrow(m2_eff_high[26:29, ])
m2c67 <- 0
m2c68 <- 0
comp_m2 <- c(m2c10, m2c13, m2c2, m2c28, m2c35, m2c49, m2c53, m2c55, m2c67, m2c68)

rownames(m3_eff_high) <- NULL
m3c10 <- nrow(m3_eff_high[1:2, ])
m3c13 <- 0
m3c2 <- 0
m3c28 <- 0
m3c35 <- nrow(m3_eff_high[3, ])
m3c49 <- nrow(m3_eff_high[4:7, ])
m3c53 <- nrow(m3_eff_high[8:10, ])
m3c55 <- nrow(m3_eff_high[11:13, ])
m3c67 <- 0
m3c68 <- nrow(m3_eff_high[14, ])
comp_m3 <- c(m3c10, m3c13, m3c2, m3c28, m3c35, m3c49, m3c53, m3c55, m3c67, m3c68)

rownames(m4_eff_high) <- NULL
m4c10 <- nrow(m4_eff_high[1, ])
m4c13 <- nrow(m4_eff_high[2:8, ])
m4c2 <- nrow(m4_eff_high[9:11, ])
m4c28 <- nrow(m4_eff_high[12:13, ])
m4c35 <- nrow(m4_eff_high[14, ])
m4c49 <- nrow(m4_eff_high[15:25, ])
m4c53 <- nrow(m4_eff_high[26:31, ])
m4c55 <- nrow(m4_eff_high[32:37, ])
m4c67 <- nrow(m4_eff_high[38, ])
m4c68 <- nrow(m4_eff_high[39, ])
comp_m4 <- c(m4c10, m4c13, m4c2, m4c28, m4c35, m4c49, m4c53, m4c55, m4c67, m4c68)

rownames(m5_eff_high) <- NULL
m5c10 <- nrow(m5_eff_high[1:3, ])
m5c13 <- nrow(m5_eff_high[4:8, ])
m5c2 <- nrow(m5_eff_high[9:15, ])
m5c28 <- nrow(m5_eff_high[16:17, ])
m5c35 <- nrow(m5_eff_high[18:20, ])
m5c49 <- nrow(m5_eff_high[21:29, ])
m5c53 <- nrow(m5_eff_high[30:34, ])
m5c55 <- nrow(m5_eff_high[35:40, ])
m5c67 <- nrow(m5_eff_high[41, ])
m5c68 <- 0
comp_m5 <- c(m5c10, m5c13, m5c2, m5c28, m5c35, m5c49, m5c53, m5c55, m5c67, m5c68)

rownames(m6_eff_high) <- NULL
m6c10 <- nrow(m6_eff_high[1, ])
m6c13 <- nrow(m6_eff_high[2, ])
m6c2 <- nrow(m6_eff_high[3:7, ])
m6c28 <- nrow(m6_eff_high[8, ])
m6c35 <- nrow(m6_eff_high[9, ])
m6c49 <- nrow(m6_eff_high[10:11, ])
m6c53 <- nrow(m6_eff_high[12:15, ])
m6c55 <- nrow(m6_eff_high[16:17, ])
m6c67 <- 0
m6c68 <- nrow(m6_eff_high[18, ])
comp_m6 <- c(m6c10, m6c13, m6c2, m6c28, m6c35, m6c49, m6c53, m6c55, m6c67, m6c68)

rownames(m7_eff_high) <- NULL
m7c10 <- nrow(m7_eff_high[1:3, ])
m7c13 <- nrow(m7_eff_high[4:6, ])
m7c2 <- nrow(m7_eff_high[7:11, ])
m7c28 <- nrow(m7_eff_high[12, ])
m7c35 <- nrow(m7_eff_high[13:14, ])
m7c49 <- nrow(m7_eff_high[15:23, ])
m7c53 <- nrow(m7_eff_high[24:32, ])
m7c55 <- nrow(m7_eff_high[33:41, ])
m7c67 <- nrow(m7_eff_high[42, ])
m7c68 <- nrow(m7_eff_high[43:44, ])
comp_m7 <- c(m7c10, m7c13, m7c2, m7c28, m7c35, m7c49, m7c53, m7c55, m7c67, m7c68)

rownames(m8_eff_high) <- NULL
m8c10 <- nrow(m8_eff_high[1:2, ])
m8c13 <- nrow(m8_eff_high[3, ])
m8c2 <- nrow(m8_eff_high[4:5, ])
m8c28 <- nrow(m8_eff_high[6:8, ])
m8c35 <- nrow(m8_eff_high[9:10, ])
m8c49 <- nrow(m8_eff_high[11:13, ])
m8c53 <- 0
m8c55 <- nrow(m8_eff_high[14:15, ])
m8c67 <- nrow(m8_eff_high[16, ])
m8c68 <- nrow(m8_eff_high[17:18, ])
comp_m8 <- c(m8c10, m8c13, m8c2, m8c28, m8c35, m8c49, m8c53, m8c55, m8c67, m8c68)

rownames(m9_eff_high) <- NULL
m9c10 <- nrow(m9_eff_high[1:4, ])
m9c13 <- nrow(m9_eff_high[5:7, ])
m9c2 <- nrow(m9_eff_high[8:11, ])
m9c28 <- nrow(m9_eff_high[12, ])
m9c35 <- nrow(m9_eff_high[13:15, ])
m9c49 <- nrow(m9_eff_high[16:26, ])
m9c53 <- nrow(m9_eff_high[27:31, ])
m9c55 <- nrow(m9_eff_high[32:35, ])
m9c67 <- nrow(m9_eff_high[36, ])
m9c68 <- 0
comp_m9 <- c(m9c10, m9c13, m9c2, m9c28, m9c35, m9c49, m9c53, m9c55, m9c67, m9c68)

rownames(m10_eff_high) <- NULL
m10c10 <- nrow(m10_eff_high[1:3, ])
m10c13 <- nrow(m10_eff_high[4:5, ])
m10c2 <- nrow(m10_eff_high[6:7, ])
m10c28 <- 0
m10c35 <- nrow(m10_eff_high[8, ])
m10c49 <- nrow(m10_eff_high[9:13, ])
m10c53 <- nrow(m10_eff_high[14, ])
m10c55 <- nrow(m10_eff_high[15, ])
m10c67 <- nrow(m10_eff_high[16, ])
m10c68 <- nrow(m10_eff_high[17:18, ])
comp_m10 <- c(m10c10, m10c13, m10c2, m10c28, m10c35, m10c49, m10c53, m10c55, m10c67, m10c68)

rownames(m11_eff_high) <- NULL
m11c10 <- nrow(m11_eff_high[1:3, ])
m11c13 <- 0
m11c2 <- nrow(m11_eff_high[4, ])
m11c28 <- 0
m11c35 <- 0
m11c49 <- nrow(m11_eff_high[5:7, ])
m11c53 <- nrow(m11_eff_high[8:11, ])
m11c55 <- nrow(m11_eff_high[12:14, ])
m11c67 <- 0
m11c68 <- 0
comp_m11 <- c(m11c10, m11c13, m11c2, m11c28, m11c35, m11c49, m11c53, m11c55, m11c67, m11c68)

comp_eff_freq <- cbind(comp_m1, comp_m2, comp_m3, comp_m4, comp_m5, comp_m6, comp_m7, comp_m8,
                       comp_m9, comp_m10, comp_m11)

comp_sum <- c(sum(comp_eff_freq[1, ]), sum(comp_eff_freq[2, ]), sum(comp_eff_freq[3, ]), sum(comp_eff_freq[4, ]),
              sum(comp_eff_freq[5, ]), sum(comp_eff_freq[6, ]), sum(comp_eff_freq[7, ]), sum(comp_eff_freq[8, ]),
              sum(comp_eff_freq[9, ]), sum(comp_eff_freq[10, ]))

comp_eff_freq <- cbind(comp_eff_freq, comp_sum)

colnames(comp_eff_freq) <- c('m1', 'm2', 'm3', 'm4', 'm5', 'm6', 'm7', 'm8', 'm9', 'm10', 'm11', 'frequency total')
rownames(comp_eff_freq) <- c('Compounds: 10', '13', '2', '28', '35', '49', '53', '55', '67', '68')


### efficiency food group impacts ##################################
data1_eff <- cbind(data1, agg_eff$m1, 1-agg_eff$m1)
names(data1_eff)[12:13] <- c('effm1', 'ineffm1')

data2_eff <- cbind(data2, agg_eff$m2, 1-agg_eff$m2)
names(data2_eff)[12:13] <- c('effm2', 'ineffm2')

data3_eff <- cbind(data3, agg_eff$m3, 1-agg_eff$m3)
names(data3_eff)[12:13] <- c('effm3', 'ineffm3')

data4_eff <- cbind(data4, agg_eff$m4, 1-agg_eff$m4)
names(data4_eff)[12:13] <- c('effm4', 'ineffm4')

data5_eff <- cbind(data5, agg_eff$m5, 1-agg_eff$m5)
names(data5_eff)[12:13] <- c('effm5', 'ineffm5')

data6_eff <- cbind(data6, agg_eff$m6, 1-agg_eff$m6)
names(data6_eff)[12:13] <- c('effm6', 'ineffm6')

data7_eff <- cbind(data7, agg_eff$m7, 1-agg_eff$m7)
names(data7_eff)[12:13] <- c('effm7', 'ineffm7')

data8_eff <- cbind(data8, agg_eff$m8, 1-agg_eff$m8)
names(data8_eff)[12:13] <- c('effm8', 'ineffm8')

data9_eff <- cbind(data9, agg_eff$m9, 1-agg_eff$m9)
names(data9_eff)[12:13] <- c('effm9', 'ineffm9')

data10_eff <- cbind(data10, agg_eff$m10, 1-agg_eff$m10)
names(data10_eff)[12:13] <- c('effm10', 'ineffm10')

data11_eff <- cbind(data11, agg_eff$m11, 1-agg_eff$m11)
names(data11_eff)[12:13] <- c('effm11', 'ineffm11')


### efficiency mapping for village means across months ##########

rownames(agg_eff) <- NULL

mapc10 <- rbind(mean(agg_eff[1:14, 'm1']), mean(agg_eff[1:14, 'm2']), mean(agg_eff[1:14, 'm3']), 
                mean(agg_eff[1:14, 'm4']), mean(agg_eff[1:14, 'm5']), mean(agg_eff[1:14, 'm6']), 
                mean(agg_eff[1:14, 'm7']), mean(agg_eff[1:14, 'm8']), mean(agg_eff[1:14, 'm9']), 
                mean(agg_eff[1:14, 'm10']), mean(agg_eff[1:14, 'm11']))

mapc13 <- rbind(mean(agg_eff[15:38, 'm1']), mean(agg_eff[15:38, 'm2']), mean(agg_eff[15:38, 'm3']), 
                mean(agg_eff[15:38, 'm4']), mean(agg_eff[15:38, 'm5']), mean(agg_eff[15:38, 'm6']), 
                mean(agg_eff[15:38, 'm7']), mean(agg_eff[15:38, 'm8']), mean(agg_eff[15:38, 'm9']), 
                mean(agg_eff[15:38, 'm10']), mean(agg_eff[15:38, 'm11']))

mapc2 <- rbind(mean(agg_eff[39:65, 'm1']), mean(agg_eff[39:65, 'm2']), mean(agg_eff[39:65, 'm3']), 
               mean(agg_eff[39:65, 'm4']), mean(agg_eff[39:65, 'm5']), mean(agg_eff[39:65, 'm6']), 
               mean(agg_eff[39:65, 'm7']), mean(agg_eff[39:65, 'm8']), mean(agg_eff[39:65, 'm9']), 
               mean(agg_eff[39:65, 'm10']), mean(agg_eff[39:65, 'm11']))

mapc28 <- rbind(mean(agg_eff[66:82, 'm1']), mean(agg_eff[66:82, 'm2']), mean(agg_eff[66:82, 'm3']), 
                mean(agg_eff[66:82, 'm4']), mean(agg_eff[66:82, 'm5']), mean(agg_eff[66:82, 'm6']), 
                mean(agg_eff[66:82, 'm7']), mean(agg_eff[66:82, 'm8']), mean(agg_eff[66:82, 'm9']), 
                mean(agg_eff[66:82, 'm10']), mean(agg_eff[66:82, 'm11']))

mapc35 <- rbind(mean(agg_eff[83:92, 'm1']), mean(agg_eff[83:92, 'm2']), mean(agg_eff[83:92, 'm3']), 
                mean(agg_eff[83:92, 'm4']), mean(agg_eff[83:92, 'm5']), mean(agg_eff[83:92, 'm6']), 
                mean(agg_eff[83:92, 'm7']), mean(agg_eff[83:92, 'm8']), mean(agg_eff[83:92, 'm9']), 
                mean(agg_eff[83:92, 'm10']), mean(agg_eff[83:92, 'm11']))

mapc49 <- rbind(mean(agg_eff[93:137, 'm1']), mean(agg_eff[93:137, 'm2']), mean(agg_eff[93:137, 'm3']), 
                mean(agg_eff[93:137, 'm4']), mean(agg_eff[93:137, 'm5']), mean(agg_eff[93:137, 'm6']), 
                mean(agg_eff[93:137, 'm7']), mean(agg_eff[93:137, 'm8']), mean(agg_eff[93:137, 'm9']), 
                mean(agg_eff[93:137, 'm10']), mean(agg_eff[93:137, 'm11']))

mapc53 <- rbind(mean(agg_eff[138:160, 'm1']), mean(agg_eff[138:160, 'm2']), mean(agg_eff[138:160, 'm3']), 
                mean(agg_eff[138:160, 'm4']), mean(agg_eff[138:160, 'm5']), mean(agg_eff[138:160, 'm6']), 
                mean(agg_eff[138:160, 'm7']), mean(agg_eff[138:160, 'm8']), mean(agg_eff[138:160, 'm9']), 
                mean(agg_eff[138:160, 'm10']), mean(agg_eff[138:160, 'm11']))

mapc55 <- rbind(mean(agg_eff[161:194, 'm1']), mean(agg_eff[161:194, 'm2']), mean(agg_eff[161:194, 'm3']), 
                mean(agg_eff[161:194, 'm4']), mean(agg_eff[161:194, 'm5']), mean(agg_eff[161:194, 'm6']), 
                mean(agg_eff[161:194, 'm7']), mean(agg_eff[161:194, 'm8']), mean(agg_eff[161:194, 'm9']), 
                mean(agg_eff[161:194, 'm10']), mean(agg_eff[161:194, 'm11']))

mapc67 <- rbind(mean(agg_eff[195:197, 'm1']), mean(agg_eff[195:197, 'm2']), mean(agg_eff[195:197, 'm3']), 
                mean(agg_eff[195:197, 'm4']), mean(agg_eff[195:197, 'm5']), mean(agg_eff[195:197, 'm6']), 
                mean(agg_eff[195:197, 'm7']), mean(agg_eff[195:197, 'm8']), mean(agg_eff[195:197, 'm9']), 
                mean(agg_eff[195:197, 'm10']), mean(agg_eff[195:197, 'm11']))

mapc68 <- rbind(mean(agg_eff[198:207, 'm1']), mean(agg_eff[198:207, 'm2']), mean(agg_eff[198:207, 'm3']), 
                mean(agg_eff[198:207, 'm4']), mean(agg_eff[198:207, 'm5']), mean(agg_eff[198:207, 'm6']), 
                mean(agg_eff[198:207, 'm7']), mean(agg_eff[198:207, 'm8']), mean(agg_eff[198:207, 'm9']), 
                mean(agg_eff[198:207, 'm10']), mean(agg_eff[198:207, 'm11']))

mapping <- data.frame(cbind(mapc10, mapc13, mapc2, mapc28, mapc35, mapc49, mapc53, mapc55, mapc67, mapc68,
                            1:11))
colnames(mapping) <- c('v10', 'v13', 'v2', 'v28', 'v35', 'v49', 'v53', 'v55', 'v67', 'v68', 'x')
rownames(mapping) <- c('jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec', 'jan', 'feb', 'mar', 'apr')

map_point <- ggplot(mapping, aes(x)) +
  geom_line(aes(y = v10), alpha = 0.5) +
  geom_point(aes(y = v10, shape = 'v10'), size = 4) +
  geom_line(aes(y = v13), alpha = 0.5) +
  geom_point(aes(y = v13, shape = 'v13'), size = 4) +
  geom_line(aes(y = v2), alpha = 0.5) +
  geom_point(aes(y = v2, shape = 'v2'), size = 4) +
  geom_line(aes(y = v28), alpha = 0.5) +
  geom_point(aes(y = v28, shape = 'v28'), size = 4) +
  geom_line(aes(y = v35), alpha = 0.5) +
  geom_point(aes(y = v35, shape = 'v35'), size = 4) +
  geom_line(aes(y = v49), alpha = 0.5) +
  geom_point(aes(y = v49, shape = 'v49'), size = 4) +
  geom_line(aes(y = v53), alpha = 0.5) +
  geom_point(aes(y = v53, shape = 'v53'), size = 4) +
  geom_line(aes(y = v55), alpha = 0.5) +
  geom_point(aes(y = v55, shape = 'v55'), size = 4) +
  geom_line(aes(y = v67), alpha = 0.5) +
  geom_point(aes(y = v67, shape = 'v67'), size = 4) +
  geom_line(aes(y = v68), alpha = 0.5) +
  geom_point(aes(y = v68, shape = 'v68'), size = 4) +
  scale_shape_manual(name = 'Villages', 
                     labels = c('v10', 'v13', 'v2', 'v28', 'v35', 'v49', 'v53', 'v55', 'v67', 'v68'),
                     values = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)) +
  scale_x_discrete(limits = c('June', 'July', 'Aug', 'Sep', 'Oct','Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr')) +
  scale_y_continuous(breaks = seq(from = 0, to = 1, by = .1), limits = c(0, 1)) +
  labs(title = "Village Average Efficiency, Monthly (2014/15 yr)", x = '', y = "Efficiency") +
  theme(plot.title = element_text(size = 20, face = 'bold', hjust = .5),
        axis.title = element_text(size = 15, face = 'bold'))+
  geom_vline(xintercept = c(3, 6, 8), color = 'red', size = 1, alpha = .3)+
  annotate("text", x = 6, y = .125, label = c("Number of high efficiency males (M) and females (F), >= .8 cutoff:")) +
  annotate('text', x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), y = .065, 
           label = c('M 18', 'M 9', 'M 7', 'M 22', 'M 16', 'M 7', 'M 21', 'M 9', 'M 19', 'M 8', 'M 4')) +
  annotate('text', x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), y = .035, 
           label = c('F 19', 'F 20', 'F 7', 'F 17', 'F  25', 'F 11', 'F 23', 'F 9', 'F 17', 'F 10', 'F 10'))

#write.csv(mapping, file = 'monthly_village_av_eff.csv')


### merging efficiency data set and household model data ###############################################

regm1 <- read.csv('regm1.csv') 

regm1 <- left_join(data1_eff, regm1, by="hhid") %>%
  mutate(chunder5 = malesunder5 + femunder5,
         ch5to10 = males5to10 + fem5to10) %>%
  na.omit(regm1) 
regm1 <- bind_cols(regm1, month = rep(6, nrow(regm1)))
colnames(regm1)[12:13] <- c('eff', 'ineff')


regm2 <- read.csv('regm2.csv') 

regm2 <- left_join(data2_eff, regm2, by="hhid") %>%
  mutate(chunder5 = malesunder5 + femunder5,
         ch5to10 = males5to10 + fem5to10) %>%
  na.omit(regm2) 
regm2 <- bind_cols(regm2, month = rep(7, nrow(regm2)))
colnames(regm2)[12:13] <- c('eff', 'ineff')


regm3 <- read.csv('regm3.csv') 

regm3 <- left_join(data3_eff, regm3, by="hhid") %>%
  mutate(chunder5 = malesunder5 + femunder5,
         ch5to10 = males5to10 + fem5to10) %>%
  na.omit(regm3) 
regm3 <- bind_cols(regm3, month = rep(8, nrow(regm3)))
colnames(regm3)[12:13] <- c('eff', 'ineff')


regm4 <- read.csv('regm4.csv') 

regm4 <- left_join(data4_eff, regm4, by="hhid") %>%
  mutate(chunder5 = malesunder5 + femunder5,
         ch5to10 = males5to10 + fem5to10) %>%
  na.omit(regm4) 
regm4 <- bind_cols(regm4, month = rep(9, nrow(regm4)))
colnames(regm4)[12:13] <- c('eff', 'ineff')


regm5 <- read.csv('regm5.csv') 

regm5 <- left_join(data5_eff, regm5, by="hhid") %>%
  mutate(chunder5 = malesunder5 + femunder5,
         ch5to10 = males5to10 + fem5to10) %>%
  na.omit(regm5) 
regm5 <- bind_cols(regm5, month = rep(10, nrow(regm5)))
colnames(regm5)[12:13] <- c('eff', 'ineff')


regm6 <- read.csv('regm6.csv') 

regm6 <- left_join(data6_eff, regm6, by="hhid") %>%
  mutate(chunder5 = malesunder5 + femunder5,
         ch5to10 = males5to10 + fem5to10) %>%
  na.omit(regm6) 
regm6 <- bind_cols(regm6, month = rep(11, nrow(regm6)))
colnames(regm6)[12:13] <- c('eff', 'ineff')


regm7 <- read.csv('regm7.csv') 

regm7 <- left_join(data7_eff, regm7, by="hhid") %>%
  mutate(chunder5 = malesunder5 + femunder5,
         ch5to10 = males5to10 + fem5to10) %>%
  na.omit(regm7) 
regm7 <- bind_cols(regm7, month = rep(12, nrow(regm7)))
colnames(regm7)[12:13] <- c('eff', 'ineff')


regm8 <- read.csv('regm8.csv') 

regm8 <- left_join(data8_eff, regm8, by="hhid") %>%
  mutate(chunder5 = malesunder5 + femunder5,
         ch5to10 = males5to10 + fem5to10) %>%
  na.omit(regm8) 
regm8 <- bind_cols(regm8, month = rep(1, nrow(regm8)))
colnames(regm8)[12:13] <- c('eff', 'ineff')


regm9 <- read.csv('regm9.csv') 

regm9 <- left_join(data9_eff, regm9, by="hhid") %>%
  mutate(chunder5 = malesunder5 + femunder5,
         ch5to10 = males5to10 + fem5to10) %>%
  na.omit(regm9) 
regm9 <- bind_cols(regm9, month = rep(2, nrow(regm9)))
colnames(regm9)[12:13] <- c('eff', 'ineff')


regm10 <- read.csv('regm10.csv') 

regm10 <- left_join(data10_eff, regm10, by="hhid") %>%
  mutate(chunder5 = malesunder5 + femunder5,
         ch5to10 = males5to10 + fem5to10) %>%
  na.omit(regm10) 
regm10 <- bind_cols(regm10, month = rep(3, nrow(regm10)))
colnames(regm10)[12:13] <- c('eff', 'ineff')


regm11 <- read.csv('regm11.csv') 

regm11 <- left_join(data11_eff, regm11, by="hhid") %>%
  mutate(chunder5 = malesunder5 + femunder5,
         ch5to10 = males5to10 + fem5to10) %>%
  na.omit(regm11) 
regm11 <- bind_cols(regm11, month = rep(4, nrow(regm11))) 
colnames(regm11)[12:13] <- c('eff', 'ineff')
regm11$totallivinc <- as.numeric(as.character(regm11$totallivinc))

reg_data_r <- bind_rows(regm1, regm2, regm3, regm4, regm5, regm6, regm7, regm8, regm9, regm10, regm11)
fac_cols <- c('maxedu', 'watersource', 'fac_watersource', 'month', 'latrine', 'fac_latrine')
reg_data_r[fac_cols] <- lapply(reg_data_r[fac_cols], factor)

str(reg_data_r)

# write.csv(reg_data_r, file = 'reg_data_r.csv')


