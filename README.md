# LAB3-

# Creation of fator
> dat_NYC <- subset(acs2017_ny, (acs2017_ny$in_NYC == 1)&(acs2017_ny$AGE > 20) & (acs2017_ny$AGE < 66))
> attach(dat_NYC)
> borough_f <- factor((in_Bronx + 2*in_Manhattan + 3*in_StatenI + 4*in_Brooklyn + 5*in_Queens), levels=c(1,2,3,4,5),labels = c("Bronx","Manhattan","Staten Island","Brooklyn","Queens"))

# Normalization of the data
> norm_varb <- function(X_in) {
+   (max(X_in, na.rm = TRUE) - X_in)/( max(X_in, na.rm = TRUE) - min(X_in, na.rm = TRUE) )
+ }
> 
> is.na(OWNCOST) <- which(OWNCOST == 9999999)
> is.na(COSTELEC) <- which(COSTELEC == 9999999)
> is.na(COSTWATR) <- which(COSTWATR == 9999999)
> housing_COST <- OWNCOST + RENT + COSTELEC + COSTWATR
> norm_HH_inc <- norm_varb(HHINCOME)
> norm_housing_cost <- norm_varb(housing_cost)
> 
# creation of data frame

> data_use_prelim <- data.frame(norm_HH_inc,norm_housing_cost)
> good_obs_data_use <- complete.cases(data_use_prelim,borough_f)
> dat_use <- subset(data_use_prelim,good_obs_data_use)
> y_use <- subset(borough_f,good_obs_data_use)
> 
# Next split the data into 2 parts: one part to train the algo, then the other part to test how well it works for new data. Here we use an 70/30 split

> set.seed(12345)
> NN_obs <- sum(good_obs_data_use == 1)
> select1 <- (runif(NN_obs) < 0.7)
> train_data <- subset(dat_use,select1)
> test_data <- subset(dat_use,(!select1))
> cl_data <- y_use[select1]
> true_data <- y_use[!select1]
> 
# Finally run the k-nn algo and compare against the simple means

> summary(cl_data)
        Bronx     Manhattan Staten Island      Brooklyn        Queens 
         4052          4294          1587         10550          9399 
> prop.table(summary(cl_data))
        Bronx     Manhattan Staten Island      Brooklyn        Queens 
   0.13560003    0.14369855    0.05310889    0.35305535    0.31453718 
> summary(train_data)
  norm_HH_inc     norm_housing_cost
 Min.   :0.0000   Min.   :0.0000   
 1st Qu.:0.9229   1st Qu.:0.7246   
 Median :0.9538   Median :0.8130   
 Mean   :0.9373   Mean   :0.7472   
 3rd Qu.:0.9755   3rd Qu.:0.8695   
 Max.   :1.0000   Max.   :0.9965   
> require(class)
> for (indx in seq(1, 9, by= 2)) {
+   pred_borough <- knn(train_data, test_data, cl_data, k = indx, l = 0, prob = FALSE, use.all = TRUE)
+   num_correct_labels <- sum(pred_borough == true_data)
+   correct_rate <- num_correct_labels/length(true_data)
+   print(c(indx,correct_rate))
+ }
[1] 1.0000000 0.7017405
[1] 3.0000000 0.4642405
[1] 5.000000 0.447231
[1] 7.0000000 0.4322785
[1] 9.0000000 0.4268196
