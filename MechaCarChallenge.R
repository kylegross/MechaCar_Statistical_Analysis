# Challenge Deliverable 1
# 1. Import dplyr library
install.packages("dplyr")
library(dplyr)
# 2. Import and read MechaCar_mpg.csv file as df
MechaCar_df <- read.csv(file='MechaCar_mpg.csv',check.names=F,stringsAsFactors = F)
head(MechaCar_df,6)
# 3. Perform linear regression using the lm() function
lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = MechaCar_df)
# 4. Summary of linear regression model using summary()
summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance + AWD, data = MechaCar_df))
#
# Challenge Deliverable 2
# 1. Import and read Suspension_Coil.csv file as table
SC_Table <- read.csv(file='Suspension_Coil.csv',check.names=F,stringsAsFactors = F)
# 2. Create a total_summary df using the summarize() function
total_summary <- SC_Table %>% summarize(mean=mean(PSI),median=median(PSI),var=var(PSI),sd=sd(PSI))
# 3. Create a lot_summary df using summarize() function
lot_summary <- SC_Table %>% group_by(Manufacturing_Lot) %>% summarize(mean=mean(PSI),median=median(PSI),var=var(PSI),sd=sd(PSI), .groups='keep')
#
# Challenge Deliverable 3
# 1. Determine if the PSI across all manufacturing lots is statistically
# different from the population mean of 1,500 pounds/square inch.
t.test(log10(SC_Table$PSI),mu=mean(log10(1500)))
# 2. Determine if the PSI for each manufacturing lot is statistically
# different from the population mean of 1,500 pounds/square inch
t.test(subset(log10(SC_Table$PSI),SC_Table$Manufacturing_Lot=='Lot1'),mu=mean(log10(1500)))
t.test(subset(log10(SC_Table$PSI),SC_Table$Manufacturing_Lot=='Lot2'),mu=mean(log10(1500)))
t.test(subset(log10(SC_Table$PSI),SC_Table$Manufacturing_Lot=='Lot3'),mu=mean(log10(1500)))
q()
