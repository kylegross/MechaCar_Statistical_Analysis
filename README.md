# MechaCar Statistical Analysis
## Linear Regression to Predict MPG
- The variables that provided a non-random amount of variance to the mpg values in the dataset are the vehicle length and ground clearance. Both variables produced notably low p-values when analyzed using the multiple linear regression statisical model. The low p-values produced are statisically significant, wherein both the vehicle length and ground clearance directly impact the mpg.
- The p-value is 5.35e-11, which is much lower than the standard threshold p-value of 0.05. As such, we reject our null hypothesis, making the slope of the linear model not equal to zero.
- This linear model does predict the mpg of MechaCar prototypes effectively because the r-squared value is 0.68, meaning that the regression model is able to predict the total mpg ~68% of the time.
![Linear_Regression_Output](https://github.com/kylegross/MechaCar_Statistical_Analysis/blob/main/Linear_Regression_Output.PNG)

## Summary Statistics on Suspension Coils
In analyzing the overall statistics on suspension coil manufacturing, the design specification requirements of under 100 pounds/square inch variance are met, with the total overall variance calculated at 62.29. That said, when analyzing the lots individually, only Lot 1 and Lot 2 meet the design specifications at 0.98 and 7.47 pounds/square inch variance, respectively. Lot 3, however, does not meet the design specifications, with a total of 170.69 pounds/square inch variance. As is evident in the images below, the suspension coil variance in Lot 3 has a large impact on the summarized total variance for all three lots, so it is recommended that Lot 3 be analyzed further to figure out why there is such a large difference in suspension coil pounds/square inch variance. The additional analysis is important to satisfy the design specification requirements of under 100 pounds.
![total_summary](https://github.com/kylegross/MechaCar_Statistical_Analysis/blob/main/total_summary.PNG)
![lot_summary](https://github.com/kylegross/MechaCar_Statistical_Analysis/blob/main/lot_summary.PNG)

## T-Tests on Suspension Coils
The t-test results across all manufacturing lots produced a p-value of 0.05734, which is much smaller than the significance level of 0.5. As the calculated p-value is smaller than the significance level, there is sufficient statistical evidence that the null hypothesis is not true, so the null hypothesis must be rejected. When running the t-test across each lot, lot 1 and lot 2 produced a p-value of 0.9982 and 0.6115, respectively. Because the calculated p-values are larger than the 0.5 significance level, there is not sufficience evidence to reject the null hypothesis, indicating a failure to reject the null hypothesis. As such, both lot 1 and lot 2 are not statistically different from the population mean of 1500 pounds/square inch. The t-test result for lot 3 is similar to the results across all manufacturing lots, with a p-value of 0.03966. Once again, the p-value is smaller than the significance level, so there is sufficient statistical evidence that the null hypothesis is not true and must be rejected.
![T-Test_SuspensionCoils](https://github.com/kylegross/MechaCar_Statistical_Analysis/blob/main/T-Test_SuspensionCoils.PNG)

## Study Design: MechaCar vs. Competition
Several metrics must be studied to quantify how the MechaCar performs against the competition. I would evaluate the overall cost, maintenance costs, highway fuel efficiency for both city and highway driving, horse power, and the safety rating. To evaluate the different cars, I would conduct a two-sample t-test, wherein the null hypothesis would be that the two cars evaluated are not statistically different across the metrics mentioned above. I recommend the two-sample t-test because we are comparing measurements of the same object type (the MechaCar and the competitor's car), and we are also comparing different methods of measurement to satisfy the variant metrics. The data required to conduct the analysis for both the MechaCar and the competitors' car(s) would be the cost evaluation of each car, maintenance data including total expenses, fuel consumption data and distance traveled (for both city and highway travel), recorded engine data to evaluate total horsepower, and safety rating data.
