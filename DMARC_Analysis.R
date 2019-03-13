library(sqldf)
library(car)
library(ggplot2)
library(plyr)
#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/ggpubr", force = TRUE)
library(ggpubr)
#install.packages("eeptools")
library(eeptools)

fc <- read.csv('/Users/tannerthurman/Desktop/DMARC data/drakeExport_foodChoices.csv', header = T) #fc raw data
hs <- read.csv('/Users/tannerthurman/Desktop/DMARC data/drakeExport_served_households.csv', header = T) #Households raw data
visits <- read.csv('/Users/tannerthurman/Desktop/DMARC data/drakeExport_visits.csv', header = T) #Visits raw data
inventory_wide <- read.csv('/Users/tannerthurman/Desktop/DMARC data/inventory_wide.csv', header = T)
inventory <- read.csv('/Users/tannerthurman/Desktop/DMARC data/inventory.csv', header = T)   #pass in the inventory file in here (not the wide one)

fc <- fc[which(fc$nutriScoreValue != "0" & fc$nutriScoreValue != "ns"),]

merged <- merge(fc, visits, by = "trans_id", all.x = FALSE, all.y = FALSE)
merged$time <- as.Date(merged$ts.x, format = "%Y-%m-%d")
merged$time_bin <- paste("before", merged$time < "2017-09-01", sep = "")

merged <- subset(merged, !nutriScoreValue %in% c("0", "ns"))
merged$nutriScoreValue <- as.numeric(as.character(merged$nutriScoreValue))
merged$month <- format(merged$time, "%m%Y")
merged_agg <- ddply(merged, .(time_bin, numInHousehold, afn.y, month), summarise, nitems = length(time), mean_nutriscore = mean(nutriScoreValue))
merged_agg$numInHousehold <- as.factor(merged_agg$numInHousehold)
merged_agg$optimal <- as.numeric(as.character(merged_agg$numInHousehold))*36/merged_agg$nitems
merged_agg$optimal[merged_agg$optimal > 5] <- 5

merged_agg_sub <- subset(merged_agg, numInHousehold == "3" & nitems <= as.numeric(numInHousehold)*36)

ggplot(data = merged_agg_sub) + geom_point(aes(y=nitems, x=mean_nutriscore, colour = time_bin), alpha = I(.4)) + geom_line(aes(y = nitems, x = optimal))

ggscatterhist(
  merged_agg_sub, y = "nitems", x = "mean_nutriscore",
  alpha = I(.2),
  color = "time_bin",
  margin.plot = "density",
  margin.params = list(fill = "time_bin", color = "black", size = 0.6),
  title = "Purchasing Behavior for 3-person Households"
)

# Getting average nutri score for inventory during a time period
inv_avg_nutri = sqldf("select StartDate, EndDate, avg(Rating) as 'avgInvRating' from inventory group by StartDate, EndDate")

inv_avg_nutri$StartDate <- as.Date(inv_avg_nutri$StartDate, format = '%Y-%m-%d')
inv_avg_nutri$EndDate <- as.Date(inv_avg_nutri$EndDate, format = '%Y-%m-%d')
hs <- hs[,-c(2)] #remove the individual_id field

hsdistinctquery <- 'select afn, served_date, avg(num_male) as num_male, avg(num_female) as num_female, avg(num_african_american) as num_african_american, avg(num_white) as num_white,
avg(num_american_indian) as num_american_indians, avg(num_asian) as num_asian, avg(num_hawaiian_or_pacific_islander) as num_hawaiian_or_pacific_islander,
avg(num_multi_race) as num_multi_race, avg(num_other_race) as num_other_race, avg(hispanic_or_latino) as hispanic_or_latino, avg(not_hispanic) as not_hispanic,
avg(upTo_8thGrade) as upTo_8thGrade, avg(X9to12_nonGrad) as HighSchoolnon_Grad, avg(hsGrad_Ged) as hsGrad_Ged, avg(hsGrad_or_Ged_some_secondary) as hsGrad_or_Ged_some_secondary, 
avg(X2_or_4_college_grad) as college_grad, avg(annual_income) as annual_income, avg(fed_poverty_level) as fed_poverty_level from hs group by afn, served_date'
hs_done <- sqldf(hsdistinctquery)
itemsQuery <- 'select afn, trans_id, ts, avg(nutriScoreValue) as avgNutriScore, count(*) as items from fc group by afn, trans_id, ts order by ts desc' 
fcbyPurchase <- sqldf(itemsQuery)
fcjoinvis <- 'select fcbyPurchase.*, serviceDate, individual_id, gender, race, ethnicity, dob, numInHousehold as hs_size from fcbyPurchase join visits on fcbyPurchase.ts = visits.ts and fcbyPurchase.afn = visits.afn and fcbyPurchase.trans_id = visits.trans_id'
fc_vis <- sqldf(fcjoinvis)
fc_join_hs <- "select distinct * from fc_vis join hs_done on fc_vis.serviceDate = hs_done.served_date and fc_vis.afn = hs_done.afn"
fchs_final <- sqldf(fc_join_hs)

fchs_final$served_date = as.Date(fchs_final$served_date, format = '%Y-%m-%d')
fchs_final <- fchs_final[,-c(6,13)]
fchs_final$hs_size <- as.factor(fchs_final$hs_size)

numeric_hs_size <- as.numeric(as.character(fchs_final$hs_size))

fchs_final$male_ratio <- fchs_final$num_male/numeric_hs_size
fchs_final$female_ratio <- fchs_final$num_female/numeric_hs_size
fchs_final$white_ratio <- fchs_final$num_white/numeric_hs_size
fchs_final$am_indian_ratio <- fchs_final$num_american_indians/numeric_hs_size
fchs_final$african_american_ratio <- fchs_final$num_african_american/numeric_hs_size
fchs_final$asian_ratio <- fchs_final$num_asian/numeric_hs_size
fchs_final$hawaiian_ratio <- fchs_final$num_hawaiian_or_pacific_islander/numeric_hs_size
fchs_final$multi_ratio <- fchs_final$num_multi_race/numeric_hs_size
fchs_final$other_ratio <- fchs_final$num_other_race/numeric_hs_size
fchs_final$hisp_latino_ratio <- fchs_final$hispanic_or_latino/numeric_hs_size
fchs_final$not_hispanic_ratio <- fchs_final$not_hispanic/numeric_hs_size
fchs_final$upTo8thGrade_ratio <- fchs_final$upTo_8thGrade/numeric_hs_size
fchs_final$HighSchoolnon_Grad_ratio <- fchs_final$HighSchoolnon_Grad/numeric_hs_size
fchs_final$HsGrad_Ged_ratio <- fchs_final$hsGrad_Ged/numeric_hs_size
fchs_final$hsGradSomeSec_ratio <- fchs_final$hsGrad_or_Ged_some_secondary/numeric_hs_size
fchs_final$college_ratio <- fchs_final$college_grad/numeric_hs_size

for(i in 1:length(fchs_final$hs_size)){
  if(fchs_final$served_date[i] >= as.Date("2017-09-01", format = "%Y-%m-%d")){   ##Needs to be greater than or equal to
    fchs_final$system_bin[i] <- as.integer(1)
  } else {
    fchs_final$system_bin[i] <- as.integer(0)
  }
}

fchs_final$total_hs_points <- as.numeric(fchs_final$hs_size)*36
fchs_final$total_vis_points <- fchs_final$avgNutriScore*fchs_final$items

#separate race and gender by before and after initiative
searchData0 = sqldf("select race, gender from fchs_final where system_bin = 0")
searchData1 = sqldf("select race, gender from fchs_final where system_bin = 1")

#compare the two data sets. If any race/gender has less than 15 data points in both, remove it
count(searchData0$race)
count(searchData1$race)

count(searchData0$gender)
count(searchData1$gender)

start = c("2017-9-1")
projectStartDate = as.Date(start, format = "%Y-%m-%d")

ggplot(data = fchs_final) + geom_point(aes(x = time, y = avgNutriScore, alpha = I(.4))) + geom_smooth(aes(x = time, y = avgNutriScore, alpha = I(.4), se = FALSE)) + geom_vline(xintercept = projectStartDate, color = "Red")
ggplot(data = fchs_final) + geom_point(aes(x = time, y = items, alpha = I(.4))) + geom_smooth(aes(x = time, y = items, alpha = I(.4), se = FALSE)) + geom_vline(xintercept = projectStartDate, color = "Red")

fchs_final$dob <- as.Date(fchs_final$dob, format = "%Y-%m-%d")
fchs_final <- fchs_final[which(fchs_final$dob < Sys.Date()),]
fchs_final$age <- age_calc(fchs_final$dob, enddate = Sys.Date(), units = "years", precise = TRUE)
fchs_final <- fchs_final[which(fchs_final$age > 20.0),]


addInvRating <- "select fchs_final.*, avgInvRating from fchs_final join inv_avg_nutri on served_date between StartDate and EndDate"
fchs_inv <- sqldf(addInvRating)

ggplot(fchs_final) + 
  geom_point(aes(x = items, y = avgNutriScore, fill = hs_size, colour = hs_size), alpha = I(.4))

ggplot(fchs_final) + 
  geom_point(aes(x = items, y = avgNutriScore, fill = hs_size, colour = hs_size), alpha = I(.4))+
  facet_wrap(~system_bin)

ggplot(fchs_final) + 
  geom_point(aes(x = total_vis_points, y = avgNutriScore, fill = hs_size, colour = hs_size), alpha = I(.4))+
  facet_wrap(~system_bin)

mInv <- lm(avgNutriScore ~ avgInvRating, data = fchs_inv)
mBefore <- lm(avgNutriScore ~ age + african_american_ratio + asian_ratio + hisp_latino_ratio + white_ratio + HighSchoolnon_Grad_ratio + HsGrad_Ged_ratio + other_ratio, data = fchs_final[which(fchs_final$system_bin == "0"),])
mAfter <- lm(avgNutriScore ~ age + african_american_ratio + asian_ratio + hisp_latino_ratio + white_ratio + HighSchoolnon_Grad_ratio + HsGrad_Ged_ratio + other_ratio, data = fchs_final[which(fchs_final$system_bin == "1"),])

summary(mInv)
summary(mBefore)
summary(mAfter)

#model <- glm(items ~ fchs_inv$system_bin + fchs_inv$annual_income + fchs_inv$fed_poverty_level + fchs_inv$gender + fchs_inv$race + offset(log(as.numeric(fchs_inv$hs_size))), ##This is only using data back to 08/28/2017
   # family=poisson, data=fchs_inv)

#summary(model)


model <- glm(items ~ fchs_final$system_bin + offset(log(as.numeric(fchs_final$hs_size))),
             family=poisson, data=fchs_final)

summary(model)
