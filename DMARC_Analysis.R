library(sqldf)
library(car)
library(ggplot2)

fc <- read.csv('/Users/tannerthurman/Desktop/DMARC data/drakeExport_foodChoices.csv', header = T) #fc raw data
hs <- read.csv('/Users/tannerthurman/Desktop/DMARC data/drakeExport_served_households.csv', header = T) #Households raw data
visits <- read.csv('/Users/tannerthurman/Desktop/DMARC data/drakeExport_visits.csv', header = T) #Visits raw data
inventory_wide <- read.csv('/Users/tannerthurman/Desktop/DMARC data/inventory_wide.csv', header = T)
inventory <- read.csv('/Users/tannerthurman/Desktop/DMARC data/inventory.csv', header = T)   #pass in the inventory file in here (not the wide one)

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

for(i in 1:length(fchs_final$hs_size)){
  if(fchs_final$served_date[i] >= as.Date("2017-09-01", format = "%Y-%m-%d")){   ##Needs to be greater than or equal to
    fchs_final$system_bin[i] <- as.integer(1)
  } else {
    fchs_final$system_bin[i] <- as.integer(0)
  }
}

fchs_final$total_hs_points <- as.numeric(fchs_final$hs_size)*36
fchs_final$total_vis_points <- fchs_final$avgNutriScore*fchs_final$items

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

m0 <- lm(fchs_inv$avgNutriScore ~ 1)
m1 <- lm(fchs_inv$avgNutriScore ~ fchs_inv$items + fchs_inv$total_vis_points + fchs_inv$hs_size + fchs_inv$avgInvRating + fchs_inv$gender + fchs_inv$num_african_american + fchs_inv$num_american_indians + fchs_inv$num_asian + fchs_inv$num_hawaiian_or_pacific_islander + fchs_inv$num_multi_race + fchs_inv$num_other_race + fchs_inv$num_white + fchs_inv$upTo_8thGrade + fchs_inv$hsGrad_Ged + fchs_inv$hsGrad_or_Ged_some_secondary + fchs_inv$college_grad + fchs_inv$hispanic_or_latino + fchs_inv$not_hispanic + fchs_inv$annual_income + fchs_inv$fed_poverty_level + fchs_inv$gender + fchs_inv$race + fchs_inv$ethnicity + fchs_inv$system_bin, data = fchs_inv)

m2 <- step(m0, scope=list(lower=m0, upper=m1, direction = "both"), alpha = 0.05)

summary(m1)
summary(m2)


#model <- glm(items ~ fchs_inv$system_bin + fchs_inv$annual_income + fchs_inv$fed_poverty_level + fchs_inv$gender + fchs_inv$race + offset(log(as.numeric(fchs_inv$hs_size))), ##This is only using data back to 08/28/2017
   # family=poisson, data=fchs_inv)

#summary(model)


model <- glm(items ~ fchs_final$system_bin + offset(log(as.numeric(fchs_final$hs_size))),
             family=poisson, data=fchs_final)

summary(model)