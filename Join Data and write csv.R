install.packages('sqldf')

#getwd()
setwd("C:/Users/Luke/Desktop/Stat 190/Project 1")


foodchoices = read.csv(choose.files(), header=TRUE)
#inventory = read.csv(choose.files(), header=TRUE)
visits = read.csv(choose.files(), header=TRUE)
households = read.csv(choose.files(), header=TRUE)
avg_nutri = read.csv(choose.files(), header=TRUE)  # FC AVG NUTRI
inventory_wide = read.csv(choose.files(), header=TRUE)
inventory = read.csv(choose.files(), header=TRUE)

library(sqldf)

#testing a variety of queries
test = sqldf("select afn, trans_id, round(avg(nutriScoreValue),2) as 'AvgNutriScore', ts from foodchoices
      group by trans_id, ts ")

# remove ethnicity and remove duplicates.
visits2 = sqldf("select afn, trans_id, ts, numinHousehold, dob, gender, race from visits group by afn, trans_id, ts, numinHousehold, dob, gender")

# visits and foodchoices

avg_nutri$ts = as.Date(avg_nutri$ts, format = "%Y-%m-%d")
visits2$ts = as.Date(visits2$ts, format = "%Y-%m-%d")
testjoin = sqldf("select * from visits2 a join avg_nutri b on a.afn = b.afn and a.ts = b.ts")
write.csv(testjoin, file = "testjoin2.csv")

write.csv(test, file = "test.csv")

# ensuring dates can be used properly
inventory_wide$StartDate = as.Date(inventory_wide$StartDate,  format = "%Y-%m-%d")
inventory_wide$EndDate = as.Date(inventory_wide$EndDate,  format = "%Y-%m-%d")
avg_nutri$ts = as.Date(avg_nutri$ts,  format = "%Y-%m-%d")
test5 = sqldf("select * from avg_nutri join inventory_wide on ts between inventory_wide.StartDate and inventory_wide.EndDate limit 5")
write.csv(test5, file = "test5.csv")

inventory$StartDate = as.Date(inventory$StartDate, format = "%Y-%m-%d")
inventory$EndDate = as.Date(inventory$EndDate, format = "%Y-%m-%d")

# avg nutriscore of inventory (purely by category of food)
inventory_avg_nutri = sqldf("select StartDate, EndDate, avg(Rating) as 'avgInvRating' from inventory group by StartDate, EndDate")
write.csv(inventory_avg_nutri, file = "InvAvgNutri.csv")

inventory_avg_nutri$StartDate = as.Date(inventory_avg_nutri$StartDate, format = "%Y-%m-%d" )
inventory_avg_nutri$EndDate = as.Date(inventory_avg_nutri$EndDate, format = "%Y-%m-%d" )

# joining on the date found in avg_nutri of a purchase with the inventory date ranges
test6 = sqldf("select * from avg_nutri join inventory_avg_nutri on ts between inventory_avg_nutri.StartDate and inventory_avg_nutri.EndDate ")
# middle table written out to see if date join is working
# used to form the later table.

write.csv(test6, file = "test7.csv")

# avg nutri score for purchases within a time period.
test10 = sqldf("select avg(AvgNutriScore) as 'AvgNutriScorePurchTimePeriod', StartDate, EndDate, avgInvRating from test6 group by StartDate, EndDate, avgInvRating")

write.csv(test10, file = "test10.csv")


sqldf("select distinct afn, (sum(num_male) + sum(num_female)) as 'num_in_household', avg(fed_poverty_level), ed), sum(9to12_nonGrad), sum(upTo_8thGrade),  from households group by afn limit 5")





