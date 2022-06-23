install.packages("readxl")
library("readxl")
library("dplyr")
library("data.table")

if(!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2)

my_data <- read_excel("Talent_migration_old.xlsx")
HDI_data <- read_excel("HDI_2018.xlsx")
country_migration_data <- read_excel("Country_Migration.xlsx")

joined_data <-  merge(my_data, HDI_data, by.x = "country_name", by.y = "Country")


joined_data_dt <- setDT(joined_data)
country_migration_data_dt <- setDT(country_migration_data)

summary(my_data)
summary(HDI_data)
summary(joined_data)
summary(country_migration_data)

# Checking missing observations
sapply(my_data, function(x) sum(is.na(x)))
sapply(HDI_data, function(x) sum(is.na(x)))
sapply(joined_data, function(x) sum(is.na(x)))
sapply(country_migration_data, function(x) sum(is.na(x)))

Freq<- my_data %>%
  group_by(wb_region,wb_income) %>%  
  summarise(Frequency = n()) %>%
  arrange(desc(wb_region)); Freq

# Topmost Skill group category
Freqcat<- my_data %>%
  group_by(skill_group_category) %>%  
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency)); Freqcat

# Topmost Skill group name
Freqname<- my_data %>%
  group_by(skill_group_name) %>%  
  summarise(Frequency = n()) %>%
  arrange(desc(Frequency)); Freqname

#Scatter Plot 
ggplot(joined_data) +
  geom_point(aes(x = wb_region, y = HDI), color = "tomato2", alpha = 0.4) +
  ggtitle("HDI Rankings")

#Plot for HDI per country
data.for.plot <- joined_data_dt[, .(meanHDI = mean(HDI)), by=country_name]
ggplot(data.for.plot) + 
  geom_bar(aes(x = country_name, y = meanHDI), stat = "identity", width =.4, fill="seagreen") +
  coord_flip() +
  ylab("mean HDI") +
  ggtitle("Average HDI per Country")


# Plot for HDI per Region
data.for.plot2 <- joined_data_dt[, .(meanHDI = mean(HDI)), by=wb_region]
ggplot(data.for.plot2) + 
  geom_bar(aes(x = wb_region, y = meanHDI), stat = "identity", width =.4, fill="seagreen") +
  coord_flip() +
  ylab("mean HDI") +
  ggtitle("Average HDI per Region")

# Bar plot for skill group distribution
count <- table(joined_data_dt$skill_group_category); count
barplot(count, 
        main="Skill Distribution",
        xlab="Skill Group Category", 
        ylab="Frequency", 
        border= "red", col="lightblue",
)

# Bar plot for skill group distribution
count1 <- table(country_migration_data$target_country_wb_region); count
barplot(count1, 
        main="Which region saw the most migration (incoming)",
        xlab="Region", 
        ylab="Frequency", 
        border= "red", col="lightblue", las=2
)

# Plot for skills contributing to HDI
data.for.plotskill <- joined_data_dt[  , .(meanHDI = mean(HDI)), by=.(skill_group_category,wb_region)]

ggplot(data = data.for.plotskill, aes( x = factor( wb_region ), y = meanHDI, fill = skill_group_category ) ) +    # print bar chart
  geom_bar( stat = 'identity', position = 'dodge' )

# Plot for skills contributing to HDI
data.for.plotskill <- joined_data_dt[  , .(meanHDI = mean(HDI)), by=.(skill_group_category,wb_income)]

ggplot(data = data.for.plotskill, aes( x = factor( wb_income ), y = meanHDI, fill = skill_group_category ) ) +    # print bar chart
  geom_bar( stat = 'identity', position = 'dodge' )


#Which region saw the most migration (incoming)

data.for.plot3 <- country_migration_data_dt[, .(freq=.N), by=target_country_wb_region]
ggplot(data.for.plot3) + 
  geom_bar(aes(x = target_country_wb_region, y = freq ), stat = "identity", width =.4, fill="seagreen") +
  coord_flip() +
  ylab("Count") +
  ggtitle("Which region saw the most migration (incoming)")

#Which region saw the most migration (outgoing)

data.for.plot4 <- country_migration_data_dt[, .(freq=.N), by=base_country_wb_region]
ggplot(data.for.plot4) + 
  geom_bar(aes(x = base_country_wb_region, y = freq ), stat = "identity", width =.4, fill="seagreen") +
  coord_flip() +
  ylab("Count") +
  ggtitle("Which region saw the most migration (outgoing)")




#==============Final===============================


# Plot for HDI per Region
data.for.plot2 <- skill_migration_data_wo_na[, .(meanHDI = mean(HDI_Skill)), by=wb_region]
ggplot(data.for.plot2) + 
  geom_bar(aes(x = wb_region, y = meanHDI), stat = "identity", width =.4, fill="seagreen") +
  coord_flip() +
  ylab("mean HDI") +
  ggtitle("Average HDI per Region")


#x-y plot for skill migration as a function of HDI over the years.
xyplot(Skill_net_per_10K_year ~ HDI_Skill | as.factor(Year), data=skill_migration_data_wo_na, auto.key=TRUE,
       xlab="HDI_Skill",
       ylab="Skill_net_per_10K_year",
       pch=19)


# Bar plot for skill group distribution
count <- table(skill_migration_data_wo_na$skill_group_category); count
barplot(count, 
        main="Skill Distribution",
        xlab="Skill Group Category", 
        ylab="Frequency", 
        border= "red", col="lightblue",
)


# Plot for skills migration wrt to a region and income
data.for.plotskill <- skill_migration_data_wo_na[  , .(mean_migration = mean(Skill_net_per_10K_year)), by=.(wb_income,wb_region)]

ggplot(data = data.for.plotskill, aes( x = factor( wb_region ), y = mean_migration, fill = wb_income ) ) +    # print bar chart
  geom_bar( stat = 'identity', position = 'dodge' )+
  coord_flip() 

# Plot for skills migration wrt to a region and skill category
data.for.plotskill <- skill_migration_data_wo_na[  , .(mean_migration = mean(Skill_net_per_10K_year)), by=.(skill_group_category,wb_region)]

ggplot(data = data.for.plotskill, aes( x = factor( wb_region ), y = mean_migration, fill = skill_group_category ) ) +    # print bar chart
  geom_bar( stat = 'identity', position = 'dodge' )+
  coord_flip() 


# Plot for skills migration wrt to a region
data.for.plotskill <- skill_migration_data_wo_na[  , .(mean_migration = mean(Skill_net_per_10K_year)), by=.(wb_region)]

ggplot(data = data.for.plotskill, aes( x = factor( wb_region ), y = mean_migration) ) +    # print bar chart
  geom_bar( stat = 'identity', position = 'dodge' )+
  coord_flip() 






