install.packages("readxl")
library("readxl")
library("dplyr")
library("data.table")

if(!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2)

skill_migration <- read_excel("Project_Skill_Migration.xlsx")
country_migration <- read_excel("Project_Country_Migration.xlsx")


skill_migration_dt <- setDT(skill_migration)
country_migration_dt <- setDT(country_migration)

summary(skill_migration)
summary(country_migration)

sapply(skill_migration, function(x) sum(is.na(x)))
sapply(country_migration, function(x) sum(is.na(x)))

# Plot for HDI per Region
data.for.plot2 <- skill_migration_dt[, .(meanHDI = mean(HDI_Skill)), by=wb_region]
ggplot(data.for.plot2) + 
  geom_bar(aes(x = wb_region, y = meanHDI), stat = "identity", width =.4, fill="seagreen") +
  coord_flip() +
  ylab("mean HDI") +
  ggtitle("Average HDI per Region")


# Bar plot for skill group distribution
count <- table(skill_migration_dt$skill_group_category); count
barplot(count, 
        main="Skill Distribution",
        xlab="Skill Group Category", 
        ylab="Frequency", 
        border= "red", col="lightblue"
)


# Plot for skills contributing to HDI
data.for.plotskill <- skill_migration_dt[  , .(meanHDI = mean(HDI_Skill)), 
                                       by=.(skill_group_category,wb_region)]

ggplot(data = data.for.plotskill, aes( x = factor( wb_region ), 
                                       y = meanHDI, fill = skill_group_category ) ) +    # print bar chart
  geom_bar( stat = 'identity', position = 'dodge' )

