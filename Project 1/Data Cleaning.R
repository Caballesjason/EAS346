library(readr)
library(tidyverse)
library(ggplot2)

Reciepts <- read.csv("Coffee Shop Data From Kaggle/201904 sales reciepts.csv")
Staff <- read.csv("Coffee Shop Data From Kaggle/staff.csv")

# Removing unwanted columns
Reciepts <- subset(Reciepts, select = -c(1:4,6:14))


Staff <- Staff[Staff$position == "Coffee Wrangler",]
Staff <- subset(Staff, select = -c(4,5,7,8))

# Combining, location, and staff_id
Reciepts <- Reciepts %>% 
              mutate(Reciepts, location = case_when(staff_id == 7 |staff_id == 8 | staff_id == 9 |staff_id == 10 ~ 3,
                                          staff_id == 12 | staff_id == 13 | staff_id == 14 | staff_id == 15 ~ 4,
                                          staff_id == 17 |staff_id ==  18 | staff_id == 19 | staff_id == 20 ~ 5,
                                          staff_id == 22 |staff_id ==  23 | staff_id == 24| staff_id == 25 ~ 6,
                                          staff_id == 27 | staff_id == 28 |staff_id ==  29|staff_id == 30 ~ 7,
                                          staff_id == 32 |staff_id == 33 |staff_id == 34 |staff_id == 35 ~ 8,
                                          staff_id == 37 |staff_id == 38 | staff_id ==39 |staff_id == 40 ~ 9,
                                          staff_id == 42 |staff_id == 43 | staff_id == 44 |staff_id == 45 ~ 10,
                                          staff_id == 47 |staff_id == 48 |staff_id == 49 |staff_id == 50| staff_id ==52 | staff_id ==53 | staff_id ==54 | 
                                            staff_id ==55 ~ 11))

# Dropping NAs
Reciepts <- Reciepts %>% drop_na()

#Creating Contingency tables for graphs in form of data frames.
table <- table(Reciepts)
table <- data.frame(table)
table$location <- droplevels(table$location)
table$location <- factor(table$location)
table$location <- as.numeric(table$location)
table$location[table$location==1] <- "A"
table$location[table$location==2] <- "B"
table$location[table$location==3] <- "C"
table$location[table$location==4] <- "D"
table$location[table$location==5] <- "E"
table$location[table$location==6] <- "F"


Bar_Data <- table[table$Freq != 0,]

table1 <- table(Reciepts$location)
Pie_Data <- data.frame(table1)
Pie_Data <- table(Reciepts)
Pie_Data <- data.frame(table1)
Pie_Data$Var1 <- droplevels(Pie_Data$Var1)
Pie_Data$Var1 <- factor(Pie_Data$Var1)
Pie_Data$Var1 <- as.numeric(Pie_Data$Var1)
Pie_Data$Var1[Pie_Data$Var1==1] <- "A"
Pie_Data$Var1[Pie_Data$Var1==2] <- "B"
Pie_Data$Var1[Pie_Data$Var1==3] <- "C"
Pie_Data$Var1[Pie_Data$Var1==4] <- "D"
Pie_Data$Var1[Pie_Data$Var1==5] <- "E"
Pie_Data$Var1[Pie_Data$Var1==6] <- "F"

write.csv(Bar_Data, "Bar Data.csv")
write.csv(Pie_Data, "Pie Data.csv")