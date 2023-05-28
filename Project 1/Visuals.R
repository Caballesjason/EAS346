library(readr)
library(ggplot2)
library(tidyverse)
library(scales)

PD <- read.csv("My Clean Data/Pie Data.csv")
BD <- read.csv("My Clean data/Bar Data.csv")

# Removing weird columns
PD <- subset(PD, select = -c(X))
BD <- subset(BD, select = -c(X))

# Changing column names
PD <- PD %>% 
            rename( Location = Var1, Sales = Freq)

BD <- BD %>% 
            rename(Location = location, "Staff ID" = staff_id, "Sales ($)" = Freq)

# Change all numerics to factors (Excluding Sales)
BD$`Staff ID` <- as.factor(BD$`Staff ID`)
BD$`Location` <- as.factor(BD$`Location`)
PD$Location <- as.factor(PD$Location)
# Creating Bar Plot

ggplot(BD, aes(x=`Staff ID`, y = `Sales ($)`,fill = Location)) + geom_bar(stat="identity") +  labs(title="Sales Made By Each Barista", x="Staff ID Number",
                                                                                        y = "Sales In Cups of Coffee") +  theme(plot.title = element_text(hjust = 0.5,
                                                                                                                                   size= 30, face="bold"))

PC <- ggplot(PD, aes(x='', y =Sales, fill = Location)) + geom_bar(width = 1, stat = "identity") + 
  labs(title="Total Sales For Each Coffee Shop", x='', y='') + 
  theme(plot.title = element_text(hjust = .5, face="bold", size = 30), axis.text = element_blank(), axis.title = element_blank(),
        panel.background = element_blank())
PC

Pie_Chart <- PC + coord_polar("y", start=0) + geom_text(aes(label = paste0(round((Sales/sum(Sales))*100, digits = 2), "%" ), x=1.3), 
position = position_stack(vjust = .55))
Pie_Chart
