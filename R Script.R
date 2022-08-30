### Texas Cities & Wasted Fuel Due to Congestion
# Load necessary packages
library(tidyverse)
library(formattable)
library(stringr)
library(lubridate)
library(ggrepel)
library(directlabels)
library(caret)

# Create plot of all Texas cities
ggplot(TexasCities, aes(x = Year, y = Gallons_Wasted, color = City)) +
  geom_line() +
  geom_dl(aes(label = City), method = "last.bumpup") +
  xlim(1980, 2022) +
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle("Texas Cities and Wasted Fuel Due to Congestion\n(In Millions of Gallons)") +
  xlab("Year") + ylab("Gallons Wasted (In Millions)")

# Create plot showing only Houston, DFA, San Antonio & Austin
Big_Four <- TexasCities %>%
  filter(City == "Austin" | City == "San Antonio" | City == "Houston" 
         | City == "Dallas-FortWorth-Arlington")
View(Big_Four)
ggplot(Big_Four, aes(x = Year, y = Gallons_Wasted, color = City)) +
  geom_line() +
  geom_dl(aes(label = City), method = "last.bumpup") +
  xlim(1980, 2022) + 
  theme_bw() +
  theme(legend.position = "none") +
  ggtitle("Big 4 Texas Cities\nWasted Fuel Due to Congestion (In Millions of Gallons)") +
  xlab("Year") + ylab("Gallons Wasted (In Millions)")

## Attempt at predicting the future of wasted gallons 
# Linear regression attempt
model <- train(Gallons_Wasted ~ Year,
               data = Big_Four,
               method = "lm")
model
# Ridge regression attempt
ridge_model <- train(Gallons_Wasted ~., 
                     data = Big_Four,
                     method = "ridge")
ridge_model





