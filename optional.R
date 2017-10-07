#data saved to working directory
#this data was preprocessed, the raw data is available at https://data.cms.gov/Medicare/Inpatient-Prospective-Payment-System-IPPS-Provider/97k6-zzx3

medicare_data <- read.csv("medicaredata.csv")

#first question: what is the relationship between mean covered charges and mean total payments in New York?
pdf("nystate.pdf")
library(ggplot2)
library(dplyr)
ny_data <- medicare_data %>%
        filter(Provider.State == "NY")

correlation = with(ny_data, cor(Average.Total.Payments, Average.Covered.Charges))
correlation = round(correlation, digits = 2)
correlation = as.character("Correlation is equal to .61")
ggplot(ny_data, aes(log10(Average.Total.Payments), log10(Average.Covered.Charges))) +
      geom_point(color = "red", alpha = .3, pch = 19) +
      geom_smooth(method = "lm", se = F) +
      labs(title = "Average Total Payments versus Average Covered Charges for NY", subtitle = correlation)
dev.off()
#there is a strong positive correlation between covered charges and total payments

#plot 2.same question, but also how do thee vary by medical condition and the state which care was received?
pdf("by_state.pdf")
library(ggplot2)
library(dplyr)
levels(medicare_data$DRG.Definition) = c("Pneumonia & Pleurisy", "Heart Failure & Shock",
                                         "Misc Digest Disorders",
                                         "Nutrition Disorders",
                                         "Kidney/Urinary Tract Inf", "Septicemia or Severe Sepsis")
by_state <- medicare_data %>%
        group_by(Provider.State, DRG.Definition) %>%
        summarise(Charges = mean(Average.Covered.Charges), Payments = mean(Average.Total.Payments))
ggplot(by_state, aes(Charges, Payments, col = Provider.State)) + 
        geom_point() +
        labs(title = "Average Payments and Charges by State for Each Disorder") +
        facet_wrap(~ DRG.Definition)
##California sees the highest charges and payments. The most costly disorders are Septicemia/Severe Sepsis
dev.off()

