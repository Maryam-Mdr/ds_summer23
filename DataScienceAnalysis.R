# Importing the required packages 
library(readxl)
library(dplyr) 

dummydf <- data.frame (date_month  = c("2022-01",
                                  "2022-02",
                                  "2022-03",
                                  "2022-04",
                                  "2022-05",
                                  "2022-06",
                                  "2022-07",
                                  "2022-08",
                                  "2022-09",
                                  "2022-10",
                                  "2022-11",
                                  "2022-12",
                                  "2023-01",
                                  "2023-02",
                                  "2023-03",
                                  "2023-04",
                                  "2023-05",
                                  "2023-06",
                                  "2023-07",
                                  "2023-08",
                                  "2023-09",
                                  "2023-10",
                                  "2023-11",
                                  "2023-12",
                                  "2024-01",
                                  "2024-02",
                                  "2024-03",
                                  "2024-04"
))
# reading the excel data file
dataSet <- read_excel("C:/Users/MandalA/Downloads/project/dataSet.xlsx")
print(dataSet)


# calculating weights 
dataSet$weight <- round((dataSet$`Man Hours`/sum(dataSet$`Man Hours`))*100, digits = 4)

dataSet$weightIFR <- round((dataSet$`Man Hours`/sum(dataSet$`Man Hours`))*70, digits = 4)

dataSet$weightIFU <- round((dataSet$`Man Hours`/sum(dataSet$`Man Hours`))*30, digits = 4)



# calculating weight per discipline 
dataSet <- dataSet %>%
  group_by(Discipline) %>%
  mutate(WeightPerDiscipline = sum(`Man Hours`))

dataSet$WeightPerDiscipline <- round((dataSet$`Man Hours`/ dataSet$WeightPerDiscipline)*100, digits = 3)

#For planned
#Aggregating IFR based on month  
dfp_IFR <- aggregate(weightIFR ~ format(as.Date(dataSet$`IFR Planned`) , "%Y-%m") , dataSet , sum )
colnames(dfp_IFR)[1] ="date_month"
#Aggregating IFU
dfp_IFU <- aggregate(weightIFU ~ format(as.Date(dataSet$`IFU Planned`) , "%Y-%m") , dataSet , sum )
colnames(dfp_IFU)[1] ="date_month"
       
dfp_IFR_IFU <- merge(x=dfp_IFR,y=dfp_IFU, by = c("date_month"),all = TRUE)   
dfp_IFR_IFU[is.na(dfp_IFR_IFU)] <- 0

dfp_IFR_IFU$totalPlaned <- dfp_IFR_IFU$weightIFR+dfp_IFR_IFU$weightIFU 

dfp_IFR_IFU_planned <- merge(x=dummydf,y=dfp_IFR_IFU, by = c("date_month"),all = TRUE)

dfp_IFR_IFU_planned[is.na(dfp_IFR_IFU_planned)] <- 0

dfp_IFR_IFU_planned <- select(dfp_IFR_IFU_planned ,date_month, totalPlaned)

dfp_IFR_IFU_planned <- dfp_IFR_IFU_planned %>%
                      mutate(totalRunningPLan = cumsum(totalPlaned))

dfp_IFR_IFU_planned <- select(dfp_IFR_IFU_planned ,date_month, totalRunningPLan)
#------------------------------------------------------------------------------
#For Forecast
#Aggregating IFR based on month  
dff_IFR <- aggregate(weightIFR ~ format(as.Date(dataSet$`IFR Forecast`) , "%Y-%m") , dataSet , sum )
colnames(dff_IFR)[1] ="date_month"
#Aggregating IFU
dff_IFU <- aggregate(weightIFU ~ format(as.Date(dataSet$`IFU Forecast`) , "%Y-%m") , dataSet , sum )
colnames(dff_IFU)[1] ="date_month"

dff_IFR_IFU <- merge(x=dff_IFR,y=dff_IFU, by = c("date_month"),all = TRUE)   
dff_IFR_IFU[is.na(dff_IFR_IFU)] <- 0

dff_IFR_IFU$totalForecast <- dff_IFR_IFU$weightIFR+dff_IFR_IFU$weightIFU 

dff_IFR_IFU_Forecast <- merge(x=dummydf,y=dff_IFR_IFU, by = c("date_month"),all = TRUE)

dff_IFR_IFU_Forecast[is.na(dff_IFR_IFU_Forecast)] <- 0

dff_IFR_IFU_Forecast <- select(dff_IFR_IFU_Forecast ,date_month, totalForecast)

dff_IFR_IFU_Forecast <- dff_IFR_IFU_Forecast %>%
  mutate(totalRunningForecast = cumsum(totalForecast))

dff_IFR_IFU_Forecast <- select(dff_IFR_IFU_Forecast ,date_month, totalRunningForecast)
#------------------------------------------------------------------------------
#For Actual
#Aggregating IFR based on month  
dfa_IFR <- aggregate(weightIFR ~ format(as.Date(dataSet$`IFR Actual`) , "%Y-%m") , dataSet , sum )
colnames(dfa_IFR)[1] ="date_month"
#Aggregating IFU
dfa_IFU <- aggregate(weightIFU ~ format(as.Date(dataSet$`IFU Actual`) , "%Y-%m") , dataSet , sum )
colnames(dfa_IFU)[1] ="date_month"

dfa_IFR_IFU <- merge(x=dfa_IFR,y=dfa_IFU, by = c("date_month"),all = TRUE)   
dfa_IFR_IFU[is.na(dfa_IFR_IFU)] <- 0

dfa_IFR_IFU$totalActual <- dfa_IFR_IFU$weightIFR+dfa_IFR_IFU$weightIFU 

dfa_IFR_IFU_Actual <- merge(x=dummydf,y=dfa_IFR_IFU, by = c("date_month"),all = TRUE)

dfa_IFR_IFU_Actual[is.na(dfa_IFR_IFU_Actual)] <- 0

dfa_IFR_IFU_Actual <- select(dfa_IFR_IFU_Actual ,date_month, totalActual)

dfa_IFR_IFU_Actual <- dfa_IFR_IFU_Actual %>%
  mutate(totalRunningActual = cumsum(totalActual))

dfa_IFR_IFU_Actual <- select(dfa_IFR_IFU_Actual ,date_month, totalRunningActual)

#-------------------------------------------------------------------------------
#merger three final dataframes

df_final <- merge(x=dfp_IFR_IFU_planned,y=dff_IFR_IFU_Forecast, by = c("date_month"),all = TRUE)
df_final <- merge(x=df_final,y=dfa_IFR_IFU_Actual, by = c("date_month"),all = TRUE)

library(ggplot2)


ggplot()+
  geom_line(data = df_final, mapping = aes(x=date_month, y=totalRunningPLan,group=1), color="blue")+
  geom_point(data = df_final, mapping = aes(x=date_month, y=totalRunningPLan,group=1 ), color="blue")+
  geom_line(data = df_final, mapping = aes(x=date_month, y=totalRunningForecast,group=1), color="orange")+
  geom_point(data = df_final, mapping = aes(x=date_month, y=totalRunningForecast,group=1 ), color="orange")+
  geom_line(data = df_final, mapping = aes(x=date_month, y=totalRunningActual,group=1), color="red")+
  geom_point(data = df_final, mapping = aes(x=date_month, y=totalRunningActual,group=1 ), color="red")+
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE))

library(ggplot2)

# Assuming you have a data frame named "df_final" with columns: date_month, totalRunningPLan, totalRunningForecast, totalRunningActual

ggplot(data = df_final) +
  geom_line(mapping = aes(x = date_month, y = totalRunningPLan, group = 1), color = "blue") +
  geom_point(mapping = aes(x = date_month, y = totalRunningPLan, group = 1), color = "blue") +
  geom_line(mapping = aes(x = date_month, y = totalRunningForecast, group = 1), color = "orange") +
  geom_point(mapping = aes(x = date_month, y = totalRunningForecast, group = 1), color = "orange") +
  geom_line(mapping = aes(x = date_month, y = totalRunningActual, group = 1), color = "red") +
  geom_point(mapping = aes(x = date_month, y = totalRunningActual, group = 1), color = "red") +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

library(ggplot2)


ggplot(data = df_final) +
  geom_line(mapping = aes(x = date_month, y = totalRunningPLan, group = 1, color = "Plan")) +
  geom_point(mapping = aes(x = date_month, y = totalRunningPLan, group = 1, color = "Plan")) +
  geom_line(mapping = aes(x = date_month, y = totalRunningForecast, group = 1, color = "Forecast")) +
  geom_point(mapping = aes(x = date_month, y = totalRunningForecast, group = 1, color = "Forecast")) +
  geom_line(mapping = aes(x = date_month, y = totalRunningActual, group = 1, color = "Actual")) +
  geom_point(mapping = aes(x = date_month, y = totalRunningActual, group = 1, color = "Actual")) +
  scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
  scale_color_manual(values = c("Plan" = "blue", "Forecast" = "orange", "Actual" = "red")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))



  