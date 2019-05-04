#Producing smoothened weight and body fat, and preddicting body fat measure

library(dplyr)
library(magrittr)

dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)
data = read.table(file = "FLF_input_table.csv", header=TRUE, sep = "\t")
print(data)
# =495 / (1.10938-(0.0008267*SUM(F4:H4))+(0.0000016*SUM(F4:H4)^2)-(0.0002574*YEARFRAC(DATE(1994,1,4),E4,3 ))) - 450

birthday = lubridate::make_date(1994, 1, 4)

data = data %>% 
  mutate(Date = lubridate::mdy(Date))  %>% 
  mutate(DaysFromBirth = as.numeric(Date - birthday) / 365)  %>% 
  mutate(PredictedBF = 495 / 
           (1.10938-(0.0008267*(Chest + Abdominal + Thigh))
            +(0.0000016*(Chest + Abdominal + Thigh)^2)
            -(0.0002574*DaysFromBirth)) - 450) %>%
  mutate(Weights = 0) %>%
  mutate(Part_Weights = 0)

MemoryCnt = 0.364670331

rows = nrow(data)
data[rows, ]$Weights = MemoryCnt

for(i in 1:6) {
  data[rows-i, ]$Weights = data[rows-i+1,]$Weights*(1-MemoryCnt)
}
sum = sum(data$Weights)
data$Weights = data$Weights / sum
data$Part_Weights = data$Weight * data$Weights
data$Part_Fat = data$PredictedBF * data$Weights

write.csv(data, file = "FLF_input_table.csv")

# IF(COUNTIFS(OFFSET(B55, 0, 0, $R$5, 1), TRUE) > 0, IF(B55, $R$6, (1-$R$6) *OFFSET(M55,1,0)), 0)


# for(i in 1:nrow(data)) {
#   data[i, ]$Chest %>% print
# 
# }

