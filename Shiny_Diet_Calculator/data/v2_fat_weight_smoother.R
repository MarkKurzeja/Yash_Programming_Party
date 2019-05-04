#Producing smoothened weight and body fat, and preddicting body fat measure
rm(list = ls())
library(dplyr)
library(magrittr)

dir = dirname(rstudioapi::getSourceEditorContext()$path)
setwd(dir)
data = read.table(file = "FLF_input_table.csv", header=TRUE, sep = ",")
print(data)
# =495 / (1.10938-(0.0008267*SUM(F4:H4))+(0.0000016*SUM(F4:H4)^2)-(0.0002574*YEARFRAC(DATE(1994,1,4),E4,3 ))) - 450
data$Date %<>% lubridate::mdy()

# Determine the number of days to offset the base date
num_days = data$Date %>% range() %>% diff %>% as.numeric()

# Create a list of all of the dates that should be in the file
dates_vec = {data$Date %>% min()} %>% {. + 0:num_days}

# Create the (eventual) full data frame
full_date = data.frame(Date = dates_vec)

# Create the ghost matrix of the values that should be in the table
full_date %<>% left_join(data)

# Impute the missing data using a linear interpolation order (n) approximation 
# using dual runners and step-wise t+1 approximation
runner_base = 1
runner_lead = 2

while(runner_base < nrow(full_date)) {
  if(is.na(full_date[runner_base, "Weight"])) {
    runner_base = runner_base - 1
  }
  
  while(is.na(full_date[runner_lead, "Weight"]) & runner_lead < nrow(full_date)) {
    runner_lead = runner_lead + 1
  }
  
  diff_base = runner_lead - runner_base
  for(i in c(colnames(full_date)[2:6])) {
    for(j in 1:diff_base -1) {
      full_date[runner_base + j, i] = (full_date[runner_lead, i] - full_date[runner_base, i]) / diff_base * j + full_date[runner_base, i]
    }
  }
  
  runner_base = runner_lead + 1
  runner_lead = runner_base + 1
  
}

# Tidy up and put the full_date structure back into data
data = full_date
rm(full_date)

# Check for the imputation accuracy
# ggplot() +
  # geom_line(aes(Date, Weight), data = data, color = "red") + 
  # geom_point(aes(Date, Weight), data = full_date)



birthday = lubridate::make_date(1994, 1, 4)

data = data %>% 
  mutate(Date = lubridate::ymd(Date))  %>% 
  mutate(DaysFromBirth = as.numeric(Date - birthday) / 365)  %>% 
  mutate(PredictedBF = 495 / 
           (1.10938-(0.0008267*(Chest + Abdominal + Thigh))
            +(0.0000016*(Chest + Abdominal + Thigh)^2)
            -(0.0002574*DaysFromBirth)) - 450)
  # mutate(Weights = 0) %>%
  # mutate(Part_Weights = 0) %>% mutate(norm_weight = 0)

MemoryCnt = 0.364670331



# data$Weights <- rep(NA, 52)
# data$Weights <- rep(NA, 52)
data$WeightAverage <- rep(NA, nrow(data))
for(i in 7:nrow(data)) {
  data$Weights <- 0
  data$Weights[i] = MemoryCnt
  for(j in 1:6) {
    data$Weights[i - j] = data$Weights[i - j+ 1] * (1-MemoryCnt)
  }
  data$WeightAverage[i] = crossprod(data$Weights / sum(data$Weights), data$Weight)
}

data$BFAverage <- rep(NA, nrow(data))
for(i in 7:nrow(data)) {
  data$Weights <- 0
  data$Weights[i] = MemoryCnt
  for(j in 1:6) {
    data$Weights[i - j] = data$Weights[i - j+ 1] * (1-MemoryCnt)
  }
  data$BFAverage[i] = crossprod(data$Weights / sum(data$Weights), data$PredictedBF)
}

# library(ggplot2)
# data %>% 
#   ggplot() + 
#   geom_point(aes(X, Weight)) + 
#   geom_line(aes(X, BfAverage))



# for(row in 7:nrow(data)) {
#   data[row, ]$Weights = MemoryCnt
#   for(i in 1:6) {
#     # browser()
#     data[row-i, "Weights"] = data[row-i+1,]$Weights*(1-MemoryCnt)
#     data[row, "norm_weight"] = data[row, ]$norm_weight + data[row-i, ]$Weights * data$Weight[row - i]
#   }
#   data = data %>% mutate(norm_weight = 0)
# }

sum = sum(data$Weights)
data$Weights = data$Weights / sum
data$Part_Weights = data$Weight * data$Weights
data$Part_Fat = data$PredictedBF * data$Weights

write.csv(data, file = "FLF_output_table.csv")

# IF(COUNTIFS(OFFSET(B55, 0, 0, $R$5, 1), TRUE) > 0, IF(B55, $R$6, (1-$R$6) *OFFSET(M55,1,0)), 0)


# for(i in 1:nrow(data)) {
#   data[i, ]$Chest %>% print
# 
# }

