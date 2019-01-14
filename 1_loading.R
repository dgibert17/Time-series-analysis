rm(list = ls())

setwd("C:/Users/David/Google Drive/Ubiqum/6_EnergyConsumption")

df = read.table("power_consumption.txt",
                sep = ";",
                dec = ".",
                col.names = c("date", "time", "global_active_power",
                              "global_reactive_power", "voltage", "global_intensity",
                              "kitchen", "laundry_room", "heater_conditioner"),
                na.strings = c("?", "-", "NA"),
                stringsAsFactors = F,
                header = T)

save(df, file = "C:/Users/David/Google Drive/Github/task-3-1-define-a-data-science-process-dgibert17/DFenergy_original.Rdata")

## FUNCTION TO CHANGE SOME COLUMNS'S CLASS
# toNumeric <- function(x) {
#   for (i in 3:ncol(x)) {
#     x[,i] <- as.numeric(x[,i])
#   }
# }
