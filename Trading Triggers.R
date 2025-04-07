install.packages(c("quantmod", "TTR", "httr"))

library(quantmod)
library(TTR)
library(httr)

# === Get Data ===
getSymbols(c("SPY", "^VIX"), src = "yahoo", from = Sys.Date() - 365)

# Clean & join
spy <- Cl(SPY)
vix <- Cl(VIX)
data <- merge(spy, vix, join = "inner")
colnames(data) <- c("SPY", "VIX")

# === Indicators ===
data$SMA10 <- SMA(data$SPY, 10)
data$SMA50 <- SMA(data$SPY, 50)
data$Crossover <- ifelse(Lag(data$SMA10, 1) < Lag(data$SMA50, 1) & 
                           data$SMA10 > data$SMA50, 1, 0)
data$VIX_Low <- ifelse(data$VIX < 20, 1, 0)
data$EntrySignal <- ifelse(data$Crossover == 1 & data$VIX_Low == 1, 1, 0)

# === Plot Re-Entry Points ===
plot(data$SPY, main = "SPY Price with Re-Entry Signals", col = "black")
lines(data$SMA10, col = "blue")
lines(data$SMA50, col = "red")
points(index(data)[which(data$EntrySignal == 1)],
       data$SPY[which(data$EntrySignal == 1)],
       col = "green", pch = 19)

# === Send Discord Notification ===
if (last(data$EntrySignal) == 1) {
  webhook_url <- "https://discord.com/api/webhooks/1358616232311394695/nmBhkfRgRNxvvsEsn7hGLW8lGUNNK86qER-s5Uga_Az44rBTInjQtGSfCmMOMOKziWnD"  # Replace with your actual webhook URL
  
  message <- list(
    content = paste0("📈 **SPY Re-Entry Signal Triggered!**\n",
                     "Date: ", Sys.Date(), "\n",
                     "Price: $", round(last(data$SPY), 2), "\n",
                     "VIX: ", round(last(data$VIX), 2), "\n",
                     "Strategy: 10/50 MA Crossover + VIX < 20")
  )
  
  POST(webhook_url, body = message, encode = "json")
}