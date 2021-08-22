## Title: "Plot 3"
## Author: "Josh Neale"
## Date: "22/08/2021"

## 1. Import Data

library(dplyr)
library(lubridate)

power <- read.table("household_power_consumption.txt",
                    header = TRUE,
                    sep = ";")

# Format as data frame extension
power <- as_tibble(power)

power_cln <- power %>%
                # Format Date and Time
                mutate(Date = dmy(Date), Time = hms(Time)) %>%
                # Create Date-Time Variable
                mutate(Date_Time = make_datetime(year(Date),
                                                 month(Date),
                                                 day(Date),
                                                 hour(Time),
                                                 minute(Time),
                                                 second(Time))) %>%
                # Select Date-Time Variable
                select(Date_Time, Global_active_power:Sub_metering_3) %>%
                # Format Numeric Columns as Double and Replace "?" with NA
                mutate(Global_active_power = as.double(na_if(Global_active_power, "?")),
                       Global_reactive_power = as.double(na_if(Global_reactive_power, "?")),
                       Voltage = as.double(na_if(Voltage, "?")),
                       Global_intensity = as.double(na_if(Global_intensity, "?")),
                       Sub_metering_1 = as.double(na_if(Sub_metering_1, "?")),
                       Sub_metering_2 = as.double(na_if(Sub_metering_2, "?")),
                       Sub_metering_3 = as.double(na_if(Sub_metering_3, "?")))

# Clean up to save space
rm(power)

# Select data for 2007-02-01 and 2007-02-02
power_sub <- subset(power_cln,
                    Date_Time >= as_datetime('2007-02-01') 
                    & Date_Time < as_datetime('2007-02-03'))

# Clean up to save space
rm(power_cln)

# Open PNG Graphics Device
png(filename = "plot4.png", width = 480, height = 480)

# Scatterplot to Graphics Device
par(mfrow = c(2,2))
with(power_sub, {
        plot(Date_Time, Global_active_power, type = "l",
             ylab = "Global Active Power",
             xlab = "")

        plot(Date_Time, Voltage, type = "l",
             ylab = "Voltage",
             xlab = "datetime")
        
        plot(Date_Time, Sub_metering_1, type = "l",
        ylab = "Energy sub metering",
        xlab = "")
        points(Date_Time, Sub_metering_2, type = "l", col = "red")
        points(Date_Time, Sub_metering_3, type = "l", col = "blue")
        legend("topright", 
               lty = 1,
               lwd = 1,
               col = c("black","red","blue"), 
               legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"))

        plot(Date_Time, Global_reactive_power, type = "l",
             ylab = "Global_reactive_power",
             xlab = "datetime")
})


# Close Graphics Device
dev.off()
