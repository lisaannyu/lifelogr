library("fitbitScraper")

cookie <- login(email="rohisha@gmail.com", password="datasamplepw")  

# 15_min_data "what" options: "steps", "distance", "floors", "active-minutes", "calories-burned"

# pull data in the date/time range requested
df <- get_intraday_data(cookie, what="steps", date="2017-02-16")
sleep <- get_sleep_data(cookie, start_date = "2017-01-13", end_date = "2017-02-13")[[2]]
hr <- get_daily_data(cookie, what = "getRestingHeartRateData", start_date = "2016-01-13", end_date = "2017-02-16")
hr_zones <- get_daily_data(cookie, what = "getTimeInHeartRateZonesPerDay", start_date = "2017-01-13", end_date = "2017-02-13")
steps <- get_daily_data(cookie, what = "steps", start_date = "2017-01-13", end_date = "2017-02-13")

steps$date <- as.Date(strptime(steps$time, format="%Y-%m-%d"))
sleep$date <- as.Date(sleep$date, "%Y-%m-%d")
hr$date <- as.Date(strptime(hr$time, format="%Y-%m-%d"))
all <- merge(x=sleep, y=hr, by.x="date", by.y="date", all=TRUE)
all <- merge(all, steps, by="date", all=TRUE)

plot(hr$date, hr$restingHeartRate, type='l')
