
# prep and options --------------------------------------------------------

# rm and clear console
rm(list = ls()); cat("\14")
devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/misc/function_poverty/func_ins.pack.R")
# setwd
setwd("d:/R_wd/")
# option: scipen
options(scipen = 999)
# load packages
ins.pack("tidyverse", 
         "pastecs", 
         "data.table", 
         "ggplot2", 
         "plotly")

# read the file -----------------------------------------------------------

# read the file with specified format
dt <- read_csv("Dr Parenting/usagetime/usage_time_20191026-20191125_new.csv", 
               col_types = "cccdTTd")
# setDT
setDT(dt)

# date transformation -----------------------------------------------------

dt[ , start := as.POSIXct(format(started_at, tz = "Asia/Taipei", usetz = TRUE))]
dt[ , end := as.POSIXct(format(ended_at, tz = "Asia/Taipei", usetz = TRUE))]

# calc: duration ----------------------------------------------------------

dt[ , diff_sec := difftime(end, start, units = "secs")]
dt[ , diff_min := difftime(end, start, units = "mins")]
dt[ , diff_hr := difftime(end, start, units = "hours")]
dt[ , diff_day := difftime(end, start, units = "days")]

# format: year ------------------------------------------------------------

dt[ , year_s := format(start, "%Y")]
dt[ , year_e := format(end, "%Y")]

# format: month -----------------------------------------------------------

dt[ , month_s := format(start, "%m")]
dt[ , month_e := format(end, "%m")]

# format: weekday ---------------------------------------------------------

dt[ , week_s := format(start, "%w")]
dt[ , week_e := format(end, "%w")]

# format: day -------------------------------------------------------------

dt[ , day_s := format(start, "%d")]
dt[ , day_e := format(end, "%d")]

# format:year-month-day ---------------------------------------------------

dt[, date_s := format(start, "%Y-%m-%d")]
dt[, date_e := format(end, "%Y-%m-%d")]


# sum: day ----------------------------------------------------------------

# tidyverse
dt1 <- dt %>% 
        group_by(member_id, day_s) %>% 
        summarise(daily_day = sum(diff_day))

# data.table
dt2 <- dt[, c("member_id", "day_s", "week_s", "date_s", "diff_min")]
dt2 <- dt2[ , lapply(.SD, sum), by = .(member_id, day_s, week_s, date_s)]
dt2 <- setorderv(dt2, c("member_id", "date_s", "day_s"))
head(dt2)

dt2[ , diff_min := as.numeric(diff_min)]
head(dt2)
p <- dt2 %>% 
        filter(member_id == 1061) 
        ggplot(aes(x = date_s, y = diff_min), data = p) +
        geom_bar(stat = "identity")
p
p
head(p)
