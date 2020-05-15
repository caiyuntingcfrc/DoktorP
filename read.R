
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
         "dygraphs",
         "xts", 
         "lubridate", 
         "ggthemes", 
         "plotly", 
         "hrbrthemes")
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

dt[ , date_s := format(start, "%Y-%m-%d")]
dt[ , date_e := format(end, "%Y-%m-%d")]


# sum: day ----------------------------------------------------------------

# # tidyverse
# dt1 <- dt %>%
        # group_by(member_id, started_at, ended_at) %>% 
        # summarise(daily_day = sum(diff_day))

# debug:duplicated --------------------------------------------------------

# select
test <- dt[ , c("member_id", "started_at")]

# indices
dupe_indices <- duplicated(test) | duplicated(test, fromLast = TRUE)

# dupe
dupe <- dt[dupe_indices, c("member_id", "started_at", "ended_at", "duration")]

# diff
dupe[ , diff := as.numeric(ended_at - started_at)]
setorderv(dup, c("member_id", "started_at", "ended_at"))

# check if 
dup[(duration / 1000) != diff, ]
dup2 <- unique(dup)



# calc --------------------------------------------------------------------

# data.table
dt2 <- dt[, c("member_id", "day_s", "week_s", "date_s", "diff_min")]
dt2 <- dt2[ , lapply(.SD, sum), by = .(member_id, day_s, week_s, date_s)]
setorderv(dt2, c("member_id", "date_s", "day_s"))
head(dt2)

# data.table
dt2 <- dt[ , c("member_id", "day_s", "week_s", "date_s", "diff_hr")]
dt2 <- dt2[ , lapply(.SD, sum), by = .(day_s, week_s, date_s, member_id)]
setorderv(dt2, c("date_s", "day_s"))
head(dt2)

# total hours each day
dt_hr <- dt[ , c("date_s", "diff_hr")]
# sum and keyby date_s
dt_hr <- dt_hr[ , lapply(.SD, sum), keyby = .(date_s)]
head(dt_hr)

# number of members each day
dt_member <- dt2[ , .N, by = .(date_s, week_s, day_s)]
head(dt_member)

# cbind
dt_test <- dt_hr[dt_member][ , avr_hr := diff_hr / N][ , avr_hr := as.numeric(avr_hr)]

# weekdays, weekends
dt_test[ , weekend := factor(ifelse(week_s == 6, "Sat", ifelse(week_s == 0, "Sun", "weekday")))]
dt_test[ , date_s := ymd(date_s)]
head(dt_test)


# dygraphs ----------------------------------------------------------------

t <- xts(x = dt_test$avr_hr, order.by = dt_test$date_s)
p <- dygraph(t) %>% 
        dyOptions(stemPlot = TRUE)
p

# plot --------------------------------------------------------------------

# user per day
p1 <- ggplot(data = dt_test) +
        geom_bar(aes(x = date_s, y = N, fill = `weekend`), 
                 color = c("white"),
                 stat = "identity", alpha = .8) +
        scale_x_date(name = "Date", date_breaks = "1 day") +
        scale_y_continuous(name = "number of users per day", 
                           expand = c(0, 0), 
                           limits = c(0, max(dt_test$N) + 80)) +
        scale_fill_manual("", 
                          values = c("Sat" = "#04BFAD", 
                                     "Sun" = "#F28D8D", 
                                     "weekday" = "lightgrey")) +
        sjPlot::theme_sjplot2() +
        theme(axis.text.x = element_text(angle = 90, 
                                         vjust = .5), 
              legend.position = "top")
p1

p2 <- ggplot(data = dt_test) + 
        geom_line(aes(x = date_s, y = avr_hr, color = `weekend`), 
                  size = 1.2, 
                  group = 1) + 
        scale_x_date(name = "Date", date_breaks = "1 day") + 
        scale_y_continuous(name = "average usage time (hr/member)") +
        scale_color_manual("", 
                          values = c("Sat" = "#04BFAD", 
                                     "Sun" = "#F28D8D", 
                                     "weekday" = "lightgrey")) +
        sjPlot::theme_sjplot2() +
        theme(axis.text.x = element_text(angle = 90), 
              legend.position = "top")
p2


gridExtra::grid.arrange(p1, p2, nrow = 2)


# ribbon ------------------------------------------------------------------

dates <- c("02/01/00", "02/04/00", "02/07/00", "02/10/00", "02/01/01", "02/04/01", "02/07/01", "02/10/01")
dat <- data.frame(date=as.Date(dates, "%d/%m/%y"), count=(sample(1:8)))
dat["month"] <- as.factor(month(dat$date))
dat["df"] <- as.factor(dat$date)

dat2 <-data.frame(date=dat$date[-1],count=dat$count[-1], month=dat$month[-8],df=dat$df[-8])
dat3 <- rbind(dat,dat2)
ggplot(data=dat3, aes(x=date, ymax=count, ymin=0, group=df, fill=month)) + geom_ribbon()
