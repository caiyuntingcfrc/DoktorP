
# prep and options --------------------------------------------------------

# rm and clear console
rm(list = ls()); cat("\14")
devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/misc/function_poverty/func_ins.pack.R")
# setwd
setwd("d:/R_wd/Dr Parenting/pcc_usagetime")
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

# log file ----------------------------------------------------------------

# timestamp
timestamp <- format(Sys.time(), "%m%d-%H%M")
# processing time
ptm <- proc.time()

# file names
file_name <- file.choose()
# file directory
file_path <- paste(dirname(file_name), "/", "analysis", sep = "")
# create an "analysis" directory
if(length(dir(file_path)) == 0) {
        dir.create(file_path)
}
# output text file of anlysis
output_file_path <- paste(file_path, "/", timestamp, ".txt", sep = "")
# add to analysis file
cat("##### file #####\n", paste("The file is ", file_name, sep = ""), 
    "\n", 
    file = output_file_path, sep = "\n", append = FALSE)

# read the file -----------------------------------------------------------

# read the file with specified format
dt <- data.table::fread("d:/R_wd/Dr Parenting/pcc_usagetime/usage_time_20200826-20200925.csv")
dt <- read_csv(file = file_name, 
               col_types = "cccdTTd")
# add to analysis file
cat("\n##### n:raw #####\n", paste0("the data has ", nrow(dt), " cases", "\n"), 
    file = output_file_path, sep = "\n", append = TRUE)
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

# format: year-month-day --------------------------------------------------

dt[ , date_s := format(start, "%Y-%m-%d")]
dt[ , date_e := format(end, "%Y-%m-%d")]

# debug: duration ---------------------------------------------------------

# compare duration with diff_sec
if(with(dt, all((duration / 1000) == diff_sec, na.rm = TRUE))){
        # add to analysis file
        cat("\n##### duration #####\n", 
            "Duration is correct.\n",
            file = output_file_path, sep = "\n", append = TRUE)
}

# filter: account  ids ----------------------------------------------------

# survey
df <- expss::read_spss("d:/R_wd/Dr Parenting/survey/20201005_merge.sav") %>% 
    expss::drop_all_labels()

# setDT
setDT(df)

# accID
accID <- df$app_account_ID

# Dr parenting
pccID <- dt$account_id
# grep
l <- list()
for(i in 1:length(accID)){
        l[[i]] <- grep(accID[i], pccID)
}
# subset
dt <- dt[unlist(l), ]
# order by member_id
dt <- dt[order(member_id)]
# extract device id and account id
accountID <- unique(dt$account_id)
deviceID <- unique(dt$device_id)
memberID <- unique(dt$member_id)

l <- list()
for(i in 1:length(accountID)){
        l[[i]] <- grep(substr(accountID[[i]], 1, 8), df$app_account_ID)
}
# assign values
df[unlist(l), device_id := deviceID]
df[unlist(l), account_id := accountID]
df[unlist(l), member_id := memberID]
# order
df <- df[order(member_id)]

token_w1 <- c("prgxfq", "fo7nlx", "och739", "0tztmb", "ubi2fj")
# 1st wave and 2nd wave
w1 <- df %>% filter(token %in% token_w1)
w2 <- df %>% filter(!(token %in% token_w1))

# wave1 -------------------------------------------------------------------
# calc --------------------------------------------------------------------

# filter account_id
dt1 <- dt[account_id %in% w1$account_id, ]

# data.table: min member id  day e
dt_min <- dt1[ , c("member_id", "day_e", "week_e", "date_e", "diff_min")]
dt_agg <- dt_min[ , lapply(.SD, sum), by = .(member_id, day_e, week_e, date_e)][order(member_id)]
setorderv(dt_agg, c("member_id", "date_e", "day_e"))
# set key
setkey(dt_agg, member_id)
# as date
dt_agg[ , date_e := date(date_e)][ , week := week(date_e)]
# weekend
dt_agg[, weekday := wday(date_e)]
dt_agg[ , diff_min := as.numeric((diff_min))]

head(dt_agg)

dList <- list()

# usage time: average time per week ---------------------------------------

# # week
# dt_w <- dt_agg[ , c("member_id", "diff_min", "week")]
# dt_w <- dt_w[ , lapply(.SD, sum), by = .(member_id, week)]
# # freq: weeks per member id
# dt_w <- dt_w[ , .N, by = .(member_id, week, diff_min)][ , lapply(.SD, sum), by = .(member_id)]
# dt_w[ , n_Weeks := N]
# # remove
# dt_w <- dt_w[ , !c("N", "week")]
# # average time per week
# dt_w[ , avrMin_week_all := round(diff_min / n_Weeks, 2)]
# # subset
# d1 <- dt_w[ , c("member_id", "n_Weeks", "avrMin_week_all")]

# day
dt_w <- dt_agg[ , c("member_id", "diff_min", "weekday", "date_e")]
# freq: days
dt_w <- dt_w[ , .N, by = .(member_id, date_e, diff_min)]
dt_w <- dt_w[ , lapply(.SD, sum), by = .(member_id)][ , n_Days := N][ , !c("date_e", "N")]
# average time per weekday
dt_w[ , avrMin_day_all := round(diff_min / n_Days, 2)]
# subset
dt_w <- dt_w[ , c("member_id", "n_Days", "avrMin_day_all")]

head(dt_w)

dList[[length(dList) + 1]] <- dt_w

# usage time: weekdays ----------------------------------------------------

dt_w <- dt_agg[ , c("member_id", "diff_min", "weekday", "date_e")]
# exclude Saturday and Sunday
dt_w <- dt_w[!(weekday %in% c(7, 1)) , ]
# freq: days
dt_w <- dt_w[ , .N, by = .(member_id, date_e, diff_min)]
dt_w <- dt_w[ , lapply(.SD, sum), by = .(member_id)][ , n_Weekdays := N][ , !c("date_e", "N")]
# average time per weekday
dt_w[ , avrMin_weekdays := round(diff_min / n_Weekdays, 2)]
# subset
dt_w <- dt_w[ , c("member_id", "n_Weekdays", "avrMin_weekdays")]

head(dt_w)

dList[[length(dList) + 1]] <- dt_w


# usage time: weekends ----------------------------------------------------

# Saturday
dt_w <- dt_agg[ , c("member_id", "diff_min", "weekday", "date_e")]
# Saturday
dt_w <- dt_w[weekday == 7, ]
dt_w <- dt_w[ , lapply(.SD, sum), by = .(member_id)]
# number of Saturdays
dt_w <- dt_w[ , .N, by = .(member_id, date_e, diff_min)][ , n_Sat := N][ , !c("N", "date_e")]
# average time per week
dt_w[ , avrMin_Sat := round(diff_min / n_Sat, 2)]
d1 <- dt_w[ , c("member_id", "n_Sat", "avrMin_Sat")]

# Sunday
dt_w <- dt_agg[ , c("member_id", "diff_min", "weekday", "date_e")]
# Sunday
dt_w <- dt_w[weekday == 1, ]
dt_w <- dt_w[ , lapply(.SD, sum), by = .(member_id)]
# number of Saturdays
dt_w <- dt_w[ , .N, by = .(member_id, date_e, diff_min)][ , n_Sun := N][ , !c("N", "date_e")]
# average time per week
dt_w[ , avrMin_Sun := round(diff_min / n_Sun, 2)]
d2 <- dt_w[ , c("member_id", "n_Sun", "avrMin_Sun")]

# merge
dt_w <- d1[d2, on = "member_id"]

head(dt_w)

dList[[length(dList) + 1]] <- dt_w
# merge data list
dw1 <- Reduce(function(...) left_join(..., by = "member_id"), dList)

# wave2 -------------------------------------------------------------------
# calc --------------------------------------------------------------------

# filter after 20200913 and account_id
dt2 <- dt[started_at >= date("2020-09-13"), ][account_id %in% w2$account_id, ]

# data.table: min member id  day e
dt_min <- dt2[ , c("member_id", "day_e", "week_e", "date_e", "diff_min")]
dt_agg <- dt_min[ , lapply(.SD, sum), by = .(member_id, day_e, week_e, date_e)][order(member_id)]
setorderv(dt_agg, c("member_id", "date_e", "day_e"))
# set key
setkey(dt_agg, member_id)
# as date
dt_agg[ , date_e := date(date_e)][ , week := week(date_e)]
# weekend
dt_agg[, weekday := wday(date_e)]
dt_agg[ , diff_min := as.numeric((diff_min))]

head(dt_agg)

dList <- list()

# usage time: average time per week ---------------------------------------

# # week
# dt_w <- dt_agg[ , c("member_id", "diff_min", "week")]
# dt_w <- dt_w[ , lapply(.SD, sum), by = .(member_id, week)]
# # freq: weeks per member id
# dt_w <- dt_w[ , .N, by = .(member_id, week, diff_min)][ , lapply(.SD, sum), by = .(member_id)]
# dt_w[ , n_Weeks := N]
# # remove
# dt_w <- dt_w[ , !c("N", "week")]
# # average time per week
# dt_w[ , avrMin_week_all := round(diff_min / n_Weeks, 2)]
# # subset
# d1 <- dt_w[ , c("member_id", "n_Weeks", "avrMin_week_all")]

# day
dt_w <- dt_agg[ , c("member_id", "diff_min", "weekday", "date_e")]
# freq: days
dt_w <- dt_w[ , .N, by = .(member_id, date_e, diff_min)]
dt_w <- dt_w[ , lapply(.SD, sum), by = .(member_id)][ , n_Days := N][ , !c("date_e", "N")]
# average time per weekday
dt_w[ , avrMin_day_all := round(diff_min / n_Days, 2)]
# subset
dt_w <- dt_w[ , c("member_id", "n_Days", "avrMin_day_all")]

head(dt_w)

dList[[length(dList) + 1]] <- dt_w

# usage time: weekdays ----------------------------------------------------

dt_w <- dt_agg[ , c("member_id", "diff_min", "weekday", "date_e")]
# exclude Saturday and Sunday
dt_w <- dt_w[!(weekday %in% c(7, 1)) , ]
# freq: days
dt_w <- dt_w[ , .N, by = .(member_id, date_e, diff_min)]
dt_w <- dt_w[ , lapply(.SD, sum), by = .(member_id)][ , n_Weekdays := N][ , !c("date_e", "N")]
# average time per weekday
dt_w[ , avrMin_weekdays := round(diff_min / n_Weekdays, 2)]
# subset
dt_w <- dt_w[ , c("member_id", "n_Weekdays", "avrMin_weekdays")]

head(dt_w)

dList[[length(dList) + 1]] <- dt_w


# usage time: weekends ----------------------------------------------------

# Saturday
dt_w <- dt_agg[ , c("member_id", "diff_min", "weekday", "date_e")]
# Saturday
dt_w <- dt_w[weekday == 7, ]
dt_w <- dt_w[ , lapply(.SD, sum), by = .(member_id)]
# number of Saturdays
dt_w <- dt_w[ , .N, by = .(member_id, date_e, diff_min)][ , n_Sat := N][ , !c("N", "date_e")]
# average time per week
dt_w[ , avrMin_Sat := round(diff_min / n_Sat, 2)]
d1 <- dt_w[ , c("member_id", "n_Sat", "avrMin_Sat")]

# Sunday
dt_w <- dt_agg[ , c("member_id", "diff_min", "weekday", "date_e")]
# Sunday
dt_w <- dt_w[weekday == 1, ]
dt_w <- dt_w[ , lapply(.SD, sum), by = .(member_id)]
# number of Saturdays
dt_w <- dt_w[ , .N, by = .(member_id, date_e, diff_min)][ , n_Sun := N][ , !c("N", "date_e")]
# average time per week
dt_w[ , avrMin_Sun := round(diff_min / n_Sun, 2)]
d2 <- dt_w[ , c("member_id", "n_Sun", "avrMin_Sun")]

# merge
dt_w <- d1[d2, on = "member_id"]

head(dt_w)

dList[[length(dList) + 1]] <- dt_w
# merge data list
dw2 <- Reduce(function(...) left_join(..., by = "member_id"), dList)

# merge and join ----------------------------------------------------------

d_bind <- rbind(dw1, dw2)
d_bind <- d_bind[order(avrMin_day_all)]

# merge with df
d_merge <- left_join(df, d_bind, by  = "member_id")
# encoding
d_merge$contact_Name <- enc2native(d_merge$contact_Name)

# export
saveRDS(d_merge, "d:/R_wd/Dr Parenting/proc/20201007_merge.rds")

DescTools::Freq(d_merge$childage)
