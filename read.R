
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

# sum: day ----------------------------------------------------------------

# # tidyverse
# dt1 <- dt %>%
        # group_by(member_id, started_at, ended_at) %>% 
        # summarise(daily_day = sum(diff_day))

# debug: duration ---------------------------------------------------------

# compare duration with diff_sec
if(with(dt, all((duration / 1000) == diff_sec, na.rm = TRUE))){
        # add to analysis file
        cat("\n##### duration #####\n", 
            "Duration is correct.\n",
            file = output_file_path, sep = "\n", append = TRUE)
}

# # debug: duplicated -------------------------------------------------------
# 
# # function fi()
# fi <- function(data){
#         m <- t(data)
#         m <- as.numeric(m[, which(m["duration", ] == max(m["duration", ]))]["n"])
#         return(m)
# }
# dupe_n1 <- nrow(dt) - nrow(unique(dt))
# 
# # 1: unique
# dt <- unique(dt)
# # 2: same started_at time but different ended_at
# # check with member_id and started_at
# check_dupe <- dt[ , c("account_id", "device_id", "member_id", "started_at")]
# # indices
# dupe_indices <- duplicated(check_dupe) | duplicated(check_dupe, fromLast = TRUE)
# # extract duplications and order by member_id
# dupe <- dt[dupe_indices, ][order(member_id), ]
# # save duplicated data (for PCC debugging)
# a <- str_extract(dir("duplicated/"), "[0-9]{4,}-[0-9]{4,}")
# b <- str_extract(file_name, "[0-9]{4,}-[0-9]{4,}")
# if(is.na(which(a == b))){
#     write_excel_csv(dupe[ , 1:7], 
#                     paste0(getwd(), "/duplicated/", "duplicated", "_", basename(file_name)))
# }
# # add to analysis file
# cat("\n##### duplicated #####\n", 
#     paste0("duplicated rows: ", dupe_n1, "\n"), 
#     paste0("duplicated rows with same start time: ", nrow(dupe), "\n"), 
#     file = output_file_path, sep = "\n", append = TRUE)
# # recode n and s
# dupe <- dupe[ , n:= 1:.N][ , s := fi(.SD), by = list(member_id, started_at)]
# # apply fi()
# dupe[ , s := fi(.SD), by = list(member_id, started_at)]
# # extract rows with larger duration and remove s and n
# dupe <- dupe[n == s, ][ , c("n", "s"):= NULL]
# # row binding
# dt_c <- rbind(dt, dupe)[order(member_id), ]
# 
# # add to analysis file
# cat("\n##### n:raw #####\n", 
#     paste0("the cleansed data has ", nrow(dt_c), " cases"), 
#     "\n", 
#     file = output_file_path, sep = "\n", append = TRUE)
# 
# # # diff
# # dupe[ , diff := as.numeric(ended_at - started_at)]
# # setorderv(dup, c("member_id", "started_at", "ended_at"))

# filter: account  ids ----------------------------------------------------

# survey
accID <- expss::read_spss("d:/R_wd/Dr Parenting/survey/20201005_merge.sav") %>% 
    expss::drop_all_labels() %>% .$app_account_ID
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
# setDT
df <- expss::read_spss("d:/R_wd/Dr Parenting/survey/20201005_merge.sav") %>% 
    expss::drop_all_labels()
setDT(df)
# assign values
df[unlist(l), device_id := deviceID]
df[unlist(l), accound_id := accountID]
df[unlist(l), member_id := memberID]
# order
df <- df[order(member_id)]

# filter: my ids ----------------------------------------------------------
# # account ID
# accID <- "13ad4046"
# myUsage <- dt[grep(accID, dt$account_id), ]
# 
# # device ID
# devID <- readLines("myDevID.txt")
# myUsage <- dt %>% 
#     filter(device_id == devID)
# 
# filePath <- paste0("d:/R_wd/Dr Parenting/pcc_myUsage/", b, "myUsage.csv"); filePath
# write_excel_csv(myUsage, filePath)

# save devID
# devID <- myUsage$device_id[[1]]
# fileConn <- file("myDevID.txt")
# writeLines(devID, fileConn)
# close(fileConn)

# # read: myUsage -----------------------------------------------------------
# 
# fileDir <- "d:/R_wd/Dr Parenting/pcc_myUsage/"
# filePath <- paste0(fileDir, list.files(fileDir))
# 
# df <- lapply(filePath, 
#              read.csv, 
#              fileEncoding = "CP950") %>% 
#     rbindlist()
# # names
# n <- c("account_id", names(df[ , -1]))
# setnames(df, n)
# 
# dt <- df

# calc --------------------------------------------------------------------

# data.table: min member id  day e
dt_min <- dt[ , c("member_id", "day_e", "week_e", "date_e", "diff_min")]
dt_agg <- dt_min[ , lapply(.SD, sum), by = .(member_id, day_e, week_e, date_e)][order(member_id)]

setorderv(dt_agg, c("member_id", "date_e", "day_e"))
head(dt_agg)

# data.table: hr member id
diff_hr <- dt[ , c("member_id", "day_e", "week_e", "date_e", "diff_hr")]
diff_hr <- diff_hr[ , lapply(.SD, sum), by = .(day_e, week_e, date_e, member_id)][order(member_id)]
dt_agg
head(dt2)

# data.table: hr device id date_s
dt2 <- dt[ , c("device_id", "day_s", "week_s", "date_s", "diff_hr")]
dt2 <- dt2[ , lapply(.SD, sum), by = .(day_s, week_s, date_s, device_id)]
setorderv(dt2, c("date_s", "day_s"))
head(dt2)
# data.table: hr device id date_e
dt2 <- dt[ , c("device_id", "day_e", "week_e", "date_e", "diff_hr")]
dt2 <- dt2[ , lapply(.SD, sum), by = .(day_e, week_e, date_e, device_id)]
setorderv(dt2, c("date_e", "day_e"))
head(dt2)

# total hours each day
dt_hr <- dt[ , c("date_s", "diff_hr", "device_id")]
# sum and keyby date_s
dt_hr <- dt_hr[ , lapply(.SD, sum), keyby = .(date_s, device_id)]
head(dt_hr)

# number of members each day
dt_member <- dt2[ , .N, by = .(date_s, week_s, day_s)]
head(dt_member)

# number of devices each day
dt_device <- dt2[ , .N, by = .(date_s, week_s, day_s)]
head(dt_device)

# cbind
dt_test <- dt_hr[dt_device][ , avr_hr := diff_hr / .N][ , avr_hr := as.numeric(avr_hr)]

# weekdays, weekends
dt_test[ , weekend := factor(ifelse(week_s == 6, "Sat", ifelse(week_s == 0, "Sun", "weekday")))]
dt_test[ , date_s := ymd(date_s)]
head(dt_test)


# debug: hr ---------------------------------------------------------------

dt_test <- dt[date_s %in% c("2020-06-19", "2020-06-20"), ]
dt_test <- unique(dt_test)



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
        scale_y_continuous(name = "number of users per day") +  
                           # expand = c(0, 0), 
                           # limits = c(0, max(dt_test$N) + 80)) +
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
        geom_line(aes(x = date_s, y = avr_hr), 
                  size = 1.2, 
                  group = 1) + 
        scale_x_date(name = "Date", date_breaks = "1 day") + 
        scale_y_continuous(name = "average usage time (hr/member)") +
        # scale_color_manual("", 
        #                   values = c("Sat" = "#04BFAD", 
        #                              "Sun" = "#F28D8D", 
        #                              "weekday" = "lightgrey")) +
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
