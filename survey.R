# prep and options --------------------------------------------------------

# rm and clear console
rm(list = ls()); cat("\14")
devtools::source_url("https://raw.githubusercontent.com/caiyuntingcfrc/misc/function_poverty/func_ins.pack.R")
# setwd
setwd("d:/R_wd/Dr Parenting/survey/")
# option: scipen
options(scipen = 999)
# library
ins.pack("tidyverse", "ggplot2", "plotly", 
         "epiDisplay")

# read csv files ----------------------------------------------------------

# complete cases
df <- readxl::read_xlsx("20200729_completed_results-survey615734.xlsx")

# filter: qualified -------------------------------------------------------

df <- df %>%
        # condition1: with chilidren
        filter(condition1 == "是") %>% 
        # qualified
        filter(item09 != "N/A") %>% 
        # filter(!is.na(`cantectA[Name]`)) %>%
        # filter(!is.na(`cantectA[Phone]`)) %>%
        # filter(!is.na(`cantectA[Email]`))
        filter(id != 15)

# desc --------------------------------------------------------------------

d <- df %>% .[ , 32:41]
summarytools::descr(d, transpose = TRUE)

test <- df %>% filter(token == "0tztmb")
