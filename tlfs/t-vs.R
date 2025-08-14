# *******************************************************************************************************
# Program Name:                 t-vs.R
# Project:                      TRA-025
# Purpose:                      Create Table of VS
# Original Author:              Binod Jung Bogati (linkedin.com/in/bjungbogati)
# Copyright:                    © 2025. Unauthorized distribution or reuse prohibited.
# Date Created:                 8/12/2025
# Parameters:                   NA
#
# Input:                        
# Output:                       
# Modifications:
#   Date        By             Changes
# **********  *************  *************************************
#   
# ********************************************************************************************************/


library(tidyverse)
library(r2rtf)


load(url("https://tinyurl.com/y7abk6h6"))




visit_fmt <- c( "SCREENING", "DAY 1",  "DAY 2",     "DAY 3" ,    "DAY 4" ,    "DAY 5" ,   
                "WEEK 6")

atpt_fmt <- c("Pre-infusion", "Post-infusion")

advs_tbl <- advs |> 
  select(PARAM, AVISIT, ATPT, AVAL, BASE, CHG, TRTP, ABLFL) |> 
  mutate(
    AVISIT = factor(AVISIT, levels = visit_fmt), 
    ATPT = factor(ATPT, levels = atpt_fmt)
  )




advs_plac <- advs_tbl |> 
  filter(TRTP == "Placebo")


fmt_c <- function(val, digits){
  
  round(val, digits) |> format(nsmall = digits)
  
}



desc_stats <- function(data, val) {
  
  data |> 
    group_by(PARAM, AVISIT, ATPT) |> 
    summarise(
      n = n() |> fmt_c(0), 
      mean = mean({{ val }}) |> fmt_c(1), 
      std = sd({{ val }}) |> fmt_c(2), 
      median = median({{ val }}) |> fmt_c(1),
      min = min({{ val }}) |> fmt_c(0), 
      max = max({{ val }}) |> fmt_c(0), 
      .groups = "drop"
    )  |> 
    pivot_longer(
      cols = n:max,
      names_to = "stats", 
      values_to = "val"
    )
  
}



aval <- bind_rows(
  desc_stats(advs_plac |> filter(AVISIT == "SCREENING"), BASE),
  desc_stats(advs_plac |> filter(AVISIT != "SCREENING" & ABLFL != "Y"), AVAL)
) |> 
  arrange(PARAM, AVISIT, ATPT)


chg <- desc_stats(advs_plac  |> filter(AVISIT != "SCREENING"), CHG)




vs_table <- left_join(
  aval, chg, by = c("PARAM", "AVISIT",  "ATPT" ,  "stats")
) |> 
  arrange(PARAM, AVISIT, ATPT) |> 
  mutate(
    AVISIT = if_else(AVISIT == "SCREENING", "Baseline", AVISIT), 
    AVISIT = if_else(stats != "n", "", AVISIT), 
    ATPT =  if_else(stats != "n", "", ATPT), 
    PARAM = if_else(AVISIT != "Baseline", "", PARAM)
  )


colheader <- str_c(
  "Parameter (unit)", 
  "VISIT", 
  "Time point", 
  "Statistics", 
  "Absolute Value", 
  "Change from Baseline", 
  sep = " | "
)


colheader0 <- str_c(
  "", 
  "", 
  "", 
  "", 
  "Placebo \n (N=xx)", 
  sep = " | "
)

vs_table |> 
  rtf_title("Table 14.3.2 \n Summary of Vital Signs and Change from Baseline by Visit") |> 
  rtf_colheader(colheader0) |> 
  rtf_colheader(colheader) |> 
  rtf_body() |> 
  rtf_encode() |> 
  write_rtf("14_table_vs-main/t-vs.rtf")






# 
# 
# 
# advs_sum <- advs_plac |> 
#   group_by(PARAM, AVISIT, ATPT) |> 
#   summarise(
#     n = n(), 
#     mean = mean(AVAL), 
#     std = sd(AVAL), 
#     median = median(AVAL),
#     min = min(AVAL), 
#     max = max(AVAL), 
#     .groups = "drop"
#   )  |> 
#   pivot_longer(
#     cols = n:max,
#     names_to = "stats", 
#     values_to = "val"
#   )
# 
# 
# 
# 
















