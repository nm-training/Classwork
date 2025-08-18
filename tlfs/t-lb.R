# *******************************************************************************************************
# Program Name:                 t-lb.R
# Project:                      TRA-025
# Purpose:                      Create Table of LB
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
library(Tplyr)
library(r2rtf)

load(url("https://tinyurl.com/4refv4m2"))

adlb_tbl <- adlb |> 
  mutate(
    BTOXGRH = factor(BTOXGRH, levels = c("Grade 0", "Grade 1", "Grade 2", "Grade 3", "Grade 4")),
    ATOXGRH = factor(ATOXGRH, levels = c("Grade 0", "Grade 1", "Grade 2", "Grade 3", "Grade 4"))
  )



adlb_shift <- tplyr_table(adlb_tbl, 
                          TRTP, 
                          where = SAFFL == "Y" & WTOXGRH == "Y" & TRTP == "TRA25" & PARCAT1 == "CHEMISTRY" & PARAMCD == "SODIUM") |> 
  
  add_layer(
    group_shift(
      vars(
        row = BTOXGRH, 
        column = ATOXGRH
      ), 
      by  = vars(PARAM, ATOXDSCH, PARCAT1)
    )
  ) |> 
  build() |>
  select(-starts_with("var1_Placebo"), -starts_with("ord_layer"))  |> 
  rename_with(~ str_remove_all(., "var1_TRA25_")) |> 
  mutate(
    across(
      starts_with("Grade"), as.numeric
    )
  ) |> 
  mutate(
    Total = rowSums(across(starts_with("Grade"))), .before= 10
  )

total_row <- adlb_shift |> 
  summarise(
    across(
      where(is.numeric), sum
    )
  ) |> 
  mutate(
    row_label4 = "Total"
  )

shift_tbl <- bind_rows(
  adlb_shift, 
  total_row
)


total_c <- adsl |> 
  count(TRTP) |> 
  pivot_wider(
    names_from = TRTP, 
    values_from = n
  )

final_table <- shift_tbl |> 
  mutate(
    `Grade 0` = str_c(`Grade 0`, " ( ", round(`Grade 0`/total_c$TRA25 * 100, digits = 1), "%)"), 
  ) 

final_table[final_table == "0 ( 0%)"] <- " 0"

colheader <- str_c(
  "Lab Parameter (unit)", 
  "Treatment", 
  "Baseline Grade", 
  "Grade 0", 
  "Grade 0", 
  "Grade 0", 
  "Grade 0", 
  "Grade 0", 
  sep = " | "
)

final_table |> 
  rtf_title("Table xx \n Summary ") |> 
  rtf_colheader() |> 
  rtf_body( border_left = NULL,
            border_right = NULL,
            border_top = NULL,
            border_bottom = NULL,
            border_first = "",
            border_last = "") |> 
  rtf_encode() |> 
  write_rtf("15_table_lb-main/t-lb.rtf")