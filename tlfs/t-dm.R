# *******************************************************************************************************
# Program Name:                 t-dm.R
# Project:                      TRA-025
# Purpose:                      Create Table of DM
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


# install.packages("janitor")

library(tidyverse)
library(Tplyr)
library(r2rtf)

load(url("https://tinyurl.com/2tev5w6n"))


bigN <- adsl |> 
  count(TRTP) |> 
  pivot_wider(
    names_from = TRTP, 
    values_from = n
  ) |> 
  mutate(
    Overall = Placebo + TRA25
  )


sex_fmt <- c("Male", "Female")


race_fmt <- c(
  "AMERICAN INDIAN OR ALASKA NATIVE" =  "American Indian / Alaska Native", 
  "ASIAN"   = "Asian"   , 
  "BLACK OR AFRICAN AMERICAN"   =  "Black or African American",
  "NATIVE HAWAIIN or Pacific ISLANDER" = "Native Hawaiin / Pacific Islander",
  "WHITE" = "White", 
  "NOT REPORTED" = "Other"
)

ethnic_fmt <- c(
  "HISPANIC OR LATINO" = "Hispanic or Latino",
  "NOT HISPANIC OR LATINO" = "Not Hispanic or Latino",
  "UNKNOWN" = "Unknown"
)

desc_fmt <- c("n", "Mean (SD)", "Median", "Min - Max")


adsl_tbl <- adsl |> 
  mutate(
    SEX = factor(SEX, levels = c("M", "F"), 
                      labels = sex_fmt), 
    RACE = factor(RACE, levels = names(race_fmt), 
                  labels = unname(race_fmt)), 
    ETHNIC = factor(ETHNIC, levels = names(ethnic_fmt), 
                  labels = unname(ethnic_fmt)), 
    
  )



adsl_tbl |> 
  tplyr_table(
    TRTP, 
    where = SAFFL == "Y"
  ) |> 
  set_desc_layer_formats(
    "n" = f_str("xx", n),
    "Mean (SD)" = f_str("xx.xx (xx.xxx)", mean, sd), 
    "Median" = f_str("xx.xx", median), 
    "Min - Max" = f_str("xx.x - xx.x", min, max)
  ) |>
  add_total_group() |> 
  add_layer(
    group_desc(
      AGE, by = "Age (years)"
    )
  ) |> 
  
  add_layer(
    group_count(
      SEX, by = "Sex"
    )
  ) |> 
  add_layer(
    group_count(
      RACE, by = "Race"
    )
  ) |> 
  add_layer(
    group_count(
      ETHNIC, by = "Ethnicity"
    )
  ) |> 
  build() |>
  apply_row_masks(row_breaks = T) |> 
  janitor::clean_names() |> 
  mutate(
    row_label3 = case_when(
      row_label2 == desc_fmt[1] ~ desc_fmt[1], 
      row_label2 == desc_fmt[2] ~ desc_fmt[2], 
      row_label2 == desc_fmt[3] ~ desc_fmt[3], 
      row_label2 == desc_fmt[4] ~ desc_fmt[4], 
      row_label2 != "" ~ "n (%)", 
      TRUE ~ ""
    ), 
    
    row_label2 = if_else(
      row_label2 %in% desc_fmt, "", row_label2
    )
  ) |> 
  select(starts_with("row_"), everything()) |> 
  select(1:6) 
  






























