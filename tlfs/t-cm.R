# *******************************************************************************************************
# Program Name:                 t-cm.R
# Project:                      TRA-025
# Purpose:                      Create Table of CM
# Original Author:              Binod Jung Bogati (linkedin.com/in/bjungbogati)
# Copyright:                    © 2025. Unauthorized distribution or reuse prohibited.
# Date Created:                 8/11/2025
# Parameters:                   NA
#
# Input:                        
# Output:                       
# Modifications:
#   Date        By             Changes
# **********  *************  *************************************
#   
# ********************************************************************************************************/

# install.packages("Tplyr")


library(tidyverse)
library(Tplyr)


load(url("https://tinyurl.com/bp66vz5a"))


adcm <- adcm |> 
  mutate(
    CMDECOD = CMTRT
  )

total_c <- adsl |> 
  count(TRTP) |> 
  pivot_wider(
    names_from = TRTP, 
    values_from = n
  )

any_cm <- adcm |> 
  distinct(USUBJID, ARMCD, .keep_all = TRUE) |> 
  count(ARMCD, .drop = FALSE) |> 
  pivot_wider(
    names_from = ARMCD, 
    values_from = n
  ) |> 
  mutate(
    ATCPT = "Any concomitant medication",
    TRA25 = str_c(TRA25, " ( ", round(TRA25/total_c$TRA25 * 100, digits = 1), "%)"), 
    PLAC = str_c(PLAC, " ( ", round(PLAC/total_c$Placebo * 100, digits=1), "%)"), 
  ) |> 
  select(
    row_label2 = ATCPT,
    var1_TRA25 = TRA25, 
    var1_PLAC = PLAC
  )

adcm_table <- adcm |> 
tplyr_table(
  ARMCD
) |> 
  add_layer(
    group_count(
      vars(ATC, CMDECOD)
    )
  ) |> 
  build() |> 
  # select(2:4) |> 
  select(row_label2,  var1_TRA25, var1_PLAC )
  # arrange(desc(ord_layer_1), desc(ord_layer_2))
  

final_table <- bind_rows(any_cm, adcm_table) 


names(final_table) <- c(
  "ATC Class / Preferred Term", 
  str_c("Treatment A", " (N=", total_c$TRA25, ")"), 
  str_c("Placebo", " (N=", total_c$Placebo, ")")
)



















































