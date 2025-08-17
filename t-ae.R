# *******************************************************************************************************
# Program Name:                 t-vs.R
# Project:                      TRA-025
# Purpose:                      Create Table of VS
# Original Author:              Binod Jung Bogati (linkedin.com/in/bjungbogati)
# Copyright:                    © 2025. Unauthorized distribution or reuse prohibited.
# Date Created:                 8/18/2025
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

load(url("https://tinyurl.com/27b25u64"))

aesev_fmt <- c("MILD", "MODERATE", "SEVERE")

ae_max <- adae |>
  filter(TRTEMFL == "Y" & SAFFL == "Y") |> 
  group_by(USUBJID, TRTP, AESOC, AEDECOD) |>
  summarise(
    AESEV = as.character(max(factor(AESEV, levels = aesev_fmt, ordered = TRUE))),
    .groups = "drop"
  )

anyteae <- tplyr_table(adae, TRTP, where= TRTEMFL == "Y" & SAFFL == "Y") |>
  set_pop_data(adsl) |>
  set_pop_treat_var(TRTP) |>
  set_pop_where(TRUE) |>
  add_layer(
    group_count("Any TEAEs", vars(AESEV)) |>
      set_distinct_by(USUBJID)
  ) |>
  build() |>
  setNames(c(
    "row_label2", "row_label3", "var1_Placebo", "var1_TRA25"
  )) |> 
  select(1:4)

soc_pt <- tplyr_table(ae_max, TRTP) |>
  set_pop_data(adsl) |>
  set_pop_treat_var(TRTP) |>
  set_pop_where(TRUE) |>
  add_layer(
    group_count(vars(AESOC, AEDECOD), by = vars(AESEV))
  ) |>
  set_distinct_by(USUBJID) |>
  build() |>
  mutate(across(starts_with("var"), ~ if_else(ord_layer_3 == Inf, "", .x))) |>
  select(2:5) 


final_table <- bind_rows(
  anyteae, 
  soc_pt
) |> 
  pivot_wider(
    names_from = row_label2, # put severity as column headers
    values_from = c(`var1_TRA25`, `var1_Placebo`),
    values_fill = "0"
  )

final_table[final_table == "0 (  0.0%)"] <- "0"

blank_rows <- which(final_table[[2]] == "")
for(i in rev(blank_rows)) final_table <- final_table |> add_row(.before=i)

colheader <- str_c(
  "\\    Preferred Term",
  "Mild",
  "Moderate",
  "Severe",
  "Mild",
  "Moderate",
  "Severe",
  sep = " | "
)

colheader0 <- str_c(
  "System Organ Class",
  "",
  "Treatment \n (N=xx)",
  "",
  "",
  "Placebo \n (N=xx)",
  "",
  "",
  sep = "|"
)

final_table |>
  rtf_title("Table 4.1 \n Treatment Emergent Adverse Event by SOC PT by Maximum Severity \n Safety Population") |>
  rtf_colheader(
    colheader = colheader0,
    col_rel_width = c(1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
    border_top = "",
    border_left = "",
    border_bottom = "single",
    border_right = "",
    text_justification = c("l", rep("c", 6))
  ) |>
  rtf_colheader(
    colheader = colheader,
    col_rel_width = c(1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
    border_top = "",
    border_left = "",
    border_bottom = "",
    border_right = "",
    text_justification = c("l", rep("c", 6))
  ) |>
  rtf_body(
    col_rel_width = c(1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5),
    border_top = "",
    border_left = "",
    border_bottom = "",
    border_right = "",
    border_first = "",
    border_last = "",
    text_justification = c("l", rep("c", 6))
  ) |>
  rtf_encode() |>
  write_rtf("16_submission/t-ae.rtf")
