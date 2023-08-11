# Packages ----------------------------------------------------------------
library(tidyverse)    # data manipulation
library(here)         # reproducible file handling
library(lubridate)    # dealing with dates
library(readxl)       # excel files
# library(validate)     # validate dataset
library(janitor)
library(writexl)

# Load Data ---------------------------------------------------------------
dat_fb <- readxl::read_excel(here::here("data/lit_review_data - Frantisek.xlsx"))
dat_bs <- readxl::read_excel(here::here("data/lit_review_data_bjoern.xlsx"))
dat_sp <- readxl::read_excel(here::here("data/lit_review_data_SP.xlsx"))


# Merge Data --------------------------------------------------------------
# Check for non-overlapping cols
diff_cols <- setdiff(colnames(dat_sp), union(colnames(dat_fb), dat_bs))

# Delete redundant cols
dat_sp <- dat_sp[,!colnames(dat_sp) %in% diff_cols]
dat_bs <- dat_bs[,!colnames(dat_bs) %in% diff_cols]

# Find non-compatible cols
non_comp_cols <- janitor::compare_df_cols(dat_fb, 
                                          dat_bs, 
                                          dat_sp, 
                                          return = "mismatch")

# Convert numeric to character for safe merging
dat_bs <- dat_bs %>% 
  mutate(across(all_of(non_comp_cols$column_name),
                ~as.character(.)))
dat_sp <- dat_sp %>% 
  mutate(across(all_of(non_comp_cols$column_name),
                ~as.character(.)))

# Check compatibility again
janitor::compare_df_cols_same(dat_fb, dat_bs, dat_sp)

# Combine dataframes
all_res <- rbind(dat_fb, dat_bs, dat_sp)

# Clean names
all_res <- janitor::clean_names(all_res)

# Only look at valid simulation studies
sim_res <- all_res %>% 
  filter(simstudy_q1 == "yes") %>% 
  filter(x1 != "poor/medium") %>% 
  filter(x1 != "Poor/Medium")

# Fix data structure -----------------------------------------------------
# Dateofreview: proper conversion
sim_res$dateofreview <- as.Date(sim_res$dateofreview)

# Issue: Excel converted "2-3" to 44987
sim_res <- sim_res %>% 
  mutate(issue = as.character(issue)) %>% 
  mutate(issue = ifelse(issue == 44987, "2-3", 1))

# Software: Draw multiple mentions apart
sim_res <- sim_res %>% 
  tidyr::separate_wider_delim(cols = software_q18, 
                              delim = ", ",
                              names = c("software_1_q18", 
                                        "software_2_q18", 
                                        "software_3_q18"),
                              too_few = "align_start") 

# Seed: Fill in NAs with not found
sim_res <- sim_res %>% 
  mutate(seedprovided_q21 = tidyr::replace_na(seedprovided_q21, "not found"))


# Reformat most cols to factor
non_factor_vars <- c("year", "issue", "doi", "quoteaims_q3",
                     "note", "quote", "link", "comments", "dateofreview")

sim_res_fac <- sim_res %>% 
  mutate(across(!contains(non_factor_vars),
         ~as.factor(.)))

# Save data
writexl::write_xlsx(sim_res_fac, path = here("data/sim_res_fac.xlsx"))
saveRDS(sim_res_fac, file = here("data/sim_res_fac.RDS"))


# Alternative: delete strings from some columns, convert these to numeric
numeric_vars <- c("nsimstudies_q2", "nconds_q6", "q7", "q8", "q11", "q14")

# Remove everything but digits
sim_res_num <- sim_res_fac %>% 
  mutate(across(contains(numeric_vars),
         ~ as.numeric(stringr::str_extract(., "\\d+"))))

# Save data
writexl::write_xlsx(sim_res_num, path = here("data/sim_res_num.xlsx"))
saveRDS(sim_res_num, file = here("data/sim_res_num.RDS"))


