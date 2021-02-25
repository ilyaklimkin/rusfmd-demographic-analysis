# ================= Calculation of TFRs with RusFMD data =======================
# ==============================================================================
# We use data from >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Russian Fertility and Mortality Database. Center for Demographic Research,
# Moscow (Russia). Available at http://demogr.nes.ru/index.php/ru/demogr_indicat/data
# (data downloaded on [22-10-2020]).
# ==============================================================================

# set working directory
setwd ("C:/Users/Ilya/Google Drive/R Projects/github/rusfmd-demographic-analysis") # <- set YOUR wd
getwd ()

# install (if necessary) & load R packages
#install.packages ("tidyverse")
library (tidyverse)

# clear the R environment
ls ()
rm (list = ls())

# load RusFMD files
br1989_2014 <- read.table ("data/rusfmd/fertility/BRa1989-2014.txt", sep=",", head=T, na = ".")
br2015_2019 <- read.table ("data/rusfmd/fertility/BRa2015-2019.txt", sep=",", head=T, na = ".")

# bind tables
br <- bind_rows(br1989_2014, br2015_2019)
str (br) # explore data

# transform data into longer format, transform age var into integer var
br <- br %>%
  pivot_longer (cols = starts_with("Bra"), names_to = "Age", values_to = "BR") %>%
  mutate (Age = as.integer(gsub("Bra","",Age))) # Age into integer

# calculate TFRs
tfr <- br %>%
  filter (Age %in% c(15:49)) %>% # filter ages
  group_by (Year, Reg, Group) %>%
  summarise (TFR = round((sum(BR)/1000000),2)) %>%
  ungroup () %>%
  #drop_na ()

# look at results
head (tfr)

# export
write_csv (tfr, "tfr.csv")
