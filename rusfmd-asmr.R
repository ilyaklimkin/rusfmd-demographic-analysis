# ================= Calculation of ASMRs with RusFMD data =======================
# ==============================================================================
# We use data from >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Russian Fertility and Mortality Database. Center for Demographic Research,
# Moscow (Russia). Available at http://demogr.nes.ru/index.php/ru/demogr_indicat/data
# (data downloaded on [22-10-2020]).
# ==============================================================================

# set working directory
setwd ("C:/XXX") # <- set YOUR working directory
getwd ()

# install (if necessary) & load R packages
#install.packages ("tidyverse")
library (tidyverse)

# clear the R environment
ls ()
rm (list = ls())

# load RusFMD files
dr1989_2014 <- read.table ("DR5a1989-2014.txt", sep=",", head=T, na = ".")
dr2015_2019 <- read.table ("DR5a2015-2019.txt", sep=",", head=T, na = ".")

# bind tables
dr <- bind_rows(dr1989_2014, dr2015_2019)
str (dr) # explore data

# transform data into longer format
dr <- dr %>%
  pivot_longer (cols = starts_with("DrAa"), names_to = "Age", values_to = "DR")

# set the age standard (European Population Standard 1976 by default)
Age <- unique(dr$Age)
Weight <- c(0.016,0.064,0.07,0.07,0.07,0.07,0.07,0.07,0.07,0.07,0.07,0.07,0.06,0.05,0.04,0.03,0.02,0.01,0.01) # set the vector of weights
ESP <- as.data.frame(cbind(Age,Weight)) # merge the vector of age groups and the vector population weights
ESP$Weight <- as.numeric(ESP$Weight) # transform weights into numeric format

# join population weights with mortality data
dr <- left_join (dr, ESP)

# calculate ASMRs
asmr <- dr %>%
  mutate (DR_Weighted = DR*Weight) %>%
  group_by (Year, Reg, Group, Sex) %>%
  summarise (ASMR_per_1000 = round((sum(DR_Weighted)/1000),1)) %>%
  ungroup ()

# export
write_csv (asmr, "asmr.csv")
