# ================= Calculation of Life Tables with RusFMD data ================
# ==============================================================================
# We use data from >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Russian Fertility and Mortality Database. Center for Demographic Research,
# Moscow (Russia). Available at http://demogr.nes.ru/index.php/ru/demogr_indicat/data
# (data downloaded on [22-10-2020]).
# ==============================================================================

# set working directory
setwd ("C:/Users/Ilya/Google Drive/R Projects/github/rusfmd-demographic-analysis") # <- set YOUR working directory
getwd ()

# install (if necessary) & load R packages
#install.packages ("tidyverse")
library (tidyverse)

# clear the R environment
ls ()
rm (list = ls ())

# load RusFMD files
dr1989_2014 <- read.table ("data/rusfmd/mortality/DR5a1989-2014.txt", sep=",", head=T, na = ".")
dr2015_2019 <- read.table ("data/rusfmd/mortality/DR5a2015-2019.txt", sep=",", head=T, na = ".")

# bind tables
dr <- bind_rows (dr1989_2014, dr2015_2019)
str (dr) # explore data

# transform data into longer format
dr <- dr %>%
  drop_na () %>%
  pivot_longer (cols = starts_with ("DrAa"), names_to = "Age", values_to = "DR")

# calculate Life Table
lt <- dr %>%
  transmute (year = Year, reg = Reg, group = Group, sex = Sex, x = Age, mx = DR) %>%
  filter (year == 2019 & reg == 1100 & group == "T" & sex == "B") %>% # select only one Year-Reg-Group-Sex
  mutate  (x = as.integer (gsub ("DrAa", "", x)),
           mx = mx/1000000,
           n = case_when (
             x == 0 ~ 1,
             x == 1 ~ 4,
             x %in% c(5:80) ~ 5),
           ax = case_when (
             x == 0 ~ 0.07+1.7*mx,
             x %in% c(1:80) ~ n/2,
             x == 85 ~ 1/mx),
           qx = case_when (
             x < 85 ~ n*mx/(1+(n-ax)*mx), # Chiang
             x == 85 ~ 1),
           px = 1-qx,
           lx = cumprod (lag (px, default = 1)), # need to explain the calculation
           dx = lx*qx,
           Lx = case_when (
             x < 85 ~ ax*lx+(n-ax)*lead(lx),
             x == 85 ~ ax*lx),
           ex = rev (cumsum (rev (Lx)))/lx) # Tx = ex; need to explain the calculation

# look at the results
lt

# export
write_csv (lt, "lt.csv")
