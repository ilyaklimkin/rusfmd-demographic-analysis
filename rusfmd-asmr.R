# ================= Calculation of ASMRs with RusFMD data =======================
# ==============================================================================
# We use data from >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Russian Fertility and Mortality Database. Center for Demographic Research,
# Moscow (Russia). Available at http://demogr.nes.ru/index.php/ru/demogr_indicat/data
# (data downloaded on [22-10-2020]).
# ==============================================================================

# install (if necessary) & load R packages
#install.packages("tidyverse")
library(tidyverse)

# clear the R environment
ls()
rm(list = ls())

# load RusFMD files
dr1989_2014 <- read.table("data/rusfmd/mortality/DR5a1989-2014.txt",
                          sep = ",", head=T, na = ".")
dr2015_2019 <- read.table("data/rusfmd/mortality/DR5a2015-2019.txt",
                          sep = ",", head=T, na = ".")

# unite tables
dr <- bind_rows(dr1989_2014, dr2015_2019)
str(dr) # explore data

# transform data into longer format (tidy data)
dr <- dr %>%
  pivot_longer(cols = starts_with("DrAa"), names_to = "Age", values_to = "DR")

# set the age standard (European Population Standard 1976 by default)
Age <- unique(dr$Age)
Weight <- c(0.016,0.064,0.07,0.07,0.07,0.07,0.07,0.07,0.07,0.07,0.07,
            0.07,0.06,0.05,0.04,0.03,0.02,0.01,0.01) # set the vector of weights
ESP <- tibble(Age, Weight) # merge the vector of age groups and the vector population weights into tibble

# join population weights with mortality data
dr <- left_join(dr, ESP, by = "Age")

# calculate ASMRs (per 1000 person-years)
asmr <- dr %>%
  mutate(DR_Weighted = DR * Weight) %>%
  group_by(Year, Reg, Group, Sex) %>%
  summarise(ASMR_per_1000 = round((sum(DR_Weighted) / 1000), 1)) %>%
  ungroup ()

# look at the results
head(asmr)

# plot
asmr %>%
  mutate(
    Group = case_when(
      Group == "T" ~ "Total",
      Group == "U" ~ "Urban",
      Group == "R" ~ "Rural"
    ),
    Sex = case_when(
      Sex == "M" ~ "Males",
      Sex == "F" ~ "Females",
      Sex == "B" ~ "Both sexes"
    )
  ) %>%
  filter(Reg == 1100) %>% # see codebook at http://demogr.nes.ru/index.php/en/demogr_indicat/data_description
  ggplot(aes(Year, ASMR_per_1000)) +
  geom_line(aes(color = Group)) +
  facet_wrap(~Sex) +
  scale_x_continuous(breaks = seq(1990, 2020, 5)) +
  labs(
    title = "Russia",
    x = "",
    y = "Age-standardised mortality rate,\nper 1000 person-years",
    color = "Type of settlement"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

# export (if necessary)
#ggsave("data/asmr.jpg", plot = last_plot(), dpi = 300)
#write_csv(asmr, "data/asmr.csv")
