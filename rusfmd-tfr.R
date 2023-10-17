# ================= Calculation of TFRs with RusFMD data =======================
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
br1989_2014 <- read.table("data/rusfmd/fertility/BRa1989-2014.txt",
                          sep = ",", head = T, na = ".")
br2015_2019 <- read.table("data/rusfmd/fertility/BRa2015-2019.txt",
                          sep = ",", head = T, na = ".")

# bind tables
br <- bind_rows(br1989_2014, br2015_2019)
str(br) # explore data

# transform data into longer format, transform age var into integer var
br <- br %>%
  pivot_longer(cols = starts_with("Bra"), names_to = "Age", values_to = "BR") %>%
  mutate(Age = as.integer(str_remove(Age, "Bra"))) # Age into integer

# calculate TFRs
tfr <- br %>%
  filter(Age %in% c(15:49)) %>% # drop ages above 49
  group_by(Year, Reg, Group) %>%
  summarise(TFR = round((sum(BR) / 1000000), 2)) %>%
  ungroup() %>%
  drop_na() # drop empty rows

# look at the results
head(tfr)

# plot
tfr %>%
  mutate(
    Group = case_when(
      Group == "T" ~ "Total",
      Group == "U" ~ "Urban",
      Group == "R" ~ "Rural"
    )
  ) %>%
  filter(Reg == 1165) %>% # see codebook at http://demogr.nes.ru/index.php/en/demogr_indicat/data_description
  ggplot(aes(Year, TFR)) +
  geom_line(aes(color = Group)) +
  scale_x_continuous(breaks = seq(1989, 2019, 1)) +
  labs(
    title = "Sverdlovsk oblast",
    x = "",
    y = "Children per woman aged 15 to 49 years",
    color = "Type of settlement"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# export (if necessary)
#ggsave("data/tfr.jpg", plot = last_plot(), dpi = 300)
#write_csv(tfr, "data/tfr.csv")
