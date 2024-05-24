library(dplyr)
library(tidyr)
library(ggplot2)

df <- df_master %>%
  filter(Reason_for_budget_set...4 %in% c("Standard non-London area budget","Standard London area budget" )) %>% # 637 obs
  select(Office_spend,Staffing_spend,`Winding-up_spend`,Accommodation_spend,Travel_and_subsistence_uncapped,Other_costs_uncapped) %>%
  summarise_all(sum) %>%
  mutate(dummy = " ") %>%
  pivot_longer(!dummy, names_to = "type",values_to="total")


plt <- ggplot(df, aes(x = dummy, y = total, fill = type)) +
  geom_bar(position = "stack", stat = "identity")
show(plt)
