library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

## create data frames

# budget data frame
df_budget <- df_master %>%
  filter(Reason_for_budget_set...4 %in% c("Standard non-London area budget","Standard London area budget" )) %>% # 637 obs
  select(Office_budget,Staffing_budget,`Winding-up_budget`,Accommodation_budget) %>%
  summarise_all(sum) %>%
  mutate(dummy = " ") %>%
  pivot_longer(!dummy, names_to = "type",values_to="total") %>%
  filter(total >0) %>%
  mutate(dummy = gsub("_budget","",type))

# expenditure data frame
df_spend <- df_master %>%
  filter(Reason_for_budget_set...4 %in% c("Standard non-London area budget","Standard London area budget" )) %>% # 637 obs
  select(matches("spend|uncapped")) %>%
  summarise_all(sum) %>%
  mutate(dummy = " ") %>%
  pivot_longer(!dummy, names_to = "type",values_to="total") %>%
  filter(total >0) %>%
  mutate(dummy = gsub("_spend","",type))

# surplus data frame
df_rem <- df_master %>%
  filter(Reason_for_budget_set...4 %in% c("Standard non-London area budget","Standard London area budget" )) %>% # 637 obs
  mutate(location = ifelse(Reason_for_budget_set...4 ==  "Standard non-London area budget", "Non-London","London")) %>%
  select(location,matches("Remaining")) %>%
  summarise_all(sum) %>%
  mutate(dummy = " ") %>%
  pivot_longer(!dummy, names_to = "type",values_to="total") %>%
  filter(total >0) %>%
  mutate(dummy = str_to_sentence(gsub("Remaining_","",gsub("_budget","",type))))


# total expenditure
exp_plt <- ggplot(df_spend, aes(x = dummy, y = total, fill = type)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_x_discrete("") +
  scale_y_continuous("",labels = function(x) paste0("£",x/1000000,"M"))
show(exp_plt)

# expenditure chart
exp_bud_plt <- ggplot(df_spend, aes(x = dummy, y = total, fill = type)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_x_discrete("") +
  scale_y_continuous("",labels = function(x) paste0("£",x/1000000,"M"))
show(exp_bud_plt)


# expenditure and surplus chart
df_spend_rem <- rbind(df_spend,df_rem) %>%
  filter(dummy %in% df_rem$dummy) %>%
  filter(total > 0)

exp_spend_rem_plt <- ggplot(df_spend_rem, aes(x = dummy, y = total, fill = type)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_x_discrete("") +
  scale_y_continuous("",labels = function(x) paste0("£",x/1000000,"M"))
show(exp_spend_rem_plt)
