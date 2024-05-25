# 2: Do MPs use all of their allocated budget?

# expenditure data frame
df_spend_rem <- df_master %>%
  filter(Reason_for_budget_set...4 %in% c("Standard non-London area budget", "Standard London area budget")) %>% # 637 obs
  mutate(location = ifelse(Reason_for_budget_set...4 == "Standard non-London area budget", "Non-London", "London")) %>%
  select(location, matches("spend|uncapped|Remaining")) %>%
  group_by(location) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_longer(-location, names_to = "type", values_to = "total") %>%
  mutate(spend_or_remain = ifelse(grepl("Remaining",type),"Remaining","Spend")) %>%
  mutate(type = gsub("_spend", "", type)) %>%
  mutate(type = gsub("_budget", "", type)) %>%
  mutate(type = gsub("Remaining_", "", type)) %>%  
  mutate(type = gsub("uncapped", "(uncapped)", type)) %>%
  mutate(type = gsub("_", " ", type)) %>%
  mutate(type = str_to_sentence(type)) %>%
  filter(type != "Winding-up")


# stack bar chart
locations <- c("London","Non-London")

for (selected_location in locations){
  spend_rem_plt <- ggplot(df_spend_rem %>% filter(location == selected_location), aes(type,total, fill = spend_or_remain)) +
    geom_bar(position = "stack", stat = "identity") +
    scale_x_discrete("Type") +
    scale_y_continuous("Total Expenditure",labels = function(x) paste0("£",x/1000000,"M")) +
    labs(title = selected_location) +
    theme_minimal() +
    theme(legend.position = "bottom")
  show(spend_rem_plt)
}

