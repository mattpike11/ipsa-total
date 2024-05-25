library(scales)
# 1: What was the total expenditure?

# expenditure data frame
df_spend <- df_master %>%
  filter(Reason_for_budget_set...4 %in% c("Standard non-London area budget", "Standard London area budget")) %>% # 637 obs
  mutate(location = ifelse(Reason_for_budget_set...4 == "Standard non-London area budget", "Non-London", "London")) %>%
  select(location, matches("spend|uncapped")) %>%
  group_by(location) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_longer(-location, names_to = "type", values_to = "total") %>%
  filter(type != "Winding-up_spend") %>%
  mutate(type = gsub("_spend", "", type)) %>%
  mutate(type = gsub("uncapped", "(uncapped)", type)) %>%
  mutate(type = gsub("_", " ", type))


spend_plt <- ggplot(df_spend, aes(location, total, fill = type)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_x_discrete("") +
  scale_y_continuous("Total Expenditure",labels = function(x) paste0("£",x/1000000,"M")) +
  theme_minimal() +
  theme(legend.position = "bottom")
show(spend_plt)

total_spend <- sum(df_spend$total)
total_spend_ldn <- sum(df_spend$total[df_spend$location == "London"])
total_spend_nonldn <- sum(df_spend$total[df_spend$location != "London"])

spend_text <- paste(sep = "",
  "MP expenses totalled £",
  prettyNum(total_spend, big.mark = ","),
  " in 2022-23, of which £",
  prettyNum(total_spend_ldn, big.mark = ","),
  " (",
  percent(total_spend_ldn/total_spend,0.1),
  ") was attributed to those based in London."
)
print(spend_text)
