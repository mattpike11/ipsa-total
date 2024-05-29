# 2: Do MPs use all of their allocated budget?

# expenditure data frame
df_spend_rem <- df_master %>%
  filter(year == "2022/23") %>%
  select(year, location, matches("spend|Remaining")) %>%
  group_by(year, location) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_longer(-c(year, location), names_to = "type", values_to = "total") %>%
  mutate(spend_or_remain = ifelse(grepl("Remaining", type), "Remaining", "Spend")) %>%
  mutate(type = gsub(".spend", "", type)) %>%
  mutate(type = gsub("Remaining.", "", type)) %>%
  mutate(type = gsub(".budget", "", type)) %>%
  mutate(type = gsub("\\.", " ", type)) %>%
  mutate(type = str_to_sentence(type)) %>%
  mutate(type = factor(
    type,
    levels = c(
      "Staffing",
      "Office",
      "Accommodation",
      "Travel and subsistence",
      "Other costs"
    )))
df_spend_rem[df_spend_rem == Inf] <- NaN

# stack bar chart
locations <- c("London","Non-London")

for (selected_location in locations){
  spend_rem_plt <- ggplot(df_spend_rem %>% filter(location == selected_location), aes(type,total, fill = spend_or_remain)) +
    geom_bar(position = "stack", stat = "identity") +
    scale_x_discrete("") +
    scale_y_continuous("Total Expenditure",labels = function(x) paste0("£",x/1000000,"M")) +
    scale_fill_manual(values = c("light grey","sky blue"),
                    guide = guide_legend(reverse = TRUE)) +
    labs(title = paste(selected_location, "Total Spend, 2022/23")) + 
    theme_minimal() +
    theme(legend.position = "bottom",
          legend.title=element_blank(),
          plot.title = element_text(hjust = 0.5))
  show(spend_rem_plt)
}

