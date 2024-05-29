# 1: What was the total expenditure?

# expenditure data frame
df_spend <- df_master %>%
  filter(year == "2022/23") %>%
  select(year, location, matches("spend|uncapped")) %>%
  group_by(year, location) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_longer(-c(year, location), names_to = "type", values_to = "total") %>%
  mutate(type = gsub(".spend", "", type)) %>%
  mutate(type = gsub("\\.", " ", type)) %>%
  mutate(type = factor(
    type,
    levels = c(
      "Other costs",
      "Travel and subsistence",
      "Accommodation",
      "Office",
      "Staffing"
    )
  ))



# create stacked bar chart of total spend
spend_plt <- ggplot(df_spend, aes(location, total, fill = type)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_x_discrete("") +
  scale_y_continuous(
    "Total Expenditure",
    labels = function(x)
      paste0("£", x / 1000000, "M")
  ) +
  scale_fill_manual(values = c("grey","purple","turquoise","orange","sky blue"),
                    guide = guide_legend(reverse = TRUE)) +
  labs(title = "Total Spend, 2022/23") + 
  # reverse legend and remove legend header
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.title=element_blank(),
        plot.title = element_text(hjust = 0.5))
plot(spend_plt)

# summary text
total_spend <- sum(df_spend$total)
total_spend_ldn <-
  sum(df_spend$total[df_spend$location == "London"])

spend_text <- paste(
  sep = "",
  "MP expenses totalled £",
  prettyNum(total_spend, big.mark = ","),
  " in 2022-23, of which £",
  prettyNum(total_spend_ldn, big.mark = ","),
  " (",
  percent(total_spend_ldn / total_spend, 0.1),
  ") was attributed to those based in London."
)
print(spend_text)
