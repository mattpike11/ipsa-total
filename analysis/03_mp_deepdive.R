
# 3a: Historical trends
# Time Series Plot
df_per_time_series <- df_master %>%
  select(year, location, matches("spend|budget")) %>%
  group_by(year, location) %>%
  summarise(across(everything(), sum, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(office.usage = Office.spend / Office.budget,
         staff.usage = Staffing.spend / Staffing.budget,
         accomm.usage = Accommodation.spend / Accommodation.budget) %>%
  select(year, location, matches("usage")) %>%
  mutate(year = as.numeric(substr(year,1,4))) %>%
  pivot_longer(matches("usage"), names_to = "usage", values_to = "per") %>%
  filter(!(location == "London" & usage == "accomm.usage"))

df_per_time_series[df_per_time_series == Inf] <- NaN

text_df <- df_per_time_series %>%
  filter(year == max(year)) %>% # Filter for the most recent year
  mutate(label = gsub(".usage","",paste(location, usage)))
  

# Function to convert fin_year to "YYYY/YY" format
convert_to_fin_year <- function(year) {
  start_year <- floor(year)
  end_year <- start_year + 1
  return(paste0(start_year, "/", substr(as.character(end_year), 3, 4)))
}

mp_time_series <- ggplot(df_per_time_series, 
                         aes(x = year, y = per, 
                             color = usage,
                             linetype = location
                         )) +
  geom_line() +
  scale_x_continuous(breaks = unique(df_per_time_series$year),labels = function(x) {
    sapply(x, convert_to_fin_year)
  }) +
  scale_y_continuous("Percentage of Budget Used", labels = scales::percent, limits = c(0.6,1)) +
  labs(title = "Budget Usage Over Time by Location and Usage",
       x = "End of Financial Year") +
  # add labels
  geom_text(
    data = text_df %>%
      filter(year == max(year)),
    aes(
      x =  max(year),
      y = per,
      label = label
    ),
    position = position_nudge(x = 0.02),
    hjust = 0
  ) +
  coord_cartesian(expand = TRUE, clip = 'off') +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(0.1, 1.5, 0.1, 0.1, unit = 'in'))
plot(mp_time_series)


# 3b: Specific MPs
# Data Preparation
df_mp_usage <- df_master %>%
  filter(year == "2022/23") %>%
  mutate(office.usage = Office.spend / Office.budget,
         staff.usage = Staffing.spend / Staffing.budget,
         accomm.usage = Accommodation.spend / Accommodation.budget) %>%
  select(MP.name, location, matches("usage"))
df_mp_usage[df_mp_usage == Inf] <- NaN

df_boxplot <- df_mp_usage %>%
  pivot_longer(matches("usage"), names_to = "usage", values_to = "per")


# Box Plot
mp_boxplot <- ggplot(df_boxplot, aes(x = usage, y = per, fill = location)) +
  geom_boxplot() +
  scale_y_continuous("Percentage of budget used", labels = scales::percent, limits = c(0,1.09)) +
  theme_minimal() +
  coord_flip() +
  labs(title = "Budget Usage, 2022/23", 
       x = "Usage Category", 
       y = "Percentage of Budget Used") +
  theme(legend.position = "top",
      legend.title=element_blank(),
      plot.title = element_text(hjust = 0.5))
plot(mp_boxplot)

# Min and Max Staff Usage
df_mp_usage <- df_mp_usage %>% 
  filter(!is.na(staff.usage) ) %>%
  arrange(desc(staff.usage))

min_max_staff_usage <- rbind(head(df_mp_usage,3),tail(df_mp_usage,3)) %>%
  mutate_if(is.numeric, percent, accuracy = 0.01)

print(min_max_staff_usage)

# Min and Max Office Usage
df_mp_usage <- df_mp_usage %>% 
  filter(!is.na(office.usage) ) %>%
  arrange(desc(office.usage))

min_max_office_usage <- rbind(head(df_mp_usage,3),tail(df_mp_usage,3)) %>%
  mutate_if(is.numeric, percent, accuracy = 0.01)

print(min_max_office_usage)

