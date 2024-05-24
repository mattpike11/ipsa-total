library(ggplot2)

columns <- c("Office.budget",                  
             "Reason.for.budget.set",         
             "Office.spend",                   
             "Remaining.office.budget")

df_vis <- df[c("X.MP.s.name",
               "Constituency",
               columns)]

plot <- ggplot(df_vis, aes(x=X.MP.s.name,y=Office.spend)) +
  geom_bar(stat = "identity") +
  labs(title = "Office Spend by Name",
       x = "Name",
       y = "Office Spend (£)") +
  theme_minimal()

