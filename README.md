# Canadian_employment
Employment in Canada





R code


Demographics

final_dataset %>%
group_by(`Education qualification`) %>%
summarize(Sum = sum(sum_of_graduates, na.rm = T))
final_dataset %>%
group_by(`Education qualification`) %>%
summarize(Sum = round(sd(sum_of_median_income, na.rm = T),0))
Bivariate and inferential analysis
# Median income by sex and educational qualification (Figure 2)
final_dataset %>%
group_by(`Education qualification`, Sex) %>%
summarise(`Median income` =
round(mean(sum_of_median_income, na.rm=TRUE),0)) %>%
ggplot(aes(x = `Education qualification`,
y = `Median income`, fill = Sex)) +
geom_col(position = "dodge") + coord_flip() +
theme_bw() +
theme(legend.key.height= unit(3, 'mm'),
legend.key.width= unit(3, 'mm'),
legend.title = element_text(size=8),
legend.text = element_text(size = 7),
axis.text.y = element_text(size = 8),
axis.text.x = element_text(size = 7),
text = element_text(family = "serif", color = "black"),
panel.grid.major = element_blank(),
panel.grid.minor = element_blank(),
axis.title = element_text(size=10))+
labs(y = "Median income", x = "Educational qualification")
