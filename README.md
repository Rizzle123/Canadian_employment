# Canadian employment

This study seeks to answer the question, "What influence does one’s educational qualification have on income among male and female graduates?"

The following are available for review:

1. Dataframes
2. Code used for the study
3. Final pdf report
4. Microsoft PowerPoint presentation

The data was taken from Statistics Canada in 2022 at https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=3710011401

## R code

**Demographics**

```{r, eval=FALSE}
final_dataset %>% 
  group_by(`Education qualification`) %>% 
  summarize(Sum = sum(sum_of_graduates, na.rm = T))
```


```{r, eval=FALSE}
final_dataset %>% 
  group_by(`Education qualification`) %>% 
  summarize(Sum = round(sd(sum_of_median_income, na.rm = T),0))

```

**Bivariate and inferential analysis**


```{r, fig.cap = "Median income by sex and educational qualification", fig.height=3, message =FALSE, eval=FALSE}
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
```


```{r, eval=FALSE, warning=FALSE, fig.cap = "Mean median differences in income by educational qualification", message=FALSE, fig.height=3.5}
# The median differences in income by educational
#qualification were calculated and saved in 
#excel and plotted in R (Figure 3)

library(ggrepel)

Mean_income_differences_gender <- 
  read_csv("Mean_income_differences_gender.csv")

ggplot(data=Mean_income_differences_gender, 
       aes(x= `Educational qualification`, y= Difference, 
           color = Difference)) + geom_point(size = 4) + 
  coord_flip() + theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        text = element_text(family = "serif"),
        legend.key.height= unit(3, 'mm'), 
        legend.key.width= unit(3, 'mm'), 
        legend.title = element_text(size=7), 
        legend.text = element_text(size = 7), 
        axis.title = element_text(size = 8.5)) + 
  labs(x = "Educational qualification", 
       y = "Differences in median income") + 
  geom_text_repel(aes(label = Difference), 
                  box.padding   = 0.35,  
                  point.padding = 0.5, 
                  segment.color = 'grey50', size = 3)


```
<!-- Regression with dummy variables -->

<!-- When we use the contrasts(final_dataset$Sex) function, it tells us what the levels are coded as.  -->

```{r, eval=FALSE}

#Regression with dummy variables for certificate programs

certificate_only <- final_dataset %>% 
  filter(`Education qualification` == 
           "Career, technical or professional training certificate") %>% 
  lm(sum_of_median_income ~ Sex, data =.) %>% 
  summary()
  
certificate_only$coefficients[,c(1,4)]

```


```{r, include=FALSE, eval=FALSE}
#T test and Mann-Whitney U test for certificate programs. 
                #T test assumptions and result.
certificate_t_test <- final_dataset %>% 
  filter(`Education qualification` == "Career, technical or professional training certificate") 

shapiro.test(certificate_t_test$sum_of_median_income) #Normality test
library(car)#The levene's test is located here. Testing equality of variance.
leveneTest(certificate_t_test$sum_of_median_income, certificate_t_test$Sex, center=mean)
  
                     # Mann Whitney U test
library(ggpubr)
x <- final_dataset %>% 
  filter(`Education qualification` == "Career, technical or professional training certificate", Sex == "Male")

y <- final_dataset %>% 
  filter(`Education qualification` == "Career, technical or professional training certificate", Sex == "Female")

#The distribution in each is not the same!!!!The assume is the distribution in each must be about the same. Do some reading here.
ggdensity(x$sum_of_median_income)
ggdensity(y$sum_of_median_income)

wilcox.test(sum_of_median_income~Sex, mu=0, alt = "two.sided", conf.int=T, conf.level=0.95,exact =F, data = certificate_t_test)
```


```{r, eval=FALSE}
#Regression with dummy variables for diploma programs

diploma_only <- final_dataset %>% 
  filter(`Education qualification` == 
           "Career, technical or professional training diploma") %>% 
  lm(sum_of_median_income ~ Sex, data =.) %>% 
  summary()
  
diploma_only$coefficients[,c(1,4)]
```


```{r, eval=FALSE}
#Regression with dummy variables for dotorate programs

doctorate_only <- final_dataset %>% 
  filter(`Education qualification` == "Doctoral degree") %>% 
  lm(sum_of_median_income ~ Sex, data =.) %>% 
  summary()
  
doctorate_only$coefficients[,c(1,4)]
```

```{r, eval=FALSE}
#Regression with dummy variables for masters programs

masters_only <- final_dataset %>% 
  filter(`Education qualification` == "Master's degree") %>% 
  lm(sum_of_median_income ~ Sex, data =.) %>% 
  summary()
  
masters_only$coefficients[,c(1,4)]
```


```{r, eval=FALSE}
#Regression with dummy variables for professional programs

professional_only <- final_dataset %>% 
  filter(`Education qualification` == "Professional degree") %>% 
  lm(sum_of_median_income ~ Sex, data =.) %>% 
  summary()
  
professional_only$coefficients[,c(1,4)]
```

```{r, eval=FALSE}
#Regression with dummy variables for undergraduate programs

undergraduate_only <- final_dataset %>% 
  filter(`Education qualification` == "Undergraduate degree") %>% 
  lm(sum_of_median_income ~ Sex, data =.) %>% 
  summary()
  
undergraduate_only$coefficients[,c(1,4)]

```

```{r eval=FALSE, message=FALSE, warning=FALSE, fig.cap="Linear regression of male and female income by educational qualification", fig.width=6, fig.height=4}

#Correlation between male and female median income by 
#educational qualification (Figure 5)

library(ggpmisc)
library(ggpubr)


Male_median_income <- final_dataset%>% 
  dplyr::filter(Sex == "Male") %>% 
  dplyr::mutate(`Education qualification` = 
                  forcats::fct_recode(`Education qualification`, 
'Certificate' = "Career, technical or professional training certificate",
'Diploma' = "Career, technical or professional training diploma",
'Undergraduate degree' = "Undergraduate degree",
'Professional degree' = "Professional degree",
'Doctoral degree' = "Doctoral degree",
"Master's degree" = "Master's degree"))


Female_median_income <- final_dataset%>% 
  dplyr::filter(Sex == "Female")

ggplot(Male_median_income, aes(x = sum_of_median_income, 
    y = Female_median_income$sum_of_median_income)) + 
    geom_point()  +  stat_cor(method = "pearson",
    p.accuracy = 0.05, size = 2.9, position = "jitter") + 
  facet_wrap(~Male_median_income$`Education qualification`) + 
  stat_poly_line(se =FALSE) + 
  labs( y = "Females median income", x = "Males median income") + 
  theme_bw() + theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), 
                     text = element_text(family = "serif")) 


```


```{r, eval=FALSE, message=FALSE, fig.cap="Income difference by education qualification and year", fig.height=4,warning=FALSE}
#Income difference by educational qualification and year (Figure 6)

final_dataset %>% 
  pivot_wider(names_from = "Sex",
              values_from = sum_of_median_income) %>% 
  group_by(Year, `Education qualification`) %>% 
  summarize(Male = round(mean(Male, na.rm =T),0), 
            Female = round(mean(Female, na.rm=T),0)) %>% 
  mutate(Difference = Female - Male, Qualification = 
           forcats::fct_recode(`Education qualification`, 
'Certificate' = "Career, technical or professional training certificate",
'Diploma' = "Career, technical or professional training diploma",
'Undergraduate degree' = "Undergraduate degree",
'Professional degree' = "Professional degree",
'Doctoral degree' = "Doctoral degree",
"Master's degree" = "Master's degree")) %>% 
  ggplot(aes(x = Year, y = Difference, 
             group= Qualification, color = Qualification)) + 
  geom_point() + geom_line(size=1, alpha=.8) + theme_bw() + 
  theme(legend.key.height= unit(3, 'mm'), 
        legend.key.width= unit(3, 'mm'), 
        legend.title = element_text(size=8), 
        legend.text = element_text(size = 7), 
        axis.text.y = element_text(size = 8),  
        axis.text.x = element_text(size = 7), 
        text = element_text(family = "serif", color = "black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.title = element_text(size=10)) + 
  labs(y = "Income difference")


```


```{r, warning=FALSE, message=FALSE, comment= NA}

#Percentage change in differences in median income by qualification
#for the period 2010-2015

x <- final_dataset %>% 
  pivot_wider(names_from = "Sex",
              values_from = sum_of_median_income) %>% 
  group_by(Year, `Education qualification`) %>% 
  summarize(Male = round(mean(Male, na.rm =T),0), 
            Female = round(mean(Female, na.rm=T),0)) %>% 
  mutate(Difference = Female - Male, Qualification = 
           forcats::fct_recode(`Education qualification`, 
'Certificate' = "Career, technical or professional training certificate",
'Diploma' = "Career, technical or professional training diploma",
'Undergraduate degree' = "Undergraduate degree",
'Professional degree' = "Professional degree",
'Doctoral degree' = "Doctoral degree",
"Master's degree" = "Master's degree")) %>% 
  group_by(Year, Qualification) %>% 
  summarize(Difference = sum(Difference)) %>% 
  pivot_wider(names_from = "Year",
              values_from = Difference) %>% 
  mutate(Percentage_change = round((`2015`-`2010`)/`2010`*100, 0)) %>% 
  select(Qualification, Percentage_change) %>% 
  arrange(Percentage_change) 

data.frame(x)

```

## References

1. Statistics Canada (2022). *Immigrants make up the largest share of the population in over 150 years and continue to shape who we are as Canadians*. Retrieved November 22, 2022 from         https://www150.statcan.gc.ca/n1/daily-quotidien/221026/dq221026a-eng.htm


2. Loriggio, P., (2022). *Toronto police chief apologizes to Black community as race-based data released*.
Retrieved November 22, 2022 from https://globalnews.ca/news/8922183/toronto-police-chief-apologizes-black-community-race-based-data/

3. Kassambara, A. (2018). *Machine Learning Essentials: Practical Guide in R*. CreateSpace Independent Publishing Platform.  
