library(tidyverse)
library(scales)

setwd('Learning/Challenges/MakeoverMonday/week37')

teachers <- read.csv("https://query.data.world/s/6hngybdsptcx4odq2a4icqhrfpozng", header=TRUE, stringsAsFactors=FALSE);

teachers <- teachers %>% 
  filter(gender == "Male"| gender == "Female") %>%
  mutate(year = substr(time_period, 1, 4)) %>%
  filter(grade != "Total") %>%
  mutate(wage = as.numeric(average_mean)) %>%
  mutate(gender = factor(gender, levels = c("Male", "Female"))) %>%
  mutate(grade = factor(grade, levels = c("Classroom teachers", "Other Leadership teachers", "Head teachers")))


get_lower_value <- function(row1, df) {
  if (row1[['gender']] != 'Female') {
    df <- df %>%
      filter(grade == row1[['grade']]) %>%
      filter(time_period == row1[['time_period']]) %>%
      filter(gender == "Female")
    return(df[['wage']])
  } else {
    return(row1[['wage']])
  }
}

df_as_rows <- split(teachers, seq(nrow(teachers)))
lower_values <- map_dbl(df_as_rows, get_lower_value, teachers)
teachers$lower_values <- lower_values

ggplot(aes(x = year, y = wage, group = gender, color = gender, fill = gender), data = teachers) +
  geom_point() + 
  geom_path() + 
  geom_ribbon(data=teachers, 
              aes(ymin=lower_values - 2,ymax=wage), alpha=0.5) +
  facet_wrap(vars(grade), strip.position = "bottom") +
  ggtitle("THE TEACHER\n    GENDER PAY GAP\n       GROWS WITH SENIORITY") +
  xlab("Year") + 
  ylab("Average Salary") + 
  scale_y_continuous(labels = label_dollar(prefix = "£"), limits = c(30000, 80000)) + 
  scale_x_discrete(breaks = c("2010", "2015", "2019")) +
  scale_color_manual(values = c("#2b287a", "#e0a551")) + 
  scale_fill_manual(values=c("#2b287a", "white")) +
  theme_minimal() + 
  theme(plot.title = element_text(vjust = -18.5, hjust = 0, face = "bold", color = "#605cad", size = 24),
        text = element_text(),
        axis.title.y = element_text(),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.9, 0.15), 
        panel.grid = element_blank(),
        panel.grid.major.y = element_line(color = "#A9A9A9", linetype = 2))

ggsave("week37.pdf", width = 8, height = 6, units = "in")
