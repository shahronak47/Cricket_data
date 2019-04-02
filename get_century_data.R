library(rvest)
library(gganimate)
library(tidyverse)
library(gapminder)
library(gifski)
library(png)

first_part <- "http://stats.espncricinfo.com/ci/engine/stats/index.html?class=1;filter=advanced;orderby=batted_score;page="
second_part <- ";qualmin1=100;qualval1=batted_score;spanmin1=15+Mar+1980;spanval1=span;template=results;type=batting;view=innings"



get_table <- function (url) {
  url %>%
    read_html() %>%
    html_table(fill = TRUE) %>%
    .[[3]]
}

lst <- map(paste0(first_part, 1:55, second_part), get_table)
century_data$century_year <- year(dmy(century_data$Start_Date))
names(century_data)[11] <- "Start_Date"
century_data <- do.call(rbind, lst)
century_data[c(9, 13)] <- NULL
write.csv(century_data, "/Users/ronak/Downloads/Century.csv", row.names = FALSE)

century_data <- read.csv("/Users/ronak/Downloads/Century.csv")


cum_century <- century_data %>%
  group_by(Player, century_year) %>%
  summarise(century = n()) %>%
  ungroup() %>%
  arrange(Player, century_year) %>%
  group_by(Player) %>%
  mutate(century = cumsum(century))

write.csv(cum_century, "/Users/ronak/Downloads/Cum_Century.csv", row.names = FALSE)

n <- 1995

cum_century %>%
  group_by(century_year) %>%
  top_n(10, century) %>%
  ungroup() %>%
  ggplot() + 
  aes(Player, century, frame = century_year) + 
  geom_bar(stat="identity") + 
  coord_flip() +
  transition_time(century_year)


ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, colour = country)) +
  geom_point(alpha = 0.7) +
  scale_colour_manual(values = country_colors) +
  scale_size(range = c(2, 12)) +
  scale_x_log10() +
  facet_wrap(~continent) +
  theme(legend.position = 'none') +
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year)
