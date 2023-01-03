library(readr)
library(ggplot2)
library(tidyverse)
library(ggthemes)

data <- read.csv("C:\\Users\\User\\Documents\\netflix_titles_cleaned.csv")
View(data)

dv1 <- data %>%
  count(type) %>%
  mutate(percentage=n/nrow(data))
dv1

p1 <- ggplot(dv1, aes(x="", y=n, fill=type)) +
  geom_bar(stat = "identity") +
  coord_polar("y") +
  theme(axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        panel.grid  = element_blank(),
        axis.line = element_blank(),
        legend.position="none",
        plot.background = element_rect(fill = "#D5E4EB"),
        panel.background = element_rect(fill = "#D5E4EB", colour = "#D5E4EB"),
        panel.border = element_blank()) +
  geom_text(aes(label=n), position = position_stack(vjust = 0.5)) +
  geom_text(aes(label=paste(round(percentage*100, digits = 2),"%")), position = position_stack(vjust = 0.65)) +
  labs(
    x = "",
    y = "",
    title = "Number and Percentage of \nNetflix Movies and TV Shows") 
p1


dv2 <- data %>%
  group_by(release_year, type) %>%
  summarise(count = n()) %>%
  filter(release_year > 1980) 
dv2

p2 <- ggplot(dv2[1:78,], aes(x=release_year, y=count, group=type, color=type))+
  geom_line(size=1.5, alpha=0.7) +
  scale_x_discrete(breaks=seq(1980, 2021, 5)) +
  theme_classic() +
  theme(legend.position="none",
        plot.background = element_rect(fill = "#D5E4EB"),
        panel.background = element_rect(fill = "#D5E4EB", colour = "#D5E4EB"),
        panel.border = element_blank()) +
  labs(
    x = "Release Year",
    y = "Number of Movies/TV Shows",
    title = "Release Year of Netflix \nMovies/TV Shows as of 2021")
p2

dv3 <- data %>%
  select(year_added, type) %>%
  filter(!is.na(year_added)) %>%
  group_by(year_added, type) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))
dv3 <- dv3[1:12,]

p3 <- ggplot(dv3, aes(x = year_added, y = Count, fill=type, color=type, group = type)) +
  geom_bar(stat='identity', position='dodge', width=0.5) +
  geom_line(aes(x = year_added, y = Count), position=position_dodge(width = 0.5), size=1) +
  theme_classic() +
  theme(legend.position="none",
        plot.background = element_rect(fill = "#D5E4EB"),
        panel.background = element_rect(fill = "#D5E4EB", colour = "#D5E4EB"),
        panel.border = element_blank()) +
  labs(
    x = "Year",
    y = "Number of Movies/TV Shows",
    title = "Number of Movies/TV Shows \nAdded Per Year")
p3

dv4 <- data %>%
  filter(country != is.na(country)) %>%
  mutate(country = fct_lump(country, 10)) %>%
  select(country, type) %>%
  group_by(country, type) %>%
  filter(country != "Other") %>%
  summarise(count = n()) %>%
  filter(country != "") %>%
  arrange(desc(count))
dv4

p4 <- ggplot(dv4, aes(x=count, y=reorder(country, count), fill=type)) +
  geom_bar(stat='identity') +
  theme_classic() +
  theme(plot.background = element_rect(fill = "#D5E4EB"),
        panel.background = element_rect(fill = "#D5E4EB", colour = "#D5E4EB"),
        panel.border = element_blank(),
        legend.background =element_rect(fill = "#D5E4EB")) +
  labs(
    x = "Number of Movies/TV Shows",
    y = "Country",
    title = "Top Countries that \nProduce Most Shows")
p4

if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
library(ggpubr)

#final <- ggarrange(p4,p1,p2,p3, common.legend=TRUE, ncol = 2, nrow = 2, legend='bottom', align = 'h') +
#  bgcolor("#D5E4EB") 
#annotate_figure(final, top = text_grob("Netflix Movies and TV Shows", color = "red", face = "bold", size = 14))

devtools::install_github("thomasp85/patchwork")
library(patchwork)

patchwork <- (p1 | p4) /
(p2 | p3)
patchwork + plot_annotation(
  title='Netflix Movies and TV Shows',
  caption = 'Data source: Kaggle'
)


