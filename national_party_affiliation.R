library(tidyverse)
library(lubridate)
library(rvest)
library(stringr)

party_affiliation_file <- "data/party_affliliation.RData"

if (!dir.exists("data")) {
  dir.create("data")
}

if (!file.exists(party_affiliation_file)) {
  party_affiliation <- read_html("https://news.gallup.com/poll/15370/Party-Affiliation.aspx")

  parties_table <- party_affiliation %>%
    html_node(xpath = "/html/body/div[2]/div/main/div/article/div[1]/section[1]/div[1]/div/div/figure/table") %>%
    html_table(fill = TRUE)

  leaning_table <- party_affiliation %>%
    html_node(xpath = "/html/body/div[2]/div/main/div/article/div[1]/section[1]/div[2]/div/div/figure/table") %>%
    html_table(fill = TRUE)
  
  save(parties_table, leaning_table, file = party_affiliation_file)
} else {
  load(party_affiliation_file)
}

date_pattern <- "(\\d{4}) (\\w+) (\\d+)"

parties <- parties_table %>%
  select(date_range = X1, Republicans = X2, Independents = X3, Democrats = X4) %>%
  filter(dplyr::row_number() > 2, date_range != "Gallup") %>%
  extract(date_range, c("year", "month", "day"), date_pattern) %>%
  mutate(Date = ymd(str_c(year, month, day, sep = "-"))) %>%
  select(-year, -month, -day) %>%
  gather(-Date, key = "Party", value = "Percent") %>%
  mutate(Party = as_factor(Party), Percent = as.numeric(Percent))

leaning <- leaning_table %>%
  select(date_range = X1, Republicans = X2, Democrats = X3) %>%
  filter(dplyr::row_number() > 2, date_range != "Gallup") %>%
  extract(date_range, c("year", "month", "day"), date_pattern) %>%
  mutate(Date = ymd(str_c(year, month, day, sep = "-"))) %>%
  select(-year, -month, -day) %>%
  gather(-Date, key = "Party", value = "Percent") %>%
  mutate(Party = as_factor(Party), Percent = as.numeric(Percent))

parties_plot <- parties %>%
  ggplot() +
  geom_line(aes(Date, Percent, group = Party, color = Party)) +
  scale_color_manual(values = c("Democrats" = "blue", "Republicans" = "red", "Independents" = "grey")) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal() +
  geom_smooth(aes(Date, Percent, group = Party, color = Party))

leaning_plot <- leaning %>%
  ggplot() +
  geom_line(aes(Date, Percent, group = Party, color = Party)) +
  scale_color_manual(values = c("Democrats" = "blue", "Republicans" = "red")) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_minimal() +
  geom_smooth(aes(Date, Percent, group = Party, color = Party))
