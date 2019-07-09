library(ggplot2)
library(dplyr)

## Latest data 2019
fl <- tempfile()
download.file("https://projects.fivethirtyeight.com/soccer-api/club/spi_global_rankings.csv", fl)
dat2019 <- readr::read_csv(fl)
dat2019$year <- '2019'

## 2017
dat2017 <- as_tibble(fivethirtyeight::spi_global_rankings)
dat2017$year <- '2017'

## Full data
dat <- bind_rows(dat2019, dat2017)
table(duplicated(dat$name))

## Select top 15 clubs in 2019 to compare to 2017
clubs <- dat2019 %>%
    select(name, year, spi) %>%
    filter(year==2019) %>%
    select(name) %>%
    head(15)

dat_top15 <- dat %>% filter(name %in% as.vector(t(clubs)))


## Plot 2019 vs 2017
ggplot(dat_top15, aes(x = reorder(name, -spi), y =spi, group=year,fill=year)) +
    geom_col(width=0.5, position = position_dodge(0.5)) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    xlab("Club Names") +
    scale_y_continuous(name = "Soccer Power Index (SPI)", limits = c(0,100)) +
    ggtitle("SPI of Clubs in 2019 vs their 2017 selves")

## Rank qualities
dat_top15 %>%
    ggplot(aes(y=reorder(name, -rank), x=rank, col=year)) +
    geom_point() +
    scale_x_continuous(name="Rank", limits = c(1,100)) +
    ylab("Football club") +
    geom_vline(xintercept = 15, alpha=0.2) +
    geom_hline(yintercept = 6, alpha=0.4, col="red")

## Offensive vs defensive
dat_top15 %>%
    ggplot(aes(x = off, y = def, color=year)) +
    geom_point() +
    facet_wrap(~name) +
    scale_x_continuous(name = "Offensive score",
                       limits=c(min(dat_top15$off), max(dat_top15$off))) +
    scale_y_continuous(name="Defensive score") +
    ggtitle('Top 15 teams, Offensive vs Defensive score',subtitle = 'ordered lexicographically')

## Overall trend
dat_top15 %>%
    ggplot(aes(x=def, y=off, group=year, color=year, fill=year)) +
    geom_point() +
    geom_smooth(method = "lm")
