library(tidyverse)
library(lubridate)
library(colorspace)
library(scales)


loans <- read_csv("2019/2019-11-26/loans.csv")

loans_df <- loans %>% 
    select(year, quarter, starting, total) %>% 
    group_by(year, quarter) %>% 
    summarise(start_value = sum(starting, na.rm = TRUE)) %>% 
    ungroup()


qs <- tibble(quarter = c(1:4), month = c(1,4,7,10))

df <- 
    loans_df %>% 
    left_join(qs, by = "quarter") %>% 
    mutate(quarter_start = ymd(paste0("20",year,"=",month,"=",1))) %>% 
    select(quarter_start, start_value)


df1 <- df  %>%
    mutate(
        ymax = cumsum(start_value),
        ymin = lag(cumsum(start_value), default = 0),
        xmin = c(head(quarter_start, -1), NA),
        xmax = c(tail(quarter_start, -1), NA))


ds_palette <- c("gold1", "goldenrod1", "darkgoldenrod1","goldenrod2", 
                "darkgoldenrod2", "darkorange", "darkorange1", "darkorange2",
                "orangered", "red", "red1", "red2")

ggplot(df1[-1,]) +
    geom_rect(aes(xmin = ymd(quarter_start) - days(12),
                  xmax = ymd(quarter_start) + days(12), ymin = ymin, ymax = ymax,
                  fill = factor(quarter_start))) +
    scale_fill_manual(values = ds_palette) +  
    annotate("text", x  = ymd(df1$xmin[4]), y = max(df1$ymax), label = "$tudent Debt", color = "white",
             size = 12) +
    annotate("text", x  = ymd(df1$xmin[2])+ days(50), y = 1, label = dollar(round(min(df1$ymax),-9)), color = "white",
             size = 4) +
    annotate("text", x  = ymd(df1$xmin[12]) + days(30), y = max(df1$ymax) + 50e+9, label = dollar(round(max(df1$ymax),-9)), 
             color = "white",size = 4) +
    theme(panel.background=element_rect(fill = "black"), axis.ticks.y = element_line(size = NA),
          legend.position = "none", panel.grid = element_blank(), axis.text.y=element_blank(),
          axis.ticks = element_blank(), axis.title = element_blank(), plot.background=element_rect(fill = "black"),
          axis.text.x=element_text(colour = "white")) 

