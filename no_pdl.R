library(tidyverse)
library(quanteda)
library(plotly)


df <- read_csv("C:\\Users\\jacop\\Tesi\\individual_results.csv")


## 

p <- df %>% 
     group_by(year) %>% 
     summarise(avg_perc_of_populist_tokens = mean(perc_of_populist_toks, na.rm = TRUE)) %>% 
     ggplot(aes(x = year, y = avg_perc_of_populist_tokens)) +
     geom_line(colour = "sky blue", size = 1) +
     theme_classic() +
     scale_x_continuous(breaks = seq(from = 1950, to = 2020, by = 10)) +
     labs(title = "Average % of populist tokens by year") +
     theme(line = element_blank(),
           axis.title = element_blank(),
           plot.title = element_text(size = 30),
           axis.text = element_text(size = 12))

ggplotly(p)


no_pdl <- read_csv("C:\\Users\\jacop\\Tesi\\no_pdl.csv")


p2 <- no_pdl %>% 
      group_by(year) %>% 
      summarise(avg_perc_of_populist_tokens = mean(perc_of_populist_toks, na.rm = TRUE)) %>% 
      ggplot(aes(x = year, y = avg_perc_of_populist_tokens)) +
      geom_line(colour = "#00A550", size = 1) +
      theme_classic() +
      scale_x_continuous(breaks = seq(from = 1950, to = 2020, by = 10)) +
      labs(title = "Average % of populist tokens by year without mentions of 'popolo della liberta'") +
      theme(line = element_blank(),
            axis.title = element_blank(),
            plot.title = element_text(size = 30),
            axis.text = element_text(size = 12))

ggplotly(p2)
