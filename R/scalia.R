
# Load packages -----------------------------------------------------------

library(ggplot2)
library(ggthemes)
library(readr)
library(dplyr)
library(extrafont)
library(extrafontdb)
library(ggrepel)

# Load data ---------------------------------------------------------------
list.files('./data', full.names = TRUE)

df <- read_csv("./data/justices.csv")
nominations <- read_csv("./data/nominations.csv")
court <- read_csv("./data/court.csv")

# Clean data --------------------------------------------------------------
justices <- c('Alito', 'Thomas', 'Scalia', 'Roberts', 'Kennedy', 'Breyer', 'Kagan', 'Sotomayor', 'Ginsburg')

df <- df %>%
  filter(justiceName %in% justices) %>%
  left_join(nominations, by = 'justiceName')

scalia <- df %>%
  filter(justiceName == 'Scalia')

labels <- df %>%
  filter(term == max(term))
labels <- labels[order(labels$post_mn), ] 
labels$adjustments <- c(-0.05, 0.1, -0.05, 0.1, 0, 0, 0, 0 , 0)
labels$post_mn = labels$post_mn + labels$adjustments

court <- court %>%
  filter(term >= 1967)

# Make chart --------------------------------------------------------------

p <- ggplot()

p <- p + geom_segment(aes(x = 1967, xend = 2014, y = -2, yend = -2 ), color = 'grey', alpha = .7)
p <- p + geom_segment(aes(x = 1967, xend = 2014, y = 0, yend = 0 ), color = 'grey', alpha = .7)
p <- p + geom_segment(aes(x = 1967, xend = 2014, y = 2, yend = 2 ), color = 'grey', alpha = .7)
p <- p + geom_segment(aes(x = 1967, xend = 2014, y = 4, yend = 4 ), color = 'grey', alpha = .7)

p <- p + geom_line(data = court, aes(x = term, y = med), color = '#9a9899', size = rel(1), alpha = .4)
p <- p + geom_line(data = df, aes(x = term, y = post_mn, group = justiceName, color = party), size = rel(1), , alpha = .3)
p <- p + geom_line(data = scalia,
                   aes(x = term, y = post_mn), size = rel(1.8), color = '#C00000')

p <- p + geom_text(data = labels[labels$justiceName != 'Scalia', ],
                   aes(x = term+0.5, y = post_mn, label = justiceName), hjust = 0, alpha = .7)
p <- p + geom_text(data = labels[labels$justiceName == 'Scalia', ],
                   aes(x = term+0.5, y = post_mn, label = justiceName), hjust = 0, fontface = 'bold')

p <- p + scale_color_manual(values = c('#002CC0', '#C00000'),
                            labels = c("Nominated by a Republican", "Nominated by a Democrat"),
                            name = '')
p <- p + scale_x_continuous(limits = c(1967, 2020), expand = c(0, 0))

p <- p + ggtitle('Justice ideology based on Martin-Quinn scores\n')
p <- p + annotate("text", x = 1971, y = 3.5, label = "MORE CONSERVATIVE", hjust = 0, vjust = 0)
p <- p + annotate("text", x = 1971, y = 3, label = "MORE LIBERAL", hjust = 0, vjust = 1)
p <- p + annotate("segment", x = 1970, xend = 1970, y = 3.4, yend = 3.9, arrow = arrow(length = unit(0.25, "cm"), type = 'closed'), size = rel(1))
p <- p + annotate("segment", x = 1970, xend = 1970, y = 3, yend = 2.5, arrow = arrow(length = unit(0.25, "cm"), type = 'closed'), size = rel(1))
p <- p + annotate("segment", x = 1975, xend = 1975, y = 0.8, yend = 1.2, color = 'grey', alpha = .6)
p <- p + annotate("text", x = 1975, y = 1.3, label = "Median justice", vjust = 0, size = rel(5), fontface = 'bold')

p <- p + theme_tufte()
p <- p + theme(
  text = element_text(family = 'Calibri'),
  plot.title = element_text(hjust = -0.04, face = 'bold'),
  legend.position = c(0, 0.95),
  legend.justification = c(0.1, 0),
  legend.direction = 'horizontal',
  axis.title = element_blank(),
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank()
  
)
p

ggsave(filename = './output/upshot_justices.png', units = 'cm', height = 10.5, width = 20)


