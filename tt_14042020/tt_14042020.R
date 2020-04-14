loadfonts(device = "win")
library(tidyverse)
library(extrafont)
library(ggtext)
font_import()

tuesdata <- tidytuesdayR::tt_load('2020-04-14')
tuesdata <- tidytuesdayR::tt_load(2020, week = 16)
polls <- tuesdata$polls
ranks <- tuesdata$rankings

theme_set(theme_minimal(base_family = "Roboto"))
theme_update(plot.background = element_rect(fill = "#EEEAE8",
                                            color = "#EEEAE8"),
             axis.text.x = element_text(vjust = 2),
             axis.text.y = element_text(margin=margin(0,-20,0,0),
                                        size = 6),
             plot.title.position = 'plot',
             plot.title = element_text(size = 18,
                                       color = "#262126",
                                       face = "bold"),
             plot.subtitle = element_markdown(size = 16,
                                          color = "#262126",
                                          face = "plain"),
             plot.caption = element_text(size = 10,
                                         color = "grey70",
                                         face = "bold"))
                        
p <- ranks %>%
  separate_rows(artist, sep = ' ft. ') %>%
  separate_rows(artist, sep = ' & ')  %>%
  group_by(artist) %>% 
  summarise_at(vars(n:n5), sum) %>%
  mutate(n1 = 10*n1, n2 = 8*n2, n3 = 6*n3, n4 = 4*n4, n5 = 2*n5,
         n = n1 + n2 + n3 + n4 + n5) %>%
  arrange(-n,-n1,-n2,-n3,-n4,-n5) %>% # rank including ties
  slice(1:30) %>%
  mutate(rank = 1:30) %>%
  pivot_longer(cols = 3:7, values_drop_na = TRUE, names_to = 'song_rank',
               values_to = 'points') %>%
  rename(total_points = n) %>%
  mutate(artist = fct_reorder(artist, -rank),
         first_rank = if_else(song_rank == 'n1', 'n1', 'n_other'),
         first_rank = as.factor(first_rank)) %>%
  ggplot(aes(x = artist, y = points, fill = first_rank)) +
  geom_bar(stat = 'identity') +
  #theme_minimal() +
  labs(x = NULL, y = NULL,
       title = 'Top 30 Rap Artists by BBC Score of Songs',
       subtitle = 'Proportion of Total Points from <b style="color:#843229">#1 Song Votes</b>',
       caption = 'Data: BBC Music \n Viz: @obrienj_') + 
  scale_x_discrete(expand = c(-1,0)) +
  theme(panel.grid.major.x = element_line(color = "grey25", 
                                          linetype = "dotted", 
                                          size = .4)) +
  scale_fill_manual(values = c('#9E7457', '#843229'), guide = F) +
  coord_flip()

ggsave('bbc_rb.png', height = 4.5, width = 8, units = 'in', dpi = 1000)
ggsave('bbc_rb.pdf', height = 4.5, width = 8, units = 'in',
       device = cairo_pdf)
            
       