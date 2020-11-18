# Masters Golf Viz
library(tidyverse)

df <- read.csv("https://raw.githubusercontent.com/joseph-shaw/Sports-Data-Visualization/main/Masters%202020/Masters2020.csv", stringsAsFactors = F) %>% 
  rename("name" = 1)

par <- as.data.frame(t(df[1:2,])) %>% 
  slice(2:19) %>% 
  mutate(
    hole = 1:18
  ) %>% 
  select(3,1) %>% 
  rename(par = 2)

df.long <- df[2:nrow(df),] %>% 
  gather(key = "hole", value = "score", 2:73) %>% 
  arrange(name) %>% 
  mutate(
    round = substr(hole, 2, 2),
    hole = substr(hole, 4, nchar(hole)),
    hole = as.numeric(ifelse(hole == "1", 10, hole))
    ) %>% 
  left_join(par, by = "hole") %>% 
  select(1,4,2,5,3) %>% 
  mutate(
    par = as.numeric(par),
    vs.par = score - par
    ) %>% 
  group_by(name, round) %>% 
  mutate(
    round.score = sum(score)
  ) %>% 
  group_by(name) %>% 
  mutate(
    total.score = sum(score),
    hole.2 = row_number(),
    cum.score = cumsum(vs.par),
    highlight = case_when(
      name == "Dustin Johnson" ~ "DJ",
      name == "Sungjae Im" ~ "Im",
      name == "Cameron Smith" ~ "CS",
      name == "Justin Thomas" ~ "JT",
      name == "Rory McIlroy" ~ "RM",
      name == "Dylan Frittelli" ~ "DF", 
      1==1 ~ "Other"
    )
  ) %>% 
  arrange(total.score) 

df.long$name <- factor(df.long$name, levels = rev(unique(df.long$name)))

bg <- "#F7F7F2"
c1 <- "#d36d2e"
c2 <- "#CAA638"
c3 <- "#0F6852"
c4 <- "#286278"
c5 <- "#853A1D"
c6 <- "#82A5B8"

rounds <- c("1" = "Round 1", "2" = "Round 2", "3" = "Round 3", "4" = "Round 4")

# plot 1 ####
ggplot(df.long[df.long$total.score < 284,], aes(x = hole, y = name, size = vs.par))+
  geom_point(alpha = 0.8, pch = 21, aes(colour = round, fill = round))+
  scale_radius(range = c(1.5, 18), limits = NULL, labels = c("-2", "-1", "Par", "+1", "+2", "+3", "+4"))+
  scale_color_manual(values = c( c1, c2 , c3, c4))+
  scale_fill_manual(values = c( c1, c2 , c3, c4))+
  facet_wrap(~round, ncol = 4, labeller = as_labeller(rounds))+
  coord_cartesian(xlim = c(0.5, 18.5))+
  scale_x_continuous(breaks = seq(1, 18, 1))+
  theme_minimal()+
  theme(
    strip.text = element_text(size =  12, family = "Lora"),
    panel.grid = element_blank(),
    panel.spacing = unit(-0.7, "lines"),
    panel.background = element_blank(),
    panel.border = element_blank(),
    plot.background = element_rect(fill = bg, colour = "transparent"),
    legend.position = "bottom",
    legend.justification='left',
    axis.text.x = element_text(size =  8, family = "Lora"),
    axis.text.y = element_text(size =  12, family = "Lora"),
    axis.title = element_blank(),
    plot.margin=unit(c(1,1,1.5,1.2),"cm"),
    legend.key.width=unit(2,"cm"),
    legend.spacing.x = unit(0.1, 'cm'),
    legend.text = element_text(family = "Lora")
  )+
  guides(fill=FALSE,
         colour = FALSE,
         size=guide_legend(nrow=1, title = NULL, label.position = "bottom")
         )+
  ggsave(filename = 'golf.png', dpi = 600, type = 'cairo', width = 400, height = 300, units = 'mm', limitsize = F)


# Plot 2 ####
ggplot(df.long, aes(x = hole, y = cum.score, group = name))+
  geom_line(aes(col = highlight, alpha = highlight))+
  scale_alpha_manual(values = c(0.9,0.9,0.9,0.9,0.9,0.2, 0.9))+
  scale_colour_manual(values = c(c6, c1, c2 , c3, c4, "grey", c5))+
  facet_wrap(~round, nrow = 1, labeller = as_labeller(rounds))+
  scale_x_continuous(breaks = c(1, 9, 10, 18))+
  scale_y_continuous(breaks = seq(-20, 10, 5))+
  theme_minimal()+
  theme(
    legend.position = "none",
    panel.grid.major.y =  element_line(colour = "white", size = 0.2),
    panel.grid = element_blank(), 
    strip.text = element_text(family = "Lora"),
    panel.spacing = unit(-0.7, "lines"),
    panel.background = element_blank(),
    panel.border = element_blank(),
    plot.background = element_rect(fill = bg, colour = "transparent"),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size =  8, family = "Lora"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(family = "Lora", size = 10),
    plot.margin=unit(c(1,1,1.5,1.2),"cm"),
    legend.key.width=unit(2,"cm"),
    legend.spacing.x = unit(0.8, 'cm'),
    legend.text = element_text(family = "Lora")
    )+
  ylab("Cumulative Score")+
  ggsave(filename = 'golf2.png', dpi = 600, type = 'cairo', width = 400, height = 150, units = 'mm', limitsize = F)


  
  
  












