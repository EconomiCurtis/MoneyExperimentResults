#' Plots
#' Blue ticket trading. 
#' 
#' 


library("ggplot2")
library("dplyr")
source("data/01_dataCleaning.R") #requires data from here...

#' Goal:
#' show all the times a sub


# Plot prep
M.E.df.plotPrep <- M.E.df %>%
  mutate(
    TradeType = NA,
    TradeType = ifelse(player_type == 3 & partner_type == 2 & (player_makes_offer + partner_makes_offer == 2),
                       yes = "Trade: My Good for Partner's Blue Ticket",
                       no = TradeType),
    TradeType = ifelse(player_type == 2 & partner_type == 3 & (player_makes_offer + partner_makes_offer == 2),
                       yes = "Trade: My Blue Ticket for Partner's Good",
                       no = TradeType),
    TradeType = ifelse(player_type == 3 & partner_type == 2 & (player_makes_offer == 1 & partner_makes_offer != 1),
                       yes = "Rejected by Partner: My Good for Partner's Blue Ticket",
                       no = TradeType),
    TradeType = ifelse(player_type == 2 & partner_type == 3 & (player_makes_offer == 1 & partner_makes_offer != 1),
                       yes = "Rejected by Partner: My Blue Ticket for Partner's Good",
                       no = TradeType)
  ) 


ggplot(
  data = M.E.df.plotPrep, 
  aes(
    x = period,
    y = subject)
  ) + 
  geom_vline(
    xintercept = (PeriodsTab %>% dplyr::filter(seq_period == 1))$period - 1,
    color = "black", size = 1) +
  geom_segment(
    data = BlueTicketHolder_startstop,
    aes(
      y = subject,
      yend = subject,
      x = start, 
      xend = stop
    ),
    color = "blue",
    size = 1.5, alpha = 0.5
  ) +
  geom_point(
    data = (M.E.df.plotPrep %>% dplyr::filter(!is.na(TradeType))),
    aes(colour = TradeType),
    size = 3, alpha = 0.9) +
  theme_minimal() +
  theme(
    legend.position	= "top",
    legend.direction = "vertical"
  ) + 
  ggtitle(" ") +
  guides(col = guide_legend(ncol = 2)) +
  theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())

# Things to check out: 

#' For selecting points colors manually, check out http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
#' I'm guessing you'll want maybe:
#' - BLUE for trade, my good for a Blue ticket, 
#' - Red reject my blue for a good
#' - Green, trade my blue ticket for a good
#' - Grey, the other trader rejected by Blue ticket for their good. 

# adjust postiioning of legend
# http://www.cookbook-r.com/Graphs/Legends_(ggplot2)/
# check out ncol... http://docs.ggplot2.org/0.9.3.1/guide_legend.html
