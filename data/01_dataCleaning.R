# Data loading and cleanring
require("dplyr")

M.E.df <- read.csv(
  file = "data/MoneyExperimentResults.csv"
) %>% tbl_df




#' Seq and Seq_... mappings to "Periopds"
#'   - includes a "blank" period between sequences, 
#'     to allow for subject endowements. 
PeriodsTab <- distinct(M.E.df, sequence, seq_period) %>% 
  select(sequence, seq_period) %>%
  arrange(sequence, seq_period)
PeriodsTab$tmp <- c(0,(diff(PeriodsTab$sequence)))
PeriodsTab$period = 1:nrow(PeriodsTab)
PeriodsTab <- PeriodsTab %>%
  mutate(
    period = period + tmp
  ) %>% 
  group_by(sequence) %>%
  mutate(peirod.max = max(period)) %>%
  ungroup() %>%
  select(-tmp)

M.E.df <- M.E.df %>%
  left_join(
    PeriodsTab
  )

# Nice subject naming 
M.E.df <- M.E.df %>%
  left_join(
    data.frame(
      subject_id = 1:16,
      subject = factor(
        x = paste("subject",1:16), 
        levels = paste("subject",1:16))
    )
  )


# Function: Create table of all itemType (e.g. blue-ticket, good) holding spells ----------
itemLogger <- function(AllData = M.E.df, 
                       TicketHolders = BlueTicketHolders,  
                       itemName = "blue ticket"
){
  
  #' M.E.df: money experiemnt data frame
  #' TicketHolders: subset of M.E.df for this class of item, e.g. BlueTicketHolders
  #' TicketHolder_startstop: empty df to store item-holding spells
  #' 
  #' delta: 
  #' 
  #' Returns: data frame of item holdings, start adn stop times
  #' 

  
  TicketHolder_startstop <- data.frame(
  subject_id = rep(NA, nrow(TicketHolders)), 
  sequence = rep(NA, nrow(TicketHolders)),
  seq_period = rep(NA, nrow(TicketHolders)),
  start = rep(NA, nrow(TicketHolders)), 
  stop = rep(NA, nrow(TicketHolders)),
  AcquiredBy = rep(NA, nrow(TicketHolders)),
  HeldTil = rep(NA, nrow(TicketHolders))
  )
  
  cnt = 1
  IDs <- sort(unique(AllData$subject_id))
  for (id in IDs){
    subdf = dplyr::filter(TicketHolders, subject_id == id)
    for (i in 1:nrow(subdf)){
      
      # people who start off with the blue ticket
      # or trade for it
      if (is.na(subdf$delta[i]) || (subdf$delta[i] != 1)){
        TicketHolder_startstop$subject_id[cnt] = id
        TicketHolder_startstop$sequence[cnt] = subdf$sequence[i]
        TicketHolder_startstop$seq_period[cnt] = subdf$seq_period[i]
        TicketHolder_startstop$start[cnt] = subdf$period[i] - 1
        
        if ((!is.na(subdf$delta[i]) || is.na(subdf$delta[i])) && subdf$seq_period[i] == 1){
          TicketHolder_startstop$AcquiredBy[cnt] = paste("Endowed with,",itemName)
        } else {
          TicketHolder_startstop$AcquiredBy[cnt] = paste("Traded for", itemName)
        }
      }
      
      if (is.na(subdf$delta[i]) && is.na(subdf$delta[i+1])){
        # pepole that hold the blue ticket from start to end until the end of the sequence
        TicketHolder_startstop$stop[cnt] = subdf$peirod.max[i] + 1
        TicketHolder_startstop$HeldTil[cnt] = "End of sequence"
        cnt <- cnt + 1
        
      } else if ((subdf$delta[i] == 1 || is.na(subdf$delta[i])) && (subdf$delta[i + 1] != 1 | is.na(subdf$delta[i + 1]))){
        # pepole that manage to trade away the blue ticket in a sequence
        TicketHolder_startstop$stop[cnt] = subdf$period[i]
        TicketHolder_startstop$HeldTil[cnt] = "Traded away"
        if (subdf$period[i] == subdf$peirod.max[i]){
          TicketHolder_startstop$HeldTil[cnt] = "End of sequence"
        }
        
        cnt <- cnt + 1
      }
      
      
    }
    
  }
  
  TicketHolder_startstop <- TicketHolder_startstop %>%
    dplyr::filter(!is.na(subject_id)) %>%
    tbl_df %>%
    left_join(  # adding subject factor (subject 1, ...)
      data.frame(
        subject_id = 1:16,
        subject = factor(
          x = paste("subject",1:16), 
          levels = paste("subject",1:16))
      ),
      by = "subject_id"
    ) %>% 
    mutate(
      ItemType = itemName
    )
  
  
  return(TicketHolder_startstop)
  
}

# test <- itemLogger()








