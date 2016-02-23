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



# all subject+periods of blue ticket holders
BlueTicketHolders <- M.E.df %>%
  dplyr::filter(
    player_type == 2
  ) %>% 
  arrange(subject_id, sequence, seq_period) %>%
  group_by(subject_id, sequence) %>%
  mutate(delta = seq_period - lag(seq_period))

# Create table - all blue ticket holding spells -------------

# Bin, to track people that hold the blue ticket
BlueTicketHolder_startstop <- data.frame(
  subject_id = rep(NA, nrow(BlueTicketHolders)), 
  sequence = rep(NA, nrow(BlueTicketHolders)),
  seq_period = rep(NA, nrow(BlueTicketHolders)),
  start = rep(NA, nrow(BlueTicketHolders)), 
  stop = rep(NA, nrow(BlueTicketHolders)),
  AcquiredBy = rep(NA, nrow(BlueTicketHolders)),
  HeldTil = rep(NA, nrow(BlueTicketHolders))
)
cnt = 1

for (id in sort(unique(M.E.df$subject_id))){
  subdf = dplyr::filter(BlueTicketHolders, subject_id == id)
  for (i in 1:nrow(subdf)){
    
    # people who start off with the blue ticket
    # or trade for it
    if (is.na(subdf$delta[i]) || (subdf$delta[i] != 1)){
      BlueTicketHolder_startstop$subject_id[cnt] = id
      BlueTicketHolder_startstop$sequence[cnt] = subdf$sequence[i]
      BlueTicketHolder_startstop$seq_period[cnt] = subdf$seq_period[i]
      BlueTicketHolder_startstop$start[cnt] = subdf$period[i] - 1
      
      if ((!is.na(subdf$delta[i]) || is.na(subdf$delta[i])) && subdf$seq_period[i] == 1){
        BlueTicketHolder_startstop$AcquiredBy[cnt] = "Endowed with blue ticket"
      } else {
        BlueTicketHolder_startstop$AcquiredBy[cnt] = "Traded for blue ticket"
      }
    }
    
    if (is.na(subdf$delta[i]) && is.na(subdf$delta[i+1])){
      # pepole that hold the blue ticket from start to end until the end of the sequence
      BlueTicketHolder_startstop$stop[cnt] = subdf$peirod.max[i] + 1
      BlueTicketHolder_startstop$HeldTil[cnt] = "End of sequence"
      cnt <- cnt + 1
      
    } else if (subdf$delta[i] == 1 && (subdf$delta[i + 1] != 1 | is.na(subdf$delta[i + 1]))){
      # pepole that manage to trade away the blue ticket in a sequence
      BlueTicketHolder_startstop$stop[cnt] = subdf$period[i]
      BlueTicketHolder_startstop$HeldTil[cnt] = "Traded away"
      if (subdf$period[i] == subdf$peirod.max[i]){
        BlueTicketHolder_startstop$HeldTil[cnt] = "End of sequence"
      }
      
      cnt <- cnt + 1
    }

    
  }
  
}

BlueTicketHolder_startstop <- BlueTicketHolder_startstop %>%
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
    )
  

rm(subdf)



