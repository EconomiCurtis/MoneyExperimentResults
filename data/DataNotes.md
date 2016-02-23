

From Justin: 

OK, attached is the data set for my first session.  Some descriptions of the variables:

`sequence`:  the sequence number (starts with 3 as the first 2 are practice).

`seq_period`:  period number within the sequence.  Note that there is no variable that captures the overall period number.

`subject_id`:  unique subject ID indicator

`player_type`:  1 = red ticket holder, 2 = blue ticket holder, 3 = good holder (Seller)

`partner_type`:  same as player_type identifiers

`trade_possible`:  1 = matched players may trade,  0 = no trade possible.

`player_makes_offer`:  1 = the player agrees to accept partner's trade offer, 0 = player does not accept

So what I'm interested in is `player_makes_offer` where:
1) `player_type` = 3 (good holder)
2) `partner_type` = 2 (blue ticket holder)
3) `trade_possible` = 1 (matched player - partner may trade)

Hopefully this makes more sense!