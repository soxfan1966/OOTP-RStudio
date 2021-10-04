# stuff for player comparison
#rm(OneH)
#rm(OneP)
playerCID <- readline(prompt="OOTP Pitcher CID to Compare: ")
OneP <- filter(PitcherCombined, CID==playerCID)
OneP <- rbind(OneH, filter(PitcherCombinedMyTeam, CID==playerCID))
View(OneP)
