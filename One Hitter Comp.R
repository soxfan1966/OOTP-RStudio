playerCID <- readline(prompt="OOTP Player CID to Compare: ")
OneH <- filter(HitterCombined, CID==playerCID)
OneH <- rbind(OneH, filter(HitterCombinedMyTeam, CID==playerCID))
View(OneH)