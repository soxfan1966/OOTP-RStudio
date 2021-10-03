# Load Library files we need
library(dplyr)
library(readxl)
library(writexl)
# load Multipliers
wOBAMult <- read_excel("wOBAMultipliers.xls")
#View(wOBAMult)
# clean up prior objects first
rm(HitterCombined)
rm(HitterCombinedTm)
rm(HitterCombined200PA)
rm(HitterCombinedVsL)
rm(HitterCombinedVsR)
rm(HitterCombinedMyTeam)
#
rm(PitcherCombined)
rm(PitcherCombinedTm)
rm(PitcherCombinedVsL)
rm(PitcherCombinedVsR)
rm(PitcherCombinedMyTm)
# Ask user for input needed to get file
my.team <- readline(prompt="Your OOTP Team Mnemonic from TM2 field: ")
stats.type <- "league"
readline(prompt="You will now identify the excel file, should be in DRC format.  Hit enter to continue.")
filename <- file.choose(new="FALSE")
# now try and load in the files
if(stats.type=="league") {
   Hitters <- read_excel(filename,1)
   HittersVsR <- read_excel(filename, 2)
   HittersVsL <- read_excel(filename, 3)
   Pitchers <- read_excel(filename, 4)
   PitchersVsR <- read_excel(filename, 5)
   PitchersVsL <- read_excel(filename, 6)
  } else {
  Hitters <- read_excel(filename,1)
  Pitchers <- read_excel(filename, 2)
}
#
#write_xlsx("C:\\Users\\user\\Desktop\\OOTP 22 Weekly League Stats\\R Data Projects\\OOTP-Combined.xls")
#
HitterCombined <- Hitters %>% group_by(Title, CID) %>% summarise(PA = sum(PA),AB = sum(AB),WAR = round(sum(WAR), digits=4),BatR = sum(BatR),ZR = sum(ZR),BsR = sum(BsR),UbR = round(sum(UBR), digits = 4),H = sum(H), X2B = sum(`2B`), X3B = sum(`3B`), HR = sum(HR), BB = sum(BB), K = sum(SO), HBP = sum(HP), SF = sum(SF), IBB = sum(IBB),IP = sum(IP),SB = sum(SB), CS = sum(CS), MwRC = round(mean(wRC), digits=2), MwRCplus = mean(`wRC+`), MOPSplus = mean(`OPS+`), MBatR = mean(BatR), MwRAA = mean(wRAA)) %>% mutate(X1B = H - X2B - X3B - HR,BABIP = round((H - HR)/(AB - HR - K + SF), digits = 4), XBHR = round((X2B + X3B)/H, digits = 4), X3BR = round(X3B/(X2B+X3B), digits = 4), KR = round(K/(PA-HBP-BB-HR), digits = 4), BBR = round(BB/(PA-HBP), digits = 4), HRR = round(HR/(PA-HBP-BB), digits = 4), WAR600 = round(WAR/PA*600, digits = 4), BATR600 = round(BatR/PA*600, digits = 4), BSR600 = round(BsR/PA*600, digits = 4), UBR600 = round(UbR+0/PA*600, digits = 4), AVG = round(H/AB, digits = 3), TB = (H-X2B-X3B-HR)+(X2B*2)+(X3B*3)+(HR*4), SLG = round(TB/AB, digits = 4), OPB = round((H+BB+HBP)/(AB+BB+HBP+SF), digits = 4), wOBP = round((((wOBAMult$BBMult * (BB-IBB)) + (wOBAMult$HBPMult * HBP) + (wOBAMult$SGMult * X1B) + (wOBAMult$DBMult * X2B) + (wOBAMult$TRMult * X3B) + (wOBAMult$HRMult * HR)) / (AB+BB+IBB+SF+HBP)), digits = 4), wRC = round((((wOBP-wOBAMult$LGOBA)/wOBAMult$LGwOBAScale)+wOBAMult$LGRpPA)*PA, digits = 2))
HitterCombinedTm <- Hitters %>% group_by(TM) %>% summarise(PA = sum(PA),AB = sum(AB),WAR = round(sum(WAR), digits=4),BatR = sum(BatR),ZR = sum(ZR),BsR = sum(BsR),UbR = round(sum(UBR), digits = 4),H = sum(H), X2B = sum(`2B`), X3B = sum(`3B`), HR = sum(HR), BB = sum(BB), K = sum(SO), HBP = sum(HP), SF = sum(SF), IBB = sum(IBB),IP = sum(IP),SB = sum(SB), CS = sum(CS), MwRC = round(mean(wRC), digits=2), MwRCplus = mean(`wRC+`), MOPSplus = mean(`OPS+`), MBatR = mean(BatR), MwRAA = mean(wRAA)) %>% mutate(X1B = H - X2B - X3B - HR,BABIP = round((H - HR)/(AB - HR - K + SF), digits = 4), XBHR = round((X2B + X3B)/H, digits = 4), X3BR = round(X3B/(X2B+X3B), digits = 4), KR = round(K/(PA-HBP-BB-HR), digits = 4), BBR = round(BB/(PA-HBP), digits = 4), HRR = round(HR/(PA-HBP-BB), digits = 4), WAR600 = round(WAR/PA*600, digits = 4), BATR600 = round(BatR/PA*600, digits = 4), BSR600 = round(BsR/PA*600, digits = 4), UBR600 = round(UbR+0/PA*600, digits = 4), AVG = round(H/AB, digits = 3), TB = (H-X2B-X3B-HR)+(X2B*2)+(X3B*3)+(HR*4), SLG = round(TB/AB, digits = 4), OPB = round((H+BB+HBP)/(AB+BB+HBP+SF), digits = 4), wOBP = round((((wOBAMult$BBMult * (BB-IBB)) + (wOBAMult$HBPMult * HBP) + (wOBAMult$SGMult * X1B) + (wOBAMult$DBMult * X2B) + (wOBAMult$TRMult * X3B) + (wOBAMult$HRMult * HR)) / (AB+BB+IBB+SF+HBP)), digits = 4), wRC = round((((wOBP-wOBAMult$LGOBA)/wOBAMult$LGwOBAScale)+wOBAMult$LGRpPA)*PA, digits = 2))
HitterCombined200PA <- filter(HitterCombined, PA>199)
#
PitcherCombined <- Pitchers %>% group_by(Title, CID) %>% summarise(G = sum(G),GS = sum(GS),QS=sum(QS),W = sum(W), L=sum(L),SVO=sum(SVO),SV=sum(SV),IP=sum(IP),BF=sum(BF),AB=sum(AB),HA=sum(HA),X1B=sum(`1B`),X2B=sum(`2B`), X3B=sum(`3B`),HR=sum(HR),TB=sum(TB),R=sum(R),ER=sum(ER),BB=sum(BB),IBB=sum(IBB),K=sum(K),HP=sum(HP),DP=sum(DP),SF=sum(SF),SH=sum(SH), GB=sum(GB),FB=sum(FB)) %>% mutate( BABIP = round((HA - HR)/(AB - HR - K + SF), digits = 4), XBHR = round((X2B + X3B)/HA, digits = 4), KR = round(K/(BF-HP-BB-HR), digits = 4), BBR = round(BB/(BF-HP), digits = 4), HRR = round(HR/(BF-HP-BB), digits = 4), ERA = round(ER*9/IP, digits = 3), WHIP = round((HA+BB)/IP, digits = 4),BAA = round((HA/(BF-BB-HP-SH-SF)), digits = 4), SLGA = round((TB/(BF-BB-HP-SH-SF)), digits = 4), FIP = round(((((13*HR)+(3*(BB+HP))-(2*K))/IP)+wOBAMult$LGcFIP), digits = 3))
PitcherCombinedTm <- Pitchers %>% group_by(TM) %>% summarise(G = sum(G),GS = sum(GS),QS=sum(QS),W = sum(W), L=sum(L),SVO=sum(SVO),SV=sum(SV),IP=sum(IP),BF=sum(BF),AB=sum(AB),HA=sum(HA),X1B=sum(`1B`),X2B=sum(`2B`), X3B=sum(`3B`),HR=sum(HR),TB=sum(TB),R=sum(R),ER=sum(ER),BB=sum(BB),IBB=sum(IBB),K=sum(K),HP=sum(HP),DP=sum(DP),SF=sum(SF),SH=sum(SH), GB=sum(GB),FB=sum(FB)) %>% mutate( BABIP = round((HA - HR)/(AB - HR - K + SF), digits = 4), XBHR = round((X2B + X3B)/HA, digits = 4), KR = round(K/(BF-HP-BB-HR), digits = 4), BBR = round(BB/(BF-HP), digits = 4), HRR = round(HR/(BF-HP-BB), digits = 4), ERA = round(ER*9/IP, digits = 3), WHIP = round((HA+BB)/IP, digits = 4),BAA = round((HA/(BF-BB-HP-SH-SF)), digits = 4), SLGA = round((TB/(BF-BB-HP-SH-SF)), digits = 4), FIP = round(((((13*HR)+(3*(BB+HP))-(2*K))/IP)+wOBAMult$LGcFIP), digits = 3))
#
if(stats.type=="league") {
  HitterCombinedVsL <- HittersVsL %>% group_by(Title, CID) %>% summarise(PA = sum(PA),AB = sum(AB),WAR = round(sum(WAR), digits=4),BatR = sum(BatR),ZR = sum(ZR),BsR = sum(BsR),UbR = round(sum(UBR), digits = 4),H = sum(H), X2B = sum(`2B`), X3B = sum(`3B`), HR = sum(HR), BB = sum(BB), K = sum(SO), HBP = sum(HP), SF = sum(SF), IBB = sum(IBB),IP = sum(IP),SB = sum(SB), CS = sum(CS), MwRC = round(mean(wRC), digits=2), MwRCplus = mean(`wRC+`), MOPSplus = mean(`OPS+`), MBatR = mean(BatR), MwRAA = mean(wRAA)) %>% mutate(X1B = H - X2B - X3B - HR,BABIP = round((H - HR)/(AB - HR - K + SF), digits = 4), XBHR = round((X2B + X3B)/H, digits = 4), X3BR = round(X3B/(X2B+X3B), digits = 4), KR = round(K/(PA-HBP-BB-HR), digits = 4), BBR = round(BB/(PA-HBP), digits = 4), HRR = round(HR/(PA-HBP-BB), digits = 4), WAR600 = round(WAR/PA*600, digits = 4), BATR600 = round(BatR/PA*600, digits = 4), BSR600 = round(BsR/PA*600, digits = 4), UBR600 = round(UbR+0/PA*600, digits = 4), AVG = round(H/AB, digits = 3), TB = (H-X2B-X3B-HR)+(X2B*2)+(X3B*3)+(HR*4), SLG = round(TB/AB, digits = 4), OPB = round((H+BB+HBP)/(AB+BB+HBP+SF), digits = 4), wOBP = round((((wOBAMult$BBMult * (BB-IBB)) + (wOBAMult$HBPMult * HBP) + (wOBAMult$SGMult * X1B) + (wOBAMult$DBMult * X2B) + (wOBAMult$TRMult * X3B) + (wOBAMult$HRMult * HR)) / (AB+BB+IBB+SF+HBP)), digits = 4), wRC = round((((wOBP-wOBAMult$LGOBA)/wOBAMult$LGwOBAScale)+wOBAMult$LGRpPA)*PA, digits = 2))
  HitterCombinedVsR <- HittersVsR %>% group_by(Title, CID) %>% summarise(PA = sum(PA),AB = sum(AB),WAR = round(sum(WAR), digits=4),BatR = sum(BatR),ZR = sum(ZR),BsR = sum(BsR),UbR = round(sum(UBR), digits = 4),H = sum(H), X2B = sum(`2B`), X3B = sum(`3B`), HR = sum(HR), BB = sum(BB), K = sum(SO), HBP = sum(HP), SF = sum(SF), IBB = sum(IBB),IP = sum(IP),SB = sum(SB), CS = sum(CS), MwRC = round(mean(wRC), digits=2), MwRCplus = mean(`wRC+`), MOPSplus = mean(`OPS+`), MBatR = mean(BatR), MwRAA = mean(wRAA)) %>% mutate(X1B = H - X2B - X3B - HR,BABIP = round((H - HR)/(AB - HR - K + SF), digits = 4), XBHR = round((X2B + X3B)/H, digits = 4), X3BR = round(X3B/(X2B+X3B), digits = 4), KR = round(K/(PA-HBP-BB-HR), digits = 4), BBR = round(BB/(PA-HBP), digits = 4), HRR = round(HR/(PA-HBP-BB), digits = 4), WAR600 = round(WAR/PA*600, digits = 4), BATR600 = round(BatR/PA*600, digits = 4), BSR600 = round(BsR/PA*600, digits = 4), UBR600 = round(UbR+0/PA*600, digits = 4), AVG = round(H/AB, digits = 3), TB = (H-X2B-X3B-HR)+(X2B*2)+(X3B*3)+(HR*4), SLG = round(TB/AB, digits = 4), OPB = round((H+BB+HBP)/(AB+BB+HBP+SF), digits = 4), wOBP = round((((wOBAMult$BBMult * (BB-IBB)) + (wOBAMult$HBPMult * HBP) + (wOBAMult$SGMult * X1B) + (wOBAMult$DBMult * X2B) + (wOBAMult$TRMult * X3B) + (wOBAMult$HRMult * HR)) / (AB+BB+IBB+SF+HBP)), digits = 4), wRC = round((((wOBP-wOBAMult$LGOBA)/wOBAMult$LGwOBAScale)+wOBAMult$LGRpPA)*PA, digits = 2))
  PitcherCombinedVsL <- PitchersVsL %>% group_by(Title, CID) %>% summarise(G = sum(G),GS = sum(GS),QS=sum(QS),W = sum(W), L=sum(L),SVO=sum(SVO),SV=sum(SV),IP=sum(IP),BF=sum(BF),AB=sum(AB),HA=sum(HA),X1B=sum(`1B`),X2B=sum(`2B`), X3B=sum(`3B`),HR=sum(HR),TB=sum(TB),R=sum(R),ER=sum(ER),BB=sum(BB),IBB=sum(IBB),K=sum(K),HP=sum(HP),DP=sum(DP),SF=sum(SF),SH=sum(SH), GB=sum(GB),FB=sum(FB)) %>% mutate( BABIP = round((HA - HR)/(AB - HR - K + SF), digits = 4), XBHR = round((X2B + X3B)/HA, digits = 4), KR = round(K/(BF-HP-BB-HR), digits = 4), BBR = round(BB/(BF-HP), digits = 4), HRR = round(HR/(BF-HP-BB), digits = 4), ERA = round(ER*9/IP, digits = 3), WHIP = round((HA+BB)/IP, digits = 4),BAA = round((HA/(BF-BB-HP-SH-SF)), digits = 4), SLGA = round((TB/(BF-BB-HP-SH-SF)), digits = 4), FIP = round(((((13*HR)+(3*(BB+HP))-(2*K))/IP)+wOBAMult$LGcFIP), digits = 3))
  PitcherCombinedVsR <- PitchersVsR %>% group_by(Title, CID) %>% summarise(G = sum(G),GS = sum(GS),QS=sum(QS),W = sum(W), L=sum(L),SVO=sum(SVO),SV=sum(SV),IP=sum(IP),BF=sum(BF),AB=sum(AB),HA=sum(HA),X1B=sum(`1B`),X2B=sum(`2B`), X3B=sum(`3B`),HR=sum(HR),TB=sum(TB),R=sum(R),ER=sum(ER),BB=sum(BB),IBB=sum(IBB),K=sum(K),HP=sum(HP),DP=sum(DP),SF=sum(SF),SH=sum(SH), GB=sum(GB),FB=sum(FB)) %>% mutate( BABIP = round((HA - HR)/(AB - HR - K + SF), digits = 4), XBHR = round((X2B + X3B)/HA, digits = 4), KR = round(K/(BF-HP-BB-HR), digits = 4), BBR = round(BB/(BF-HP), digits = 4), HRR = round(HR/(BF-HP-BB), digits = 4), ERA = round(ER*9/IP, digits = 3), WHIP = round((HA+BB)/IP, digits = 4),BAA = round((HA/(BF-BB-HP-SH-SF)), digits = 4), SLGA = round((TB/(BF-BB-HP-SH-SF)), digits = 4), FIP = round(((((13*HR)+(3*(BB+HP))-(2*K))/IP)+wOBAMult$LGcFIP), digits = 3))
}
#
HittersMyTeam <- filter(Hitters, TM2 == my.team)
PitchersMyTeam <- filter(Pitchers, TM2 == my.team)
#
HitterCombinedMyTeam <- HittersMyTeam %>% group_by(Title, CID) %>% summarise(PA = sum(PA),AB = sum(AB),WAR = round(sum(WAR), digits=4),BatR = sum(BatR),ZR = sum(ZR),BsR = sum(BsR),UbR = round(sum(UBR), digits = 4),H = sum(H), X2B = sum(`2B`), X3B = sum(`3B`), HR = sum(HR), BB = sum(BB), K = sum(SO), HBP = sum(HP), SF = sum(SF), IBB = sum(IBB),IP = sum(IP),SB = sum(SB), CS = sum(CS), MwRC = round(mean(wRC), digits=2), MwRCplus = mean(`wRC+`), MOPSplus = mean(`OPS+`), MBatR = mean(BatR), MwRAA = mean(wRAA)) %>% mutate(X1B = H - X2B - X3B - HR,BABIP = round((H - HR)/(AB - HR - K + SF), digits = 4), XBHR = round((X2B + X3B)/H, digits = 4), X3BR = round(X3B/(X2B+X3B), digits = 4), KR = round(K/(PA-HBP-BB-HR), digits = 4), BBR = round(BB/(PA-HBP), digits = 4), HRR = round(HR/(PA-HBP-BB), digits = 4), WAR600 = round(WAR/PA*600, digits = 4), BATR600 = round(BatR/PA*600, digits = 4), BSR600 = round(BsR/PA*600, digits = 4), UBR600 = round(UbR+0/PA*600, digits = 4), AVG = round(H/AB, digits = 3), TB = (H-X2B-X3B-HR)+(X2B*2)+(X3B*3)+(HR*4), SLG = round(TB/AB, digits = 4), OPB = round((H+BB+HBP)/(AB+BB+HBP+SF), digits = 4), wOBP = round((((wOBAMult$BBMult * (BB-IBB)) + (wOBAMult$HBPMult * HBP) + (wOBAMult$SGMult * X1B) + (wOBAMult$DBMult * X2B) + (wOBAMult$TRMult * X3B) + (wOBAMult$HRMult * HR)) / (AB+BB+IBB+SF+HBP)), digits = 4), wRC = round((((wOBP-wOBAMult$LGOBA)/wOBAMult$LGwOBAScale)+wOBAMult$LGRpPA)*PA, digits = 2))
PitcherCombinedMyTeam <- PitchersMyTeam %>% group_by(Title, CID) %>% summarise(G = sum(G),GS = sum(GS),QS=sum(QS),W = sum(W), L=sum(L),SVO=sum(SVO),SV=sum(SV),IP=sum(IP),BF=sum(BF),AB=sum(AB),HA=sum(HA),X1B=sum(`1B`),X2B=sum(`2B`), X3B=sum(`3B`),HR=sum(HR),TB=sum(TB),R=sum(R),ER=sum(ER),BB=sum(BB),IBB=sum(IBB),K=sum(K),HP=sum(HP),DP=sum(DP),SF=sum(SF),SH=sum(SH), GB=sum(GB),FB=sum(FB)) %>% mutate( BABIP = round((HA - HR)/(AB - HR - K + SF), digits = 4), XBHR = round((X2B + X3B)/HA, digits = 4), KR = round(K/(BF-HP-BB-HR), digits = 4), BBR = round(BB/(BF-HP), digits = 4), HRR = round(HR/(BF-HP-BB), digits = 4), ERA = round(ER*9/IP, digits = 3), WHIP = round((HA+BB)/IP, digits = 4),BAA = round((HA/(BF-BB-HP-SH-SF)), digits = 4), SLGA = round((TB/(BF-BB-HP-SH-SF)), digits = 4), FIP = round(((((13*HR)+(3*(BB+HP))-(2*K))/IP)+wOBAMult$LGcFIP), digits = 3))
#
#remove objects we dont need now
rm(Hitters)
rm(HittersVsR)
rm(HittersVsL)
rm(HittersMyTeam)
rm(Pitchers)
rm(PitchersVsR)
rm(PitchersVsL)
rm(PitchersMyTeam)
#
# stuff for player comparison
#rm(OneH)
#rm(OneP)
#playerCID <- readline(prompt="OOTP Player CID to Compare: ")
#OneH <- filter(HitterCombined, CID==playerCID)
#OneH <- rbind(OneH, filter(HitterCombinedMyTeam, CID==playerCID))
#View(OneH)
