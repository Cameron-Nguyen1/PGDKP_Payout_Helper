#!/bin/Rscript
library(dplyr)
library(stringr)
library(optparse)
option_list = list(
  make_option(c("--Dsheet"), type="character", default=NULL,help="Dam Sheet", metavar="character"),
  make_option(c("--Hsheet"), type="character", default=NULL,help="Healing Sheet", metavar="character"),
  make_option(c("--Pot"), type="character", default=NULL,help="The Pot Size", metavar="character"),
)
p = parse_args(OptionParser(option_list=option_list))
total_pot=as.numeric(p[3])
mgmt_pay=list(Host=total_pot*.05, #Hostcut = 5%, MT = 2.5%, OT = 1.5%, XtraTank1 = .5%, Xtratank2 = .5%, HunterPuller = .5%, Arbitrary = .25%
                MT=total_pot*.025, # -> 13% MGMT Cut
                OT=total_pot*.015,
                Tank3=total_pot*.005,
                Tank4=total_pot*.005,
                Hunter=total_pot*.005, 
                Arbitrary=total_pot*.025)
net_pot = .87*total_pot
bonuspot = .5*net_pot # Half of the pot will be guaranteed pay, the other half is to be sorted out as bonuses.
Damage_Tanks = read.csv(as.character(p[1]))[,1:ncol(read.csv(as.character(p[1])))-1]
Heals = read.csv(as.character(p[2]))[,1:ncol(read.csv(as.character(p[2])))-1]

colnames(Damage_Tanks) = c("Parse","Name","Amount","iLvL","iLvL.pct","Active.pct","DPS")
colnames(Heals) = c("Parse","Name","Amount","Overheal","iLvL","iLvL.pct","Active.pct","HPS")
Heals["Total_Healing"] = as.numeric(str_extract(Heals$Amount,"[0-9]*"))
Damage_Tanks["Total_Damage"] = as.numeric(str_extract(Damage_Tanks$Amount,"[0-9]*"))

#Filter for grey parses + inactivity, nonparticipation = no pay!!!
    #Calculate bonus weights to apply to remainder of pay.
Damage_Tanks = Damage_Tanks %>% filter(DPS > 100 & as.numeric(gsub("%","",Active.pct)) > 20 & as.numeric(Parse) > 24) %>% 
    mutate("Role"="DPS/TANK") %>% 
    mutate("pct_cut" = Total_Damage/sum(Total_Damage,na.rm=TRUE))
Heals = Heals %>% filter(HPS > 50 & as.numeric(Parse) > 24) %>% 
    mutate("Role"="HEALS") %>% 
    mutate("pct_cut" = Total_Healing/sum(Total_Healing,na.rm=TRUE))

pct_dam = signif(nrow(Damage_Tanks)/(nrow(Damage_Tanks)+nrow(Heals)),3) #Determine how much of the bonus pot goes to DPS/Tanks.
pct_heal = signif(nrow(Heals)/(nrow(Damage_Tanks)+nrow(Heals)),3) #Determine how much of the bonus pot goes to heals.
dps_pot = (bonuspot*pct_dam)*.98
heal_pot = (bonuspot*pct_heal)*.95
bonus_withheld = .07*bonuspot #Withholding 7% of the bonus pot... for reasons... lol
distrib = bonus_withheld+mgmt_pay$Arbitrary
print(paste0("To distribute arbitrarily: ",distrib))
print(paste0("Host cut as stands: ",mgmt_pay$Host))

#Organize and structure information
Damage_Tanks["Bonus"] = dps_pot*Damage_Tanks$pct_cut
Damage_Tanks["Base.Pay"] = rep((net_pot/2)/(nrow(Damage_Tanks)+nrow(Heals)),nrow(Damage_Tanks))
Heals["Bonus"] = heal_pot*Heals$pct_cut
Heals["Base.Pay"] = rep((net_pot/2)/(nrow(Damage_Tanks)+nrow(Heals)),nrow(Heals))

#Build the final dataframe and write it to file.
eligible = bind_rows(Damage_Tanks,Heals) %>% mutate("Total.Payout"=floor(Bonus+Base.Pay))
pframe = eligible[,c("Name","Role","Base.Pay","Bonus","Total.Payout")]
write.csv(pframe,file="PayoutSheet.csv")

#Try running this command:
    #Rscript docuts.r --Dsheet=Dam.csv --Hsheet=Heal1.csv --Pot=21550