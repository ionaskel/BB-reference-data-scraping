library(XML)
library(RCurl)
library(data.table)
library(dplyr)


team = c("ATL" , "BOS" , "BRK" , "CHO" , "CHI" , "CLE" , "DAL" , "DEN" ,
         "DET" , "GSW" , "HOU" , "IND" , "LAC" , "LAL" , "MEM" , "MIA" , "MIL" ,
         "MIN" , "NOP" , "NYK" , "OKC" , "ORL" , "PHI" , "PHO" , "POR" , "SAC" ,
         "SAS" , "TOR" , "UTA" , "WAS")


year = c(as.character(2015:2018))

final.data.table = data.frame()

for(yr in year){
        for(tm in team){
                
                url = getURL(paste("https://www.basketball-reference.com/teams/" , tm , "/" , yr , "/gamelog/" , sep = ''))
                
                data.table = readHTMLTable(url , which = 1)
                data.table = data.frame(data.table , stringsAsFactors = F)
                
                colnames(data.table) = sapply(colnames(data.table) , function(x) gsub('tgl_basic.' , '' , x)) # Remove'tgl_basic'
                data.table = data.table[ , -which(colnames(data.table) == 'Rk')] # Remove rank variable
                
                ## Fixing the variables names
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                # Team columns
                columns_changed_names = c('Game' , 'Date' , 'Home' , 'Opponent' , 'WINorLOSS' , 'Team' , 'FieldGoals' , 
                                          'FieldGoalsAttempted' , 'FieldGoals%' , '3PointShots' , '3PointShotsAttempted' , 
                                          '3PointShots%' , 'FreeThrows' , 'FreeThrowsAttempted' , 'FreeThrows%' , 
                                          'OffRebounds' , 'TotalRebounds' , 'Assists' , 'Steals' , 
                                          'Blocks' , 'Turnovers' , 'TotalFouls')
                
                colnames(data.table)[!grepl('.1' , colnames(data.table))] = columns_changed_names
                
                
                # Opponent columns
                columns_changed_names_2 = paste('Opp.' , columns_changed_names[-(1:6)] , sep = '')
                
                colnames(data.table)[grepl('.1' , colnames(data.table))][-(1:2)] = columns_changed_names_2
                rm(columns_changed_names , columns_changed_names_2)
                #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
                
                # Find rows without values and remove them
                idx = which(data.table[ , 1] == '' | data.table[ , 1] == 'G')
                data.table = data.table[-idx , ]
                rm(idx)
                
                # Row names
                row.names(data.table) = data.table$Game
                
                # Home variable
                data.table$Home = ifelse(data.table$Home == '@' , 'Away' , 'Home')
                
                # Opp.1
                colnames(data.table)[which(colnames(data.table) == 'Opp.1')] = 'OpponentPoints'
                colnames(data.table)[which(colnames(data.table) == 'Team')] = 'TeamPoints'
                
                data.table = data.frame(Team = tm , data.table)
                
                
                # Merge datasets
                final.data.table = rbind(final.data.table , data.table)
                
        }
}


final.data.table = final.data.table[ , -which(colnames(final.data.table) == 'Â..1')]

final.data.table$Date = as.Date(final.data.table$Date)

write.csv(final.data.table , "nba.games.stats.csv")



        