load("buli.RData")
#-------------------------------------------------------------------------------
#Erster Teil:

#Auswaerts und Heim nebeneinander.
barplot(matrix(c(table(buli$HR), 0, table(buli$AR)), 2, byrow = TRUE), beside = TRUE,
        main = "Rote Karten nach Heim bzw. Auswärtsteam", ylab = "Absolute Anzahl",
        xlab = "Rote Karten")


#Haufiger wenn gewonnen oder verloren?
barplot(matrix(c(table(buli$AR[buli$FTR == "H"]) / length(buli$AR[buli$FTR == "H"]), 0, 
                 table(buli$AR[buli$FTR == "D"]) / length(buli$AR[buli$FTR == "D"]), 
                 table(buli$AR[buli$FTR == "A"]) / length(buli$AR[buli$FTR == "A"]), 0 ), 
               3, byrow = TRUE), 
        beside = TRUE, main = "Rote Karten des Auswärtsteams nach Ergebnissen", 
        ylab = "Relative Anzahl", xlab = "Anzahl Rote Karten")


barplot(matrix(c(table(buli$HR[buli$FTR == "A"]) / length(buli$HR[buli$FTR == "A"]), 
                 table(buli$HR[buli$FTR == "D"]) / length(buli$HR[buli$FTR == "D"]), 
                 table(buli$HR[buli$FTR == "H"]) / length(buli$HR[buli$FTR == "H"])), 
               3, byrow = TRUE), 
        beside = TRUE, main = "Rote Karten des Heimteams nach Ergebnissen", 
        ylab = "Relative Anzahl", xlab = "Anzahl Rote Karten")

#Gesamt
barplot(matrix(c(table(buli$HR[buli$FTR == "A"]) / length(buli$HR[buli$FTR == "A"]), 0,
         table(buli$HR[buli$FTR == "D"]) / length(buli$HR[buli$FTR == "D"]), 0,
         table(buli$HR[buli$FTR == "H"]) / length(buli$HR[buli$FTR == "H"]), 0),
       3, byrow = TRUE) + 
         matrix(c(table(buli$AR[buli$FTR == "H"]) / length(buli$AR[buli$FTR == "H"]), 0, 
                  table(buli$AR[buli$FTR == "D"]) / length(buli$AR[buli$FTR == "D"]), 
                  table(buli$AR[buli$FTR == "A"]) / length(buli$AR[buli$FTR == "A"]), 0 ), 
                3, byrow = TRUE),
       beside = TRUE, main = "Rote Karten nach Ergebnissen", 
       ylab = "Relative Anzahl", xlab = "Anzahl Rote Karten")



#Nach Saison gesplittet:
buliSa <- split(buli, as.factor(buli$Saison))
barplot(sapply(buliSa, function(x) { sum(x$AR + x$HR) }), main = "Gesamtzahl Roter Karten pro Saison",
        ylab = "Anzahl Roter Karten", xlab = "Saison")

#Nach Teams gesplittet
buliHT <- split(buli, as.factor(buli$HomeTeam))
buliAT <- split(buli, as.factor(buli$AwayTeam))
#buliTe <- c(buliHT, buliAT)
#as.matrix(lapply(buliTe, function(x) {
#  return(sum(x$HR[x$HomeTeam == names(which.max(table(x$HomeTeam)))]) + sum(x$AR[x$AwayTeam == names(which.max(x$AwayTeam))])/(2*length(x$HomeTeam)))
#}))
#lapply(buliTe, function(x) { sum(x$HR)/length(x) })

reds <- 1:34
redHome <- 1:34
redAway <- 1:34
for(i in 1:34) {
  reds[i] <- (sum(buliHT[[i]]$HR) + sum(buliAT[[i]]$AR)) / (2 * length(buliHT[[i]]$HR))
  redHome[i] <- sum(buliHT[[i]]$HR)
  redAway[i] <- sum(buliAT[[i]]$AR)
}
reds <- data.frame(Rote_Karten = reds, Rote_Heim = redHome, RoteAusw = redAway, Verein =  names(buliHT))
reds[order(reds[,1]),]
#Greuther, St Pauli, Duisburg und Aachen waren allesamt nur eine Saison in der Bundesliga 
#und sind dann direkt abgestiegen. Auch Stuttgart war jahrelang im Abstiegskampf.

#-------------------------------------------------------------------------------
#Zweiter Teil:

#Gehe von einer Poissont-Verteilung aus:
#Schätze lambda per Momenten-Schätzer:
l1 <- mean(buli$HR)
l2 <- mean(buli$AR)

#Vergleiche theoretische Verteilung mit vorliegenden Daten
barplot( `colnames<-`(matrix(c(table(buli$HR)/sum(table(buli$HR)), dpois(0:2, l1)), 2, byrow = TRUE), 0:2),
         beside = TRUE, main = "Vergleich der theoretischen Poisson-Verteilung mit den Heimspielen",
         ylab = "relative Häufigkeit bzw. Wahrscheinlichkeit", xlab = "Anzahl rote Karten")
barplot( `colnames<-`(matrix(c(table(buli$AR)/sum(table(buli$AR)), dpois(0:3, l2)), 2, byrow = TRUE), 0:3),
         beside = TRUE, main = "Vergleich der theoretischen Poisson-Verteilung mit den Auswärtsspielen",
         ylab = "relative Häufigkeit bzw. Wahrscheinlichkeit", xlab = "Anzahl rote Karten")

table(buli$HR) - dpois(0:2, l1) * sum(table(buli$HR))
table(buli$AR) - dpois(0:3, l2) * sum(table(buli$AR))
#Die maximale Differenz zu den tatsächlichen Werten liegt bei AR 1 mit 3.53 aus 334
#Beide Parameterschätzungen passen sehr gut zu ihren jeweiligen Daten

