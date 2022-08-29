library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(wesanderson)
library(gt)


circuits <- read.csv(file = 'archive/circuits.csv')
constructorResults <- read.csv(file = 'archive/constructor_results.csv')
constructorStandings <- read.csv(file = 'archive/constructor_standings.csv')
constructors <- read.csv(file = 'archive/constructors.csv')
driverStandings <- read.csv(file = 'archive/driver_standings.csv')
drivers <- read.csv(file = 'archive/drivers.csv')
lapTimes <- read.csv(file = 'archive/lap_times.csv')
pitStops <- read.csv(file = 'archive/pit_stops.csv')
qualifying <- read.csv(file = 'archive/qualifying.csv')
races <- read.csv(file = 'archive/races.csv')
results <- read.csv(file = 'archive/results.csv')
seasons <- read.csv(file = 'archive/seasons.csv')
sprintResults <- read.csv(file = 'archive/sprint_results.csv')
status <- read.csv(file = 'archive/status.csv')

incidenti = full_join(full_join(full_join(full_join(results %>% select(raceId, constructorId, statusId, driverId), races %>% select(raceId, year, circuitId)), status), constructors %>% select(constructorId, name)), drivers %>% select(driverId, surname, forename)) %>% filter( statusId %in% c(3, 104, 4, 29, 33, 42, 44, 47, 56, 59, 66, 73, 82, 95, 137))#c(1, 11, 12, 13, 14, 15, 16, 17, 18, 19, 45, 50, 128, 53, 55, 58, 88, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 122, 123, 124, 125, 127, 133, 134, 140, 139, 138, 136, 132, 126, 121, 110, 109, 107, 108, 106, 105, 103, 102, 100, 98, 97, 96, 95, 93, 92, 90, 89, 88, 87, 86, 85, 83, 81, 77, 79, 72, 71, 70, 68, 65, 62, 61, 54, 49, 47, 46, 42, 31, 20))

colnames(incidenti)[8] <- "constructorName"
incidenti = full_join(incidenti, circuits %>% select(circuitId, name))
colnames(incidenti)[11] <- "circuitName"

incidenti <- incidenti %>% unite('driver', forename:surname, sep = " ", remove = TRUE)

tempi = full_join(full_join(races %>% select(raceId, circuitId, year), full_join(constructors %>% select(constructorId, name), results %>% select(raceId, driverId, constructorId, fastestLapTime, rank))), drivers %>% select(driverId, surname)) %>% filter(rank==1 & fastestLapTime < "1:15" & fastestLapTime > "0:30") %>% arrange(year)

#Incidenti mortali

incidentiMortali <- incidenti %>% filter(driverId %in% c(824, 102, 204, 238, 252, 324, 335, 361, 377, 399, 476, 502, 505, 590, 581, 597, 657, 700) & raceId %in% c(914, 259, 474, 540, 545, 604, 614, 647, 672, 695, 744, 750, 775, 772, 768, 794, 809) & statusId %in% c(3, 104)) %>% select(driver, circuitName, year, constructorName) %>% arrange(year) 

colnames(incidentiMortali)[1] <- "Pilota"
colnames(incidentiMortali)[2] <- "Circuito"
colnames(incidentiMortali)[3] <- "Anno"
colnames(incidentiMortali)[4] <- "Scuderia"

tabellaIncidentiMortali <- incidentiMortali %>% gt() %>% tab_options(table.width = pct(100))

#Tipologie Incidenti

graficoIncidenti <- ggplot(data = incidenti) +
  geom_area( stat="bin", binwidth=3, mapping = aes(x = year,  fill = status)) +
  labs(x = "Anno", y = "Numero Incidenti", title = "Grafico sul numero di incidenti per ogni anno") +
  scale_fill_discrete(name = "Tipologia Incidente")

#Influenza velocità

graficoTempi <- ggplot(data = tempi, mapping = aes(x = year, y = fastestLapTime , group = 1)) +
  geom_smooth(se=FALSE, colour = "red") +
  geom_point(mapping = aes(color = name)) + labs(x = "Anno", y = "Tempi", title = "Grafico dei migliori tempi in gara") + scale_color_discrete(name = "Scuderia") +
  geom_point(data = tempi %>% filter(fastestLapTime == min(tempi[,7])), size = 3, shape = 1) + 
  ggrepel::geom_label_repel(data = tempi %>% filter(fastestLapTime == min(tempi[,7])),
                                    mapping = aes(label = surname)) + font("xy.text", size = 7)

graficoIncidenti2003 <- ggplot(data = incidenti %>% filter(year>2003)) +
  geom_density( mapping = aes(x = year, group = 1), colour = "red")+ scale_y_reverse()  +
  theme(axis.title.x=element_blank()) + labs(y = "Densità Incidenti") + font("xy.text", size = 7)

graficoTempi2 <- ggplot(data = tempi, mapping = aes(x = year, y = fastestLapTime , group = 1)) +
  geom_smooth(se=FALSE, colour = "red", size = 0.5) + theme(axis.title.x=element_blank()) + labs(y = "Tempi") + font("xy.text", size = 7)

graficoIncidenzaVelocita <- ggarrange(graficoIncidenti2003, graficoTempi2, nrow = 1, ncol = 2)

#Incidenti scuderie
bella <- 82

mcLaren <- ggplot(incidenti %>%  filter(constructorId=="1"), aes(x="", y=status, fill=status)) +
         geom_bar(width = 1, stat = "identity") +
         coord_polar("y", start=0) +theme_void() +
         scale_fill_discrete(name = "Tipologia Incidente") +
         ggtitle("McLaren")

ferrari <- ggplot(incidenti %>%  filter(constructorId=="6"), aes(x="", y=status, fill=status)) +
         geom_bar(width = 1, stat = "identity") +
         coord_polar("y", start=0) +theme_void() +
         scale_fill_discrete(name = "Tipologia Incidente") +
         ggtitle("Ferrari")

williams <- ggplot(incidenti %>%  filter(constructorId=="3"), aes(x="", y=status, fill=status)) +
         geom_bar(width = 1, stat = "identity") +
         coord_polar("y", start=0) +theme_void() +
         scale_fill_discrete(name = "Tipologia Incidente") +
         ggtitle("Williams")

renault <- ggplot(incidenti %>%  filter(constructorId=="4"), aes(x="", y=status, fill=status)) +
         geom_bar(width = 1, stat = "identity") +
         coord_polar("y", start=0) +theme_void() +
         scale_fill_discrete(name = "Tipologia Incidente") +
         ggtitle("Renault")

sauber <- ggplot(incidenti %>%  filter(constructorId=="15"), aes(x="", y=status, fill=status)) +
         geom_bar(width = 1, stat = "identity") +
         coord_polar("y", start=0) +theme_void() +
         scale_fill_discrete(name = "Tipologia Incidente") +
         ggtitle("Sauber")

ligier <- ggplot(incidenti %>%  filter(constructorId=="27"), aes(x="", y=status, fill=status)) +
         geom_bar(width = 1, stat = "identity") +
         coord_polar("y", start=0) +theme_void() +
         scale_fill_discrete(name = "Tipologia Incidente") +
         ggtitle("Ligier")

tyrrell <- ggplot(incidenti %>%  filter(constructorId=="25"), aes(x="", y=status, fill=status)) +
         geom_bar(width = 1, stat = "identity") +
         coord_polar("y", start=0) +theme_void() +
         scale_fill_discrete(name = "Tipologia Incidente") +
         ggtitle("Tyrrell")

teamLotus <- ggplot(incidenti %>%  filter(constructorId=="32"), aes(x="", y=status, fill=status)) +
         geom_bar(width = 1, stat = "identity") +
         coord_polar("y", start=0) +theme_void() +
         scale_fill_discrete(name = "Tipologia Incidente") +
         ggtitle("Team Lotus")

graficoScuderie1 <-ggarrange(mcLaren, ferrari, williams, renault, ncol = 2, nrow = 2)
graficoScuderie2 <-ggarrange(ligier, tyrrell, sauber, teamLotus, ncol = 2, nrow = 2)

graficoIncidentiScuderieTop <- incidenti %>% group_by(constructorName) %>% count %>%filter(n > 50)  %>% ggplot(aes(x=reorder(constructorName, n), y=n)) +
           geom_bar(stat = "identity", colour = "red", fill = "red") +
                                                        coord_flip() + labs(x = "Scuderia", y = "Numero Incidenti", title = "Grafico sul numero di incidenti per ogni scuderia")

#Incidenti piloti

graficoIncidentiPiloti1 <- incidenti %>% group_by(driver) %>% count %>% filter(n > 20) %>% ggplot(aes(x=reorder(driver, n), y=n)) +
   geom_bar(stat = "identity", colour = "red", fill = "red") +
   coord_flip()  + labs(x = "Pilota", y = "Numero Incidenti", title = "Grafico sui piloti più coinvolti in incidenti")

schumacher <- ggplot(incidenti %>%  filter(driverId=="30"), aes(x="", y=status, fill=status)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +theme_void() +
  scale_fill_discrete(name = "Tipologia Incidente") +
  ggtitle("Michael Schumacher") 

trulli <- ggplot(incidenti %>%  filter(driverId=="15"), aes(x="", y=status, fill=status)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +theme_void() +
  scale_fill_discrete(name = "Tipologia Incidente") +
  ggtitle("Jarno Trulli")

tambay <- ggplot(incidenti %>%  filter(driverId=="175"), aes(x="", y=status, fill=status)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +theme_void() +
  scale_fill_discrete(name = "Tipologia Incidente") +
  ggtitle("Patrick Tambay")

sutil <- ggplot(incidenti %>%  filter(driverId=="16"), aes(x="", y=status, fill=status)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start=0) +theme_void() +
  scale_fill_discrete(name = "Tipologia Incidente") +
  ggtitle("Adrian Sutil")

graficoIncidentiPiloti2 <- ggarrange(trulli, schumacher, tambay, sutil, ncol = 2, nrow = 2)

#Incidenti circuiti

graficoIncidentiCircuiti <- incidenti %>% group_by(circuitName) %>% count %>% filter(n > 70) %>% ggplot(aes(x=reorder(circuitName, n), y=n)) +
  geom_bar(stat = "identity", colour = "red", fill = "red") +
  coord_flip()  + labs(x = "Circuito", y = "Numero Incidenti", title = "Grafico sul numero di incidenti per circuito")


circuitiMedia = incidenti %>% group_by(circuitName, year) %>% summarise(n = NROW(circuitName))

mediaCircuiti = aggregate(circuitiMedia$n, by=list(circuitName=circuitiMedia$circuitName), mean)  


graficoMediaIncidentiCircuiti <- mediaCircuiti %>%filter(x >= 4) %>% ggplot(aes(x=reorder(circuitName, x), y=x)) + 
  geom_bar(stat = "identity", colour = "red", fill = "red") +
  coord_flip() + labs(x = "Circuito", y = "Numero Incidenti", title = "Grafico sui circuiti con la media di incidenti più alta")



