#Kausalanalyse in den Sozialwissenschaften, Soziologisches Institut UZH, Prof. Dr. Elias Naumann, HS26
#Erste Dateninspektion und Vor-Analyse für Forschungsbericht
#Vorläufige Fragestellung: Inwiefern nimmt der Übergang zur Elternschaft für in der SChweiz lebende Frauen EInfluss auf ihre Lebenszufriedenheit?
#Gewählte Methode: Fixed Effects
#Daten: SHP, Wellen 1-25

#-------------------------------------------------------------------------
#-------------------------------------------------------------------------

#Packages laden
library(haven)
library(dplyr)
library(labelled)
library(tidyverse) 
library(stargazer) #Tabellen
library(sjPlot) #Visualisierung
library(ggplot2)
library(psych) #Deskriptive Stats
library(mediation) #Für die Mediationsanalyse
library(gtsummary)
library(gt)
library(patchwork)
library(gtable)
library(scales)
library(car)
library(emmeans)
library(naniar)
library(table1)
library(visreg)
library(fixest)             #lineare Modelle mit FE, flexibel und schnell

#Daten einlesen
#Nehmen das SHP long file, das alle Wellen zusammenfasst
library(haven)
shplong_p_user <- read_dta("~/Desktop/#MAJOR SOZIOLOGIE/#HS26/Datenanalyse/Project/swissubase_932_11_0/Data_STATA/SHP-Data-Longfile-STATA/shplong_p_user.dta")
View(shplong_p_user)

#Masterfile für Geburtsdingsda
library(haven)
shp_mp <- read_dta("~/Desktop/#MAJOR SOZIOLOGIE/#HS26/Datenanalyse/Project/swissubase_932_11_0/Data_STATA/SHP-Data-WA-STATA/shp_mp.dta")
View(shp_mp)

#Data Wrangling
##Data Wrangling aka auf Pers mit gült. Inti begrenzen##
#----------------------------------------------------------
#VORHER DIE DINGS MIT AUF STIPRO BEGRENZEN AUSFÜHREN!!
# Vorab-Check der Verteilung
table(as_factor(shplong_p_user$status))
#proxy: 56580; grid only: 70295; individ. quest.: 226212

table(fct_drop(as_factor(shplong_p_user$status)))
table(shplong_p_user$status)
shplong_p_user_1<-filter(shplong_p_user, status!=1) #Schliesst Personen ohne gültiges Interview aus, behält jedoch Proxy Interviews (vom SHP empfohlen)
dim(shplong_p_user_1)
nrow(shplong_p_user_1)

erstes_kind <- shp_mp %>%
  filter(!is.na(idmoth__), !is.na(birthy)) %>%
  group_by(idmoth__) %>%
  summarise(
    first_birth_year = min(birthy),
    .groups = "drop"
  )
# Frühestes Geburtsjahr der Kinder je Mutter
erstes_kind <- shp_mp %>%
  filter(!is.na(idmoth__), !is.na(birthy)) %>%
  group_by(idmoth__) %>%
  summarise(
    first_birth_year = min(birthy),
    .groups = "drop"
  )

# An das Long-File anhängen
longfile_mit_geburtsjahr <- shplong_p_user_1 %>%
  left_join(erstes_kind, by = c("idpers" = "idmoth__"))

#für masterfile auch
#names(shp_mp)
# Alle Statusspalten finden, z. B. status99, status00, ..., status24
#status_vars <- grep("^status[0-9]{2}$", names(shp_mp), value = TRUE)

# Personen behalten, deren Status in mindestens einem Jahr
# nicht fehlend und ungleich 1 ist
#shp_mp_1 <- shp_mp %>%
#  filter(
#    if_any(
 #     all_of(status_vars),
#      ~ !is.na(.x) & .x != 1
 #   )
  #)
#shp_mp_1<-filter(shp_mp, status!=1)
#dies erstmal auf eis gelegt

#Datensatz auf Stichprobe (Frauen im Alter von 18-45) begrenzen
##Datensatz auf Stichprobe begrenzen##
#----------------------------------------
# Filter auf Geschlecht (2 = Frau) und Alter (25 bis 70 Jahre)
shplong_p_user_1 <- shplong_p_user_1 %>%
  filter(sex == 2, 
         age >= 18 & age <= 45)

#n Analysestichprobe
shplong_p_stipro <- shplong_p_user_1 %>%
  filter(status != 2,             # Gültiges Interview
         sex == 2,                   # Nur Frauen
         age >= 18 & age <= 45)    # Alter 18-45


# Kontrolle der Fallzahlen und der Spannen: jetzt longfile_mit_geburtsjahr
nrow(longfile_mit_geburtsjahr)           # Wie viele Frauen (18-45) sind im DS? Sind 47558
range(longfile_mit_geburtsjahr$age)    # Zeigt Minimum und Maximum des Alters an (sollte 18 & 45 sein)
table(longfile_mit_geburtsjahr$sex)    # Sollte nur noch die Kategorie 2 anzeigen

# Auf Frauen beschränken, die ein jahr vorher und eins nacher gültige angaben gemacht haben
# Frauen mit gültigem ownkid-Wert in allen 6 benötigten Jahren
geeignete_frauen <- longfile_mit_geburtsjahr %>%
  filter(
    !is.na(first_birth_year),
    !is.na(ownkid),
    year %in% c(first_birth_year - 1, first_birth_year + 1)
  ) %>%
  distinct(idpers, year, first_birth_year) %>%
  group_by(idpers, first_birth_year) %>%
  summarise(
    anzahl_jahre = n_distinct(year),
    .groups = "drop"
  ) %>%
  filter(anzahl_jahre == 2) %>%
  select(idpers)

# Alle Long-File-Zeilen dieser Frauen behalten
analysedaten <- longfile_mit_geburtsjahr %>%
  semi_join(geeignete_frauen, by = "idpers")
nrow(analysedaten) #n = 1660?!


#Für die Analyse relevante Variablen finden und inspizieren: 
#UV: Geburt Kind 1 (arbeiten vorläufig mit ownkid)
look_for(shplong_p_stipro, "ownkid")
summary(shplong_p_stipro$ownkid) 
#dieser Variable noch die Fehlercodes zu NAs umkodieren

#AV: Lebenszufriedenheit
look_for(shplong_p_stipro, "pc44")
summary(shplong_p_stipro$pc44) #diese gut

look_for(shplong_p_stipro, "age")
summary(shplong_p_stipro$age)

look_for(shplong_p_stipro, "pd161")
summary(shplong_p_stipro$pd161) #zu viele NAs?

look_for(shplong_p_stipro, "pl01a")
summary(shplong_p_stipro$pl01a) #zu viele NAs?

look_for(shplong_p_stipro, "iwyn")
summary(shplong_p_stipro$iwyn) #keine NAs?!

look_for(shplong_p_stipro, "edu_1_")
summary(shplong_p_stipro$edu_1_)
shplong_p_stipro$edu_1_ <- as.factor(shplong_p_stipro$edu_1_) 

look_for(shplong_p_stipro, "year")
summary(shplong_p_stipro$year)

look_for(shplong_p_stipro, "w34a") #im Folgenden zunächst noch rausgelassen
summary(shplong_p_stipro$pw34a)

#Kinderwunsch
look_for(shplong_p_stipro, "pd81")
summary(shplong_p_stipro$pd81)
#Die Variable wäre eigentlich gar nicht so schlecht, aber viele NA's

#Datensatz auf analyserelevanten Variablen reduzieren
analysedaten_2 <- analysedaten %>%
  select(
    idpers,
    year,
    first_birth_year,
    ownkid,
    pc44,
    age,
    edu_1_,
    iwyn,
    pd161,
    pl01a,
    year,
    pd81
  )

##Item-Missings rekodieren##
#------------------------------
analysedaten_2<- replace_with_na_all(analysedaten_2, condition = ~.x%in% c(-8, -7, -6, -5, -4, -3, -2, -1)) #vorher auf analyserelevanten variablen beschränken!!
summary(analysedaten_2)
nrow(analysedaten_2)

#Erstmal lineare Regression mit den Variablen rechnen, so wie se sind
modell <- lm(
  pc44 ~ ownkid + age + edu_1_ + iwyn + pd161 + pl01a + year + pd81,
  data = analysedaten_2
)

summary(modell)

#nobs(modell)

#sapply(
 # analysedaten_2[c("pc44", "ownkid", "age", "edu_1_", "iwyn",
  #                 "pd161", "pl01a", "year", "pd81")],
  #function(x) c(
  #  fehlend = sum(is.na(x)),
   # verschiedene_werte = length(unique(na.omit(x)))
  #)
#)
#Regressionstabelle zur Visualisierung
stargazer(
  modell,
  type = "text",
  title = "Regression der Lebenszufriedenheit",
  dep.var.labels = "Lebenszufriedenheit (pc44)",
  covariate.labels = c(
    "Anzahl eigener Kinder (ownkid)",
    "Alter",
    "Bildung (edu_1_)",
    "Einkommen (iwyn)",
    "Migrationshintergrund",
    "Erhebungsjahr",
    "Gesundheit",
    "Kinderwunsch"),
  intercept.bottom = FALSE,
  notes = paste("Quelle: SHP; verwendete Beobachtungen: n = 1660"),
  star.cutoffs = c(0.05, 0.01, 0.001),
  omit.stat = c("ser", "f"),
  out = "Regressionsergebnisse.txt")

#Mal ohne age und year
#modell <- lm(
 # pc44 ~ ownkid + edu_1_ + iwyn + pd161 + pl01a + pd81,
#  data = analysedaten_2
#)

#summary(modell)

#verwendet <- model.frame(modell)

#table(verwendet$pd161, useNA = "ifany")

#well, ich glaube, ich muss hier mal noch paar sachen rekodieren. das ist aber future me's problem. good night
#hallo hallo hallöchen