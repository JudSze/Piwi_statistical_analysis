#Adatok beolvasása
setwd("E:/Egyetem/ATEBSc/Szakdolgozat")
adattabla=read.table("piwi_mennyiseg.csv", sep=";", dec=",", header=T)
str(adattabla)
fix(adattabla)

adattabla$Mean=numeric(adattabla$Mean)

#Csomagok
library("wesanderson")
names(wes_palettes)

library("lattice")
library("pkgconfig")
library("ggplot2")
library("plyr")
library("reshape2")
library("multcomp")

#Csoportok létrehozása
isowhite=adattabla[adattabla$Group=="1isowhite",]
whitex=adattabla[adattabla$Group=="2whitex",]
Ras=adattabla[adattabla$Group=="4Ras",]
Yki=adattabla[adattabla$Group=="3Yki",]
RasYki=adattabla[adattabla$Group=="5RasYki",]


#Átlag és szórás számítása
with(adattabladr, tapply(Mean, Group, mean))
with(adattabladr, tapply(Mean, Group, sd))

#Boxplot differenciálódó régió
pal <- wes_palette("FantasticFox1", 5,  type = "continuous")

par(mfrow=c(1,1))
plot(Mean~Group,
     data=adattabla,
     xlab="Genotipusok", ylab="Mean Gray Value",
     main="Differenciálódó régió Átlagos Szürke Értéke genotípusonként",
     pch=19, col=pal)

#Levene próba
install.packages("inferr")
library(inferr)
library(car)

with(adattabla, leveneTest(Mean~Group))
#p=0.3462
#A varianciák nem különböznek.

#Normalis eloszlas
#QQnorm fuggveny
par(mfrow=c(2,2))
#White
qqnorm(whitex$Mean)
qqline(whitex$Mean)
#Yki
qqnorm(Yki$Mean)
qqline(Yki$Mean)
#Ras
qqnorm(Ras$Mean)
qqline(Ras$Mean)
#Ras,Yki
qqnorm(RasYki$Mean)
qqline(RasYki$Mean)


#Shapiro-Wilk teszt
#vawhite
shapiro.test(isowhite$Mean)
#white
shapiro.test(whitex$Mean)
#Yki
shapiro.test(Yki$Mean)
#Ras
shapiro.test(Ras$Mean)
#RasYki
shapiro.test(RasYki$Mean)

#Mindegyiknel p>0.05, ugyhogy normalis eloszlas van

#ANOVA
mod=lm(Mean~Group, adattabla)
anova(mod)

plot(mod)
#p=nagyon kicsi szam, 61.26^(-6)
#Itt a tenyezo/faktor a genotipus, tehat azt mondhatjuk
#,hogy az atlagos pixelintenzitas erteke szignifikansan
#kulonbozik a genotipusok kozott.

#Tukey-proba
dunn=confint(glht(mod, linfct=mcp(Group="Dunnett")))
summary(dunn)

tukey=aov(Mean~Group, data=adattabla)
anova(tukey)
TukeyHSD(tukey)

#Amit latunk
#$Group
#diff         lwr       upr     p adj
#2whitex-1isowhite  25.466476   8.8607925 42.072160 0.0009439
#3Yki-1isowhite     35.889048  19.2833639 52.494731 0.0000056
#4Ras-1isowhite     21.001333   5.1605207 36.842146 0.0048282
#5RasYki-1isowhite  15.598889  -0.4437257 31.641503 0.0598382
#3Yki-2whitex       10.422571  -2.4401359 23.285279 0.1578494
#4Ras-2whitex       -4.465143 -16.3239730  7.393687 0.8102847
#5RasYki-2whitex    -9.867587 -21.9946640  2.259489 0.1549286
#4Ras-3Yki         -14.887714 -26.7465445 -3.028884 0.0082277
#5RasYki-3Yki      -20.290159 -32.4172355 -8.163082 0.0003056
#5RasYki-4Ras       -5.402444 -16.4590535  5.654165 0.6233023

