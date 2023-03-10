###############################################################################
#### GOV 212: Klima, kriser og samfunnstryggleik. #############################
#### Handtering av uregjerlege samfunnsutfordringar ###########################
###############################################################################

# Regresjonsanalyse i 10 steg #

# 1) Velg kvar p? datamaskina di du vil jobbe: Session -> Set working directory #### 
# I denne mappa lagrar du GOV212.RData-fila.

# 2) Last inn pakkar: Tools -> Install Packages                                 #### 
# N?r desse er lasta ned m? du hente dei inn til denne arbeids?kta:
library(sjlabelled)
library(dplyr)
library(pollster)
library(stargazer)  
library(dotwhisker)
library(pollster)

# 3) Last inn data: Files -> Trykk p? datafila                                  ####
# Den heiter "d" og dukkar opp i "Environment"
View(d)


# 4) F? oversikt:                                                               ####
# For ? sj? kva verdiane i dataramma st?r for kan vi bruke denne:
get_labels(d)                                                                    # Eg har allereie fjerna ein del verdiar (gjort dei til NA)
summary(d)                                                                       # Ingen variablar g?r til lenger enn 10, sj?lv om dei opprinneleg hadde fleire verdiar.

           
# 5) Det vi vil forst?/forklare (avhengig variabel).                            ####
# De kan velje ein av desse:

## a) Bekymring for antibiotikaresistens
get_label(d$Antibiotika)                                                         # r25_heanw_ran er "antibiotikaresistens" eller "at antibiotika mister sin effekt". (he = health, an = antibiotics, w = worry)
get_labels(d$Antibiotika)                                                        # H?g verdi betyr at respondenten ikkje er bekymra

## b) Bekymring for klimaendringar 
get_label(d$Klima)
get_labels(d$Klima)                                                              # H?g verdi betyr at respondenten er bekymra

## c) Tillit til handtering av koronapandemien 
get_label(d$Korona)
get_labels(d$Korona)                                                             # H?g verdi betyr at respondenten har tillit til styresmaktene si handtering

## d) Frykt for terror
get_label(d$Terror)
get_labels(d$Terror)                                                             # H?g verdi betyr at respondenten ikkje er redd for nytt terrorangrep


# 6) Det vi kan bruke til ? forst?/forklare den avhengige variabelen            ####
## (uavhengige variablar/forklaringsvariablar)

## a) Alder 
get_labels(d$Alder)                                                              ## H?g verdi betyr yngre respondent 

## b) Kj?nn 
get_labels(d$Kvinne)                                                             ## H?g verdi betyr kvinne

## c) Parti 
get_labels(d$Parti)

## d) Omr?de ein bur i
get_labels(d$Bor)                                                                ## H?g verdi betyr at ein bur meir perif?rt 

## e) Kva ein synest om den ?konomiske situasjonen 
get_labels(d$Dagens_sit)                                                         ## H?g verdi betyr at ein vurderer den som d?rleg 

## f) Generell tillit 
get_labels(d$Gen_tillit)                                                         ## H?g verdi betyr st?rre tillit 

## g) Tillit til politikarar 
get_labels(d$Pol_tillit)                                                         ## H?g verdi betyr mindre tillit 


# 7) Vel dine variablar ####
sub_d <- d[,c("Vekt", "Alder", "Klima")]
sub_d2 <- na.omit(sub_d)                                                        # na.omit fjernar alle rader som manglar ein verdi
sub_d <- na.omit(sub_d)

# 8) F?resetnader for regresjonsanalyse ####
# a) Class ####
## Vel ? behandle b?de f?dsels?r og bekymring for klima som numeriske variablar for desse analysane, men det er kanskje ikkje heilt riktig fordi avstanden mellom "Sv?rt bekymret" og "Bekymret" ikkje n?dvendigvis er den same som avstanden mellom "Bekymret" og "Noe bekymret". Avstanden mellom ? vere f?dd f?r 1939 og 1940-1949 er heller ikkje n?dvendigvis like stor som avstanden mellom ? vere f?dd mellom 1940-1949 og 1950-1959. 
## Det er utanfor omfanget til dette seminaret ? g? gjennom korleis ein unders?ker dette n?rare, men det er i orden for arbeidskravet til GOV212. 

### Ver obs p? at h?gare verdi betyr yngre respondent og meir bekymra. 

class(sub_d$Vekt)                                                               # Vekt er viktig s?rleg fordi dei med h?gare utdanning er overerrepresenterte i Norsk medborgerpanel, og dei har som regel andre haldningar enn dei utan h?gare utdanning.Norsk medborgerpanel er skeivt p? andre m?tar ogs?, men dette er den mest alvorlege skeivheita. Dette kan ein lese meir om i metoderapportane til datainnsamlingane.
class(sub_d$Alder)                                                              # Desse variablane inneheld mykje informasjon. Dei er ikkje eigna til ? bruke i regresjonsanalyse slik dei st?r no.
class(sub_d$Klima)

sub_d$Alder <- as.numeric(sub_d$Alder)                                          # Gjer dei difor om til numeriske variablar.
sub_d$Klima <- as.numeric(sub_d$Klima) 

# Til info; No kan vi ikkje lenger bruke sjlabelled:
get_labels(sub_d$Klima)
# Men vi har labelane i den andre dataramma: 
get_labels(d$Klima)

# b) Gjennomsnitt, median  ####
summary(sub_d$Klima)                                                            # Gjennomsnitssvaret ligg mellom "Noe bekymret" og "Bekymret", det er det typiske svaret. Medianen er "Bekymret".
sd(sub_d$Klima)                                                                 # Standardavviket er 1.1, det er variasjonen rundt den typiske verdien.

# 9) K?yr analyse ####

# a) Bivariat regresjonsanalyse ####
m1 <- lm(Klima ~ Alder, data = sub_d, weights = Vekt)

# N?r vi skal tolke resultata i dette kurset fokuserer vi p? retning og signifikansniv?. Vi tolkar ikkje storleiken p? effekten.
stargazer(m1, type = "html", out = "table1.html")                               # Fokuser difor p? om samanhengen er positiv eller negativ, og stjernene.

# b) Kontrollere for andre variablar: Multivariat regresjonsanalyse ####
sub_d <- d[,c("Vekt", "Alder", "Klima", "Kvinne", "Parti", "Bor")]
summary(sub_d)
sub_d <- na.omit(sub_d)                                                         # Ta berre med dei variablane du er interessert i, mistar mange observasjonar med ? ta med "Bor"

sub_d$Alder <- as.numeric(sub_d$Alder)
sub_d$Klima <- as.numeric(sub_d$Klima) 

# Desse er ikkje numeriske, s? her vil vi gjere om verdien fr? tal til ord
sub_d$Kvinne <- as.factor(to_label(sub_d$Kvinne))                               # Her brukar vi sjlabelled for ? f? dei same verdiane som i kodeboka: 
View(sub_d)

get_labels(d$Parti)
sub_d$Parti <- case_when(sub_d$Parti == 1 ~ "KrF",                              # For parti vil eg heller ha forkortingane, s? d? skriv eg inn kvar verdi sj?lv
                         sub_d$Parti == 2 ~ "H", 
                         sub_d$Parti == 3 ~ "FrP", 
                         sub_d$Parti == 4 ~ "V", 
                         sub_d$Parti == 5 ~ "SV", 
                         sub_d$Parti == 6 ~ "Sp", 
                         sub_d$Parti == 7 ~ "MDG", 
                         sub_d$Parti == 8 ~ "Ap", 
                         sub_d$Parti == 9 ~ "R")
sub_d$Parti <- as.factor(sub_d$Parti)                                           # Og gjer om til faktor etterp?

summary(sub_d$Parti)                                                            # H?gre er det st?rste partiet, vi set det som referansekategori
sub_d$Parti <- relevel(sub_d$Parti, ref = "H")

get_labels(d$Bor)
sub_d$Bor <- as.factor(to_label(sub_d$Bor))                                     # Her brukar vi sjlabelled for ? f? dei same verdiane som i kodeboka
View(sub_d)

summary(sub_d$Bor) 
sub_d$Bor <- relevel(sub_d$Bor, ref = "En liten eller mellomstor by")           # Set den st?rste kategorien som referansekategori her ogs?, det er spesielt fint fordi det er verdien i midten. Om du vil kan du argumentere for at det er best ? bruke "En storby" som referansekategori, men eg vil ikkje anbefale ? bruke motsatt ende av skalaen (Spredtbygd str?k), d? her er f? respondentar. 

m2 <- lm(Klima ~ Alder + Kvinne, data = sub_d, weights = Vekt)
stargazer(m1, m2, type = "html", out = "table2.html")

m3 <- lm(Klima ~ Alder + Kvinne + Parti, data = sub_d, weights = Vekt)
stargazer(m1, m2, m3, type = "html", out = "table3.html")

m4 <- lm(Klima ~ Alder + Kvinne + Parti + Bor, data = sub_d, weights = Vekt)
stargazer(m1, m2, m3, m4, type = "html", out = "table4.html")

# 10) Graf ut regresjonsresultata ####

dwplot(list(m1, m2, m3, m4),
       vline = geom_vline(
         xintercept = 0,
         colour = "grey60",
         linetype = 2))


# Om du ikkje ?nskjer ? gjere regresjonsanalyse er det ogs? mogeleg ? presentere basert p? fordeling innanfor variablane du er interessert i. 

# Krysstabellanalyse ####
cb_row <- crosstab(df = d, x = Alder, y = Klima, 
                   weight = Vekt, 
                   pct_type = "row",
                   n = FALSE, unwt_n = TRUE)

cb_col <- crosstab(df = d, x = Alder, y = Klima, 
                   weight = Vekt, 
                   pct_type = "col",
                   n = FALSE, unwt_n = TRUE)


## Med feilmargin ####
cb_moe <- moe_crosstab(df = d, x = Alder, y = Klima, 
                      weight = Vekt, 
                      n = FALSE, unwt_n = TRUE)

