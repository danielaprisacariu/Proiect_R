# Lista pachete necesare
pacheteBDSA <- c("knitr", "rmarkdown", "RColorBrewer", "scales", 
                 "tidyverse","dplyr", "plyr", "ggplot2", "gganimate", "Rmisc", 
                 "Hmisc","randomForest", "rattle", "rpart.plot", "caret", 
                 "gmodels", "PresenceAbsence", 
                 "ROCR", "ca", "plotROC", "ROSE", 
                 "devtools","rpart", "readxl", "ellipsis","gridExtra", "ca", 
                 "wordcloud", "radarchart", "tidytext", "tidyr", "textdata", "evaluate")
#incarcare pachete
sapply(pacheteBDSA, library, character.only = T)



# Citire date -------------------------------------------------------------
# Import fisier csv cu raspunsuri chestionar
Data2020 <- read_csv("survey_results_public.csv")
dim(Data2020) # 64461    61
#view(Data2020)


#Import fisier csv cu descrierea variabilelor
vb_desc <- read_csv("survey_results_schema.csv")
dim(vb_desc) # 61  2
view(vb_desc)


# Selectarea variabilelor de interes
Data2020 <- Data2020 %>% 
  select(c(Hobbyist, Age, Age1stCode, CompFreq, CompTotal, ConvertedComp, 
           Country, CurrencyDesc, CurrencySymbol, DevType, EdLevel, Employment, Gender, JobFactors, JobSat, 
           JobSeek,  NEWDevOps, NEWDevOpsImpt, NEWEdImpt, NEWLearn, NEWOvertime, NEWPurpleLink, 
           OpSys, OrgSize, PurchaseWhat, SOAccount, SOComm, SOPartFreq, SOVisitFreq, SurveyEase, 
           SurveyLength, UndergradMajor, WelcomeChange, WorkWeekHrs, YearsCode, YearsCodePro
  ))

dim(Data2020) # 64461    35

Data2020t <- Data2020 %>% 
  filter(!is.na(ConvertedComp))

# Vrem sa vedem cuantumul salariului pe nivele in 7 tari
# Cream un nou set de date din care eliminam inregistrarile valorile outlier (salariul NA sau salariul < 300000)

Data2020filter <- Data2020 %>% 
  filter(!is.na(ConvertedComp) & ConvertedComp > 300000)
dim(Data2020filter) # 1620   36


countries = c("Belgium", "Bulgaria", "France", "Germany", "Spain", "Italy", "Poland")
length(countries) # 7 tari

# Am pastrat in setul de date doar tarile care ma interesau
Data2020filter <- Data2020filter %>% 
  filter(Country%in%countries )
dim(Data2020filter)

sursa <- "Stack Overflow Developer Survey 2020"

options(scipen= 999)

# Reprezentarea grafica a salariului anual in dolari US
windows()
Data2020filter %>%
  filter(!is.na(ConvertedComp) & ConvertedComp > 300000) %>% 
  ggplot(aes(ConvertedComp)) +
  geom_density(color="white", fill="aquamarine3", adjust=5, alpha=.6) +
  labs(x = "ConvertedComp",
       y = "Densitate",
       caption = sursa) +
  ggtitle("Salariul anual in dolari US")

view(table(Data2020filter$ConvertedComp))

# Transformam variabila numerica ConvertedComp in variabila categoriala
Data2020filter <- Data2020filter %>% 
  mutate(Salary = as.factor(case_when(ConvertedComp <= 400000 ~ "300000-400000",
                                      ConvertedComp <= 500000 ~ "400001-500000",
                                      ConvertedComp <= 600000 ~ "500001-600000",
                                      ConvertedComp <= 700000 ~ "600001-700000",
                                      ConvertedComp > 700000 ~ "700000+")))
table(Data2020filter$Salary) 


# Tabelul de contingenta
m1_ac <- table(Data2020filter$Salary, Data2020filter$Country)

# Reprezentarea grafica a celor 2 variabile categoriale
windows()
Data2020filter  %>%
  filter(!is.na(Salary)) %>% 
  group_by(Salary, Country) %>% 
  dplyr::summarise(n=n())  %>% 
  mutate(freq = n / sum(n)) %>% 
  ggplot()+
  geom_col(aes(x=reorder(Country,n),y=n, fill=Salary, position="fill"))+
  scale_fill_brewer(palette="Dark2")


corespondente_Salary <- ca(m1_ac)
summary(corespondente_Salary)

chisq.test(m1_ac)
# p-value = 0.0000000113 < 0.05 => modelul este este semnificativ dpdv sattistic

windows()
plot(corespondente_Salary, lines=c(FALSE, F))

# Eliminarea inregistrarilor care au mai mult de 300000 
Data2020t <- Data2020 %>% 
  filter(!is.na(ConvertedComp) & ConvertedComp < 300000)


# Filtrarea setului de date in functie de gen
genders = c("Man", "Woman")

Data2020t <- Data2020t %>% 
  filter(Gender%in%genders )

Data2020t$Gender <- factor(Data2020t$Gender, labels = c("M", "F")) 

table(Data2020t$Gender)

# Crearea variabilei categoriale Age1stCode1
Data2020t <- Data2020t %>% 
  mutate(Age1stCode1 = as.factor(case_when(Age1stCode < 14 ~ "< 14",
                                           Age1stCode < 18 ~ "14-18",
                                           Age1stCode < 22 ~ "18-22",
                                           Age1stCode >= 22 ~ "> 22"
                                           )))
table(Data2020t$Age1stCode1)


# Creare variabilei categoriale Age1
Data2020t <- Data2020t %>% 
  mutate(Age1 = as.factor(case_when(Age <= 25 ~ "18-25",
                                    Age <= 30 ~ "26-30",
                                    Age <= 35 ~ "31-35",
                                    Age <= 40 ~ "36-40",
                                    Age <= 45 ~ "41-45",
                                    Age > 45  ~ "45+")))

table(Data2020t$Age1)

# SOVisitFreq
table(Data2020t$SOVisitFreq)

Data2020t <- Data2020t %>% 
  mutate(SOVisitFreq1 = as.factor(case_when(SOVisitFreq == "A few times per month or weekly" | SOVisitFreq == "A few times per week" | SOVisitFreq == "Less than once per month or monthly" ~ 0.5,
                                            SOVisitFreq == "Daily or almost daily" | SOVisitFreq == "Multiple times per day" ~ 1,
                                            SOVisitFreq == "I have never visited Stack Overflow (before today) " ~ 0
                                            )))

table(Data2020t$SOVisitFreq1)

# Creare variabila WellPaid
Data2020t <- Data2020t %>% 
  mutate(WellPaid = as.factor(case_when(ConvertedComp <  median(ConvertedComp) ~ 0,
                                        ConvertedComp >= median(ConvertedComp) ~ 1)))
table(Data2020t$WellPaid)

dim(Data2020t) #30219    40


# Estimarea modelului de Regresie Liniara ---------------------------------

summary(Data2020filter %>% 
          select (c(Age1, SOVisitFreq1, Gender, Age1stCode1)))

attach((Data2020filter))

# Eliminarea variabilelor NA din setul de date
Data2020_model <- Data2020t %>% 
  filter(!is.na(Age1)) %>% 
  filter(!is.na(Gender)) %>% 
  filter(!is.na(Age1stCode1)) %>% 
  filter(!is.na(SOVisitFreq1))
  
dim(Data2020_model) #206  40

table(Data2020_model$WellPaid)

options(scipen= 999)

# Vrem sa vedem daca salariul este dependent de varsta, de gen, de varsta la care a scris primul cod, si de cat de des frecventeaza Stack Overflow
model <- glm(WellPaid ~ Age1 + Gender + Age1stCode1 + SOVisitFreq1,
              data = Data2020_model,
              family = "binomial") 
summary(model)
# Valorile Pr(|z|) sunt mai mici decat 0.05 => ca toti parametrii sunt semnificativi dpdv statistic
# AIC = 28306


# Interpretarea modelului de regresie -------------------------------------

exp(coef(model))
# Sansele ca un dezvoltator de sex feminin sa fie bine platit cresc cu 20%
# Sansele ca un dezvoltator sa fie mai bine platit pentru cei care au inceput sa scrie cod de la o varsta cuprinsa intre 14 si 18 ani, scad cu 42%
# Sansele ca cei care viziteaza foarte frecvent Stack Overflow sa fie bine platiti scad cu 26%


# Calculul predictiilor modelului -----------------------------------------
pred <- predict(model, type = "response")

length(pred) #23028
dim(Data2020_model) # 23028    42

# Codificam predictiile in cele doua clase:
# 1 = WellPaid
# 0 = Not WellPaid (mai putin bine platiti)
summary(pred)
y_pred_num1 <- ifelse(pred > 0.5, 1, 0)
# prob de peste 5% este default in clasa 1 la arbori
table(y_pred_num1)

y_pred <- factor(y_pred_num1, labels = c(0, 1)) # var categ
y_act <- Data2020_model$WellPaid

mean(y_pred == y_act, na.rm = T) # 0.6713132

pred1 <- prediction(pred, yact)
class(pred1) 
class(Data2020_model)

pred1 %>% 
  performance(measure = "tpr", x.measure = "fpr") %>% 
  plot(colorize = T, lwd = 7)

# Curba se apropie de coltul din stanga sus => modelul este bun

# Determinare valoare AUC
AUCLog <- performance(pred1, measure = "auc")@y.values[[1]]

cat("Valoarea AUC pt Model ales de Regresie Logicatica: ", AUCLog) # 0.7230161
# Valoarea AUC pt Model nostru de Regresie Logicatica:  0.7230161

# Determinam matricea de confuzie a clasificarii pt modelul estimat 1(R.Log)
confusionMatrix(data = y_pred, as.factor(y_act))
(6970 + 8489)/(6970 + 8489 + 3471 + 4098) = 0.6713132
# Indicatorul de acuratete este 0.67 => 67% dintre observatii sunt corecte


# Arbori de clasificare------------------------------------------------------------------

library(rattle)
library(rpart) # Pachetul pentru arbori decizionali
library(rpart.plot)
library(RColorBrewer)

# Validarea incrucisata
DTfit <- rpart(WellPaid ~ Age1 + Gender + Age1stCode1 + SOVisitFreq1,
               data = Data2020_model, method = "class", minsplit=1, minbucket = 1, cp = -1, maxdepth = 7) 

printcp(DTfit)

# Reprezentare grafica Validarea Incrucisata
windows()
plotcp(DTfit)
# Prima valoare de sub grafic are size of tree = 6 si, deoarce este prea are prea putine noduri, alegem urmatorul cp (parametrul de complexitate) care are 8 noduri
# cp (media geometrica a valorilor) = 0.00094

summary(DTfit)

# Reprezentarea grafica a arborilor
prp(DTfit, type = 2, extra= 106, under = TRUE, fallen.leaves = FALSE, box.palette = "BuPu")

fancyRpartPlot(DTfit, palettes = "YlOrBr", type = 2, leaf.round = 3, tweak = 2)

# Derminarea valorilor coeficientilor de importanta
DTfit$variable.importance
# Observam ca pentru model cel mai important este parametrul Age1, urmat de Age1stCode1

# Determinatrea arborelui final aplicand functia prune() pe o valoare a cp = 0.00094
arboref <- prune(DTfit, cp = 0.00094)

windows()
fancyRpartPlot(arboref, palettes = "YlOrBr", type = 2, leaf.round = 3, tweak = 1)

# Predictiile arborelui de clasificare 
library(ROCP)

pred_DT <- predict(DTfit, Data2020_model, type = "prob")[,2]
head(pred_DT)
y_pred_num_DT <- ifelse(pred_DT > 0.5, 1, 0)
y_pred_DT <- factor(y_pred_num_DT, levels = c(0, 1))
y_act <- Data2020_model$WellPaid
length(pred_DT)

prediction(pred_DT, y_act) %>%
  performance(measure = "tpr", x.measure = "fpr") %>%
  plot(col = 'dodgerblue2', lwd=3, lty=1, main="ROC Curve - Arbore de Clasificare \n vs. Regresie Logistica")

# Predictii Regresie Logistica
prediction(pred, y_act) %>%
  performance(measure = "tpr", x.measure = "fpr") %>%
  plot(col = 'firebrick3', lwd=3, lty=1)
par(new=TRUE)
# Add a legend
abline(c(0,1), lty=2)
legend("bottomright", legend=c("Regresie Logistica", "Arbore de Clasificare"),
       col=c("firebrick3", "dodgerblue2"), lty=1:1, cex=1)

# Calculam valoarea AUC (Area Under the Curve) pentru Arborele de clasificare
pr_model <- prediction(pred_DT, y_act, label.ordering = NULL)
pr_model
perf <- performance(pr_model,"tpr","fpr")
perf
plot(perf, colorize=TRUE, lwd=5, main="Colorized ROC Curve")

AUC_Arbore <- performance(pr_model, "auc")@y.values[[1]]
cat("Valoarea AUC pentru Model 2 Arbore de Clasificare:", AUC_Arbore)
# Valoarea AUC pentru Model 2 Arbore de Clasificare: 0.7260392
# Valoarea AUC pt Model nostru de Regresie Logicatica:  0.7230161
# Observam ca aororele de clasificare este cu foarte putin mai bun decat Modelul de Regresie Liniara

# Confusion Matrix pentru Arbore
library(caret)
confusionMatrix(data = y_pred_DT, as.factor(y_act), mode="everything")

(7223 + 8285)/(7223 + 8285 + 3675 + 3845) = 0.673441
# Indicatorul de acuratete este 0.67 => 67% dintre observatii sunt corecte

