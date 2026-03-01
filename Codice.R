library(openxlsx)
library(Hmisc)
library(plyr)
library(dplyr)
library(stringr)
library(mice)
library(corrgram)
library(MLmetrics)
library(caret)
library(cluster)
library(factoextra)
library(ggplot2)
library(dendextend)
library(randomForest)
library(corrplot)
library(mclust)
library(dbscan)
library(class)
library(keras)
library(kernlab)
library(MLeval)

data <- read.xlsx("train.xlsx", na.strings=c("NA", "_______", "_", "!@9#%8", "__10000__"))
#str(data)

# ==== 1) Pre-Processing ----------------------------------------------------------


# ---- 1.1) Rimozione anomalie e imputazione tramite valore modale ------------------

##Rimozione di "ID", "NAME" e "SSN":
data_new <- data[, -c(1, 4, 6)]

###CREDIT_SCORE###
data_new$CREDIT_SCORE <- as.factor(data_new$CREDIT_SCORE)

###AGE###
data_new$AGE <- gsub("_", "", data_new$AGE)
data_new$AGE <- as.numeric(data_new$AGE)
calcola_moda <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
data_new$gruppo <- rep(1:ceiling(nrow(data_new) / 8), each = 8)[1:nrow(data_new)]
data_new$AGE <- ave(data_new$AGE, data_new$gruppo, FUN = calcola_moda)

###OCCUPATION### 
data_new$OCCUPATION <- as.factor(data_new$OCCUPATION)
vet <- numeric(0)
for (persona in 1:12500){
  ds <- data_new$OCCUPATION[((persona-1)*8+1):(persona*8)]
  new_value=impute(ds)
  #new_value <- rep(mode(ds), 8)
  vet <- c(vet, new_value)
}
data_new$OCCUPATION <- vet
data_new$OCCUPATION <- as.factor(data_new$OCCUPATION)

###ANNUAL INCOME### 
data_new$ANNUAL_INCOME <- gsub("_", "", data_new$ANNUAL_INCOME)
data_new$ANNUAL_INCOME <- round(as.numeric(data_new$ANNUAL_INCOME), 2)
data_new$gruppo <- rep(1:ceiling(nrow(data_new) / 8), each = 8)[1:nrow(data_new)]
data_new$ANNUAL_INCOME <- ave(data_new$ANNUAL_INCOME, data_new$gruppo, FUN = calcola_moda)
out_AI <- boxplot.stats(data_new$ANNUAL_INCOME)$out
which_rows_AI <- which(data_new$ANNUAL_INCOME %in% c(out_AI))

###MONTHLY SALARY### 
calcola_moda <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
data_new$gruppo <- rep(1:ceiling(nrow(data_new) / 8), each = 8)[1:nrow(data_new)]
data_new$MONTHLY_SALARY <- ave(data_new$MONTHLY_SALARY, data_new$gruppo, FUN = calcola_moda)

###NUM_BANK_ACCOUNTS, NUM_CREDIT_CARD, INTEREST_RATE###
variabili_da_controllare <- c("NUM_BANK_ACCOUNTS", "NUM_CREDIT_CARD", "INTEREST_RATE")
data_new <- data_new %>%
  mutate(NUM_BANK_ACCOUNTS = if_else(NUM_BANK_ACCOUNTS < 0, 0, NUM_BANK_ACCOUNTS))
moda <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
data_new <- data_new %>%
  group_by(CUSTOMER_ID) %>%
  mutate(across(all_of(variabili_da_controllare), ~ if (n_distinct(.) > 1) moda(.) else first(.))) %>% 
  ungroup()

###TYPE_LOAN, NUM_LOAN###
data_new$NUM_LOAN <- gsub("_", "", data_new$NUM_LOAN)
data_new$NUM_LOAN <- as.numeric(data_new$NUM_LOAN)
data_new <- data_new %>%
  mutate(
    numero_virgole = if_else(
      !is.na(TYPE_LOAN),
      str_count(TYPE_LOAN, ",")+1,
      0
    )
  )
risultato1 <- data_new$NUM_LOAN == data_new$numero_virgole
numero_false <- sum(!risultato1)
righe_false <- data_new[!risultato1, ]
data_new$NUM_LOAN <- data_new$numero_virgole

###DELAY_DAYS###
data_new$DELAY_DAYS <- gsub("_", "", data_new$DELAY_DAYS)
data_new$DELAY_DAYS <- as.numeric(data_new$DELAY_DAYS)
data_new$DELAY_DAYS <- cut(data_new$DELAY_DAYS, 
                           breaks = c(-5, 7, 14, 21, 28, Inf),
                           labels = c("First Week", "Second Week", 
                                      "Third Week", "Fourth Week", "Plus one month"),
                           right = TRUE)

###NUM_DELAY_PAYMENTS###
data_new$NUM_DELAY_PAYMENTS <- gsub("_", "", data_new$NUM_DELAY_PAYMENTS)
data_new$NUM_DELAY_PAYMENTS <- as.numeric(data_new$NUM_DELAY_PAYMENTS)
n <- dim(data_new)[1]
for (i in 1:n){
  data_new$NUM_DELAY_PAYMENTS[i] <- ifelse (data_new$NUM_DELAY_PAYMENTS[i]<0 | data_new$NUM_DELAY_PAYMENTS[i]>30, NA, data_new$NUM_DELAY_PAYMENTS[i])
}

###CHANGE_CREDIT_LIMIT###
data_new$CHANGE_CREDIT_LIMIT <- as.numeric(data_new$CHANGE_CREDIT_LIMIT)
out_CCL <- boxplot.stats(data_new$CHANGE_CREDIT_LIMIT)$out
which_rows_CCL <- which(data_new$CHANGE_CREDIT_LIMIT %in% c(out_CCL))
#outlier individuati considerando la differenza interquartile: I=[q0.25 - 1.5*IQR; q0.75 + 1.5*IQR]

###NUM_CREDIT_INQUIRIES###
val=which(data_new$NUM_CREDIT_INQUIRIES > 100) #sono 1648
data_nuovo=data[val,]
data_new$NUM_CREDIT_INQUIRIES <- gsub("_", "", data_new$NUM_CREDIT_INQUIRIES)
data_new$NUM_CREDIT_INQUIRIES=as.numeric(data_new$NUM_CREDIT_INQUIRIES) 
for (i in 1:100000){
  data_new$NUM_CREDIT_INQUIRIES[i] <- ifelse (data_new$NUM_CREDIT_INQUIRIES[i]>100, NA, data_new$NUM_CREDIT_INQUIRIES[i])
}  
vet <- numeric(0)
for (persona in (1:12500)){
  ds <- data_new$NUM_CREDIT_INQUIRIES[((persona-1)*8+1):(persona*8)]
  new_value=impute(ds)
  vet <- c(vet, new_value)
}
data_new$NUM_CREDIT_INQUIRIES<- vet
data_new$NUM_CREDIT_INQUIRIES <- as.integer(data_new$NUM_CREDIT_INQUIRIES)
out_num_credit_inq=boxplot.stats(data_new$NUM_CREDIT_INQUIRIES)$out

###CREDIT MIX###
data_new$CREDIT_MIX <- as.factor(data_new$CREDIT_MIX)

###OUTSTANDING DEBT###
data_new$OUTSTANDING_DEBT <- gsub("_", "", data_new$OUTSTANDING_DEBT)
data_new$OUTSTANDING_DEBT <- as.numeric(data_new$OUTSTANDING_DEBT)

###UTILIZATION_RATIO###
data_new$UTILIZATION_RATIO <- as.numeric(data_new$UTILIZATION_RATIO)
out_UR <- boxplot.stats(data_new$UTILIZATION_RATIO)$out

###CREDIT_AGE###
CREDIT_AGE_NEW <- c()
for (i in 1:n){
  CREDIT_AGE_NEW[i] <- substr(data_new$CREDIT_AGE[i], 1,2)
}
data_new$CREDIT_AGE <- as.numeric(CREDIT_AGE_NEW)
vet <- numeric(0)
for (persona in 1:12500){
  ds <- data_new$CREDIT_AGE[((persona-1)*8+1):(persona*8)]
  new_value=impute(ds)
  vet <- c(vet, new_value)
}
data_new$CREDIT_AGE <- vet
#controllo che l'eta creditizia sia minore dell'età
risultato <- data_new$CREDIT_AGE-1 <= data_new$AGE
righe_false <- data_new[!risultato, ]
numero_false <- sum(!risultato)/8
data_new$CREDIT_AGE <- as.integer(data_new$CREDIT_AGE)

###MINIMUM_AMOUNT###
data_new$MINIMUM_AMOUNT <- gsub("NM", "No", data_new$MINIMUM_AMOUNT)
data_new$MINIMUM_AMOUNT <- as.factor(data_new$MINIMUM_AMOUNT)

###MONTHLY_EMI###
data_new$gruppo <- rep(1:ceiling(nrow(data_new) / 8), each = 8)[1:nrow(data_new)]
data_new$uguali <- ave(data_new$MONTHLY_EMI, data_new$gruppo, FUN = function(x) length(unique(x)) == 1)
calcola_moda <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
data_new$gruppo <- rep(1:ceiling(nrow(data_new) / 8), each = 8)[1:nrow(data_new)]
data_new$MONTHLY_EMI <- ave(data_new$MONTHLY_EMI, data_new$gruppo, FUN = calcola_moda)
data_new$gruppo <- NULL
out_ME <- boxplot.stats(data_new$MONTHLY_EMI)$out
which_rows_ME <- which(data_new$MONTHLY_EMI %in% c(out_ME))

###MONTHLY_INVESTMENT###
out_MI <- boxplot.stats(data_new$MONTHLY_INVESTMENT)$out
which_rows_MI <- which(data_new$MONTHLY_INVESTMENT %in% c(out_MI))

###PAYMENT_BEHAVIOR###
data_new$PAYMENT_BEHAVIOR <- as.factor(data_new$PAYMENT_BEHAVIOR)

###MONTHLY_BALANCE###
data_new$MONTHLY_BALANCE <- as.numeric(data_new$MONTHLY_BALANCE)
out_MB <- boxplot.stats(data_new$MONTHLY_BALANCE)$out
which_rows_MB <- which(data_new$MONTHLY_BALANCE %in% c(out_MB))


# ---- 1.2) Individuazione e rimozione degli outlier --------------------------------

##Outlier complessivi:
which_rows <- c(which_rows_CCL, which_rows_ME, which_rows_MI, which_rows_MB, which_rows_AI)
mt_one_outlier <- as.numeric(names(table(which_rows)[table(which_rows) >= 1]))
#print(mt_one_outlier) #18855
##Verifiche sugli outlier:
a=table(data_new$OCCUPATION)
data_nuovo <- data.frame(data_new[mt_one_outlier,])
df_univoco <- data_nuovo%>%distinct(CUSTOMER_ID, .keep_all = T)
#table(df_univoco$OCCUPATION)/a *100

df_univoco1 <- data_nuovo%>%distinct(CUSTOMER_ID, .keep_all = T)
b <- table(df_univoco1$AGE)/table(data_new$AGE) *100

#Rimozione delle righe contenenti outlier:
data_new <- data_new[-mt_one_outlier,]

#Rimozione di "TYPE_LOAN", "CREDIT_AGE", "numero_virgole" e "uguali":
data_new <- data_new[, -c(11,19,26,27)]


# ---- 1.3) Imputazione dei dati mancanti tramite 'mice' ----------------------------

covdata <- data_new[, -23] #esclusione di "CREDIT_SCORE" per l'imputazione

#sapply(covdata, function(x)(sum(is.na(x))))
#variabili con NA:
## MONTHLY_SALARY
## DELAY_DAYS
## NUM_DELAY_PAYMENTS
## CHANGE_CREDIT_LIMIT
## CREDIT_MIX
## MONTHLY_INVESTMENT
## PAYMENT_BEHAVIOR
## MONTHLY_BALANCE

imputazione <- suppressMessages(suppressWarnings(
  mice(covdata, m=5, maxit=5, meth='cart', seed=500, printFlag=FALSE)
))

covdata_imputato <- complete(imputazione, 1)
dataset_imputato <- cbind(covdata_imputato, data_new$CREDIT_SCORE)
sapply(dataset_imputato, function(x)(sum(is.na(x))))

write.xlsx(dataset_imputato, "dataset_imputato.xlsx")


# ---- 1.4) Analisi dei valori di correlazione -----------------------------------------------

ds <- read.xlsx("dataset_imputato.xlsx")
#dim(ds) #81145 23
ds$MONTH <- as.factor(ds$MONTH)
ds$OCCUPATION <- as.factor(ds$OCCUPATION)
ds$DELAY_DAYS <- as.factor(ds$DELAY_DAYS)
ds$CREDIT_MIX <- as.factor(ds$CREDIT_MIX)
ds$MINIMUM_AMOUNT <- as.factor(ds$MINIMUM_AMOUNT)
ds$PAYMENT_BEHAVIOR <- as.factor(ds$PAYMENT_BEHAVIOR)
ds$CREDIT_SCORE <- as.factor(ds$`data_new$CREDIT_SCORE`)
ds <- ds[, -23]

b_numeric <- ds%>% dplyr::select_if(is.numeric)
cor_matrix <- cor(b_numeric)
corrplot(cor_matrix, method = "circle", type = "upper", tl.cex = 0.9, tl.col = "Red")

#Rimozione di "MONTHLY_SALARY":
ds = ds[, -6]

b_fac <- ds%>% dplyr::select_if(is.factor)
#b_fac
combos <- combn(ncol(b_fac),2)
adply(combos, 2, function(x) {
  test <- chisq.test(b_fac[, x[1]], b_fac[, x[2]])
  tab  <- table(b_fac[, x[1]], b_fac[, x[2]])
  out <- data.frame("Row" = colnames(b_fac)[x[1]]
                    , "Column" = colnames(b_fac[x[2]])
                    , "Chi.Square" = round(test$statistic,3)
                    , "df"= test$parameter
                    , "p.value" = round(test$p.value, 3)
                    , "n" = sum(table(b_fac[,x[1]], b_fac[,x[2]]))
                    , "u1" =length(unique(b_fac[,x[1]]))-1
                    , "u2" =length(unique(b_fac[,x[2]]))-1
                    , "nMinu1u2" =sum(table(b_fac[,x[1]], b_fac[,x[2]]))* min(length(unique(b_fac[,x[1]]))-1 , length(unique(b_fac[,x[2]]))-1) 
                    , "Chi.Square norm"  =test$statistic/(sum(table(b_fac[,x[1]], b_fac[,x[2]]))* min(length(unique(b_fac[,x[1]]))-1 , length(unique(b_fac[,x[2]]))-1)) 
  )
  return(out)
})
##nessuna coppia di variabili qualitative presenta valore della statistica test maggiore di 0.8 quindi non eliminiamo alcuna covariata dal modello

set.seed(123)
data <- ds %>%
  group_by(CUSTOMER_ID) %>%
  slice_sample(n = 1) %>%
  ungroup()


# ---- 1.5) Model selection ---------------------------------------------------------

set.seed(123)
cvCtrl <- trainControl(method = "cv", number = 10, search="grid", classProbs = TRUE, summaryFunction = multiClassSummary, verboseIter = TRUE)
rfTune <- train(CREDIT_SCORE ~ MONTH + AGE + OCCUPATION + ANNUAL_INCOME + NUM_BANK_ACCOUNTS + NUM_CREDIT_CARD + INTEREST_RATE + NUM_LOAN + DELAY_DAYS + NUM_DELAY_PAYMENTS + CHANGE_CREDIT_LIMIT + NUM_CREDIT_INQUIRIES + CREDIT_MIX + OUTSTANDING_DEBT + UTILIZATION_RATIO + MINIMUM_AMOUNT + MONTHLY_EMI + MONTHLY_INVESTMENT + PAYMENT_BEHAVIOR + MONTHLY_BALANCE, data = data, method = "rf", tuneLength = 10, trControl = cvCtrl, ntree = 500)
rfTune

vimp = varImp(rfTune)

varImpData <- as.data.frame(varImp(rfTune)$importance)
varImpData$Variable <- rownames(varImpData)
ggplot(varImpData, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "blue") +
  geom_hline(yintercept = 30, col = "red", linetype = "dashed", lwd = 1) +
  coord_flip() +
  labs(title = "Variable Importance", x = "Variables", y = "Importance") +
  theme(axis.text.y = element_text(size = 12, color = "black"))

vimp = data.frame(vimp[1])
vimp$var = row.names(vimp)

var_imp = vimp[vimp$Overall>30, ]
var_imp

data_def <- data[, c(5, 8, 12, 13, 14, 15, 16, 18, 19, 21, 22)]
str(data_def)

write.xlsx(data_def, "data_def.xlsx")
##Fine Pre-Processing

data_def <- read.xlsx("data_def.xlsx")
data_def$CREDIT_MIX <- as.factor(data_def$CREDIT_MIX)
data_def$CREDIT_SCORE <- as.factor(data_def$CREDIT_SCORE)
#str(data_def)
table(data_def$CREDIT_SCORE)



# ==== 2) Implementazione degli algoritmi di clustering ---------------------------


# ---- 2.1) Algoritmo: ITERATIVE DISTANCE-BASED -----------------------------------
data_def <- read.xlsx("data_def.xlsx")
str(data_def)
data_def$CREDIT_MIX <- as.factor(data_def$CREDIT_MIX)
data_def$CREDIT_SCORE <- as.factor(data_def$CREDIT_SCORE)
#Per includere la variabile fattoriale *CREDIT_MIX* è necessario trasformarla in una variabile di tipo dummy; le variabili dummy così create devono essere successivamente standardizzate:
ds_hc <- data_def
modello_var_dummy <- dummyVars(~ CREDIT_MIX, data = ds_hc, sep = ".")
var_dummy <- predict(modello_var_dummy, newdata = ds_hc)
ds_hc <- cbind(ds_hc[, !colnames(ds_hc) %in% "CREDIT_MIX"], var_dummy)
str(ds_hc)

#Escludo la variabile target *CREDIT_SCORE*:
data_cluster <- ds_hc[, -10]
str(data_cluster)

#Standardizzazione di tutte le variabili del dataset *data_cluster*:
data_cluster <- data.frame(scale(data_cluster))

# Crea il boxplot
data_def_st1 <- data_cluster[,-c(10,11,12)]
library(tidyr)
data_long <- gather(data_def_st1, key = "Variable", value = "Value")

ggplot(data_long, aes(x = Variable, y = Value)) +
  geom_boxplot(fill = "lightblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  labs( x = NULL, y = NULL)   

#K-MEANS
set.seed(123)
# Metodo del Gomito per K-Means
fviz_nbclust(data_cluster, kmeans, method = "wss") + 
  ggtitle("Metodo del Gomito per K-Means")

# Metodo della Silhouette per K-Means
fviz_nbclust(data_cluster, kmeans, method = "silhouette") + 
  ggtitle("Metodo della Silhouette per K-Means")

set.seed(123)
k_means_opt <- kmeans(data_cluster, centers = 3, nstart = 25)

table(k_means_opt$cluster, data_def$CREDIT_SCORE)

table_kmeans <- table(k_means_opt$cluster, data_def$CREDIT_SCORE)

# Funzione per calcolare la purezza
purity <- sum(apply(table_kmeans, 1, max)) / length(data_def$CREDIT_SCORE)
purity

# Calcola l'indice di silhouette
sil <- silhouette(k_means_opt$cluster, dist(data_cluster))

summary(sil)
mean(sil[,3])

tab_conf <- table(k_means_opt$cluster, data_def$CREDIT_SCORE)
tab_conf_percent <- prop.table(tab_conf, margin = 1) * 100
colors <- c("navy", "gold", "red1")
par(mfrow = c(1, 3))
clusters <- rownames(tab_conf_percent)
categories <- colnames(tab_conf_percent)
for (i in 1:nrow(tab_conf_percent)) {
  labels <- paste(round(tab_conf_percent[i, ], 1), "%")
  pie(
    tab_conf_percent[i, ],
    labels = labels,
    col = colors,
    clockwise = TRUE,
    cex = 1.2
  )
  title(main = paste("Cluster", clusters[i]), line = -6.5)
}
legend("topright", legend = categories, fill = colors, title = "Categorie")


custom_colors <- c('#cc476a', '#3f9822', '#0082ce')
par(mfrow=c(1,1))
par(mar = c(5, 5, 4, 7)) 
plot(data_cluster[, 5], data_cluster[, 2],
     col = custom_colors[k_means_opt$cluster], 
     pch = 1,                  
     xlab = "OUTSTANDING_DEBT",       
     ylab = "INTEREST_RATE" )      

plot(data_cluster[, 5], data_cluster[, 6],
     col = custom_colors[k_means_opt$cluster],  
     pch = 1,                   
     xlab = "OUTSTANDING_DEBT",       
     ylab = "UTILIZATION_RATIO",      
     xpd = TRUE)


legend("topright", 
       legend = paste("Cluster", 1:length(unique(k_means_opt$cluster))),  
       col = unique(custom_colors[k_means_opt$cluster]), 
       pch = 19,  
       title = "Cluster",
       xpd = TRUE,  
       inset = c(-0.4, 0))  

####K-MEDIAN###
set.seed(123)
pam_opt <- pam(data_cluster, k = 3)

table(pam_opt$clustering, data_def$CREDIT_SCORE)

table_kmedian <- table(pam_opt$clustering, data_def$CREDIT_SCORE)

purity <- sum(apply(table_kmedian, 1, max)) / length(data_def$CREDIT_SCORE)
purity

sil <- silhouette(pam_opt$clustering, dist(data_cluster))

summary(sil)
mean(sil[,3])

tab_conf <- table(pam_opt$clustering, data_def$CREDIT_SCORE)
tab_conf_percent <- prop.table(tab_conf, margin = 1) * 100
colors <- c("navy", "gold", "red1")
par(mfrow = c(1, 3))
clusters <- rownames(tab_conf_percent)
categories <- colnames(tab_conf_percent)
for (i in 1:nrow(tab_conf_percent)) {
  labels <- paste(round(tab_conf_percent[i, ], 1), "%")
  pie(
    tab_conf_percent[i, ],
    labels = labels,
    col = colors,
    clockwise = TRUE,
    cex = 1.2
  )
  title(main = paste("Cluster", clusters[i]), line = -6.5)
}
legend("topright", legend = categories, fill = colors, title = "Categorie")


custom_colors <- c('#cc476a', '#3f9822', '#0082ce')
par(mfrow=c(1,1))
par(mar = c(5, 5, 4, 7)) 
plot(data_cluster[, 5], data_cluster[, 2],
     col = custom_colors[pam_opt$clustering],  
     pch = 1,                  
     xlab = "OUTSTANDING_DEBT",       
     ylab = "INTEREST_RATE")




plot(data_cluster[, 5], data_cluster[, 6],
     col = custom_colors[pam_opt$clustering],  
     pch = 1,                  
     xlab = "OUTSTANDING_DEBT",       
     ylab = "UTILIZATION_RATIO")


legend("topright", 
       legend = paste("Cluster", 1:length(unique(pam_opt$clustering))),  
       col = unique(custom_colors[pam_opt$clustering]),  
       pch = 1,  
       title = "Cluster",
       xpd = TRUE,  
       inset = c(-0.4, 0))


# ---- 2.2) Algoritmo: CLUSTERING GERARCHICO, APPROCCIO BOTTOM-UP -----------------
#Per includere la variabile fattoriale *CREDIT_MIX* è necessario trasformarla in una variabile di tipo dummy; le variabili dummy così create devono essere successivamente standardizzate:
ds_hc <- data_def
modello_var_dummy <- dummyVars(~ CREDIT_MIX, data = ds_hc, sep = ".")
var_dummy <- predict(modello_var_dummy, newdata = ds_hc)
ds_hc <- cbind(ds_hc[, !colnames(ds_hc) %in% "CREDIT_MIX"], var_dummy)
#Escludo la variabile target *CREDIT_SCORE*:
ds_hc <- ds_hc[, -10]
#Standardizzazione di tutte le variabili del dataset *ds_hc*:
ds_hc <- data.frame(scale(ds_hc))
metodi <- c("ward.D", "single", "complete", "average", "centroid")
risultati <- list()
for (metodo in metodi) {
  m_dist <- dist(ds_hc, method = "euclidean")
  hc <- hclust(m_dist, method = metodo)
  cluster <- cutree(hc, k = 3)
  plot(hc)
  rect.hclust(hc, k = 3, border = 2:6)
  
  silhouette_values <- silhouette(cluster, m_dist)
  silhouette_mean <- mean(silhouette_values[, 3])
  
  confusion_matrix <- table(cluster, data_def$CREDIT_SCORE)
  
  risultati[[metodo]] <- list(silhouette_mean = silhouette_mean, confusion_matrix = confusion_matrix)
  
  cat("\nMetodo:", metodo, "\n")
  print(confusion_matrix)
}
print(risultati)

calculate_purity <- function(confusion_matrix) {
  total_correct <- sum(apply(confusion_matrix, 1, max))
  total_observations <- sum(confusion_matrix)
  purity <- total_correct / total_observations
  return(purity)
}

for (metodo in names(risultati)) {
  cat("\nMetodo:", metodo, "\n")
  confusion_matrix <- risultati[[metodo]]$confusion_matrix
  purity <- calculate_purity(confusion_matrix)
  cat("Purity Score:", round(purity, 4), "\n")
}

metodi <- c("ward.D", "single", "complete", "average", "centroid")
risultati <- list()
for (metodo in metodi) {
  m_dist <- dist(ds_hc, method = "maximum")
  hc <- hclust(m_dist, method = metodo)
  cluster <- cutree(hc, k = 3)
  plot(hc)
  rect.hclust(hc, k = 3, border = 2:6)
  
  silhouette_values <- silhouette(cluster, m_dist)
  silhouette_mean <- mean(silhouette_values[, 3])
  
  confusion_matrix <- table(cluster, data_def$CREDIT_SCORE)
  
  risultati[[metodo]] <- list(silhouette_mean = silhouette_mean, confusion_matrix = confusion_matrix)
  
  cat("\nMetodo:", metodo, "\n")
  print(confusion_matrix)
}
print(risultati)

calculate_purity <- function(confusion_matrix) {
  total_correct <- sum(apply(confusion_matrix, 1, max))
  total_observations <- sum(confusion_matrix)
  purity <- total_correct / total_observations
  return(purity)
}

for (metodo in names(risultati)) {
  cat("\nMetodo:", metodo, "\n")
  confusion_matrix <- risultati[[metodo]]$confusion_matrix
  purity <- calculate_purity(confusion_matrix)
  cat("Purity Score:", round(purity, 4), "\n")
}

metodi <- c("ward.D", "single", "complete", "average", "centroid")
risultati <- list()
for (metodo in metodi) {
  m_dist <- dist(ds_hc, method = "manhattan")
  hc <- hclust(m_dist, method = metodo)
  cluster <- cutree(hc, k = 3)
  plot(hc)
  rect.hclust(hc, k = 3, border = 2:6)
  
  silhouette_values <- silhouette(cluster, m_dist)
  silhouette_mean <- mean(silhouette_values[, 3])
  
  confusion_matrix <- table(cluster, data_def$CREDIT_SCORE)
  
  risultati[[metodo]] <- list(silhouette_mean = silhouette_mean, confusion_matrix = confusion_matrix)
  
  cat("\nMetodo:", metodo, "\n")
  print(confusion_matrix)
}
print(risultati)

calculate_purity <- function(confusion_matrix) {
  total_correct <- sum(apply(confusion_matrix, 1, max))
  total_observations <- sum(confusion_matrix)
  purity <- total_correct / total_observations
  return(purity)
}

for (metodo in names(risultati)) {
  cat("\nMetodo:", metodo, "\n")
  confusion_matrix <- risultati[[metodo]]$confusion_matrix
  purity <- calculate_purity(confusion_matrix)
  cat("Purity Score:", round(purity, 4), "\n")
}

metodi <- c("ward.D", "single", "complete", "average", "centroid")
risultati <- list()
for (metodo in metodi) {
  m_dist <- dist(ds_hc, method = "canberra")
  hc <- hclust(m_dist, method = metodo)
  cluster <- cutree(hc, k = 3)
  plot(hc)
  rect.hclust(hc, k = 3, border = 2:6)
  
  silhouette_values <- silhouette(cluster, m_dist)
  silhouette_mean <- mean(silhouette_values[, 3])
  
  confusion_matrix <- table(cluster, data_def$CREDIT_SCORE)
  
  risultati[[metodo]] <- list(silhouette_mean = silhouette_mean, confusion_matrix = confusion_matrix)
  
  cat("\nMetodo:", metodo, "\n")
  print(confusion_matrix)
}
print(risultati)

calculate_purity <- function(confusion_matrix) {
  total_correct <- sum(apply(confusion_matrix, 1, max))
  total_observations <- sum(confusion_matrix)
  purity <- total_correct / total_observations
  return(purity)
}

for (metodo in names(risultati)) {
  cat("\nMetodo:", metodo, "\n")
  confusion_matrix <- risultati[[metodo]]$confusion_matrix
  purity <- calculate_purity(confusion_matrix)
  cat("Purity Score:", round(purity, 4), "\n")
}

metodi <- c("ward.D", "single", "complete", "average", "centroid")
risultati <- list()
for (metodo in metodi) {
  m_dist <- dist(ds_hc, method = "minkowski")
  hc <- hclust(m_dist, method = metodo)
  cluster <- cutree(hc, k = 3)
  plot(hc)
  rect.hclust(hc, k = 3, border = 2:6)
  
  silhouette_values <- silhouette(cluster, m_dist)
  silhouette_mean <- mean(silhouette_values[, 3])
  
  confusion_matrix <- table(cluster, data_def$CREDIT_SCORE)
  
  risultati[[metodo]] <- list(silhouette_mean = silhouette_mean, confusion_matrix = confusion_matrix)
  
  cat("\nMetodo:", metodo, "\n")
  print(confusion_matrix)
}
print(risultati)

calculate_purity <- function(confusion_matrix) {
  total_correct <- sum(apply(confusion_matrix, 1, max))
  total_observations <- sum(confusion_matrix)
  purity <- total_correct / total_observations
  return(purity)
}

for (metodo in names(risultati)) {
  cat("\nMetodo:", metodo, "\n")
  confusion_matrix <- risultati[[metodo]]$confusion_matrix
  purity <- calculate_purity(confusion_matrix)
  cat("Purity Score:", round(purity, 4), "\n")
}

#La combinazione migliore, rispetto al valore degli indici Purity e Silhouette, è risultata essere: distanza "Manhattan" + metodo di linkage "ward.D".
m_dist <- dist(ds_hc, method = "manhattan")
hc <- hclust(m_dist, method = "ward.D")
cluster <- cutree(hc, k = 3)
k = 3
h <- hc$height[length(hc$height) - (k - 1)]
h1 <- hc$height[length(hc$height) - (k - 2)]
plot(hc, labels = FALSE)
abline(h = h, col = "violet", lty = 2)
abline(h = h1, col = "violet", lty = 2)
rect.hclust(hc, k = 3, border = 2:6)

ds_hc <- mutate(ds_hc, cluster = cluster)
num_oss <- count(ds_hc, cluster)
num_oss


tab_conf <- table(cluster, data_def$CREDIT_SCORE)
tab_conf_percent <- prop.table(tab_conf, margin = 1) * 100
colors <- c("navy", "gold", "red1")
par(mfrow = c(1, 3))
clusters <- rownames(tab_conf_percent)
categories <- colnames(tab_conf_percent)
for (i in 1:nrow(tab_conf_percent)) {
  labels <- paste(round(tab_conf_percent[i, ], 1), "%")
  pie(
    tab_conf_percent[i, ],
    labels = labels,
    col = colors,
    clockwise = TRUE,
    cex = 1.2
  )
  title(main = paste("Cluster", clusters[i]), line = -6.5)
}
legend("topright", legend = categories, fill = colors, title = "Categorie")

plot(data_cluster[, 5], data_cluster[, 6],
     col = custom_colors[k_means_opt$cluster],  
     pch = 1,                   
     xlab = "OUTSTANDING_DEBT",       
     ylab = "UTILIZATION_RATIO",      
     xpd = TRUE)
legend("topright", 
       legend = paste("Cluster", 1:length(unique(k_means_opt$cluster))),  # Etichette dei cluster
       col = unique(custom_colors[k_means_opt$cluster]),  # Colori associati ai cluster
       pch = 19,  
       title = "Cluster",
       xpd = TRUE,  
       inset = c(-0.4, 0))


# ---- 2.3) Algoritmo: SOFT CLUSTERING --------------------------------------------
#DBscan
data_def<-read_excel("data_def.xlsx")
str(data_def)

#Per includere la variabile fattoriale CREDIT_MIX è necessario trasformarla in una variabile di tipo dummy; le variabili dummy così create devono essere successivamente standardizzate:
ds_hc <- data_def
modello_var_dummy <- dummyVars(~ CREDIT_MIX, data = ds_hc, sep = ".")
var_dummy <- predict(modello_var_dummy, newdata = ds_hc)
ds_hc <- cbind(ds_hc[, !colnames(ds_hc) %in% "CREDIT_MIX"], var_dummy)
str(ds_hc)

#Escludo la variabile target CREDIT_SCORE:
ds_hc <- ds_hc[, -10]
str(ds_hc)

#prendo solo variabili numeriche
data_num <- ds_hc%>% dplyr::select_if(is.numeric)
data_def_st <- data.frame(scale(data_num))
distances <- dist(data_def_st) #matrice distanze
#sorted_distances <- sort(distances, decreasing = TRUE) #riordino in modo decrescente le distanze

#determinare epsilon, raggio da prendere in considerazione 
par(mfrow=c(1,1))
kNNdistplot(data_def_st, k = 20) #dove inizia a salire rapidamente? dove c'è il gomito?

# plot(sorted_distances, 
#      type = "l", 
#      col = "blue",
#      main = "kNN Distance Plot", 
#      xlab = "Points sorted by distance", 
#      ylab = paste("Distance to", k, "-th nearest neighbor"))

abline(h=c(1.80, 2.05),  col='red', lty=3)#scelgo questo
eps=2.05
abline(h=eps, col='dodgerblue', lty=1, lwd=2)
#algoritmo
#attenzione a scegliere minpts! faccio più tentativi, di norma dimensionalità +1


#https://sefidian.com/2022/12/18/how-to-determine-epsilon-and-minpts-parameters-of-dbscan-clustering/

point=seq(13,22)
for (pt in point){
  dbscanprova<-dbscan(data_def_st, eps=eps, minPts=pt) 
  cluster=dbscanprova$cluster
  cl0<-which(cluster==0)
  cl1<-which(cluster==1)
  cl2<-which(cluster==2)
  cl3<-which(cluster==3)
  
  aa=data_def[cl0,]
  bb=data_def[cl1,]
  cc=data_def[cl2,]
  dd=data_def[cl3,]
  tabella=rbind(table(aa$CREDIT_SCORE),
                table(bb$CREDIT_SCORE),
                table(cc$CREDIT_SCORE),
                table(dd$CREDIT_SCORE))
  #somma_righe=rowsum(tabella)
  #tabella=cbind(tabella, somma_righe)
  #rownames(tabella)=c('cluster0', 'cluster1', 'cluster2', 'cluster3' )
  tabella.perc=round(prop.table(tabella, margin=1)*100, 2)
  print(pt)
  print(cbind(tabella,tabella.perc))
  
}

dbscan13<-dbscan(data_def_st, eps=eps, minPts=13) 
cluster13=dbscan13$cluster 
unique(cluster13) #trovo 3 cluster
summary(dbscan13)
#out
set.seed(123)
dbscan13$eps; cluster=dbscan13$cluster; (dist=dbscan13$dist); dbscan13$borderPoints
summary(dbscan13)
cl0<-which(cluster==0)
cl1<-which(cluster==1)
cl2<-which(cluster==2)
cl3<-which(cluster==3)

aa=data_def[cl0,];dim(aa)
bb=data_def[cl1,];dim(bb)
cc=data_def[cl2,];dim(cc)
dd=data_def[cl3,];dim(dd)

tabella1=rbind(table(aa$CREDIT_SCORE),
               table(bb$CREDIT_SCORE),
               table(cc$CREDIT_SCORE),
               table(dd$CREDIT_SCORE))
rownames(tabella1)=c('noise points', 'cluster1', 'cluster2', 'cluster3')

tot_ptcluster<- rowSums(tabella1)
(tabella_valassoluti=cbind(tabella1, tot_ptcluster))
tabella1

#                 Good Poor Standard tot_ptcluster
# noise points    6    2        8            16
# cluster1       39 1642     1034          2715
# cluster2      285 1190     3913          5388
# cluster3     1538  509     1091          3138

tabella_perc=round(prop.table(tabella1[-1, ], 1)*100,2)
tabella_perc

eps

# Good  Poor Standard
# cluster1  1.44 60.48    38.08
# cluster2  5.29 22.09    72.62
# cluster3 49.01 16.22    34.77



# Rappresentazioni grafiche
# rosa->poor
# standard->verde
# azzurro->good

par(mfrow = c(1, 3))
colors <- c("navy", "gold", "red1")
clusters <- rownames(tabella_perc)
categories <- colnames(tabella_perc)
for (i in 1:3) {
  labels <- paste(round(tabella_perc[i, ], 1), "%")
  pie(
    tabella_perc[i, ],
    labels = labels,
    col = colors,
    clockwise = TRUE,
    cex = 1.2
  )
  title(main = paste( clusters[i]), line = -6.8)
}

legend("topright",               
       legend = categories,       
       fill = colors,             
       title = "Cluster",                
       border = "white" )

par(mfrow = c(1, 1))

custom_colors=c('black','#cc476a','#3f9822', "#0082ce")
plot(data_def$OUTSTANDING_DEBT, data_def$INTEREST_RATE, 
     col = custom_colors[cluster+1],  
     pch = 1,               
     xlab = "Outstanding Debt",
     ylab = "Interest Rate")
points(aa$OUTSTANDING_DEBT, aa$INTEREST_RATE, col = "black", pch = 19)

plot(data_def$OUTSTANDING_DEBT, data_def$UTILIZATION_RATIO, 
     col = custom_colors[cluster+1],  
     pch = 1,                              
     xlab = "Outstanding Debt",
     ylab = "Utilization Ratio")
points(aa$OUTSTANDING_DEBT, aa$UTILIZATION_RATIO, col = "black", pch = 19)


legend("bottomright", 
       legend = c("Noise points", "Cluster 1", "Cluster 2", "Cluster 3"), 
       col = custom_colors,  
       pch = 16,  
       title = "Cluster")

# Calcolare la silhouette-> snp=senza noise points
cluster_snp=cluster[cluster!=0]
data_snp=data_def_st[cluster!=0,]
distances_snp=dist(data_snp)
sil_snp=silhouette(cluster_snp, distances_snp)
summary(sil_snp)
plot(sil_snp)
mean(sil_snp[,3])
#0.2815

# Calcolare la purezza-> tolgo i noise points dalla tabella
purezza_snp = (sum(apply(tabella1[-1,], 1, max)) / (length(data_def$CREDIT_SCORE)-16)); purezza_snp
#0.6310



# ==== 3) Implementazione degli algoritmi di classificazione ---------------------------


# ---- 3.1) Algoritmo: KNN --------------------------------------------------------
data_def<-read.xlsx("data_def.xlsx")
data_def$CREDIT_MIX <- as.factor(data_def$CREDIT_MIX)
data_def$CREDIT_SCORE <- as.factor(data_def$CREDIT_SCORE)
data_num <- data_def%>% dplyr::select_if(is.numeric)
data_def_st <- data.frame(scale(data_num))# CREDIT_MIX=data_def$CREDIT_MIX, CREDIT_SCORE)

#trovo train e validation data
set.seed(123)
split <- createDataPartition(y = data_def$CREDIT_SCORE, p = 0.20, list = FALSE)
dataset <- data_def_st[-split, ]
test <- data_def_st[split, ]

#provo a vedere in base ai k che errori ho
calcola_errori = function(actual, predicted) { mean(actual != predicted) } #creo funzione per calcolare in media quanti errori faccio in una classificazione
#actual=quelli veri, predicted=quelli che prevedo io con il 

k_prova=1:200
k_errori = numeric()
for (i in k_prova) {
  pred = knn(train =data.frame(dataset), test =data.frame(test), cl = data_def$CREDIT_SCORE[-split], k = k_prova[i]) #lavora/allena su training
  k_errori[i] = calcola_errori(as.factor(data_def$CREDIT_SCORE[split]), pred)  #questa fa previsioni sul test
  
}

#visualizzazione grafica:
par(mfrow=c(1,1)) 
plot(k_errori, type = "b", col = "black", cex = 1, pch = 20,
     xlab = "k, number of neighbors", ylab = "Test error")
abline(h = min(k_errori), col = "dodgerblue", lty = 3)

min_err=min(k_errori)
k=which.min(k_errori); k#Valore di k che fornisce l'errore minimo K=31
which.max(k_errori)#Valore di k che fornisce l'errore massimo K=1
points(k, min_err, pch=19, col='dodgerblue')

mod_knn = knn(train =data.frame(dataset), test =data.frame(test), cl = data_def$CREDIT_SCORE[-split], k = k)
conf_mat=confusionMatrix(data = mod_knn, reference = as.factor(data_def$CREDIT_SCORE[split]), positive = "1") 
conf_mat$table

#stessa cosa ma manualmente:
# matr_conf= table(pred58, as.factor(data_def$CREDIT_SCORE[split]))
# matr_conf
# accuracy = function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}; accuracy(matr_conf)


#calcolo la media degli errori
erroremedio31 = calcola_errori(actual = as.factor(data_def$CREDIT_SCORE[split]), predicted = mod_knn)
round(erroremedio31*100,2)


# ---- 3.2) Algoritmo: SVM --------------------------------------------------------
set.seed(123)
data_def <- read.xlsx("data_def.xlsx")
str(data_def)
data_def$CREDIT_MIX <- as.factor(data_def$CREDIT_MIX)
data_def$CREDIT_SCORE <- as.factor(data_def$CREDIT_SCORE)
                                   
set.seed(123)
split <- createDataPartition(y = data_def$CREDIT_SCORE, p = 0.20, list = FALSE)
dataset <- data_def[-split, ]
test <- data_def[split, ]

dataset_num <- dataset%>% dplyr::select_if(is.numeric)
test_num <- test%>% dplyr::select_if(is.numeric)
mean <- apply(dataset_num, 2, mean)
std <- apply(dataset_num, 2, sd)
dataset_st <- data.frame(scale(dataset_num, center = mean, scale = std))
test_st <- data.frame(scale(test_num, center = mean, scale = std))
dataset_def_st <- cbind(dataset_st, CREDIT_MIX = dataset$CREDIT_MIX, CREDIT_SCORE = dataset$CREDIT_SCORE)
test_def_st <- cbind(test_st, CREDIT_MIX = test$CREDIT_MIX, CREDIT_SCORE = test$CREDIT_SCORE)

trc <- trainControl(method = "cv", number = 5) 

# 1. KERNEL LINEARE
svm_linear <- train(
  CREDIT_SCORE ~ ., 
  data = dataset_def_st, 
  method = "svmLinear",
  trControl = trc,
  tuneGrid = expand.grid(C = seq(0.1, 10, by = 0.5))
)

# Visualizzare i migliori parametri trovati
print(svm_linear$bestTune)

# Previsione sul test set
svm_linear_predictions <- predict(svm_linear, test_def_st[,-11])

# Matrice di confusione
confusionMatrix(svm_linear_predictions, test_def_st$CREDIT_SCORE)


# 2. KERNEL RADIALE
mod_svm_radial <- train(
  CREDIT_SCORE ~ ., 
  data = dataset_def_st, 
  method = "svmRadial",
  trControl = trc,
  tuneGrid = expand.grid(
    C = seq(1, 5, by = 1),  
    sigma = c(0.1, 0.5)     
  ),
  tuneLength = 5
)

# Visualizzare i migliori parametri trovati
print(mod_svm_radial$bestTune)

# Previsione sul test set
svm_predictions <- predict(mod_svm_radial, test_def_st[,-11])

# Matrice di confusione
confusionMatrix(svm_predictions, test_def_st$CREDIT_SCORE)


# ---- 3.3) Algoritmo: RANDOM FOREST ----------------------------------------------
set.seed(123)
split <- createDataPartition(y = data_def$CREDIT_SCORE, p = 0.20, list = FALSE)
dataset <- data_def[-split, ]
test <- data_def[split, ]

set.seed(123)
metric <- "Accuracy"
control <- trainControl(method = "repeatedcv", number = 10, search = "grid", classProbs = TRUE, repeats = 3)
tunegrid <- expand.grid(.mtry = c(1:5)) 
modelli <- list()
for (n_tree in c(500, 1000, 1500, 2000)){
  rf_tuned <- train(CREDIT_SCORE ~ ., 
                    data = dataset, 
                    method = "rf", 
                    metric = metric, 
                    tuneGrid = tunegrid, 
                    ntree = n_tree,
                    trControl = control)
  id <- toString(n_tree)
  modelli[[id]] <- rf_tuned
}

risultati <- data.frame(n_tree = integer(), mtry = integer(), Accuracy = numeric())

for (n_tree in names(modelli)) {
  modello <- modelli[[n_tree]]
  risultati_correnti <- modello$results
  risultati_correnti$n_tree <- as.integer(n_tree)
  risultati <- rbind(risultati, risultati_correnti)
}
print(risultati)

ggplot(risultati, aes(x = mtry, y = Accuracy, color = factor(n_tree), group = n_tree)) +
  geom_line() +
  geom_point() +
  labs(title = "Accuracy vs mtry per diversi valori di n_tree",
       x = "mtry",
       y = "Accuracy (Repeated Cross-Validation)",
       color = "ntree") +
  theme_minimal()

#Combinazione migliore:
mod_rf <- randomForest(CREDIT_SCORE ~ ., data = dataset, ntree = 2000, mtry = 2)
mod_rf
head(mod_rf$err.rate)
plot(mod_rf)
legend("topright",                    # Posizione della legenda
       legend = c("OOB Error",        # Etichette per le linee
                  "Good", 
                  "Poor", 
                  "Standard"),
       col = c("black", "red", "green", "blue"), # Colori delle linee
       lty = 1,                        # Tipo di linea (1 = linea continua)
       cex = 0.8)

target_pred = predict(mod_rf, newdata = test[, -11])
confusion_matrix <- confusionMatrix(test[, 11], target_pred)
confusion_matrix


# ---- 3.4) Algoritmo: RETI NEURALI -----------------------------------------------
data_def <- read.xlsx("data_def.xlsx")
set.seed(123)
split <- createDataPartition(y = data_def$CREDIT_SCORE, p = 0.20, list = FALSE)
dataset <- data_def[-split, ]
test <- data_def[split, ]
dim(test)
colnames(test)
ds_hc <- dataset
modello_var_dummy <- dummyVars(~ CREDIT_MIX, data = ds_hc, sep = ".")
var_dummy <- predict(modello_var_dummy, newdata = ds_hc)
ds_hc <- cbind(ds_hc[, !colnames(ds_hc) %in% "CREDIT_MIX"], var_dummy)
#str(ds_hc)
colnames(ds_hc)

#Escludo la variabile target *CREDIT_SCORE*:
y <-ds_hc[, 10]
ds_hc <- ds_hc[, -10]

#str(ds_hc)

#Standardizzazione di tutte le variabili del dataset *ds_hc*:
ds_hc <- data.frame(scale(ds_hc))

ds_hc1 <- test
modello_var_dummy <- dummyVars(~ CREDIT_MIX, data = ds_hc1, sep = ".")
var_dummy <- predict(modello_var_dummy, newdata = ds_hc1)
ds_hc1 <- cbind(ds_hc1[, !colnames(ds_hc1) %in% "CREDIT_MIX"], var_dummy)
#str(ds_hc)
colnames(ds_hc1)
y_test = ds_hc1[,10]
ds_hc1 = ds_hc1[,-10]
ds_hc1 <- data.frame(scale(ds_hc1))
y_test0 = as.numeric(as.factor(y_test))-1
y_test <- as.numeric(as.factor(y_test))
y_test = y_test-1
y_test <- to_categorical(y_test, num_classes = 3)
y_test

X <- ds_hc
X <- as.matrix(X) 
dim(X)
y <- as.numeric(as.factor(y))
y = y-1
y <- to_categorical(y, num_classes = 3)
y

model <- keras_model_sequential() %>%
  layer_dense(units = 512, input_shape = c(12), activation = 'relu') %>%
  layer_dense(units = 3, activation = 'softmax') 

model %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

model %>% summary()

set.seed(123)
model %>% fit(X, y, epochs = 50, batch_size = 128)

X_test <- as.matrix(ds_hc1)  # Converte il data frame in una matrice

# Assicurati che `y_test` sia una matrice o vettore numerico
Y_test <- as.matrix(y_test)
metrics <- model %>% evaluate(X_test, Y_test)
metrics

previsioni = model %>% predict(X_test) %>% k_argmax()
str(previsioni)
predictions <- as.vector(previsioni$numpy())
str(predictions)
predictions

y_test0= as.factor(y_test0)
y_test0
str(y_test0)

confusion_matrix <- table(Predicted = predictions, Actual = y_test0)
confusion_matrix

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- diag(confusion_matrix) / colSums(confusion_matrix)
recall <- diag(confusion_matrix) / rowSums(confusion_matrix)
f1_score <- 2 * (precision * recall) / (precision + recall)
macro_precision <- mean(precision, na.rm = TRUE)
macro_recall <- mean(recall, na.rm = TRUE)
macro_f1 <- mean(f1_score, na.rm = TRUE)
accuracy
precision
recall
f1_score
macro_precision
macro_recall
macro_f1



# ---- Confronto modelli di classificazione ------------------------------------
train_control <- trainControl(
  method = "cv",                  
  number = 5,                     
  classProbs = TRUE,              
  summaryFunction = multiClassSummary
)
test_knn <- as.data.frame(data_def[-split,])
knn_model <- train(
  x = data.frame(dataset), 
  y = test_knn$CREDIT_SCORE,
  method = "knn", 
  tuneGrid = data.frame(k = 31),
  trControl = train_control
)

train_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,              
  summaryFunction = multiClassSummary
)
rf_mod <- train(
  CREDIT_SCORE ~ .,          
  data = dataset,          
  method = "rf",        
  trControl = train_control
)


results <- resamples(list(KNN=knn_model, SVM=mod_svm_radial, RF=rf_mod))

