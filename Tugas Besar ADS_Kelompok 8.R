library(readxl)
library(tseries)
library(lmtest)
library(ggplot2)

Pengunjung_Mall <- read_excel("C:/@Sem3/ADS/kelompok 8/Tubes/Pengunjung_Mall.xlsx")
View(Pengunjung_Mall)
Pengunjung_Mall

str(Pengunjung_Mall)

summary(Pengunjung_Mall)

#Mmebuat plot data
plot(x = Pengunjung_Mall$Usia, y = Pengunjung_Mall$Pengeluaran_USD, 
     main = "Plot Data Pengunjung Mall rentang usia dan pengeluaran",
     xlab = "Usia", ylab = "Pengeluaran", col='red')
barplot(Pengunjung_Mall$Pengeluaran_USD, Pengunjung_Mall$Usia, xlab = "Usia", ylab = "Pengeluaran")

#Membuat Model Regresi
model_regresi<- lm(Pengunjung_Mall$Pengeluaran_USD~Pengunjung_Mall$Usia)
summary(model_regresi)

#Membuat garis regresi
abline(model_regresi)

#Korelasi Analisis Regresi Linier
cor(Pengunjung_Mall$Pengeluaran_USD, Pengunjung_Mall$Usia, method = 'pearson' )

#Koefisien Determinasi Regresi Linier
summary(lm(Pengunjung_Mall$Pengeluaran_USD~Pengunjung_Mall$Usia))

#Uji Normalitas
library(tseries)
sisa<-residuals(model_regresi)
shapiro.test(sisa)

#Uji Heterokedastisitas
library(lmtest)
bptest(model_regresi)

