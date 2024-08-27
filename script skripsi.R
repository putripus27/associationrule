setwd("D:/UNY/SKRIPSI/data")

#####========== DESKRIPTIF ==========#####
d2 <- read.csv("Book1.csv")
str(d2)

beras <- d2[d2$Kategori == "Beras", ]
b <- tapply(beras$total, beras$Detail.Produk, sum)
b
barplot(b, main="Penjualan Beras Organik",
              xlab="Total Penjualan (kg)", ylab ="Produk", col = "steelblue")

beras <- read.csv("beras.csv")
library(ggpubr)
ggdotchart(beras, x = "Produk", y = "Berat_kg",
           sorting = "descending", # Sort value in descending order
           add = "segments",
           color = "steelblue",
           rotate = TRUE, # Rotate vertically
           dot.size = 10, # Large dot size
           label = round(beras$Berat_kg), # Add mpg values as dot labels
           font.label = list(color = "white", size = 9,
                             vjust = 0.5), # Adjust label parameters
           ggtheme = theme_pubr() # ggplot2 theme
)

tepung <- read.csv("tepung organik.csv")
tepung
counts <- tepung$Penjualan
bp <- barplot(counts, main="Penjualan Tepung Organik",
                xlab="Produk", ylab ="Total Penjualan (kg)",ylim=c(-2,75), col = "steelblue")
text(bp,-1, as.character(tepung$Produk))

tepung <- d2[d2$Kategori == "tepung", ]
t <- tapply(tepung$total, tepung$Detail.Produk, sum)
t
barplot(t, main="Penjualan Tepung Organik",
        xlab="Produk", ylab ="Total Penjualan (kg)", col = "steelblue")


herco <- d2[d2$Detail.Produk == "HERCO", ]
h <- table(herco$Ukuran)
bph <- barplot(h, main="Penjualan HERCO",
              xlab="Kemasan (ml)", ylab ="Jumlah Penjualan (pcs)", col = "steelblue")

laitco <- d2[d2$Detail.Produk == "LAITCO", ]
l <- table(laitco$Ukuran)
bpl <- barplot(l, main="Penjualan LAITCO",
        xlab="Kemasan (ml)", ylab ="Jumlah Penjualan (pcs)", col = "steelblue")

tpgarut <- d2[d2$Detail.Produk == "TEPUNG GARUT", ]
tg <- table(tpgarut$Ukuran)
barplot(tg, main="Penjualan Tepung Garut",
        xlab="Kemasan (g)", ylab ="Jumlah Penjualan (pcs)", col = "steelblue")

gula <- d2[d2$Detail.Produk == "GULA SEMUT", ]
gs <- table(gula$Ukuran)
barplot(gs, main="Penjualan Gula Semut",
        xlab="Kemasan (g)", ylab ="Jumlah Penjualan (pcs)", col = "steelblue")

kacang <- d2[d2$Kategori == "Kacang", ]
k <- tapply(kacang$total, kacang$Detail.Produk, sum)
k
barplot(k, main="Penjualan Kacang - kacangan",
              xlab="Produk", ylab ="Total Penjualan (kg)", col = "steelblue")

ggdotchart(kacang, x = "Produk", y = "Jumlah",
           sorting = "descending", # Sort value in descending order
           add = "segments",
           color = "steelblue",
           rotate = TRUE, # Rotate vertically
           dot.size = 10, # Large dot size
           label = kacang$Jumlah, # Add mpg values as dot labels
           font.label = list(color = "white", size = 9,
                             vjust = 0.5), # Adjust label parameters
           ggtheme = theme_pubr() # ggplot2 theme
)

d1 <- read.csv("Book1.csv")
str(d1)

library(plyr)
library(dplyr)

total_item_perbulan = d1 %>%
  select(Bulan, Kuantitas) %>%
  group_by(Bulan) %>%
  arrange(Bulan) %>%
  summarise(Item_terjual = sum(Kuantitas))
total_item_perbulan

total_transaksi_perbulan = d1 %>%
  select(Bulan, Trx) %>%
  group_by(Bulan) %>%
  arrange(Bulan) %>%
  summarise(total_transaksi = length(unique(Trx)))
total_transaksi_perbulan

d2 <- left_join(total_item_perbulan,total_transaksi_perbulan, by = "Bulan")
#write.csv(d2, "item - transaksi perbulan.csv")

item_transaksi_perbulan <- read.csv("item - transaksi perbulan.csv")

plot(item_transaksi_perbulan$Item_terjual, 
     type = "o", pch = 19, col = "steelblue", xlab = "Bulan", ylab = "Total item",
     lwd=2,lty=1)

plot(item_transaksi_perbulan$total_transaksi,
     type = "o", pch = 19, col = "steelblue", xlab = "Bulan", ylab = "Jumlah transaksi",
     lwd=2,lty=1)

transaksi_perhari = d1 %>%
  select(Date, Trx) %>%
  group_by(Date) %>%
  arrange(Date) %>%
  summarise(jumlah_transaki = length(unique(Trx)))
transaksi_perhari

# rata - rata transaksi per hari
mean(transaksi_perhari$jumlah_transaki)

item_perhari = d1 %>%
  select(Date, Kuantitas) %>%
  group_by(Date) %>%
  arrange(Date) %>%
  summarise(item = sum(Kuantitas))
item_perhari

# rata - rata kuantitas setiap item terjual per hari
mean(item_perhari$item)

#####========== APRIORI ==========#####
library(arules)
library(arulesViz)

penjualan <- read.transactions("juli 2021 - juni 2022.csv", sep = ",")
summary(penjualan)

itemFrequencyPlot(penjualan, topN = 34, horiz = TRUE)

###=== 1-itemsets
items1 <- apriori(penjualan, parameter = list(support = 0.001, conf = 0.001, maxlen = 1, target = "rules"))
inspect(items1)
write(items1, "freq 1 items.csv", sep = ",", quote = TRUE, row.names = FALSE)

###=== 2-itemsets
items2 <- apriori(penjualan, parameter = list(support = 0.001, maxlen = 2, target = "frequent itemsets"))
inspect(items2)
write(items2, "freq 2 items.csv", sep = ",", quote = TRUE, row.names = FALSE)

###=== 3-itemsets
items3 <- apriori(penjualan, parameter = list(support = 0.001, maxlen = 3, target = "frequent itemsets"))
inspect(items3)
write(items3, "freq 3 items.csv", sep = ",", quote = TRUE, row.names = FALSE)

###=== 4-itemsets
items4 <- apriori(penjualan, parameter = list(support = 0.001, maxlen = 4, target = "frequent itemsets"))
inspect(items4)
write(items4, "freq 4 items.csv", sep = ",", quote = TRUE, row.names = FALSE)

###=== 5-itemsets
items5 <- apriori(penjualan, parameter = list(support = 0.001, maxlen = 5, target = "frequent itemsets"))
inspect(items5)
write(items5, "freq 5 items.csv", sep = ",", quote = TRUE, row.names = FALSE)

###=== 6-itemsets
items6 <- apriori(penjualan, parameter = list(support = 0.001, maxlen = 6, target = "frequent itemsets"))
inspect(items6)
write(items6, "freq 6 items.csv", sep = ",", quote = TRUE, row.names = FALSE)

###=== 7-itemsets
items7 <- apriori(penjualan, parameter = list(support = 0.001, maxlen = 7, target = "frequent itemsets"))
inspect(items7)
write(items7, "freq 7 items.csv", sep = ",", quote = TRUE, row.names = FALSE)

###=== 8-itemsets
items8 <- apriori(penjualan, parameter = list(support = 0.001, maxlen = 8, target = "frequent itemsets"))
inspect(items8)
write(items8, "freq 8 items.csv", sep = ",", quote = TRUE, row.names = FALSE)

###=== 9-itemsets
items9 <- apriori(penjualan, parameter = list(support = 0.001, maxlen = 9, target = "frequent itemsets"))
inspect(items9)
write(items9, "freq 9 items.csv", sep = ",", quote = TRUE, row.names = FALSE)

###=== 10-itemsets
items10 <- apriori(penjualan, parameter = list(support = 0.001, maxlen = 10, target = "frequent itemsets"))
inspect(items10)
write(items10, "freq 10 items.csv", sep = ",", quote = TRUE, row.names = FALSE)

###=== 11-itemsets
items11 <- apriori(penjualan, parameter = list(support = 0.001, conf = 0.001, maxlen = 11, target = "rules"))
inspect(items1)
write(items11, "freq 11 items + conf.csv", sep = ",", quote = TRUE, row.names = FALSE)

###=== rules
rules1 <- apriori(penjualan, parameter = list(support = 0.1, confidence = 0.5,
                                             maxlen = 3, target = "rules"))
write(rules1, "rule trial 1.csv", sep = ",", quote = TRUE, row.names = FALSE)
rules2 <- apriori(penjualan, parameter = list(support = 0.01, confidence = 0.5,
                                             maxlen = 3, target = "rules"))
write(rules2, "rule trial 2.csv", sep = ",", quote = TRUE, row.names = FALSE)

rules3 <- apriori(penjualan, parameter = list(support = 0.001, confidence = 0.5,
                                             maxlen = 3, target = "rules"))
write(rules3, "rule trial 3.csv", sep = ",", quote = TRUE, row.names = FALSE)


rules_by_lift <- sort(rules3, by = "lift")
write(rules_by_lift, "rules_by_lift.csv", sep = ",", quote = TRUE, row.names = FALSE)

bdiabetes <- subset(rules3, items %in% "BERAS DIABETES ORGANIK")
inspect(bdiabetes)
write(bdiabetes, "bdiabetes.csv", sep = ",", quote = TRUE, row.names = FALSE)

bfrules <- subset(bdiabetes, items %in% "BLACKFIT")
inspect(bfrules)
write(bfrules, "bfrules.csv", sep = ",", quote = TRUE, row.names = FALSE)

rfrules <- subset(bdiabetes, items %in% "REDFIT")
inspect(rfrules)
write(rfrules, "rfrules.csv", sep = ",", quote = TRUE, row.names = FALSE)

gsrules <- subset(bdiabetes, items %in% "GULA SEMUT")
inspect(gsrules)
write(gsrules, "gsrules.csv", sep = ",", quote = TRUE, row.names = FALSE)

jmrules <- subset(bdiabetes, items %in% "JAGUNG MUTIARA")
inspect(jmrules)
write(jmrules, "jmrules.csv", sep = ",", quote = TRUE, row.names = FALSE)

madurules <- subset(rules3, items %in% "Madu Hanna Raw Honey")
inspect(madurules)
write(madurules, "madurules.csv", sep = ",", quote = TRUE, row.names = FALSE)

hrules <- subset(bdiabetes, items %in% "HERCO")
inspect(hrules)
write(hrules, "hrules.csv", sep = ",", quote = TRUE, row.names = FALSE)

lrules <- subset(bdiabetes, items %in% "LAITCO")
inspect(lrules)
write(lrules, "lrules.csv", sep = ",", quote = TRUE, row.names = FALSE)

rules_2item  <- apriori(penjualan, parameter = list(support = 0.001, confidence = 0.5,
                                    maxlen = 2, target = "rules"))
inspect(rules_2item)
write(rules_2item, "rules_2item.csv", sep = ",", quote = TRUE, row.names = FALSE)

