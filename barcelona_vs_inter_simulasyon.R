# 📦 Gerekli Paketler
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)

library(ggplot2)
library(dplyr)

# 📊 1. Veriler (Barcelona vs Inter Maç Sonuçları)

# İlk yarı golleri
takimA_ilk_yari <- c(1, 1, 0, 0, 0, 1, 0, 2, 0, 0)  # Barcelona
takimB_ilk_yari <- c(0, 1, 1, 1, 0, 0, 0, 1, 0, 0)  # Inter

# Maç sonu golleri
takimA_mac_sonu <- c(3, 0, 2, 2, 1, 2, 1, 2, 2, 0)
takimB_mac_sonu <- c(3, 1, 1, 1, 1, 0, 0, 0, 0, 0)

# 🎯 2. Ortalama Gol (Poisson λ değerleri)
lambda_A_ilk <- mean(takimA_ilk_yari)
lambda_B_ilk <- mean(takimB_ilk_yari)
lambda_A_mac_sonu <- mean(takimA_mac_sonu)
lambda_B_mac_sonu <- mean(takimB_mac_sonu)

# 🎲 3. Monte Carlo Simülasyonu
set.seed(42)
n <- 10000
sim_A_ilk <- rpois(n, lambda_A_ilk)
sim_B_ilk <- rpois(n, lambda_B_ilk)
sim_A_mac_sonu <- rpois(n, lambda_A_mac_sonu)
sim_B_mac_sonu <- rpois(n, lambda_B_mac_sonu)

# 📏 4. İlk Yarı %90 Güven Aralığı
ga_A_ilk <- quantile(sim_A_ilk, c(0.05, 0.95))
ga_B_ilk <- quantile(sim_B_ilk, c(0.05, 0.95))

cat("\nİlk Yarı %90 Güven Aralıkları:\n")
cat("Barcelona:", ga_A_ilk[1], "-", ga_A_ilk[2], "\n")
cat("Inter:", ga_B_ilk[1], "-", ga_B_ilk[2], "\n")

# 🏁 5. İlk Yarı Kazanma Olasılıkları
A_ilk_yari_kazanir <- mean(sim_A_ilk > sim_B_ilk)
B_ilk_yari_kazanir <- mean(sim_B_ilk > sim_A_ilk)
ilk_yari_beraberlik <- mean(sim_A_ilk == sim_B_ilk)

cat("\nİlk Yarı Kazanma Olasılıkları:\n")
cat(sprintf("Barcelona kazanır: %.2f%%\n", 100 * A_ilk_yari_kazanir))
cat(sprintf("Inter kazanır: %.2f%%\n", 100 * B_ilk_yari_kazanir))
cat(sprintf("Beraberlik: %.2f%%\n", 100 * ilk_yari_beraberlik))

# 🧮 6. İlk Yarı Skor Dağılımı Tablosu
ilk_yari_skorlar <- data.frame(TakimA = sim_A_ilk, TakimB = sim_B_ilk)
skor_dagilimi <- ilk_yari_skorlar %>%
  group_by(TakimA, TakimB) %>%
  summarise(Adet = n(), .groups = "drop") %>%
  mutate(Olasilik = 100 * Adet / sum(Adet)) %>%
  arrange(desc(Olasilik))

print(skor_dagilimi)

# 🌡️ 7. İlk Yarı Skor Dağılımı Heatmap
ggplot(skor_dagilimi, aes(x = TakimB, y = TakimA, fill = Olasilik)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_text(aes(label = sprintf("%.1f%%", Olasilik)), size = 3) +
  labs(
    title = "İlk Yarı Skor Dağılımı (Barcelona vs Inter)",
    x = "Inter İlk Yarı Gol",
    y = "Barcelona İlk Yarı Gol",
    fill = "Olasılık (%)"
  ) +
  theme_minimal()

# 🧾 8. Maç Sonu %90 Güven Aralıkları
ga_A_mac <- quantile(sim_A_mac_sonu, c(0.05, 0.95))
ga_B_mac <- quantile(sim_B_mac_sonu, c(0.05, 0.95))

cat("\nMaç Sonu %90 Güven Aralıkları:\n")
cat("Barcelona:", ga_A_mac[1], "-", ga_A_mac[2], "\n")
cat("Inter:", ga_B_mac[1], "-", ga_B_mac[2], "\n")

# ⚖️ 9. Maç Sonu Kazanma Olasılıkları
A_mac_sonu_kazanir <- mean(sim_A_mac_sonu > sim_B_mac_sonu)
B_mac_sonu_kazanir <- mean(sim_B_mac_sonu > sim_A_mac_sonu)
mac_sonu_beraberlik <- mean(sim_A_mac_sonu == sim_B_mac_sonu)

cat("\nMaç Sonu Kazanma Olasılıkları:\n")
cat(sprintf("Barcelona kazanır: %.2f%%\n", 100 * A_mac_sonu_kazanir))
cat(sprintf("Inter kazanır: %.2f%%\n", 100 * B_mac_sonu_kazanir))
cat(sprintf("Beraberlik: %.2f%%\n", 100 * mac_sonu_beraberlik))

# 📉 10. Maç Sonu Gol Farkı Histogramı
sonuclar_mac <- data.frame(Fark = sim_A_mac_sonu - sim_B_mac_sonu)

ggplot(sonuclar_mac, aes(x = Fark)) +
  geom_histogram(binwidth = 1, fill = "lightpink", color = "black") +
  geom_vline(xintercept = quantile(sonuclar_mac$Fark, probs = c(0.05, 0.95)),
             linetype = "dashed", color = "red") +
  labs(title = "Maç Sonu Gol Farkı (Barcelona - Inter)", 
       x = "Gol Farkı", y = "Frekans")

# ⚽ 11. İlk Golü Kim Atar? (Dakika bazlı analiz)
maclar <- list(
  list("2022-10-12", c(40, 82, 92), c(50, 63, 89)),
  list("2019-10-02", c(58, 84), c(2)),
  list("2018-10-24", c(32, 83), c(27)),
  list("2010-04-28", c(84), c(35)),
  list("2009-11-24", c(10, 26), c(18))
)

ilk_gol_atan <- function(barcelona_goller, inter_goller) {
  if (length(barcelona_goller) == 0 && length(inter_goller) == 0) return("Gol Yok")
  if (length(barcelona_goller) == 0) return("Inter")
  if (length(inter_goller) == 0) return("Barcelona")
  
  if (min(barcelona_goller) < min(inter_goller)) {
    return("Barcelona")
  } else if (min(inter_goller) < min(barcelona_goller)) {
    return("Inter")
  } else {
    return("Beraberlik (Eşit zamanlı gol)")
  }
}

ilk_gol_analizi <- sapply(maclar, function(x) ilk_gol_atan(x[[2]], x[[3]]))

cat("\n📊 İlk Golü Kim Atar? Analizi:\n")
cat(sprintf("İlk golü atan takım: %s\n", paste(ilk_gol_analizi, collapse = ", ")))
