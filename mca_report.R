library(readxl)
library(ggplot2)
library(dplyr)

path_to_file1 <- file.choose()
data <- read_excel(path_to_file1)

path_to_file2 <- file.choose()
data_standaryzowane <- read_excel(path_to_file2)

###############################################################################
# Obliczanie macierzy odległości euklidesowej
dist_euclidean <- dist(data_standaryzowane)

# Hierarchiczne grupowanie
hclust_euclidean <- hclust(dist_euclidean, method = "ward.D")
hclust_groups <- cutree(hclust_euclidean, k = 4)  # Klasyfikacja na 4 grupy

# Tworzenie dendrogramu
library(dendextend)


# Tworzenie dendrogramu
dend <- as.dendrogram(hclust_euclidean)

# Przypisanie klastrów do gałęzi na podstawie wyników z cutree
labels_order <- order.dendrogram(dend)  # Pobierz kolejność etykiet w dendrogramie
grouped_labels <- hclust_groups[labels_order]  # Dopasuj grupy do tej kolejności

# Dodanie etykiet do dendrogramu
labels(dend) <- data$labels[labels_order]  # Dopasowanie etykiet do kolejności dendrogramu
dend <- set(dend, "labels_cex", 0.6)  # Ustawienie rozmiaru etykiet

my_colors <- c("green", "blue", "red", "violet")  # Zdefiniuj swoje kolory

# Kolorowanie gałęzi na podstawie grup z użyciem własnych kolorów
dend <- color_branches(dend, k = 4, col = my_colors)

# Rysowanie dendrogramu z kolorami
plot(dend, main = "Grupowanie hierarchiczne (odl. euklidesowa)")


# Dodanie grup do oryginalnych danych
data_with_groups <- data %>%
  mutate(group = as.factor(hclust_groups))  # Przypisanie grup do danych

# Wykres zależności x5 i x7 z kolorowaniem grupami
ggplot(data, aes(x = x5, y = x7, color = data_with_groups$group)) +
  geom_text(aes(label = data$labels), vjust = -1, size = 2, check_overlap = FALSE) +
  geom_point(size = 3) +
  labs(title = "Zależność między x5 a x7",
       x = "x5",  # Popraw na x5 (wcześniej był błąd)
       y = "x7",
       color = "Grupa") +
  scale_color_manual(name = "Klastry", values = kolory_grup) +  # Spójne kolory z dendrogramem
  theme_minimal()




















########################################


# Dodanie grup do oryginalnych danych
data_with_groups <- data %>%
  mutate(group = as.factor(hclust_groups))  # Przypisanie grup do danych

# Wykres zależności x5 i x7 z kolorowaniem grupami
ggplot(data, aes(x = x5, y = x7, color = data_with_groups$group)) +
  geom_text(aes(label = data$labels), vjust = -1, size = 2, check_overlap = FALSE) +
  geom_point(size = 3) +
  labs(title = "Zależność między x5 a x7",
       x = "x5",  # Popraw na x5 (wcześniej był błąd)
       y = "x7",
       color = "Grupa") +
  scale_color_manual(name = "Klastry", values = kolory_grup) +  # Spójne kolory z dendrogramem
  theme_minimal()






















dist_euclidean <- dist(data_standaryzowane)
hclust_euclidean <- hclust(dist_euclidean, method = "ward.D")
hclust_groups <- cutree(hclust_euclidean, k = 4)
dend <- as.dendrogram(hclust_euclidean)
dend <- color_branches(dend, k = 4)
labels(dend) <- data$labels
dend <- set(dend, "labels_cex", 0.6)
plot(dend, main = "Grupowanie hierarchiczne (odl. euklidesowa)")

data_with_groups <- data_standaryzowane %>%
  mutate(group = as.factor(cutree(hclust_euclidean, k =4)))

ggplot(data, aes(x = x5, y = x7, color = data_with_groups$group)) +
  geom_text(aes(label = data$labels), vjust = -1, size = 2, check_overlap = FALSE) +
  geom_point(size = 3) +
  labs(title = "Zależność między x5 a x7",
       x = "x5",
       y = "x7",
       color = "Grupa") +
  scale_color_manual(name = "Klastry", values = kolory_grup) +  # Ustaw kolor klastra
  theme_minimal()






