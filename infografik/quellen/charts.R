suppressMessages({library(tidyverse); library(svglite)})
d <- readRDS("daten.rds"); s9 <- d$s9; reihe <- d$reihe
ink <- "#1f2933"; ink2 <- "#52606d"; rot <- "#b2182b"; blau <- "#2166ac"
winter <- "#9ecae1"; sommer <- "#e08a2c"
th <- theme_minimal(base_family = "Inter", base_size = 12) +
  theme(plot.title = element_blank(), text = element_text(colour = ink),
        axis.text = element_text(colour = ink2), axis.title = element_text(colour = ink2, size = 10),
        panel.grid.minor = element_blank(), panel.grid.major = element_line(colour = "#e4e7eb", linewidth = 0.3),
        plot.margin = margin(18, 12, 4, 4), legend.position = "bottom",
        legend.title = element_text(colour = ink2, size = 10), legend.text = element_text(colour = ink2, size = 10))

# ---------- 1) Heatmap ----------
raster <- s9 |> complete(glacier_name, jahr = 1990:2025) |>
  mutate(glacier_name = fct_reorder(glacier_name, Ba, mean, na.rm = TRUE, .desc = TRUE))
p1 <- ggplot(raster, aes(jahr, glacier_name, fill = Ba)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  scale_fill_gradient2(low = rot, mid = "white", high = blau, midpoint = 0, limits = c(-3, 3),
                       oob = scales::squish, na.value = "grey72",
                       breaks = c(-3, -2, -1, 0, 1, 2, 3), labels = c("≤ −3", "−2", "−1", "0", "+1", "+2", "+3"),
                       guide = guide_colourbar(title.position = "top", title.hjust = 0.5)) +
  scale_x_continuous(breaks = seq(1990, 2025, 5), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  annotate("text", x = 2003, y = 9.95, label = "Hitzesommer 2003", vjust = 0, fontface = "bold", size = 3.6, family = "Inter", colour = ink) +
  annotate("text", x = 2022, y = 9.95, label = "Rekordjahr 2022", vjust = 0, hjust = 0.85, fontface = "bold", size = 3.6, family = "Inter", colour = ink) +
  coord_cartesian(clip = "off") +
  labs(x = NULL, y = NULL, fill = "Jahresbilanz [m w.e.]") +
  th + theme(panel.grid = element_blank(), axis.text.y = element_text(size = 11, colour = ink),
             legend.key.width = unit(2.2, "cm"), legend.key.height = unit(0.28, "cm"),
             plot.margin = margin(22, 8, 4, 4))
svglite("heat.svg", width = 10.1, height = 3.7); print(p1); invisible(dev.off())

# ---------- 2) Kumuliert ----------
k <- bind_rows(tibble(jahr = 1989, kum = 0), reihe |> select(jahr, kum))
p2 <- ggplot(k, aes(jahr, kum)) +
  geom_hline(yintercept = 0, colour = "grey35", linewidth = 0.6) +
  geom_area(fill = rot, alpha = 0.10) +
  geom_line(colour = rot, linewidth = 1.5) +
  geom_point(data = k |> filter(jahr %in% c(2003, 2022)), colour = rot, size = 3.2, shape = 21, fill = "white", stroke = 1.4) +
  annotate("text", x = 2003.7, y = k$kum[k$jahr == 2003] + 2.2, label = "2003", hjust = 0, fontface = "bold", size = 3.6, family = "Inter", colour = ink) +
  annotate("text", x = 2022.7, y = k$kum[k$jahr == 2022] + 2.4, label = "2022", hjust = 0, fontface = "bold", size = 3.6, family = "Inter", colour = ink) +
  annotate("text", x = 2025.5, y = last(k$kum), label = sprintf("%.1f", last(k$kum)), hjust = 0, fontface = "bold", size = 5, family = "Inter", colour = rot) +
  scale_x_continuous(limits = c(1989, 2029), breaks = seq(1990, 2025, 5)) +
  scale_y_continuous(breaks = seq(-35, 0, 5), limits = c(-36, 2)) +
  labs(x = NULL, y = "Kumulierte Jahresbilanz [m w.e.]") + th
svglite("kum.svg", width = 6.3, height = 4.1); print(p2); invisible(dev.off())

# ---------- 3) Winter / Sommer ----------
dek <- reihe |> mutate(dek = jahr %/% 10 * 10) |> group_by(dek) |>
  summarise(von = min(jahr) - 0.4, bis = max(jahr) + 0.4, Bw = mean(Bw), Bs = mean(Bs), .groups = "drop")
lang <- reihe |> pivot_longer(c(Bw, Bs), names_to = "saison", values_to = "wert") |>
  mutate(saison = factor(saison, c("Bw", "Bs"), c("Winterbilanz (Schneezuwachs)", "Sommerbilanz (Schmelze)")))
p3 <- ggplot(lang, aes(jahr, wert)) +
  geom_hline(yintercept = 0, colour = "grey35", linewidth = 0.6) +
  geom_col(aes(fill = saison), width = 0.78) +
  geom_segment(data = dek, aes(x = von, xend = bis, y = Bw, yend = Bw), colour = ink, linewidth = 0.9) +
  geom_segment(data = dek, aes(x = von, xend = bis, y = Bs, yend = Bs), colour = ink, linewidth = 0.9) +
  geom_text(data = dek, aes(x = (von + bis) / 2, y = 2.2, label = sprintf("Ø %+.1f", Bw)), size = 3.7, family = "Inter", fontface = "bold", colour = ink) +
  geom_text(data = dek, aes(x = (von + bis) / 2, y = -4.6, label = gsub("-", "−", sprintf("Ø %.1f", Bs))), size = 3.7, family = "Inter", fontface = "bold", colour = ink) +
  annotate("text", x = 2003.9, y = -3.65, label = "2003: Winter normal,\nSommer extrem", hjust = 0, size = 3.3, family = "Inter", colour = ink, lineheight = 0.95) +
  annotate("text", x = 2021.5, y = -3.75, label = "2022: wenig Schnee\nund extreme Schmelze", hjust = 1, size = 3.3, family = "Inter", colour = ink, lineheight = 0.95) +
  scale_fill_manual(values = c(winter, sommer)) +
  scale_x_continuous(breaks = seq(1990, 2025, 5)) +
  scale_y_continuous(limits = c(-5.7, 2.6), breaks = -5:2) +
  labs(x = NULL, y = "Bilanz [m w.e.]", fill = NULL) + th +
  theme(legend.key.size = unit(0.4, "cm"), legend.text = element_text(size = 11, colour = ink))
svglite("saison.svg", width = 10.1, height = 4.3); print(p3); invisible(dev.off())
cat("ok\n")
