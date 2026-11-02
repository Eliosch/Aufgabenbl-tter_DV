suppressMessages(library(tidyverse))
# --- Daten (gleiche Logik wie im Arbeitsblatt) ---
z <- read_lines("massbalance_fixdate.csv")[-(1:9)]
z <- z[z != ""]
spalten <- c("glacier_name","glacier_id","date_start","date_end_winter","date_end","Bw","Bs","Ba","ELA","AAR","area_km2","h_min","h_max","observer")
gl <- tibble(zeile = z) |>
  separate(zeile, into = spalten, sep = ",", extra = "merge", fill = "right") |>
  mutate(across(everything(), str_trim),
         across(c(Bw, Bs, Ba), ~ as.numeric(.x) / 1000),
         jahr = as.integer(str_sub(date_end, 1, 4))) |>
  select(glacier_name, glacier_id, jahr, Bw, Bs, Ba)
konstant <- gl |> filter(jahr >= 1990) |> count(glacier_id, glacier_name) |> filter(n >= 34)
stopifnot(nrow(konstant) == 9)
s9 <- gl |> filter(glacier_id %in% konstant$glacier_id, jahr >= 1990)
reihe <- s9 |> group_by(jahr) |> summarise(n = n(), Bw = mean(Bw), Bs = mean(Bs), Ba = mean(Ba), .groups = "drop")
reihe <- reihe |> mutate(kum = cumsum(Ba))
saveRDS(list(s9 = s9, reihe = reihe), "daten.rds")
cat("kumuliert 2025:", round(last(reihe$kum), 1), "\n")
cat("kum 2002, 2003, 2021, 2022:", round(reihe$kum[reihe$jahr %in% c(2002,2003,2021,2022)], 1), "\n")
print(reihe |> mutate(dek = jahr %/% 10 * 10) |> group_by(dek) |> summarise(n=n(), Bw = mean(Bw), Bs = mean(Bs), Ba = mean(Ba)) |> mutate(across(c(Bw,Bs,Ba), ~round(.x,2))) |> as.data.frame())
print(reihe |> filter(Ba < -1.4) |> select(jahr, Ba, Bw, Bs) |> mutate(across(-jahr, ~round(.x,2))) |> as.data.frame())
print(reihe |> filter(Ba > 0) |> pull(jahr))
cat("n pos gj:", sum(s9$Ba > 0), "von", nrow(s9), "\n")
cat("Bs min 2022/2003:", round(reihe$Bs[reihe$jahr==2022],2), round(reihe$Bs[reihe$jahr==2003],2), " Bw:", round(reihe$Bw[reihe$jahr==2022],2), round(reihe$Bw[reihe$jahr==2003],2), "\n")
cat("Bw mean/sd; Bs mean 1990-2005 vs 2011-2025:", round(mean(reihe$Bw),2), round(sd(reihe$Bw),2), round(mean(reihe$Bs[reihe$jahr<=2005]),2), round(mean(reihe$Bs[reihe$jahr>=2011]),2), "\n")
cat("Trend Bs, Bw pro Jahrzehnt:", round(coef(lm(Bs~jahr,reihe))[2]*10,2), round(coef(lm(Bw~jahr,reihe))[2]*10,2), "\n")
cat("cor Ba~Bs, Ba~Bw:", round(cor(reihe$Ba,reihe$Bs),2), round(cor(reihe$Ba,reihe$Bw),2), "\n")
# Gletscher kumuliert (fehlende Jahre) und Spannweite 2022/2003
print(s9 |> filter(jahr %in% c(2003, 2022)) |> group_by(jahr) |> summarise(min = min(Ba), max = max(Ba), n = n()) |> as.data.frame())
