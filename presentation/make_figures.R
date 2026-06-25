suppressMessages({ library(readr); library(dplyr); library(ggplot2) })
csv  <- "C:/Users/topaz/Desktop/תכנון וניתוח נתונים/פרוייקט אפשרות IBM/data/WA_Fn-UseC_-HR-Employee-Attrition.csv"
out  <- "C:/Users/topaz/Desktop/תכנון וניתוח נתונים/פרוייקט אפשרות IBM/presentation"
navy <- "#1F4E5F"; accent <- "#C0622E"; grayc <- "grey55"

ibm <- read_csv(csv, show_col_types = FALSE) %>%
  mutate(Attr = as.integer(Attrition == "Yes"),
         s_overtime = as.integer(OverTime == "Yes"),
         s_travel   = as.integer(BusinessTravel != "Non-Travel"),
         s_single   = as.integer(MaritalStatus == "Single"),
         StrainLoad = s_overtime + s_travel + s_single)
ibm$IncomeBand <- factor(ntile(ibm$MonthlyIncome, 3), labels = c("Low pay","Medium pay","High pay"))
base_rate <- mean(ibm$Attr)

## ---- Figure 1: dose-response (last bar highlighted in accent) ----
st <- ibm %>% group_by(StrainLoad) %>%
  summarise(n = n(), rate = mean(Attr), .groups = "drop") %>%
  mutate(hi = StrainLoad == 3)

p1 <- ggplot(st, aes(factor(StrainLoad), rate, fill = hi)) +
  geom_col(width = 0.62) +
  geom_text(aes(label = paste0(scales::percent(rate, accuracy = 0.1), "\n(n=", n, ")")),
            vjust = -0.35, size = 5.2, lineheight = 0.9, colour = "grey15") +
  geom_hline(yintercept = base_rate, linetype = "dashed", colour = grayc) +
  annotate("text", x = 0.7, y = base_rate + 0.025, label = "Company average 16.1%",
           hjust = 0, size = 4.2, colour = grayc) +
  scale_fill_manual(values = c(`FALSE` = navy, `TRUE` = accent), guide = "none") +
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.6)) +
  labs(x = "Number of strain markers (overtime + travel + single)", y = "Attrition rate") +
  theme_minimal(base_size = 16) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.title = element_text(face = "bold"))
ggsave(file.path(out, "dose_response.png"), p1, width = 7.2, height = 4.6, dpi = 200, bg = "white")

## ---- Figure 2: heatmap strain x pay ----
hm <- ibm %>% group_by(StrainLoad, IncomeBand) %>%
  summarise(rate = mean(Attr), n = n(), .groups = "drop") %>%
  mutate(txt = ifelse(rate > 0.30, "white", "grey15"))

p2 <- ggplot(hm, aes(IncomeBand, factor(StrainLoad), fill = rate)) +
  geom_tile(colour = "white", linewidth = 1.5) +
  geom_text(aes(label = paste0(scales::percent(rate, accuracy = 1), "\n(n=", n, ")"), colour = txt),
            size = 5, lineheight = 0.9) +
  scale_colour_identity() +
  scale_fill_gradient(low = "#DCE6E8", high = navy, labels = scales::percent, name = "Attrition") +
  labs(x = "Pay group", y = "Number of strain markers") +
  theme_minimal(base_size = 16) +
  theme(panel.grid = element_blank(),
        axis.title = element_text(face = "bold"),
        legend.position = "right")
ggsave(file.path(out, "heatmap.png"), p2, width = 7.4, height = 4.6, dpi = 200, bg = "white")

cat("figures saved to", out, "\n")
