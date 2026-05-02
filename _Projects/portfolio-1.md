---
title: "App State 2026 Baserunning"
author: "App State Baseball Analytics"
date: "`r format(Sys.Date(), '%B %d, %Y')`"
output:
  pdf_document:
    toc: true
    toc_depth: '3'
  html_document:
    theme: flatly
    highlight: tango
    toc: true
    toc_float:
      collapsed: false
      smooth_scroll: true
    toc_depth: 3
    number_sections: false
    df_print: kable
    self_contained: true
subtitle: "2026 Season - Sabermetric Approximations from Trackman Data"
params:
  data_file: "2025-2026 (2).csv"
  my_team: APP_MOU
---

```{css, echo=FALSE}
body        { font-family: 'Segoe UI', Arial, sans-serif; font-size: 15px; }
h1.title    { color: #1B3A6B; font-size: 2.1em; }
h3.subtitle { color: #555; font-size: 1.1em; margin-top:-8px; }
h2          { color: #1B3A6B; border-bottom: 2px solid #C8A951; padding-bottom: 4px; }
h3          { color: #333; }
.metric-grid {
  display: grid;
  grid-template-columns: repeat(auto-fill, minmax(160px, 1fr));
  gap: 12px;
  margin: 18px 0;
}
.metric-card {
  background: #f8f9fa;
  border-left: 4px solid #1B3A6B;
  border-radius: 6px;
  padding: 10px 14px;
  text-align: center;
}
.metric-card .label { font-size: 0.78em; color: #666; font-weight: 600; letter-spacing:.04em; }
.metric-card .value { font-size: 1.55em; font-weight: 700; color: #1B3A6B; line-height:1.2; }
.metric-card .sub   { font-size: 0.72em; color: #888; }
.callout {
  background: #eef4fb;
  border-left: 5px solid #2171b5;
  padding: 12px 16px;
  border-radius: 4px;
  margin: 14px 0;
  font-size: 0.92em;
}
.callout-warn {
  background: #fff8e1;
  border-left: 5px solid #f9a825;
  padding: 12px 16px;
  border-radius: 4px;
  margin: 14px 0;
  font-size: 0.92em;
}
table { font-size: 0.88em; }
thead tr { background-color: #1B3A6B !important; color: white !important; }
```

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo    = FALSE,
  warning = FALSE,
  message = FALSE,
  fig.align = "center",
  dpi     = 150
)

pkgs <- c("tidyverse", "fmsb", "scales", "ggrepel",
          "patchwork", "kableExtra", "RColorBrewer")
new  <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(new)) install.packages(new, repos = "https://cran.rstudio.com/")

suppressPackageStartupMessages({
  library(tidyverse)
  library(fmsb)
  library(scales)
  library(ggrepel)
  library(patchwork)
  library(kableExtra)
  library(RColorBrewer)
  library(lubridate)
})

DATA_FILE <- params$data_file
MY_TEAM   <- params$my_team

TEAM_COLORS <- c(WAK_DEA = "#1B3A6B", APP_MOU = "#1A1A1A")
TEAM_ACCENT <- c(WAK_DEA = "#C8A951", APP_MOU = "#FFCC00")
```

```{r load-data}
raw <- read_csv(DATA_FILE, show_col_types = FALSE)

raw <- raw %>%
  filter(
    !str_detect(BatterTeam,  regex("practice|scrimmage|intrasquad|fall|spring practice", ignore_case = TRUE)),
    !str_detect(PitcherTeam, regex("practice|scrimmage|intrasquad|fall|spring practice", ignore_case = TRUE)),
    !BatterTeam  %in% c("APP_PRA"),
    !PitcherTeam %in% c("APP_PRA")
  ) %>%
  filter(mdy(Date) >= as.Date("2026-02-13"))

#cat("Games included:", n_distinct(raw$GameID), "\n")
#cat("Date range:", as.character(min(mdy(raw$Date))),
   # "to", as.character(max(mdy(raw$Date))), "\n")
#cat("Teams remaining:", paste(unique(raw$BatterTeam), collapse = ", "), "\n")

total_games <- n_distinct(raw$GameID)
date_start  <- min(mdy(raw$Date))
date_end    <- max(mdy(raw$Date))
```

---

<div class="callout">
**Season:** 2026 &nbsp;|&nbsp; **Games:** `r total_games` &nbsp;|&nbsp; **Date Range:** `r date_start` to `r date_end`
<br>
**Metrics are Trackman-proxied** — no Statcast GPS. See the *Methodology* section for formula details.
</div>

---

```{r pa-classification}
pa_df <- raw %>%
  arrange(PitchNo) %>%
  group_by(Batter, BatterTeam, Inning, PAofInning) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(
    PlayResult    = replace_na(as.character(PlayResult), "Undefined"),
    KorBB         = replace_na(as.character(KorBB),      "Undefined"),
    TaggedHitType = replace_na(as.character(TaggedHitType), "Undefined"),
    ExitSpeed     = suppressWarnings(as.numeric(ExitSpeed)),
    Angle         = suppressWarnings(as.numeric(Angle)),
    Distance      = suppressWarnings(as.numeric(Distance)),
    RunsScored    = replace_na(suppressWarnings(as.numeric(RunsScored)), 0),
    OutsOnPlay    = replace_na(suppressWarnings(as.numeric(OutsOnPlay)), 0),
    Outs          = replace_na(suppressWarnings(as.numeric(Outs)), 0),
    is_1b      = PlayResult == "Single",
    is_2b      = PlayResult == "Double",
    is_3b      = PlayResult == "Triple",
    is_hr      = PlayResult == "HomeRun",
    is_hit     = is_1b | is_2b | is_3b | is_hr,
    is_walk    = KorBB == "Walk",
    is_k       = KorBB == "Strikeout",
    is_gdp     = PlayResult == "Out" & TaggedHitType == "GroundBall" & OutsOnPlay == 2,
    is_gdp_opp = PlayResult == "Out" & TaggedHitType == "GroundBall" & Outs == 0,
    is_sac     = PlayResult == "Sacrifice",
    is_error   = PlayResult == "Error",
    is_fc      = PlayResult == "FieldersChoice"
  )
```

```{r batter-stats}
batter_stats <- pa_df %>%
  group_by(Batter, BatterTeam) %>%
  summarise(
    PA       = n(),
    AB       = sum(!is_walk & !is_sac),
    H        = sum(is_hit),
    Singles  = sum(is_1b),
    Doubles  = sum(is_2b),
    Triples  = sum(is_3b),
    HR       = sum(is_hr),
    BB       = sum(is_walk),
    K        = sum(is_k),
    Runs     = sum(RunsScored),
    GDP      = sum(is_gdp),
    GDP_opp  = sum(is_gdp_opp),
    SAC      = sum(is_sac),
    Errors   = sum(is_error),
    FC       = sum(is_fc),
    avg_EV   = mean(ExitSpeed, na.rm = TRUE),
    avg_LA   = mean(Angle, na.rm = TRUE),
    avg_Dist = mean(Distance, na.rm = TRUE),
    max_EV   = max(ExitSpeed, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  mutate(
    avg_EV = ifelse(is.nan(avg_EV) | is.infinite(avg_EV), NA_real_, avg_EV),
    max_EV = ifelse(is.nan(max_EV) | is.infinite(max_EV), NA_real_, max_EV),
    avg_LA = ifelse(is.nan(avg_LA) | is.infinite(avg_LA), NA_real_, avg_LA),
    AVG  = ifelse(AB > 0, H / AB, 0),
    OBP  = ifelse(PA > 0, (H + BB) / PA, 0),
    SLG  = ifelse(AB > 0, (Singles + 2*Doubles + 3*Triples + 4*HR) / AB, 0),
    OPS  = OBP + SLG
  )

batter_stats <- batter_stats %>%
  filter(PA >= 10)
```


```{r sabermetrics}
# D1 College Baseball Linear Weights (Frey, 2019 NCAA Play-by-Play Data)
# Source: rfrey22.medium.com/collegiate-linear-weights
wBB        <- 0.64
w1B        <- 0.79
w2B        <- 1.12
w3B        <- 1.40
wHR        <- 1.74
lg_wOBA    <- 0.363
wOBA_scale <- 1.194

batter_stats <- batter_stats %>%
  mutate(
    wOBA = ifelse(PA > 0,
                  (wBB*BB + w1B*Singles + w2B*Doubles + w3B*Triples + wHR*HR) / PA, 0),
    wRAA      = round((wOBA - lg_wOBA) / wOBA_scale * PA, 3),
    SB_proxy  = Triples + FC + Errors,
    wSB       = round(SB_proxy * 0.20, 3),
    Exp_Runs  = HR*1.74 + Doubles*1.12 + Triples*1.40 + Singles*0.79 + BB*0.64,
    UBR_raw   = Runs - Exp_Runs,
    UBR       = round(rescale(UBR_raw, to = c(-2, 2)), 3),
    GDP_rate  = ifelse(GDP_opp > 0, GDP / GDP_opp, 0),
    wGDP      = round(-0.37 * GDP, 3),
    GDP_avoid = round((1 - GDP_rate) * 100, 1),
    XBT_pct   = round(ifelse((Doubles + Triples) > 0,
                             Triples / (Doubles + Triples) * 100, 0), 1),
    speed_input      = coalesce(avg_EV, 85) + (FC + Errors + Triples) * 2.5,
    sprint_speed_est = round(rescale(speed_input, to = c(21, 32)), 1),
    BsR    = round(wSB + UBR + wGDP, 3),
    WAR_PC = round((wRAA + BsR) / 10, 3)
  )

radar_df <- batter_stats %>%
  transmute(
    Batter, BatterTeam,
    OBP         = round(rescale(OBP,              to=c(0,100), from=c(0.0, 0.6)),  1),
    SLG         = round(rescale(SLG,              to=c(0,100), from=c(0.0, 1.0)),  1),
    `XBT%`      = round(rescale(XBT_pct,          to=c(0,100), from=c(0, 100)),    1),
    `GDP Avoid` = round(rescale(GDP_avoid,         to=c(0,100), from=c(0, 100)),    1),
    `Spd Proxy` = round(rescale(sprint_speed_est,  to=c(0,100), from=c(21, 32)),   1),
    wOBA        = round(rescale(wOBA,              to=c(0,100), from=c(0.20, 0.60)),1),
    BsR         = round(rescale(BsR,               to=c(0,100), from=c(-3, 3)),    1)
  ) %>%
  mutate(across(where(is.numeric), ~pmin(pmax(., 0), 100)))
```

## Game Summary

````{r game-summary-cards, results='asis'}
my_team_stats <- batter_stats %>% filter(BatterTeam == MY_TEAM)

total_pa       <- sum(my_team_stats$PA)
avg_ev_my      <- round(mean(my_team_stats$avg_EV, na.rm=TRUE), 1)
avg_bsr_my     <- round(mean(my_team_stats$BsR), 2)
top_bsr_player <- my_team_stats %>% slice_max(BsR, n=1) %>% pull(Batter) %>% str_extract("^[^,]+")
top_spd_player <- my_team_stats %>% slice_max(sprint_speed_est, n=1) %>% pull(Batter) %>% str_extract("^[^,]+")
team_war       <- round(sum(my_team_stats$WAR_PC), 3)

cat('<div class="metric-grid">')
cat(sprintf('<div class="metric-card"><div class="label">GAMES</div><div class="value">%d</div><div class="sub">2026 season</div></div>', total_games))
cat(sprintf('<div class="metric-card"><div class="label">TOTAL PA</div><div class="value">%d</div><div class="sub">%s</div></div>', total_pa, MY_TEAM))
cat(sprintf('<div class="metric-card"><div class="label">AVG EXIT VELO</div><div class="value">%s</div><div class="sub">mph</div></div>', avg_ev_my))
cat(sprintf('<div class="metric-card"><div class="label">TEAM AVG BsR</div><div class="value">%s</div><div class="sub">runs above avg</div></div>', avg_bsr_my))
cat(sprintf('<div class="metric-card"><div class="label">TOP BsR</div><div class="value">%s</div><div class="sub">best baserunner</div></div>', top_bsr_player))
cat(sprintf('<div class="metric-card"><div class="label">TEAM WAR-PC</div><div class="value">%s</div><div class="sub">baserunning WAR</div></div>', team_war))
cat('</div>')
````

## Baserunning Metrics — `r MY_TEAM`

### BsR — Base Running Runs Above Average

```{r bsr-bar, fig.width=9, fig.height=5.5}
batter_stats %>%
  filter(BatterTeam == MY_TEAM) %>%
  mutate(Name = str_extract(Batter, "^[^,]+"),
         Name = fct_reorder(Name, BsR)) %>%
  ggplot(aes(x = Name, y = BsR, fill = BsR)) +
  geom_col(width = 0.7, color = "white", linewidth = 0.3) +
  geom_hline(yintercept = 0, linewidth = 0.7, linetype = "dashed", color = "grey40") +
  geom_text(aes(label = round(BsR, 2),
                hjust = ifelse(BsR >= 0, -0.2, 1.2)),
            size = 3.5, fontface = "bold") +
  scale_fill_gradient2(low = "#d73027", mid = "#ffffbf", high = "#1a9850", midpoint = 0) +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) +
  coord_flip() +
  labs(title = paste0(MY_TEAM, " — BsR (Base Running Runs)"),
       subtitle = "BsR = wSB + UBR + wGDP  |  0 = league-average runner",
       x = NULL, y = "BsR (runs above average)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title    = element_text(face = "bold", hjust = 0.5),
        plot.subtitle = element_text(color = "grey45", hjust = 0.5),
        panel.grid.major.y = element_blank())
```

### wSB & UBR — Stolen Base Value vs. Base Running Value

```{r wsb-ubr, fig.width=9, fig.height=6}
batter_stats %>%
  filter(BatterTeam == MY_TEAM) %>%
  mutate(Name = str_extract(Batter, "^[^,]+")) %>%
  ggplot(aes(x = wSB, y = UBR, label = Name)) +
  annotate("rect", xmin=-Inf, xmax=0, ymin=0,    ymax=Inf,  fill="#fee0d2", alpha=0.35) +
  annotate("rect", xmin=0,    xmax=Inf, ymin=0,  ymax=Inf,  fill="#e5f5e0", alpha=0.35) +
  annotate("rect", xmin=-Inf, xmax=0, ymin=-Inf, ymax=0,    fill="#fee0d2", alpha=0.35) +
  annotate("rect", xmin=0,    xmax=Inf, ymin=-Inf, ymax=0,  fill="#fff7bc", alpha=0.35) +
  annotate("text", x=Inf,  y=Inf,  label="Elite", hjust=1.2, vjust=1.4, color="grey40", size=3.5, fontface="italic") +
  annotate("text", x=-Inf, y=Inf,  label="not stealing", hjust=-0.2, vjust=1.3, color="grey40", size=3.2, fontface="italic") +
  geom_vline(xintercept = 0, color = "grey50", linewidth = 0.6) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.6) +
  geom_point(aes(color = BsR), size = 4.5) +
  geom_text_repel(size = 3.4, max.overlaps = 20, box.padding = 0.5) +
  scale_color_gradient2(low="#d73027", mid="#ffffbf", high="#1a9850", midpoint=0, name="BsR") +
  labs(title    = paste0(MY_TEAM, " — wSB vs. UBR"),
       subtitle = "Upper-right = best overall baserunner; green = above average",
       x = "wSB (stolen-base run value proxy)",
       y = "UBR (base running value ex-SB)") +
  theme_minimal(base_size = 13) +
  theme(plot.title    = element_text(face="bold", hjust=0.5),
        plot.subtitle = element_text(color="grey45", hjust=0.5),
        legend.position = "right")
```

### XBT% — Extra Bases Taken Rate

```{r xbt-bar, fig.width=9, fig.height=5}
batter_stats %>%
  filter(BatterTeam == MY_TEAM) %>%
  mutate(Name = str_extract(Batter, "^[^,]+"),
         Name = fct_reorder(Name, XBT_pct)) %>%
  ggplot(aes(x = Name, y = XBT_pct, fill = XBT_pct)) +
  geom_col(width = 0.7, color = "white") +
  geom_text(aes(label = paste0(round(XBT_pct, 0), "%")),
            hjust = -0.2, size = 3.4, fontface = "bold") +
  scale_fill_gradient(low = "#fee8c8", high = "#e34a33") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  coord_flip() +
  labs(title    = paste0(MY_TEAM, " — Extra Bases Taken %"),
       subtitle = "XBT% = Triples ÷ (Doubles + Triples) × 100",
       x = NULL, y = "XBT%") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title    = element_text(face="bold", hjust=0.5),
        plot.subtitle = element_text(color="grey45", hjust=0.5),
        panel.grid.major.y = element_blank())
```

### Sprint Speed Proxy
```{r sprint-bar, fig.width=9, fig.height=5}
batter_stats %>%
  filter(BatterTeam == MY_TEAM) %>%
  mutate(Name = str_extract(Batter, "^[^,]+"),
         Name = fct_reorder(Name, sprint_speed_est)) %>%
  ggplot(aes(x = Name, y = sprint_speed_est, fill = sprint_speed_est)) +
  geom_col(width = 0.7, color = "white") +
  geom_text(aes(label = paste0(round(sprint_speed_est, 1), " ft/s")),
            hjust = -0.15, size = 3.4, fontface = "bold") +
  scale_fill_gradient(low = "#deebf7", high = "#2171b5") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18)), limits = c(0, NA)) +
  coord_flip() +
  labs(title    = paste0(MY_TEAM, " — Sprint Speed Proxy"),
       subtitle = "Estimated from Exit Velo + aggressive base events (Triples, FC, Errors reached)",
       x = NULL, y = "Sprint Speed Proxy (ft/sec)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title    = element_text(face="bold", hjust=0.5),
        plot.subtitle = element_text(color="grey45", hjust=0.5, size=9),
        panel.grid.major.y = element_blank())
```

### wGDP — Double Play Run Value

```{r wgdp-bar, fig.width=9, fig.height=5}
batter_stats %>%
  filter(BatterTeam == MY_TEAM) %>%
  mutate(Name = str_extract(Batter, "^[^,]+"),
         Name = fct_reorder(Name, wGDP)) %>%
  ggplot(aes(x = Name, y = wGDP, fill = wGDP)) +
  geom_col(width = 0.7, color = "white") +
  geom_hline(yintercept = 0, linewidth = 0.7, linetype = "dashed", color = "grey40") +
  geom_text(aes(label = round(wGDP, 3),
                hjust = ifelse(wGDP >= 0, -0.2, 1.2)),
            size = 3.4) +
  scale_fill_gradient2(low="#d73027", mid="#ffffbf", high="#1a9850", midpoint=0) +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) +
  coord_flip() +
  labs(title    = paste0(MY_TEAM, " — wGDP (Double Play Run Value)"),
       subtitle = "Positive = fewer DPs than expected | −0.37 runs per GDP",
       x = NULL, y = "wGDP (runs)") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title    = element_text(face="bold", hjust=0.5),
        plot.subtitle = element_text(color="grey45", hjust=0.5, size=9),
        panel.grid.major.y = element_blank())
```

### WAR-PC — Baserunning Partial Credit WAR

```{r war-pc, fig.width=9, fig.height=5}
batter_stats %>%
  filter(BatterTeam == MY_TEAM) %>%
  mutate(Name = str_extract(Batter, "^[^,]+"),
         Name = fct_reorder(Name, WAR_PC)) %>%
  ggplot(aes(x = Name, y = WAR_PC)) +
  geom_segment(aes(xend = Name, yend = 0), color = "grey65", linewidth = 1.2) +
  geom_point(aes(color = WAR_PC), size = 5.5) +
  geom_hline(yintercept = 0, linewidth = 0.7, linetype = "dashed", color = "grey40") +
  geom_text(aes(label = round(WAR_PC, 3),
                hjust = ifelse(WAR_PC >= 0, -0.4, 1.4)),
            size = 3.2, fontface = "bold") +
  scale_color_gradient2(low="#d73027", mid="#ffffbf", high="#1a9850", midpoint=0) +
  scale_y_continuous(expand = expansion(mult = c(0.2, 0.2))) +
  coord_flip() +
  labs(title    = paste0(MY_TEAM, " — WAR-PC (Partial Contribution WAR)"),
       subtitle = "WAR-PC = (wRAA + BsR) ÷ 10  |  2026 Season",
       x = NULL, y = "WAR-PC") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title    = element_text(face="bold", hjust=0.5),
        plot.subtitle = element_text(color="grey45", hjust=0.5),
        panel.grid.major.y = element_blank())
```

##  Radar Maps — Individual Baserunning Profiles

> Each axis is normalized 0–100 across all qualifying batters in the 2026 season.

```{r radar-grid, fig.width=12, fig.height=4 * ceiling(nrow(radar_df %>% filter(BatterTeam == MY_TEAM)) / 3)}

draw_radar_gg <- function(batter_name, r_df, color_fill, color_border) {
  row <- r_df %>% filter(Batter == batter_name)
  if (nrow(row) == 0) return(invisible(NULL))
  
  metric_cols <- c("OBP","SLG","XBT%","GDP Avoid","Spd Proxy","wOBA","BsR")
  
  # Extract as a named numeric vector explicitly
  vals <- unlist(as.numeric(row[1, metric_cols]))
  vals <- replace(vals, is.na(vals), 0)   # replace any NAs with 0
  
  rdata <- as.data.frame(rbind(
    max    = rep(100, length(metric_cols)),
    min    = rep(0,   length(metric_cols)),
    player = vals
  ))
  colnames(rdata) <- metric_cols

  radarchart(rdata, axistype=1,
             pcol=color_border, pfcol=adjustcolor(color_fill, alpha.f=0.35),
             plwd=2.5, plty=1, cglcol="grey75", cglty=1, cglwd=0.6,
             axislabcol="grey40", vlcex=0.78,
             caxislabels=c("0","25","50","75","100"),
             title=str_extract(batter_name, "^[^,]+"))
}

team_batters <- radar_df %>% filter(BatterTeam == MY_TEAM) %>% pull(Batter)
n  <- length(team_batters)
nc <- 3
nr <- ceiling(n / nc)

par(mfrow=c(nr, nc), mar=c(1.5,1.5,2.5,1.5), oma=c(0,0,3,0), bg="#F9FAFB")

fill_col   <- TEAM_COLORS[MY_TEAM]
border_col <- TEAM_ACCENT[MY_TEAM]

for (b in team_batters) draw_radar_gg(b, radar_df, fill_col, border_col)

blanks <- nc * nr - n
if (blanks > 0) for (i in seq_len(blanks)) plot.new()

mtext(paste("Baserunning Radar Maps —", MY_TEAM), outer=TRUE, cex=1.3, font=2, col=fill_col, line=1)
```

##  Full Metrics Table

```{r metrics-table}
batter_stats %>%
  filter(BatterTeam == MY_TEAM) %>%
  arrange(desc(BsR)) %>%
  mutate(Name = str_extract(Batter, "^[^,]+")) %>%
  select(Name, PA, H, Doubles, Triples, HR, BB, K,
         AVG, OBP, SLG, wOBA, wRAA, wSB, UBR, wGDP, BsR,
         XBT_pct, GDP, sprint_speed_est, WAR_PC) %>%
  rename(`2B`=Doubles, `3B`=Triples, `XBT%`=XBT_pct,
         `Spd Est`=sprint_speed_est, `WAR-PC`=WAR_PC) %>%
  mutate(across(where(is.double), ~round(., 3))) %>%
  kbl(align = c("l", rep("c", 20))) %>%
  kable_styling(bootstrap_options = c("striped","hover","condensed","responsive"),
                full_width=TRUE, font_size=12) %>%
  add_header_above(c(" "=8, "Traditional"=4, "Sabermetric Baserunning"=5, "Speed/GDP"=3, "Value"=1)) %>%
  column_spec(1, bold=TRUE) %>%
 # column_spec(14, background="#e5f5e0") %>%
  column_spec(21, background="#eef4fb")
```

## Team Trend by Game — Comparative Dashboard
````{r season-trend, fig.width=11, fig.height=5.5}
# BsR trend across the season by game date
raw %>%
  filter(BatterTeam == MY_TEAM) %>%
  arrange(PitchNo) %>%
  group_by(Batter, BatterTeam, Inning, PAofInning, Date, GameID) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(
    is_hit  = PlayResult %in% c("Single","Double","Triple","HomeRun"),
    is_walk = KorBB == "Walk",
    is_sac  = PlayResult == "Sacrifice",
    is_gdp  = PlayResult == "Out" & TaggedHitType == "GroundBall" & as.numeric(OutsOnPlay) == 2,
    is_fc   = PlayResult == "FieldersChoice",
    is_error = PlayResult == "Error",
    is_3b   = PlayResult == "Triple"
  ) %>%
  group_by(GameID, Date) %>%
  summarise(
    PA      = n(),
    GDP     = sum(is_gdp),
    SB_prox = sum(is_3b) + sum(is_fc) + sum(is_error),
    Runs    = sum(as.numeric(RunsScored), na.rm=TRUE),
    Exp_R   = sum(PlayResult=="HomeRun")*1.74 + sum(PlayResult=="Double")*1.12 +
              sum(PlayResult=="Triple")*1.4  + sum(PlayResult=="Single")*0.79,
    .groups = "drop"
  ) %>%
  mutate(
    wSB_g  = SB_prox * 0.20,
    UBR_g  = rescale(Runs - Exp_R, to = c(-2, 2)),
    wGDP_g = -0.37 * GDP,
    BsR_g  = wSB_g + UBR_g + wGDP_g,
    Date   = mdy(Date)
  ) %>%
  ggplot(aes(x = Date, y = BsR_g)) +
  geom_line(color = TEAM_COLORS[MY_TEAM], linewidth = 1) +
  geom_point(aes(color = BsR_g), size = 3) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_smooth(method = "loess", se = TRUE, color = TEAM_ACCENT[MY_TEAM],
              fill = TEAM_ACCENT[MY_TEAM], alpha = 0.15, linewidth = 0.8) +
  scale_color_gradient2(low="#d73027", mid="#ffffbf", high="#1a9850", midpoint=0) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 week") +
  labs(
    title    = paste0(MY_TEAM, " — BsR Trend by Game (2026 Season)"),
    subtitle = "Each point = one game | smoothed trend line",
    x = NULL, y = "BsR (game total)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none",
        plot.title    = element_text(face="bold", hjust=0.5),
        plot.subtitle = element_text(color="grey45", hjust=0.5),
        axis.text.x   = element_text(angle=45, hjust=1))

````

## Methodology & Metric Definitions

| Metric | Formula (Trackman Proxy) | True Statcast Source |
|--------|--------------------------|----------------------|
| **BsR** | wSB + UBR + wGDP | FanGraphs / Baseball Savant |
| **wSB** | (Triples + FC + ROE) × 0.20 | SB × +0.20 + CS × −0.38 |
| **UBR** | Rescaled (Actual Runs − Expected Runs) | Retrosheet base-to-base tracking |
| **wGDP** | −0.37 × GDP | Same formula |
| **XBT%** | Triples ÷ (2B + 3B) × 100 | Extra bases taken / opportunities |
| **Sprint Speed** | Rescaled (avg EV + aggressive events) | Statcast 2–8 sec GPS window |
| **wOBA** | (wBB×BB + w1B×1B + … + wHR×HR) / PA | FanGraphs linear weights |
| **WAR-PC** | (wRAA + BsR) / 10 | Aggregated WAR model |

**Linear Weights:** BB=0.64 | 1B=0.79 | 2B=1.12 | 3B=1.40 | HR=1.74 | lg wOBA=0.363 | wOBA Scale=1.194

*Report generated from Trackman game export. Knit with:*
`rmarkdown::render("baserunning_report.Rmd", params = list(data_file = "2025-2026 (2).csv", my_team = "YOUR_TEAM"))`