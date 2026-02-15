# Input ----
#+ eval=F, warning=F, message=F

StillingURL_V   <- "https://bordtennisportalen.dk/DBTU/HoldTurnering/Stilling/#2,42025,15034,4006,4000,,,,"
Titel_V         <- "🏓 1. Division Grundspil"
Klub_V          <- "Sisu/MBK 1"
Hjemmebane_V    <- "Sisu/MBK"
VarighedTimer_V <- 3

# Opsætning ----
#+ eval=F, warning=F, message=F

for (Packages_V in c(
  "dplyr", "rvest", "lubridate", "stringr")) {
  if (!requireNamespace(Packages_V, quietly = TRUE)) {
    install.packages(Packages_V, dependencies = TRUE)
  }
  suppressWarnings(suppressPackageStartupMessages(library(Packages_V, character.only = TRUE)))
  Sys.setlocale("LC_TIME", "English")
}

# STG_Holdturnering ----

AlleKampeURL_V  <- paste0(
  substr(StillingURL_V,  1, 49), "Udskriv",
  substr(StillingURL_V, 50, 58), "?page=4&season=",
  substr(StillingURL_V, 62, 67), "&region=",
  substr(StillingURL_V, 79, 83), "&agegroup=",
  substr(StillingURL_V, 74, 77), "&group=",
  substr(StillingURL_V, 68, 72), "&team=&match=&club=&player=") # Webscraping

STG_Holdturnering <- data.frame(
  "Titel_RD"      = read_html(AlleKampeURL_V) %>% html_nodes("h2")              %>% html_text(),
  "Overskrift_RD" = read_html(AlleKampeURL_V) %>% html_nodes("h3")              %>% html_text(),
  "Tid_RD"        = read_html(AlleKampeURL_V) %>% html_nodes(".time")           %>% html_text(),
  "Kampnr_RD"     = read_html(AlleKampeURL_V) %>% html_nodes(".matchno")        %>% html_text(),
  "Hjemmehold_RD" = read_html(AlleKampeURL_V) %>% html_nodes(".matchno+ .team") %>% html_text(),
  "Udehold_RD"    = read_html(AlleKampeURL_V) %>% html_nodes(".team+ .team")    %>% html_text(),
  "Spillested_RD" = read_html(AlleKampeURL_V) %>% html_nodes(".venue")          %>% html_text(),
  "Resultat_RD"   = read_html(AlleKampeURL_V) %>% html_nodes(".score")          %>% html_text(),
  "Point_RD"      = read_html(AlleKampeURL_V) %>% html_nodes(".points")         %>% html_text(),
  stringsAsFactors = F) %>% slice(-1) %>%
  mutate("Rang_RD" = row_number()) %>% select(Rang_RD, everything())

# CALC_Holdturnering ----
#+ eval=F, warning=F, message=F

CALC_Holdturnering <- STG_Holdturnering %>%
  
  mutate(EgetHold_DW = case_when(
    grepl(Klub_V, Hjemmehold_RD) | grepl(Klub_V, Udehold_RD) ~ Klub_V,
    TRUE ~ NA_character_)) %>%
  
  mutate(ModstanderHold_DW = case_when(
    is.na(EgetHold_DW) ~ NA_character_,
    !grepl(Klub_V, Hjemmehold_RD) ~ Hjemmehold_RD,
    !grepl(Klub_V, Udehold_RD) ~ Udehold_RD)) %>%
  
  # StartDatoTid_DW
  mutate(StartDatoTid_DW = as_datetime(paste0(
    substr(Tid_RD, 10, 13), "-",
    substr(Tid_RD,  7,  8), "-",
    substr(Tid_RD,  4,  5), "-",
    substr(Tid_RD, 15, 19), ":00"))) %>%
  
  # StartDato_DW
  mutate(StartDato_DW = format(StartDatoTid_DW, "%d/%m/%Y")) %>%
  
  # StartTid_DW
  mutate(StartTid_DW = format(StartDatoTid_DW, "%I:%M %p")) %>%
  
  #SlutDatoTid_DW
  arrange(StartDatoTid_DW, EgetHold_DW, Rang_RD) %>%
  group_by(StartDato_DW, Spillested_RD, EgetHold_DW) %>%
  mutate(SlutDatoTid_DW = StartDatoTid_DW + hours(
    ifelse(!is.na(EgetHold_DW), VarighedTimer_V*sum(!is.na(EgetHold_DW)), VarighedTimer_V))) %>%
  ungroup() %>%
  
  # SlutDato_DW
  mutate(SlutDato_DW = format(SlutDatoTid_DW,  "%d/%m/%Y")) %>%
  
  # SlutTid_DW
  mutate(SlutTid_DW = format(SlutDatoTid_DW,  "%I:%M %p")) %>%
  
  # Titel_DW
  arrange(StartDatoTid_DW, EgetHold_DW, Rang_RD) %>%
  group_by(StartDato_DW, Spillested_RD, EgetHold_DW) %>%
  mutate(Titel_DW = case_when(
    is.na(EgetHold_DW) ~ Titel_V,
    TRUE ~ paste0(
      Titel_V, " på ",
      ifelse(grepl(Hjemmebane_V, Spillested_RD), "hjemmebane mod ", "udebane mod "),
      str_c(ModstanderHold_DW, collapse = " + ")))) %>%
  ungroup() %>%
  
  # Beskrivelse_DW
  arrange(StartDatoTid_DW, EgetHold_DW, Rang_RD) %>%
  group_by(StartDato_DW, Spillested_RD) %>%
  mutate(Beskrivelse_DW = paste0(
    str_c(paste(format(StartDatoTid_DW, "Kl. %H:%M:"), Hjemmehold_RD, "VS", Udehold_RD), collapse = "\n"),
    "\n",
    "\n",
    "--------------------",
    "\n",
    Titel_RD, ": ", Overskrift_RD, " ", format(StartDatoTid_DW, "%d.%m.%Y."),
    "\n",
    "\n",
    "Link til stilling i rækken:",
    "\n",
    StillingURL_V, "0")) %>%
  ungroup() %>%
  
  arrange(Rang_RD)

# DM_Holdturnering ----
#+ eval=F, warning=F, message=F

DM_Holdturnering <- CALC_Holdturnering %>%
  arrange(StartDatoTid_DW, Rang_RD) %>%
  filter(!is.na(EgetHold_DW)) %>%
  distinct(StartDato_DW, .keep_all = T) %>%
  select(
    "Subject"     = Titel_DW,
    "Start Date"  = StartDato_DW,
    "Start Time"  = StartTid_DW,
    "End Date"    = SlutDato_DW,
    "End Time"    = SlutTid_DW,
    "Description" = Beskrivelse_DW,
    "Location"    = Spillested_RD)

# Fil ----
#+ eval=F, warning=F, message=F

write.table(x = DM_Holdturnering, file = "bordtennisportalen-holdturnering.csv", sep = ",", row.names = F)
shell.exec("")