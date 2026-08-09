# Input ----
#+ eval=F, warning=F, message=F

StillingURL_V   <- "https://bordtennisportalen.dk/DBTU/HoldTurnering/Stilling/#2,42026,15477,4006,4000,,,,"
Titel_V         <- "🏓 1. Division Grundspil"
Klub_V          <- "Sisu/MBK"
Hjemmebane_V    <- "Sisu/MBK"
VarighedTimer_V <- 3

# Opsætning ----
#+ eval=F, warning=F, message=F

for (Packages_V in c(
  "dplyr", "rvest", "lubridate", "stringr", "purrr")) {
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
    "Location"    = Spillested_RD,
    # Rå datoklokkeslæt bevares til ICS-eksporten nedenfor
    StartDatoTid_DW,
    SlutDatoTid_DW)

# Fil (CSV) ----
#+ eval=F, warning=F, message=F

write.table(
  x = DM_Holdturnering %>% select(-StartDatoTid_DW, -SlutDatoTid_DW),
  file = "bordtennisportalen-holdturnering.csv", sep = ",", row.names = F)

# ICS ----
#+ eval=F, warning=F, message=F

# as_datetime() i CALC_Holdturnering mærker klokkeslættet som UTC, men det er
# reelt dansk lokal tid (som vist på bordtennisportalen.dk). til_utc() retter
# det ved først at "omdøbe" tidszonen til dansk lokal tid, og derefter regne
# om til det faktiske UTC-tidspunkt (håndterer sommer-/vintertid automatisk).
til_utc <- function(dansk_lokal_tid) {
  dansk_lokal_tid %>%
    force_tz("Europe/Copenhagen") %>%
    with_tz("UTC")
}

# Escaper tegn som ICS-formatet (RFC 5545) kræver escapes for
escape_ics_tekst <- function(tekst) {
  tekst %>%
    str_replace_all("\\\\", "\\\\\\\\") %>%
    str_replace_all(",", "\\\\,") %>%
    str_replace_all(";", "\\\\;") %>%
    str_replace_all("\r\n|\n|\r", "\\\\n")
}

# Folder lange linjer, så ingen linje overstiger 75 tegn (RFC 5545-krav).
# Fortsættelseslinjer skal starte med et enkelt mellemrum.
fold_ics_linje <- function(linje, maks_tegn = 75) {
  if (str_length(linje) <= maks_tegn) return(linje)
  
  foerste <- str_sub(linje, 1, maks_tegn)
  rest <- str_sub(linje, maks_tegn + 1)
  
  stykker <- character()
  while (str_length(rest) > 0) {
    stykker <- c(stykker, str_sub(rest, 1, maks_tegn - 1))
    rest <- str_sub(rest, maks_tegn)
  }
  
  paste(c(foerste, stykker), collapse = "\r\n ")
}

lav_ics <- function(data, filnavn) {
  
  Nu_V <- format(with_tz(Sys.time(), "UTC"), "%Y%m%dT%H%M%SZ")
  
  VEvents_V <- data %>%
    mutate(Row_DW = row_number()) %>%
    pmap_chr(function(Subject, `Start Date`, `Start Time`, `End Date`, `End Time`,
                       Description, Location, StartDatoTid_DW, SlutDatoTid_DW, Row_DW) {
      
      DtStart_V <- format(til_utc(StartDatoTid_DW), "%Y%m%dT%H%M%SZ")
      DtEnd_V   <- format(til_utc(SlutDatoTid_DW), "%Y%m%dT%H%M%SZ")
      Uid_V     <- paste0(Row_DW, "-", DtStart_V, "@sisumbk-kampkalender")
      
      Linjer_V <- c(
        "BEGIN:VEVENT",
        paste0("UID:", Uid_V),
        paste0("DTSTAMP:", Nu_V),
        paste0("DTSTART:", DtStart_V),
        paste0("DTEND:", DtEnd_V),
        paste0("SUMMARY:", escape_ics_tekst(Subject)),
        paste0("DESCRIPTION:", escape_ics_tekst(Description)),
        paste0("LOCATION:", escape_ics_tekst(Location)),
        "END:VEVENT"
      )
      
      Linjer_V %>%
        map_chr(fold_ics_linje) %>%
        paste(collapse = "\r\n")
    }) %>%
    paste(collapse = "\r\n")
  
  Kalender_V <- paste(
    "BEGIN:VCALENDAR",
    "VERSION:2.0",
    paste0("PRODID:-//", Klub_V, "//Kampkalender//DA"),
    "CALSCALE:GREGORIAN",
    VEvents_V,
    "END:VCALENDAR",
    sep = "\r\n"
  )
  
  # skrives som rå bytes, så der ikke sker dobbelt CRLF-konvertering
  Con_V <- file(filnavn, open = "wb")
  writeChar(Kalender_V, Con_V, eos = NULL, useBytes = TRUE)
  close(Con_V)
  
  invisible(Kalender_V)
}

lav_ics(DM_Holdturnering, "bordtennisportalen-holdturnering.ics")