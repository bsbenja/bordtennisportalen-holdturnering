# Webscraping af holdturning fra Bordtennisportalen

Dette program henter automatisk kampdata fra **Bordtennisportalen.dk** og laver en CSV‑fil, som kan importeres i din kalender.

## Hvad programmet gør

- Henter alle kampe i den valgte række.
- Finder dit holds egne kampe.
- Udregner start- og sluttider.
- Laver kampbeskrivelser og titler.
- Eksporterer en kalenderklar CSV‑fil.

## Sådan bruger du programmet

1.  Åbn Positron eller VS Code.
2.  Åbn R‑scriptet.
3.  Ret evt. disse værdier i toppen:
    - `StillingURL_V` – link til rækkens stilling
    - `Titel_V` – navn på rækken
    - `Klub_V` – dit hold
    - `Hjemmebane_V` – din hjemmebane
    - `VarighedTimer_V` – kampens varighed\
4.  Kør scriptet.
5.  CSV‑filen **bordtennisportalen-holdturnering.csv** bliver oprettet i mappen.

## Output

CSV‑filen kan importeres i fx:

- Google Kalender
- Outlook
- Apple Kalender

Den indeholder:

- Kampens titel
- Start- og sluttid
- Beskrivelse
- Spillested