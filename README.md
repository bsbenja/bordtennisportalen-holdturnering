# Webscraping af holdturnering fra Bordtennisportalen

Dette projekt henter automatisk kampdata fra **Bordtennisportalen.dk** og laver kalenderfiler (`.ics`), som man kan abonnere på i Google Calendar, Outlook eller Apple Kalender. Kalenderne opdateres automatisk hver dag via GitHub Actions.

## Hvad projektet gør

- Henter alle kampe i den valgte række.
- Finder det angivne holds egne kampe.
- Udregner start- og sluttider (korrekt tidszone, håndterer sommer-/vintertid).
- Laver kampbeskrivelser og titler.
- Eksporterer en `.ics`-fil pr. konfigureret kalender.
- Kører automatisk hver dag kl. 00:00 UTC via GitHub Actions og pusher opdaterede filer til repoet.

## Projektstruktur

| Fil | Indhold |
|------------------------------------|------------------------------------|
| `funktioner.R` | Al logik: webscraping, beregning af tider/titler/beskrivelser og ICS-generering. Skal normalt ikke ændres. |
| `kalendere.R` | Konfiguration – én række pr. kalender. Køres ind af GitHub Actions. |
| `.github/workflows/opdater-kalender.yml` | Kører `kalendere.R` dagligt og committer opdaterede `.ics`-filer. |
| `*.ics` | De genererede kalenderfiler, én pr. række/hold, navngivet efter `Filnavn_V`. |

## Tilføj en ny kalender

Åbn `kalendere.R` og tilføj en ny række i `Kalendere_V`-tabellen:

- **`Filnavn_V`** – bliver navnet på `.ics`-filen (uden endelse) og skal være unikt.
- **`StillingURL_V`** – findes på bordtennisportalen.dk under "Stilling" for den pågældende række.
- **`Titel_V`** – vises som kampenes titel og som kalenderens navn, når man abonnerer.
- **`Klub_V`** / **`Hjemmebane_V`** – dit hold og din hjemmebane, bruges til at afgøre hjemme-/udekampe.
- **`VarighedTimer_V`** – hvor mange timer en enkelt kamp varer.

Commit og push – GitHub Actions opretter og opdaterer filen automatisk ved næste kørsel (eller kør workflowet manuelt under fanen *Actions*).

## Abonnér på en kalender

Når en `.ics`-fil ligger i repoet, er den tilgængelig på:

```         
https://raw.githubusercontent.com/bsbenja/bordtennisportalen-holdturnering/main/<Filnavn_V>.ics
```

**Google Calendar:** "Andre kalendere" → "+" → "Fra URL" → indsæt linket. Google tjekker filen periodisk (typisk hver 12-24 timer) og henter ændringer automatisk.

**Outlook / Apple Kalender:** har tilsvarende "abonnér via URL"-funktioner, hvor det samme link kan bruges.

## Køre projektet lokalt

``` r
source("kalendere.R")
```

Dette kører `funktioner.R`, installerer manglende R-pakker automatisk, og genererer én `.ics`-fil pr. række i `Kalendere_V`.

## Automatisk opdatering (GitHub Actions)

Workflowet i `.github/workflows/opdater-kalender.yml` kører dagligt kl. 00:00 UTC, kører `kalendere.R` og committer eventuelle ændrede `.ics`-filer tilbage til repoet. Kør det manuelt via fanen *Actions* → *Opdater kampkalendere* → *Run workflow* for at teste.