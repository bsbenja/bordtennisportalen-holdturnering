source("funktioner.R")

# Én linje pr. kalender/abonnement. Filnavn_V bliver navnet på .ics-filen
# og skal være unikt. StillingURL_V finder du på bordtennisportalen.dk
# under "Stilling" for den pågældende række.

Kalendere_V <- tribble(
  ~Filnavn_V,                       ~StillingURL_V,                                                                           ~Titel_V,                    ~Klub_V,     ~Hjemmebane_V, ~VarighedTimer_V,
  "1-division-grundspil-2026-27", "https://bordtennisportalen.dk/DBTU/HoldTurnering/Stilling/#2,42026,15477,4006,4000,,,,", "Test",  "Sisu/MBK",  "Sisu/MBK",    3,
  "2-division-grundspil-2026-27", "https://bordtennisportalen.dk/DBTU/HoldTurnering/Stilling/#2,42026,15478,4006,4000,,,,", "🏓 2. Division Grundspil",  "Sisu/MBK",  "Sisu/MBK",    3,
)

pwalk(Kalendere_V, opdater_kalender)