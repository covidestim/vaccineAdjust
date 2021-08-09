abbr2statePath <- system.file(
  "abbr2state", # Refers to subdirectory of 'inst/'
  "abbr2state.csv",
  package = "vaccineAdjust"
)

abbr2stateTbl <- read_csv(abbr2statePath)

abbr2state <- abbr2stateTbl$NAME
names(abbr2state) <- abbr2stateTbl$ABBREVIATION

