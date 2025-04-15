# Namensliste generieren
library(tidyverse)
files <- list.files(getwd(), pattern = ".csv")

namensliste <- files[length(files)] %>%
  read.csv() %>%
  filter(
    str_detect(
      N001_01,
      pattern = "test",
      negate = TRUE
    )
  ) %>%
  pull(N001_01) %>%
  str_replace_all(
    pattern = c(
      " " = "",
      "@stud.hmtm-hannover.de" = ""
    )
  ) %>%
  sort() %>%
  str_to_lower() %>%
  unique() %>%
  str_c("\n")

file_con <- file("Namensliste_Experiment.md")
writeLines(c("<h1>soscisurvey.de/ki-melodie-24/</h1>\n",
             "<style>.columns{column-count: 3;}</style>",
             "<div class='columns' style='font-size=200%;'>",
             "<h2>", namensliste, "</h2>",
             "<img src='QR-Code-ki-melodie-24.svg'></img>",
             "</div>"),
           file_con)
close(file_con)

rmarkdown::render("Namensliste_Experiment.md",
                  output_file = "Namensliste_Experiment.html")

