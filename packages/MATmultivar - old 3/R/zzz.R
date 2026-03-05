# internal: declare global variables to satisfy R CMD check (NSE in dplyr/ggplot2)
utils::globalVariables(c(
  "GRUPO",
  "Componente",
  "Autovalor",
  "Varianza_Acumulada",
  ".case_id", ".data"))
