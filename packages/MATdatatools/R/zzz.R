# Evita NOTE en R CMD check por el uso de .data (dplyr/rlang)
#' @importFrom rlang .data
NULL

utils::globalVariables(".data")
