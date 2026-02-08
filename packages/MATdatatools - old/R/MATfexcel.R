#' MATfexcel: Importar datos desde una hoja de Excel
#'
#' Esta función importa datos desde una hoja de cálculo Excel, convierte la primera columna
#' en nombres de fila, y muestra un resumen de las variables. Además, si el argumento `viz` es TRUE,
#' se genera una visualización de las variables usando `gt_plt_summary()` del paquete `gtExtras`,
#' la cual se guardará en el entorno global con el mismo nombre del dataframe resultante, pero con el sufijo "_viz".
#'
#' @param file_path Ruta al archivo Excel.
#' @param sheet_name Nombre o índice de la hoja de Excel a importar.
#' @param na_values Un vector de caracteres que representa valores que deben tratarse como `NA`. Por defecto, las celdas vacías se consideran `NA`.
#' @param viz Lógico. Si es TRUE, se genera una visualización de las variables. Por defecto es FALSE.
#' @return Un dataframe con los datos importados.
#' @import readxl
#' @import gtExtras
#' @examples
#' \dontrun{
#' # Importar datos con celdas vacías como NA
#' ola <- MATfexcel("datos.xlsx", "Datos")
#'
#' # Importar datos con valores adicionales considerados como NA
#' ola <- MATfexcel("datos.xlsx", "Datos", na_values = c("n.d.", "s.d."))
#'
#' # Importar datos y visualizar variables
#' ola <- MATfexcel("datos.xlsx", "Datos", viz = TRUE)
#' # La visualización estará en "ola_viz"
#' }
#' @export
MATfexcel <- function(file_path, sheet_name, na_values = NULL, viz = FALSE) {
  # Verificar si los paquetes necesarios están instalados y cargarlos
  if (!requireNamespace("readxl", quietly = TRUE)) install.packages("readxl")
  if (!requireNamespace("gtExtras", quietly = TRUE)) install.packages("gtExtras")
  
  library(readxl)
  library(gtExtras)
  
  # Leer los datos desde la hoja de Excel
  data <- read_excel(file_path, sheet = sheet_name, na = na_values)
  
  # Convertir a dataframe y establecer la primera columna como nombres de fila
  data <- data.frame(data, row.names = 1)
  
  # Mostrar un resumen de las variables
  print(summary(data))
  
  # Si viz es TRUE, generar visualización y asignarla con el nombre adecuado en el entorno global
  if (viz) {
    viz_output <- gtExtras::gt_plt_summary(data)
    assign("viz_output", viz_output, envir = .GlobalEnv)
  }
  
  # Devolver el dataframe
  return(data)
}