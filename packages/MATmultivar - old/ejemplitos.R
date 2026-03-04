library (MATmultivar)
MATcor(mtcars, mpg, hp, wt)
mtcars_correlaciones_info$correlaciones
componentes <-MATpca(mtcars, mpg, hp, wt)
componentes$pca
componentes$info$resumen_componentes
componentes$info$cargas_componentes
componentes$info$graficos
