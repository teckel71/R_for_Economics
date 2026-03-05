# MATmultivar 0.1.2 (2026-03-04)

## Correcciones
- `MATclus_Ward()`: el dendrograma ahora replica mejor la estética de la práctica de referencia:
  - grosor de ramas `lwd = 0.7`
  - hojas alineadas (`hang = -1`)
  - coloreado por clúster (paleta `"Set1"`) cuando `k > 0`
  - rectángulos de clúster con relleno y transparencia (`rect_fill = TRUE`, `rect_alpha = 0.25`)
  - se elimina la leyenda del grosor (`legend.position = "none"`)

