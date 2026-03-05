# MATmultivar 0.1.4 (2026-03-05)

## Correcciones
- `MATclus_Ward()`: dendrograma ajustado para igualar la guía de la práctica:
  - ramas extremadamente finas (se fuerza `geom_segment()` a `size = 0.2`, evitando diferencias entre versiones de {factoextra})
  - ramas siempre en negro (no se colorean por clúster)
  - hojas alineadas (`hang = -1`)
  - rectángulos por clúster con bordes en paleta "Set1" y relleno semitransparente (`rect_alpha = 0.25`)
  - sin leyenda

# MATmultivar 0.1.3 (2026-03-05)

## Correcciones
- `MATclus_Ward()`: dendrograma ajustado para igualar la guía de la práctica (primera corrección):
  - ramas más finas (`lwd = 0.25`)
  - ramas siempre en negro
  - hojas alineadas (`hang = -1`)
  - rectángulos por clúster con paleta "Set1" y relleno semitransparente
  - sin leyenda

# MATmultivar 0.1.2 (2026-03-04)

## Correcciones
- `MATclus_Ward()`: primera aproximación de ajustes estéticos del dendrograma (lwd, hang, rectángulos, sin leyenda).


# MATmultivar 0.1.5
- MATclus_Ward(): forzar grosor real de ramas (linewidth) y mantener ramas negras; corregida duplicación de columna de casos en tablas por grupo.
