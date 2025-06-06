---
title: "Análisis de Varianza (ANOVA) Simple"
format: 
  html:
    theme: cosmo
    toc: false
    embed-resources: true
    standalone: true
    code-tools: true  # Añade un botón global para mostrar/ocultar código
    code-fold: true   # Código plegado por defecto
    code-summary: "Mostrar código"  # Texto para el botón de despliegue
---

## Análisis ANOVA Básico

```{r setup}
#| warning: false
#| message: false
#| include: false

# Cargamos mtcars
data(mtcars)

# Convertir el número de cilindros a factor
mtcars$cyl <- as.factor(mtcars$cyl)

# Realizar ANOVA
modelo_anova <- aov(mpg ~ cyl, data = mtcars)

# Guardar resultados para uso en ambas pestañas
resultados <- summary(modelo_anova)
```

::: {.panel-tabset}

### Test ANOVA

```{r test}
#| warning: false
#| message: false
#| code-fold: true
#| code-summary: "Mostrar código del análisis ANOVA"

# Mostrar resultados del test
print(resultados)

# Interpretación
cat("\n## Interpretación\n\n")
cat(paste0("El valor p del análisis es ", summary(modelo_anova)[[1]][["Pr(>F)"]][1], 
           ", lo que indica que existen diferencias significativas en el rendimiento ",
           "de combustible (MPG) entre los diferentes grupos de cilindros."))
```

### Visualización

```{r plot}
#| warning: false
#| message: false
#| fig-width: 8
#| fig-height: 6
#| code-fold: true
#| code-summary: "Mostrar código de visualización"

# Visualización con boxplot
boxplot(mpg ~ cyl, data = mtcars, 
        main = "MPG por número de cilindros",
        xlab = "Número de cilindros", 
        ylab = "Millas por galón (MPG)",
        col = c("lightblue", "lightgreen", "lightpink"))
```

### Verificación de Requisitos

```{r requisitos}
#| warning: false
#| message: false
#| fig-width: 8
#| fig-height: 10
#| code-fold: true
#| code-summary: "Mostrar código de verificación de requisitos"

# Cargar paquete para pruebas
library(car)
library(ggplot2)

# Crear un panel de 2x2 para los gráficos
par(mfrow = c(2, 2))

# 1. Normalidad de residuos
residuos <- residuals(modelo_anova)
qqnorm(residuos, main = "Gráfico Q-Q de Normalidad")
qqline(residuos, col = "red")

# Prueba de normalidad Shapiro-Wilk
test_norm <- shapiro.test(residuos)
norm_result <- paste("Prueba Shapiro-Wilk: W =", round(test_norm$statistic, 4), 
                    ", p-value =", round(test_norm$p.value, 4))

# 2. Homogeneidad de varianzas
plot(modelo_anova, 1, main = "Homocedasticidad")  # Residuos vs. Ajustados

# Prueba de Levene para homogeneidad de varianzas
test_var <- leveneTest(mpg ~ cyl, data = mtcars)

# 3. Histograma de residuos
hist(residuos, main = "Histograma de Residuos",
     xlab = "Residuos", col = "lightblue", breaks = 10)

# Restaurar configuración gráfica
par(mfrow = c(1, 1))

# Mostrar resultados de las pruebas
cat("\n## Verificación de supuestos del ANOVA\n\n")
cat("### 1. Normalidad de los residuos\n")
cat(norm_result, "\n\n")
cat("Interpretación: ", ifelse(test_norm$p.value > 0.05, 
                              "Los residuos parecen seguir una distribución normal (p > 0.05).",
                              "Los residuos no siguen una distribución normal (p < 0.05)."), "\n\n")

cat("### 2. Homogeneidad de varianzas\n")
print(test_var)
cat("\nInterpretación: ", ifelse(test_var$`Pr(>F)`[1] > 0.05, 
                               "Las varianzas son homogéneas entre los grupos (p > 0.05).",
                               "Las varianzas no son homogéneas entre los grupos (p < 0.05)."), "\n\n")

cat("### 3. Independencia de las observaciones\n")
cat("Este supuesto no se puede verificar mediante una prueba estadística simple, ",
    "ya que depende del diseño del estudio y la forma en que se recolectaron los datos. ",
    "Para mtcars, asumimos que las observaciones son independientes entre sí.\n\n")

cat("### Conclusión\n")
if(test_norm$p.value > 0.05 && test_var$`Pr(>F)`[1] > 0.05) {
  cat("Se cumplen los supuestos principales del ANOVA. Los resultados del análisis son confiables.")
} else if(test_norm$p.value <= 0.05 && test_var$`Pr(>F)`[1] > 0.05) {
  cat("No se cumple el supuesto de normalidad. Considere usar pruebas no paramétricas como Kruskal-Wallis.")
} else if(test_norm$p.value > 0.05 && test_var$`Pr(>F)`[1] <= 0.05) {
  cat("No se cumple el supuesto de homogeneidad de varianzas. Considere usar una corrección como Welch ANOVA.")
} else {
  cat("No se cumplen los supuestos principales del ANOVA. Considere métodos alternativos como pruebas no paramétricas.")
}
```

:::
