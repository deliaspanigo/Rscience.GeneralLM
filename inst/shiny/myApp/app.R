library(shiny)
library(bslib)
library(shinyjs)
library(quarto)
library(shinyjs)
library(plotly)
library(htmlwidgets)
library(knitr)

library(shiny)
library(bslib)
library(shinyjs)
library(shinyAce)
# 
# source("global.R")
# 
# # Cargar los módulos
# source("modulos.R")
# source("modules_model_selection.R")
# source("modules_variables_anova.R")
# source("modules_quarto.R")


# Definir la interfaz de usuario principal
ui <- page_fluid( # Cambiado de page_sidebar a page_fluid para más flexibilidad
  #theme = bs_theme(version = 5, bootswatch = "minty"),
  theme = bs_theme(version = 5),
  
  # Necesario para manipular clases de CSS
  useShinyjs(),
  
  # CSS para botón verde chillón y para corregir el problema de los cards
  # tags$head(
  #   tags$style("
  #     .btn-neon-green {
  #       background-color: #39ff14 !important; /* Verde neón chillón */
  #       color: #000000 !important;
  #       border-color: #32cd32 !important;
  #       font-weight: bold !important;
  #       text-shadow: 0 0 5px rgba(0,255,0,0.5) !important;
  #       box-shadow: 0 0 8px rgba(57,255,20,0.8) !important;
  #     }
  # 
  #     /* Estilos para evitar que los cards se contraigan */
  #     .card {
  #       height: auto !important;
  #       overflow: visible !important;
  #     }
  # 
  #     .card-body {
  #       height: auto !important;
  #       overflow: visible !important;
  #       min-height: fit-content !important;
  #     }
  # 
  #     /* Estilo específico para el contenedor del Quarto */
  #     #quarto_doc-contenedor_html {
  #       height: auto !important;
  #       overflow: visible !important;
  #       max-height: none !important;
  #     }
  #   "),
  #   tags$style(HTML("
  #       /* Estilo para opciones seleccionadas en radioButtons - colores del tema minty */
  #       .radio input[type='radio']:checked + span {
  #         font-weight: bold;
  #         background-color: #78c2ad; /* Color principal del tema minty */
  #         color: black;
  #         border-radius: 4px;
  #         padding: 2px 8px;
  #       }
  # 
  #       /* Estilo para efecto hover en las opciones de radioButtons */
  #       .radio:hover {
  #         background-color: #5eb69d; /* Versión más oscura del color principal de minty */
  #         color: white;
  #         border-radius: 4px;
  #         transition: all 0.2s;
  #       }
  # 
  #       /* Enfoque y estado activo consistente con minty */
  #       .radio:focus-within {
  #         box-shadow: 0 0 0 0.25rem rgba(120, 194, 173, 0.25);
  #         outline: 0;
  #       }
  #     "))
  # ),
  
  # Layout con dos columnas - una para la barra lateral y otra para el contenido principal
  
    
    MASTER_module_fixed_anova_1_way_ui(id = "MASTER_fixed_anova_1_way") 
  
)

# Definir el servidor
server <- function(input, output, session) {
  # Valores predeterminados para reseteo
  
  MASTER_module_fixed_anova_1_way_server(id = "MASTER_fixed_anova_1_way", show_dev = F)
  
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)
