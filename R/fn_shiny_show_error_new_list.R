#' @export
fn_shiny_show_error_new_list <- function(step_number, new_list, ref_list) {
  # Construir el contenido HTML de la notificación
  notif_html <- HTML(
    paste0(
      "<strong>Error en ", step_number, ":</strong> La lista no es válida.<br>",
      "<em>Campos de referencia:</em> ", paste(names(new_list), collapse = ", "), "<br>",
      "<em>Campos observados:</em> ", paste(names(ref_list), collapse = ", ")
    )
  )
  
  # Mostrar la notificación en Shiny
  showNotification(
    notif_html,
    type = "error",
    duration = 10
  )
}