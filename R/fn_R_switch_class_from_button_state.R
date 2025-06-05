#' @export
fn_R_switch_class_from_button_state <- function(button_state){
  
# selected_opt <- switch(button_state,
#        "initial"   = "btn-outline-primary",    # Azul inicial
#        "confirmed" = "btn-outline-success",    # Verde después de confirmar
#        "error"     = "btn-outline-danger")

selected_opt <- switch(button_state,
                       "initial"   = "btn-primary",    # Azul inicial
                       "confirmed" = "btn-success",    # Verde después de confirmar
                       "error"     = "btn-danger")
return(selected_opt)
}
