#' @export
fn_shiny_remove_future_steps <- function(APP_STATE, current_step, STR_STEP_NAME) {
  step_names <- names(APP_STATE)
  the_search_str <- paste0("^", STR_STEP_NAME, "\\d+$")
  steps_to_remove <- step_names[grepl(the_search_str, step_names)]
  steps_to_remove <- steps_to_remove[as.numeric(gsub(STR_STEP_NAME, "", steps_to_remove)) > current_step]
  
  if (length(steps_to_remove) > 0) {
    for (step in steps_to_remove) {
      APP_STATE[[step]] <- NULL
    }
  }
}