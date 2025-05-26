#' @export

fn_R_validate_new_list <- function(new_list, ref_list) {
  # Get the names of the fields in both lists
  observed_fields <- names(new_list)
  ref_fields <- names(ref_list)
  
  # Check if the fields are the same and in the same order
  if (!identical(observed_fields, ref_fields)) {
    # Fields that are extra (in new_list but not in ref_list)
    extra_fields <- setdiff(observed_fields, ref_fields)
    
    # Fields that are missing (in ref_list but not in new_list)
    missing_fields <- setdiff(ref_fields, observed_fields)
    
    # Show detailed messages
    message("Reference fields: ", paste(ref_fields, collapse = ", "))
    message("Observed fields: ", paste(observed_fields, collapse = ", "))
    
    if (length(extra_fields) > 0) {
      message("Extra fields: ", paste(extra_fields, collapse = ", "))
    }
    
    if (length(missing_fields) > 0) {
      message("Missing fields: ", paste(missing_fields, collapse = ", "))
    }
    
    if (!identical(observed_fields, ref_fields)) {
      message("The fields are not in the same order.")
    }
    
    # Return FALSE if the list is invalid
    return(FALSE)
  }
  
  # Return TRUE if the list is valid
  return(TRUE)
}
