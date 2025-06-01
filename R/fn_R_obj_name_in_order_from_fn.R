#' @export
fn_R_obj_name_in_order_from_fn <- function(fn){
    fn_text <- deparse(fn)
    fn_body <- fn_text[2:(length(fn_text)-1)]
    modified_code <- paste(fn_body, collapse = "\n")
    # 
    # modified_code <- gsub("\\{[^}]*\\}", "", modified_code)
    # modified_code <- gsub("\\[[^]]*\\]", "", modified_code)
    # modified_code <- gsub("\\([^)]*\\)", "", modified_code)

    vector_limpio <- unlist(strsplit(modified_code, "\n"))
    vector_limpio <- vector_limpio[grepl("<-", vector_limpio)]
    vector_limpio <- trimws(vector_limpio, which = "left")
    vector_limpio <- vector_limpio[!grepl("\\._", vector_limpio)]
    vector_limpio <- unlist(strsplit(vector_limpio, "<-"))[c(T, F)]
    vector_limpio <- trimws(vector_limpio, which = "right")
    vector_limpio <- unique(vector_limpio)
    vector_limpio
    patron <- "\\$|\\,|\\(|\\)|\"|'|\\[|\\]" #[\"'\\[\\]\\(\\)]"
    vector_limpio <- vector_limpio[!grepl(patron, vector_limpio)]
    patron2 <- "['\"/]"
    vector_limpio <- vector_limpio[!grepl(patron2, vector_limpio)]
    
    # Posiciones que solo tienen letras, numeros y guiones bajos,
    #y no poseen ningun otro tipod e simbolo.
    patron3 <- "^[a-zA-Z0-9_]+$"
    vector_limpio <- vector_limpio[grepl(patron3, vector_limpio)]
    vector_limpio <- unique(vector_limpio)

return(vector_limpio)

}