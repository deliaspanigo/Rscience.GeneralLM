#' @export
fn_html_cartel <- function(my_text){
  div(
  class = "row mb-4",
  div(
    class = "col-12",
    div(
      style = "background: linear-gradient(90deg, #2C3E50, #4CA1AF); color: white; border-radius: 10px; padding: 20px; box-shadow: 0 4px 10px rgba(0,0,0,0.15);",
      div(
        class = "d-flex justify-content-between align-items-center",
        div(
          h3(icon("edit"), my_text, style = "margin: 0; font-weight: 600;"),
          p("Personaliza tu texto y visualízalo en tiempo real", style = "margin: 5px 0 0 0; opacity: 0.9;")
        ),
        div(
          icon("boxes-stacked", style = "font-size: 2.5rem; opacity: 0.8;")
        )
      )
    )
  )
)
}