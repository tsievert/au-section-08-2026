#' Wrap the current selection in a two-column Quarto layout
#'
#' Takes the currently selected text in the active RStudio source document and
#' wraps it in a Quarto/Pandoc `.columns` / `.column` fenced-div layout, with
#' the selection placed as the left column and an empty right column left for
#' you to fill in.
#'
#' @export
wrap_selection_columns <- function() {
  ctx <- rstudioapi::getActiveDocumentContext()
  sel <- ctx$selection[[1]]
  left_text <- sel$text
  if (identical(left_text, "")) {
    left_text <- "left column text"
  }

  new_text <- paste(
    ':::: {.columns}',
    '',
    '::: {.column width="50%"}',
    left_text,
    ':::',
    '',
    '::: {.column width="50%"}',
    '',
    ':::',
    '',
    '::::',
    sep = "\n"
  )

  rstudioapi::modifyRange(sel$range, new_text, id = ctx$id)
}
