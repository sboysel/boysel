# I map to Ctrl+Shift+- using Tools > Modify Keyboard Shortcuts...
insertDashesAddin <- function() {
  con <- rstudioapi::getActiveDocumentContext()
  row <- con$selection[[1]]$range$start["row"]
  col <- con$selection[[1]]$range$start["column"]
  pos <- rstudioapi::document_position(row,
                                      (col + 1))
  dashes <- paste0(" ", paste0(rep("-", 80 - col), collapse = ""))
  rstudioapi::insertText(location = pos,
                         text = dashes,
                         id = con$id)
}
