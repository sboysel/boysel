# I map to Ctrl+Shift+- using Tools > Modify Keyboard Shortcuts...
insertDashesAddin <- function() {
  
  con <- rstudioapi::getActiveDocumentContext()
  
  pos <- make_position(con = con)
  
  rstudioapi::insertText(
    location = pos,
    text = make_dashes(col = col),
    id = con$id
  )
  
}

make_position <- function(con) {
  
  row <- con$selection[[1]]$range$start["row"]
  col <- con$selection[[1]]$range$start["column"]
  
  rstudioapi::document_position(
    row = row,
    col = (col + 1)
  )
  
}

make_dashes <- function(col, max_width = 80) {
  paste0(" ", paste0(rep("-", max_width - col), collapse = ""))
}
