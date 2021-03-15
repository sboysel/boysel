#' R Markdown Notes Template (PDF)
#' 
#' Attribution: \url{https://github.com/rstudio/rticles}.
#'
#' @inheritParams rmarkdown::pdf_document
#' @param ... Additional arguments to \code{rmarkdown::pdf_document}
#'
#' @return R Markdown output format to pass to
#'   \code{\link[rmarkdown:render]{render}}
#'
#' @export
notes_pdf <- function(...,
                      keep_tex = TRUE,
                      md_extensions = c("-autolink_bare_uris",
                                        "+header_attributes",
                                        "+auto_identifiers",
                                        "+fenced_code_blocks",
                                        "+fenced_code_attributes",
                                        "+fancy_lists",
                                        "+subscript",
                                        "+superscript",
                                        "+raw_tex",
                                        "+citations",
                                        "+inline_notes",
                                        "+footnotes",
                                        "+table_captions",
                                        "+pipe_tables")) {
  template <- system.file("rmarkdown", "templates", "notes", "resources", 
                          "notes.tex", package = "boysel")
  inherit_pdf_document(...,
                       template = template,
                       keep_tex = keep_tex,
                       md_extensions = md_extensions)
}


inherit_pdf_document <- function(...) {
  fmt <- rmarkdown::pdf_document(...)
  fmt$inherits <- "pdf_document"
  fmt
}