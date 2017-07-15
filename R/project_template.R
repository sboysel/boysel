#' Create new project
#' 
#' Project template consistent with the conventions followed by Boysel & Parsa.
#' 
#' @param path directory filepath of project.
#' @param use_git logical indicating if \code{git2r::init(path)} should be run.
#' @param use_packrat logical indicating if \code{packrat::init()} should be run.  Default settings
#' disable automatic snapshots and instructs git to ignore the \code{packrat/src} directory.
#' @return creates project directory and subdirectories.  Initializes git and packrat if specified.
#' 
project_template <- function(path, use_git, use_packrat) {
  # path <- temp_dir()
  
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
  
  for (x in c("analysis", "build")) {
    for (y in c("input", "code", "temp", "output")) {
      dir.create(file.path(path, x, y), showWarnings = FALSE, recursive = TRUE)
      file.create(file.path(path, x, y, ".gitignore"))
    }
  }
  
  rmarkdown::draft(
    file = file.path(path, "analysis", "output", "analysis.Rmd"),
    template = "article",
    package = "boysel",
    create_dir = FALSE,
    edit = FALSE
  )
  
  # git init
  if (use_git) {
    git2r::init(path)
  }
  
  # packrat init
  if (use_packrat) {
    packrat::init(
      project = path,
      options = list(
        auto.snapshot = FALSE,
        vcs.ignore.src = TRUE
      ),
      enter = TRUE
    )
  }
  
}