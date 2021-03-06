% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/project_template.R
\name{project_template}
\alias{project_template}
\title{Create new project}
\usage{
project_template(path, use_git, use_packrat)
}
\arguments{
\item{path}{directory filepath of project.}

\item{use_git}{logical indicating if \code{git2r::init(path)} should be run.}

\item{use_packrat}{logical indicating if \code{packrat::init()} should be run.  Default settings
disable automatic snapshots and instructs git to ignore the \code{packrat/src} directory.}
}
\value{
creates project directory and subdirectories.  Initializes git and packrat if specified.
}
\description{
Project template consistent with the conventions followed by Boysel & Parsa.
}
