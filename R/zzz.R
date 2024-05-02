.onAttach <- function(libname, pkgname) {
  # Display ASCII art
  ascii_art_path <- system.file("extdata", "ascii_art.txt", package = "TransProR")
  if (file.exists(ascii_art_path)) {
    ascii_art_lines <- readLines(ascii_art_path)
    ascii_art <- paste(ascii_art_lines, collapse = "\n")
    packageStartupMessage(ascii_art)
  } else {
    packageStartupMessage("Welcome to TransProR!")
  }
}


