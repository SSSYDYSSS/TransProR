.onAttach <- function(libname, pkgname) {
  # Display an ASCII art message when the package is loaded.
  ascii_art_path <- system.file("extdata", "ascii_art.txt", package = "TransProR")
  ascii_art_lines <- readLines(ascii_art_path)

  # Combine the vector of strings into one long string, preserving newlines.
  ascii_art <- paste(ascii_art_lines, collapse = "\n")

  # Use packageStartupMessage instead of cat for displaying the message.
  packageStartupMessage(ascii_art)
}
