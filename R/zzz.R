.onAttach <- function(libname, pkgname) {
  # Ensure 'reticulate' is installed and available; it's essential for interfacing with Python.
  if (!requireNamespace("reticulate", quietly = TRUE)) {
    stop("The 'reticulate' package is required for this package to work. Please install it first.")
  }

  # Specify the name of the Conda environment we are targeting.
  target_conda_env <- "TransPro"

  # Attempt to use the specified Conda environment.
  # 'required = TRUE' makes the function stop with an error if the environment does not exist.
  reticulate::use_condaenv(target_conda_env, required = TRUE)

  # At this point, 'reticulate' should be using Python from the "TransPro" environment.
  # We retrieve configuration information of the Python currently in use.
  python_config <- reticulate::py_config()

  # Extract the major and minor Python version numbers.
  version_numbers <- strsplit(python_config$version, "\\.")[[1]]
  major_version <- as.integer(version_numbers[1])
  minor_version <- as.integer(version_numbers[2])

  # Assert that the Python version is 3.10 or higher, as required.
  if (major_version < 3 || (major_version == 3 && minor_version < 10)) {
    stop("Python 3.10 or higher is required in the 'TransPro' Conda environment.")
  }

  # Define the Python packages that are necessary for our R package.
  python_dependencies <- c("numpy", "scipy", "TransProPy")

  # Discover if all required Python packages are available.
  missing_py_packages <- reticulate::py_discover_config(python_dependencies)$required

  # If any packages are missing, we attempt to install them within the active Conda environment.
  if (length(missing_py_packages) > 0) {
    packageStartupMessage("Attempting to install missing Python packages in the 'TransPro' environment...")
    tryCatch({
      reticulate::py_install(missing_py_packages)
    }, error = function(e) {
      stop("An error occurred during the installation of Python packages: ", e$message)
    })
  }

  # Additional setup or checks related to the 'TransPro' environment can be added here if necessary.
  # This is also the place where package maintainers might add other environment-related configurations.

  ascii_art_path <- system.file("extdata", "ascii_art.txt", package = "TransProR")
  ascii_art_lines <- readLines(ascii_art_path)

  # Combine the vector of strings into one long string, preserving newlines.
  ascii_art <- paste(ascii_art_lines, collapse = "\n")

  # Use packageStartupMessage instead of cat for displaying the message.
  packageStartupMessage(ascii_art)

}
