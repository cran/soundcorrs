# vim: set shiftwidth=4 tabstop=4 foldmarker=<<<,>>>:

.onAttach <- function (libname, pkgname) {
	packageStartupMessage ("NOTE. Version 0.2.0 introduced some important changes.\nPlease run vignette(\"soundcorrs\")\n and consult https://cran.r-project.org/web/packages/soundcorrs/NEWS.\n")
}
