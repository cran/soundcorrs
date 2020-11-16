# vim: set shiftwidth=4 tabstop=4 foldmarker=<<<,>>>:

.onAttach <- function (libname, pkgname) {
	if (.Platform$OS.type == "windows")
		packageStartupMessage ("To start the graphic interface, run \"soundcorrsGUI()\"\n(requires packages \"shiny\" and \"shinyjqui\").\nCaution! R does not really support Unicode under Windows.\nPlease only use ASCII (X-SAMPA or any other substitution).\n")
	else
		packageStartupMessage ("To start the graphic interface, run \"soundcorrsGUI()\"\n(requires packages \"shiny\" and \"shinyjqui\").\n")
}
