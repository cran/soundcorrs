# vim: set shiftwidth=4 tabstop=4 foldmarker=<<<,>>>:

# - exp soundchange -------------------------------------------------------------------------------- <<< -

#' @title Constructor function for the \code{soundchange} class.
#' @description Either takes a sound change, translates it to a function, and wraps that into a \code{soundchange} object, or takes a function and wraps it into a \code{soundchange} object.
#' @param x [character/function] Either a sound change, or a function that takes as arguments a character string named \code{x} and an object named \code{meta}, and returns a vector of character strings.
#' @param name [character] Name of the sound change. Length limit is not enforced, but it is recommended to not exceed 30 characters.
#' @param transcription [transcription] The \code{\link{transcription}} used in the notation of the sound change.
#' @param desc [character] A description of the change. May be as short or as long as needed. Defaults to \code{NULL}.
#' @param perl [logical] Use Perl-compatible regular expressions? This argument is only used when \code{x} is a character string. Defaults to \code{FALSE}.
#' @details A \code{soundchange} object is basically a sound change function with some metadata: name, description, and associated transcription. The sound change function can be a simple substitution using \code{\link{gsub}}, or as complex as required. It must take two arguments: \code{x} which is a single character string to which the change is to be applied, and \code{meta} which can be any object that holds additional metadata; \code{meta} can also be \code{NULL}. The return value of a sound change function must be a vector of character strings. The ability to output multiple strings is primarily intended for reconstruction, and it is not recommended that it be used to fit multiple changes into a single function. Example: in language L, "*a" and "*e" merged to "a", and simultaneously *o, *u > o. Regardless of whether our functions depart from the current state and reconstruct proto-forms, or the opposite, two functions should be defined: one for the a-e merger, and one for the o-u merger. The difference between them will be that with a progressive change, each function will return only one value ("[ae] > a") whereas in a regressive one it will return two ("a > a, e").
#'
#'	When the sound change is given as a simple character string, rather than a complete function, the string must contain exactly one ">" or "<" sign. Spaces around it are ignored. The string may take full advantage of regular expressions, both the ones available in pure R, and custom ones defined by the user and accepted by \code{\link{expandMeta}}. Backreferences may be of particular usefulness. In short, any part of the 'find' string that is enclosed in brackets, is also available in the 'replace' string as \code{"\\\\1"}, \code{"\\\\2"}, etc. For example, if k's followed by e or i all change into s's, this process cannot be encoded as simply *\code{"k[ei] > s"} because this would result in the e/i being deleted. The correct notation would be \code{"k([ei]) > s\\\\1"}. However, see the caveat below.
#'
#'  Warning! When the META column is missing from the transcription and generated automatically, the alternatives are listed using round- rather than square-bracket notation, i.e. as \code{"(x|y)"} rather than \code{"[xy]"}. This is necessary because some graphemes may be longer than one character, but a side effect of this is that when a user-defined metacharacter ("wild-card") is used in the 'find' part of the sound change string (the part before \code{"<"}), it adds a set of round brackets to that part of the string. In turn, this means that backreferences in the 'replace' part of the sound change string must either be shifted accordingly, or round brackets around metacharacters in the 'find' part omitted. For example, if intervocalic s is to be replaced with r, the sound change string should be \code{"VsV > \\\\1r\\\\2"} rather than *\code{"(V)s(V) > \\\\1r\\\\2"} because \code{"V"} itself is already translated to \code{"(a|ä|e|…"}.
#'
#'	Note that sound changes and their application do not require the data to be segmented and aligned. If sound changes are the only goal of the project, these two time-consuming steps can be safely omitted.
#' @return [soundchange] An object containing the provided data.
#' @field fun [function] The sound change function.
#' @field desc [character] A description of the change. May be \code{NULL}.
#' @field name [character] The name of the change.
#' @seealso \code{\link{print.soundchange}}, \code{\link{applyChanges}}
#' @export
#' @examples
#' # prepare sample transcription
#' trans <- loadSampleDataset ("trans-common")
#' # run soundchange
#' a2b <- soundchange ("a > b", "sample change", trans)
#' a2b <- soundchange ("b < a", "sample change", trans)
#' a2b <- soundchange (function(x,meta) gsub("a","b",x), "sample change", trans)

soundchange <- function (x, name, transcription, desc=NULL, perl=F) {

	# check args
	if (class(x) %nin% c("character", "function"))
		stop ("\"x\" must be a character string or a function.")
	if (length(x) != 1)
		stop ("\"x\" must be one character string.")
	if (class(name) != "character")
		stop ("\"name\" must be a character string.")
	if (is.null(name) || is.na(name) || is.nan(name) || name=="")
		stop ("\"name\" cannot be empty string, NA, NaN or NULL.")
	if (class(transcription) != "transcription")
		stop ("\"transcription\" must be a \"transcription\" object.")

	# check function
	fun <- if (class(x)=="function") {

		# check args
		if (!identical (names(formals(x)), c("x","meta")))
			stop ("\"x\" must take two arguments: \"x\" and \"meta\".")

		# keep the function
		x

	# convert to function
	} else {

		# split on < or >
		tmp <- strsplit (x, "\\s*[<>]\\s*") [[1]]
		if (length(tmp)!=2)
			stop("\"x\" must contain exactly one \"<\" or \">\".")

		# expand metacharacters
		tmp <- sapply (tmp, function(x) expandMeta(transcription,x))

		# make a function
		fun <- function (x,meta) gsub (x,x,x,perl=perl)

		# evaluate tmps so fun is printed nicely
		if (regexpr(">",x)!=-1) {
			body(fun)[[2]] <- tmp[[1]]
			body(fun)[[3]] <- tmp[[2]]
		} else {
			body(fun)[[2]] <- tmp[[2]]
			body(fun)[[3]] <- tmp[[1]]
		}
		body(fun)[[5]] <- perl

		# return fun
		fun

	}

	# wrap in an object
	res <- list (
		name = name,
		desc = desc,
		fun = fun,
		trans = transcription
	)
	class(res) <- "soundchange"

	# return the result
	return (res)

}


# -------------------------------------------------------------------------------------------------- >>> -
# - exp print.soundchange -------------------------------------------------------------------------- <<< -

#' A more reasonable display of a \code{\link{soundchange}} object.
#' @param x [soundchange] The \code{\link{soundchange}} object.
#' @param ... Unused; only for consistency with \code{\link{print}}.
#' @details The structure of a \code{\link{soundchange}} object is probably not too dificult to read for a human, but this does not mean it cannot be presented in a slightly more convenient form.
#' @return [soundchange] The same object that was given as \code{x}
#' @seealso \code{\link{soundchange}}
#' @export
#' @examples
#' # prepare sample transcription
#' trans <- loadSampleDataset ("trans-common")
#' # run print.soundchange
#' a2b <- soundchange ("a > b", "sample change", trans,
#'	"This is a very simple sample sound change whereby \"a\" is turned into \"b\".")
#' a2b

print.soundchange <- function (x, ...) {

	# print the data
	cat ("A \"soundchange\" object.\n")
	cat (paste0("  Name: ", x$name, ".\n"))
	if (!is.null(x$desc))
		cat (paste0("  Description: ", x$desc, "\n"))
	tmp <- attr (x$trans, "file")
	if (!is.null(tmp))
		cat (paste0("  Transcription: ", tmp, ".\n"))
	cat ("\n")

	# and return it under the counter
	invisible (x)

}

# -------------------------------------------------------------------------------------------------- >>> -
