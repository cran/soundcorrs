# vim: set shiftwidth=4 tabstop=4 foldmarker=<<<,>>>:

# - exp transcription ------------------------------------------------------------------------------ <<< -

#' @title Constructor function for the \code{transcription} class.
#' @description Take a data frame containing transcription and turn it into a \code{transcription} object, as required by the \code{\link{soundcorrs}} constructor function. In the normal workflow, the user should have no need to call this function other than through \code{\link{read.transcription}}.
#' @param data [data.frame] Data frame containing the transcription and its meaning.
#' @param col.grapheme [character] Name of the column with graphemes. Defaults to \code{"GRAPHEME"}.
#' @param col.meta [character] Name of the column with the coverage of metacharacters. If empty string or \code{NA}, the column will be generated automatically. Defaults to \code{"META"}.
#' @param col.value [character] Name of the column with values of graphemes. Defaults to \code{"VALUE"}.
#' @details The primary reason why transcription needs to be defined, are regular expressions. R has a powerful system of regular expressions but they are general, not designed specifically for use in linguistics. Linguistics has its own convention of regular expressions, or rather two conventions, and to emulate them, it is necessary for \code{\link{soundcorrs}} to know the linguistic value of individual graphemes. One convention is the traditional, 'European' one where typically single characters represent entire classes of sounds, e.g. "C" stands for 'any consonant', "A" for 'any back vowel', etc. The other convention is the 'binary', 'American' notation where instead of using single characters, one lists all the distinctive features, e.g. "[+cons]" or "[+vowel,+back]". Having the values of graphemes encoded in a \code{transcription} object, \code{\link{expandMeta}} is able to translate these two notations into regular expressions that R can understand.
#'
#'	This constructor function is not really intended for the end user. Whenever possible, \code{\link{read.transcription}} should be used instead. Regardless of the function used, a data frame with two columns is required in order to create a \code{transcription} object: one column for the graphemes, and one for their values. It is probably not necessary, but nevertheless recommended, just to be on the safe side, that graphemes be single characters. (This also excludes combining diacritical marks.) Values must be separated by commas, without spaces. Typically, they will be phonetic features, but in principle they can be anything. A transcription may also have a third column that holds the string that the given grapheme is going to be turned into by \code{\link{expandMeta}}. Regular graphemes should be simply repeated in this column, whereas metacharacters (such as "C" or "A" mentioned above) should be expanded into all the graphemes they represent, separated by a bar ("|"), and enclosed in brackets, e.g. "(a|o|u)". If the third column is missing, this function will generate it automatically. Note, however, that the generation is based on the value column, and any grapheme whose value is a subset of the value of another grapheme, will be considered a metacharacter. For example, if "p" is defined as "cons,stop,blab", and "b" as "cons,stop,blab,voiced", "p" will be considered a metacharacter for both "p" and "b", and translated into "(p|b)" by \code{\link{expandMeta}}.
#'
#'	Graphemes cannot contain in them characters reserved for regular expressions: . + * ^ \ $ ? | ( ) [ ] \{ \}, and they also cannot contain in them characters defined as metacharacters in the transcription. For example, if "A" is defined as "vowel,back", and therefore represents all the back vowels in the transcription, a regular grapheme "A:" is forbidden. A metacharacter "A:", on the other hand, is permitted (e.g. for 'any long back vowel'), though it is recommended that such overlapping metacharacters be avoided as much as possible.
#'
#'	Lastly, a transcription must contain so-called linguistic zero. This is a character which signifies an empty segment in a word, a segment which has been only added in order to align the segments in all the words in a pair/triple/.... For example, English passport has two phonemes fewer than Spanish pasaporte id., so in order for the two words to be aligned, the English one needs two filler segments:p|a|s|-|p|o|r|t|- : p|a|s|a|p|o|r|t|e. To designate a character as linguistic zero in a transcription, its value must be \code{"NULL"}.
#'
#'	Two sample transcriptions are available: \code{trans-common}, \code{trans-ipa}; they can be loaded with the help of \code{\link{loadSampleDataset}}.
#' @return [transcription] An object containing the provided data.
#' @field data [data.frame] The original data frame.
#' @field cols [character list] Names of the important columns in the data frame.
#' @field meta [character] A vector of character strings which act as metacharacters in regular expressions. Mostly useful to speed up \code{\link{expandMeta}}.
#' @field values [character] A named list with values of individual graphemes exploded into vectors.
#' @field zero [character] A regular expression to catch linguistic zeros.
#' @seealso \code{link{expandMeta}}, \code{\link{print.transcription}}, \code{\link{read.transcription}}
#' @export
#' @examples
#' # path to a sample transcription
#' fName <- system.file ("extdata", "trans-common.tsv", package="soundcorrs")
#' fut <- transcription (read.table(fName,header=TRUE))

transcription <- function (data, col.grapheme="GRAPHEME", col.meta="META", col.value="VALUE") {

	# check the args
	tmp <- c (col.grapheme, col.meta, col.value)
	if (any(tmp=="" | is.na(tmp) | is.nan(tmp) | is.null(tmp)))
		stop ("Column names cannot be empty strings, NA, NaN, or NULL.")

	# check column names
	tmp <- c (col.grapheme, col.value)
	if (any(tmp %nin% colnames(data)))
		stop ("One or more column names are missing from \"data\".")

	# check for empty strings, nas, nans, and nulls
	tmp <- data [, col.grapheme]
	if (any(tmp=="" | is.na(tmp) | is.nan(tmp) | is.null(tmp)))
		stop ("Graphemes cannot be empty strings, NA, NaN, or NULL.")

	# remove factors
	data[] <- lapply (data, as.character)

	# check for duplicated graphemes
	tmp <- unique (data [duplicated(data[,col.grapheme]), col.grapheme])
	if (length(tmp) > 0)
		stop ("Multiple definitions for graphemes: ", collapse(tmp,inter=", "), ".")

	# check for duplicated definitions
	tmp <- strsplit (data[,col.value], ",")
	tmp <- lapply (tmp, sort)
	tmp <- unique (tmp[duplicated(tmp)])
	if (length(tmp) > 0) {
		t <- lapply (tmp, function(y) paste0("[",collapse(y,inter=","),"]"))
		warning ("Multiple graphemes for values: ", collapse(t,inter=", "), ".")
	}

	# check for linguistic zero
	tmp <- data [data[,col.value]=="NULL", col.grapheme]
	if (length(tmp) != 1)
		stop ("Linguistic zero not defined or defined multiple times.")

	# facilitate lookup
	rownames(data) <- data [,col.grapheme]

	# if needed, figure out metacharacters
	if (col.meta %nin% colnames(data)) {
		warning ("Missing the metacharacters column. The \"",col.meta,"\" column was generated.")

		# find which graphemes' values are subsets of other graphemes' values
		expl <- strsplit (as.vector(data[,col.value]), ",")
		meta <- list ()
		for (i in seq_along(expl)) {
			tmp <- c ()
			for (ii in seq_along(expl))
				if (all(expl[[i]] %in% expl[[ii]]))
					tmp <- c(tmp, ii)
			meta[[i]] <- tmp
		}
		# remove recursive metacharacters
		meta <- lapply (meta, function (x)
			Filter (function(y) length(meta[[y]])==1, x))
		# translate numbers to characters
		data[,col.grapheme] <- as.vector (data[,col.grapheme])
		data[,col.meta] <- unlist (lapply (meta, function (x)
			if (length(x) == 1)
				data[x,col.grapheme]
			else
				paste0("(", collapse(data[x,col.grapheme],inter="|"), ")")
		))

	}

	# find metacharacters
	meta.nrs <- which (data[,col.grapheme]!=data[,col.meta] & data[,col.value]!="NULL")
	tmp <- data [meta.nrs, c(col.grapheme,col.meta)]
	tmp <- tmp [order(nchar(tmp[,col.grapheme]), decreasing=F), ]
	meta <- setNames (tmp[,col.meta], tmp[,col.grapheme])

	# check for eregexp metacharacters
	tmp <- gregexpr ("[][\\(\\)\\{\\}\\.\\+\\*\\^\\\\\\$\\?\\|]", data[,col.grapheme])
	tmp <- which (tmp!=-1)
	if (length(tmp) > 0) {
		tmp <- data [unlist(tmp), col.grapheme]
		stop ("Extended regular expressions metacharacters in graphemes: ", collapse(tmp,inter=", "), ".")
	}

	# check for custom metacharacters
	if (length(meta) > 0) {

		# in graphemes
		tmp <- gregexpr (paste0("(",collapse(names(meta),inter="|"),")"), data[,col.grapheme])
		err <- c ()
		for (i in seq_along(tmp))
			if (tmp[[i]][1]!=-1 & i %nin% meta.nrs) err<-c(err,i)
		if (length(err) > 0) {
			tmp <- data [err, col.grapheme]
			stop ("Custom regular expressions metacharacters in graphemes: ", collapse(tmp,inter=", "), ".")
		}

		# and in metacharacters
		err <- lapply (names(meta), function (x) {
			tmp1 <- gregexpr(x,data[,col.grapheme])
			tmp2 <- c ()
			for (i in seq_along(tmp1))
				if (tmp1[[i]][1]!=-1 & data[i,col.grapheme]!=x) tmp2<-c(tmp2,i)
			if (!is.null(tmp2))
				paste0 (x, " in ", collapse(data[tmp2,col.grapheme],inter=", "))
		})
		err <- err [err!="NULL"]
		if (length(err) > 0)
			warning ("Custom regular expressions metacharacters in metacharacters: ", collapse(err,inter="; "), ".")
	}

	# find the grapheme for linguistic zero
	zero <- data [data[,col.value]=="NULL", col.grapheme]

	# find the values of individual graphemes
	values <- strsplit (data[,col.value], ",")
	names(values) <- data[,col.grapheme]

	# create an object
	res <- list (
		data = data,
		cols = list (grapheme=col.grapheme, meta=col.meta, value=col.value),
		meta = meta,
		values = values,
		zero = zero
	)
	class(res) <- "transcription"

	# return the result
	return (res)

}


# -------------------------------------------------------------------------------------------------- >>> -
# - exp expandMeta.transcription ------------------------------------------------------------------- <<< -

#' @title Expand custom metacharacters to regular expressions.
#' @description Turn characters defined in a \code{\link{transcription}} as metacharacters into the corresponding regular expression.
#' @param data [transcription] The \code{\link{transcription}} to use.
#' @param x [character] A single string that contains metacharacters.
#" @details Linguistic regular expressions, such as "C" or "[+cons]" for 'any consonant', are incomprehensible to R. \code{expandMeta} translates them to a format which R can digest. Two notations are possible. The traditional, 'European' convention ("C" for 'any consonat') must be defined inside the \code{\link{transcription}} object; \code{expandMeta} merely substitutes a grapheme with its value taken from the "META" column. The 'binary', 'American' notation ("[+cons]") needs to be first interpreted by \code{expandMeta}. Such expressions must be formatted as follows: enclosed in square brackets, each feature prepended with "+" or "-", features comma-separated, no spaces. For example, "[+cons,-labial]", or "[+vowel,-front,-high]". Needless to say, names of features must correspond to the values of graphemes as defined in the \code{transcription} object. Failure to conform to any of the above will result in \code{\link{expandMeta}} not recognizing the expression as such, and simply returning it as is.
#' @return [character] The string with metacharacters expanded.
#' @seealso \code{\link{transcription}}
#' @export
#' @examples
#' dataset <- loadSampleDataset ("data-abc")
#' expandMeta (dataset$trans[[1]], "aN")
#' expandMeta (dataset$trans[[1]], "[+vow,-high]")

expandMeta <- function (data, x)
	UseMethod ("expandMeta")

#' @export
expandMeta.default <- function (data, x)
	stop ("This function does not know how to handle an object of class \"",class(data),"\".")

#' @export
expandMeta.transcription <- function (data, x) {

# - bin2reg - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -<<< -

	bin2reg <- function (y) {

		# prep vars
		tmp1 <- strsplit (y, "[][,]") [[1]] [-1]
		tmp2 <- sub (".", "", tmp1)
		plus <- tmp2 [which(substr(tmp1,1,1)=="+")]
		mins <- tmp2 [which(substr(tmp1,1,1)=="-")]

		# find which graphemes have the requested values
		tmp <- lapply (data$values, function (z)
			all(plus %in% z) && !any(mins %in% z))

		# turn them into actual graphemes
		tmp <- names (which (unlist (tmp==T)))
		tmp <- tmp [tmp!=data$zero & tmp %nin% names(data$meta)]

		# and return them as a regexp
		return (paste0 ("(", collapse(tmp,inter="|"), ")"))
	}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ->>> -

	# prep vars
	res <- x

	# expand binary notation
	tmp <- gregexpr ("\\[[+-].*?\\]", res)
	if (tmp[[1]][1] != -1)
		regmatches(res,tmp)[[1]] <- lapply (regmatches(res,tmp)[[1]], bin2reg)

	# expand simple metacharacters
	tmpHlp <- function (meta, y)
		if (length(meta) == 0) y else
			gsub (names(meta)[[1]], meta[[1]], tmpHlp(meta[-1],y))
	res <- tmpHlp (data$meta, res)

	# return the result
	return (res)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp print.transcription ------------------------------------------------------------------------ <<< -

#' A more reasonable display of a \code{\link{transcription}} object.
#' @param x [transcription] The \code{\link{transcription}} object.
#' @param ... Unused; only for consistency with \code{\link{print}}.
#' @details A \code{\link{transcription}} object may be quite large and therefore difficult to digest for a human. This function reduces it to a brief, easy to understand summary.
#' @return [transcription] The same object that was given as \code{x}.
#' @seealso \code{\link{transcription}}
#' @export
#' @examples
#' # path to a sample transcription
#' fName <- system.file ("extdata", "trans-common.tsv", package="soundcorrs")
#' read.transcription (fName)

print.transcription <- function (x, ...) {

	# print the data
	cat ("A \"transcription\" object.\n")
	if (!is.null(attr(x,"file")))
		cat (paste0("  File: ", attr(x,"file"), ".\n"))
	cat (paste0("  Graphemes: ", nrow(x$data), ".\n"))
	cat ("\n")

	# and return it under the counter
	invisible (x)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp read.transcription ------------------------------------------------------------------------- <<< -

#' @title Read transcription from a tsv file.
#' @description Read a table from file and create a \code{\link{transcription}} object out of it.
#' @param file [character] Path to the data file.
#' @param col.grapheme [character] Name of the column with graphemes. Defaults to \code{"GRAPHEME"}.
#' @param col.meta [character] Name of the column with the coverage of metacharacters. If empty string or NA, the column will be generated automatically. Defaults to \code{"META"}.
#' @param col.value [character] Name of the column with values of graphemes. Defaults to \code{"VALUE"}.
#' @details The constructor for the \code{\link{transcription}} class requires a data frame which means that the user would need to first read it from a file, and only then pass it to the constructor. This function saves this one step. In addition, it attaches the name of the file to the object, which allows for easier identification later. It is recommended to use \code{read.transcription} instead of the raw \code{\link{transcription}} constructor whenever possible.
#' @return [transcription] A transcription object containing the read transcription.
#' @seealso \code{\link{transcription}}
#' @export
#' @importFrom utils read.table
#' @examples
#' # path to a sample transcription
#' fName <- system.file ("extdata", "trans-common.tsv", package="soundcorrs")
#' read.transcription (fName)

read.transcription <- function (file, col.grapheme="GRAPHEME", col.meta="META", col.value="VALUE") {

	# check args
	tmp <- list (file=file, col.grapheme=col.grapheme, col.meta=col.meta, col.value=col.value)
	err <- which (lapply(tmp,class) != "character")
	if (length(err) > 0)
		stop ("\"", names(tmp)[err[1]], "\" must be a character string.")

	# read in the data
	data <- read.table (file, header=T, stringsAsFactors=F, quote="")

	# pack data into an object
	res <- transcription (data, col.grapheme, col.meta, col.value)
	attr(res,"file") <- file

	# return the result
	return (res)

}

# -------------------------------------------------------------------------------------------------- >>> -
