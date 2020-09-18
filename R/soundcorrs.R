# vim: set shiftwidth=4 tabstop=4 foldmarker=<<<,>>>:

# - exp soundcorrs --------------------------------------------------------------------------------- <<< -

#' @title Constructor function for the \code{soundcorrs} class.
#' @description Take a data frame and turn it into a \code{soundcorrs} object containing data for one language. To obtain a \code{soundcorrs} object containing data for multiple languages, see \code{\link{merge.soundcorrs}}. In the normal workflow, the user should have no need to call this constructor other than through \code{\link{read.soundcorrs}}.
#' @param data [data.frame] Data for one language.
#' @param name [character] Name of the language.
#' @param col.aligned [character] Name of the column with the aligned words.
#' @param transcription [transcription] The \code{\link{transcription}} for the given language.
#' @param separator [character] String used to separate segments in \code{col.aligned}. Defaults to \code{"\\|"}.
#' @details \code{soundcorrs} is the fundamental class of the entire soundcorrs package, and it is required for most tasks that the package promises to make easier and faster than manual labour. A \code{soundcorrs} object is a list containing the original data frame, some metadata (names of languages, names of columns, transcriptions), as well as transformations of the original data for faster processing in \code{\link{findExamples}} and other functions (words exploded into individual segments, with segment separators removed, etc.). The basic unit in \code{soundcorrs} is a pair/triple/... of words, each of which is assigned to a specific language.
#'
#'	This constructor function is not really intended for the end user. Whenever possible, \code{\link{read.soundcorrs}} should be used instead. Regardless of the function used, two pieces of information are required for each word: the language it comes from, and its segmented and aligned form. Segmentation means that the word is cut into parts which can represent phonemes, morphemes, or anything else (the default separator is a vertical bar, \code{"|"}). A word with no separators in it is considered one big segment, and in fact, for \code{\link{soundchange}}'s this is enough. Alignment means that each word in a pair/triple/... has the same number of segments, and that those segments are in the corresponding places. Often, one of the words in a pair/triple/... will naturally have fewer segments than the others; in such cases, a filler character, 'linguistic zero' needs to be used (\code{"-"} is a good choice); for example, to align the Spanish and Swedish names for 'Stockholm', a total of three such 'empty' segments is required: e|s|t|o|k|-|o|l|m|o : -|s|t|o|k|k|o|l|m|-. Linguistic zero must be defined in the \code{\link{transcription}}.
#'
#'	Typically, a \code{soundcorrs} object will be used to hold an entire list of pairs/triples/... of words from various languages. However, both this constructor function and \code{\link{read.soundcorrs}} can only read data from one language at a time. This is because each language requires relatively many pieces of metadata (name, column names, transcription), and if all of this information for multiple languages were to be passed as arguments to one function, the call would very quickly become illegible. Multiple \code{\link{soundcorrs}} objects can be merged into one using \code{\link{merge.soundcorrs}}.
#'
#'	Three sample datasets are available: \code{data-abc}, \code{data-capitals}, and \code{data-ie}; they can be loaded with the help of \code{\link{loadSampleDataset}}.
#' @return [soundcorrs] An object containing the provided data and metadata for one language.
#' @field cols [character list] Names of important columns.
#' @field data [data.frame] The original data.
#' @field names [character] Name of the language.
#' @field segms [character list] Words exploded into segments. With linguistic zeros preserved (\code{$z}) or removed (\code{$nz}).
#' @field segpos [integer list] A lookup list to check which character belongs to which segment. Counted with linguistic zeros preserved (\code{$z}) and removed (\code{$nz}).
#' @field separators [character] The strings used as segment separator in \code{cols$aligned}.
#' @field trans [transcription] The transcription.
#' @field words [character list] Words obtained by removing separators from the \code{cols$aligned} columns. With linguistic zeros (\code{$z}) or without them (\code{$nz}).
#' @export
#' @importFrom utils type.convert
#' @examples
#' # prepare sample transcription
#' trans <- loadSampleDataset ("trans-common")
#' # read sample data in the "wide format"
#' fNameData <- system.file ("extdata", "data-capitals.tsv", package="soundcorrs")
#' readData <- read.table (fNameData, header=TRUE)
#' # make out of them a soundcorrs object
#' ger <- soundcorrs (readData, "German", "ALIGNED.German", trans)
#' pol <- soundcorrs (readData, "Polish", "ALIGNED.Polish", trans)
#' spa <- soundcorrs (readData, "Spanish", "ALIGNED.Spanish", trans)
#' dataset <- merge (ger, pol, spa)

soundcorrs <- function (data, name, col.aligned, transcription, separator="\\|") {

	# check args
	if (class(data) != "data.frame")
		stop ("\"data\" must be a data.frame")
	if (class(name) != "character")
		stop ("\"name\" must be a string.")
	if (class(col.aligned) != "character")
		stop ("\"col.aligned\" must be a string.")
	if (class(transcription) != "transcription")
		stop ("\"col.aligned\" must be a \"transcription\" object")
	if (class(separator) != "character")
		stop ("\"separator\" must be a string.")

	# check column name
	if (length(col.aligned) != 1)
		stop ("\"col.aligned\" must be exactly one column name.")
	if (col.aligned %nin% colnames(data))
		stop ("Column \"", col.aligned, "\" missing from \"data\".")

	# check separator not empty string, na, nan, or null
	if (separator=="" | is.na(separator) | is.nan(separator) | is.null(separator))
		stop ("\"separator\" cannot be an empty string, NA, NaN, or NULL")

	# remove factors
	data <- type.convert (data, as.is=T)

	# explode into segments
	tmp <- as.vector (data[,col.aligned])
	tmp <- gsub (paste0(separator,"+"), separator, tmp)		# rm multiple separators
	tmp <- gsub (paste0("^",separator), "", tmp)			# rm separators from beginning, cause strsplit doesn't
	segms.z <- strsplit (tmp, separator)
	segms.nz <- strsplit (gsub(transcription$zero,"",tmp), separator)

	# check zeros always separate segments
	tmp.err <- Filter (function(x) nchar(x)>1 && grep(transcription$zero,x), unlist(segms.z))
	if (length(tmp.err) > 0)
		stop ("Linguistic zeros must be separate segments: ", collapse(tmp.err,inter=", "), ".")

	# warn if the transcription doesn't cover everything
	tmp.used <- unique (unlist (segms.z))
	tmp.used <- tmp.used [!is.na(tmp.used)]
	tmp.err <- tmp.used[tmp.used %nin% transcription$data[,transcription$cols$grapheme]]
	if (length(tmp.err) > 0)
		warning ("The following segments are not covered by the transcription: ", collapse(tmp.err,inter=", "), ".")

	# unexplode words, with and without zeros
	words.z <- unlist (lapply (segms.z, function(x) if (is.na(x[1])) NA else collapse(x)))
	words.nz <- unlist (lapply (segms.nz, function(x) if (is.na(x[1])) NA else collapse(x)))

	# check for eregexp metacharacters
	meta <- gregexpr ("[][\\(\\)\\{\\}\\.\\+\\*\\^\\\\\\$\\?\\|]", words.z)[[1]]
	meta [is.na(meta)] <- -1
	if (meta != -1) {
		tmp <- strsplit (words.z, "")[[1]] [meta]
		tmp <- collapse (sort(unique(tmp)), inter=", ")
		stop ("Extended regular expressions metacharacters in the data: ", tmp, ".")
	}

	# check for transcription metacharacters
	tmp <- transcription$data[,transcription$cols$grapheme] != transcription$data[,transcription$cols$meta]
	if (any(tmp)) {
		tmp2 <- paste0 ("(",collapse(transcription$data[tmp,transcription$cols$grapheme],inter="|"),")")
		meta <- gregexpr (tmp2, words.z)[[1]]
		meta [is.na(meta)] <- -1
		if (meta != -1) {
			tmp3 <- strsplit (words.z, "")[[1]] [meta]
			tmp3 <- collapse (sort(unique(tmp3)), inter=", ")
			stop ("Transcription metacharacters in the data: ", tmp3, ".")
		}
	}

	# generate a lookup table for segments after separators and zeros are removed
	segpos.z <- lapply (segms.z, function (i)
		if (is.na(i[1])) NA else
			unlist (mapply (rep, seq_along(i), nchar(i), SIMPLIFY=F))
	)
	segpos.nz <- mapply (function (p, m)
		p [p %nin% grep(transcription$zero, m)]
		, segpos.z, segms.z, SIMPLIFY=F)

	# wrap in an object
	res <- list (
		cols = list (list (aligned=col.aligned)),			# an overkill, but flexible for the future and in line with transcription
		data = data,
		names = name,
		segms = list (list (z=segms.z, nz=segms.nz)),
		segpos = list (list (z=segpos.z, nz=segpos.nz)),
		separators = separator,
		trans = list (transcription),
		words = list (list (z=words.z, nz=words.nz))
	)
	class(res) <- "soundcorrs"

	# return the result
	return (res)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp allPairs.soundcorrs ------------------------------------------------------------------------ <<< -

#' @title Produce a list of all sound correspondences and all pairs in which they are attested.
#' @description Take all segment-to-segment correspondences in the dataset, and produce for each a section composed of a title, a contingency table of all renderings of the given segment, and subsections listing all word pairs in which the given rendering is attested, all nicely formatted.
#' @param data [character] The dataset. Only datasets with two languages are supported.
#' @param file [character] Name of the file to write the formatted list to. If \code{NULL}, the output will be printed to the screen. Defaults to \code{NULL}.
#' @param count [character] Report the absolute number of times or words, or relative to how many times or in how many words the given segments co-occur in L1 or L2. Accepted values are \code{"a(bs(olute))"} and \code{"r(el(ative))"}. Defaults to "a".
#' @param unit [character] Count how many times a correspondence occurs or in how many words it occurs. Accepted values are \code{"o(cc(ur(ence(s))))"} and \code{"w(or(d(s)))"}. Defaults to \code{"w"}.
#' @param direction [integer] If \code{1}, correspondences are in the order Language1 > Language2 ("x yields y"). If \code{2}, the order is Language2 < Language1 ("y originates from x"). Defaults to \code{1}.
#' @param cols [character vector] Which columns of the dataset to print. Can be a vector of names, \code{"aligned"} (the two columns with segmented, aligned words), or \code{"all"} (all columns). Defaults to \code{"aligned"}.
#' @param formatter [function] The function to which to pass unformatted data. Available formatters are: \code{formatter.none}, \code{formatter.html}, and \code{formatter.latex}. Defaults to \code{formatter.none}.
#' @param ... Additional arguments passed to \code{formatter}.
#' @details \code{\link{summary.soundcorrs}} can produce a table of all segment-to-segment correspondences in a dataset, and \code{\link{findExamples}} and \code{\link{findPairs}} can find all the pairs of words which realize those correspondences, but combining their outputs is a time-consuming, and unnecessary manual labour. The same, or at least a very similar result can be produced automatically by this function. Its output is divided into sections, each comprised of the appropriate slice of the contingency table, and a list of all the examples which are relevant for the given correspondence. The output can be raw, or formatted as LaTeX or HTML, and it is not too difficult to write one's own, custom formatting function.
#' @return [character] A formatted list of of all segment-to-segment correspondences and all pairs in which they are attested.
#' @seealso \code{\link{findPairs}}, \code{\link{summary.soundcorrs}}
#' @export
#' @importFrom utils browseURL
#' @examples
#' dataset <- loadSampleDataset ("data-abc")
#' allPairs (dataset)
#' allPairs (dataset, formatter=formatter.latex, cols=c("ORTHOGRAPHY.L1", "ORTHOGRAPHY.L2"))

allPairs <- function (data, file, count, unit, direction, cols, formatter, ...)
	UseMethod ("allPairs")

#' @export
allPairs.default <- function (data, file, count, unit, direction, cols, formatter, ...)
	stop ("This function does not know how to handle an object of class \"",class(data),"\".")

#' @export
allPairs.soundcorrs <- function (data, file=NULL, count="a", unit="w", direction=1,
								cols="aligned", formatter=formatter.none, ...) {

	# check exactly two languages
	if (length(data$names) != 2)
		stop ("This function only supports two languages.")

	# convert cols if necessary
	if (cols[1] == "all")
		cols <- colnames (data$data)
	else if (cols[1] == "aligned")
		cols <- sapply (data$cols, `[[`, "aligned")

	# check the other args
	count <- checkCount (count)
	unit <- checkUnit (unit)
	if (direction %nin% c(1,2))
		stop ("\"direction\" must be either 1 or 2.")

	# look one way or the other
	if (direction == 1)
		segms <- sort (unique (unlist (data$segms[[1]]$z)))
	else
		segms <- sort (unique (unlist (data$segms[[2]]$z)))

	# prepare to catch the result
	res <- ""

	# extract and format the needed bits
	for (i in segms) {

		# findPairs won't handle NA in a query
		if (!is.na(i)) {

			# prepare the section title
			res <- paste0 (res, formatter("section",i,...))

			# prepare the table
			tab <- summary (data, count, unit, direction)[i,]
			tab <- tab [tab!=0]
			res <- paste0 (res, formatter("table",tab,...))

			# prepare the words
			for (j in names(tab)) {
				if (!is.na(j)) {
					res <- paste0 (res, formatter("subsection",c(i,j),direction,...))
					res <- paste0 (res, if (direction == 1)
						formatter("data.frame", findPairs(data,i,j,exact=T,cols)$data, direction, ...)
					else
						formatter("data.frame", findPairs(data,j,i,exact=T,cols)$data, direction, ...))
				}
			}

		}

	}

	# print the result
	if (is.null(file)) {
		cat (res)
	} else {
		write (res, file)
		if (identical(formatter,formatter.html)) browseURL(normalizePath(file))
	}

	# return the result under the table
	invisible (res)

}

# - formatter.html - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - <<< -

#' A formatter for \code{\link{allPairs}}. This one formats to HTML.
#' @param what [character] What type of data is \code{x}.
#' @param x The object to be formatted.
#' @param direction [integer] If 1, correspondences are in the order Language1 > Language2 ("x yields y"). If 2, the order is Language2 < Language1 ("y originates from x"). Defaults to 1.
#' @return [character] Formatted x.
#' @export
#' @examples
#' # prepare sample dataset
#' fTrans <- system.file ("extdata", "trans-common.tsv", package="soundcorrs")
#' fData <- system.file ("extdata", "data-capitals.tsv", package="soundcorrs")
#' tmp.ger <- read.soundcorrs (fData, "German", "ALIGNED.German", fTrans)
#' tmp.pol <- read.soundcorrs (fData, "Polish", "ALIGNED.Polish", fTrans)
#' dataset <- merge (tmp.ger, tmp.pol)
#' # run allPairs
#' allPairs (dataset, unit="o", formatter=formatter.html)

formatter.html <- function (what, x, direction=1) {

	# four possibilities
	if (what == "section") {

		paste0 ("<h1><i>", x, "</i></h1>\n\n")

	} else if (what == "subsection") {

		res <- if (direction == 1)
			paste0 ("<h2><i>", x[1], "</i> &gt; <i>", x[2], "</i></h2>")
		else
			paste0 ("<h2><i>", x[1], "</i> &lt; <i>", x[2], "</i></h2>")
		paste0 (res, "\n\n")

	} else if (what == "table") {

		res <- "<table>\n"
		res <- paste0 (res, "\t<tr>\n", paste0("\t\t<th><i>",names(x),"</i></th>\n",collapse=""), "\t</tr>\n")
		res <- paste0 (res, "\t<tr>\n", paste0("\t\t<td>",x,"</td>\n",collapse=""), "\t</tr>\n")
		res <- paste0 (res, "</table>")
		paste0 (res, "\n\n")

	} else if (what == "data.frame") {

		res <- if (direction == 1)
			paste0 ("<i>", x[,1], "</i> &gt; <i>", x[,2], "</i>", collapse=", ")
		else
			paste0 ("<i>", x[,2], "</i> &lt; <i>", x[,1], "</i>", collapse=", ")
		paste0 (res, "\n\n")

	} else {

		stop ("\"what\" must be one of \"section\", \"subsection\", \"table\", or \"data.frame\".")

	}

}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ->>> -
# - formatter.latex - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -<<< -

#' A formatter for \code{\link{allPairs}}. This one formats to LaTeX.
#' @param what [character] What type of data is \code{x}.
#' @param x The object to be formatted.
#' @param direction [integer] If 1, correspondences are in the order Language1 > Language2 ("x yields y"). If 2, the order is Language2 < Language1 ("y originates from x"). Defaults to 1.
#' @return [character] Formatted x.
#' @export
#' @examples
#' # prepare sample dataset
#' fTrans <- system.file ("extdata", "trans-common.tsv", package="soundcorrs")
#' fData <- system.file ("extdata", "data-capitals.tsv", package="soundcorrs")
#' tmp.ger <- read.soundcorrs (fData, "German", "ALIGNED.German", fTrans)
#' tmp.pol <- read.soundcorrs (fData, "Polish", "ALIGNED.Polish", fTrans)
#' dataset <- merge (tmp.ger, tmp.pol)
#' # run allPairs
#' allPairs (dataset, unit="o", formatter=formatter.latex)

formatter.latex <- function (what, x, direction=1) {

	# four possibilities
	if (what == "section") {

		paste0 ("\\section{\\textit{", x, "}}\n\n")

	} else if (what == "subsection") {

		res <- if (direction == 1)
			paste0 ("\\subsection{\\textit{", x[1], "} > \\textit{", x[2], "}}")
		else
			paste0 ("\\subsection{\\textit{", x[1], "} < \\textit{", x[2], "}}")
		paste0 (res, "\n\n")

	} else if (what == "table") {

		res <- paste0 ("\\begin{tabular}{", paste0(rep("l",length(x)),collapse=""), "}\n")
		res <- paste0 (res, "\t\\toprule\n")
		res <- paste0 (res, "\t", paste0(paste0("\\textbf{",names(x),"}"), collapse="\t&\t"), "\t\\\\\n")
		res <- paste0 (res, "\t\\midrule\n")
		res <- paste0 (res, "\t", paste0(x,collapse="\t&\t"), "\t\\\\\n")
		res <- paste0 (res, "\t\\bottomrule\n")
		res <- paste0 (res, "\\end{tabular}\n")
		paste0 (res, "\n")

	} else if (what == "data.frame") {

		res <- if (direction == 1)
			paste0 ("\\textit{", x[,1], "} > \\textit{", x[,2], "}", collapse=", ")
		else
			paste0 ("\\textit{", x[,2], "} < \\textit{", x[,1], "}", collapse=", ")
		paste0 (res, "\n\n")

	} else {

		stop ("\"what\" must be one of \"section\", \"subsection\", \"table\", or \"data.frame\".")

	}

}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ->>> -
# - formatter.none - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - <<< -

#' A formatter for \code{\link{allPairs}}. This one does practically no formatting at all.
#' @param what [character] What type of data is \code{x}.
#' @param x The object to be formatted.
#' @param direction [integer] If 1, correspondences are in the order Language1 > Language2 ("x yields y"). If 2, the order is Language2 < Language1 ("y originates from x"). Defaults to 1.
#' @return [character] Formatted x.
#' @export
#' @importFrom utils capture.output
#' @examples
#' # prepare sample dataset
#' fTrans <- system.file ("extdata", "trans-common.tsv", package="soundcorrs")
#' fData <- system.file ("extdata", "data-capitals.tsv", package="soundcorrs")
#' tmp.ger <- read.soundcorrs (fData, "German", "ALIGNED.German", fTrans)
#' tmp.pol <- read.soundcorrs (fData, "Polish", "ALIGNED.Polish", fTrans)
#' dataset <- merge (tmp.ger, tmp.pol)
#' # run allPairs
#' allPairs (dataset, unit="o", formatter=formatter.none)

formatter.none <- function (what, x, direction=1)
		paste0 (collapse (what, "\t", capture.output(x), inter="\n"), "\n")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ->>> -

# -------------------------------------------------------------------------------------------------- >>> -
# - exp allCooccs.soundcross ----------------------------------------------------------------------- <<< -

#' @title Generate all co-occurrence contingency tables for a dataset.
#' @description Generate all correspondence-to-correspondence or correspondence-to-metadata contingnecy tables for a dataset.
#' @param data [soundcorrs] The dataset from which to draw frequencies. Only datasets with two languages are supported.
#' @param column [character] Name of the column with metadata. If \code{NULL}, sound correspondences are cross-tabulated with themselves. Defaults to \code{NULL}.
#' @param unit [character] Count how many times a correspondence occurs or in how many words it occurs. Accepted values are \code{"o(cc(ur(ence(s))))"} and \code{"w(or(d(s)))"}. Defaults to \code{"w"}.
#' @param count [character] Report the absolute number of times or words, or relative to how many times or in how many words the given segments co-occur in L1 or L2. Accepted values are \code{"a(bs(olute))"} and \code{"r(el(ative))"}. Defaults to "a".
#' @param bin [logical] Whether to bin tables before applying \code{fun} to them. Defaults to \code{TRUE}.
#' @details A contingency table such as produced by \code{\link{coocc}} may be insightful but more often than not statistical tests cannot be applied directly to it, or at least they would not produce meaningful results. This function splits such a table into blocks such that each block only contains the correspondences of a single segment. The resulting slices, additionally binned or not (cf. \code{\link{binTable}}), can be then passed to \code{\link{lapplyTest}} for a near-automatic application of a test.
#' @return [list] A list of tables.
#' @seealso \code{\link{coocc}}, \code{\link{binTable}}, \code{\link{lapplyTest}}
#' @export
#' @importFrom utils getTxtProgressBar setTxtProgressBar txtProgressBar
#' @examples
#' dataset <- loadSampleDataset ("data-abc")
#' allCooccs (dataset)
#' allCooccs (dataset, "DIALECT.L2", unit="o")

allCooccs <- function (data, column, count, unit, bin)
	UseMethod ("allCooccs")

#' @export
allCooccs.default <- function (data, column, count, unit, bin)
	stop ("This function does not know how to handle an object of class \"",class(data),"\".")

#' @export
allCooccs.soundcorrs <- function (data, column=NULL, count="a", unit="w", bin=T) {

# - allBins - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -<<< -

	allBins <- function (tab) {

		# nrow and ncol don't work for tables
		tab <- as.matrix (tab)

		# prepare to catch the results
		res <- list ()

		# go through the table
		for (i in 1:nrow(tab))
			for (ii in 1:ncol(tab))
				res[[paste0(rownames(tab)[i],"_",colnames(tab)[ii])]] <- binTable (tab, i, ii)

		# return the result
		return (res)

	}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ->>> -

	# prepare the table
	tab <- coocc (data, column, count, unit)

	# prepare the blocks
	blocks <- unlist (lapply (strsplit(rownames(tab),"_"), `[[`, 1))
	blocks.unq <- unique (blocks)

	# this might take a while
	pb <- txtProgressBar (min=0, max=length(blocks.unq), style=3)

	# cut up the table
	res <- lapply (blocks.unq, function (i) {

		# increase the progress bar
		pb <- setTxtProgressBar (pb, getTxtProgressBar(pb)+1)

		# get a chunk of the table
		nums <- which (blocks == i)
		tmp <- if (is.null(column)) tab[nums,-nums,drop=F] else tab[nums,,drop=F]

		# maybe bin it
		if (bin) tmp<-allBins(as.matrix(tmp))

		# and return it
		return (tmp)

	})

	# clean up
	close (pb)

	# prettify the result
	if (bin)
		res <- do.call (c, res)
	else
		names(res) <- blocks.unq
	res <- Filter (Negate(is.null), res)

	# and return it, with arguments stored as an attribute
	return (res)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp cbind.soundcorrs --------------------------------------------------------------------------- <<< -

#' @title Attach one or more columns to a \code{\link{soundcorrs}} object.
#' @description Attach one or more columns to a \code{\link{soundcorrs}} object. Note that sound correspondences attached with this function will not be usable as such.
#' @param data [soundcorrs] The \code{\link{soundcorrs}} object.
#' @param ... Objects to be attached.
#' @details Once a data frame is enclosed in a \code{\link{soundcorrs}} object, it is recommended that it not be manually altered in any way. \code{cbind.soundcorrs} provides a safe way of adding a column to it.
#' @return [soundcorrs] The original \code{\link{soundcorrs}} object with the columns attached.
#' @export
#' @examples
#' dataset <- loadSampleDataset ("data-ie")
#' cbind (dataset, ID=1:nrow(dataset$data))
#' cbind (dataset, FAMILY="Indo-European")

cbind.soundcorrs <- function (data, ...) {

	# the attaching
	data$data <- cbind (data$data, ...)

	# the returning
	return (data)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp coocc.soundcorrs --------------------------------------------------------------------------- <<< -

#' @title Generate a contingency table of co-occurrences of sound correspondences with themselves, or with metadata.
#' @description Take all segment-to-segment correspondences in a dataset, and cross-tabulate them with themselves or with metadata taken from a separate column.
#' @param data [soundcorrs] The dataset from which to draw frequencies. Only datasets with two languages are supported.
#' @param column [character] Name of the column with metadata. If \code{NULL}, sound correspondences are cross-tabulated with themselves. Defaults to \code{NULL}.
#' @param count [character] Report the absolute number of times or words, or relative to how many times or in how many words the given segments co-occur in L1 or L2. Accepted values are \code{"a(bs(olute))"} and \code{"r(el(ative))"}. Defaults to "a".
#' @param unit [character] Count how many times a correspondence occurs or in how many words it occurs. Accepted values are \code{"o(cc(ur(ence(s))))"} and \code{"w(or(d(s)))"}. Defaults to \code{"w"}.
#' @details A set of segmented and aligned word pairs/triples/..., such as one held in a \code{\link{soundcorrs}} object, can be turned into a contingency table in more than one way. This function creates a table which details how often various sound correspondences co-occur in one word. Both rows and columns are named in the same way: L1 phoneme + underscore ("_") + L2 phoneme. The values in the table can be absolute or relative, and they can represent the number of times the given correspondence co-occurs, or the number of words in which it co-occurs. For example, in the pair German koala : French koala, the correspondence G a : Fr a (\code{"a_a"}) co-occurs twice: the correspondence of the first two a's co-occurs with the correspondence of the second two a's, and vice versa. When the numbers are relative, they add up to 1 in blocks where each block is an intersection of rows and columns whose names begin with the same segment, i.e. those which refer to the correspondences of the same segment. In the relative view, empty cells appear when the given correspondence never co-occurs, and therefore its relative frequency is 0 divided by 0.
#' @return [table] The contingency table. The values represent how often the given correspondence co-occurs in the same word with the other correspondence or with the piece of metadata (cf. \code{\link{summary}}).
#' @seealso \code{\link{summary.soundcorrs}}, \code{\link{allCooccs}}
#' @export
#' @examples
#' dataset <- loadSampleDataset ("data-abc")
#' coocc (dataset)
#' coocc (dataset, "DIALECT.L2")
#' round (coocc(dataset,"DIALECT.L2",count="r"), digits=3)

coocc <- function (data, column, count, unit)
	UseMethod ("coocc")

#' @export
coocc.default <- function (data, column, count, unit)
	stop ("This function does not know how to handle an object of class \"",class(data),"\".")

#' @export
coocc.soundcorrs <- function (data, column=NULL, count="a", unit="w") {

	# check the remaining args
	if (!is.null(column) && column %nin% colnames(data$data))
		stop ("Column \"", column, "\" missing from \"data\".")
	count <- checkCount (count)
	unit <- checkUnit (unit)

	# prep vars
	tmp <- lapply (data$segms, `[[`, "z")

	# as a side effect, this fills in the missing NAs
	tmp <- lapply (list.transpose(tmp), list.transpose)

	# convert to characters
	tmp <- rapply (tmp, collapse, inter="_", how="replace")

	# find the combinations
	rows <- cols <- c ()
	for (i in seq_along(tmp)) {

		# corr-to-corr
		if (is.null(column) && length(tmp[[i]])>1) {
			# combinations without repetition
			tmp2 <- combn (tmp[[i]], 2)
			# fill both triangles of the table (permutations)
			tmp2 <- cbind (tmp2, tmp2[2:1,])
			# remove duplicates, maybe
			if (unit=="w") tmp2<-t(unique(t(tmp2)))
			# separate into rows and cols
			rows <- c (rows, unlist(tmp2[1,]))
			cols <- c (cols, unlist(tmp2[2,]))

		# corr-to-column
		} else if (!is.null(column)) {
			# permutations with repetition
			tmp2 <- expand.grid (tmp[[i]], data$data[i,column])
			# remove duplicates, maybe
			if (unit=="w") tmp2<-unique(tmp2)
			# prepare for tabling
			rows <- c (rows, unlist(tmp2$Var1))
			cols <- c (cols, as.vector(tmp2$Var2))
		}

	}

	# make the table
	res <- table (rows, cols, useNA="ifany")

	# fix the names
	names(dimnames(res)) <-
		if (is.null(column))
			rep (collapse(data$names,inter="_"), 2)
		else
			c (collapse(data$names,inter="_"), column)

	# convert to relative, maybe
	if (count == "r")
		res <- tabAbs2Rel (res, column)

	# return the result
	return (res)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp findExamples.soundcorrs -------------------------------------------------------------------- <<< -

#' @title Find all pairs/triples/... with corresponding sequences of sounds.
#' @description Sift the dataset for word pairs/triples/... such that the first word in the first languages contains the first sequence, the one in the second language the second sequence, and so on.
#' @param data [soundcorrs] The dataset in which to look.
#' @param ... [character] Sequences for which to look. May be regular expressions as defined in R, or in the \code{\link{transcription}}. If an empty string, anything will be considered a match.
#' @param distance.start [integer] The allowed distance between segments where the sound sequences begin. A negative value means alignment of the beginning of sequences will not be checked. Defaults to -1.
#' @param distance.end [integer] The allowed distance between segments where the sound sequences end. A negative value means alignment of the end of sequences will not be checked. Defaults to -1.
#' @param na.value [numeric] Treat \code{NA}'s as matches (\code{0}) or non-matches (\code{-1})? Defaults to \code{0}.
#' @param zeros [logical] Take linguistic zeros into account? Defaults to \code{FALSE}.
#' @param cols [character vector] Which columns of the dataset to return as the result. Can be a vector of names, \code{"aligned"} (the two columns with segmented, aligned words), or \code{"all"} (all columns). Defaults to \code{"aligned"}.
#' @details One of the more time-consuming tasks, when working with sound correspondences, is looking for specific examples which realize the given correspondence. \code{findExamples} can fully automate this process. It has several arguments that can help fine-tune the search, of which perhaps the most important are \code{distance.start} and \code{distance.end}. It should be noted that their default values (\code{-1} for both) mean that \code{findExamples} will find every such pair/triple/... of words, that the first word contains the first query, the second word the second query, etc. -- regardless of whether these segments do in fact correspond to each other in the alignment. This is intentional, and stems from the assumption that in this case, false positives are generally less harmful, and most of all easier to spot than false negatives.
#'
#'	\code{findExamples} accepts regular expressions in queries, both such as are available in pure R, and such as have been defined in the \code{\link{transcription}}, in both notations accepted by \code{\link{expandMeta}}. It is highly recommended that the user acquaints him or herself with the concept, as it is in it that the true power of \code{findExamples} lies.
#' @return [df.findExamples] A list with two fields: \code{$data}, a data frame with found examples; and \code{$which}, a logical vector showing which rows of \code{data} are considered matches.
#' @seealso \code{\link{findPairs}}
#' @export
#' @importFrom utils combn
#' @examples
#' # In the examples below, non-ASCII characters had to be escaped for technical reasons.
#' # In the actual usage, Unicode is supported under BSD, Linux, and macOS.
#'
#' # prepare sample dataset
#' dataset <- loadSampleDataset ("data-capitals")
#' # find examples which have "a" in all three languages
#' findExamples (dataset, "a", "a", "a")
#' # find examples where German has schwa, and Polish and Spanish have a Vr sequence
#' findExamples (dataset, "\u0259", "Vr", "Vr")
#' # find examples where German has a-umlaut, Polish has a or e, and Spanish has any sound at all
#' findExamples (dataset, "\u00E4", "[ae]", "")
#' # find examples where German has a linguistic zero while Polish and Spanish do not
#' findExamples (dataset, "-", "[^-]", "[^-]", zeros=TRUE)
#' # find examples where German has schwa, and Polish and Spanish have a
#' findExamples (dataset, "\u0259", "a", "a", distance.start=-1, distance.end=-1)
#' # as above, but the schwa and the two a's must be in the same segment
#' findExamples (dataset, "\u0259", "a", "a", distance.start=0, distance.end=0)

findExamples <- function (data, ..., distance.start, distance.end, na.value, zeros, cols)
	UseMethod ("findExamples")

#' @export
findExamples.default <- function (data, ..., distance.start, distance.end, na.value, zeros, cols)
	stop ("This function does not know how to handle an object of class \"",class(data),"\".")

#' @export
findExamples.soundcorrs <-
	function (data, ..., distance.start=-1, distance.end=-1, na.value=0, zeros=F, cols="aligned") {

# - sift - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - <<< -

	sift <- function (x, s, dist) {
		# prepare vars
		tmp <- unlist (s)
		full <- if (all(is.na(tmp)) || max(tmp,na.rm=T)==0) 0 else seq(tmp)
		# convert to segpos
		xs <- mapply (function (x2,s2) {
			if (x2[1]==-1)		-1			# non-matches remain non-matches
			else if (x2[1]==0)	full		# 0 are catch-alls
			else				s2[x2]		# everyone else gets converted to segpos
		}, x, s, SIMPLIFY=F)
		# find all the combinations
		xs <- expand.grid (xs)
		# remove useless rows
		xs <- xs [apply(xs, 1, function(y) all(y!=-1)), , drop=F]
		# don't bother if there are no matches
		if (nrow(xs)==0) return(FALSE)
		# check pairwise distances
		if (dist>=0 & ncol(xs)>1) {
			xs <- rbind (combn(ncol(xs), 2, function(y) abs(xs[,y[1]]-xs[,y[2]])))
			xs <- xs [apply(xs, 1, function(y) all(y<=dist)),,drop=F]
		}
		# return the result
		if (nrow(xs)==0) return(FALSE) else return(TRUE)
	}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ->>> -

	# unpack args
	quer <- c (...)

	# check args
	if (class(data) != "soundcorrs")
		stop ("\"data\" must be of class \"soundcorrs\".")
	if (any(lapply(quer,class) != "character"))
		stop ("Queries must be character strings.")
	if (any(is.na(quer) | is.nan(unlist(quer)) | is.null(quer)))
		stop ("Queries cannot be NA, NaN, or NULL.")
	if (length(quer) != length(data$names))
		stop ("There must be as many queries as there are languages in \"data\".")
	if (class(na.value) != "numeric" | na.value %nin% c(-1,0))
		stop ("\"na.value\" must be eithe -1 or 0.")
	if (class(zeros) != "logical")
		stop ("\"zeros\" must be either \"TRUE\" or \"FALSE\".")
	if (class(cols) != "character")
		stop ("\"cols\" must be a character string or strings.")

	# convert cols if necessary
	if (cols[1] == "all")
		cols <- colnames (data$data)
	else if (cols[1] == "aligned")
		cols <- sapply (data$cols, `[[`, "aligned")

	# check column names ok
	if (any(cols %nin% colnames(data$data)))
		stop ("One or more column names are missing from \"data\".")

	# convenience vars
	segpos <- lapply (data$segpos, `[[`, if (zeros) "z" else "nz")
	trans <- data$trans
	words <- lapply (data$words, `[[`, if (zeros) "z" else "nz")

	# expand metacharacters in queries
	expanded <- mapply (expandMeta, trans, quer)

	# find the starts of matches
	starts <- mapply (gregexpr, expanded, words, SIMPLIFY=F)

	# fix empty strings and NAs
	starts[quer==""] <- rapply (starts[quer==""], function(...) 0, how="replace")
	starts <- rapply (starts, function(x) if (is.na(x[1])) na.value else x, how="replace")

	# find the ends of matches
	ends <- rapply (starts, function (x)
		if (x[1] %in% c(-1,0)) x else
			x + attr(x,"match.length") - (attr(x,"match.length")[1]!=0)
	, how="replace")

	# sift for distances
	tmp <- list.transpose (segpos)
	starts <- mapply (sift, list.transpose(starts), tmp, MoreArgs=list(distance.start))
	ends <- mapply (sift, list.transpose(ends), tmp, MoreArgs=list(distance.end))

	# prepare the result
	res <- list (
		data = data$data [starts & ends, cols, drop=F],
		which = starts & ends
	)
	class(res) <- "df.findExamples"

	# and return it
	return (res)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp findPairs.soundcorrs ----------------------------------------------------------------------- <<< -

#' @title A convenience wrapper around \code{\link{findExamples}}.
#' @description Sift the dataset for word pairs such that the first word contains \code{x} and the second word contains \code{y} in the corresponding segment or segments.
#' @param data [soundcorrs] The dataset in which to look. Only datasets with two languages are supported.
#' @param x [character] The sequence to find in language1. May be a regular expression. If an empty string, anything will be considered a match.
#' @param y [character] The sequence to find in language2. May be a regular expression. If an empty string, anything will be considered a match.
#' @param exact [numeric] If 0 or \code{FALSE}, \code{distance.start}=\code{distance.end}=-1, \code{na.value}=0, and \code{zeros}=\code{FALSE}. If 0.5, \code{distance.start}=\code{distance.end}=1, \code{na.value}=0, and \code{zeros}=\code{FALSE}. If 1 or \code{TRUE}, \code{distance.start}=\code{distance.end}=0, \code{na.value}=-1, and \code{zeros}=\code{TRUE}. Defaults to 0.
#' @param cols [character vector] Which columns of the dataset to return as the result. Can be a vector of names, \code{"aligned"} (the two columns with segmented, aligned words), or \code{"all"} (all columns). Defaults to \code{"aligned"}.
#' @details Probably the most common usage of \code{\link{findExamples}} is with datasets containing pairs of words. This function is a simple wrapper around \code{\link{findExamples}} which hopes to facilitate its use in this most common case. Instead of the five arguments that \code{\link{findExamples}} requires, this function only takes two. It is, of course, at the cost of control but should a more fine-tuned search be required, \code{\link{findExamples}} can always still be used instead of \code{findPairs}.
#' @return [df.findExamples] A subset of the dataset, containing only the pairs with corresponding sequences. Warning: pairs with multiple occurrences of such sequences are only included once.
#' @seealso \code{\link{findExamples}}, \code{\link{allPairs}}
#' @export
#' @examples
#' # In the examples below, non-ASCII characters had to be escaped for technical reasons.
#' # In the actual usage, Unicode is supported under BSD, Linux, and macOS.
#'
#' # prepare sample dataset
#' dataset <- loadSampleDataset ("data-ie")
#' # run findPairs
#' findPairs (dataset, "a", "a")
#' findPairs (dataset, "e", "f", exact=0)
#' findPairs (dataset, "e", "f", exact=0.5)
#' findPairs (dataset, "e", "f", exact=1)

findPairs <- function (data, x, y, exact, cols)
	UseMethod ("findPairs")

#' @export
findPairs.default <- function (data, x, y, exact, cols)
	stop ("This function does not know how to handle an object of class \"",class(data),"\".")

#' @export
findPairs.soundcorrs <- function (data, x, y, exact=0, cols="aligned") {

	# check args
	if (length(data$names) != 2)
		stop ("This function only supports two languages. Please use \"findExamples()\" instead.")

	# prepare vars
	if (exact == 0) {
		dist <- -1
		nas <- 0
		zers <- F
	} else if (exact == 0.5) {
		dist <- 1
		nas <- 0
		zers <- F
	} else if (exact == 1) {
		dist <- 0
		nas <- -1
		zers <- T
	} else {
		stop ("\"exact\" must be 0, 0.5, 1, TRUE, or FALSE.")
	}

	# do the search
	res <- findExamples (data, x, y, distance.start=dist, distance.end=dist, na.value=nas, zeros=zers, cols=cols)

	# return the result
	return (res)


}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp merge.soundcorrs --------------------------------------------------------------------------- <<< -

#' @title Merge two or more \code{\link{soundcorrs}} objects.
#' @description Take multiple \code{\link{soundcorrs}} objects and combine them into one.
#' @param ... [soundcorrs] Objects to be merged.
#' @details Data can be turned into a \code{\link{soundcorrs}} object using either \code{\link{read.soundcorrs}} or, the less preferred method, the raw \code{\link{soundcorrs}} constructor. However, both can only produce \code{\link{soundcorrs}} objects with only the data for a single language in them, whereas the typical usage of the \code{soundcorrs} package would require it to hold data for several languages simultaneously. This function can be used to safely combine multiple \code{\link{soundcorrs}} objects into one. The individual objects can all hold data for one or more languages, the only requirement being that the data from the different languages are compatible with each other, i.e. that they have the same number of words, and each word has the same number of segments as its counterparts in the pair/triple/.... An error will be also thrown if two or more of the datasets contain a column with the same name and different content, or when they contain two or more rows with identical content.
#' @return [soundcorrs] The single, merged object.
#' @export
#' @examples
#' # path to sample data in the "wide format"
#' fNameData <- system.file ("extdata", "data-capitals.tsv", package="soundcorrs")
#' # path to a sample transcription
#' fNameTrans <- system.file ("extdata", "trans-common.tsv", package="soundcorrs")
#' ger <- read.soundcorrs (fNameData, "German", "ALIGNED.German", fNameTrans)
#' pol <- read.soundcorrs (fNameData, "Polish", "ALIGNED.Polish", fNameTrans)
#' merge (ger, pol)


merge.soundcorrs <- function (...) {

	# prepare args
	ones <- list (...)

	# check args
	if (length(ones) < 2)
		stop ("At least two \"soundcorrs\" objects are required.")
	if (any(sapply(ones,class) != "soundcorrs"))
		stop ("All arguments must be of class \"soundcorrs\".")

	# check all have the same nr of entries
	if (length(unique(sapply(ones, function(x) nrow(x$data)))) != 1)
		stop ("Differing number of entries between the specified objects.")

	# check segments align
	tmp <- unlist (lapply(ones,`[[`,"segms"), recursive=F)
	tmp <- lapply (tmp, `[[`, "z")
	tmp <- rapply (tmp, function(x) if (is.na(x[1])) NA else length(x))
	tmp <- matrix (tmp, ncol=length(unlist(lapply(ones,`[[`,"names"))))
	tmp <- apply (tmp, 1, function(x) length(unique(x[!is.na(x)]))<=1)
	tmp <- which (!tmp)
	if (length(tmp) > 0)
		stop ("Differing number of segments in entries: ", collapse(tmp,inter=", "), ".")

	# merge the data frames
	data <- Reduce (function (x, y)
		if (length(intersect(names(x),names(y)))==0) cbind(x,y) else merge(x,y,all=T,sort=F),
		lapply(ones, `[[`, "data"))
	if (nrow(data) != nrow(ones[[1]]$data))		# we've checked all have the same nr of rows
		stop ("Incompatible datasets. Perhaps conflicting column names or duplicate rows?")

	# wrap into an object
	res <- list (
		cols = unlist (lapply (ones, `[[`, "cols"), recursive=F),
		data = data,
		names = unlist (lapply (ones, `[[`, "names")),
		segms = unlist (lapply (ones, `[[`, "segms"), recursive=F),
		segpos = unlist (lapply (ones, `[[`, "segpos"), recursive=F),
		separators = unlist (lapply (ones, `[[`, "separators")),
		trans = unlist (lapply (ones, `[[`, "trans"), recursive=F),
		words = unlist (lapply (ones, `[[`, "words"), recursive=F)
	)
	class(res) <- "soundcorrs"

	# return the result
	return (res)

}
# -------------------------------------------------------------------------------------------------- >>> -
# - exp print.df.findExamples ---------------------------------------------------------------------- <<< -

#' @title Pretty printing for the result of \code{\link{findExamples}}.
#' @param x [df.findExamples] The output of \code{\link{findExamples}}.
#' @param ... Unused; only for consistency with \code{\link{print}}.
#' @details The output of \code{\link{findExamples}} is a list, potentially a very long one, and difficult to read. To make it easier to digest, this function only prints the \code{$data} element, i.e. the found matches.
#' @return [df.findExamples] The same object that was given as \code{x}.
#' @seealso \code{\link{findExamples}}
#' @export
#' @examples
#' dataset <- loadSampleDataset ("data-capitals")
#' findExamples (dataset, "a", "a", "a", cols="all")

print.df.findExamples <- function (x, ...) {

	# do the printing
	if (nrow(x$data) > 0)
		print (x$data)
	else
		cat ("No matches found.\n")

	# return the object, invisibly
	invisible (x)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp print.soundcorrs --------------------------------------------------------------------------- <<< -

#' A more reasonable display of a \code{\link{soundcorrs}} object.
#' @param x [soundcorrs] The \code{\link{soundcorrs}} object.
#' @param ... Unused; only for consistency with \code{\link{print}}.
#' @details A \code{\link{soundcorrs}} may be quite large and therefore difficult to digest for a human. This function reduces it to a brief, easy to understand summary.
#' @return [soundcorrs] The same object that was given as \code{x}.
#' @seealso \code{\link{soundcorrs}}
#' @export
#' @examples
#' dataset <- loadSampleDataset ("data-abc")
#' dataset

print.soundcorrs <- function (x, ...) {

	# print the data
	cat ("A \"soundcorrs\" object.\n")
	cat (paste0("  Languages (", length(x$names), "): ", collapse(x$names, inter=", "), ".\n"))
	cat (paste0("  Entries: ", nrow(x$data), ".\n"))
	cat (paste0("  Columns (", ncol(x$data), "): ", collapse(colnames(x$data),inter=", "), ".\n"))
	cat ("\n")

	# and return it under the counter
	invisible (x)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp read.soundcorrs ---------------------------------------------------------------------------- <<< -

#' @title Read data for a single language from a tsv file.
#' @description Read the data for one language, from a file in the wide format, and combine it with metadata into a \code{\link{soundcorrs}} object. To obtain a \code{soundcorrs} object containing data for multiple languages, see \code{\link{merge.soundcorrs}}.
#' @param file [character] Path to the data file in the wide format.
#' @param name [character] Name of the language.
#' @param col.aligned [character] Name of the column with the aligned words.
#' @param transcription [character] Path to the file with the transcription.
#' @param separator [character] String used to separate segments in \code{col.aligned}. Defaults to \code{"\\|"}.
#' @details The constructor for the \code{\link{soundcorrs}} class requires a data frame and a \code{\link{transcription}} object which means that the user would need to first read both from a file, and only then pass them to the constructor. This function saves these two steps. In addition, it attaches the name of the file to the object, which allows for easier identification later. It is recommended to use \code{read.soundcorrs} instead of the raw \code{\link{soundcorrs}} constructor whenever possible.
#' @return [scOne] An object containing the data and metadata for one language.
#' @seealso \code{\link{soundcorrs}}
#' @export
#' @importFrom utils read.table
#' @examples
#' # path to sample data in the "wide format"
#' fNameData <- system.file ("extdata", "data-ie.tsv", package="soundcorrs")
#' # path to a sample transcription
#' fNameTrans <- system.file ("extdata", "trans-common.tsv", package="soundcorrs")
#' ger <- read.soundcorrs (fNameData, "Latin", "LATIN", fNameTrans)

read.soundcorrs <- function (file, name, col.aligned, transcription, separator="\\|") {

	# check args
	tmp <- list (file=file, name=name, col.aligned=col.aligned, transcription=transcription, separator=separator)
	err <- which (lapply(tmp,class) != "character")
	if (length(err) > 0)
		stop ("\"", names(tmp)[err[1]], "\" must be a character string.")

	# read in the data
	data <- read.table (file, header=T, stringsAsFactors=F, quote="")

	# pack data into an object
	res <- soundcorrs (data, name, col.aligned, read.transcription(transcription), separator)
	attr(res,"file") <- file

	# return the result
	return (res)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp subset.soundcorrs -------------------------------------------------------------------------- <<< -

#' @title Return a subset of sound correspondences data which meets a condition.
#' @description Reduce a \code{\link{soundcorrs}} object to just those word pairs/triples/... which meet a certain condition.
#' @param x [soundcorrs] The dataset to be subsetted.
#' @param condition [logical] The condition the subsetted data must meet.
#' @param ... Unused; only for consistency with \code{\link{subset}}.
#' @details Once a data frame is enclosed in a \code{\link{soundcorrs}} object, it is recommended that it not be manually altered in any way. \code{subset.soundcorrs} provides a safe way of subsetting it.
#' @return [soundcorrs] A soundcorrs object containing the subsetted dataset.
#' @export
#' @examples
#' # In the examples below, non-ASCII characters had to be escaped for technical reasons.
#' # In actual usage, all soundcorrs functions accept characters from beyond ASCII.
#'
#' dataset <- loadSampleDataset ("data-capitals")
#' subset (dataset, OFFICIAL.LANGUAGE=="German")
#' subset (dataset, grepl("German",OFFICIAL.LANGUAGE))
#' subset (dataset, findExamples(dataset, "\u00E4", "e", "")$which)  # a-diaeresis

subset.soundcorrs <- function (x, condition, ...) {

	# find which rows are left after subsetting
	tmp <- eval (substitute(condition), x$data, parent.frame())
	nums <- which (tmp)

	# create a subsetted soundcorrs object
	res <- x
	res$data <- res$data [nums,]
	res$segms <- lapply (res$segms, function(x) lapply(x,`[`,nums))
	res$segpos <- lapply (res$segpos, function(x) lapply(x,`[`,nums))
	res$words <- lapply (res$words, function(x) lapply(x,`[`,nums))

	# return the result
	return (res)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp summary.soundcorrs ------------------------------------------------------------------------- <<< -

#' @title Generate a segment-to-segment contingency table for two languages.
#' @description Produce a contingency table detailing all segment-to-segment correspondences in a dataset.
#' @param object [soundcorrs] The dataset from which to draw frequencies. Only datasets with two languages are supported.
#' @param count [character] Report either the absolute or the relative numbers?. Accepted values are \code{"a(bs(olute))"} and \code{"r(el(ative))"}. Defaults to "a".
#' @param unit [character] Count how many times a correspondence occurs or in how many words it occurs? Accepted values are \code{"o(cc(ur(ence(s))))"} and \code{"w(or(d(s)))"}. Defaults to \code{"w"}.
#' @param ... Unused; only for consistency with \code{\link{print}}.
#' @details A set of segmented and aligned word pairs/triples/..., such as one held in a \code{\link{soundcorrs}} object, can be turned into a contingency table in more than one way. Perhaps the simplest option is to see how often various segments from one language correspond to various segments from another language, which is the kind of table this function produces. Correspondences can be reported in absolute or relative numbers, and can represent the number of times the given correspondence occurs, or in how many words it occurs (the same correspondence can occur more than once in a single pair/triple/... of words, e.g. in German koala : French koala, the correspondence G a : Fr a occurs twice). When the numbers are relative, each row in the table adds up to 1. In theory, \code{summary.soundcorrs} can support a \code{\link{soundcorrs}} objects with any number of languages in it, but the legibility of the output drops very quickly when that number exceeds two.
#' @return [table] The contingency table. The values represent how often the given segments correspond to each other, not how often they co-occur in the same word (cf. \code{\link{coocc}}).
#' @seealso \code{\link{coocc}}
#' @export
#' @examples
#' dataset <- loadSampleDataset ("data-ie")
#' summary (dataset)
#' round (summary(dataset,count="r"), digits=3)
#' summary (dataset, unit="o")

summary.soundcorrs <- function (object, count="a", unit="w", ...) {

	# check args
	count <- checkCount (count)
	unit <- checkUnit (unit)

	# extract the segments
	tmp <- lapply (object$segms, `[[`, "z")

	# as a side effect, this fills in the missing NAs
	tmp <- lapply (list.transpose(tmp), list.transpose)

	# remove duplicates, maybe
	if (unit == "w")
		tmp <- lapply (tmp, unique)

	# reformat back
	tmp <- list.transpose (lapply(tmp,list.transpose))
	tmp <- lapply (tmp, unlist)

	# make the table
	res <- table (tmp, useNA="ifany")

	# fix the names
	names(dimnames(res)) <- object$names

	# convert to relative, maybe
	if (count == "r")
		res <- tabAbs2Rel (res, "")

	# return the result
	return (res)

}

# -------------------------------------------------------------------------------------------------- >>> -
