# vim: set shiftwidth=4 tabstop=4 foldmarker=<<<,>>>:

# = helpers ======================================================================================== <<< =

# - %.% -------------------------------------------------------------------------------------------- <<< -

#' @title Compose two functions.
#' @description Function composition, acts like \code{.} in Haskell, only limited to two functions.
#' @param f [function] The outer function.
#' @param g [function] The inner function.
#' @return [function] Function composed of \code{f} and \code{g}.
#' @keywords internal
#' @examples
#' f <- function(x) x+1
#' g <- function(x) x*2
#' lapply (1:3, soundcorrs:::'%.%' (f,g))

'%.%' <- function (f, g)
	function (...) f(g(...))

# -------------------------------------------------------------------------------------------------- >>> -
# - %nin% ------------------------------------------------------------------------------------------ <<< -

#' @title The inverse of \%in\%.
#' @description The inverse (negation) of \%in\%.
#' @param x The needle.
#' @param y The haystack.
#' @return [logical] \code{TRUE} iff \code{x} is not an element of \code{y}.
#' @keywords internal
#' @examples
#' if (soundcorrs:::'%nin%' (1,c(1))) print("What sorcery is this?!")

'%nin%' <- function (x, y)
	!(x %in% y)

# -------------------------------------------------------------------------------------------------- >>> -
# - checkCount-------------------------------------------------------------------------------------- <<< -

#' @title Check if the \code{count} argument is correct.
#' @description Makes sure the \code{count} argument has one of the many available values.
#' @param count [character] The string to check.
#' @return [character] Either \code{"a"} or \code{"r"}.
#' @keywords internal
#' @examples
#' soundcorrs:::checkCount ("abs")
#' soundcorrs:::checkCount ("absolute")

checkCount <- function (count) {

	# some shortcuts are allowed, so check for those
	if (count %in% c("a", "abs", "absolute"))
		return ("a")
	else if (count %in% c("r", "rel", "relative"))
		return ("r")
	else
		stop ("\"count\" must refer to \"absolute\" or \"relative\".")

}

# -------------------------------------------------------------------------------------------------- >>> -
# - checkUnit -------------------------------------------------------------------------------------- <<< -

#' @title Check if the \code{unit} argument is correct.
#' @description Makes sure the \code{unit} argument has one of the many available values.
#' @param unit [character] The string to check.
#' @return [character] Either \code{"o"} or \code{"w"}.
#' @keywords internal
#' @examples
#' soundcorrs:::checkUnit ("occ")
#' soundcorrs:::checkUnit ("occurrence")

checkUnit <- function (unit) {

	# some shortcuts are allowed, so check for those
	if (unit %in% c("o","occ","occurrence","occurrences"))
		return ("o")
	else if (unit %in% c("w", "wor", "word", "words"))
		return ("w")
	else
		stop ("\"unit\" must refer to \"occurrences\" or \"words\".")

}

# -------------------------------------------------------------------------------------------------- >>> -
# - collapse --------------------------------------------------------------------------------------- <<< -

#' @title Paste and collapse.
#' @description Concatenate strings, possibly interspersing them with another string.
#' @param ... Objects to be pasted and collapsed.
#' @param inter [character] String with which to intersperse the result. Defaults to an empty string.
#' @return [character] The collapsed string.
#' @keywords internal
#' @examples
#' tmp <- c ("i", "am", "a", "sample", "vector")
#' identical (soundcorrs:::collapse(tmp), paste0(tmp,collapse=""))

collapse <- function (..., inter="")
	paste0 (collapse=inter, ...)

# -------------------------------------------------------------------------------------------------- >>> -
# - list.depth ------------------------------------------------------------------------------------- <<< -

#' @title Measure the depth of a nested list.
#' @description Check how many levels of nesting there are in a list.
#' @param x [list] The list whose depth is to be gauged.
#' @param d [integer] A technical variable, not supposed to be defined by the user. Defaults to \code{0}.
#' @return [integer] The depth of \code{x}.
#' @keywords internal
#' @examples
#' soundcorrs:::list.depth (list (1))
#' soundcorrs:::list.depth (list (list (1, list(2))))

list.depth <- function (x, d=0) {
	if (is.list(x))
		return (max (unlist (lapply (x, list.depth, d+1))))
	else
		return (d)
}

# -------------------------------------------------------------------------------------------------- >>> -
# - tabAbs2Rel ------------------------------------------------------------------------------------- <<< -

#' @title Convert a table from absolute to relative values.
#' @description Cross-tabulating sound correspondences with themselves may be misleading if the values are absolute, as it may well be that some segments simply do co-occur frequently, while others only rarely. The conversion is done in blocks: rows with the same prefix vs cols with the same prefix (if \code{column = NULL}) or vs all columns (if \code{column} is a string).
#' @param tab [table] The contingency table with sound correspondences.
#' @param column [character] Do columns contain the same data as rows (\code{NULL}) or some other data (a string)?
#' @return [table] The converted table.
#' @keywords internal
#' @examples
#' dataset <- sampleSoundCorrsData.abc
#' soundcorrs:::tabAbs2Rel (table(dataset,unit="o",direction=2), NULL)

tabAbs2Rel <- function (tab, column) {

	# find blocks in rows
	bl.rows <- unique (unlist (lapply (strsplit(rownames(tab),"_"), `[[`, 1)))

	# find blocks in columns, maybe
	if (is.null(column))
		bl.cols <- unique (unlist (lapply (strsplit(colnames(tab),"_"), `[[`, 1)))

	# do the conversion
	res <- tab
	for (i in bl.rows) {

		# select rows for the current block
		rows <- which (rownames(tab) %hasPrefix% i)

		# go through column blocks or take rows as a whole
		if (exists("bl.cols"))
			for (ii in bl.cols) {
				cols <- which (colnames(tab) %hasPrefix% ii)
				res[rows,cols] <- res[rows,cols] / sum(res[rows,cols])
			}
		else
			res[rows,] <- res[rows,] / sum(res[rows,])
	}

	# return the result
	return (res)
}

# -------------------------------------------------------------------------------------------------- >>> -
# - tableOne --------------------------------------------------------------------------------------- <<< -

#' @title Build a contingency table counting either occurrences or words.
#' @description Make a table out of one list.
#' @param rows [list] The list that will become both rows and cols, divided into words.
#' @param unit [character] Count occurrences or words? Accepted values are \code{"o(cc(urence(s))))"} and \code{"w(or(d(s)))"}.
#' @return [table] The contingency table.
#' @keywords internal
#' @examples
#' dataset <- sampleSoundCorrsData.abc
#' soundcorrs:::tableOne (dataset$segms[[1]]$z, "o")

tableOne <- function (rows, unit) {

	# convenience vars
	rows.unq <- sort (unique (unlist (rows)))

	# prepare to capture the result
	res <- matrix (0, ncol=length(rows.unq), nrow=length(rows.unq))
	rownames(res) <- colnames(res) <- rows.unq

	# count the occurrences or words
	for (i in 1:length(rows))
		if (unit == "o") {

			tmp <- table (expand.grid (rows[[i]], rows[[i]]))
			res[rownames(tmp),colnames(tmp)] <- res[rownames(tmp),colnames(tmp)] + tmp

		} else {

			res[rows[[i]],rows[[i]]] <- res[rows[[i]],rows[[i]]] + 1

		}

	# convert from matrix
	return (as.table(res))

}

# -------------------------------------------------------------------------------------------------- >>> -
# - tableTwo --------------------------------------------------------------------------------------- <<< -

#' @title Build a contingency table counting either occurrences or words.
#' @description Make a table out of two lists.
#' @param rows [list] The list that will become rows, divided into words.
#' @param cols [list] The list that will become cols, divided into words.
#' @param unit [character] Count occurrences or words? Accepted values are \code{"o(cc(urence(s))))"} and \code{"w(or(d(s)))"}.
#' @return [table] The contingency table.
#' @keywords internal
#' @examples
#' dataset <- sampleSoundCorrsData.abc
#' soundcorrs:::tableTwo (dataset$segms[[1]]$z, dataset$segms[[2]]$z, "o")

tableTwo <- function (rows, cols, unit) {

	# make the table for occurrences
	if (unit == "o") {

		# this is the easy variant
		res <- table (unlist(rows), unlist(cols))

	# or for words
	} else {

		# zip rows, cols, and word number
		wordpos <- mapply (rep, 1:length(rows), lapply(rows,length), SIMPLIFY=F)
		tmp <- mapply (c, unlist(rows), unlist(cols), unlist(wordpos), SIMPLIFY=F)

		# remove duplicate triples
		tmp <- unique (tmp)

		# and then make a table out of them
		res <- table (unlist(lapply(tmp,`[[`,1)), unlist(lapply(tmp,`[[`,2)))

	}

	# and return
	return (res)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - revChar ---------------------------------------------------------------------------------------- <<< -

#' @title Rev (reverse the order) for character strings.
#' @description Like \code{rev} in Haskell when applied to a string. When applied to a vector of strings, reverses each string but maintains their order. That's why it's not called "rev.character": to avoid having to use \code{\link{rev.default}} everywhere else.
#' @param x [character] String or a vector of strings to be reversed.
#' @return [character] Reversed string or a vector of reversed strings in the original order.
#' @keywords internal
#' @examples
#' soundcorrs:::revChar (c("kayak", "madam", "racecar"))

revChar <- function (x)
	unlist (lapply (strsplit (x,""), collapse %.% rev))

# -------------------------------------------------------------------------------------------------- >>> -
# - exp %hasPrefix% -------------------------------------------------------------------------------- <<< -

#' @title Check if a string starts with another string.
#' @description Within \code{soundcorrs}, primarily intended to extract rows and columns from contingency tables. Other than that, of general applicability.
#' @param x [character] The string or strings in which to look.
#' @param prefix [character] The string to look for. May be a regular expression.
#' @return [logical] \code{TRUE} iff \code{x} begins with \code{prefix}.
#' @export
#' @examples
#' "loans.tsv" %hasPrefix% "loans"
#' c("abc","bbc","cbc") %hasPrefix% "[bc]"

'%hasPrefix%' <- function (x, prefix)
	unlist (lapply (x, function(i) regexpr(prefix,i)[1] == 1))

# -------------------------------------------------------------------------------------------------- >>> -
# - exp %hasSuffix% -------------------------------------------------------------------------------- <<< -

#' @title Check if a string ends in another string.
#' @description Within \code{soundcorrs}, primarily intended to extract rows and columns from contingency tables. Other than that, of general applicability.
#' @param x [character] The string or strings in which to look.
#' @param suffix [character] The string to look for. May be a regular expression.
#' @return [logical] \code{TRUE} iff \code{x} ends with \code{suffix}.
#' @export
#' @examples
#' "loans.tsv" %hasSuffix% ".tsv"
#' c("aba","abb","abc") %hasSuffix% "[bc]"

'%hasSuffix%' <- function (x, suffix)
	unlist (lapply (x, function(i) {
		tmp <- gregexpr (suffix, i) [[1]]
		last <- length (tmp)
		tmp[last] + attr(tmp,"match.length")[last] == nchar(i) + 1
	}))

# -------------------------------------------------------------------------------------------------- >>> -
# - exp addSeparators ------------------------------------------------------------------------------ <<< -

#' @title Intersperse a vector of strings with a character or string.
#' @description Primarily intended to insert separators into a column of words, to facilitate manual segmentation and aligning.
#' @param x [character vector] The strings to be interspersed.
#' @param separator [character] The string with which to intersperse. Defaults to \code{"|"}.
#' @return [character vector] A vector of interspersed strings.
#' @export
#' @examples
#' addSeparators (c("word","mot","focal"), ".")

addSeparators <- function (x, separator="|")
	unlist (lapply (strsplit (as.character(x),""), paste0, collapse=separator))

# -------------------------------------------------------------------------------------------------- >>> -
# - exp binTable ----------------------------------------------------------------------------------- <<< -

#' @title Sum all rows and all columns in a table, except for the selected ones.
#' @description Useful for when the data are scarce and \code{\link{chisq.test}} returns a warning, or when a more specific analysis of the data is required.
#' @param x [data.frame/matrix/table] Table to be binned.
#' @param row [integer/vector] The rows to not be binned.
#' @param col [integer/vector] The columns to not be binned.
#' @return [table] Table with some of its data binned.
#' @export
#' @examples
#' mtx <- matrix (1:16, nrow=4, dimnames=list(paste0("r",1:4),paste0("c",1:4)))
#' binTable (mtx, 1, 1)
#' binTable (mtx, 1, c(1,3))

binTable <- function (x, row, col) {

	# fix if vector
	if (is.null(rownames(x))) {
		x2 <- as.data.frame (x)
		colnames(x2) <- "unnamed"
		if (length(row)==1 & length(col)>1) x2<-t(x2)
		if (is.null(colnames(x2))) colnames(x2)<-paste0("[,",1:ncol(x2),"]")
	} else {
		x2 <- x
	}

	# check args
	row <- unique (row)
	col <- unique (col)
	if (min(row)<1 | min(col)<1 | max(row)>nrow(x2) | max(col)>ncol(x2))
		stop ("It is required that 0 < row \u2264 nrow(x) and 0 < col < ncol(x).")

	# prepare
	res <- x2

	# collapse rows
	t.pl <- res[row,,drop=F]
	t.mi <- res[-row,,drop=F]
	if (nrow(t.mi) != 0)
		res <- rbind (t.pl, apply (t.mi, 2, sum))

	# and collapse columns
	t.pl <- res[,col,drop=F]
	t.mi <- res[,-col,drop=F]
	if (ncol(t.mi) != 0)
		res <- cbind (t.pl, apply (t.mi, 1, sum))

	# fix row names
	rownames(res) <-
		if (length(row) == nrow(x2))
			rownames (x2)
		else if (length(row) > 1)
			c (rownames(x2)[row], "other")
		else
			c (rownames(x2)[row], paste0("non-",rownames(x2)[row]))

	# and fix column names
	colnames(res) <-
		if (length(col) == ncol(x2))
			colnames (x2)
		else if (length(col) > 1)
			c (colnames(x2)[col], "other")
		else
			c (colnames(x2)[col], paste0("non-",colnames(x2)[col]))

	# return the result
	return (res)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp expandMeta --------------------------------------------------------------------------------- <<< -

#' @title Expand custom metacharacters to a regular expression.
#' @description Turn characters defined in a \code{\link{transcription}} as metacharacters into the corresponding regular expression.
#' @param transcription [transcription] The \code{\link{transcription}} to use.
#' @param x [character] The string containing metacharacters.
#' @return [character] A string with metacharacters expanded.
#' @export
#' @examples
#' dataset <- sampleSoundCorrsData.capitals
#' expandMeta (dataset$trans[[1]], "aN")
#' orth.german <- dataset$data$ORTHOGRAPHY.German
#' query <- "lin"
#' orth.german [ grep (query, orth.german) ]
#' query <- expandMeta (dataset$trans[[1]], "lin$")
#' orth.german [ grep (query, orth.german) ]

expandMeta <- function (transcription, x) {

	# expand metacharacters
	exploded <- unlist (strsplit (x, ""))
	res <- transcription$data [exploded, transcription$cols$meta]

	# replace unexpanded (unknown) graphemes with the original
	metas <- is.na (res)
	res[metas] <- exploded[metas]

	# return the result
	return (collapse(res))

}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp fitTable ----------------------------------------------------------------------------------- <<< -

#' @title Fit multiple models to multiple datasets.
#' @description Apply \code{\link{multiFit}} to all rows or all columns of a table.
#' @param models [list] A list of models to fit \code{data} to. Each element must be a list with at least two named fields: \code{formula} which contains the formula, and \code{start} which is a list of lists of starting estimates. Regarding the formula, the converter functions (\code{fun}, below) use "X" and "Y" for column names.
#' @param data [matrix/table] The data to fit \code{models} to.
#' @param margin [integer] As in \code{\link{apply}}: the subscripts which the fitting function (cf. \code{\link{multiFit}}) will be applied over. Accepted values are: \code{1} for rows, and \code{2} for columns.
#' @param conv [function] Function that converts vectors into data frames to which \code{models} will be fitted. Available functions are: \code{vec2df.id}, \code{vec2df.hist}, and \code{vec2df.rank}. Defaults to \code{vec2df.id}.
#' @param ... Additional arguments passed to \code{\link{multiFit}}).
#' @return [list.multiFit] A list of results returned by the fitting function (cf. \code{\link{multiFit}}).
#' @export
#' @examples
#' dataset <- summary (sampleSoundCorrsData.abc)
#' models <- list (
#' 	"model A" = list (
#' 		formula = "Y ~ a/X",
#' 		start = list (list(a=1))),
#' 	"model B" = list (
#' 		formula = "Y ~ a/(1+exp(1)^X)",
#' 		start = list (list(a=1)))
#' )
#' fitTable (models, dataset, 1, vec2df.rank)

fitTable <- function (models, data, margin, conv=vec2df.id, ...) {

	# check args
	if (class(data)[[1]] %nin% c("matrix","table"))
		# the [[1]] here is silly but check for r-devel 2020-01-03 r77630
		# thought class(data) has length 2
		# i wasn't able to replicate the error
		# but the [[1]] fixes it without breaking anything else
		stop ("\"data\" must be either a matrix or a table.")
	if (length(margin)>1 || margin %nin% c(1,2))
		stop ("\"margin\" must be either 1 or 2.")
	if (class(conv) != "function")
		stop ("\"conv\" must be a function.")

	# convert table to a list of data frames
	tmp <- apply (data, margin, conv)

	# run the whole thing
	res <- lapply (tmp, function(x) multiFit(models,x,...))

	# remove leftover attributes from multiFit()
	res <- lapply (res, function(x) { attr(x,"class")<-NULL; attr(x,"depth")<-NULL; x })

	# return the result, as "list.multiFit", for prettier printing
	class(res) <- "list.multiFit"
	attr(res,"depth") <- 2
	return (res)

}

# - vec2df.id - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -<<< -

#' A vector to data frame converter for \code{\link{fitTable}}. This one only does the necessary minimum.
#' @param data [numeric vector] The data to be converted.
#' @return [data.frame] Converted \code{data}.
#' @export
#' @examples
#' dataset <- summary (sampleSoundCorrsData.abc)
#' models <- list (
#' 	"model A" = list (
#' 		formula = "Y ~ a/X",
#' 		start = list (list(a=1))),
#' 	"model B" = list (
#' 		formula = "Y ~ a/(1+exp(1)^X)",
#' 		start = list (list(a=1)))
#' )
#' fitTable (models, dataset, 1, vec2df.id)

vec2df.id <- function (data) {

	# return the result
	return (data.frame (X=1:length(data), Y=data))

}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ->>> -
# - vec2df.hist - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -<<< -

#' A vector to data frame converter for \code{\link{fitTable}}. This one makes a histogram, and returns a data frame with midpoints and counts.
#' @param data [numeric vector] The data to be converted.
#' @return [data.frame] Converted \code{data}.
#' @export
#' @importFrom graphics hist
#' @examples
#' dataset <- summary (sampleSoundCorrsData.abc)
#' models <- list (
#' 	"model A" = list (
#' 		formula = "Y ~ a/X",
#' 		start = list (list(a=1))),
#' 	"model B" = list (
#' 		formula = "Y ~ a/(1+exp(1)^X)",
#' 		start = list (list(a=1)))
#' )
#' fitTable (models, dataset, 1, vec2df.hist)

vec2df.hist <- function (data) {

	# prepare the data
	tmp <- hist (data, plot=F)

	# return the result
	return (data.frame (X=tmp$mids, Y=tmp$counts))

}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ->>> -
# - vec2df.rank - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -<<< -

#' A vector to data frame converter for \code{\link{fitTable}}. This one orders data by rank.
#' @param data [numeric vector] The data to be converted.
#' @return [data.frame] Converted \code{data}.
#' @export
#' @examples
#' dataset <- summary (sampleSoundCorrsData.abc)
#' models <- list (
#' 	"model A" = list (
#' 		formula = "Y ~ a/X",
#' 		start = list (list(a=1))),
#' 	"model B" = list (
#' 		formula = "Y ~ a/(1+exp(1)^X)",
#' 		start = list (list(a=1)))
#' )
#' fitTable (models, dataset, 1, vec2df.rank)

vec2df.rank <- function (data) {

	# return the result
	return (data.frame (X=1:length(data), Y=sort(data,decreasing=T)))

}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ->>> -

# -------------------------------------------------------------------------------------------------- >>> -
# - exp lapplyTest --------------------------------------------------------------------------------- <<< -

#' @title Apply a function to a list.
#' @description Takes a list and applies to each of its elements a function, returning a list of outputs. Primary intended for tests of independence on a list of contingency tables.
#' @param x [list] The list to which to apply \code{fun}.
#' @param fun [function] The function which to apply to \code{data}. Must return an object containing an element named \code{p.value}. Defaults to \code{\link{chisq.test}}.
#' @param ... Additional arguments passed to \code{fun}.
#' @return [list.lapplyTest] A list of outputs of \code{fun}.
#' @export
#' @examples
#' dataset <- sampleSoundCorrsData.abc
#' lapplyTest (allTables(dataset))
#' lapplyTest (allTables(dataset), fisher.test, simulate.p.value=TRUE)

lapplyTest <- function (x, fun=chisq.test, ...) {

	# no point showing the warnings
	suppressWarnings (

	# apply fun
	res <- lapply (x, function (tab)
		tryCatch (

			fun (tab, ...),

			warning = function (w) {
				tmp <- fun (tab, ...)
				attr(tmp,"warning") <- w
				return (tmp)

			}, error = function (e) {
				tmp <- list (p.value=Inf)
				attr(tmp,"error") <- e
				return (tmp)

			}, finally =
				list (p.value=Inf)

		)
	)

	# to close suppressWarnings
	)

	# return the result, as "list.lapplyTest" for prettier printing
	class(res) <- "list.lapplyTest"
	return (res)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp long2wide ---------------------------------------------------------------------------------- <<< -

#' @title Convert from the long format (single entry per row) to the wide format (multiple entries per row).
#' @description Takes a data frame of word pairs/triples/..., each stored in multiple rows, and returns a data frame with the same words but each pair/triple/... stored in one row. WARNING: in the original data frame, entries from all languages must be in the same order.
#' @param data [data.frame] The dataset to be converted.
#' @param col.lang [character] Name of the column with language names. Defaults to \code{"LANGUAGE"}.
#' @param skip [character vector] Names of columns to not convert. Defaults to \code{NULL}.
#' @return [data.frame] A data frame in the wide format (multiple entries per row).
#' @export
#' @examples
#' # path to sample data in the "long format"
#' fName <- system.file ("extdata", "data-abc.tsv", package="soundcorrs")
#' long <- read.table (fName, header=TRUE)
#' wide <- long2wide (long, skip=c("ID"))

long2wide <- function (data, col.lang="LANGUAGE", skip=NULL) {

	# check column names
	tmp <- c(col.lang,skip)
	if (any(is.na(tmp) | tmp==""))
		stop ("Column names cannot be empty strings or NA.")
	if (any(tmp %nin% colnames(data)))
		stop ("One or more column names are missing from \"",substitute(data),"\".")

	# split the data
	res <- split (data, data[,col.lang])

	# make sure all have the same number of entries
	if (length(unique(unlist(lapply(res,nrow)))) != 1)
		stop ("Differing number of entries for different languages.")

	# separate columns to be skipped, and add suffixes to column names
	skips <- list ()
	for (i in 1:length(res)) {
		# separate columns to be skipped
		tmp <- res[[i]][,skip,drop=F]
		rownames(tmp) <- 1:nrow(tmp)
		skips <- c(skips, list(tmp))
		# remove columns to be skipped
		res[[i]] <- res[[i]][,-which(colnames(res[[i]]) %in% c(col.lang,skip))]
		# add suffixes to column names
		colnames(res[[i]]) <- paste0 (colnames(res[[i]]), ".", names(res)[i])
	}

	# make sure columns to be skipped are all the same
	if (length(unique(skips)) != 1)
		stop ("Differing values between columns specified in \"skip\".")

	# stitch everything together
	res <- Reduce (cbind, res, skips[[1]])

	# return the result
	return (res)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp multiFit ----------------------------------------------------------------------------------- <<< -

#' @title Fit multiple models to one dataset.
#' @description Apply a fitting function, with multiple models and multiple starting estimates, to one dataset.
#' @param models [list] A list of models to fit \code{data} to. Each element must be a list with at least two named fields: \code{formula} which contains the formula, and \code{start} which is a list of lists of starting estimates.
#' @param data [numeric data.frame/list] A list of vectors to fit \code{models} to.
#' @param fun [function] The function to use for fitting. Defaults to \code{nls}.
#' @param ... Additional arguments passed to \code{fun}.
#' @return [list.multiFit] A list of results returned by \code{fun} or, if it ended with an error, \code{NULL}.
#' @export
#' @importFrom stats nls
#' @examples
#' set.seed (27)
#' dataset <- data.frame (X=1:10, Y=(1:10)^2+runif(10,-10,10))
#' models <- list (
#' 	"model A" = list (
#' 		formula = "Y ~ X^a",
#' 		start = list (list(a=100), list(a=1))),
#' 	"model B" = list (
#' 		formula = "Y ~ a*(X+b)",
#' 		start = list (list(a=1,b=1)))
#' 	)
#' multiFit (models, dataset)

multiFit <- function (models, data, fun=nls, ...) {

# - fit.hlp - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -<<< -

	fit.hlp <- function (model, dat)
		tryCatch ({

			fun (formula=model$formula, data=dat, start=model$start[[1]])

		}, warning = function (w) {
			
			tmp <- fun (formula=model$formula, data=dat, start=model$start[[1]])
			attr(tmp,"warning") <- w
			return (tmp)

		}, error = function (e) {
			if (length(model$start) > 1) {
				fit.hlp (list(formula=model$formula,start=model$start[-1]), dat)
			} else {
				tmp <- NA
				attr(tmp,"error") <- e
				return (tmp)
			}

		}, finally = NULL

		)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ->>> -

	# check args
	if (class(data) %nin% c("data.frame","list"))
		stop ("\"data\" must be a data frame or a list.")
	if (class(models) != "list")
		stop ("\"models\" must be a list.")

	# model$start must be a list of lists for fit.hlp to work
	tmp <- unlist (lapply (models, function(x) list.depth(x$start)))
	if (any(tmp!=2))
		stop ("Element \"start\" in \"models\" must be a list of lists.")

	# do the fitting
	res <- lapply (models, fit.hlp, data)

	# return the result, as "list.multiFit" for a nice summary() to work
	class(res) <- "list.multiFit"
	attr(res,"depth") <- 1
	return (res)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp summary.list.lapplyTest -------------------------------------------------------------------- <<< -

#' @title A quick summary of the result of \code{\link{lapplyTest}}.
#' @description Take the output of \code{\link{lapplyTest}}, and extract from it only the noteworthy results.
#' @param object [list.lapplyTest] The output of \code{\link{lapplyTest}}.
#' @param p.value [double] Results above this value will not be reported. Defaults to 0.05.
#' @param ... Unused; only for consistency with \code{\link{summary}}.
#' @return A more human-friendly digest.
#' @export
#' @examples
#' dataset <- sampleSoundCorrsData.abc
#' lapplyTest (allTables(dataset))

summary.list.lapplyTest <- function (object, p.value=0.05, ...) {

	# only show results below the selected p.value
	res <- Filter (function(x) x$p.value<=p.value, object)

	# print the summary
	cat (paste0 ("Total results: ", length(object), "; with p-value \u2264 ", p.value,": ", length(res), ".\n"))

	# and the lucky results
	if (length(res) > 0) {
		for (i in 1:length(res)) {
			prnt <- if (any(c("error","warning") %in% names(attributes(res[[i]])))) "! " else "  "
			tmp <- unlist (strsplit (names(res)[i], "_"))
			prnt <-
				if (length(tmp)==1)
					paste0 (prnt, tmp)
				else if (length(tmp)==3)
					paste0 (prnt, tmp[1], ":", tmp[2], " with ", tmp[3])
				else
					paste0 (prnt, tmp[1], ":", tmp[2], " with ", tmp[3], ":", tmp[4])
			prnt <- paste0 (prnt, ": p-value = ", round(res[[i]]$p.value,3), "\n")
			cat (prnt)
		}
	}
	cat ("\n")

	# return the object under the table
	invisible (object)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp summary.list.multiFit ---------------------------------------------------------------------- <<< -

#' @title A comparison of the results produced by \code{\link{fitTable}} or \code{\link{multiFit}}.
#' @description Take the output of \code{\link{fitTable}} or \code{\link{multiFit}}, extract a specific metric from the fits, and present them in the form of a table.
#' @param object [list.multiFit] The output of \code{\link{fitTable}} or \code{\link{multiFit}}.
#' @param metric [character] The metric to extract from \code{object}. Available metrics are: "aic", "bic", "rss", and "sigma". Defaults to "rss".
#' @param ... Unused; only for consistency with \code{\link{summary}}.
#' @return A more human-friendly digest.
#' @export
#' @importFrom stats AIC BIC chisq.test resid
#' @examples
#' set.seed (27)
#' dataset <- data.frame (X=1:10, Y=(1:10)^2+runif(10,-10,10))
#' models <- list (
#' 	"model A" = list (
#' 		formula = "Y ~ X^a",
#' 		start = list (list(a=100), list(a=1))),
#' 	"model B" = list (
#' 		formula = "Y ~ a*(X+b)",
#' 		start = list (list(a=1,b=1)))
#' 	)
#' summary (multiFit(models,dataset))
#' summary (fitTable(models,as.matrix(dataset),1,vec2df.rank), "sigma")

summary.list.multiFit <- function (object, metric="rss", ...) {

	# check args
	if (metric %nin% c("aic","bic","rss","sigma"))
		stop ("\"metric\" must be one of \"aic\", \"bic\", \"rss\", or \"sigma\".")
	if ("depth" %nin% names(attributes(object)) || attr(object,"depth") %nin% c(1,2))
		stop ("\"object\" must have the attribute \"depth\" with value 1 or 2.")

	# prepare the data
	tmp <- if (attr(object,"depth")==1) object
		else if (attr(object,"depth")==2) unlist(object,recursive=F)

	# extract the metric
	res <- lapply (tmp, function (x)
		tryCatch ({
			if (metric == "aic") AIC(x)
			else if (metric == "bic") BIC(x)
			else if (metric == "rss") sum(resid(x)^2)
			else if (metric == "sigma") summary(x)$sigma
		}, error = function (e) {
			return (NA)
		})
	)

	# convert to a matrix
	res <- matrix (res, ncol=length(object))
	colnames(res) <- names (object)
	rownames(res) <-
		if (attr(object,"depth")==1) metric
		else if (attr(object,"depth")==2) names(object[[1]])

	# return the result
	return (as.data.frame(res))

}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp wide2long ---------------------------------------------------------------------------------- <<< -

#' @title Convert from the wide format (multiple entries per row) to the long format (single entry per row).
#' @description Takes a data frame of word pairs/triples/..., each stored in a single row, and returns a data frame with the same pairs/triples/... but with each word stored in its own row.
#' @param data [data.frame] The dataset to be converted.
#' @param suffixes [character vector] Suffixes used to differentiate column names; in the output, those will be used as language names.
#' @param col.lang [character] Name of the column in which language names are to be stored. Defaults to \code{"LANGUAGE"}.
#' @param strip [integer] The number of characters to strip from the beginning of suffixes when they are turned into language names. Defaults to 0.
#' @return [data.frame] A data frame in the long format (single entry per row).
#' @export
#' @examples
#' # path to sample data in the "wide format"
#' fName <- system.file ("extdata", "data-capitals.tsv", package="soundcorrs")
#' wide <- read.table (fName, header=TRUE)
#' long <- wide2long (wide, c(".German",".Polish",".Spanish"), strip=1)

wide2long <- function (data, suffixes, col.lang="LANGUAGE", strip=0) {

	# split the columns by suffixes
	cols <- lapply (suffixes, function(x) which(colnames(data) %hasSuffix% x))

	# make sure the number of columns is the same
	if (length(unique(lapply(cols,length))) > 1)
		stop ("Differing number of columns for different suffixes.")

	# make sure column names match
	nams <- lapply (1:length(cols), function (i)
		revChar (substr (revChar(colnames(data)[cols[[i]]]), nchar(suffixes[i])+1, 10000)))
	if (length(unique(unlist(nams))) != length(cols[[1]]))
		stop ("Differing column names for different suffixes.")

	# make sure all columns in the same order
	cols <- mapply (function(c,n) list(c[order(n)]), cols, nams)

	# shave off the beginnings of suffixes
	suffixes <- unlist (lapply (suffixes, substring, strip+1))

	# find suffixless columns
	sless <- which (1:ncol(data) %nin% unlist(cols))

	# the actual conversion
	cn <- c (sort(nams[[1]]), colnames(data)[sless], col.lang)
	res <- data.frame ()
	for (i in 1:length(cols)) {
		tmp <- data [, c(cols[[i]], sless)]
		tmp <- cbind (tmp, suffixes[i])
		colnames(tmp) <- cn
		res <- rbind (res, tmp)
	}

	# return the result
	return (res)

}

# -------------------------------------------------------------------------------------------------- >>> -

# ================================================================================================== >>> =
# = scOne ========================================================================================== <<< =

# - exp scOne -------------------------------------------------------------------------------------- <<< -

#' @title Constructor function for the \code{scOne} class.
#' @description Take a data frame containing data for one language, in the wide format, and combine it with metadata into a \code{scOne} object. In a normal workflow, the user should have no need to invoke this function other than through \code{\link{read.scOne}}.
#' @param data [data.frame] Data for one language.
#' @param name [character] Name of the language.
#' @param col.aligned [character] Name of the column with the aligned words.
#' @param transcription [transcription] The \code{\link{transcription}} for the given language.
#' @param separator [character] String used to separate segments in \code{col.aligned}. Defaults to \code{"\\|"}.
#' @return [scOne] A \code{scOne} object containing the data and metadata for one language.
#' @field cols [character list] Names of important columns.
#' @field data [data.frame] The original data.
#' @field name [character] Name of the language.
#' @field segms [character list] Words exploded into segments. With linguistic zeros preserved (\code{$z}) or removed (\code{$nz}).
#' @field segpos [integer list] A lookup list to check which character belongs to which segment. Counted with linguistic zeros preserved (\code{$z}) and removed (\code{$nz}).
#' @field separator [character] The string used as segment separator in \code{col.aligned}.
#' @field trans [transcription] The \code{\link{transcription}} object for the language.
#' @field words [character list] Words obtained by removing separators from the \code{col.aligned} column. With linguistic zeros (\code{$z}) or without them (\code{$nz}).
#' @export
#' @importFrom utils type.convert
#' @examples
#' fNameData <- system.file ("extdata", "data-capitals.tsv", package="soundcorrs")
#' fNameTrans <- system.file ("extdata", "trans-common.tsv", package="soundcorrs")
#' readData <- read.table (fNameData, header=TRUE)
#' readTrans <- read.transcription (fNameTrans)
#' ger <- scOne (readData, "German", "ALIGNED.German", readTrans)

scOne <- function (data, name, col.aligned, transcription, separator="\\|") {

	# check column name
	if (length(col.aligned) != 1)
		stop ("\"col.aligned\" must be exactly one column name.")
	if (col.aligned %nin% colnames(data))
		stop ("Column \"", col.aligned, "\" is missing from \"", substitute(data), "\".")

	# check transcription is a transcription
	if (class(transcription) != "transcription")
		stop ("\"", substitute(transcription), "\" must be of class \"transcription\".")

	# check separator is not empty string
	if (separator=="" | is.na(separator) | is.null(separator))
		stop ("\"separator\" cannot be an empty string, NA, or NULL.")

	# remove factors
	data <- type.convert (data, as.is=T)

	# explode into segments
	tmp <- as.vector (data[,col.aligned])
	segms.z <- strsplit (tmp, separator)
	if (!is.na(transcription$zero)) {
		tmp <- gsub (transcription$zero, "", tmp)
		tmp <- gsub (paste0(separator,"+"), separator, tmp)		# rm multiple separators
		tmp <- gsub (paste0("^",separator), "", tmp)			# rm separators from beginning, cause strsplit doesn't
		segms.nz <- strsplit (tmp, separator)
	} else {
		segms.nz <- segms.z
	}

	# check zeros always separate segments
	if (!is.na(transcription$zero)) {
		tmp.err <- Filter (function(x) nchar(x)>1 && grep(transcription$zero,x), unlist(segms.z))
		if (length(tmp.err) > 0)
			stop ("Linguistic zeros must be separate segments: ", collapse(tmp.err,inter=", "), ".")
	}

	# warn if the transcription doesn't cover everything
	tmp.used <- unique (unlist (segms.z))
	tmp.err <- tmp.used[tmp.used %nin% transcription$data[,transcription$cols$grapheme]]
	if (length(tmp.err) > 0)
		warning ("The following segments are not covered by the transcription: ", collapse(tmp.err,inter=", "), ".")

	# unexplode words, with and without zeros
	words.z <- unlist (lapply (segms.z, collapse))
	words.nz <- unlist (lapply (segms.nz, collapse))

	# check for eregexp metacharacters
	meta <- gregexpr ("[][\\(\\)\\{\\}\\.\\+\\*\\^\\\\\\$\\?\\|]", words.z)[[1]]
	if (meta != -1) {
		tmp <- strsplit (words.z, "")[[1]] [meta]
		tmp <- collapse (sort(unique(tmp)), inter=", ")
		stop ("Extended regular expressions metacharacters are used in the data: ", tmp, ".")
	}

	# check for transcription metacharacters
	tmp <- transcription$data[,transcription$cols$grapheme] != transcription$data[,transcription$cols$meta]
	if (any(tmp)) {
		tmp2 <- paste0 ("(",collapse(transcription$data[tmp,transcription$cols$grapheme],inter="|"),")")
		meta <- gregexpr (tmp2, words.z)[[1]]
		if (meta != -1) {
			tmp3 <- strsplit (words.z, "")[[1]] [meta]
			tmp3 <- collapse (sort(unique(tmp3)), inter=", ")
			stop ("Transcription metacharacters are used in the data: ", tmp3, ".")
		}
	}

	# generate a lookup table for segments after separators and zeros are removed
	segpos.z <- lapply (segms.z, function (i)
		unlist (mapply (rep, 1:length(i), nchar(i), SIMPLIFY=F)))
	if (!is.na(transcription$zero))
		segpos.nz <- mapply (function (p, m)
				p [p %nin% grep(transcription$zero, m)]
			, segpos.z, segms.z, SIMPLIFY=F)
	else
		segpos.nz <- segpos.z

	# wrap in an object
	res <- list (
		cols = list (aligned=col.aligned),			# an overkill, but flexible for the future and in line with transcription
		data = data,
		name = name,
		segms = list (z=segms.z, nz=segms.nz),
		segpos = list (z=segpos.z, nz=segpos.nz),
		separator = separator,
		trans = transcription,
		words = list (z=words.z, nz=words.nz)
	)
	class(res) <- "scOne"

	# return the result
	return (res)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp ngrams.scOne ------------------------------------------------------------------------------- <<< -

#' @title Frequencies of n-grams.
#' @description Find n-grams of specified length and return their counts.
#' @param data [scOne] A \code{\link{scOne}} object in which to look for n-grams.
#' @param n [integer] The length of n-grams to look for. Defaults to \code{1}.
#' @param zeros [logical] Include linguistic zeros? Defaults to \code{TRUE}.
#' @param as.table [logical] Return the result as a table? Defaults to \code{TRUE}.
#' @return [table] Table with counts of n-grams.
#' @export
#' @examples
#' # path to sample data in the "wide format"
#' fNameData <- system.file ("extdata", "data-capitals.tsv", package="soundcorrs")
#' # path to a sample transcription
#' fNameTrans <- system.file ("extdata", "trans-common.tsv", package="soundcorrs")
#' d.cap.ger <- read.scOne (fNameData, "German", "ALIGNED.German", fNameTrans)
#' ngrams (d.cap.ger, 2)
#' ngrams (d.cap.ger, 2, FALSE)

ngrams <- function (data, n, zeros, as.table)
	UseMethod ("ngrams")

#' @export
ngrams.default <- function (data, n, zeros, as.table)
	stop ("This function does not know how to handle an object of class \"",class(data),"\".")

#' @export
ngrams.scOne <- function (data, n=1, zeros=T, as.table=T) {

	# check args
	if (class(data) != "scOne")
		stop ("\"data\" must be of class \"scOne\".")
	if (n%%1!=0 || n<1)
		stop ("\"n\" must be a positive integer.")
	if (class(zeros) != "logical")
		stop ("\"zeros\" must be either TRUE or FALSE.")

	# prep the vars
	sep <- gsub ("\\\\", "", data$separator)
	segms <- if (zeros) data$segms$z else data$segms$nz

	# extract the ngrams
	res <- lapply (segms, function (x) {if (length(x) >= n) {
		froms <- 1:(length(x)-n+1)
		tos <- n:length(x)
		mapply (function(y,z) collapse(x[y:z],inter=sep), froms, tos)
	}})

	# prepare the result
	if (as.table) res<-table(unlist(res))

	# return the result
	return (res)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp print.scOne -------------------------------------------------------------------------------- <<< -

#' A more reasonable display of a \code{\link{scOne}} object.
#' @param x [scOne] The \code{\link{scOne}} object.
#' @param ... Unused; only for consistency with \code{\link{print}}.
#' @return A more human-friendly digest.
#' @export
#' @examples
#' # path to sample data in the "wide format"
#' fNameData <- system.file ("extdata", "data-capitals.tsv", package="soundcorrs")
#' # path to a sample transcription
#' fNameTrans <- system.file ("extdata", "trans-common.tsv", package="soundcorrs")
#' read.scOne (fNameData, "German", "ALIGNED.German", fNameTrans)

print.scOne <- function (x, ...) {

	# print the data
	cat ("A \"scOne\" object.\n")
	if (!is.null(attr(x,"file")))
		cat (paste0("  File: ", attr(x,"file"), ".\n"))
	cat (paste0("  Language: ", x$name, ".\n"))
	cat (paste0("  Entries: ", nrow(x$data), ".\n"))
	cat (paste0("  Columns (", ncol(x$data), "): ", collapse(colnames(x$data),inter=", "), ".\n"))
	if (!is.null(attr(x$trans,"file")))
		cat (paste0("  Transcription: ", attr(x$trans,"file"), ".\n"))
	cat ("\n")

	# and return it under the counter
	invisible (x)
}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp read.scOne --------------------------------------------------------------------------------- <<< -

#' @title Read data for a single language from a tsv file.
#' @description Read the data for one language, from a file in the wide format, and combine it with metadata into a \code{\link{scOne}} object.
#' @param file [character] Path to the data file in the wide format.
#' @param name [character] Name of the language.
#' @param col.aligned [character] Name of the column with the aligned words.
#' @param transcription [character] Path to the file with the transcription.
#' @param separator [character] String used to separate segments in \code{col.aligned}. Defaults to \code{"\\|"}.
#' @return [scOne] An object containing the data and metadata for one language.
#' @export
#' @importFrom utils read.table
#' @examples
#' # path to sample data in the "wide format"
#' fNameData <- system.file ("extdata", "data-capitals.tsv", package="soundcorrs")
#' # path to a sample transcription
#' fNameTrans <- system.file ("extdata", "trans-common.tsv", package="soundcorrs")
#' ger <- read.scOne (fNameData, "German", "ALIGNED.German", fNameTrans)

read.scOne <- function (file, name, col.aligned, transcription, separator="\\|") {

	# check args
	tmp <- list (file=file, name=name, col.aligned=col.aligned, transcription=transcription, separator=separator)
	err <- which (lapply(tmp,class) != "character")
	if (length(err) > 0)
		stop ("\"", names(tmp)[err[1]], "\" must be a character string.")

	# read in the data
	data <- read.table (file, header=T, stringsAsFactors=F, quote="")

	# pack data into an object
	res <- scOne (data, name, col.aligned, read.transcription(transcription), separator)
	attr(res,"file") <- file

	# return the result
	return (res)

}

# -------------------------------------------------------------------------------------------------- >>> -

# ================================================================================================== >>> =
# = soundcorrs ===================================================================================== <<< =

# - exp soundcorrs --------------------------------------------------------------------------------- <<< -

#' @title Constructor function for the \code{soundcorrs} class.
#' @description Take multiple \code{\link{scOne}} objects and combine them into a single \code{soundcorrs} object.
#' @param ... [scOne] Multiple \code{\link{scOne}} objects to be combined.
#' @return [soundcorrs] An object containing the data and metadata for multiple language.
#' @field cols [character list] Names of important columns.
#' @field data [data.frame] The original data, merged.
#' @field name [character] Names of the languages.
#' @field segms [character list] Words exploded into segments. With linguistic zeros preserved (\code{$z}) or removed (\code{$nz}).
#' @field segpos [integer list] A lookup list to check which character belongs to which segment. Counted with linguistic zeros preserved (\code{$z}) and removed (\code{$nz}).
#' @field separators [character] Strings used as segment separators in \code{cols$aligned}.
#' @field trans [transcription] \code{\link{transcription}} objects.
#' @field words [character list] Words obtained by removing separators from the \code{cols$aligned} columns. With linguistic zeros (\code{$z}) or without them (\code{$nz}).
#' @export
#' @examples
#' # path to sample data in the "wide format"
#' fNameData <- system.file ("extdata", "data-capitals.tsv", package="soundcorrs")
#' # path to a sample transcription
#' fNameTrans <- system.file ("extdata", "trans-common.tsv", package="soundcorrs")
#' ger <- read.scOne (fNameData, "German", "ALIGNED.German", fNameTrans)
#' pol <- read.scOne (fNameData, "Polish", "ALIGNED.Polish", fNameTrans)
#' spa <- read.scOne (fNameData, "Spanish", "ALIGNED.Spanish", fNameTrans)
#' dataset <- soundcorrs (ger, pol, spa)

soundcorrs <- function (...) {

	# convert the dots
	ones <- list (...)

	# make sure there are at least two languages
	if (length(ones) < 2)
		stop ("At least two \"scOne\" objects are required.")
	if (length(ones) > 2)
		warning ("In this version of \"soundcorrs\", most functions only support two languages.")

	# make sure all are scOne objects
	if (any(sapply(ones,class) != "scOne"))
		stop ("All arguments must be of class \"scOne\".")

	# make sure all have the same nr of entries
	if (length(unique(sapply(ones, function(x) nrow(x$data)))) != 1)
		stop ("Differing number of entries between the specified \"scOne\" objects.")

	# make sure segments align
	tmp.lng <- lapply (ones, function(x) sapply(x$segms$z,length))
	tmp.err <- c ()
	for (i in 1:length(tmp.lng[[1]])) {
		tmp <- sapply (tmp.lng, function(x) x[i])
		if (length(unique(tmp))!=1) tmp.err<-c(tmp.err,i)
	}
	if (length(tmp.err) > 0)
		stop ("Differing number of segments in entries: ", collapse(tmp.err,inter=", "), ".")

	# combine all data frames into one
	data <- Reduce (function(x,y) merge(x,y,sort=F), lapply(ones, `[[`, "data"))

	# make sure the merging worked
	if (nrow(data) != nrow(ones[[1]]$data))
		stop ("Incompatible datasets. Perhaps conflicting column names?")

	# wrap into an object
	res <- list (
		cols = lapply (ones, `[[`, "cols"),
		data = data,
		names = sapply (ones, `[[`, "name"),
		segms = lapply (ones, `[[`, "segms"),
		segpos = lapply (ones, `[[`, "segpos"),
		separators = sapply (ones, `[[`, "separator"),
		trans = lapply (ones, `[[`, "trans"),
		words = lapply (ones, `[[`, "words")
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
#' @export
#' @importFrom utils browseURL
#' @examples
#' dataset <- sampleSoundCorrsData.capitals
#' allPairs (dataset)
#' allPairs (dataset, formatter=formatter.latex, cols=c("ORTHOGRAPHY.German","ORTHOGRAPHY.Polish"))

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
		warning ("This function only supports two languages. The search is limited to ", data$names[1], " and ", data$names[2], ".")

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

		# prepare the section title
		res <- paste0 (res, formatter("section",i,...))

		# prepare the table
		tab <- summary (data, count, unit, direction)[i,]
		tab <- tab [tab!=0]
		res <- paste0 (res, formatter("table",tab,...))

		# prepare the words
		for (j in names(tab)) {
			res <- paste0 (res, formatter("subsection",c(i,j),direction,...))
			res <- paste0 (res, if (direction == 1)
				formatter("data.frame", findPairs(data,i,j,exact=T,cols)$data, direction, ...)
			else
				formatter("data.frame", findPairs(data,j,i,exact=T,cols)$data, direction, ...))
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
#' dataset <- sampleSoundCorrsData.capitals
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
#' dataset <- sampleSoundCorrsData.capitals
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
#' dataset <- sampleSoundCorrsData.capitals
#' allPairs (dataset, unit="o", formatter=formatter.none)
formatter.none <- function (what, x, direction=1)
		paste0 (collapse (what, "\t", capture.output(x), inter="\n"), "\n")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ->>> -

# -------------------------------------------------------------------------------------------------- >>> -
# - exp allTables.soundcross ----------------------------------------------------------------------- <<< -

#' @title Generate all contingency tables for a dataset.
#' @description Generate all correspondence-to-correspondence or correspondence-to-metadata contingnecy tables for a dataset.
#' @param data [soundcorrs] The dataset from which to draw frequencies. Only datasets with two languages are supported.
#' @param column [character] Name of the column with metadata. If \code{NULL}, sound correspondences are cross-tabulated with themselves. Defaults to \code{NULL}.
#' @param unit [character] Count how many times a correspondence occurs or in how many words it occurs. Accepted values are \code{"o(cc(ur(ence(s))))"} and \code{"w(or(d(s)))"}. Defaults to \code{"w"}.
#' @param count [character] Report the absolute number of times or words, or relative to how many times or in how many words the given segments co-occur in L1 or L2. Accepted values are \code{"a(bs(olute))"} and \code{"r(el(ative))"}. Defaults to "a".
#' @param direction [integer] If \code{1}, correspondences are in the order Language1 > Language2 ("x yields y"). If \code{2}, the order is Language2 < Language1 ("y originates from x"). Defaults to \code{1}.
#' @param bin [logical] Whether to bin tables before applying \code{fun} to them. Defaults to \code{TRUE}.
#' @return [list] A list of tables.
#' @export
#' @importFrom utils getTxtProgressBar setTxtProgressBar txtProgressBar
#' @examples
#' dataset <- sampleSoundCorrsData.abc
#' allTables (dataset)
#' allTables (dataset, "DIALECT.L2", unit="o")

allTables <- function (data, column, count, unit, direction, bin)
	UseMethod ("allTables")

#' @export
allTables.default <- function (data, column, count, unit, direction, bin)
	stop ("This function does not know how to handle an object of class \"",class(data),"\".")

#' @export
allTables.soundcorrs <- function (data, column=NULL, count="a", unit="w", direction=1, bin=T) {

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

	# check exactly two languages
	if (length(data$names) != 2)
		warning ("This function only supports two languages. The search is limited to ", data$names[1], " and ", data$names[2], ".")

	# prepare the table
	tab <- table (data, column, count, unit, direction)

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
#' @return [soundcorrs] The original \code{\link{soundcorrs}} object with the columns attached.
#' @export
#' @examples
#' dataset <- sampleSoundCorrsData.capitals
#' cbind (dataset, ID=1:nrow(dataset$data))
#' cbind (dataset, CONTINENT="Europe")

cbind.soundcorrs <- function (data, ...) {

	# the attaching
	data$data <- cbind (data$data, ...)

	# the returning
	return (data)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp char2value.soundcorrs ---------------------------------------------------------------------- <<< -

#' @title Convert characters to their values.
#' @description Convert a vector of characters to their values, as defined in the \code{\link{transcription}}.
#' @param data [soundcorrs] The \code{\link{soundcorrs}} object which holds the \code{\link{transcription}}.
#' @param language [character or integer] Which \code{\link{transcription}} to use; can be the name of the language, or its number (in which place it was listed when creating the \code{\link{soundcorrs}} object.
#' @param x [character vector] Characters to convert.
#' @return [character vector] Values, as defined in the \code{\link{transcription}}.
#' @export
#' @examples
#' dataset <- sampleSoundCorrsData.abc
#' segms <- findSegments (dataset, "a", "o", +1)
#' char2value (dataset, "L1", segms$L1)

char2value <- function (data, language, x)
	UseMethod ("char2value")

#' @export
char2value.default <- function (data, language, x)
	stop ("This function does not know how to handle an object of class \"",class(data),"\".")

#' @export
char2value.soundcorrs <- function (data, language, x) {

	# convert language to number, maybe
	if (typeof(language) == "character")
		language <- which (data$names == language)

	# make sure the data contain the language
	if (length(language)==0 || language>length(data$names))
		stop ("The specified \"language\" is missing from \"", substitute(data), "\".")

	# do the conversion
	res <- data$trans[[language]]$data [x, data$trans[[language]]$cols$value]

	# and return
	return (res)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp findPairs.soundcorrs ----------------------------------------------------------------------- <<< -

#' @title Find all pairs with corresponding sequences of sounds.
#' @description Sift the dataset for word pairs such that the first word contains \code{x} and the second word contains \code{y} in the corresponding segment or segments.
#' @param data [soundcorrs] The dataset in which to look. Only datasets with two languages are supported.
#' @param x [character] The sequence to find in language1. May be a regular expression. If an empty string, anything will be considered a match.
#' @param y [character] The sequence to find in language2. May be a regular expression. If an empty string, anything will be considered a match.
#' @param exact [logical] Only return exact, full-segment to full-segment matches? If \code{TRUE}, linguistic zeros are not ignored. Defaults to \code{FALSE}.
#' @param cols [character vector] Which columns of the dataset to return as the result. Can be a vector of names, \code{"aligned"} (the two columns with segmented, aligned words), or \code{"all"} (all columns). Defaults to \code{"aligned"}.
#' @return [df.findPairs] A subset of the dataset, containing only the pairs with corresponding sequences. Warning: pairs with multiple occurrences of such sequences are only included once.
#' @export
#' @examples
#' # In the examples below, non-ASCII characters had to be escaped for technical reasons.
#' # In actual usage, all soundcorrs functions accept characters from beyond ASCII.
#' dataset <- sampleSoundCorrsData.capitals
#' findPairs (dataset, "\u00E4", "e", cols=c("ORTHOGRAPHY.German","ORTHOGRAPHY.Polish"))  # a-diaeresis
#' findPairs (dataset, "a", "[ae]", cols="all")
#' findPairs (dataset, "\u0259", "Vr", exact=FALSE)  # schwa
#' findPairs (dataset, "\u0259", "Vr", exact=TRUE)  # schwa
#' subset (dataset, findPairs(dataset, "\u00E4", "e")$which)  # a-diaeresis

findPairs <- function (data, x, y, exact, cols)
	UseMethod ("findPairs")

#' @export
findPairs.default <- function (data, x, y, exact, cols)
	stop ("This function does not know how to handle an object of class \"",class(data),"\".")

#' @export
findPairs.soundcorrs <- function (data, x, y, exact=F, cols="aligned") {


# - is.entireSegm - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -<<< -

	is.entireSegm <- function (start, end, segpos) (

		# starts at the beginning of a segment
		(start==1 || segpos[start-1] < segpos[start])

		# ends at the end of a segment
		& (end==length(segpos) || segpos[end+1] > segpos[end])

	)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ->>> -
# - list2df - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -<<< -

	list2df <- function (data, segpos) {

		# which results are we even interested in
		nums <- which (unlist(data)!=-1)

		# like segpos, just for entries
		tmp <- unlist (lapply (data, length))
		entries <- unlist (mapply (rep, 1:length(tmp), tmp)) [nums]

		# now the starts and the ends, in characters
		starts.ch <- unlist (data) [nums]
		tmp <- unlist (lapply (data, attr, "match.length"))
		ends.ch <- starts.ch + tmp[nums] - 1

		# if x or y == 0, some ends.ch end up being 0
		tmp <- which (ends.ch < starts.ch)
		ends.ch[tmp] <- starts.ch[tmp]

		# convert characters to segments
		starts.sg <- mapply (function(i,ii) segpos[[i]][ii], entries, starts.ch)
		ends.sg <- mapply (function(i,ii) segpos[[i]][ii], entries, ends.ch)

		# return the result
		return (data.frame (ENTRY=entries,
							START.CH=starts.ch, END.CH=ends.ch,
							START.SG=starts.sg, END.SG=ends.sg))

	}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ->>> -

	# check exactly two languages
	if (length(data$names) != 2)
		warning ("This function only supports two languages. The search is limited to ", data$names[1], " and ", data$names[2], ".")

	# convert cols if necessary
	if (cols[1] == "all")
		cols <- colnames (data$data)
	else if (cols[1] == "aligned")
		cols <- sapply (data$cols, `[[`, "aligned")

	# check column names ok
	if (any(cols %nin% colnames(data$data)))
		stop ("One or more column names are missing from \"",substitute(data),"\".")

	# some convenience variables
	tmp <- if (exact) "z" else "nz"
	segpos <- lapply (data$segpos, `[[`, tmp) [c(1,2)]
	words <- lapply (data$words, `[[`, tmp) [c(1,2)]

	# expand metacharacters in x and y
	expanded <- mapply (expandMeta, data$trans[c(1,2)], list(x,y))

	# find the expanded x and y
	founds <- mapply (gregexpr, expanded, words, SIMPLIFY=F)

	# convert the finds to a saner format and merge them
	if (x == "")
		found <- merge (list2df(founds[[2]],segpos[[2]]), list2df(founds[[2]],segpos[[2]]), by="ENTRY")
	else if (y == "")
		found <- merge (list2df(founds[[1]],segpos[[1]]), list2df(founds[[1]],segpos[[1]]), by="ENTRY")
	else
		found <- merge (list2df(founds[[1]],segpos[[1]]), list2df(founds[[2]],segpos[[2]]), by="ENTRY")

	# sift the matches
	sifted <- apply (found, 1, function (n)
		if (exact) (
			# occupies the same segment
			abs (n["START.SG.x"]-n["START.SG.y"]) == 0
			& abs (n["END.SG.x"]-n["END.SG.y"]) == 0
			# and it is the entire segment (or empty string, meaning anything goes)
			& (x=="" | is.entireSegm (n["START.CH.x"], n["END.CH.x"], segpos[[1]][[n["ENTRY"]]]))
			& (y=="" | is.entireSegm (n["START.CH.y"], n["END.CH.y"], segpos[[2]][[n["ENTRY"]]]))
		) else (
			# occupies the same segment, or one to the left or right
			abs (n["START.SG.x"]-n["START.SG.y"]) <= 1
			& abs (n["END.SG.x"]-n["END.SG.y"]) <= 1
		)
	)
	nums <- unique (found [which(sifted), "ENTRY"])

	# return the result, as "df.findPairs" for prettier printing, for findSegments() and for subset()
	res <- list (
		data = data$data [nums, cols, drop=F],
		found = found [sifted, ],
		which = unlist (lapply (seq_len(nrow(data$data)), function(i) i %in% nums))
	)
	class(res) <- "df.findPairs"
	return (res)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp findSegments.soundcorrs -------------------------------------------------------------------- <<< -

#' @title Segments in relation to segments exhibiting a correspondence.
#' @description Find pairs with a specific sound correspondence, and extract from them the segments which come before or after the segments exhibiting that correspondence.
#' @param data [soundcorrs] The dataset in which to look. Only datasets with two languages are supported.
#' @param x [character] The sequence to find in language1. May be a regular expression. If an empty string, anything will be considered a match.
#' @param y [character] The sequence to find in language2. May be a regular expression. If an empty string, anything will be considered a match.
#' @param segment [integer] Number of the segment to be returned, in relation to segments containing \code{x} and \code{y}. Defaults to 0.
#' @return [list] Vectors for both languages, each of the same length as the dataset.
#' @export
#' @examples
#' # In the examples below, non-ASCII characters had to be escaped for technical reasons.
#' # In actual usage, all soundcorrs functions accept characters from beyond ASCII.
#' dataset <- sampleSoundCorrsData.capitals
#' findPairs (dataset, "\u00E4", "e")  # a-diaeresis
#' findSegments (dataset, "\u00E4", "e")
#' findSegments (dataset, "\u00E4", "e", -1)

findSegments <- function (data, x, y, segment)
	UseMethod ("findSegments")

#' @export
findSegments.default <- function (data, x, y, segment)
	stop ("This function does not know how to handle an object of class \"",class(data),"\".")

#' @export
findSegments.soundcorrs <- function (data, x, y, segment=0) {

	# check exactly two languages
	if (length(data$names) != 2)
		warning ("This function only supports two languages. The search is limited to ", data$names[1], " and ", data$names[2], ".")

	# make sure findPairs doesn't warn about >2 languages again
	suppressWarnings (

	# sift the dataset
	found <- findPairs (data, x, y, exact=T, cols="aligned") $found

	# turn on warnings back again
	)

	# which segment are we looking for
	# findPairs() had exact=T, so xs and ys must be the same
	target <- if (segment>=0) found$END.SG.x+segment else found$START.SG.x+segment

	# in which words does this segment even exist
	tmp <- unlist (lapply (data$segms[[1]]$z[found$ENTRY], length))
	nums <- which (target>0 & target<=tmp)
	entries <- found [nums, "ENTRY"]

	# prepare the result
	res.x <- res.y <- rep (NA, length(data$segms[[1]]$z))

	# and fill it with target segments
	for (i in unique(entries)) {
		res.x[i] <- collapse (data$segms[[1]]$z [[i]] [target[which(entries==i)]], inter=",")
		res.y[i] <- collapse (data$segms[[2]]$z [[i]] [target[which(entries==i)]], inter=",")
	}

	# prettify the result
	res <- list (res.x, res.y)
	names(res) <- data$names[c(1,2)]

	# and return it
	return (res)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp print.df.findPairs ------------------------------------------------------------------------- <<< -

#' @title Pretty printing for the result of \code{\link{findPairs}}.
#' @param x [df.findPairs] The output of \code{\link{findPairs}}.
#' @param ... Unused; only for consistency with \code{\link{print}}.
#' @return A more human-friendly digest.
#' @export
#' @examples
#' dataset <- sampleSoundCorrsData.capitals
#' findPairs (dataset, "a", "[ae]", cols="all")

print.df.findPairs <- function (x, ...) {

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
#' @return A more human-friendly digest.
#' @export
#' @examples
#' sampleSoundCorrsData.abc
#' sampleSoundCorrsData.capitals
#' sampleSoundCorrsData.ie

print.soundcorrs <- function (x, ...) {

	# print the data
	cat ("A \"soundcorrs\" object.\n")
	cat (paste0("  Languages: (", length(x$names), "): ", collapse(x$names, inter=", "), ".\n"))
	cat (paste0("  Entries: ", nrow(x$data), ".\n"))
	cat (paste0("  Columns (", ncol(x$data), "): ", collapse(colnames(x$data),inter=", "), ".\n"))
	cat ("\n")

	# and return it under the counter
	invisible (x)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp subset.soundcorrs -------------------------------------------------------------------------- <<< -

#' @title Return a subset of sound correspondences data which meets a condition.
#' @description Reduce a \code{\link{soundcorrs}} object to just those word pairs/triples/... which meet a certain condition.
#' @param x [soundcorrs] The dataset to be subsetted.
#' @param condition [logical] The condition the subsetted data must meet.
#' @param ... Unused; only for consistency with \code{\link{subset}}.
#' @return [soundcorrs] A soundcorrs object containing the subsetted dataset.
#' @export
#' @examples
#' # In the examples below, non-ASCII characters had to be escaped for technical reasons.
#' # In actual usage, all soundcorrs functions accept characters from beyond ASCII.
#' dataset <- sampleSoundCorrsData.capitals
#' subset (dataset, OFFICIAL.LANGUAGE=="German")
#' subset (dataset, grepl("German",OFFICIAL.LANGUAGE))
#' subset (dataset, findPairs(dataset, "\u00E4", "e")$which)  # a-diaeresis

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
#' @param count [character] Report the absolute number of times or words, or relative to how many times or in how many words the given segments co-occur in L1 or L2. Accepted values are \code{"a(bs(olute))"} and \code{"r(el(ative))"}. Defaults to "a".
#' @param unit [character] Count how many times a correspondence occurs or in how many words it occurs. Accepted values are \code{"o(cc(ur(ence(s))))"} and \code{"w(or(d(s)))"}. Defaults to \code{"w"}.
#' @param direction [integer] If \code{1}, correspondences are in the order Language1 > Language2 ("x yields y"). If \code{2}, the order is Language2 < Language1 ("y originates from x"). Defaults to \code{1}.
#' @param ... Unused; only for consistency with \code{\link{print}}.
#' @return [table] The contingency table.
#' @export
#' @examples
#' dataset <- sampleSoundCorrsData.abc
#' summary (dataset)
#' round (summary(dataset,count="r"), digits=3)
#' summary (dataset, unit="o")
#' summary (dataset, direction=2)

summary.soundcorrs <- function (object, count="a", unit="w", direction=1, ...) {

	# check exactly two languages
	if (length(object$names) != 2)
		warning ("This function only supports two languages. The search is limited to ", object$names[1], " and ", object$names[2], ".")

	# check the remaining args
	count <- checkCount (count)
	unit <- checkUnit (unit)
	if (direction %nin% c(1,2))
		stop ("\"direction\" must be either 1 or 2.")

	# look one way or the other
	if (direction == 1) {
		res <- tableTwo (object$segms[[1]]$z, object$segms[[2]]$z, unit)
		names(dimnames(res)) <- object$names[c(1,2)]
	} else {
		res <- tableTwo (object$segms[[2]]$z, object$segms[[1]]$z, unit)
		names(dimnames(res)) <- rev (object$names[c(1,2)])
	}

	# convert to relative, maybe
	if (count == "r")
		res <- tabAbs2Rel (res, "")

	# return the result
	return (res)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp table.soundcorrs --------------------------------------------------------------------------- <<< -

#' The base::\code{\link{table}} function.
#' @param ... base::\code{\link{table}}'s arguments.
#' @export
table <- function (...)
	UseMethod ("table")

#' @export
table.default <- function (...)
	base::table (...)

#' @title Generate a correspondence-to-correspondence or correspondence-to-metadata contingency table.
#' @description Take all segment-to-segment correspondences in a dataset, and cross-tabulate them with themselves or with metadata taken from a separate column.
#' @param data [soundcorrs] The dataset from which to draw frequencies. Only datasets with two languages are supported.
#' @param column [character] Name of the column with metadata. If \code{NULL}, sound correspondences are cross-tabulated with themselves. Defaults to \code{NULL}.
#' @param count [character] Report the absolute number of times or words, or relative to how many times or in how many words the given segments co-occur in L1 or L2. Accepted values are \code{"a(bs(olute))"} and \code{"r(el(ative))"}. Defaults to "a".
#' @param unit [character] Count how many times a correspondence occurs or in how many words it occurs. Accepted values are \code{"o(cc(ur(ence(s))))"} and \code{"w(or(d(s)))"}. Defaults to \code{"w"}.
#' @param direction [integer] If \code{1}, correspondences are in the order Language1 > Language2 ("x yields y"). If \code{2}, the order is Language2 < Language1 ("y originates from x"). Defaults to \code{1}.
#' @param ... Unused; only for consistency with \code{\link{table}}.
#' @return [table] The contingency table.
#' @export
#' @examples
#' dataset <- sampleSoundCorrsData.abc
#' table (dataset)
#' table (dataset, direction=2)
#' table (dataset, "DIALECT.L2")
#' round (table(dataset,"DIALECT.L2",count="r"), digits=3)

table.soundcorrs <- function (data, column=NULL, count="a", unit="w", direction=1, ...) {

	# check exactly two languages
	if (length(data$names) != 2)
		warning ("This function only supports two languages. The search is limited to ", data$names[1], " and ", data$names[2], ".")

	# check the remaining args
	count <- checkCount (count)
	unit <- checkUnit (unit)
	if (direction %nin% c(1,2))
		stop ("\"direction\" must be either 1 or 2.")

	# prepare the rows (correllations)
	if (direction == 1)
		rows <- mapply (function(x,y) paste0(x,"_",y), data$segms[[1]]$z, data$segms[[2]]$z, SIMPLIFY=T)
	else
		rows <- mapply (function(x,y) paste0(x,"_",y), data$segms[[2]]$z, data$segms[[1]]$z, SIMPLIFY=T)

	# prepare the cols (correllations or metadata)
	if (is.null(column))
		cols <- rows
	else
		cols <- mapply (rep, data$data[,column], lapply(rows,length), SIMPLIFY=F)

	# and make a table out of them
	res <- if (identical(rows,cols)) tableOne(rows,unit) else tableTwo(rows,cols,unit)

	# fix the names
	tmp <- if (direction==1) collapse(data$names[c(1,2)],inter="\u2192") else collapse(rev(data$names[c(1,2)]),inter="\u2190")
	names(dimnames(res)) <- if (is.null(column)) c(tmp,tmp) else c(tmp,column)

	# convert to relative, maybe
	if (count == "r")
		res <- tabAbs2Rel (res, column)

	# return the result
	return (res)

}

# -------------------------------------------------------------------------------------------------- >>> -

# ================================================================================================== >>> =
# = transcription ================================================================================== <<< =

# - exp transcription ------------------------------------------------------------------------------ <<< -

#' @title Constructor function for the \code{transcription} class.
#' @description Take a data frame containing transcription and turn it into a \code{transcription} object, as required by the \code{\link{soundcorrs}} constructor function. In a normal workflow, the user should have no need to call this function other than through \code{\link{read.transcription}}.
#' @param data [data.frame] Data frame containing the transcription and its meaning.
#' @param col.grapheme [character] Name of the column with graphemes. Defaults to \code{"GRAPHEME"}.
#' @param col.meta [character] Name of the column with the coverage of metacharacters. If empty string or \code{NA}, the column will be generated automatically. Defaults to \code{"META"}.
#' @param col.value [character] Name of the column with values of graphemes. Defaults to \code{"VALUE"}.
#' @return [transcription] A \code{transcription} object containing the provided data.
#' @field data [data.frame] The original data frame.
#' @field cols [character list] Names of the important columns in the data frame.
#' @field zero [character] A regular expression to catch linguistic zeros.
#' @export
#' @examples
#' # path to a sample transcription
#' fName <- system.file ("extdata", "trans-common.tsv", package="soundcorrs")
#' fut <- transcription (read.table(fName,header=TRUE))

transcription <- function (data, col.grapheme="GRAPHEME", col.meta="META", col.value="VALUE") {

	# check column names
	tmp <- c(col.grapheme,col.value)
	if (any(is.na(tmp) | tmp==""))
		stop ("Column names cannot be empty strings or NA.")
	if (any(tmp %nin% colnames(data)))
		stop ("One or more column names are missing from \"",substitute(data),"\".")

	# check for multiple definitions
	tmp <- unique (data [duplicated(data[,col.grapheme]), col.grapheme])
	if (length(tmp)>0)
		stop ("Multiple definitions for graphemes: ",collapse(tmp,inter=", "),".")
	rownames(data) <- data[,col.grapheme]

	# check for eregexp metacharacters
	tmp <- collapse (data[,col.grapheme])		# roundabout, but needed to find metas in >1-character graphemes
	meta <- gregexpr ("[][\\(\\)\\{\\}\\.\\+\\*\\^\\\\\\$\\?\\|]", tmp)[[1]]
	if (meta != -1) {
		tmp2 <- strsplit (tmp, "")[[1]] [meta]
		tmp2 <- collapse (sort(unique(tmp2)), inter=", ")
		stop ("Extended regular expressions metacharacters are used as graphemes: ", tmp2, ".")
	}

	# if needed, find metacharacters
	if (col.meta %nin% colnames(data)) {
		warning ("Missing the metacharacters column. The \"",col.meta,"\" column was generated.")

		# find which graphemes' values are subsets of other graphemes' values
		expl <- strsplit (as.vector(data[,col.value]), ",")
		meta <- list ()
		for (i in 1:length(expl)) {
			tmp <- c ()
			for (ii in 1:length(expl))
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

	# find symbol(s) for linguistic zero
	tmp <- data [data[,col.value]=="NULL" & !is.na(data[,col.value]), col.grapheme]
	if (identical(tmp,character(0))) {
		zero <- NA
		warning ("Linguistic zero is not defined in the transcription.")
	} else {
		zero <- paste0 ("(", collapse(tmp,inter="|"), ")")
	}

	# create an object
	res <- list (
		data = data,
		cols = list (grapheme=col.grapheme, meta=col.meta, value=col.value),
		zero = zero
		)
	class(res) <- "transcription"

	# return the result
	return (res)

}


# -------------------------------------------------------------------------------------------------- >>> -
# - exp print.transcription ------------------------------------------------------------------------ <<< -

#' A more reasonable display of a \code{\link{transcription}} object.
#' @param x [transcription] The transcription.
#' @param ... Unused; only for consistency with \code{\link{print}}.
#' @return A more human-friendly digest.
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
#' @return [transcription] A transcription object containing the read transcription.
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

# ================================================================================================== >>> =
