# vim: set shiftwidth=4 tabstop=4 foldmarker=<<<,>>>:

# - %.% -------------------------------------------------------------------------------------------- <<< -

#' @title Compose two functions. Haskell-inspired syntactic sugar.
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
#' if (soundcorrs:::'%nin%' (1,c(1))) print("What sorcery is this?")

'%nin%' <- function (x, y)
	!(x %in% y)

# -------------------------------------------------------------------------------------------------- >>> -
# - checkCount-------------------------------------------------------------------------------------- <<< -

#' @title Check if the \code{count} argument is correct.
#' @description Makes sure the \code{count} argument has one of the many available values.
#' @param count [character] The string to check.
#' @details Functions which produce contingency tables, \code{\link{coocc}} and \code{\link{summary}}, have an argument \code{count} which can take as many as six different values which all point to just two unique behaviours. \code{checkCount} reduces this multitude to just the two meaningful values. It also throws an error if \code{count} is none of the six possibilities.
#' @return [character] Either \code{"a"} or \code{"r"}.
#' @seealso \code{\link{checkUnit}}
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
#' @details Functions which produce contingency tables, \code{\link{coocc}} and \code{\link{summary}}, have an argument \code{count} which can take as many as eight different values which all point to just two unique behaviours. \code{checkCount} reduces this multitude to just the two meaningful values. It also throws an error if \code{count} is none of the eight possibilities.
#' @return [character] Either \code{"o"} or \code{"w"}.
#' @seealso \code{\link{checkCount}}
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
#' @details \code{paste0(..., collapse="")} takes a little more space than \code{collapse(...)}, though if the result is to be interspersed with something, the difference becomes merely one character: \code{paste0(..., collapse="_")} vs \code{collapse(..., inter="_")}.
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
#' @details Checks how many times a list is nested. The function is recursive, but to save on execution time it doesn't have an elegant wrapper around it and has the argument \code{d} which collects the result, and which the user is supposed not to tinker with.
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
# - list.transpose --------------------------------------------------------------------------------- <<< -

#' @title Transpose a nested list.
#' @description Taken from https://rdrr.io/cran/stackoverflow/src/R/tlist.R. I prefer to copy a short bit of code than to add a dependency.
#' @param x [list] The list to be transposed.
#' @details R's \code{\link{t}} function does not have a method for lists. This pretty little piece of code is effectively that. Say you have a list with three sublists, each containg a vector of ten elements. The three sublists are languages, and the ten elements are words in those languages. Functions from the \code{\link{apply}} family only let you loop through languages and with each iteration you have access to all the words from the given language, but only from that language. \code{list.transpose} turns such a list into a list of ten sublists, each containing a vector with three elements -- so you can loop through words, and have in each iteration access to word nr 1 from all three languages, then word nr 2, and so on.
#' @return [list] The transposed list.
#' @keywords internal
#' @importFrom stats setNames
#' @examples
#' soundcorrs:::list.transpose (list (1:3, 4:6, 7:9))

list.transpose <- function (x) {
	x <- do.call (rbind, x)
	lapply (setNames (seq(ncol(x)), colnames(x)), function(j) x[,j])
}

# -------------------------------------------------------------------------------------------------- >>> -
# - tabAbs2Rel ------------------------------------------------------------------------------------- <<< -

#' @title Convert a table from absolute to relative values.
#' @description Cross-tabulating sound correspondences with themselves may be misleading if the values are absolute, as it may well be that some segments simply do co-occur frequently, while others only rarely. The conversion is done in blocks: rows with the same prefix vs cols with the same prefix (if \code{column = NULL}) or vs all columns (if \code{column} is a string).
#' @param tab [table] The contingency table with sound correspondences.
#' @param column [character] Do columns contain the same data as rows (\code{NULL}) or some other data (a string)?
#' @details Functions that produce contingency tables, \code{\link{coocc}} and \code{\link{summary}}, return a result in absolute numbers, but sometimes the relative perspective may be more useful. This function converts from absolute to relative, but it does so in blocks where each block is an intersection of all the columns and the all rows whose names begin with the same prefix. 'Prefix' in this case is whatever comes before an underscore (\code{"_"}), because \code{\link{coocc}} and others use underscore to connect names of columns and rows.
#' @return [table] The converted table.
#' @keywords internal
#' @examples
#' dataset <- loadSampleDataset ("data-abc")
#' soundcorrs:::tabAbs2Rel (coocc(dataset,unit="o"), NULL)

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
#' @details Contingency tables, such as produced by \code{\link{coocc}} and \code{\link{summary}}, can get quite sizeable and therefore difficult to read with larger datasets. Since both their column and row names are composed from individual segments connected by an underscore (\code{"_"}), \code{\%hasPrefix\%} offers an easy way to select the interesting bit of the table by the first segment.
#' @return [logical] \code{TRUE} iff \code{x} begins with \code{prefix}.
#' @seealso \code{\link{\%hasSuffix\%}}
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
#' @details Contingency tables, such as produced by \code{\link{coocc}} and \code{\link{summary}}, can get quite sizeable and therefore difficult to read with larger datasets. Since both their column and row names are composed from individual segments connected by an underscore (\code{"_"}), \code{\%hasSuffix\%} offers an easy way to select the interesting bit of the table by the last segment.
#' @return [logical] \code{TRUE} iff \code{x} ends with \code{suffix}.
#' @seealso \code{\link{\%hasPrefix\%}}
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
#' @details Preparation of data for \code{\link{soundcorrs}} consists of segmentation and alignment. Segmentation can proceed on phoneme-by-phoneme, morpheme-by-morpheme, or any other basis; the only constraint is that each word in a pair/triple/... of words must contain the same number of segments. Segments are indicated by separators, by default the character \code{"|"}. The action of inserting separators, potentially between every two letters, in a large dataset, can become time consuming. \code{addSeparators} automates at least this part of the process.
#' @return [character vector] A vector of interspersed strings.
#' @export
#' @examples
#' addSeparators (c("word","mot","focal"), ".")

addSeparators <- function (x, separator="|")
	unlist (lapply (x, function(y)
		if (is.na(y))
			y
		else
			paste0 (strsplit(as.character(y),"")[[1]], collapse=separator)))

# -------------------------------------------------------------------------------------------------- >>> -
# - exp applyChanges ------------------------------------------------------------------------------- <<< -

#' @title Apply a series of sound changes to a vector of characters strings.
#' @description Apply a list of \code{\link{soundchange}}'s to a vector of charcter strings, possibly with additional metadata, and possibly compare the results to a prediction.
#' @param data [character] The strings to which to apply the changes.
#' @param changes [soundchange] The list of \code{\link{soundchange}}'s to apply.
#' @param target [character] The strings to which to compare the results. Defaults to \code{NULL}.
#' @param meta [list] Additional metadata to pass to \code{\link{soundchange}} functions. Must be the same length as \code{data}. Defaults to \code{NULL}.
#' @details Functions in \code{\link{soundchange}} objects are allowed to return more than one value, which makes manual application of a series of changes highly inconvenient and prone to errors. This function automates the process, while keeping track of all the intermediate forms. It returns the result in three formats: only the final shapes, their comparison to the shapes given under the \code{target} argument, and a tree with all the steps along the way. By default, only the final shapes are printed. The other two formats are accessible as simply elements of a named list.
#'
#'	Note that the application of sound changes does not require the data to be segmented and aligned. If sound changes are the only goal of the project, these two time-consuming steps can be safely omitted.
#' @return [list.applyChanges] A list with three fields: \code{$end}, a named list with the final results; \code{$match}, a named list with one of three values: \code{0} when none of the final results matches the \code{target}, \code{0.5} when at least one of the final results matches the \code{target}, or \code{1} when all the final results match the \code{target}; lastly \code{$tree}, a list tracing all the intermediate forms.
#' @seealso \code{\link{print.list.applyChanges}}, \code{\link{print.tree.applyChanges}}
#' @export
#' @examples
#' # prepare sample transcription
#' trans <- loadSampleDataset ("trans-common")
#' # define sound changes and data
#' i2a <- soundchange ("i > a", "lowering", trans)
#' a2u <- soundchange ("a > u", "backing", trans)
#' dataset <- c ("begin", "ring", "swim")
#' # apply the changes
#' applyChanges (dataset, list(i2a,a2u))
#' applyChanges (dataset, list(i2a,a2u))$tree

applyChanges <- function (data, changes, target=NULL, meta=NULL) {

# - applyChangesHlp - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -<<< -

	applyChangesHlp <- function (chngs, src, meta)

		# it's a recursive function
		if (length(chngs)==0) src else

			# src may contain multiple strings
			mapply (function (s, m) {

				# apply the change
				tmp <- chngs[[1]]$fun (s$start, m)

				# wrap in a list so the next iteration can handle it
				tmp <- lapply (tmp, function(x) list(start=x))

				# return the recursive result
				list(start	= s$start
					,change	= chngs[[1]]$name
					,end	= applyChangesHlp (chngs[-1], tmp, if (is.null(m)) list(m) else m)
				)

			}, src, meta, SIMPLIFY=F)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ->>> -

	# check args
	if (class(changes)!="list" | !identical (unique(unlist(lapply(changes,class))),"soundchange"))
		stop ("\"changes\" must be a list of \"soundchange\"'s.")
	if (!is.null(target) && length(data)!=length(target))
		stop ("\"data\" and \"meta\" must be of the same length.")
	if (!is.null(meta) && length(data)!=length(meta))
		stop ("\"data\" and \"meta\" must be of the same length.")

	# prep vars
	src <- lapply (data, function(x) list(start=x))
	if (is.null(meta)) meta<-rep(list(NULL),length(src))

	# apply the changes
	tree <- applyChangesHlp (changes, src, meta)
	class(tree) <- "tree.applyChanges"

	# extract the final results
	end <- lapply (tree, getEnds <- function (x)
		if (is.null(x$end)) x$start else sapply(x$end,getEnds))
	end <- lapply (end, as.vector)

	# compare the results to target, maybe
	match <- if (is.null(target)) NULL else
		mapply (function(e,t) if (all(e==t)) 1 else if (any(e==t)) 0.5 else 0, end, target, SIMPLIFY=F)

	# fix the names
	names(end) <- names(tree) <- data
	if (!is.null(match)) names(match)<-data

	# wrap the result in an object
	res <- list (
		end = end,
		match = match,
		tree = tree
	)
	class(res) <- "list.applyChanges"

	# and return it
	return (res)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp binTable ----------------------------------------------------------------------------------- <<< -

#' @title Sum all rows and all columns in a table, except for the selected ones.
#' @description Useful for when the data are scarce and \code{\link{chisq.test}} returns a warning, or when a more specific analysis of the data is required.
#' @param x [data.frame/matrix/table] Table to be binned.
#' @param row [integer/vector] The rows to not be binned.
#' @param col [integer/vector] The columns to not be binned.
#' @details When working with sparse data, the absolute values in a table are sometimes too low to allow for the use of various statistical tests, or the features too numerous for the result of a statistical test to be clearly interpretable. In such cases, a solution may be found in binning, i.e. in combining all the rows or columns into one, with the exception of select few. For example, a 10x10 table may be thus reduced to a 2x2 or a 2x3 one. The values are magnified while the number of features is reduced.
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
# - exp fitTable ----------------------------------------------------------------------------------- <<< -

#' @title Fit multiple models to multiple datasets.
#' @description Apply \code{\link{multiFit}} to all rows or all columns of a table.
#' @param models [list] A list of models to fit \code{data} to. Each element must be a list with at least two named fields: \code{formula} which contains the formula, and \code{start} which is a list of lists of starting estimates. Regarding the formula, the converter functions (\code{fun}, below) use "X" and "Y" for column names.
#' @param data [matrix/table] The data to fit \code{models} to.
#' @param margin [integer] As in \code{\link{apply}}: the subscripts which the fitting function (cf. \code{\link{multiFit}}) will be applied over. Accepted values are: \code{1} for rows, and \code{2} for columns.
#' @param conv [function] Function that converts vectors into data frames to which \code{models} will be fitted. Available functions are: \code{vec2df.id}, \code{vec2df.hist}, and \code{vec2df.rank}. Defaults to \code{vec2df.id}.
#' @param ... Additional arguments passed to \code{\link{multiFit}}).
#' @details Finding the right model and the right starting estimates for a model is often a time consuming process, very inconvenient to do manually. This function automates it as much as possible. It takes a list of models and starting estimates, as well as a list of datasets, and fits all the models to all the datasets. If any of the fits results in an error or a warning, the message is saved and can be inspected in the output, but it does not halt the process. \code{fitTable} is an extension of \code{\link{multiFit}} which fits multiple models to a single dataset.
#' @return [list.multiFit] A list of results returned by the fitting function (cf. \code{\link{multiFit}}).
#' @seealso \code{\link{multiFit}}
#' @export
#' @examples
#' dataset <- loadSampleDataset ("data-abc")
#' models <- list (
#'	"model A" = list (
#'		formula = "Y ~ a/X",
#'		start = list (list(a=1))),
#'	"model B" = list (
#'		formula = "Y ~ a/(1+exp(1)^X)",
#'		start = list (list(a=1)))
#' )
#' fitTable (models, summary(dataset), 1, vec2df.rank)

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
#' # prepare sample dataset
#' fTrans <- system.file ("extdata", "trans-common.tsv", package="soundcorrs")
#' fData <- system.file ("extdata", "data-capitals.tsv", package="soundcorrs")
#' tmp.ger <- read.soundcorrs (fData, "German", "ALIGNED.German", fTrans)
#' tmp.pol <- read.soundcorrs (fData, "Polish", "ALIGNED.Polish", fTrans)
#' dataset <- merge (tmp.ger, tmp.pol)
#' # prepare and run fitTable
#' models <- list (
#' 	"model A" = list (
#' 		formula = "Y ~ a/X",
#' 		start = list (list(a=1))),
#' 	"model B" = list (
#' 		formula = "Y ~ a/(1+exp(1)^X)",
#' 		start = list (list(a=1)))
#' )
#' fitTable (models, summary(dataset), 1, vec2df.id)

vec2df.id <- function (data) {

	# return the result
	return (data.frame (X=seq_along(data), Y=data))

}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ->>> -
# - vec2df.hist - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -<<< -

#' A vector to data frame converter for \code{\link{fitTable}}. This one makes a histogram, and returns a data frame with midpoints and counts.
#' @param data [numeric vector] The data to be converted.
#' @return [data.frame] Converted \code{data}.
#' @export
#' @importFrom graphics hist
#' @examples
#' # prepare sample dataset
#' fTrans <- system.file ("extdata", "trans-common.tsv", package="soundcorrs")
#' fData <- system.file ("extdata", "data-capitals.tsv", package="soundcorrs")
#' tmp.ger <- read.soundcorrs (fData, "German", "ALIGNED.German", fTrans)
#' tmp.pol <- read.soundcorrs (fData, "Polish", "ALIGNED.Polish", fTrans)
#' dataset <- merge (tmp.ger, tmp.pol)
#' # prepare and run fitTable
#' models <- list (
#' 	"model A" = list (
#' 		formula = "Y ~ a/X",
#' 		start = list (list(a=1))),
#' 	"model B" = list (
#' 		formula = "Y ~ a/(1+exp(1)^X)",
#' 		start = list (list(a=1)))
#' )
#' fitTable (models, summary(dataset), 1, vec2df.hist)

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
#' # prepare sample dataset
#' fTrans <- system.file ("extdata", "trans-common.tsv", package="soundcorrs")
#' fData <- system.file ("extdata", "data-capitals.tsv", package="soundcorrs")
#' tmp.ger <- read.soundcorrs (fData, "German", "ALIGNED.German", fTrans)
#' tmp.pol <- read.soundcorrs (fData, "Polish", "ALIGNED.Polish", fTrans)
#' dataset <- merge (tmp.ger, tmp.pol)
#' # prepare and run fitTable
#' models <- list (
#' 	"model A" = list (
#' 		formula = "Y ~ a/X",
#' 		start = list (list(a=1))),
#' 	"model B" = list (
#' 		formula = "Y ~ a/(1+exp(1)^X)",
#' 		start = list (list(a=1)))
#' )
#' fitTable (models, summary(dataset), 1, vec2df.rank)

vec2df.rank <- function (data) {

	# return the result
	return (data.frame (X=seq_along(data), Y=sort(data,decreasing=T)))

}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ->>> -

# -------------------------------------------------------------------------------------------------- >>> -
# - exp lapplyTest --------------------------------------------------------------------------------- <<< -

#' @title Apply a function to a list.
#' @description Takes a list and applies to each of its elements a function, returning a list of outputs. Primary intended for tests of independence on a list of contingency tables.
#' @param x [list] The list to which to apply \code{fun}.
#' @param fun [function] The function which to apply to \code{data}. Must return an object containing an element named \code{p.value}. Defaults to \code{\link{chisq.test}}.
#' @param ... Additional arguments passed to \code{fun}.
#' @details When applying a function to a list, any iteration that results in an error, breaks the whole loop. This is not always the most convenient behaviour, in particular when the function is a statistical test and the error is to do with sparse data in one of the tables in the list. \code{lapplyTest} is a wrapper around \code{base::lapply} which only differs from the original in its treatment of errors. It saves the message associated with the error or warning, but then continues to the next iteration rather than quitting the loop altogether.
#' @return [list.lapplyTest] A list of outputs of \code{fun}.
#' @seealso \code{\link{summary.list.lapplyTest}}, \code{\link{allCooccs}}
#' @export
#' @examples
#' dataset <- loadSampleDataset ("data-abc")
#' lapplyTest (allCooccs(dataset))
#' lapplyTest (allCooccs(dataset), fisher.test, simulate.p.value=TRUE)

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
# - exp loadSampleDataset -------------------------------------------------------------------------- <<< -

#' @title Load one of \code{\link{soundcorrs}}' sample datasets.
#' @description Retrieve and return one the sample datasets included in \code{\link{soundcorrs}}.
#' @param x [character] Name of the dataset to load. Available sets are: \code{\link{soundchange}}'s: \code{change-dl2l}, \code{change-palatalization}, \code{change-rhotacism}; \code{\link{soundcorrs}}'s: \code{data-abc}, \code{data-capitals}, \code{data-ie}; and \code{\link{transcription}}'s: \code{trans-common}, \code{trans-ipa}.
#' @details R does not allow non-ASCII characters in preloaded datasets, and linguistic datasets can hardly fit within ASCII. Unicode is, however, allowed in raw data files. They cannot be automatically loaded when \code{\link{soundcorrs}} is attached because staged install makes it impossible to use \code{\link{system.file}} in this manner, and they cannot be included as a Unicode-escaped output of \code{\link{dput}} because Windows does not know how to convert this to its native encoding. This function makes the process of loading as painless as possible.
#' @return [soundchange/soundcorrs/transcription] The selected sample dataset.
#' @export
#' @examples
#' loadSampleDataset ("data-abc")
#' loadSampleDataset ("trans-ipa")
#' loadSampleDataset ("change-palatalization")

loadSampleDataset <- function (x) {

	# read the requested dataset
	res <- switch (x,
		"change-dl2l" = {
			trans <- read.transcription (system.file ("extdata", "trans-common.tsv", package="soundcorrs"))
			eval (parse (system.file("extdata","change-dl2l.R",package="soundcorrs")))
		},
		"change-palatalization" = {
			trans <- read.transcription (system.file ("extdata", "trans-common.tsv", package="soundcorrs"))
			eval (parse (system.file("extdata","change-palatalization.R",package="soundcorrs")))
		},
		"change-rhotacism" = {
			trans <- read.transcription (system.file ("extdata", "trans-common.tsv", package="soundcorrs"))
			eval (parse (system.file("extdata","change-rhotacism.R",package="soundcorrs")))
		},
		"data-abc" = {
			trans <- read.transcription (system.file ("extdata", "trans-common.tsv", package="soundcorrs"))
			path.d <- system.file ("extdata", "data-abc.tsv", package="soundcorrs")
			tmp <- long2wide (read.table(path.d,header=T), skip=c("ID"))
			d.l1 <- soundcorrs (tmp, "L1", "ALIGNED.L1", trans)
			d.l2 <- soundcorrs (tmp, "L2", "ALIGNED.L2", trans)
			merge (d.l1, d.l2)
		},
		"data-capitals" = {
			path.t <- system.file ("extdata", "trans-common.tsv", package="soundcorrs")
			path.d <- system.file ("extdata", "data-capitals.tsv", package="soundcorrs")
			d.ger <- read.soundcorrs (path.d, "German", "ALIGNED.German", path.t)
			d.pol <- read.soundcorrs (path.d, "Polish", "ALIGNED.Polish", path.t)
			d.spa <- read.soundcorrs (path.d, "Spanish", "ALIGNED.Spanish", path.t)
			merge (d.ger, d.pol, d.spa)
		},
		"data-ie" = {
			path.tc <- system.file ("extdata", "trans-common.tsv", package="soundcorrs")
			path.ti <- system.file ("extdata", "trans-ipa.tsv", package="soundcorrs")
			path.d <- system.file ("extdata", "data-ie.tsv", package="soundcorrs")
			d.lat <- read.soundcorrs (path.d, "Latin", "LATIN", path.tc)
			d.eng <- read.soundcorrs (path.d, "English", "ENGLISH", path.ti)
			merge (d.lat, d.eng)
		},
		"trans-common" = {
			path.t <- system.file ("extdata", "trans-common.tsv", package="soundcorrs")
			read.transcription (path.t)
		},
		"trans-ipa" = {
			path.t <- system.file ("extdata", "trans-ipa.tsv", package="soundcorrs")
			read.transcription (path.t)
		})

	# and return it
	return (res)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp long2wide ---------------------------------------------------------------------------------- <<< -

#' @title Convert from the long format (single entry per row) to the wide format (multiple entries per row).
#' @description Takes a data frame of word pairs/triples/..., each stored in multiple rows, and returns a data frame with the same words but each pair/triple/... stored in one row. WARNING: in the original data frame, entries from all languages must be in the same order.
#' @param data [data.frame] The dataset to be converted.
#' @param col.lang [character] Name of the column with language names. Defaults to \code{"LANGUAGE"}.
#' @param skip [character vector] Names of columns to not convert. Defaults to \code{NULL}.
#' @details Data for \code{\link{soundcorrs}} can be prepared in one of two formats: the 'long format' and the 'wide format'. In the 'long format', each row contains only a single word and metadata associated with it. In the 'wide format', each row contains the entire pair/triple/... of words, and all the metadata associated with them. The 'long format' is convenient for making sure that all the words in a pair/triple/... have the same number of segments, but it cannot be read directly by \code{\link{soundcorrs}}. \code{long2wide} and \code{\link{wide2long}} convert between the two formats.
#' @return [data.frame] A data frame in the wide format (multiple entries per row).
#' @seealso \code{\link{wide2long}}
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
		stop ("One or more column names are missing from \"data\".")

	# split the data
	res <- split (data, data[,col.lang])

	# make sure all have the same number of entries
	if (length(unique(unlist(lapply(res,nrow)))) != 1)
		stop ("Differing number of entries for different languages.")

	# separate columns to be skipped, and add suffixes to column names
	skips <- list ()
	for (i in seq_along(res)) {
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
#' @details Finding the right model and the right starting estimates for a model is often a time consuming process, very inconvenient to do manually. This function automates it as much as possible. It takes a list of models and starting estimates, and fits them to data not stopping whenever an error occurs or a warning is issued. Error and warning messages are saved and can be inspected in the output, they just do not halt the process. \code{multiFit} has an extension in the form of \code{\link{fitTable}} which applies multiple models to multiple datasets.
#' @return [list.multiFit] A list of results returned by \code{fun} or, if it ended with an error, \code{NULL}.
#' @seealso \code{\link{fitTable}}, \code{\link{summary.list.multiFit}}
#' @export
#' @importFrom stats nls
#' @examples
#' set.seed (27)
#' dataset <- data.frame (X=1:10, Y=(1:10)^2+runif(10,-10,10))
#' models <- list (
#'	"model A" = list (
#'		formula = "Y ~ X^a",
#'		start = list (list(a=100), list(a=1))),
#'	"model B" = list (
#'		formula = "Y ~ a*(X+b)",
#'		start = list (list(a=1,b=1)))
#'	)
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
# - exp ngrams ------------------------------------------------------------------------------------- <<< -

#' @title N-grams and their frequencies.
#' @description Find n-grams of specified length and return them as a list, or their counts as a table.
#' @param x [character vector] Words to be cut into n-grams.
#' @param n [integer] The length of n-grams to look for. Defaults to \code{1}.
#' @param borders [character] Characters to prepend and append to every word. Must be a vector of exactly two character strings. Defaults to \code{c("","")}.
#' @param rm [character] Characters to be removed from \code{x} before cutting into n-grams. May be a regular expression, f.ex. "[-\\|]" will capture the default symbol for linguistics zeros as well as the default segment separators. Empty string denotes nothing to replace. Defaults to empty string.
#' @param as.table [logical] Return the result as a table? Defaults to \code{TRUE}.
#' @details Data processed with \code{\link{soundcorrs}} are generally expected to be segmented and aligned, and both segmentation and alignment are recommended to be performed manually. This is a laborious process, but it is feasible when segments represent morphemes or phonemes. Should segments represent n-grams, however, the fully manual approach would have been very time consuming and prone to errors.
#' @return [table] Table with counts of n-grams.
#' @export
#' @examples
#' dataset <- loadSampleDataset ("data-capitals")
#' ngrams(dataset$data[,"ALIGNED.German"], n=2)
#' ngrams(dataset$data[,"ALIGNED.German"], n=3, as.table=FALSE)
#' ngrams(dataset$data[,"ALIGNED.German"], n=4, rm="[-\\|]", as.table=FALSE)
#' ngrams(dataset$data[,"ALIGNED.German"], n=5, borders=c(">","<"), rm="[-\\|]", as.table=FALSE)

#' @export
ngrams <- function (x, n=1, borders=c("",""), rm="", as.table=T) {

	# check args
	if (n%%1!=0 || n<1)
		stop ("\"n\" must be a positive integer.")
	if (class(borders)!="character" | length(borders)!=2)
		stop ("\"borders\" must be a vector of two character strings.")
	if (class(rm)!="character" | length(rm)>1)
		stop ("\"rm\" must be a single character string.")
	if (class(as.table) != "logical")
		stop ("\"as.table\" must be either TRUE or FALSE.")

	# remember where NAs are
	nas <- which (is.na(x))

	# maybe add something
	tmp <- lapply (x, function(y) paste0(borders[1],y,borders[2]))

	# maybe remove something
	tmp <- gsub (rm, "", tmp)

	# extract the ngrams
	res <- lapply (tmp, function (y)
		if (n >= nchar(y)) y else
			mapply (substr, 1:(nchar(y)-n+1), n:nchar(y), MoreArgs=list(x=y))
	)

	# put back the NAs
	res[nas] <- NA

	# prepare the result
	if (as.table) res<-table(unlist(res))

	# return the result
	return (res)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp print.list.applyChanges -------------------------------------------------------------------- <<< -

#' @title Pretty printing for the result of \code{\link{applyChanges}}.
#' @param x [list.applyChanges] The output of \code{\link{applyChanges}}.
#' @param ... Unused; only for consistency with \code{\link{print}}.
#' @details The output of \code{\link{applyChanges}} is a list, potentially a very long one, and difficult to read. To make it easier to digest, this function only prints the \code{$end} element, i.e. the final shapes produced by the application of all of the sound changes.
#' @return [list.applyChanges] The same object that was given as \code{x}.
#' @seealso \code{\link{applyChanges}}, \code{\link{print.tree.applyChanges}}
#' @export
#' @examples
#' # prepare sample transcription
#' trans <- loadSampleDataset ("trans-common")
#' # define sound changes
#' a2b <- soundchange ("a > b", "change 1", trans)
#' b2c <- soundchange ("b > c", "change 2", trans)
#' # and apply them
#' applyChanges (c("a","b","c"), list(a2b,b2c))

print.list.applyChanges <- function (x, ...) {

	# do the printing
	print (x$end)

	# return the object, invisibly
	invisible (x)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp print.tree.applyChanges -------------------------------------------------------------------- <<< -

#' @title Pretty printing for part of the result of \code{\link{applyChanges}}.
#' @param x [tree.applyChanges] The \code{tree} element in the output of \code{\link{applyChanges}}.
#' @param ... Unused; only for consistency with \code{\link{print}}.
#' @details One of the elements in the output of \code{\link{applyChanges}} is a tree. It is represented as a nested list, potentially a very deeply nested and a very long list which would have been all but impossible to digest for a human. This function prints it as a structure that more resembles a tree, very similar to the output of \code{\link{str}}.
#' @return [tree.applyChanges] The same object that was given as \code{x}.
#' @seealso \code{\link{applyChanges}}, \code{\link{print.list.applyChanges}}
#' @export
#' @examples
#' # prepare sample transcription
#' trans <- loadSampleDataset ("trans-common")
#' # define sound changes
#' ab <- soundchange (function(x,meta) c("a","b"), "change 1", trans)
#' ab2c <- soundchange ("[ab] > c", "change 2", trans)
#' # and apply them
#' applyChanges (c("a","b","c"), list(ab,ab2c))$tree

print.tree.applyChanges <- function (x, ...) {

# - hlp - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -<<< -

	hlp <- function (y, lvl)

		# it's a recursive function
		if (length(y) > 0)

			# print all the branches
			lapply (y, function (z) {

				# do the printing
				cat (paste0 (lvl, " "))
				cat (collapse(rep(".. ",lvl-1), inter=""))
				cat (z$start)
				if (!is.null(z$change)) cat(paste0(" [", z$change, "]"))
				cat ("\n")

				# continue onto the branches
				hlp (z$end, lvl+1)

			})

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ->>> -

	# do the printing
	hlp (x, 1)
	cat ("\n")

	# return the object, invisibly
	invisible (x)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - exp summary.list.lapplyTest -------------------------------------------------------------------- <<< -

#' @title A quick summary of the result of \code{\link{lapplyTest}}.
#' @description Take the output of \code{\link{lapplyTest}}, and extract from it only the noteworthy results.
#' @param object [list.lapplyTest] The output of \code{\link{lapplyTest}}.
#' @param p.value [double] Results above this value will not be reported. Defaults to 0.05.
#' @param ... Unused; only for consistency with \code{\link{summary}}.
#' @details The output of \code{\link{lapplyTest}} may be difficult to digest for a human. This function selects from it only the results that are of particular interest, and presents them in an easy to read form.
#' @return A more human-friendly digest.
#' @seealso \code{\link{lapplyTest}}
#' @export
#' @examples
#' dataset <- loadSampleDataset ("data-abc")
#' lapplyTest (allCooccs(dataset))

summary.list.lapplyTest <- function (object, p.value=0.05, ...) {

	# only show results below the selected p.value
	res <- Filter (function(x) x$p.value<=p.value, object)

	# print the summary
	cat (paste0 ("Total results: ", length(object), "; with p-value \u2264 ", p.value,": ", length(res), ".\n"))

	# and the lucky results
	if (length(res) > 0) {
		for (i in seq_along(res)) {
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
#' @details The output of \code{\link{multiFit}} may be difficult to digest for a human. This function selects from it only the results that are of particular interest, and presents them in an easy to read form.
#' @return A more human-friendly digest.
#' @seealso \code{\link{multiFit}}
#' @export
#' @importFrom stats AIC BIC chisq.test resid
#' @examples
#' set.seed (27)
#' dataset <- data.frame (X=1:10, Y=(1:10)^2+runif(10,-10,10))
#' models <- list (
#'	"model A" = list (
#'		formula = "Y ~ X^a",
#'		start = list (list(a=100), list(a=1))),
#'	"model B" = list (
#'		formula = "Y ~ a*(X+b)",
#'		start = list (list(a=1,b=1)))
#'	)
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
#' @details Data for \code{\link{soundcorrs}} can be prepared in one of two formats: the 'long format' and the 'wide format'. In the 'long format', each row contains only a single word and metadata associated with it. In the 'wide format', each row contains the entire pair/triple/... of words, and all the metadata associated with them. The 'long format' is convenient for making sure that all the words in a pair/triple/... have the same number of segments, but it cannot be read directly by \code{\link{soundcorrs}}. \code{\link{long2wide}} and \code{wide2long} convert between the two formats.
#' @return [data.frame] A data frame in the long format (single entry per row).
#' @seealso \code{\link{long2wide}}
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
	nams <- lapply (seq_along(cols), function (i)
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
	for (i in seq_along(cols)) {
		tmp <- data [, c(cols[[i]], sless)]
		tmp <- cbind (tmp, suffixes[i])
		colnames(tmp) <- cn
		res <- rbind (res, tmp)
	}

	# return the result
	return (res)

}

# -------------------------------------------------------------------------------------------------- >>> -
