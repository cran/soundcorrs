## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ------------------------------------------------------------------------
# install.packages ("soundcorrs")
library (soundcorrs)

## ------------------------------------------------------------------------
# establish the paths of the samples included in ‘soundcorrs’
path.trans.com <- system.file ("extdata", "trans-common.tsv", package="soundcorrs")
path.trans.ipa <- system.file ("extdata", "trans-ipa.tsv", package="soundcorrs")

# and load them
trans.com <- read.transcription (path.trans.com)
trans.ipa <- read.transcription (path.trans.ipa)

# transcription needs to be an object of class ‘transcription’
class (trans.com)

# a basic summary
trans.com

# ‘data’ is the original data frame
# ‘cols’ is a guide to column names in ‘data’
# ‘meta’ is a vector of characters which act as metacharacters
# ‘values’ is a named list of the values of graphemes, exploded into vectors
# ‘zero’ are the characters denoting the linguistic zero
str (trans.com, max.level=1)

## ------------------------------------------------------------------------
# establish the paths of the two datasets
path.abc <- system.file ("extdata", "data-abc.tsv", package="soundcorrs")
path.cap <- system.file ("extdata", "data-capitals.tsv", package="soundcorrs")
path.ie <- system.file ("extdata", "data-ie.tsv", package="soundcorrs")

# read “capitals”
d.cap.ger <- read.soundcorrs (path.cap, "German", "ALIGNED.German", path.trans.com)
d.cap.pol <- read.soundcorrs (path.cap, "Polish", "ALIGNED.Polish", path.trans.com)
d.cap.spa <- read.soundcorrs (path.cap, "Spanish", "ALIGNED.Spanish", path.trans.com)
d.cap <- merge (d.cap.ger, d.cap.pol, d.cap.spa)

# read “ie”
d.ie.lat <- read.soundcorrs (path.ie, "Lat", "LATIN", path.trans.com)
d.ie.eng <- read.soundcorrs (path.ie, "Eng", "ENGLISH", path.trans.ipa)
d.ie <- merge (d.ie.lat, d.ie.eng)

# read “abc”
tmp <- long2wide (read.table(path.abc,header=T), skip=c("ID"))
d.abc.l1 <- soundcorrs (tmp, "L1", "ALIGNED.L1", trans.com)
d.abc.l2 <- soundcorrs (tmp, "L2", "ALIGNED.L2", trans.com)
d.abc <- merge (d.abc.l1, d.abc.l2)

# some basic summary
d.abc.l1
d.abc

# ‘cols’ are the names of important columns
# ‘data’ is the original data frame
# ‘names’ are the names of the languages
# ‘segms’ are words exploded into segments; ‘$z’ is a variant with linguistic zeros; ‘$nz’ without them
# ‘segpos’ is a lookup list to check which character belongs to which segment; ‘$z’ is a variant with linguistic zeros; ‘$nz’ without them
# ‘separators’ are the strings used as segment separators
# ‘trans’ are ‘transcription’ objects
# ‘words’ are words obtained by removing separators from the ‘col.aligned’ column; ‘$z’ is a variant with linguistic zeros; ‘$nz’ without them
str (d.abc, max.level=1)

## ------------------------------------------------------------------------
# a simple sound change
sc.V2a <- soundchange ("V > a", "V>a", trans.com, "All vowels change into a.")

# basic summary
sc.V2a

# ‘name’ is the name of the sound change
# ‘desc’ is a brief description
# ‘fun’ is the sound change function
# ‘trans’ is the transcription used in the change function
str (sc.V2a, max.level=1)

# if need be, functions inside ‘soundchange’ objects can be applied directly
sc.V2a$fun ("ouroboros", NULL)

# a slightly more complex change
sc.VV2a <- soundchange ("V{2,} > a", "VV>a", trans.com, "Only diphthongs change into a.")
sc.VV2a$fun ("ouroboros", NULL)

# a slightly more complex change
sc.CV2Ca <- soundchange ("(C)V > \\1a", "CV>Ca", trans.com, "Only postconsonantal vowels change into a.")
sc.CV2Ca$fun ("ouroboros", NULL)

# a more complex sound change
sc.2ndV2a.fun <- function (x, meta) {
    tmp <- gregexpr (expandMeta(trans.com,"V+"), x)
    regmatches(x,tmp)[[1]][2] <- "a"
    return (x)
}
sc.2ndV2a <- soundchange (sc.2ndV2a.fun, "2ndV>a", trans.com,
    "Only the vowel in the second syllable changes into a.")
sc.2ndV2a$fun ("ouroboros", NULL)

## ------------------------------------------------------------------------
# a general overview of the dataset as a whole
summary (d.abc)

# words are the default ‘unit’
summary (d.abc, unit="o")

# in relative values …
rels <- summary (d.abc, count="r")
round (rels, 2)

# … relative to entire rows
apply (rels, 1, sum)

## ------------------------------------------------------------------------
# a general look in the internal mode
coocc (d.abc)

# now with metadata
coocc (d.abc, "DIALECT.L2")

# in the internal mode,
#    the relative values are with regard to segment-to-segment blocks
tab <- coocc (d.abc, count="r")
rows.a <- which (rownames(tab) %hasPrefix% "a")
cols.b <- which (colnames(tab) %hasPrefix% "b")
sum (tab [rows.a, cols.b])

# there are four different segments in L1
sum (tab)

# if two correspondences never co-occur, the relative value is 0/0
#    which R represents as ‘NaN’, and prints as empty space
coocc (d.abc, count="r")

# in the external mode,
#    the relative values are with regard to blocks of rows, and all columns
tab <- coocc (d.abc, "DIALECT.L2", count="r")
rows.a <- which (rownames(tab) %hasPrefix% "a")
sum (tab [rows.a, ])

## ------------------------------------------------------------------------
# for a small dataset, the result is going to be small
str (allCooccs(d.abc), max.level=0)

# but it can grow quite quickly with a larger dataset
str (allCooccs(d.cap), max.level=0)

# the naming scheme
names (allCooccs(d.abc))

# and with ‘column’ not ‘NULL’
names (allCooccs(d.abc,column="DIALECT.L2"))

## ------------------------------------------------------------------------

# “ab” spans segments 1–2, while “a” only occupies segment 1
findExamples (d.abc, "ab", "a", distance.end=0)
findExamples (d.abc, "ab", "a", distance.end=1)

# linguistic zeros cannot be found if ‘zeros’ is set to ‘FALSE’
findExamples (d.abc, "-", "", zeros=T)
findExamples (d.abc, "-", "", zeros=F)

# both the usual and custom regular expressions are permissible
findExamples (d.abc, "a", "[ou]")
findExamples (d.abc, "a", "O")

# the output is actuall a list
str (findExamples(d.abc,"a","a"), max.level=1)

# ‘data’ is what is displayed on the screen
# ‘which’ is useful for subsetting
subset (d.abc, findExamples(d.abc,"a","O")$which)

# ‘which’ can also be used to find examples
#    that exhibit more than one correspondence.
aaa <- findExamples (d.cap, "a", "a", "a", distance.start=0, distance.end=0)$which
bbb <- findExamples (d.cap, "b", "b", "b", distance.start=0, distance.end=0)$which
d.cap$data [aaa & bbb,]


# the ‘cols’ argument can be used to alter the printed output
findExamples (d.abc, "a", "O", cols=c("ORTHOGRAPHY.L1","ORTHOGRAPHY.L2"))

## ------------------------------------------------------------------------
# and see what result this gives
allPairs (d.abc, cols=c("ORTHOGRAPHY.L1","ORTHOGRAPHY.L2"))

# a clearer result could be obtained by running
# allPairs (d.cap, cols=c("ORTHOGRAPHY.German","ORTHOGRAPHY.Polish"),
#    file="~/Desktop/d.cap.html", formatter=formatter.html)

## ------------------------------------------------------------------------
# load the new formatter function …
# source ("~/Desktop/myFormatter.R")

# … and use it instead of ‘formatter.html()’
# allPairs (d.cap, cols=c("ORTHOGRAPHY.German","ORTHOGRAPHY.Polish"),
#    file="~/Desktop/d.cap.html", formatter=myFormatter)
# note that this time the output will not open in the web browser automatically 

## ------------------------------------------------------------------------
# prepare some random data
set.seed (27)
dataset <- data.frame (X=1:10, Y=1:10 + runif(10,-1,1))

# prepare models to be tested
models <- list (
    "model A" = list( formula="Y~a+X", start=list(list(a=1)) ),
    "model B" = list( formula="Y~a^X", start=list(list(a=-1),list(a=1)) ))
# normally, (-1)^X would produce an error with ‘nls()’

# fit the models to the dataset
fit <- multiFit (models, dataset)

# inspect the results
summary (fit)

## ------------------------------------------------------------------------
# prepare the data
dataset <- coocc (d.abc)

# prepare the models to be tested
models <- list (
	"model A" = list( formula="Y~a*(X+b)^2", start=list(list(a=1,b=1)) ),
	"model B" = list( formula="Y~a*(X-b)^2", start=list(list(a=1,b=1)) ))
# vanilla nls() often requires fairly accurate starting estimates

# fit the models to the dataset
fit <- fitTable (models, dataset, 1, vec2df.hist)

# inspect the results
summary (fit, metric="sigma")

## ------------------------------------------------------------------------
# prepare a list of changes, in the order of application
sc.list <- list (sc.VV2a, sc.2ndV2a, sc.CV2Ca)

# prepare the data and the expected results
data <- c ("ouroboros", "jormungandr")
target <- c ("arabaras", "jarmangandr")

# and apply the changes to our data
res <- applyChanges (data, sc.list, target, meta=NULL)
res

# see if they match the expectations
res$match

# see which change did not work as expected
#    it was CV > Ca because our changes use the sample "common" transcription,
#    and j does not count in it as a consonant (it's a semivowel)
res$tree

## ------------------------------------------------------------------------
# using the default ‘|’ …
addSeparators (d.abc$data$ORTHOGRAPHY.L1)

# … or a full stop
addSeparators (d.abc$data$ORTHOGRAPHY.L1, ".")

## ------------------------------------------------------------------------
# build a table for a slightly larger dataset
tab <- coocc (d.cap)

# let us focus on L1 a and o
rows <- which (rownames(tab) %hasPrefix% "a")
cols <- which (colnames(tab) %hasPrefix% "o")
binTable (tab, rows, cols)

# or on all a-like and o-like vowels
rows <- which (rownames(tab) %hasPrefix% "[aāäǟ]")
cols <- which (colnames(tab) %hasPrefix% "[oōöȫ]")
binTable (tab, rows, cols)

## ------------------------------------------------------------------------
# let us search a column other than the one specified as ‘aligned’
orth <- d.abc$data [, "ORTHOGRAPHY.L2"]

# look for all VCC sequences
query <- expandMeta (d.cap$trans[[1]], "VCC")
orth [grep(query,orth)]

# look for all VCC words
query <- expandMeta (d.cap$trans[[1]], "^VCC$")
orth [grep(query,orth)]

# the same in the binary notation
query <- expandMeta (d.cap$trans[[1]], "^[+vow][+cons][+cons]$")
orth [grep(query,orth)]

## ------------------------------------------------------------------------
# build a table for a slightly larger dataset
tab <- coocc (d.cap)

# it is quite difficult to read as a whole, so let us focus
#    on a-like vowels in L1 and s-like consonants in L2
rows <- which (rownames(tab) %hasPrefix% "[aāäǟ]")
cols <- which (colnames(tab) %hasPrefix% "[sśš]")
tab [rows, cols]

## ------------------------------------------------------------------------
# build a table for a slightly larger dataset
tab <- coocc (d.cap)

# it is quite difficult to read as a whole, so let us focus
#    on what corresponds to a-like vowels in L1 and s-like consonants in L2
rows <- which (rownames(tab) %hasSuffix% "[aāäǟ]")
cols <- which (colnames(tab) %hasSuffix% "[sśš]")
tab [rows, cols]

## ------------------------------------------------------------------------
# let us prepare the tables
tabs <- allCooccs (d.abc, bin=F)

# and apply the chi-squared test to them
chisq <- lapplyTest (tabs)
chisq

# this is only an example on a tiny dataset, so let us be more forgiving
summary (chisq, p.value=0.3)

# let us see the problems with ‘a’
attr (chisq$a, "error")
attr (chisq$a, "warning")

# this warning often means that the data were insufficient
tabs$a

## ------------------------------------------------------------------------
# load a transcription
tmp <- loadSampleDataset ("trans-common")

# it's the same that we've already loaded above
identical (tmp, trans.com)

## ------------------------------------------------------------------------
# the “abc” dataset is in the long format
abc.long <- read.table (path.abc, header=T)

# the simplest conversion unnecessarily doubles the ID column
long2wide (abc.long)

# but this can be avoided with the ‘skip’ argument
abc.wide <- long2wide (abc.long, skip="ID")

## ------------------------------------------------------------------------
# with n==1, ngrams() returns simply the frequencies of segments
ngrams (d.cap$data[,"ORTHOGRAPHY.Spanish"])

# counts can easily be turned into a data frame with ranks
tab <- ngrams (d.cap$data[,"ORTHOGRAPHY.Spanish"], n=2)
mtx <- as.matrix (sort(tab,decreasing=T))
head (data.frame (RANK=1:length(mtx), COUNT=mtx, FREQ=mtx/sum(mtx)))

## ------------------------------------------------------------------------
# select only examples from L2’s northern dialect
subset (d.abc, DIALECT.L2=="north") $data

# select only capitals of countries where German is an official language
subset (d.cap, grepl("German",d.cap$data$OFFICIAL.LANGUAGE)) $data

# select only pairs in which L1 a : L2 a
subset (d.abc, findPairs(d.abc,"a","a")$which) $data

## ------------------------------------------------------------------------
# let us use the converted “abc” dataset
abc.wide

# with the separator preserved
wide2long (abc.wide, c(".L1",".L2"))

# and with the separator removed
wide2long (abc.wide, c(".L1",".L2"), strip=1)

