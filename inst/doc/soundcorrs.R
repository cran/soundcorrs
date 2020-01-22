## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
# install.packages ("soundcorrs")
library ("soundcorrs")

## -----------------------------------------------------------------------------
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
# ‘zero’ are the characters denoting the linguistic zero
str (trans.com, max.level=1)

## -----------------------------------------------------------------------------
# establish the paths of the two datasets
path.abc <- system.file ("extdata", "data-abc.tsv", package="soundcorrs")
path.cap <- system.file ("extdata", "data-capitals.tsv", package="soundcorrs")
path.ie <- system.file ("extdata", "data-ie.tsv", package="soundcorrs")

# read “capitals”
d.cap.ger <- read.scOne (path.cap, "German", "ALIGNED.German", path.trans.com)
d.cap.pol <- read.scOne (path.cap, "Polish", "ALIGNED.Polish", path.trans.com)
d.cap <- soundcorrs (d.cap.ger, d.cap.pol)

# read “ie”
d.ie.lat <- read.scOne (path.ie, "Lat", "LATIN", path.trans.com)
d.ie.eng <- read.scOne (path.ie, "Eng", "ENGLISH", path.trans.ipa)
d.ie <- soundcorrs (d.ie.lat, d.ie.eng)

# read “abc”
tmp <- long2wide (read.table(path.abc,header=T), skip=c("ID"))
d.abc.l1 <- scOne (tmp, "L1", "ALIGNED.L1", trans.com)
d.abc.l2 <- scOne (tmp, "L2", "ALIGNED.L2", trans.com)
d.abc <- soundcorrs (d.abc.l1, d.abc.l2)

# individual languages are objects of class ‘scOne’
class (d.abc.l1)

# some basic summary
d.abc.l1

# ‘cols’ are names of the important columns
# ‘data’ is the original data frame
# ‘name’ is the name of the language
# ‘segms’ are words exploded into segments; ‘$z’ is a variant with linguistic zeros; ‘$nz’ without them
# ‘segpos’ is a lookup list to check which character belongs to which segment; ‘$z’ is a variant with linguistic zeros; ‘$nz’ without them
# ‘separator’ is the string used as segment separator
# ‘trans’ is a ‘transcription’ object
# ‘words’ are words obtained by removing separators from the ‘col.aligned’ column; ‘$z’ is a variant with linguistic zeros; ‘$nz’ without them
str (d.abc.l1, max.level=1)

# datasets are objects of class ‘soundcorrs’
class (d.abc)

# some basic summary
d.abc

# ‘data’ is the original data frame
# ‘cols’ are the same as with ‘scOne’ above, wrapped in a list
# ‘names’ are the names of the languages,
# ‘segms’ are the same as with ‘scOne’ above, wrapped in a list
# ‘segpos’ are likewise
# ‘separators’ are likewise, only a vector instead of a list
# ‘trans’ are the individual transcriptions wrapped in a list
# ‘words’ are the same as with ‘scOne’ above, wrapped in a list
str (d.abc, max.level=1)

## -----------------------------------------------------------------------------
# a general overview of the dataset as a whole
summary (d.abc)

# words are the default ‘unit’
summary (d.abc, unit="o")

# in relative values …
rels <- summary (d.abc, count="r")
round (rels, 2)

# … relative to entire rows
apply (rels, 1, sum)

## -----------------------------------------------------------------------------
# a general look in the internal mode
table (d.abc)

# … and in the other direction
table (d.abc, direction=2)

# now with metadata
table (d.abc, "DIALECT.L2")

# in the internal mode,
#    the relative values are with regard to segment-to-segment blocks
tab <- table (d.abc, count="r")
rows.a <- which (rownames(tab) %hasPrefix% "a")
cols.b <- which (colnames(tab) %hasPrefix% "b")
sum (tab [rows.a, cols.b])

# there are four different segments in L1
sum (tab)

# if two correspondences never co-occur, the relative value is 0/0
#    which R represents as ‘NaN’, and prints as empty space
table (d.abc, direction=2, count="r")

# in the external mode,
#    the relative values are with regard to blocks of rows, and all columns
tab <- table (d.abc, "DIALECT.L2", count="r")
rows.a <- which (rownames(tab) %hasPrefix% "a")
sum (tab [rows.a, ])

## -----------------------------------------------------------------------------
# for a small dataset, the result is going to be small
str (allTables(d.abc), max.level=0)

# but it can grow quite quickly with a larger dataset
str (allTables(d.cap), max.level=0)

# the naming scheme
names (allTables(d.abc))

# and with ‘column’ not ‘NULL’
names (allTables(d.abc,column="DIALECT.L2"))

## -----------------------------------------------------------------------------
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

## -----------------------------------------------------------------------------
# prepare the data
dataset <- table (sampleSoundCorrsData.abc)

# prepare the models to be tested
models <- list (
	"model A" = list( formula="Y~a*(X+b)^2", start=list(list(a=1,b=1)) ),
	"model B" = list( formula="Y~a*(X-b)^2", start=list(list(a=1,b=1)) ))
# vanilla nls() often requires fairly accurate starting estimates

# fit the models to the dataset
fit <- fitTable (models, dataset, 1, vec2df.hist)

# inspect the results
summary (fit, metric="sigma")

## -----------------------------------------------------------------------------
# with n==1, ngrams() returns simply the frequencies of segments
ngrams (d.cap.ger)

# counts can easily be turned into a data frame with ranks
tab <- ngrams (d.cap.ger, 9, F)
mtx <- as.matrix (sort(tab,decreasing=T))
data.frame (RANK=1:length(mtx), COUNT=mtx, FREQ=mtx/sum(mtx))

## -----------------------------------------------------------------------------
# the difference between the two sifting modes

#    “ab” spans segments 1–2, while “a” only occupies segment 1
findPairs (d.abc, "ab", "a", exact=T)
findPairs (d.abc, "ab", "a", exact=F)

#    the exact mode ignores linguistic zeros
findPairs (d.abc, "-", "", exact=T)
findPairs (d.abc, "-", "", exact=F)

# ‘findPairs()’ accepts the usual and the custom regular expressions
findPairs (d.abc, "a", "o|u")
findPairs (d.abc, "a", "O")

# the output is actually a list
str (findPairs(d.abc,"a","a"), max.level=1)

# ‘data’ is what is displayed on the screen
# ‘found’ is a data.frame with the exact positions
# ‘which’ is useful for subsetting
subset (d.abc, findPairs(d.abc,"a","O")$which)

# the ‘cols’ argument can be used to alter the printed output
findPairs (d.abc, "a", "O", cols=c("ORTHOGRAPHY.L1","ORTHOGRAPHY.L2"))

## -----------------------------------------------------------------------------
# and see what result this gives
allPairs (d.abc, cols=c("ORTHOGRAPHY.L1","ORTHOGRAPHY.L2"))

# a clearer result could be obtained by running
# allPairs (d.cap, cols=c("ORTHOGRAPHY.German","ORTHOGRAPHY.Polish"),
#    file="~/Desktop/d.cap.html", formatter=formatter.html)

## -----------------------------------------------------------------------------
# load the new formatter function …
# source ("~/Desktop/myFormatter.R")

# … and use it instead of ‘formatter.html()’
# allPairs (d.cap, cols=c("ORTHOGRAPHY.German","ORTHOGRAPHY.Polish"),
#    file="~/Desktop/d.cap.html", formatter=myFormatter)
# note that this time the output will not open in the web browser automatically 

## -----------------------------------------------------------------------------
# in the ‘d.abc’ dataset, only one word exhibits L1 a : L2 o
ao <- findPairs (d.abc, "a", "o")

# it is the third one
ao$which

# and it has three segments, of which the first is the one we are looking for
ao

# hence
findSegments (d.abc, "a", "o", segment=0)

# and
findSegments (d.abc, "a", "o", segment=2)

# but
findSegments (d.abc, "a", "o", segment=-1)

# the output of ‘findSegments()’ can be turned into phonetic values
segms <- findSegments (d.abc, "b", "b", segment=1)
phon <- char2value (d.abc, "L1", segms$L1)
phon

# a table for manual inspection
mapply (function(l,s) char2value(d.abc,l,s), d.abc$names, segms)

# this result can then be further processed…
phon <- unlist (lapply (phon, function(i) grepl("cons",i)))

# … attached to a dataset
d.abc.new <- cbind (d.abc, BEFORE.CONSONANT=phon)

# … and analysed
table (d.abc.new, "BEFORE.CONSONANT")

# sadly, the procedure becomes more complicated if a correspondence
#    occurs more than once in a single word
findSegments (d.abc, "a", "a", segment=1)

## -----------------------------------------------------------------------------
# using the default ‘|’ …
addSeparators (d.abc$data$ORTHOGRAPHY.L1)

# … or a full stop
addSeparators (d.abc$data$ORTHOGRAPHY.L1, ".")

## -----------------------------------------------------------------------------
# build a table for a slightly larger dataset
tab <- table (d.cap)

# let us focus on L1 a and o
rows <- which (rownames(tab) %hasPrefix% "a")
cols <- which (colnames(tab) %hasPrefix% "o")
binTable (tab, rows, cols)

# or on all a-like and o-like vowels
rows <- which (rownames(tab) %hasPrefix% "[aāäǟ]")
cols <- which (colnames(tab) %hasPrefix% "[oōöȫ]")
binTable (tab, rows, cols)

## -----------------------------------------------------------------------------
# let us search a column other than the one specified as ‘aligned’
orth <- d.abc$data [, "ORTHOGRAPHY.L2"]

# look for all VCC sequences
query <- expandMeta(d.cap$trans[[1]],"VCC")
orth [grep(query,orth)]

# look for all VCC words
query <- expandMeta(d.cap$trans[[1]],"^VCC$")
orth [grep(query,orth)]

## -----------------------------------------------------------------------------
# build a table for a slightly larger dataset
tab <- table (d.cap)

# it is quite difficult to read as a whole, so let us focus
#    on a-like vowels in L1 and s-like consonants in L2
rows <- which (rownames(tab) %hasPrefix% "[aāäǟ]")
cols <- which (colnames(tab) %hasPrefix% "[sśš]")
tab [rows, cols]

## -----------------------------------------------------------------------------
# build a table for a slightly larger dataset
tab <- table (d.cap)

# it is quite difficult to read as a whole, so let us focus
#    on what corresponds to a-like vowels in L1 and s-like consonants in L2
rows <- which (rownames(tab) %hasSuffix% "[aāäǟ]")
cols <- which (colnames(tab) %hasSuffix% "[sśš]")
tab [rows, cols]

## -----------------------------------------------------------------------------
# let us prepare the tables
tabs <- allTables (d.abc, bin=F)

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

## -----------------------------------------------------------------------------
# the “abc” dataset is in the long format
abc.long <- read.table (path.abc, header=T)

# the simplest conversion unnecessarily doubles the ID column
long2wide (abc.long)

# but this can be avoided with the ‘skip’ argument
abc.wide <- long2wide (abc.long, skip="ID")

## -----------------------------------------------------------------------------
# select only examples from L2’s northern dialect
subset (d.abc, DIALECT.L2=="north") $data

# select only capitals of countries where German is an official language
subset (d.cap, grepl("German",d.cap$data$OFFICIAL.LANGUAGE)) $data

# select only pairs in which L1 a : L2 a
subset (d.abc, findPairs(d.abc,"a","a")$which) $data

## -----------------------------------------------------------------------------
# let us use the converted “abc” dataset
abc.wide

# with the separator preserved
wide2long (abc.wide, c(".L1",".L2"))

# and with the separator removed
wide2long (abc.wide, c(".L1",".L2"), strip=1)

