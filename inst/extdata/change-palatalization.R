# A progressive implementation of the first palatalization in Slavic.

soundchange (

	# the sound change
	function (x, meta) {
		# NOTE:
		# If the encoding is garbled, make sure both your operating system and
		# RStudio (or whichever program is used for R) are set to UTF-8.
		# If this doesn’t help, please only use ASCII in your project.
		# Support for Unicode under Windows is highly problematic for R, and
		# indeed many other platforms. Future versions of R, or of soundcorrs,
		# might be able to solve the problem, but for now the only solutions
		# are either a switch to Linux or Mac, or sticking to pure ASCII.
		tmp <- x
		tmp <- gsub ("k([eēiī])", "č\\1", tmp)
		tmp <- gsub ("g([eēiī])", "ž\\1", tmp)
		tmp <- gsub ("χ([eēiī])", "š\\1", tmp)
		return (tmp)
	},

	# name
	"first palatalization",

	# transcription
	trans,

	# description
	"A sample progressive implementation of Slavic first palatalization."

)
