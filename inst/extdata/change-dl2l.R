# A regressive implementation of the *l, *dl > *l merger in Slavic.

soundchange (

	# the sound change
	function (x, meta) {
		# if x contains "l" …
		if (length(grep("l",x)) > 0)
			# … return both "l" and "dl"
			return (c(x, gsub("l","dl",x)))
		else
			# … or just x
			return (x)
	},

	# name
	"l < dl, l",

	# transcription
	trans,

	# description
	"A sample regressive implementation of Slavic *dl > *l simplification."

)
