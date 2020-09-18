# A regressive implementation of the *l, *dl > *l merger in Slavic.

soundchange (

	# the sound change
	function (x, meta) {
		if (length(grep("l",x)) > 0)
			return (c(x, gsub("l","dl",x)))
		else
			return (x)
	},
	
	# name
	"l < dl, l",
	
	# transcription
	trans,

	# description
	"A sample regressive implementation of Slavic *dl > *l simplification."

)
