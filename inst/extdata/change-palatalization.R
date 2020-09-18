# A progressive implementation of the first palatalization in Slavic.

soundchange (

	# the sound change
	function (x, meta) {
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
