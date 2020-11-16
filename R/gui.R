# vim: set shiftwidth=4 tabstop=4 foldmarker=<<<,>>>:

# = ui ============================================================================================= <<< =

# - ui.data ---------------------------------------------------------------------------------------- <<< -

#' @title The UI for the Data page.
#' @description The UI for the data loading page.
#' @keywords internal

ui.data <- function () {

	if (!requireNamespace("shiny", quietly=TRUE) || !requireNamespace("shinyjqui", quietly=TRUE))
		stop ("\"soundcorrsGUI\" requires packages \"shiny\" and \"shinyjqui\".")

	shiny::fluidPage (

		# transcription
		shiny::fluidRow (
			shiny::column (4,
				shiny::h3("Transcription"),
				shiny::fileInput ("data.trans.file", "Load from file:", accept=".tsv")),
			shiny::column (4),
			shiny::column (4,
				shiny::radioButtons ("data.trans.rad", shiny::HTML("Select the transcription for<br/>datasets / sound changes:"), c("")),
				shiny::actionButton ("data.trans.unload.btn", "Unload"),
				shiny::actionButton ("data.trans.view.btn", "View"))),

		# data
		shiny::hr (),
		shiny::fluidRow (
			shiny::column (4,
				shiny::h3("Data"),
				shiny::fileInput ("data.data.file", "Load from file:", accept=".tsv")),
			shiny::column (4,
				shiny::tags$div (id="data.data.placeholder")),
			shiny::column (4,
				shiny::radioButtons ("data.data.rad", "Loaded datasets:", c("")),
				shiny::actionButton ("data.data.unload.btn", "Unload"),
				shiny::actionButton ("data.data.view.btn", "View"))),

		# sound changes
		shiny::hr (),
		shiny::fluidRow (
			shiny::column (4,
				shiny::h3("Sound changes"),
				shiny::fileInput ("data.chngs.file", "Load from file:", accept=c(".R",".rda",".Rda",".RData"))),
			shiny::column (4),
			shiny::column (4,
				shiny::radioButtons ("data.chngs.rad", "Loaded changes:", c("")),
				shiny::actionButton ("data.chngs.unload.btn", "Unload"),
				shiny::actionButton ("data.chngs.view.btn", "View")))

	)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - ui.examples ------------------------------------------------------------------------------------ <<< -

#' @title The UI for the Examples page.
#' @description The UI for the examples-search page.
#' @keywords internal

ui.examples <- function () {

	if (!requireNamespace("shiny", quietly=TRUE) || !requireNamespace("shinyjqui", quietly=TRUE))
		stop ("\"soundcorrsGUI\" requires packages \"shiny\" and \"shinyjqui\".")

	shiny::fluidPage (

		# dataset selection
		shiny::fluidRow (
			shiny::column (3, shiny::selectInput ("exam.data.slct", "Select dataset:", NULL)),
			shiny::column (9)
		),

		# search options and results
		shiny::hr (),
		shiny::sidebarLayout (
			shiny::sidebarPanel (width = 3,
				shiny::actionButton (style="margin-bottom:30px", "exam.apply.btn", "Apply", width="100%"),
				shiny::uiOutput ("exam.query.txt"),
				shiny::sliderInput ("exam.distS.sli", "Distance at the beginning:", min=-1, max=5, value=-1),
				shiny::sliderInput ("exam.distE.sli", "Distance at the end:", min=-1, max=5, value=-1),
				shiny::checkboxInput ("exam.nas.chk", "NA counts as match", value=T),
				shiny::checkboxInput ("exam.zero.chk", "Include zeros", value=F),
				shiny::checkboxInput ("exam.perl.chk", "Use Perl-like regexp", value=F),
				shiny::checkboxGroupInput ("exam.cols.chk", "Display columns:")),
			shiny::mainPanel (width = 9,
				shiny::div(style="height:80vh;overflow:auto", shiny::uiOutput ("exam.res.tbl")))
		),

		# bottom info
		shiny::hr (),
		shiny::fluidRow (
			shiny::column (6, shiny::htmlOutput ("exam.sheet.txt1")),
			shiny::column (6, shiny::htmlOutput ("exam.sheet.txt2"))
		)

	)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - ui.soundchanges -------------------------------------------------------------------------------- <<< -

#' @title The UI for the Sound changes page.
#' @description The UI for the sound change application page.
#' @keywords internal

ui.soundchanges <- function () {

	if (!requireNamespace("shiny", quietly=TRUE) || !requireNamespace("shinyjqui", quietly=TRUE))
		stop ("\"soundcorrsGUI\" requires packages \"shiny\" and \"shinyjqui\".")

	shiny::fluidPage (

		# dataset selection
		shiny::fluidRow (
			shiny::column (3, shiny::selectInput ("chngs.data.slct", "Select dataset:", NULL)),
			shiny::column (3, shiny::selectInput ("chngs.src.slct", "Select source:", NULL)),
			shiny::column (3, shiny::selectInput ("chngs.tgt.slct", "Select target:", NULL)),
			shiny::column (3, shiny::selectInput ("chngs.met.slct", "Select metadata:", NULL))
		),

		# changes selection and results
		shiny::hr (),
		shiny::sidebarLayout (
			shiny::sidebarPanel (width = 3,
				shiny::actionButton (style="margin-bottom:30px", "chngs.apply.btn", "Apply", width="100%"),
				shinyjqui::sortableCheckboxGroupInput ("chngs.chngs.chk", "Select and order sound changes:"),
				shiny::downloadButton ("chngs.save.btn", "Save")),
			shiny::mainPanel (width = 9,
				# shiny::div(style="height:70vh;overflow:auto", shinyjqui::selectableTableOutput ("chngs.res.tbl", selection_mode="row")))
				shiny::htmlOutput ("chngs.info.txt"),
				shiny::dataTableOutput ("chngs.res.tbl"))
		)

	)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - ui.start --------------------------------------------------------------------------------------- <<< -

#' @title The UI for the Start page.
#' @description The UI for the starting page.
#' @keywords internal

ui.start <- function () {

	if (!requireNamespace("shiny", quietly=TRUE) || !requireNamespace("shinyjqui", quietly=TRUE))
		stop ("\"soundcorrsGUI\" requires packages \"shiny\" and \"shinyjqui\".")

	shiny::fluidPage (
		shiny::HTML("<noscript>JavaScript must be enabled. Perhaps an extension in your browser is blocking it?</noscript>"),
		shiny::htmlOutput ("start.txt")
	)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - ui.tables -------------------------------------------------------------------------------------- <<< -

#' @title The UI for the Tables changes page.
#' @description The UI for the contingency tables page.
#' @keywords internal

ui.tables <- function () {

	if (!requireNamespace("shiny", quietly=TRUE) || !requireNamespace("shinyjqui", quietly=TRUE))
		stop ("\"soundcorrsGUI\" requires packages \"shiny\" and \"shinyjqui\".")

	shiny::fluidPage (

		# dataset selection
		shiny::fluidRow (
			shiny::column (3, shiny::selectInput ("tbls.data.slct", "Select dataset:", NULL)),
			shiny::column (3, shiny::tags$div (id="tbls.column.placeholder")),
			shiny::column (6)
		),

		# different tables
		shiny::hr (),
		shiny::sidebarLayout (
			shiny::sidebarPanel (width=3,
				shiny::actionButton (style="margin-bottom:30px", "tbls.apply.btn", "Apply", width="100%"),
				shiny::radioButtons ("tbls.count.rad", "Count:", c("Absolute","Relative")),
				shiny::radioButtons ("tbls.unit.rad", "Unit:", c("Occurrences","Words"), "Words")),
			shiny::mainPanel (
				shiny::tabsetPanel (id="tbls.tbls.panl",
					shiny::tabPanel ("Segment-to-segment", shiny::dataTableOutput ("tbls.summ.tbl")),
					shiny::tabPanel ("Correspondence-to-correspondence", shiny::dataTableOutput ("tbls.cooccI.tbl")),
					shiny::tabPanel ("Correspondence-to-metadata", shiny::dataTableOutput ("tbls.cooccE.tbl")))
			)
		)

	)

}

# -------------------------------------------------------------------------------------------------- >>> -
# - ui --------------------------------------------------------------------------------------------- <<< -

#' @title The UI for navigation bar.
#' @description The UI for the navigation bar at the top of the screen, and simultaneously for the entire app.
#' @keywords internal

ui <- function () {

	if (!requireNamespace("shiny", quietly=TRUE) || !requireNamespace("shinyjqui", quietly=TRUE))
		stop ("\"soundcorrsGUI\" requires packages \"shiny\" and \"shinyjqui\".")

	shiny::navbarPage ("soundcorrs",
		shiny::tabPanel ("Start", ui.start()),
		shiny::tabPanel ("Data", ui.data()),
		shiny::tabPanel ("Examples", ui.examples()),
		shiny::tabPanel ("Sound changes", ui.soundchanges()),
		shiny::tabPanel ("Tables", ui.tables())
	)

}

# -------------------------------------------------------------------------------------------------- >>> -

# ================================================================================================== >>> =
# = server ========================================================================================= <<< =

#' @title The main server for the UI.
#' @description Can't be split into separate bits as neatly as the UI, so the entire server is one function.
#' @keywords internal
#' @importFrom tools file_ext

server <- function (input, output, session) {

	if (!requireNamespace("shiny", quietly=TRUE) || !requireNamespace("shinyjqui", quietly=TRUE))
		stop ("\"soundcorrsGUI\" requires packages \"shiny\" and \"shinyjqui\".")

# - preparation ------------------------------------------------------------------------------------ <<< -

	# load the start pages
	output$start.txt <- shiny::renderText ({
		shiny::addResourcePath ("docPath", system.file("doc","",package="soundcorrs"))
		paste0("<iframe style=\"position:absolute; height:100%; width:100%; border:none\"
			src=\"docPath/soundcorrs.html\" />")
	})

	# fill in the cheat sheet in examples
	output$exam.sheet.txt1 <- shiny::renderText (
		"<table>
			<tr><th>Alternatives</th></tr>
			<tr><td>(<i>ab</i>|<i>cd</i>)</td><td>any of <i>ab</i>, </cd> (multiple characters)</td></tr>
			<tr><td>[<i>abc</i>]</td><td>any of <i>a</i>, <i>b</i>, <i>c</i> (single character)</td></tr>
			<tr><td>[^<i>abc</i>]</td><td>any character except <i>a</i>, <i>b</i>, <i>c</i> (single character)</td></tr>
			<tr><th>Position</th></tr>
			<tr><td>^<i>abc</i></td><td>beginning of the word</td></tr>
			<tr><td><i>abc</i>$</td><td>end of the word</td></tr>
			<tr><th>Repetition</th></tr>
			<tr><td>(<i>abc</i>)*</td><td>any number of times (greedy)</td></tr>
			<tr><td>(<i>abc</i>)*?</td><td>any number of times (lazy)</td></tr>
			<tr><td>(<i>abc</i>)+</td><td>one or more times</td></tr>
			<tr><td>(<i>abc</i>)?</td><td>one or fewer times</td></tr>
			<tr><th>Lookarounds</th></tr>
			<tr><th>(require Perl-like regexp)</th></tr>
			<tr><td><i>a</i>(?=<i>b</i>)</td><td><i>a</i> followed by <i>b</i></td></tr>
			<tr><td>(?<=<i>a</i>)<i>b</i></td><td><i>b</i> preceded by <i>a</i></td></tr>
		</table>"
	)

	# prep vars
	tmpChngs <- NULL	# result holder for chngs.tree.txt
	tmpData <- NULL		# result holder for data.load.btn

	# to remember which datasets are loaded
	loaddChngs <- list (
		suppressWarnings (loadSampleDataset("change-dl2l")),
		suppressWarnings (loadSampleDataset("change-palatalization")),
		suppressWarnings (loadSampleDataset("change-rhotacism")))
	loaddData <- list (
		suppressWarnings (loadSampleDataset("data-abc")),
		suppressWarnings (loadSampleDataset("data-capitals")),
		suppressWarnings (loadSampleDataset("data-ie")))
	loaddTrans <- list (
		suppressWarnings (loadSampleDataset("trans-common")),
		suppressWarnings (loadSampleDataset("trans-ipa")))

	# fix names in loadd
	names(loaddChngs) <- sapply (loaddChngs, `[[`, "name")
	names(loaddData) <- path2name (sapply (loaddData, attr, "file"))
	names(loaddTrans) <- path2name (sapply (loaddTrans, attr, "file"))

	# update the loaded fields
	# changes
	tmp <- names (loaddChngs)
	shiny::updateRadioButtons (session, "data.chngs.rad", choices=tmp)
	shiny::updateCheckboxGroupInput (session, "chngs.chngs.chk", choices=tmp, selected=tmp)
	# data
	tmp <- names (loaddData)
	shiny::updateRadioButtons (session, "data.data.rad", choices=tmp)
	shiny::updateSelectInput (session, "chngs.data.slct", choices=tmp)
	shiny::updateSelectInput (session, "exam.data.slct", choices=tmp)
	shiny::updateSelectInput (session, "tbls.data.slct", choices=tmp)
	# transcriptions
	shiny::updateRadioButtons (session, "data.trans.rad", choices=names(loaddTrans))

	# kill server when tab closed
	session$onSessionEnded (shiny::stopApp)

# -------------------------------------------------------------------------------------------------- >>> -
# - chngs.apply.btn -------------------------------------------------------------------------------- <<< -

	# prepare and display the result of applyChanges
	shiny::observeEvent (input$chngs.apply.btn, {

		# check which changes are active and in what order
		tmp.chngs <- if (is.null(input$chngs.chngs.chk_order)) input$chngs.chngs.chk else input$chngs.chngs.chk_order
		tmp.chngs <- tmp.chngs [tmp.chngs %in% input$chngs.chngs.chk]
		tmp.chngs <- loaddChngs [tmp.chngs]

		# check which dataset
		dat <- loaddData [[input$chngs.data.slct]]

		# prepare columns
		tmp.src <- input$chngs.src.slct
		tmp.tgt <- if (input$chngs.tgt.slct=="---") NULL else input$chngs.tgt.slct
		tmp.met <- if (input$chngs.met.slct=="---") NULL else input$chngs.met.slct

		# try to prepare the result
		tmp.res <- tryCatch (
			applyChanges (dat, tmp.chngs, tmp.src, tmp.tgt, tmp.met, "HTML")
		, warning = function (w) {
			applyChanges (dat, tmp.chngs, tmp.src, tmp.tgt, tmp.met, "HTML")
		}, error = function (e)
			shiny::showModal (shiny::modalDialog (title="Error", e$message, easyClose=T))
		)

		# update display, maybe
		if (!is.null(tmp.res)) {

			# prepare the core of the data frame
			res <- data.frame (
				Source = names (tmp.res$end),
				Result = sapply (tmp.res$end, collapse, inter=", ")
			)

			# add target, meta, and match maybe
			if (!is.null(tmp.tgt)) res<-cbind(res,Target=dat$data[,tmp.tgt])
			if (!is.null(tmp.met)) res<-cbind(res,Metadata=dat$data[,tmp.met])
			if (!is.null(tmp.tgt)) {
				tmp <- tmp.res$match
				tmp <- gsub (.5, "partial", tmp)
				tmp <- gsub (0, "none", tmp)
				tmp <- gsub (1, "full", tmp)
				res <- cbind (res, Match=tmp)
			}

			# add buttons for tree viewing
			res <- cbind (res, Intermediate = sapply(seq_along(tmp.res$end), function (i) paste0 (
				"<button id=\"chngs.res.btn_", i, "\" ",
				"onclick=\"Shiny.onInputChange('chngs.res.btn', this.id);\" ",
				"type=\"button\">View</button>")))

			# update display and holder for the tree view
			output$chngs.info.txt <- shiny::renderText (if (is.null(tmp.tgt)) "" else {
				tmp.full <- length (tmp.res$match[tmp.res$match==1])
				tmp.part <- length (tmp.res$match[tmp.res$match==.5])
				tmp.none <- length (tmp.res$match[tmp.res$match==0])
				tmp.all <- length (tmp.res$match)
				paste0 (
					"Full matches: ", tmp.full, " (", round(tmp.full/tmp.all*100), "%)&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;",
					"Partial matches: ", tmp.part, " (", round(tmp.part/tmp.all*100), "%)&nbsp;&nbsp;&nbsp;|&nbsp;&nbsp;&nbsp;",
					"Non-matches: ", tmp.none, " (", round(tmp.none/tmp.all*100), "%)<br/><br/>")
			})
			output$chngs.res.tbl <- shiny::renderDataTable (res, escape=F)
			tmpChngs <<- tmp.res

		}

	})

# -------------------------------------------------------------------------------------------------- >>> -
# - chngs.data.slct -------------------------------------------------------------------------------- <<< -

	# update parameter selection for changes
	shiny::observeEvent (input$chngs.data.slct, {

		tmp <- colnames (loaddData[[input$chngs.data.slct]]$data)
		shiny::updateSelectInput (session, "chngs.src.slct", choices=tmp)
		shiny::updateSelectInput (session, "chngs.tgt.slct", choices=c("---",tmp))
		shiny::updateSelectInput (session, "chngs.met.slct", choices=c("---",tmp))

	})

# -------------------------------------------------------------------------------------------------- >>> -
# - chngs.res.btn ---------------------------------------------------------------------------------- <<< -

	# update the tree in bottom info
	shiny::observeEvent (input$chngs.res.btn, {

		# find which row
		row <- as.numeric (strsplit(input$chngs.res.btn, "_")[[1]][2])

		# get the tree
		tmp <- tmpChngs$tree [row]
		tmp <- capture.output (print.tree.applyChanges(tmp))
		tmp <- collapse (tmp, inter="<br/>")

		# show the tree
		shiny::showModal (shiny::modalDialog (
			title = paste0 ("Intermediate forms for ", names(tmpChngs$tree)[row]),
			shiny::HTML (tmp),
			easyClose=T))

	})

# -------------------------------------------------------------------------------------------------- >>> -
# - chngs.save.btn --------------------------------------------------------------------------------- <<< -

	# save the modified changes
	output$chngs.save.btn <- shiny::downloadHandler (

		filename = "soundchanges.rda",
		content = function (file) {
			tmp <- if (is.null(input$chngs.chngs.chk_order)) input$chngs.chngs.chk else input$chngs.chngs.chk_order
			tmp <- tmp [tmp %in% input$chngs.chngs.chk]
			soundcorrsSavedSoundChanges <- loaddChngs[tmp]
			save (soundcorrsSavedSoundChanges, file=file)
		}

	)

# -------------------------------------------------------------------------------------------------- >>> -
# - data.chngs.file -------------------------------------------------------------------------------- <<< -

	# load file for transcription
	shiny::observeEvent (input$data.chngs.file, {

		# make sure it isn't already loaded
		if (input$data.chngs.file$name %in% names(loaddChngs)) {
			shiny::showModal (shiny::modalDialog (title="Error", paste0("File \"",input$data.chngs.file$name,"\" already loaded."), easyClose=T))
			return ()
		}

		# make sure a transcription is selected
		if (input$data.trans.rad %nin% names(loaddTrans)) {
			shiny::showModal (shiny::modalDialog (title="Error", "No transcription is selected.", easyClose=T))
			return ()
		}

		# prepare temporary environment
		tmp.env <- new.env ()

		# prepare ..selectedTranscription..
		assign ("..selectedTranscription..", loaddTrans[[input$data.trans.rad]], envir=tmp.env)

		# load from rda
		if (tools::file_ext(input$data.chngs.file$name) %in% c("rda","Rda","RData")) {
			# try loading
			tryCatch (
				load (input$data.chngs.file$datapath, tmp.env)
			, warning = function (w) {
				load (input$data.chngs.file$datapath, tmp.env)
				shiny::showNotification (w$message)
			}, error = function (e)
				shiny::showModal (shiny::modalDialog (title="Error", e$message, easyClose=T))
			)
			# load the whole loaddChngs
			if ("soundcorrsSavedSoundChanges" %nin% ls(tmp.env))
				shiny::showModal (shiny::modalDialog (title="Error", "Not a soundcorrs file.", easyClose=T))
			else
				res <- get("soundcorrsSavedSoundChanges",tmp.env)

		# load from R
		} else if (tools::file_ext(input$data.chngs.file$name) == "R") {
			# try sourcing
			tryCatch (
				source (input$data.chngs.file$datapath, tmp.env)
			, warning = function (w) {
				source (input$data.chngs.file$datapath, tmp.env)
				shiny::showNotification (w$message)
			}, error = function (e)
				shiny::showModal (shiny::modalDialog (title="Error", e$message, easyClose=T))
			)
			# load soundchange objects
			tmp <- ls(tmp.env) [sapply (ls(tmp.env), class %.% get, tmp.env) == "soundchange"]
			res <- lapply (tmp, get, tmp.env)

		# wrong file type
		} else
			shiny::showModal (shiny::modalDialog (title="Error", "Unsupported file type.", easyClose=T))

		# update loaddChngs
		for (i in res)
			if (i$name %nin% names(loaddChngs))
				loaddChngs[i$name] <<- list(i)
			else
				shiny::showNotification (paste0 ("Sound change \"", i$name,"\" already loaded."))

		# update display
		if (length(loaddChngs) == 0) {
			shiny::updateRadioButtons (session, "data.chngs.rad", choices=character(0), selected="")
			shiny::updateCheckboxGroupInput (session, "chngs.chngs.chk", choices=character(0), selected="")
		} else {
			shiny::updateRadioButtons (session, "data.chngs.rad", choices=names(loaddChngs))
			shiny::updateCheckboxGroupInput (session, "chngs.chngs.chk", choices=names(loaddChngs), selected=names(loaddChngs))
		}

	})

# -------------------------------------------------------------------------------------------------- >>> -
# - data.chngs.unload.btn -------------------------------------------------------------------------- <<< -

	# unload selected changes
	shiny::observeEvent (input$data.chngs.unload.btn, {

		# unload
		loaddChngs[[input$data.chngs.rad]] <<- NULL

		# update display
		if (length(loaddChngs) == 0) {
			shiny::updateRadioButtons (session, "data.chngs.rad", choices=character(0), selected="")
			shiny::updateCheckboxGroupInput (session, "chngs.chngs.chk", choices=character(0), selected="")
		} else {
			shiny::updateRadioButtons (session, "data.chngs.rad", choices=names(loaddChngs))
			shiny::updateCheckboxGroupInput (session, "chngs.chngs.chk", choices=names(loaddChngs))
		}

	})

# -------------------------------------------------------------------------------------------------- >>> -
# - data.chngs.view.btn ---------------------------------------------------------------------------- <<< -

	# preview selected change
	shiny::observeEvent (input$data.chngs.view.btn, {

		# prepare ui
		shiny::showModal (shiny::modalDialog (title=input$data.chngs.rad,
			shiny::htmlOutput ("data.chngs.view.txt"),
			shiny::hr (),
			shiny::htmlOutput ("data.chngs.view.txt2")
		, easyClose=T))

		# fill in the ui
		dat <- loaddChngs[[input$data.chngs.rad]]
		tmp <- if (is.null(dat$desc)) "" else paste0("<b>Description:</b> ",dat$desc,"<br/>")
		tmp <- if (is.null(attr(dat$trans,"file"))) tmp else paste0(tmp,"<b>Transcription:</b> ",attr(dat$trans,"file"))
		output$data.chngs.view.txt <- shiny::renderText (tmp)
		# must be a separate variable or data.chns.view.txt gets updated too
		tmp2 <- capture.output (print (dat$fun))
		tmp2 <- gsub (" ", "&nbsp;", tmp2)
		tmp2 <- gsub ("\t", "&nbsp;&nbsp;&nbsp;&nbsp;", tmp2)
		tmp2 <- gsub ("\n", "<br/>", tmp2)
		tmp2 <- gsub ("\r", "<br/>", tmp2)
		tmp2 <- collapse (tmp2, inter="<br/>")
		output$data.chngs.view.txt2 <- shiny::renderText (tmp2)

	})

# -------------------------------------------------------------------------------------------------- >>> -
# - data.load.btn ---------------------------------------------------------------------------------- <<< -

	# load soundcorrs from selected columns
	shiny::observeEvent (input$data.load.btn, {

		# make sure some columns are selected
		if (is.null(input$data.data.chk)) {
			shiny::showModal (shiny::modalDialog (title="Error", "No columns are selected.", easyClose=T))
			return ()
		}

		# make sure a transcription is selected
		if (input$data.trans.rad %nin% names(loaddTrans)) {
			shiny::showModal (shiny::modalDialog (title="Error", "No transcription is selected.", easyClose=T))
			return ()
		}

		# try to create individual soundcorrs objects
		tmp <- lapply (input$data.data.chk, function (x)
			tryCatch (
				soundcorrs (tmpData, x, x, loaddTrans[[input$data.trans.rad]])
			, warning = function (w) {
				shiny::showNotification (w$message)
				soundcorrs (tmpData, x, x, loaddTrans[[input$data.trans.rad]])
			}, error = function (e)
				shiny::showModal (shiny::modalDialog (title="Error", e$message, easyClose=T))
			)
		)

		# on error abort (won't work inside the loop above)
		if (any(sapply(tmp,is.null))) return()

		# try to merge into a single object
		res <- if (length(tmp)==1) tmp[[1]] else
			tryCatch (
				do.call (merge, tmp)
			, warning = function (w) {
				shiny::showNotification (w$message)
				do.call (merge, tmp)
			}, error = function (e)
				shiny::showModal (shiny::modalDialog (title="Error", e$message, easyClose=T))
			)

		# update
		if (!is.null(res)) {

			# loaddData
			attr(res,"file") <- input$data.data.file$datapath
			loaddData[path2name(input$data.data.file$name)] <<- list(res)

			# clean up display
			shiny::removeUI (selector="#data\\.data\\.chk")
			shiny::removeUI (selector="#data\\.load\\.btn")

			# outputs everywhere
			tmp <- names (loaddData)
			shiny::updateRadioButtons (session, "data.data.rad", choices=tmp)
			shiny::updateSelectInput (session, "chngs.data.slct", choices=tmp)
			shiny::updateSelectInput (session, "exam.data.slct", choices=tmp)
			shiny::updateSelectInput (session, "tbls.data.slct", choices=tmp)
		}

	})

# -------------------------------------------------------------------------------------------------- >>> -
# - data.data.file --------------------------------------------------------------------------------- <<< -

	# load file for soundcorrs
	shiny::observeEvent (input$data.data.file, {

		# make sure it isn't already loaded
		if (input$data.data.file$name %in% names(loaddData)) {
			shiny::showModal (shiny::modalDialog (title="Error", paste0("File \"",input$data.data.file$name,"\" already loaded."), easyClose=T))
			return ()
		}

		# try to read the file
		tmpData <<- tryCatch (
			read.table (input$data.data.file$datapath, header=T, stringsAsFactors=F, quote="")
		, warning = function (w)
			shiny::showNotification (w$message)
		, error = function (e)
			shiny::showModal (shiny::modalDialog (title="Error", e$message, easyClose=T))
		)

		# display column names
		if (!is.null(tmpData)) {

			shiny::insertUI (
				selector = "#data\\.data\\.placeholder",
				where = "beforeBegin",
				ui = shiny::tags$div (
					id = "data.data.chk",
					shiny::checkboxGroupInput ("data.data.chk", "Select columns with segmented, aligned data:", choices=colnames(tmpData))
				)
			)

			shiny::insertUI (
				selector = "#data\\.data\\.placeholder",
				where = "beforeBegin",
				ui = shiny::tags$div (
					id = "data.load.btn",
					shiny::actionButton ("data.load.btn", "Load")
				)
			)

		}

	})

# -------------------------------------------------------------------------------------------------- >>> -
# - data.data.unload.btn --------------------------------------------------------------------------- <<< -

	# unload selected dataset
	shiny::observeEvent (input$data.data.unload.btn, {

		# unload
		loaddData[[input$data.data.rad]] <<- NULL

		# update display
		if (length(loaddData) == 0) {

			shiny::updateSelectInput (session, "chngs.data.slct", choices=character(0), selected="")
			shiny::updateSelectInput (session, "exam.data.slct", choices=character(0), selected="")
			shiny::updateSelectInput (session, "tbls.data.slct", choices=character(0), selected="")
			shiny::updateRadioButtons (session, "data.data.rad", choices=character(0), selected="")

			# if loaddData is empty fields in other tabs must be updated manually
			output$exam.query.txt <- shiny::renderUI (NULL)
			shiny::updateSelectInput (session, "chngs.src.slct", choices=character(0), selected="")
			shiny::updateSelectInput (session, "chngs.tgt.slct", choices=character(0), selected="")
			shiny::updateSelectInput (session, "chngs.met.slct", choices=character(0), selected="")
			shiny::updateSelectInput (session, "tbls.column.slct", choices=character(0), selected="")

		} else {

			# if loaddData isn't empty fields in other tabs will update automatically
			tmp <- names (loaddData)
			shiny::updateSelectInput (session, "chngs.data.slct", choices=tmp)
			shiny::updateSelectInput (session, "exam.data.slct", choices=tmp)
			shiny::updateSelectInput (session, "tbls.data.slct", choices=tmp)
			shiny::updateRadioButtons (session, "data.data.rad", choices=tmp)

		}

	})

# -------------------------------------------------------------------------------------------------- >>> -
# - data.data.view.btn ----------------------------------------------------------------------------- <<< -

	# preview selected dataset
	shiny::observeEvent (input$data.data.view.btn, {

		# prepare ui
		shiny::showModal (shiny::modalDialog (title=input$data.data.rad,
			shiny::htmlOutput ("data.data.view.txt"),
			shiny::hr (),
			shiny::dataTableOutput ("data.data.view.tbl")
		, easyClose=T))

		# fill in the ui
		dat <- loaddData[[input$data.data.rad]]
		output$data.data.view.txt <- shiny::renderText (paste0 (
			"<b>Languages (", length(dat$names), "):</b> ", collapse(dat$names,inter=", "), "<br/>",
			"<b>Entries:</b> ", nrow(dat$data), "<br/>",
			"<b>File:</b> ", attr(dat,"file")
		))
		output$data.data.view.tbl <- shiny::renderDataTable (dat$data, options=list(scrollX=T))

	})

# -------------------------------------------------------------------------------------------------- >>> -
# - data.trans.file -------------------------------------------------------------------------------- <<< -

	# load file for transcription
	shiny::observeEvent (input$data.trans.file, {

		# make sure it isn't already loaded
		if (input$data.trans.file$name %in% names(loaddTrans)) {
			shiny::showModal (shiny::modalDialog (title="Error", paste0("File \"",input$data.trans.file$name,"\" already loaded."), easyClose=T))
			return ()
		}

		# try to load the transcription
		tmp <- tryCatch (
			read.transcription (input$data.trans.file$datapath)
		, warning = function (w) {
			shiny::showNotification (w$message)
			read.transcription (input$data.trans.file$datapath)
		}, error = function (e)
			shiny::showModal (shiny::modalDialog (title="Error", e$message, easyClose=T))
		)

		# if successful, update loaddTrans and display
		if (!is.null(tmp)) {
			attr(tmp,"file") <- input$data.trans.file$datapath
			loaddTrans[path2name(input$data.trans.file$name)] <<- list(tmp)
			shiny::updateRadioButtons (session, "data.trans.rad", choices=names(loaddTrans))
		}

	})

# -------------------------------------------------------------------------------------------------- >>> -
# - data.trans.unload.btn -------------------------------------------------------------------------- <<< -

	# unload selected transcription
	shiny::observeEvent (input$data.trans.unload.btn, {

		# unload
		loaddTrans[[input$data.trans.rad]] <<- NULL

		# update display
		if (length(loaddTrans) == 0)
			shiny::updateRadioButtons (session, "data.trans.rad", choices=character(0), selected="")
		else
			shiny::updateRadioButtons (session, "data.trans.rad", choices=names(loaddTrans))

	})

# -------------------------------------------------------------------------------------------------- >>> -
# - data.trans.view.btn ---------------------------------------------------------------------------- <<< -

	# preview selected transcription
	shiny::observeEvent (input$data.trans.view.btn, {

		# prepare ui
		shiny::showModal (shiny::modalDialog (title=input$data.trans.rad,
			shiny::htmlOutput ("data.trans.view.txt"),
			shiny::hr (),
			shiny::dataTableOutput ("data.trans.view.tbl")
		, easyClose=T))

		# fill in the ui
		dat <- loaddTrans[[input$data.trans.rad]]
		output$data.trans.view.txt <- shiny::renderText (paste0 (
			"<b>Graphemes:</b> ", nrow(dat$data), "<br/>",
			"<b>File:</b> ", attr(dat,"file")
		))
		output$data.trans.view.tbl <- shiny::renderDataTable (dat$data, options=list(scrollX=T))

	})

# -------------------------------------------------------------------------------------------------- >>> -
# - exam.apply.btn --------------------------------------------------------------------------------- <<< -

	# prepare and display the result of findExamples
	shiny::observeEvent (input$exam.apply.btn, {

		# collect the queries from text inputs
		queries <- lapply (seq_along(loaddData[[input$exam.data.slct]]$names), function (i)
			input[[paste0("exam.query.txt",i)]])

		# try to execute the search
		res <- tryCatch (findExamples (
			data = loaddData[[input$exam.data.slct]],
			queries,
			distance.start = input$exam.distS.sli,
			distance.end = input$exam.distE.sli,
			na.value = if (input$exam.nas.chk) 0 else -1,
			zeros = input$exam.zero.chk,
			cols = input$exam.cols.chk,
			perl = input$exam.perl.chk
		), error = function (e)
			shiny::showModal (shiny::modalDialog (title="Error", e$message, easyClose=T))
		)

		# display the results, maybe
		if (!is.null(res))
			output$exam.res.tbl <- if (nrow(res$data) == 0)
				shiny::renderText ("No matches found.")
			else
				shiny::renderTable (res$data)

	})

# -------------------------------------------------------------------------------------------------- >>> -
# - exam.data.slct --------------------------------------------------------------------------------- <<< -

	# update display for examples
	shiny::observeEvent (input$exam.data.slct, {

		# prep var
		dat <- loaddData[[input$exam.data.slct]]

		# update inputs for queries
		output$exam.query.txt <- shiny::renderUI (lapply (seq_along(dat$names), function (i)
			shiny::textInput (inputId=paste0("exam.query.txt",i), label=dat$names[i])))

		# update columns
		shiny::updateCheckboxGroupInput (session, "exam.cols.chk", choices=colnames(dat$data), selected=sapply(dat$cols,`[[`,"aligned"))

		# update regexp cheat sheet
		output$exam.sheet.txt2 <- shiny::renderText ({
			tbl <- "<table>"
			tbl <- paste0 (tbl, "<tr><th>Wildcards</th></tr>")
			tbl <- paste0 (tbl, "<tr><td>.</td><td>any character</td></tr>")
			tbl <- paste0 (tbl, if (length(unique(dat$trans)) == 1) {
				tmp <- dat$trans[[1]]$data[,dat$trans[[1]]$cols$grapheme] %in% names(dat$trans[[1]]$meta)
				res <- collapse (mapply (function (g, v)
					paste0 ("<tr><td>",g,"</td><td>",v,"</td></tr>"),
					dat$trans[[1]]$data [tmp, dat$trans[[1]]$cols$grapheme],
					dat$trans[[1]]$data [tmp, dat$trans[[1]]$cols$value], SIMPLIFY=T))
				paste0 (res, "</table>")
			} else
				"</table>The dataset uses different transcriptions for different languages. Custom wildcards cannot be listed."
			)

		})

	})

# -------------------------------------------------------------------------------------------------- >>> -
# - tbls.apply.btn --------------------------------------------------------------------------------- <<< -

	# prepare and display the result of summary or coocc
	shiny::observeEvent (input$tbls.apply.btn, {

		# prep vars
		dat <- loaddData[[input$tbls.data.slct]]
		cnt <- tolower (input$tbls.count.rad)
		unt <- tolower (input$tbls.unit.rad)

		# prepare and update display
		res <- switch (input$tbls.tbls.panl,
			"Segment-to-segment" = tryCatch (
				summary(dat,cnt,unt),
				warning=function(w) {shiny::showNotification(w$message); summary(dat,cnt,unt)}),
			"Correspondence-to-correspondence" = coocc (dat, column=NULL, count=cnt, unit=unt),
			"Correspondence-to-metadata" = coocc (dat, column=input$tbls.column.slct, count=cnt, unit=unt))

		# round, maybe, and convert
		if (cnt=="relative") res<-round(res,2)
		res <- as.data.frame (res)

		# fix column names
		if (input$tbls.tbls.panl == "Correspondence-to-correspondence")
			colnames(res) <- c(rep(colnames(res)[1],2), "Frequency")
		else
			colnames(res)[ncol(res)] <- "Frequency"

		# update display
		switch (input$tbls.tbls.panl,
			"Segment-to-segment" = output$tbls.summ.tbl <- shiny::renderDataTable (res),
			"Correspondence-to-correspondence" = output$tbls.cooccI.tbl <- shiny::renderDataTable (res),
			"Correspondence-to-metadata" = output$tbls.cooccE.tbl <- shiny::renderDataTable (res))

	})

# -------------------------------------------------------------------------------------------------- >>> -
# - tbls.data.slct --------------------------------------------------------------------------------- <<< -

	# update display for tables
	shiny::observeEvent (input$tbls.data.slct,

		if (input$tbls.tbls.panl == "Correspondence-to-metadata") {

			tmp <- colnames (loaddData[[input$tbls.data.slct]]$data)
			shiny::updateSelectInput (session, "tbls.column.slct", choices=tmp, selected=tmp[1])

		}
	)

# -------------------------------------------------------------------------------------------------- >>> -
# - tbls.tbls.panl ---------------------------------------------------------------------------------- <<< -

	# insert/remove column selection for tables
	shiny::observeEvent (input$tbls.tbls.panl,

		if (input$tbls.tbls.panl == "Correspondence-to-metadata")

			shiny::insertUI (
				selector = "#tbls\\.column\\.placeholder",
				where = "beforeBegin",
				ui = shiny::tags$div (
					id = "tbls.column.slct",
					shiny::selectInput ("tbls.column.slct", "Select column:",
						colnames(loaddData[[input$tbls.data.slct]]$data))
				)
			)

		else

			shiny::removeUI (selector="#tbls\\.column\\.slct")

	)

# -------------------------------------------------------------------------------------------------- >>> -

}

# ================================================================================================== >>> =
# = exp soundcorrsGUI ============================================================================== <<< =

#' @title GUI for \code{\link{soundcorrs}}.
#' @description A graphic user interface for the \code{\link{soundcorrs}} package.
#' @details The console version of \code{\link{soundcorrs}} is flexible and offers a wide range of possibilities but it also requires a degree of experience with R and programming in general. This object provides an interface to \code{\link{soundcorrs}} which removes a considerable part of this requirement, though it is at the cost of flexibility and power.
#'
#'	When running the app from the console, press ctrl-c to quit the server.
#' @return [shiny.appobj] A \code{shiny} app object. To run the app, print it or pass it to \code{shiny::runApp}.
#' @export

soundcorrsGUI <- function () {

	if (!requireNamespace("shiny", quietly=TRUE) || !requireNamespace("shinyjqui", quietly=TRUE))
		stop ("\"soundcorrsGUI\" requires packages \"shiny\" and \"shinyjqui\".")

	shiny::shinyApp (ui, server)

}

# ================================================================================================== >>> =
