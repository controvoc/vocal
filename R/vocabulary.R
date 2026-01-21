
.vocal_environment <- new.env(parent=emptyenv())


#setClass("Vocabulary",
#	representation (
#		name = "character",
#		checked = "logical",
#		read = "logical",
#		voc = "list"
#	)	is.valid(object)
#)



vocabulary_path <- function(voc) {
	if (grepl("^github:", voc)) {
		voc <- gsub("^github:", "", voc)
		file.path(rappdirs::user_data_dir(), ".vocal", voc)
	} else { 
		# local
		voc
	}
}


exists_vocabulary <- function() {
	TRUE
}


valid_vocabulary <- function() {
	TRUE
}

set_vocabulary <- function(name, update=TRUE, force=FALSE, quiet=FALSE) {
	oldname <- .vocal_environment$name
	if (!isTRUE(identical(name, oldname))) {
		.vocal_environment$name <- name
		.vocal_environment$checked <- FALSE
		.vocal_environment$read <- FALSE
		check_installed(name)
		d <- try(read_vocabulary())
		if (!inherits(d, "try-error")) {
			.vocal_environment$voc <- d
			.vocal_environment$read <- TRUE
		}
	}
}

get_vocabulary <- function() {
	voc <- .vocal_environment$name
	if (is.null(voc)) {
		stop("No vocabulary has been set", call. = FALSE)
	}
	voc
}


has_names <- function(v, req) {
	test <- sapply(v, \(i) req %in% names(i))
	if (!(all(test))) {
		stop("incomplete variables files")
	}
}

read_one_voc <- function(voc) {
		
	p <- ifelse(grepl("github:", voc), vocabulary_path(voc), voc)

	ff <- list.files(file.path(p, "variables"), pattern=paste0("^variables_.*\\.csv$"), full.names=TRUE)
	gg <- gsub("^variables_|\\.csv$", "", basename(ff))
	v <- lapply(1:length(ff), \(i) data.frame(group=gg[i], utils::read.csv(ff[i])))
	if (length(ff) > 0) {
		reqs <- c("name", "type", "vocabulary", "valid_min", "valid_max")
		has_names(v, reqs)
		# "NAok", "multiple_allowed", "required", 
	}
	v <- do.call(dplyr::bind_rows, v)

	ff <- list.files(file.path(p, "values"), pattern=paste0("^values_.*\\.csv$"), full.names=TRUE)
	values <- lapply(ff, utils::read.csv)
	names(values) <- gsub("^values_|\\.csv$", "", basename(ff))
	if (length(ff) > 0) {
		reqs <- c("name")
		has_names(values, reqs)
	}
	list(variables=v, values=values)
}


read_vocabulary <- function() {

	vocs <- get_vocabulary()
	if (length(vocs) == 1) {
		return(read_one_voc(vocs))
	}
	v <- lapply(vocs, read_one_voc)
	out <- v[[1]]
	
	for (i in 2:length(v)) {
		if (!is.null(v[[i]]$variables)) {
			out$variables <- dplyr::bind_rows(out$variables, v[[i]]$variables)
		}
		vnm <- names(v[[i]]$values)
		if (length(vnm) < 1) next
		outnm <- names(out$values)
		k <- vnm %in% outnm
		if (any(k)) {
			for (j in which(k)) {
				h <- match(vnm[j], outnm)
				out$values[[h]] <- dplyr::bind_rows(out$values[[h]], v[[i]]$values[[j]])
			}
		}
		if (any(!k)) {
			out$values <- c(out$values, v[[i]]$values[!k])
		}
	}
	out
}


clone_github <- function(name, path) {
	fgz <- tempfile()
	url <- paste0("https://api.github.com/repos/", name, "/tarball/HEAD")
	utils::download.file(url, fgz, mode="wb", quiet = TRUE)
	dzip <- tempfile()
	utils::untar(fgz, exdir=dzip)
	ff <- list.files(dzip, recursive=TRUE, full.names=TRUE)
	relff <- list.files(dzip, recursive=TRUE)
	rem <- strsplit(relff[1], "/")[[1]][1]
	outf <- file.path(path, name, gsub(rem, "", relff))
	outd <- unique(dirname(outf))
	for (d in outd) dir.create(d, FALSE, TRUE)
	exf <- list.files(path, recursive=TRUE, full.names=TRUE)
	file.remove(exf)	
	all(file.copy(ff, outf))
}


is_up2date <- function(gsha, gvoc, delay) {
	pvoc <- vocabulary_path(gvoc)
	f <- file.path(pvoc, "sha.txt")
	if (file.exists(f)) {
		rsha <- readLines(f)
		if (gsha == rsha) {
			return(TRUE)
		}		
	}
	return(FALSE)
}


github_sha <- function(voc) {
	burl <- file.path("https://api.github.com/repos", voc)
	# use GET instead to make sure it exists
	# and to exit if no internet
	v <- try(readLines(file.path(burl, "commits/main")))
	if (inherits(v, "try-error")) {
		return(NA)
	}
	jsonlite::fromJSON(v)$sha
}


check_installed <- function(voc) {
	if (length(voc) < 1) return(NA)
	out <- rep(NA, length(voc))
	for (i in 1:length(voc)) {
		if (!grepl("^github:", voc[i])) {
			out[i] <- TRUE
			next
		}
		pvoc <- vocabulary_path(voc[i])
		f <- file.path(pvoc, "sha.txt")
		if (!file.exists(f)) {
			message(paste("installing", voc[i])); utils::flush.console()
			p <- vocabulary_path("github:")
			v <- gsub("^github:", "", voc[i])
			gsha <- github_sha(v)
			updated <- try(clone_github(v, p))
			if (isTRUE(updated)) {
				writeLines(gsha, file.path(pvoc, "sha.txt"))
				.vocal_environment$checked <- TRUE
				out[i] <- TRUE
			}
		} else {
			out[i] <- TRUE		
		}
	}
	out
}



check_one_vocabulary <- function(gvoc, update, force, quiet, delay=0) {

		if (!grepl("^github:", gvoc)) {
			return(FALSE)
		} 

		voc <- gsub("^github:", "", gvoc)
		pth <- vocabulary_path(gvoc)

# check/update delay
		f <- file.path(pth, "sha.txt")
		if (file.exists(f)) {
			info <- file.info(f)
			hrs <- as.double(Sys.time() - info$mtime, units = "hours")
			if (hrs < delay) {
				return(FALSE)
			}
		}
		
		gsha <- github_sha(voc)
		if (is.na(gsha)) return(NA)
		
		up2d <- try(is_up2date(gsha, gvoc, delay))
		if (inherits(up2d, "try-error")) {
			if (!quiet) message("cannot update vocabulary")
			return(NA)		
		}
		if (up2d) {
			if (!quiet) message("vocabulary is up-to-date")
			return(FALSE)
		}
		if (!update) {
			if (!quiet) message("the vocabulary is not up-to-date")
			return(FALSE)	
		}
		if (!quiet) message("checking for updated vocabulary")
		if (!quiet) message(paste("updating", voc, "to version", gsha)); utils::flush.console()
		updated <- try(clone_github(voc, vocabulary_path("github:")))
		if (isTRUE(updated)) {
			writeLines(gsha, file.path(pth, "sha.txt"))	
			result <- TRUE
		} else {
			if (!quiet) message("update failed"); utils::flush.console()
			result <- NA
		}
		result
}


check_vocabulary <- function(update=TRUE, force=FALSE, delay=0, quiet=FALSE) {

	if ((!force) && isTRUE(.vocal_environment$checked)) {
		return(TRUE)
	}
	voc <- get_vocabulary()
	out <- rep(FALSE, length(voc))
	for (i in 1:length(voc)) {
		out[i] <- check_one_vocabulary(voc[i], update=update, force=force, delay=delay, quiet=quiet)
	}
	if (any(is.na(out))) {
		warning(paste("could not check for update:", paste(voc[is.na(out)], collapse="; ")))
		FALSE
	}
	if (isTRUE(any(out, na.rm=TRUE))) {
		d <- try(read_vocabulary())
		if (!inherits(d, "try-error")) {
			.vocal_environment$voc <- d
			.vocal_environment$read <- TRUE
		}
	}
	.vocal_environment$checked <- TRUE
	!any(is.na(out))
}


obsolete_add_local <- function(voc, local_terms=NULL) {
 
	if (is.null(local_terms)) return()
	
	voc_path <- vocabulary_path(voc)
   	lf <- list.files(local_terms, recursive = TRUE) 
	if (length(lf) > 0) {
	   	pf <- list.files(voc_path, recursive = TRUE)
		for (i in 1:length(lf)) {
		  if (basename(lf[i]) %in% basename(pf)) {
		    v1 <- utils::read.csv(file.path(voc_path, pf[grepl(basename(lf[i]), pf)]))
		    if (!is.null(v1$local)) {
				v1 <- v1[!v1$local, ]
			}
			v1$local <- FALSE
			v2 <- utils::read.csv(file.path(local_terms, lf[i]))
			v2$local <- TRUE
		    v <- NULL
		    v <- try(rbind(v1, v2))
		    if (!is.null(v)) {
				d <- duplicated(v$name)	
				if (any(d)) {
					warning(paste("removing duplicate names:", paste0(d, collapse=", ")))
					v <- v[!d, ]
				}
				utils::write.csv(v, file.path(voc_path, lf[i]), row.names=FALSE)
			}
		  } else {
		    nt <- file.path(local_terms, lf[i])
		    ot <- file.path(voc_path, lf[i])
		    file.copy(nt, ot, overwrite=TRUE)
		  }
		}
	}
}



