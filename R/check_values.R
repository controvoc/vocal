
check_missingvalues <- function(x, trms) {
	answ <- data.frame(check="", msg="")[0,]
	nms <- trms$name[which(trms$NAok == "no")]
	nms <- nms[nms %in% names(x)]
	x <- is.na(x[,nms,drop=FALSE])
	a <- apply(x, 2, all)
	if (any(a)) {
		answ[nrow(answ)+1, ] <- c("all NA", paste(nms[a], collapse=", "))
		nms <- nms[!a]
		x <- x[, !a, drop=FALSE]
		if (ncol(x) == 0) {
			return(answ)
		}
	}

	a <- apply(x, 2, any)
	if (any(a)) {
		answ[nrow(answ)+1, ] <- c("NA values", paste(nms[a], collapse=", "))
	}
	answ
}


check_empty <- function(x, answ) {
	bad1 <- bad2 <- rep(FALSE, ncol(x))
	chars <- sapply(x, is.character)
	for (i in which(chars)) {
		j <- nchar(x[,i])
		x[,i] <- trimws(x[,i])
		bad1[i] <- isTRUE(any(stats::na.omit(x[,i]) == ""))
		bad2[i] <- isTRUE(any(stats::na.omit(j > nchar(x[,i]))))
		
	}
	if (any(bad1)) {
		b <- paste0(colnames(x)[bad1], collapse= ", ")
		answ[nrow(answ)+1, ] <- c("empty character values", b)
	}
	if (any(bad2)) {
		b <- paste0(colnames(x)[bad2], collapse= ", ")
		answ[nrow(answ)+1, ] <- c("untrimmed characters", b)
	}
	answ
}


check_ranges <- function(x, trms, answ) {
	nms <- colnames(x)
	trms <- trms[stats::na.omit(match(nms, trms$name)), ]
	trms <- trms[trms$type == "numeric",]
	
	bad <- NULL
	trms <- stats::na.omit(trms[, c("name", "valid_min", "valid_max"), ])
	if (nrow(trms) == 0) return(answ)
	for (i in 1:nrow(trms)) {
		rng <- unlist(trms[i,c("valid_min", "valid_max")])
		v <- stats::na.omit(x[[trms$name[i]]])
 		if ( any((v < rng[[1]]) | (v > rng[2])) ) {
			ok <- FALSE
			vrng <- range(v, na.rm=TRUE)
			if (is.numeric(vrng)) vrng <- round(vrng, 3)
			msg  <- paste0(trms$name[i], " (", vrng[1], ", ", vrng[2], ")")
			bad  <- c(bad, msg)
		}
	}
	if (!is.null(bad)) {
		answ[nrow(answ)+1, ] <- c("out of bounds", paste(bad, collapse=", "))
		bad <- NULL
	}
	
	if (!is.null(bad)) {
		bad <- paste(bad, collapse=", ")
		answ[nrow(answ)+1, ] <- c("invalid", paste0("invalid: ", bad))
		bad <- NULL
	}

	answ
}


check_type_range <- function(x, trms, answ) {
	nms <- colnames(x)
	i <- match(nms, trms$name)
	x <- x[, !is.na(i), drop=FALSE]
	if (ncol(x) == 0) return(answ)
	nms <- colnames(x)
	trs <- trms[match(nms, trms$name), ]
	skip <- vapply(x, function(v) all(is.na(v)), logical(1))
	if (all(skip)) return(answ)
	if (any(skip)) {
		x <- x[, !skip, drop=FALSE]
		nms <- colnames(x)
		trs <- trs[!skip, , drop=FALSE]
	}
	cls <- vapply(x, function(v) {
		cl <- class(v)
		if (length(cl) > 1 && any(cl %in% c("POSIXct", "POSIXt", "Date"))) {
			"date"
		} else {
			cl[[1]]
		}
	}, character(1))
	cls <- cbind(cls, trs$type, nms)
	cls <- cls[cls[,2] != "", , drop=FALSE]
	if (NROW(cls) == 0) return(check_ranges(x, trs, answ))
	i <- (cls[,1] == "character") & (cls[,2] == "date")
	cls[i, 2] <- "character"
	i <- (cls[,1] == "integer") & (cls[,2] == "numeric")
	cls[i, 1] <- "numeric"
	i <- cls[,1] != cls[,2]
	if (isTRUE(any(i))) {
		bad <- paste(paste0(cls[i, 3], " (", cls[i, 1], ", not ", cls[i, 2], ")"), collapse=", ")
		answ[nrow(answ)+1, ] <- c("bad datatype", bad)
		trs <- trs[!i, ]
	} 
	check_ranges(x, trs, answ)
}

check_accepted <- function(x, trms, answ) {

	trms <- trms[which(trms$vocabulary != ""), ]
	trms <- trms[stats::na.omit(match(names(x), trms$name)), ]
	if (nrow(trms) == 0) return(answ)
	
	for (i in 1:nrow(trms)) {
		accepted <- accepted_values(trms$vocabulary[i])[,1]
		provided <- unique(x[, trms$name[i]])
		if (!is.null(trms$required)) {
			if (!isTRUE(trms$required[i] == "no")) {
				provided <- stats::na.omit(provided)
			} 
		}
		if (length(provided) > 0) {
			if (!is.null(trms$multiple_allowed)) {
				if (!isTRUE(trms$multiple_allowed[i] == "no")) {
					if (!is.na(provided[1])) {
						provided <- unique(unlist(strsplit(as.character(provided), ";|; ")))
					}
				}
			}
			if (trms$vocabulary[i] == "crop") {
				if (any(grepl("_", provided))) {
					provided <- unique(unlist(strsplit(as.character(provided), "_")))
				}
			}
			if (isTRUE(trms$NAok[i]=="yes")) {
				provided <- stats::na.omit(provided)
			}
			if (length(provided) > 0) {
				bad <- provided[!(provided %in% accepted)]
				if (length(bad) > 0) {
					bad <- sort(unique(bad))
					answ[nrow(answ)+1, ] <- c("invalid terms",
						paste0(trms$name[i], ": ", paste(bad, collapse=", ")))
				}
			}
		}
	}
	answ
}


check_values <- function(x, trms) {
	answ <- check_missingvalues(x, trms)
	answ <- check_empty(x, answ)
	answ <- check_type_range(x, trms, answ)
	check_accepted(x, trms, answ)
}


check_caps <- function(x, vars, minchar=5, frac=0) {
	answ <- data.frame(check="", msg="")[0,]
	stopifnot(frac >= 0 && frac <= 1)
	for (v in vars) {
		m <- unique(stats::na.omit(x[[v]]))
		if (minchar > 0) {
			m <- m[nchar(m) >= 5]
		}
		i <- sum(toupper(m) == m)
		if (i > (frac * length(m))) {
			answ[nrow(answ)+1, ] <- c("all uppercase", v)
		}
	}
	answ
}


check_span <- function(x, start, end, smin=0, smax=366) {
	answ <- data.frame(check="", msg="")[0,]

	s <- x[[start]]
	e <- x[[end]]
	if (is.null(s) || is.null(e)) {
		return(answ)
	}
	d <- (e - s)
	i <- d < smin #45
	if (any(i)) { 
		answ[nrow(answ)+1, ] <- c("span", paste(sum(i), "records with", end, "within", smin, "of", start))
	} 
	i <- d > smax #365
	if (any(i)) { 
		answ[nrow(answ)+1, ] <- c("span", paste(sum(i), "records with", end, "more than", smax, "of", start))
	} 
	answ
}

