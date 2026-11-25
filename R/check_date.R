
check_date <- function(x, name, trms=NULL) {

	answ <- data.frame(check="", msg="")[0,]

	x <- stats::na.omit(x[[name]])
	if (length(x) == 0) return(answ)
	if (any(grepl(";", x))) {
		if (!is.null(trms$name)) {
			i <- match(name, trms$name)
			if (length(i) == 1) {
				if (isTRUE(trms$multiple_allowed[i] != "yes")) {
					answ[nrow(answ)+1, ] <- c("date", paste0("multiple dates in: ", name))
				}
			}
		}
		x <- unlist(strsplit(x, ";|; "))
	}
	
	n <- nchar(x)
	if (any(!(n %in% c(4, 7, 10)))) {
		answ[nrow(answ)+1, ] <- c("invalid date format", name)
	}
	today <- as.Date(Sys.time())
	ymd <- stats::na.omit(x[n==10])
#	if (any(is.na(ymd))) {
#		return(FALSE)
#	}
	if (length(ymd) > 0) {
		i <- grepl("/", ymd)
		if (any(i)) {
			answ[nrow(answ)+1, ] <- c("date", paste0("found '/' signs in date(s) in: ", name))
			ymd <- ymd[!i]
		}
		if (length(ymd) > 0) {
			d <- try(as.Date(ymd, format="%Y-%m-%d"), silent=TRUE)
			if (inherits(d, "try-error")) {
				answ[nrow(answ)+1, ] <- c("date", paste0("invalid date(s) in: ", name))
				d <- as.Date(character(0))
			} else if (any(is.na(d))) {
				answ[nrow(answ)+1, ] <- c("date", paste0("invalid date(s) in: ", name))
				d <- stats::na.omit(d)
			}
			if (length(d) > 0) {
				if (isTRUE(any(d < as.Date("1950-01-01"), na.rm=TRUE))) {
					answ[nrow(answ)+1, ] <- c("date", paste0("date(s) before 1950 in: ", name))
				}
				if (isTRUE(any(d > today, na.rm=TRUE))) {
					answ[nrow(answ)+1, ] <- c("date", paste0("future date(s) in: ", name))
				}
			}
		}
	}
	thisyear <- as.numeric(format(today, "%Y"))
	ym <- stats::na.omit(x[n==7])
	if (length(ym) > 0) {
		bad <- !grepl("^[0-9]{4}-[0-9]{2}$", ym)
		if (any(bad)) {
			answ[nrow(answ)+1, ] <- c("date", paste0("bad date(s) in: ", name))
			ym <- ym[!bad]
		}
		if (length(ym) > 0) {
			y <- as.numeric(substr(ym, 1, 4))
			if (any(y < 1960)) {
				answ[nrow(answ)+1, ] <- c("date", paste0("date(s) before 1960 in: ", name))
			} 
			if (any(y > thisyear)) {
				answ[nrow(answ)+1, ] <- c("date", paste0("date(s) after ", thisyear, " in: ", name))
			}

			m <- as.numeric(substr(ym, 6, 7))
			if (any((m < 1) | (m > 12))) {
				answ[nrow(answ)+1, ] <- c("date", paste0("months not between 1 and 12): ", name))
			} 
		}
	}

	y <- stats::na.omit(x[n==4])
	if (length(y) > 0) {
		bad <- !grepl("^[0-9]{4}$", y)
		if (any(bad)) {
			answ[nrow(answ)+1, ] <- c("date", paste0("bad date(s) in: ", name))
			y <- y[!bad]
		}
		if (length(y) > 0) {
			y <- as.numeric(y)
			if (any(y < 1960)) {
				answ[nrow(answ)+1, ] <- c("date", paste0("date(s) before 1960 in: ", name))
			} 
			if (any(y > thisyear)) {
				answ[nrow(answ)+1, ] <- c("date", paste0("date(s) after ", thisyear, " in: ", name))
			}
		}
	}
	answ
}


check_datespan <- function(x, startdate, enddate, smin=0, smax=366) {

	answ <- data.frame(check="", msg="")[0,]

	dstart <- x[[startdate]]
	dend <- x[[enddate]]
	if (is.null(dstart) || is.null(dend)) {
		return(answ)
	}
	i <- which((nchar(dstart) == 10) & (nchar(dend) == 10))
	if (length(i) == 0) return(NULL)
	s <- try(as.Date(dstart[i], format="%Y-%m-%d"), silent=TRUE)
	e <- try(as.Date(dend[i], format="%Y-%m-%d"), silent=TRUE)
	if (inherits(s, "try-error") || inherits(e, "try-error")) {
		return(answ)
	}
	d <- as.numeric(e - s)
	i <- !is.na(d) & (d < smin) #45
	if (any(i)) { 
		answ[nrow(answ)+1, ] <- c("datespan", paste(sum(i), "records with", enddate, "within", smin, "days of", startdate))
	} 
	i <- !is.na(d) & (d > smax) #365
	if (any(i)) { 
		answ[nrow(answ)+1, ] <- c("datespan", paste(sum(i), enddate, "more than", smax, "days after", startdate))
	} 
	answ
}

