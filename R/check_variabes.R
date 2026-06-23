

.var_name_key <- function(s) {
	gsub("_", "", tolower(trimws(s)))
}


.common_prefix_len <- function(a, b) {
	a <- .var_name_key(a)
	b <- .var_name_key(b)
	n <- min(nchar(a), nchar(b))
	if (n == 0L) return(0L)
	for (k in seq_len(n)) {
		if (substr(a, k, k) != substr(b, k, k)) return(k - 1L)
	}
	n
}


.common_suffix_len <- function(a, b) {
	a <- .var_name_key(a)
	b <- .var_name_key(b)
	na <- nchar(a)
	nb <- nchar(b)
	n <- min(na, nb)
	if (n == 0L) return(0L)
	for (k in seq_len(n)) {
		if (substr(a, na - k + 1L, na - k + 1L) !=
			substr(b, nb - k + 1L, nb - k + 1L)) return(k - 1L)
	}
	n
}


.term_matches_start_or_end <- function(unknown, known) {
	if (!nzchar(unknown)) return(FALSE)
	pairs <- list(
		list(u = unknown, k = known),
		list(u = .var_name_key(unknown), k = .var_name_key(known))
	)
	for (p in pairs) {
		u <- p$u
		k <- p$k
		nu <- nchar(u)
		nk <- nchar(k)
		if (nu == 0L || nk == 0L) next
		lu <- tolower(u)
		lk <- tolower(k)
		if (nu <= nk) {
			if (substr(lk, 1L, nu) == lu) return(TRUE)
			if (substr(lk, nk - nu + 1L, nk) == lu) return(TRUE)
		}
		if (nk <= nu) {
			if (substr(lu, 1L, nk) == lk) return(TRUE)
			if (substr(lu, nu - nk + 1L, nu) == lk) return(TRUE)
		}
	}
	FALSE
}


suggest_term_name <- function(name, terms, max_dist = 2, max_rel = 0.25,
	min_len_ratio = 0.65, min_prefix = 3) {
	if (!length(terms)) return(NULL)
	name <- trimws(name)
	terms <- unique(trimws(terms))
	if (name %in% terms) return(NULL)

	nk <- .var_name_key(name)
	nt <- .var_name_key(terms)
	d1 <- utils::adist(name, terms, ignore.case = TRUE)
	d2 <- if (identical(nk, tolower(name))) d1 else utils::adist(nk, nt)

	best <- NULL
	best_score <- Inf
	best_dist <- Inf
	for (j in seq_along(terms)) {
		cand <- terms[j]
		if (identical(cand, name)) next
		nc <- nt[j]
		ln <- max(nchar(nk), nchar(nc), 1L)
		affix <- .term_matches_start_or_end(name, cand)
		dist <- min(d1[j], d2[j])

		if (affix) {
			score <- ln - min(nchar(nk), nchar(nc))
		} else {
			if (min(nchar(nk), nchar(nc)) / ln < min_len_ratio) next
			overlap <- max(
				.common_prefix_len(name, cand),
				.common_suffix_len(name, cand)
			)
			if (overlap < min(min_prefix, min(nchar(nk), nchar(nc)))) next
			rel <- dist / ln
			if (dist > max_dist && rel > max_rel) next
			score <- 1000L + dist
		}

		if (score < best_score || (score == best_score && dist < best_dist)) {
			best_score <- score
			best_dist <- dist
			best <- cand
		}
	}
	best
}


check_known <- function(x, trms, suggest=FALSE) {
	answ <- data.frame(check="", msg="")[0,]
	nms <- names(x)
	xnms <- nms[!(nms %in% trms$name)]
	if (length(xnms) > 0) {
		if (isTRUE(suggest)) {
			suggested <- character()
			unsuggested <- character()
			for (u in xnms) {
				sug <- suggest_term_name(u, trms$name)
				if (!is.null(sug)) {
					suggested <- c(suggested, paste0(u, " (", sug, "?)"))
				} else {
					unsuggested <- c(unsuggested, u)
				}
			}
			if (length(suggested) > 0) {
				answ <- data.frame(
					check = "unknown variable",
					msg = suggested,
					stringsAsFactors = FALSE
				)
			}
			if (length(unsuggested) > 0) {
				row <- data.frame(
					check = "unknown variables",
					msg = paste(unsuggested, collapse = ", "),
					stringsAsFactors = FALSE
				)
				answ <- if (nrow(answ)) rbind(answ, row) else row
			}
		} else {
			answ <- data.frame(
				check = "unknown variables",
				msg = paste(xnms, collapse = ", "),
				stringsAsFactors = FALSE
			)
		}
	}
	answ
}


check_required <- function(x, trms, type="") {
	answ <- data.frame(check="", msg="")[0,]
	if (is.null(trms$required)) return(answ)
	req <- trms[which(trms$required == "yes"), "name"]  #| trms$required == group
	r <- req[!(req %in% names(x))]
	if (length(r) > 0) {
		if (type != "") type <- paste0(type, ": ")
		answ[nrow(answ)+1, ] <- c("missing variables", paste0(type, paste(r, collapse=", ")))
	}
	answ
}

check_dups <- function(x) {
	answ <- data.frame(check="", msg="")[0,]
	nms <- names(x)
	tnms <- table(nms)
	if (any(tnms>1)) {
		tnms <- paste(tnms[tnms>1], collapse=", ")
		answ[nrow(answ)+1, ] <- c("duplicate variables", tnms)		
	}
	answ
}

check_variables <- function(x, trms, required=TRUE, suggest=TRUE) {
	known <- check_known(x, trms, suggest=suggest)
	dups <- check_dups(x)
	if (required) {
		rbind(known, check_required(x, trms), dups)
	} else {
		rbind(known, dups)
	}
}
