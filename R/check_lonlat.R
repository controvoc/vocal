
check_lonlat <- function(x) {

	answ <- data.frame(check="", msg="")[0,]

	if (!all(c("longitude", "latitude") %in% colnames(x))) {
		answ[nrow(answ)+1, ] <- c("location", paste0("variables 'longitude' and/or 'latitude' are missing"))
		return(answ)
	}
	if (!("country" %in% colnames(x))) {
		answ[nrow(answ)+1, ] <- c("location", paste0("variable 'country' is missing"))
		return(answ)
	}

#	path <- system.file(package="carobiner")
	p <- file.path(rappdirs::user_data_dir(), ".carob")
	fbw <- file.path(p, "gadm", "gadm_buf.rds")
	if (!file.exists(fbw)) {
		w <- geodata::world(path=p, res=1)
		w <- terra::buffer(w, 2500)
		print("preparing gadm")
		terra::saveRDS(w, fbw)
	} else {
		w <- terra::readRDS(fbw)
	}
	
	xy <- unique(stats::na.omit(x[, c("country", "longitude", "latitude")]))
	if (nrow(xy) > 0) {
		e <- terra::extract(w, xy[, c("longitude", "latitude")])
		if (!is.null(e) && nrow(e) > 0) {
			e$country <- xy$country[e$id.y]
			e$match <- e$NAME_0 == e$country
			a <- aggregate(e["match"], e[,c("id.y", "country")], max)

			i <- is.na(a$match)
			if (any(i)) {
				u <- unique(a$country[i])
				bad <- paste(u, collapse=", ")
				answ[nrow(answ)+1, ] <- c("coordinates not on land", bad)
			}
			i <- which(!is.na(a$match) & a$match == 0)
			if (length(i) > 0) {
				b <- e[e$id.y %in% a$id.y[i], c("id.y", "NAME_0", "country")]
				b <- reshape(transform(b, time = ave(id.y, id.y, FUN = seq_along)),
					idvar = c("id.y", "country"), timevar = "time", direction = "wide")[,-1]
				b <- cbind(b[,1], apply(b[,-1,drop=FALSE], 1, \(x) paste(x[!is.na(x)], collapse=" or ")))
				u <- apply(b, 1, paste, collapse="/")
				bad <- paste(u, collapse=", ")
				answ[nrow(answ)+1, ] <- c("coordinates in wrong country", bad)
			}
		}
	}
	
	locvars <- c("country", paste0("adm", 1:5), "location", "site", "longitude", "latitude")
	i <- which(locvars %in% colnames(x))
	locs <- unique(x[, locvars[i]])
	xy <- unique(x[, c("longitude", "latitude")])
	if (nrow(xy) < (0.9 * nrow(locs))) {
		answ[nrow(answ)+1, ] <- c("duplicate coordinates", "fewer coordinates than locations")
	}
	return(answ)		
}

