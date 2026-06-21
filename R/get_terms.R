

accepted_variables <- function(include=NULL, exclude=NULL) {
	if (!isTRUE(.vocal_environment$read)) {
		stop("no vocabulary data")
	}
	v <- .vocal_environment$voc$variables
	if (!is.null(include)) {
		v <- v[v$group %in% include, ]
	}
	if (!is.null(exclude)) {
		v <- v[!(v$group %in% exclude), ]
	}
	v
}

accepted_values <- function(name) {
	if (!isTRUE(.vocal_environment$read)) {
		stop("no vocabulary data")
	}
	if (missing(name)) {
		stop("provde a variable name")
	}
	name <- name[1]
	if (!(name %in% names(.vocal_environment$voc$values))) {
		if (!(name %in% names(.vocal_environment$voc$variables$name))) {
			stop(paste(name, "is not a variable in this vocabuarly")	)
		} else {
			stop(paste(name, "does not have accepted values")	)
		}
	}
	.vocal_environment$voc$values[[name]]
}




.get_variables <- function(group, path) {
	f <- file.path(path, "variables", paste0("variables_", group, ".csv"))		
	if (file.exists(f)) {
		data.frame(group=group, utils::read.csv(f)	)
	} else {
		warning(paste(group, " variables do not exist"))
		NULL
	}
}


.get_variable_group_names <- function(path) {
	gsub("^variables_|\\.csv$", "", list.files(file.path(path, "variables"), pattern="variables_.*.\\.csv$"))
}



# .old.accepted_variables <- function(include=NULL) {
	# voc <- get_vocabulary()
	# p <- ifelse(grepl("github:", voc), vocabulary_path(voc), voc)
	# p <- vocabulary_path(voc)
	# if (is.null(include)) {
		# include <- get_variable_group_names(p)
		# include <- gsub("variables_|\\.csv$", "", include)
	# }
	# v <- lapply(include, function(inc) get_variables(inc, p))
	# return(do.call(rbind, v))	
# }


# .old.accepted_values <- function(name) {

	# voc <- get_vocabulary()
	# p <- ifelse(grepl("github:", voc), vocabulary_path(voc), voc)
	# f <- file.path(p, "values", paste0("values_", name, ".csv"))
	# if (file.exists(f)) {
		# utils::read.csv(f)
	# } else {
		# NULL
	# }
# }

