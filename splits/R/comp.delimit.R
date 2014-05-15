`comp.delimit` <- function(obj, alt, method="EM") {
	gsp <- spec.list(obj)

	if (nrow(alt) != nrow(gsp)) {
		print ("Numbers of samples in tables do not match.")
		return (NA)
	}
	
	gsp <- gsp[match(alt[,2], gsp[,2]),]

	if (!all(gsp[,2] == alt[,2])) {
		print ("Sample names do not match.")
		return (NA)
	}

	mutual.info <- function(x, y) {
		if (length(x) != length(y)) {
			return (NA)
		}
	
		N <- length(x) 
		I <- 0.0

		eps = .Machine$double.eps 
		for (l1 in unique(x)) {
			for (l2 in unique(y)) {
				l1_ids <- which(x == l1)
				l2_ids <- which(y == l2)

				pxy <- length(intersect(l1_ids, l2_ids))/N+eps
				I <- I + pxy*log(pxy/((length(l1_ids)/N)*(length(l2_ids)/N)), base=2)
			}
		}
		return (I)
	}

	nmi <- function(x, y) {
		if (length(x) != length(y)) {
			return (NA)
		}
	
		N <- length(x) 
		I <- mutual.info(x, y)

		Hx <- 0
		for (l1 in unique(x)) {
			l1_count <- length(which(x == l1))
			px <- l1_count/N 
			Hx <- Hx - px*log(px, base=2)
		}
		Hy <- 0
		for (l2 in unique(y)) {
			l2_count <- length(which(y == l2))
			py <- l2_count/N
			Hy <- Hy - py*log(py, base=2)
		}
	
		if (Hx+Hy == 0) {
			return (1.0)
		} else {
			return (2*I/(Hx+Hy))
		}
	}

	exact.match <- function(x, y) {	#x is "true" partitioning
		if (length(x) != length(y)) {
			return (NA)
		}

		match <- 0

		for (l1 in unique(x)) {
			for (l2 in unique(y)) {
				l1_ids <- sort(which(x == l1))
				l2_ids <- sort(which(y == l2))
			
				if (length(l1_ids) == length(l2_ids) && all(l1_ids == l2_ids)) {
					match <- match + 1
				}
			}
		}
		return (match)
		#return (match/length(unique(x)))
	}			


	if (method=="EM") {
		return (exact.match(alt[,1], gsp[,1]))
	} else if (method=="NMI") {
		return (nmi(alt[,1], gsp[,1]))
	}

}


