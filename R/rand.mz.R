rand.mz <-
function(acc_mass_final){	  # randomize feature lists in acc_mass_final
	mz.1 <- NULL
	for(i in 1:length(acc_mass_final)){
		mz <- acc_mass_final[[i]]
		randmz <- sample(mz[,"mz"])
		mz.1[i] <- list(randmz)
	}
	names(mz.1) <- c("pos","neg")
	return(mz.1)		
}
