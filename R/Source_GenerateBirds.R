 #Functions that generate birds
#Generate the initial Population

#' Generate Founder Males
#'
#' Creates the population at time step 0.  Generates 1) a matrix of male syllable vectors, 2) an optional matrix of female syllable vectors, 3) a dataframe of bird features, 4) an optional locality list, and 5) structures for keeping track of bird survival if a type II survival curve is implemented.
#' @param P a list of parameters
#' @keywords initialize
#' @export
GenerateFounderMales <- function(P){
  #Generate a population of adult males to begin the simulation

  #Make males and their songs
  TerritorialMales <- list(MSongs=GenerateNovelSong(P, P$numBirds))
  TerritorialMales[["Males"]] <- GenerateAdultBirds(P, TerritorialMales$MSongs)

  #If Females choose males based on matching to their mental template, add the following
  if(P$MatPref != 0 || P$SMat == TRUE){
    TerritorialMales[["FSongs"]] <- CreateFemaleSongs(P)
    if(P$MDial == "Similar"){
      TerritorialMales$MSongs <- EstablishDialects(P, TerritorialMales$MSongs)
    }else if(P$MDial == "Same"){
      TerritorialMales$MSongs <- TerritorialMales$FSongs
    }
    #Randomly assign females or allow to pair with males who match them best
    if(P$FEvo == TRUE && P$ChoMate == TRUE  && P$MDial != "Same"){
      Assign <- AssignFemale(P, TerritorialMales$MSongs, TerritorialMales$FSongs)
      TerritorialMales$FSongs <- TerritorialMales$FSongs[Assign[[1]],]
      TerritorialMales$Males <- cbind(TerritorialMales$Males, Match = Assign[[2]])
    }else{
      TerritorialMales$Males <- cbind(TerritorialMales$Males,
                                Match = TestMatch(P, TerritorialMales$MSongs, TerritorialMales$FSongs))
    }
  }


  #Directions for any local scope strategies
  if(P$ScopeB || P$ScopeT){
    TerritorialMales[["Directions"]] <- FinalDirections(P)
  }


  #Death Parameters for type 2 survival curve
  if(P$DStrat){
    if(P$MAge == 1){
      TerritorialMales[["AgeDeathProb"]] <- P$Pc
      TerritorialMales[["AgeStore"]] <- P$Pc
    }else{
      TerritorialMales[["AgeDeathProb"]] <- c(P$Pc, rep(P$InitProp,P$MAge))
      TerritorialMales[["AgeStore"]] <- TerritorialMales$AgeDeathProb[2]
    }
  }
  return(TerritorialMales)
}


#' Generate Adult Birds
#'
#' Generates the features for each bird in the population at timestep 0.
#' @param P a list of parameters
#' @param songs a matrix of syllable vectors
#' @keywords initialize
#' @export
GenerateAdultBirds <- function(P, songs){
  #Randomly create a generation 0 bird defined within user-defined presets (P)
  Males <- data.frame(Age=InitAgeDistribution(P))  #only adults

  if(P$IAccN != 0){#PERT distribution allowing for +/- IAccN percentage deviance from Acc)
    MAX <- min(P$MaxAcc, P$Acc0+P$IAccN)
    MIN <- max(P$MinAcc, P$Acc0-P$IAccN)
    Males[,"Acc"] <- rpert(P$numBirds, MIN,P$Acc0,MAX)
  }else{Males[,"Acc"] <- rep(P$Acc0, P$numBirds)}
  if(P$ILrnN != 0){#PERT distribution allowing for +/- 12*ILrnN month deviance from LrnThsh)
    MAX <- min(P$MaxLrn, P$LrnThrsh0+P$ILrnN)
    MIN <- max(P$MinLrn, P$LrnThrsh0-P$ILrnN)
    Males[,"LrnThsh"] <- rpert(P$numBirds, MIN,P$LrnThrsh0,MAX)
  }else{Males[,"LrnThsh"] <- rep(P$LrnThrsh0, P$numBirds)}

  if(P$ICtIN != 0){#PERT distribution allowing for +/- ICtIN pecentage deviance from CtI0
    MAX <- min(P$MaxCtI, P$CtI0+P$ICtIN)
    MIN <- max(P$MinCtI, P$CtI0-P$ICtIN)
    Males[,"ChanceInv"] <- rpert(P$numBirds, MIN,P$CtI0,MAX)
  }else{Males[,"ChanceInv"] <- rep(P$CtI0, P$numBirds)}

  if(P$ICtFN != 0){#PERT distribution allowing for +/- ICtFN percentage deviance from CtF0
    MAX <- min(P$MaxCtF, P$CtF0+P$ICtFN)
    MIN <- max(P$MinCtF, P$CtF0-P$ICtFN)
    Males[,"ChanceFor"] <- rpert(P$numBirds, MIN,P$CtF0,MAX)
  }else{Males[,"ChanceFor"] <- rep(P$CtF0, P$numBirds)}

  #calc syl rep based on previously generated songs
  Males[,"SylRep"] <- rowSums(songs)

  if(P$SNam == TRUE){#generate names if required
    Males[,"FatherName"] <- rep(0, P$numBirds)
    Males[,"Name"] <- sapply(rep(TRUE,P$numBirds),UUIDgenerate)
  }
  return(Males)
}

#Generate chicks for subsequent generations


#' Generate Chicks
#'
#' Creates chicks that are similar to their fathers.
#' @param P a list of parameters
#' @param fatherInd the index of the fathers
#' @param territorialMales the population
#' @param vacancy the index of future chicks (aligned with fathers)
#' @keywords birth
#' @export
GenerateChicks <- function(P, fatherInd, territorialMales, vacancy){
  #Create chicks that Inherit paternal values with some
  #parameterized random noise introduced
  Fathers <- territorialMales$Males[fatherInd,]
  MSongs <- territorialMales$MSongs[fatherInd,]

  #newborn chick!
  Males <- data.frame(Age=rep(0, length(vacancy)))

  if(P$IAccN != 0){#Distribution around Father's accuracy based on IAccN
    #If these boundaries go beyond the absolute max or min, reset to the Absolute min or Max
    MAX <- sapply(Fathers$Acc, function(Acc) min(P$MaxAcc, Acc+P$IAccN))
    MIN <- sapply(Fathers$Acc, function(Acc) max(P$MinAcc, Acc-P$IAccN))
    Males[,"Acc"] <- rpert(length(vacancy), MIN, Fathers$Acc, MAX)
  }else{Males[,"Acc"] <- Fathers$Acc}

  if(P$ILrnN != 0){#Distribution around Father's learning threshold based on ILrnN
    MAX <- sapply(Fathers$LrnThsh, function(Lrn) min(P$MaxLrn, Lrn+P$ILrnN))
    MIN <- sapply(Fathers$LrnThsh, function(Lrn) max(P$MinLrn, Lrn-P$ILrnN))
    Males[,"LrnThsh"] <- rpert(length(vacancy), MIN, Fathers$LrnThsh, MAX)
  }else{Males[,"LrnThsh"] <- Fathers$LrnThsh}

  if(P$ICtIN != 0){#Distribution around Father's ChanceInv based on ICtIN
    MAX <- sapply(Fathers$ChanceInv, function(CtI) min(P$MaxCtI, CtI+P$ICtIN))
    MIN <- sapply(Fathers$ChanceInv, function(CtI) max(P$MinCtI, CtI-P$ICtIN))
    Males[,"ChanceInv"] <- rpert(length(vacancy), MIN, Fathers$ChanceInv, MAX)
  }else{Males[,"ChanceInv"] <- Fathers$ChanceInv}

  if(P$ICtFN != 0){#Distribution around Father's ChanceFor based on ICtFN
    MAX <- sapply(Fathers$ChanceFor, function(CtF) min(P$MaxCtF, CtF+P$ICtFN))
    MIN <- sapply(Fathers$ChanceFor, function(CtF) max(P$MinCtF, CtF-P$ICtFN))
    Males[,"ChanceFor"] <- rpert(length(vacancy), MIN, Fathers$ChanceFor, MAX)
  }else{Males[,"ChanceFor"] <- Fathers$ChanceFor}

  #Song inherited from father based on chick accuracy
  if(P$Vert){
    InherSong <- VerticalSongLearning(P, MSongs, Males)
  }else{
    InherSong <- matrix(0, nrow=length(fatherInd), ncol=P$MaxRSize)
  }
  Males[,"SylRep"] <- rowSums(InherSong)

  #Generate names if required
  if(P$SNam == TRUE){
    Males[,"FatherName"] <- paste0(Fathers$Name)
    Males[,"name"] <- sapply(rep(TRUE,length(vacancy)),UUIDgenerate)
  }
  #Generate Match if required
  if(P$MatPref != 0 || P$SMat == TRUE){
    Males[,"Match"] <- TestMatch(P, InherSong,territorialMales$FSongs[vacancy,])
  }
  Chicks <- list(Males=Males, MSongs=InherSong)

  return(Chicks)
}
