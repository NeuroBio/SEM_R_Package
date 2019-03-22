#Bird Song Characteristics

#' Generate Novel Song
#'
#'Generates one or more song templates based on parameters in P; tests of inheriting [RSize0] syllables (90\%), [RSize0]*[PerROh] syllables (10\%), and [RSize0]*[PerROh] syllables (1\%), by random sampling.  Creates a numeric vector where learned syllables are 1, and unlearned syllables are 0.  It then appends 0s to the end, so the vector is of length [MaxRSize].
#' @param P a list of parameters
#' @param numTemplates the number of song templates to create
#' @keywords song-template
#' @export
GenerateNovelSong <- function(P, numTemplates){
  #generares songs for the population based on a likely initial rep size;
  #figures out the probability of inheriting the syllables, randomly samples the chance
  #to do so and makes the logical into numeric.  Then append on 0s to make max repsize.

  SylProb <- c(rep(.9, P$RSize0),rep(.1, P$RSize0*P$PerROh),rep(.01,P$RSize0*P$PerROh))
  Chances <- runif(length(SylProb)*numTemplates)
  SongCore <- matrix(as.numeric(Chances < SylProb), nrow=numTemplates, byrow=TRUE)
  FullSong <- cbind(SongCore, matrix(0, ncol=P$MaxRSize-length(SylProb), nrow=numTemplates))
  return(FullSong)
}


#' Assign Female
#'
#'Assigns females to males based on template matching.  Females are randomly chosen to pick a male from the population.  Males that better match her template are more likely to be chosen.  Assumes that there are as many females as there are males, and all birds are paired in the end.
#' @param P a list of parameters
#' @param maleSong a matrix of syllable vectors
#' @param femaleSong a matrix of syllable vectors
#' @keywords female-choice
#' @export
AssignFemale <- function(P, maleSong, femaleSong){
  #Females pick mates with probability mased on how well each male
  #matches her individual template
  Order <- numeric(length=nrow(femaleSong))
  Match <- Order
  Available <- 1:nrow(maleSong)

  for(i in 1:(nrow(femaleSong)-1)){
    FSong <- matrix(femaleSong[i,], nrow=length(Available), ncol=P$MaxRSize, byrow=TRUE)
    MatchTemp <- TestMatch(P, maleSong[Available,], FSong)
    MatchModify <- MatchTemp
    WrongSong <- which(MatchTemp == 0)
    if(length(WrongSong) > 0){#otherwise not all females will get a mate
      MatchModify[WrongSong] <- .001
    }

    #load data
    Ind <- sample(length(Available), 1, prob = MatchModify)
    Order[i] <- Available[Ind]
    Match[i] <- MatchTemp[Ind]
    Available <- Available[-Ind]
  }

  #get final mated pair
  Order[length(Order)] <- Available
  Match[length(Order)] <- TestMatch(P, matrix(maleSong[Available,], nrow=1),
                                    matrix(femaleSong[length(Order),], nrow=1))
  return(list(Order, Match))
}


#' Test Match
#'
#' Calculates how well the female template matches the male template.  Mismatch is based how many syllables the female knows that the male does not (Missing) + how many more syllables does the male know than the female (Extra, min 0).  Match = 1 - Mismatch/Number of Female Syllables.
#' @param P a list of parameters
#' @param maleSong a syllable vector
#' @param femaleSong a syllable vector
#' @keywords female-choice
#' @export
TestMatch <- function(P, maleSong, femaleSong){
  #find the mismatch between the two songs.

  #get num syls a female has that a male lacks
  Missing <- integer(nrow(femaleSong))
  for(i in 1:nrow(femaleSong)){
    Missing[i]  <- sum((femaleSong[i,] == 1) & (femaleSong[i,] != maleSong[i,]))
  }

  #how long is the male song vs. the female song
  FSyls <- rowSums(femaleSong)
  Extra <- rowSums(maleSong) - FSyls
  Extra[which(Extra < 0)] <- 0

  #calc match
  if(P$MScl == 1){
    Match <- 1 - (((Extra+Missing))/FSyls)
  }else{
    stop("NOT YET READY")
  }
  Match[which(Match < 0)] <- 0
  return(Match)
}


#' Create Female Songs
#'
#' A wrapper that creates identical (uniform) or noisy female songs, and offsets their position in the syllable vector to create dialects if necessary.
#' @param P a list of parameters
#' @keywords song-template
#' @export
CreateFemaleSongs <- function(P){
  if(!P$UniMat){
    #templates with the same variation as the original male population
    FSongs <- GenerateNovelSong(P, P$numBirds)
  }else{
    #generate seed template
    FSongs <- GenerateNovelSong(P, 1)
    FSongs <- matrix(FSongs, ncol=length(FSongs), nrow=P$numBirds, byrow = TRUE)
  }
  #offset to make dialects if needed
  if(P$Dial != 1){FSongs <- EstablishDialects(P, FSongs)}
  return(FSongs)
}


#' Establish Dialects
#'
#' Modifies a matrix of syllable vectors to create dialects (regions of syllables that are separated from one another in the syllable space).  Regions are defined so that each dialect space is as square as possible.
#' @param P a list of parameters
#' @param fSongs a matrix of syllable vectors
#' @keywords song-template
#' @export
EstablishDialects <- function(P, fSongs){
  #Set up the dialects; they are derivatives of the initial song
  #offset by the initial repsize

  #get prime factorization of Dial to see how it can be best
  #fit into the matrix space
  Facts <- primeFactors(P$Dial)
  if(length(Facts) == 1){#Dial is prime
    Divisors <- c(Facts,1)
  }else{#Dial is not prime
    Divisors <- c(prod(Facts[seq(1,length(Facts),2)]),
                  prod(Facts[seq(2,length(Facts),2)]))
  }
  if(P$R < P$C){#Make sure that the larger divisor is matched with the larger dimention
    Divisors <- rev(Divisors)
  }

  #get dimentions of a dialect submatrix and make final song matrix
  SplitRow <- P$R/Divisors[1]
  SplitCol <- P$C/Divisors[2]
  k <- 1
  for(i in 1:Divisors[1]){#populate in the dialects
    for(j in 1:Divisors[2]){
      if(i==1 && j==1){next()}
      #Gets indicies that need to be assigned as a given song
      RowSet <- (SplitRow*(i-1)+1):(i*SplitRow)
      ColSet <- ((SplitCol*(j-1)+1):(j*SplitCol)-1)*P$R
      Index  <- as.numeric(sapply(RowSet,"+",ColSet))
      fSongs[Index,] <- cbind(matrix(0,nrow=length(Index),ncol=P$RSize0*k),fSongs[Index,1:(P$MaxRSize-(P$RSize0*k))])
      k <- k+1
    }
  }
  return(fSongs)
}






#Age functions

#' Calculate Adult Survival Proportion
#'
#' Calculates the proportion of adults that survive from one timestep to the next.
#' @param n the population size
#' @param t the death threshold
#' @param pc the proportion of chicks that survive
#' @param mAge the max age of the population
#' @keywords survival-curve
#' @export
CalculateProportion <- function(n=400, t=1, pc=.3, mAge=20){
  #get roots based on set values
  Coeff <- c(t-n, rep(t,mAge-1), (t/pc)+t)
  Roots <- polyroot(Coeff)
  #pull out the real root
  RealRoots <- suppressWarnings(as.numeric(Roots[which(Im(zapsmall(Roots, 12)) == 0)]))
  RealRoots <- 1/RealRoots
  Pa <- RealRoots[which(RealRoots > 0 & RealRoots < 1)]
  if(length(Pa)==0){
    stop(paste("MaxAge too large or population of birds (matrix size) too small.", P$SimStep))
  }
  #print(Pa)
  return(Pa)
}



#' Calculate All Generations
#'
#' Calculates the proportion of the population that is in each generation when the simulation starts.
#' @param pa the proportion of adults that survive to the next age
#' @param t the death threshold
#' @param pc the proportion of chicks that survive
#' @param mAge the max age of the population
#' @keywords survival-curve
#' @export
CalculateAllGen <- function(pa, pc, t, mAge){
  N0 <-t/(pc*pa^mAge)
  N1 <- N0*pc
  NNext <- N1*pa^(1:(mAge))
  return(c(N0,N1,NNext))
}


#' Initial Age Distribution
#'
#' Creates the age distribution of the population.  Either follows a type II survival curve, or uniformly samples from 1 to the max age.
#' @param P a list of parameters
#' @keywords survival-curve
#' @export
InitAgeDistribution <- function(P){
  if(P$DStrat){
    AgeRates <- GetAgeRates(P)
    AgeGroup <- GetAgeGroup(P,AgeRates)
  }else{
    AgeGroup <- sample(1:P$MAge, P$numBird,replace = TRUE)
  }
  return(AgeGroup)
}


#' Get Age Rates
#'
#' Calculates the number of birds that should be in each generation.
#' @param P a list of parameters
#' @keywords survival-curve
#' @export
GetAgeRates <- function(P){
  #calculate birds in all states
  SurvivalRates <- CalculateAllGen(P$InitProp, P$Pc, P$DeadThrsh, P$MAge)
  #convert to percentage and remove would-be dead birds
  AgeRates <- SurvivalRates[1:(length(SurvivalRates)-1)]/P$numBirds
  #Correct for lost zombies by scaling remaining precentages to 100%
  AgeRates <- AgeRates*(1/(1-(P$DeadThrsh/P$numBirds)))
  return(AgeRates)
}



#' Get Age Group
#'
#' Given the number of birds that should be in each generation, creates a vector of ages and scrambles them for random assignment.
#' @param P a list of parameters
#' @param ageRates the output from GetAgeRates()
#' @keywords survival-curve
#' @export
GetAgeGroup <- function(P, ageRates){
  #AgeUp <- runif(P$numBirds)
  AgeGroup <- list()
  #Aging process: Required
  for(i in 1:(P$MAge+1)){
    Birds <- P$numBirds*ageRates[i]
    AgeGroup[[i]] <- rep(i-1, Birds)
  }
  #Aging process: By Chance
  Drifters <- P$numBirds-length(unlist(AgeGroup))
  if(Drifters > 0){
    AgeChance <- runif(Drifters)
    ageRates <- cumsum(ageRates)
    DriftersVec <- vector("numeric",Drifters)
    for(i in 1:P$MAge){
      Index <- which(AgeChance > ageRates[i])
      DriftersVec[Index]<- i
    }
    AgeGroup[[P$MAge+2]] <- DriftersVec
  }
  return(sample(unlist(AgeGroup)))
}


#Functions for getting the local IND
#Initial generation of directions

#' Final Directions
#'
#' A wrapper that calls StepOne() and EachStep() to create lists of locality data.
#' @param P a list of parameters
#' @keywords locality
#' @export
FinalDirections <- function(P){
  #get the indicies directly connected to a cell
  DirList <- rep(list(NA),max(P$R,P$C)-(P$Steps-1))
  StepOne <- OneStepDirections(P$R, P$C)
  EachStep <- StepOne

  #Extend as many steps away as the user specified
  if(P$Steps > 1){
    for(i in 2:P$Steps){
      EachStep <- NextStepDirections(EachStep, StepOne)
    }
  }
  DirList[[1]] <- EachStep
  #####
  for(i in seq_along((P$Steps+1):(max(P$R, P$C)))){
    DirList[[i+1]] <- NextStepDirectionsMain(DirList[[i]], StepOne)
  }
  #####
  return(DirList)
}


#' One Step Directions
#'
#' Creates the location data for what is one "step" away form each territory.
#' @param R rows
#' @param C columns
#' @keywords locality
#' @export
OneStepDirections <- function(R, C){
  CellDirections <- as.list(1:(R*C))
  #Find ind in the center and give all 8 directions
  Center <- matrix(1:((C-2)*(R-2)), nrow=(R-2), ncol=(C-2))
  for(i in 1:(C-2)){
    Center[,i] <- Center[,i] +(R+1+2*(i-1))}
  for(i in Center){
    #south, north, east, west, SE, NE, SW, NW
    CellDirections[[i]] <- c((i+1), (i-1), (i+R), (i-R),
                             (i+R+1), (i+R-1), (i-R+1), (i-R-1))}
  #Edges but not corners
  #West Edge
  #south, north, east, SE, NE
  for(i in 2:(R-1) ){
    CellDirections[[i]] <- c((i+1), (i-1), (i+R),(i+R+1), (i+R-1))}
  #South Edge
  #north, east, west, NE, NW
  for(i in R*(2:(C-1)) ){
    CellDirections[[i]] <- c((i-1), (i+R), (i-R), (i+R-1), (i-R-1))}
  #North Edge
  #south, east, west, SE, SW
  for(i in 1+R*(1:(C-2)) ){
    CellDirections[[i]] <- c((i+1), (i+R), (i-R), (i+R+1), (i-R+1))}
  #East Edge
  #south, north, west, SW, NW
  for(i in (R*(C-1)+2):(R*C-1) ){
    CellDirections[[i]] <- c((i+1), (i-1), (i-R), (i-R+1), (i-R-1))}
  #Corners
  #NW Corner
  #south, east, SE
  CellDirections[[1]] <- c((2), (1+R), (R+2))
  #SW Corner
  #north, east, NE
  CellDirections[[R]] <- c((R-1), (2*R), (2*R-1))
  #NE Corner
  #south, west, SW
  CellDirections[[1+(R*(C-1))]] <- c((R*(C-1)+2), (1+R*(C-2)), (R*(C-2)+2))
  #SE Corner
  #north, west, NW
  CellDirections[[R*C]] <- c((R*C-1), (R*C-R), (R*C-R-1))
  return(CellDirections)
}


#' Next Step Directions
#'
#' Extends the locality data by one step.
#' @param currentStep the current list of location data
#' @param firstStep the output form OneStepDirections()
#' @keywords locality
#' @export
NextStepDirections <- function(currentStep, firstStep){
  #serially expand the steps away from a target cell by one by taking the
  #indicies from cells that are currently connected to the target cell
  for(i in seq_along(currentStep)){
    Index <- unique(unlist(firstStep[currentStep[[i]]]))
    #remove the target cell from the list
    currentStep[[i]] <- Index[-which(Index == i)]
  }
  return(currentStep)
}


#' Local Search
#'
#' Given a target territory, find which local males are alive.  If none are alive, extend the search by one step.
#' @param P a list of parameters
#' @param population the population
#' @param targetMale index of the territory around which local birds should be found
#' @param notAvailable vectors of males that cannot be chosen
#' @keywords locality
#' @export
LocalSearch <- function(P, population, targetMale, notAvailable){
    for(i in 1:max(P$R,P$C)){
    CurrentIndex <- population$Directions[[i]][[targetMale]]
    CurrentIndex <- CurrentIndex[which(!(CurrentIndex %in% notAvailable))]
    if(length(CurrentIndex) != 0){
      return(CurrentIndex)
    }
  }
  stop(paste0("All males are dead; Game Over, SimStep=", P$SimStep,"."))
}
