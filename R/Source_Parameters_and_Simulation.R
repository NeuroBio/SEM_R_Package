#Source for parameters and main simulation code:

#Create Parameter function

#' Define Parameters
#'
#' Creates a parameter list and error checks chosen parameters.  This is the only place where extensive error checking is done on parameters!
#' @param Rows the number of rows in the bird matrix
#' @param Cols the number of columns in the bird matrix
#' @param Steps the number of spaces away from a focal territory "local" is considered to be
#' @param InitialSylRepSize the number of syllables birds have a 90\% chance to know when the bird matrix is intialized
#' @param PrcntSylOverhang the fraction of InitialSylRepsize that birds have a 10\% and 1\% chance to know when the bird matrix is intialized
#' @param MaxSylRepSize the length of the syllable vector
#' @param InitialAccuracy the mode value for accuracy when the bird matrix is intialized
#' @param InherAccuracyNoise the area around the mode that can be sampled from for accuracy inheritance and establishing the initial distribution
#' @param AccuracyLimits the absolute min and max values that accuracy can be
#' @param MaxAge the maximum age of birds in the population (only in use with the type II survival curve)
#' @param InitialLearningThreshold the mode value for the learning threshold when the bird matrix is intialized
#' @param InherLearningNoise the area around the mode that can be sampled from for learning threshold inheritance and establishing the initial distribution
#' @param LearningLimits the absolute min and max values that the learning threshold can be
#' @param InitialChancetoInvent the mode value for the chance to invent when the bird matrix is intialized
#' @param InherChancetoInventNoise the area around the mode that can be sampled from for chance to invent inheritance and establishing the initial distribution
#' @param ChancetoInventLimits the absolute min and max values that the chance to invent can be
#' @param InitialChancetoForget the mode value for the chance to forget when the bird matrix is intialized
#' @param InherChancetoForgetNoise the area around the mode that can be sampled from for chance to forget inheritance and establishing the initial distribution
#' @param ChancetoForgetLimits the absolute min and max values that the chance to forget can be
#' @param ListeningThreshold the max absolute number or fraction of syllables a bird hears from one oblique tutor
#' @param FatherListeningThreshold the max absolute number or fraction of syllables a bird hears from his father tutor
#' @param MinLearnedSyls when either listenign threshodl is less than 0, this is the number of syllables a bird hears from his tutor before the fraction is applied
#' @param EncounterSuccess the chance that a male finds suitable tutors
#' @param LearningPenalty an artitrary scale for how severly longer learning is punished
#' @param AgeDeath whether to model death on a type II survival curve (TRUE) or random death (FALSE)
#' @param PrcntRandomDeath the percentage of birds that die each time step when death is random
#' @param DeathThreshold the numbers of birds at which a group is considered to be extinquished (you probably should not change this)
#' @param ChickSurvival the proportion of chicks that survive to age 1
#' @param LocalBreed whether empty territory are filled by chicks from local males (TRUE) or any male (FALSE)
#' @param LocalTutor whether obliqgue learners pick tutors from from local males (TRUE) or any male (FALSE)
#' @param LearnerStrategy the mode by which birds learn; can be "Add", "Forget", "AddForget", or "Consensus"
#' @param ConsensusNoTut the number of tutors sampled in the consensus strategy
#' @param ConsensusStrategy the method by which consensus decisions are made; can be "Conform" (chance = based on conformity bias), "AllNone" (all tutors must sing the syllable), "Percentage" (chance = percent of tutor that sang a syllable)
#' @param OverLearn whether males overlearn from many tutors as chicks
#' @param OverLearnNoTut the number of tutors sampled in the overlearning strategy
#' @param VerticalLearnCutOff this minimum value the learning window can be while still allowing males to learn vertically.
#' @param ObliqueLearning whether the population undergoes oblique learning (TRUE) or not (FALSE)
#' @param VerticalLearning whether the population undergoes vertical learning (TRUE) or not (FALSE)
#' @param RepSizePrefer the fraction of female preference dedicated to larger repertoires
#' @param LogScale whether females percieve repertoire size on a natural log scale (TRUE) or not (FALSE)
#' @param MatchPrefer the fraction of female preference dedicated to template matching
#' @param UniformMatch whether all females have the same song template (TRUE) or variations on a template (FALSE)
#' @param MatchScale an equation for how matching is perceived; not yet implemented!
#' @param Dialects the number of dialects; must be a factor of the matrix size
#' @param MaleDialects whether males start the simulation with dialects; can be "None" (all males are similar to dialect 1), "Similar" (male songs are in teh correct syllable space, but are not identical to female songs), "Same" (male song temapltes are identicle to their female's template)
#' @param FemaleEvolve whether the female templates can evolve (TRUE) or stay static throughout teh simmulation (FALSE)
#' @param ChooseMate whether females can pick their mate (TRUE) or not (FALSE)
#' @param SaveMatch whether to save matches; can be NA (the program decides based on other parameters) or TRUE/FALSE
#' @param SaveAccuracy whether to save the accuracy values; can be NA (the program decides based on other parameters) or TRUE/FALSE
#' @param SaveLearningThreshold whether to save the learning thresholds; can be NA (the program decides based on other parameters) or TRUE/FALSE
#' @param SaveChancetoInvent whether to save the chance to invent; can be NA (the program decides based on other parameters) or TRUE/FALSE
#' @param SaveChancetoForget whether to save the chance to forget; can be NA (the program decides based on other parameters) or TRUE/FALSE
#' @param SaveNames whether to save the UID and father's UID of the birds; can be TRUE or FALSE
#' @param SaveAge whether to save the age of the birds; can be TRUE or FALSE
#' @param SaveMaleSong whether to save male song templates; can be TRUE or FALSE
#' @param SaveFemaleSong whether to save female song templates; can be TRUE or FALSE
#' @param numSim the number of sim steps to complete
#' @param Seed seed to run simulation on for reproducibility
#' @keywords initialize
#' @export
DefineParameters <- function(Rows=20, Cols=20, Steps=1,
                             InitialSylRepSize=5, PrcntSylOverhang=.2, MaxSylRepSize=500,
                             InitialAccuracy=.7, InherAccuracyNoise=.15,  AccuracyLimits=c(0,1),
                             MaxAge=20, InitialLearningThreshold=2, InherLearningNoise=.25, LearningLimits=c(0,MaxAge),
                             InitialChancetoInvent=.1, InherChancetoInventNoise=0, ChancetoInventLimits=c(0,1),
                             InitialChancetoForget=.2, InherChancetoForgetNoise=0, ChancetoForgetLimits=c(0,1),
                             ListeningThreshold=7, FatherListeningThreshold=.999, MinLearnedSyls=7,
                             EncounterSuccess=.95, LearningPenalty=.75, AgeDeath=TRUE,
                             PrcntRandomDeath=.1, DeathThreshold=1, ChickSurvival=.3,
                             LocalBreed=FALSE, LocalTutor=FALSE, LearnerStrategy="Add",
                             ConsensusNoTut=8, ConsensusStrategy="Conform",
                             OverLearn=FALSE, OverLearnNoTut=3, VerticalLearnCutOff=.25,
                             ObliqueLearning=TRUE, VerticalLearning=TRUE,
                             RepSizePrefer=1, LogScale=TRUE, MatchPrefer=0, UniformMatch=TRUE, MatchScale=1,
                             Dialects=1, MaleDialects="None", FemaleEvolve=FALSE, ChooseMate=FALSE,
                             SaveMatch=NA, SaveAccuracy=NA, SaveLearningThreshold=NA, SaveChancetoInvent=NA, SaveChancetoForget=NA,
                             SaveNames=FALSE, SaveAge=FALSE, SaveMaleSong=FALSE, SaveFemaleSong=FALSE,
                             numSim=1000, Seed=NA){
  numBirds <- Rows*Cols
  if(AgeDeath){
    InitProp <- CalculateProportion(numBirds, DeathThreshold, ChickSurvival, MaxAge)
  }else{
    InitProp <- 0
  }

  if((LearnerStrategy %in% c("Add", "AddForget", "Forget", "Consensus")) == FALSE){
    stop("LearnerStrategy must be either Add, Forget, AddForget, or Consensus.")
  }
  if(LearnerStrategy == "Consensus"){
    Consensus=TRUE
    Add=TRUE
    Forget=TRUE
  }else{
    Consensus=FALSE
    if(LearnerStrategy %in% c("Add", "AddForget")){
      Add=TRUE
    }else{Add=FALSE}
    if(LearnerStrategy %in% c("AddForget", "Forget")){
      Forget=TRUE
    }else{Forget=FALSE}
  }

  #match the saves
  SaveMatch <- TestRequirement(SaveMatch, MatchPrefer, FemaleEvolve)
  SaveAccuracy <- TestRequirement(SaveAccuracy, InherAccuracyNoise)
  SaveLearningThreshold <- TestRequirement(SaveLearningThreshold, InherLearningNoise)
  SaveChancetoInvent <- TestRequirement(SaveChancetoInvent, InherChancetoInventNoise)
  SaveChancetoForget <- TestRequirement(SaveChancetoForget, InherChancetoForgetNoise)

  Parameters <- data.frame(R=Rows, C=Cols, numBirds=numBirds, Steps=Steps,
                           RSize0=InitialSylRepSize, PerROh=PrcntSylOverhang, MaxRSize=MaxSylRepSize,
                           Acc0=InitialAccuracy, IAccN=InherAccuracyNoise,
                           MinAcc=AccuracyLimits[1], MaxAcc=AccuracyLimits[2],
                           MAge=MaxAge, LrnThrsh0=InitialLearningThreshold, ILrnN=InherLearningNoise,
                           MinLrn=LearningLimits[1], MaxLrn=LearningLimits[2],
                           CtI0=InitialChancetoInvent, ICtIN=InherChancetoInventNoise,
                           MinCtI=ChancetoInventLimits[1], MaxCtI=ChancetoInventLimits[2],
                           CtF0=InitialChancetoForget, ICtFN=InherChancetoForgetNoise,
                           MinCtF=ChancetoForgetLimits[1], MaxCtF=ChancetoForgetLimits[2],
                           LisThrsh=ListeningThreshold, FLisThrsh=FatherListeningThreshold,
                           MinLrnSyl=MinLearnedSyls, EnSuc=EncounterSuccess,
                           Lpen=LearningPenalty, DStrat=AgeDeath, PDead=PrcntRandomDeath,
                           DeadThrsh=DeathThreshold, Pc=ChickSurvival, InitProp=InitProp,
                           ScopeB=LocalBreed, ScopeT=LocalTutor,
                           Consen=Consensus, ConsenS=ConsensusStrategy, Add=Add, Forget=Forget,
                           ConNoTut=ConsensusNoTut, OvrLrn=OverLearn, OLNoTut=OverLearnNoTut,
                           Obliq=ObliqueLearning, Vert=VerticalLearning,
                           VertLrnCut=VerticalLearnCutOff,
                           RepPref=RepSizePrefer, LogScl=LogScale, MatPref=MatchPrefer,
                           NoisePref=1-(RepSizePrefer + MatchPrefer), UniMat=UniformMatch, MScl=MatchScale,
                           Dial=Dialects, MDial=MaleDialects, FEvo=FemaleEvolve, ChoMate=ChooseMate,
                           SMat=SaveMatch, SAcc=SaveAccuracy, SLrn=SaveLearningThreshold,
                           SCtI=SaveChancetoInvent, SCtF=SaveChancetoForget, SNam=SaveNames,
                           SAge=SaveAge, SMSng=SaveMaleSong, SFSng=SaveFemaleSong,
                           SimStep=1, nSim=numSim, Seed=Seed, stringsAsFactors=FALSE)
  P <- CheckP(Parameters)
  return(Parameters)
}
#' Check Parameters
#'
#' Checks whether user defined parameters fit all requirements
#' @param P a list of parameters
#' @keywords error-check
#' @export
CheckP <- function(P){
  #test if ints within min/max range
  CheckMinMaxInt(P$R, "Rows", 3)
  CheckMinMaxInt(P$C, "Cols", 3)
  CheckMinMaxInt(P$nSim,"numSim", 1)
  CheckMinMaxInt(P$Steps, "Steps", 1)
  CheckMinMaxInt(P$RSize0, "InitialSylRepSize", 1)
  CheckMinMaxInt(P$MaxRSize, "InitialSylRepSize", 1)
  CheckMinMaxInt(P$PerROh, "PrcntSylOverhang", 0, int=FALSE)
  CheckMinMaxInt(P$MAge, "MaxAge", 3, P$MAge, TRUE)
  CheckMinMaxInt(P$MinLrnSyl, "MinLearnedSyls", 0, P$MaxRSize, TRUE)
  CheckMinMaxInt(P$Lpen,"LearningPenalty", 0, int=FALSE)
  CheckMinMaxInt(P$EnSuc,"EncounterSuccess", 0, 1, TRUE, FALSE)
  CheckMinMaxInt(P$OLNoTut,"OverLearnNoTut", 1)
  CheckMinMaxInt(P$ConNoTut,"ConsensusNoTut", 2)
  CheckMinMaxInt(P$VertLrnCut,"VerticalLearnCutOff", 0, 1, TRUE, FALSE)
  CheckMinMaxInt(P$PDead,"PrcntRandomDeath", .01, .9, TRUE, FALSE)
  CheckMinMaxInt(P$RepPref, "RepSizePrefer", 0, 1, TRUE, FALSE)
  CheckMinMaxInt(P$MatPref, "MatchPrefer", 0, 1, TRUE, FALSE)
  CheckMinMaxInt(P$DeadThrsh,"DeathThreshold", .0001, .2*P$numBirds, TRUE, FALSE)
  CheckMinMaxInt(P$Pc,"ChickSurvival", .1, 1, TRUE, FALSE)

  #make sure bools are bool
  CheckBool(P$OvrLrn,"OverLearn")
  CheckBool(P$DStrat,"AgeDeath")
  CheckBool(P$SMat,"SaveMatch", TRUE)
  CheckBool(P$SAcc,"SaveAccuracy", TRUE)
  CheckBool(P$SLrn,"SaveLearningThreshold", TRUE)
  CheckBool(P$SCtF,"SaveChancetoForget", TRUE)
  CheckBool(P$SCtI,"SaveChancetoInvent", TRUE)
  CheckBool(P$SNam,"SaveNames")
  CheckBool(P$SAge,"SaveAge")
  CheckBool(P$SMSng,"SaveMaleSong")
  CheckBool(P$SFSng,"SaveFemaleSong")
  CheckBool(P$FEvo,"FemaleEvolve")
  CheckBool(P$ChoMate,"ChooseMate")
  CheckBool(P$LogScl,"LogScale")
  CheckBool(P$UniMat, "UniformMatch")
  CheckBool(P$ScopeB,"LocalBreed")
  CheckBool(P$ScopeT,"LocalTutor")
  CheckBool(P$Vert, "VerticalLearning")


  #make sure all of the trait vals line up
  CheckTrait(P$Acc0, P$IAccN, P$MinAcc, P$MaxAcc, "Accuracy")
  CheckTrait(P$LrnThrsh0, P$ILrnN, P$MinLrn, P$MaxLrn, "Learning Threshold", P$MAge)
  CheckTrait(P$CtI0, P$ICtIN, P$MinCtI, P$MaxCtI, "Chance Invent")
  CheckTrait(P$CtF0, P$ICtFN, P$MinCtF, P$MaxCtF, "Chance Forget")



  #Misc Errors
  if(P$RSize0*(1+2*P$PerROh) > P$MaxRSize){
    stop(paste0("InitialSylRepSize and/or PrcntSylOverhang is too large for the given MaxSylRepSize. (", P$MaxRSize, ")"))
  }

  if(P$Dial < 1 ||
     P$Dial >= P$numBirds ||
     P$numBirds%%P$Dial != 0 ||
     P$Dial%%1 != 0 ||
     P$Dial > floor(P$MaxRSize/(P$RSize0*(1+2*P$PerROh)))){
    stop("Dialects must meet the following criterion:
         1) Must be an integer of 1 or greater.
         2) Cannot be larger than the number of birds.
         3) Must be a factor of the number of birds.
         4) Must be less than or equal to MaxSylRepSize/(InitialSylRepSize*(1+2*PrcntSylOverhang)).")
  }
  if((P$MDial %in% c("None", "Similar", "Same")) == FALSE){
    stop("MaleDialects must None, Similar, or Same.")
  }
  if((P$ConsenS %in% c("Conform", "AllNone", "Percentage")) == FALSE){
    stop("MaleDialects must Conform, AllNone, or Percentage.")
  }
  if(P$RepPref+P$MatPref > 1){
    stop("RepSizePrefer+MatchPrefer cannot exceed 1")
  }

  if(is.na(P$Seed)==FALSE && is.numeric(P$Seed) == FALSE){
    stop("Seed must be a number or NA")
  }
  if(P$MatPref == 0  && P$SMat == FALSE && P$SFSng == TRUE){
    stop("Cannot save female song unless it is generated.
         It is not generated unless 1) MatchPrefer > 0,
         2) FemaleEvolve == TRUE,
         or 3) SaveMatch == TRUE.")
  }
  if((P$LisThrsh%%1 != 0 && P$LisThrsh > 1) || (P$LisThrsh > P$MaxRSize)  || P$LisThrsh < 0){
    stop(paste0("ListeningThreshold must either be an integer from 1 to MaxSylRepSize (", P$MaxRSize, ")",
                "or a fraction representing a precentage.*
                *If .999 or greater is typed, it is converted to 100%."))
  }
  if((P$FLisThrsh%%1 != 0 && P$FLisThrsh > 1) || (P$FLisThrsh > P$MaxRSize)  || P$FLisThrsh < 0){
    stop(paste0("FatherListeningThreshold must either be an integer from 1 to MaxSylRepSize (", P$MaxRSize, ")",
                "or a fraction representing a precentage.*
                *If .999 or greater is typed, it is converted to 100%."))
  }


  #Misc Warnings
  if(P$EnSuc == 0){
    warning("If EncounterSuccess is set to 0, no oblique learning can occur.")
  }
  if(P$Steps >= max(P$R, P$C)-1 && (P$ScopeB || P$ScopeT)){
    warning("Steps cover entire matrix.  LocalBreed and LocalTutor switched to Global")
    P$ScopeB <- FALSE
    P$ScopeT <- FALSE
  }
  if(P$DeadThrsh < 1){
    warning("Small DeathThresholds decrease the chances that any birds will survive
            the selection process long enough to reach the MaxAge.")
  }
  if(P$FEvo == TRUE && P$MatPref ==0){
    warning("FemaleEvolve implimented only when females have a match preference > 0.")
  }
  if(P$MatPref == 0 && P$MDial != "None"){
    warning("MaleDialects only implemented when MatchPrefer is > 0.")
  }
  if(P$MScl != 1){
    warning("MatchScale is not yet implemented!!!")
  }
  return(P)
}

#' Check Trait
#'
#' Checks whether user defined parameters make sense for each song-learning trait
#' @param initial user defined initial value
#' @param noise user defined noise value
#' @param min user defined min
#' @param max user defined max
#' @param name name fo the trait in question
#' @param absMax absolute max possible for parameter, usually 1
#' @keywords error-check
#' @export
CheckTrait <- function(initial, noise, min, max, name, absMax=1){
  if(min >= max){
    stop(paste("The min for", name, "must be less than the max."))
  }
  if(noise > (max-min)/2){
    stop(paste("The noise for", name, "is too large."))
  }
  if(noise < 0){
    stop(paste("The noise for", name, "cannot be less than 0"))
  }
  if(min < 0){
    stop(paste("The min", name, "cannot be less than 0."))
  }
  if(max > absMax){
    stop(paste("The max", name, "cannot be greater than", absMax, "."))
  }
  if(initial > max || initial < min){
    stop(paste("The initial value for", name, "must be with within the range of its min and max."))
  }
}



#' Check Min/Max/Int
#'
#' Checks whether values that should be integers are and ensures that are within the correct range.
#' @param value the user defined parameter
#' @param valueName the name of the value being checked
#' @param min minumum value for a feature
#' @param max maximum value value for a feature
#' @param maxed whether a value has a maximum
#' @param int whether a value is an integer
#' @keywords error-check
#' @export
CheckMinMaxInt <- function(value, valueName, min=0, max=1, maxed=FALSE, int=TRUE){
  if(value < min){stop(paste(valuename, "cannot be less than",min,"."))}
  if(maxed){if(value>max){stop(paste(valueName, "cannot be greater than",max,"."))}}
  if(int){if(value%%1 !=0){stop(paste(valueName, "must be an integer."))}}
}



#' Check Boolean
#'
#' Checks whether a value that should be Boolean is, allowing NA if required.
#' @param value user parameter to check
#' @param valueName name of teh checked parameter
#' @param NAer whther the value can be NA
#' @keywords error-check
#' @export
CheckBool <- function(value, valueName, NAer=FALSE){
  if(!NAer){
    if(!(value %in% c(TRUE,FALSE))){
      stop(paste(valueName, "must be TRUE or FALSE."))
    }
  }else{
    if(!is.logical(value)){
      stop(paste(valueName, "must be TRUE, FALSE, or NA."))
    }
  }
}



#' Test Requirement
#'
#' If a Save parameter is set to NA, checks whether they should be set to TRUE or FALSE.
#' @param test a SaveTrait
#' @param dependancy1 a value for trait noise
#' @param dependancy2 a secon dependancy that requires teh trait
#' @keywords error-check
#' @export
TestRequirement <- function(test, dependancy1 = 0, dependancy2 = FALSE){
  if(is.na(test)){
    if(dependancy1 == 0 && !dependancy2){return(FALSE)}
    return(TRUE)
  }else{return(test)}
}
#Save and load in parameters



#' Save Parameterss
#'
#' Saves a list of parameters as a .SEMP file.
#' @param P a list of parameters
#' @param folderName where to save the .SEMP
#' @param fName file name for the .SEMP
#' @param type the simulation type run (accepts any string)
#' @keywords read-write-run
#' @export
SaveParam <- function(P, folderName, fileName="Parameters", type="Basic"){
  P$SimStep <- 1
  UsedP <- paste0(names(P), "=", P)
  if(any(grepl("Type", UsedP))){
    UsedP <- UsedP[-which(grepl("Type", UsedP))]
  }
  UsedP <- c(UsedP, paste0("Type=",type))
  write.table(UsedP, file.path(folderName, paste0(fileName,".semp")), quote=FALSE,
              sep = "\n", row.names=FALSE, col.names=FALSE)
}



#' Reload Parameters
#'
#' Loads a .SEMP file and converts it into a list of parameters.
#' @param filePath the pather where a .SEMP is located
#' @keywords read-write-run
#' @export
ReloadParam <- function(filePath){
  Raw <- read.table(filePath, sep="=", as.is=TRUE)
  Final <- rbind.data.frame(Raw$V2, stringsAsFactors=FALSE)
  names(Final) <- Raw$V1
  Log <- which(Final %in% c("TRUE", "FALSE", "True", "False"))
  Char <- which(names(Final) %in% c("MDial", "ConsenS"))
  suppressWarnings(Final[,-c(Char, Log)] <- as.numeric(Final[,-c(Char, Log)]))
  Final[,Log] <- as.logical(Final[,Log])
  return(Final)
}


#' SEM Simulation
#'
#' A wrapper that conveniently handles data saving and times the simulation
#' @param P a list of parameters
#' @param type what type of simulation to run ('Basic', 'Light', 'Insult)
#' @param folderName where to save the simualtion data, defaults to a timestemp in the current directory
#' @param save whether to write the data to .csvs
#' @param return whether to return the data in R
#' @param verbose whether to print the running time and folder name
#' @param ... arguments to the simulation types.  See documentation for individual sim type arguments.
#' @keywords read-write-run
#' @family Sim Functions
#' @examples
#' P <- DefineParameters(RepSizePrefer = 0, MatchPrefer = 1, numSim=100)
#' SEMSimulation(P, 'Basic', 'Example', return=TRUE)
#' SEMSimulation(P, 'Interval', 'Example', return=TRUE, freq=2)
#'
#' P2 <- DefineParameters(numSim=600, MatchPrefer = 1, RepSizePrefer = 0)
#' P3 <- DefineParameters(numSim=600, SaveMatch = TRUE)
#' SEMSimulation(P2, insultP=P3, 'Insult', when=100, freq=2, save=FALSE, return = TRUE)
#' @export
SEMSimulation <- function(P, type='Basic', folderName=NA, save=TRUE, return=FALSE, verbose=TRUE, ...){
  Time <- proc.time()
  MiscArgs <- list(...)
  if(is.na(P$Seed) == FALSE){
    set.seed(P$Seed)
  }


  if(save){#Set up folder
    if(is.na(folderName)){
      folderName <- file.path(format(Sys.time(), "%F_%H-%M-%S"))
    }
    if(dir.exists(file.path(folderName)) == FALSE){
      dir.create(file.path(folderName))
    }
  }

  #Run Simulations
  if(type == 'Basic'){
    Data <- BasicSimulation(P, MiscArgs$freq)
  }else if(type == 'Light'){
    Data <- LightSimulation(P, MiscArgs$freq)
  }else if(type == 'Insult'){
    Data <- InsultSimulation(P, MiscArgs$insultP, MiscArgs$when, MiscArgs$freq)
  }else if(type == 'Invasion'){
    Data <- InvasionSimulation(P, MiscArgs$numInvader, MiscArgs$trait, MiscArgs$stat, MiscArgs$when)
  }else{
    stop("Simulation type not recognized; check your spelling and case!")
  }

  if(save){#write the data
    if(type == 'Invasion'){
      write.csv(Data,file.path(folderName,'Invasion.csv'),row.names=FALSE)
    }else{
      eval(parse(text=paste0("write.csv(Data$",SaveInfo$Name,",file.path(folderName,'",
                             SaveInfo$Name, ".csv'),row.names=FALSE)")))
      SaveParam(P, folderName, type="Basic")
    }
  }

  #return a sense of satisfaction
  if(verbose){
    print("Simulation Completed!")
    print(proc.time()-Time)
    if(save){
      print(paste("Saved to", folderName))
    }
  }

  if(return){#return the sim data
    return(Data)
  }
}



#' Get Save Info
#'
#' Generates the information to form appropriate datastructures
#' @param P a list of parameters
#' @family Sim Functions
#' @keywords read-write-run
#' @export
GetSaveInfo <- function(P){
  Params <- c(TRUE, P$SMat, P$SAcc, P$SLrn, P$SCtI, P$SCtF,
              P$SNam, P$SNam, P$SAge, P$SMSng, P$SFSng)
  Names <- c("SylRep", "Match", "Acc", "LrnThsh", "ChanceInv", "ChanceFor",
             "Name", "FatherName", "Age", "MSong", "FSong")
  SaveInfo <- data.frame(Name=Names, Has=Params,
                         Size=c(rep(P$numBirds,9), P$MaxRSize, P$MaxRSize),
                         Location=c(rep('Males',9), 'MSongs', 'FSongs'),
                         Sublocation=c(Names[1:9], NA, NA), stringsAsFactors = FALSE)
  return(SaveInfo[SaveInfo$Has,])
}



#' Basic Simulation
#'
#' Runs a simulation where individual values are saved for every time step.  No parameters change during the simulation.
#' @param P a list of parameters
#' @param freq how often to sample data from the simulation
#' @family Sim Functions
#' @keywords read-write-run
#' @export
BasicSimulation <- function(P, freq=1){
  if(is.null(freq)){
    freq <- 1
  }
  SaveInfo <- GetSaveInfo(P)
  Population <- GenerateFounderMales(P)
  #Make Matricies and save initial data
  eval(parse(text=paste0(SaveInfo$Name, "<-matrix(0,nrow=", SaveInfo$Size,
                         ", ncol=", (P$nSim/freq)+1, ")")))
  eval(parse(text=paste0(SaveInfo$Name,"[,", 1, "]<-",
                         ifelse(is.na(SaveInfo$Sublocation),
                                paste0("colSums(Population$", SaveInfo$Location, ")"),
                                paste0("Population$", SaveInfo$Location, "$", SaveInfo$Sublocation)))))
  #Run the simulation
  Counter <- 2
  for(i in 2:(P$nSim+1)){
    Population <- BirthDeathCycle(P, Population)
    if(i%%freq == 0){#Save
      eval(parse(text=paste0(SaveInfo$Name,"[,", Counter, "]<-",
                             ifelse(is.na(SaveInfo$Sublocation),
                                    paste0("colSums(Population$", SaveInfo$Location, ")"),
                                    paste0("Population$", SaveInfo$Location, "$", SaveInfo$Sublocation)))))
      Counter <- Counter+1
    }
    P$SimStep <- i
  }
  eval(parse(text=paste0("Data <- list(", paste0(SaveInfo$Name, "=", SaveInfo$Name, collapse = ","), ")" )))
  return(Data)
}




#' Light Simulation
#'
#' Runs a simulation where the only average values are saved every [freq] time step.  No parameters change during the simulation.
#' @param P a list of parameters
#' @param freq how often to sample data from the simulation
#' @family Sim Functions
#' @keywords read-write-run
#' @export
LightSimulation <- function(P, freq=200){
  if(is.null(freq)){
    freq <- 200
  }

  SaveInfo <- GetSaveInfo(P)
  Population <- GenerateFounderMales(P)
  #Make Matricies and save initial data
  eval(parse(text=paste0(SaveInfo$Name, "<-matrix(0,nrow=",
                         ifelse(SaveInfo$Name %in% c("MSong", "FSong"), SaveInfo$Size, 1),
                         ", ncol=", (P$nSim/freq)+1, ")")))
  eval(parse(text=paste0(SaveInfo$Name,"[,", 1, "]<-",
                         ifelse(is.na(SaveInfo$Sublocation),
                                paste0("colSums(Population$", SaveInfo$Location, ")"),
                                paste0("mean(Population$", SaveInfo$Location, "$", SaveInfo$Sublocation, ")")))))

  #Run the simulation
  Counter <- 2
  for(i in 2:(P$nSim+1)){
    Population <- BirthDeathCycle(P, Population)
    if(i%%freq == 0){#Save
      eval(parse(text=paste0(SaveInfo$Name,"[,", Counter, "]<-",
                             ifelse(is.na(SaveInfo$Sublocation),
                                    paste0("colSums(Population$", SaveInfo$Location, ")"),
                                    paste0("mean(Population$", SaveInfo$Location, "$", SaveInfo$Sublocation, ")")))))
      Counter <- Counter+1
    }
    P$SimStep <- i
  }
  eval(parse(text=paste0("Data <- list(", paste0(SaveInfo$Name, "=", SaveInfo$Name, collapse = ","), ")" )))
  return(Data)
}



#' Invasion Simulation
#'
#' Runs an invasion simulation where the time to conversion and final average value of the trait is returned.  [numInvader] invaders have their [trait] changed to [invaderStat] and their age reset to 1 time step [when].  Simulation ends when invaders are expelled, take over, or P$nSim time steps have passed.
#' @param P a list of parameters
#' @param numInvader the number of invaders to add
#' @param trait the stat to change: LrnThsh, Acc, ChanceInv, or ChanceFor
#' @param invaderStat the stat to change: LrnThsh, Acc, ChanceInv, or ChanceFor
#' @param when the time step at which to introduce the invaders
#' @family Sim Functions
#' @keywords read-write-run
#' @export
InvasionSimulation <- function(P, numInvader, trait, stat, when){
  P <- CheckInvasion(P, trait, stat)
  Population <- GenerateFounderMales(P)

  #Run simulation
  for(i in 1:(when-1)){
    #Complete a cycle
    Population <- BirthDeathCycle(P, Population)
    P$SimStep <- i
  }

  #Introduce invader(s)
    Invaders <- sample(P$numBirds, numInvader)
    Population$Males[Invaders, trait] <- stat
    Population$Males[Invaders,'Age'] <- 1

  for(i in 1:P$nSim){
    Population <- BirthDeathCycle(P, Population)
    if(length(unique(Population$Males[,trait]))== 1){
      break
    }
  }
  return(c(i, mean(Population$Males[,trait])))
}
#' Check Invasion
#'
#' Checks whether the noise for the [trait] was set to 0, and does so it not.  Also ensures that [stat] is within the range for the [trait]..
#' @param P a list of parameters
#' @param numInvader the number of invaders to add
#' @param trait the stat to change: LrnThsh, Acc, ChanceInv, or ChanceFor
#' @param invaderStat the stat to change: LrnThsh, Acc, ChanceInv, or ChanceFor
#' @param when the time step at which to introduce the insult
#' @param freq how often to sample data from the simulation
#' @family Sim Functions
#' @keywords read-write-run
#' @export
CheckInvasion <- function(P, trait, stat){
  if(trait == "LrnThrsh"){
    if(P$ILrnN != 0){
      P$ILrnN <- 0
      warning("ILrnN reset to 0")
    }
    if(stat > P$MaxLrn || stat < P$MinLrn){
      stop("stat must be in the min and max bounds of trait")
    }
  }else if(trait == "Acc"){
    if(P$IAccN != 0){
      P$IAccN <- 0
      warning("IAccN reset to 0")
    }
    if(stat > P$MaxAcc || stat < P$MinAcc){
      stop("stat must be in the min and max bounds of trait")
    }
  }else if(trait == "ChanceInv"){
    if(P$ICtIN != 0){
      P$ICtIN <- 0
      warning("ICtIN reset to 0")
    }
    if(stat > P$MaxCtI || stat < P$MinCtI){
      stop("stat must be in the min and max bounds of trait")
    }
  }else if(trait == "ChanceFor"){
    if(P$ICtFN != 0){
      P$ICtFN <- 0
      warning("ICtFN reset to 0")
    }
    if(stat > P$MaxCtF || stat < P$MinCtF){
      stop("stat must be in the min and max bounds of trait")
    }
  }else{
    stop("Unknown trait passed to invasion; Trait must be LrnThrsh, Acc, ChainceInv, or ChanceFor.")
  }

  return(P)
}



#' Insult Simulation
#'
#' Runs a simulation where the only average values are saved every [freq] time step.  Parameters change from P to insultP at time step [when].
#' @param P a list of parameters
#' @param insultP a list of parameters to switch to at time step [when]
#' @param when the time step at which to introduce the insult
#' @param freq how often to sample data from the simulation
#' @family Sim Functions
#' @keywords read-write-run
#' @export
InsultSimulation <- function(P, insultP, when, freq=200){
  CheckInsultPs(P, insultP)
  if(is.null(freq)){
    freq <- 200
  }

  SaveInfo <- GetSaveInfo(P)
  Population <- GenerateFounderMales(P)
  #Make Matricies and save initial data
  eval(parse(text=paste0(SaveInfo$Name, "<-matrix(0,nrow=",
                         ifelse(SaveInfo$Name %in% c("MSong", "FSong"), SaveInfo$Size, 1),
                         ", ncol=", (P$nSim/freq)+1, ")")))
  eval(parse(text=paste0(SaveInfo$Name,"[,", 1, "]<-",
                         ifelse(is.na(SaveInfo$Sublocation),
                                paste0("colSums(Population$", SaveInfo$Location, ")"),
                                paste0("mean(Population$", SaveInfo$Location, "$", SaveInfo$Sublocation, ")")))))



  #Run simulation
  Counter <- 2
  for(i in 2:(P$nSim+1)){

    if(i == when){#Introduce insult
      #fix songs if needed
      if( (P$Dial != insultP$Dial) ||
          (P$UniMat != insultP$UniMat) ||
          (insultP$MatPref != 0 && (exists('FSongs', where=Population) == FALSE)) ){
            Population[["FSongs"]] <- CreateFemaleSongs(insultP)
            Population$Males[["Match"]] <- TestMatch(P, Population$MSongs, Population$FSongs)
      }
      P <- insultP
    }

    #Complete a cycle and save data
     Population <- BirthDeathCycle(P, Population)
    if(i%%freq == 0){#Save
      eval(parse(text=paste0(SaveInfo$Name,"[,", Counter, "]<-",
                             ifelse(is.na(SaveInfo$Sublocation),
                                    paste0("colSums(Population$", SaveInfo$Location, ")"),
                                    paste0("mean(Population$", SaveInfo$Location, "$", SaveInfo$Sublocation, ")")))))
      Counter <- Counter+1
    }
    P$SimStep <- i
  }
  eval(parse(text=paste0("Data <- list(", paste0(SaveInfo$Name, "=", SaveInfo$Name, collapse = ","), ")" )))
  return(Data)
}



#' Insult Simulation
#'
#' Tests whether the initial and insult parameters are compatable.
#' @param P a list of parameters
#' @param insultP a list of parameters to switch to at time step [when]
#' @family Sim Functions
#' @keywords error-check
#' @export
CheckInsultPs <-function(P, insultP){
  if(P$R != insultP$R ||
     P$C != insultP$C ||
     P$numBirds != insultP$numBirds ||
     P$MaxRSize != insultP$MaxRSize ||
     P$MinAcc != insultP$MinAcc ||
     P$MaxAcc != insultP$MaxAcc ||
     P$MAge != insultP$MAge ||
     P$MinLrn != insultP$MinLrn ||
     P$MaxLrn != insultP$MaxLrn ||
     P$MinCtI != insultP$MinCtI ||
     P$MaxCtI != insultP$MaxCtI ||
     P$MinCtF != insultP$MinCtF ||
     P$MaxCtF != insultP$MaxCtF ||
     P$DStrat != insultP$DStrat ||
     P$DeadThrsh != insultP$DeadThrsh
  ){
    stop("You cannot change the following parameters in the insult prameters:
         Rows, Columns, numBirds, Max Repertoire Size, Min Accuracy, Max Accuracy,
         Max Age, Min Learning Threshold, Max Learning Threshold, Min Chance to Invent,
         Max Chance to Invent, Min Chance to Forget, Max Chance to Forget,
         Death Strategy, Death Threshold.")
  }

  if( P$Steps != insultP$Steps ||
      P$RSize0 != insultP$RSize0 ||
      P$PerROh != insultP$PerROh ||
      P$Acc0 != insultP$Acc0 ||
      P$LrnThrsh0 != insultP$LrnThrsh0 ||
      P$CtI0 != insultP$CtI0 ||
      P$CtF0 != insultP$CtF0 ||
      P$InitProp != insultP$InitProp ||
      P$MDial != insultP$MDial ||
      P$SMat != insultP$SMat ||
      P$SAcc != insultP$SAcc ||
      P$SLrn != insultP$SLrn ||
      P$SCtI != insultP$SCtI ||
      P$SCtF != insultP$SCtF ||
      P$SNam != insultP$SNam ||
      P$SAge != insultP$SAge ||
      P$SMSng != insultP$SMSng ||
      P$SFSng != insultP$SFSng ||
      P$nSim != insultP$nSim ||
      !identical(P$Seed, insultP$Seed)
    ){
      print("Warning: You changed an insult parameter that only affects the initilization step;
            This could include:
            Steps, Repertoire Percent Overhang, any initial trait value, Male Dialects,
            saving variables, number of simuation steps, or the seed.")
  }

}
