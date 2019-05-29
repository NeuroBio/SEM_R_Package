#Functions related to learning

#Main core:
#' Core Learning Process
#'
#' Tests whether learners successfully acquire new syllables from their tutor(s) and modifies their song if this occurs.
#' @param P a list of parameters
#' @param newSongs the learner's plastic song
#' @param tutorSyllables the syllables the learner wants to learn from the tutor
#' @param accuracy the learner's accuracy
#' @param chanceInv the learners' chance to invent
#' @keywords song-learning
#' @export
LearningProcess <- function(P, newSongs, tutorSyllables, accuracy, chanceInv){
  for(i in seq_along(accuracy)){
    AccRoll <- runif(length(tutorSyllables[[i]]))
    #correct learning!
    SongCore <- AccRoll < accuracy[i]
    newSongs[i,tutorSyllables[[i]][which(SongCore)]] <- 1

    #Incorrect Learning Process
    Unlearned <- which(!SongCore)
    if(length(Unlearned) > 0){
      Improv <- sum(as.numeric(AccRoll[Unlearned] >= 1-((1-accuracy[i])*chanceInv[i])))
      if(Improv > 0){
        NewSongSyls <- which(newSongs[i,] == 1)
        TakenSyls <- unique(c(tutorSyllables[[i]], NewSongSyls))
        #Incorrect Learning generates novel syllables (not in father's repertoire)
        if(Improv <= P$MaxRSize-length(TakenSyls)){
          #Generate all new syllables
          NewSyl <- sample((1:P$MaxRSize)[-TakenSyls], Improv)
          newSongs[i,NewSyl] <- 1
        }else if(P$MaxRSize-length(TakenSyls) > 0){
          #Generate as many novel sylabels as syllable space allows
          NewSyl <- (1:P$MaxRSize)[-TakenSyls]
          newSongs[i,NewSyl] <- 1
        }#Else the father's syl rep is maxed out and nothing new can be generated
      }#Else nothing is invented due to poor learning
    }#Else father's repertoire learned perfectly
  }
  return(newSongs)
}

#Types of learning
#' Vertical Song Learning
#'
#' A wrapper that prepares chick and tutor template data during vertical learning.
#' @param P a list of parameters
#' @param templates matrix of the fathers' syllable vectors
#' @param chicks song-learning traits of chicks form the population ($Males)
#' @keywords song-learning
#' @export
VerticalSongLearning <- function(P, templates, chicks){
  #Method by which chicks inherit their father's song; accuracy
  #based on inherited accuracy and parameterized noise
  dim(templates) <- c(nrow=nrow(chicks), P$MaxRSize)#this line allows the code to work for a single father.
  NewSongs <- matrix(0,ncol=P$MaxRSize, nrow=nrow(chicks))
  Learn <- 1:nrow(chicks)
  NoLearn <- which(chicks$LrnThsh < P$VertLrnCut)
  if(length(NoLearn != 0)){
      Learn <- Learn[-NoLearn]
  }

  templates <- ListeningTest(P, templates[Learn,], P$FLisThrsh)
  FatherSyllables <- vector(mode="list",length=length(Learn))
  for(i in seq_along(Learn)){
    FatherSyllables[[i]] <- which(templates[Learn[i],] == 1)
  }

  NewSongs[Learn,] <- LearningProcess(P, NewSongs[Learn,], FatherSyllables, chicks$Acc[Learn], chicks$ChanceInv[Learn])
  return(NewSongs)
}


#' Oblique Learning
#'
#' A wrapper that checks which birds learn, allows them to do so, then updates syllable repertoire size and match (if needed).
#' @param P a list of parameters
#' @param population the population of birds
#' @param vacancy the indicies of dead birds
#' @keywords song-learning
#' @export
ObliqueLearning <- function(P, population, vacancy){
  #test which birds can and will learn
  Learners <- GetLearners(P, population, vacancy)
    if(length(Learners)==0){
      warning(paste("There were no learners in step", P$SimStep))
      return(population)
    }

  #get tutors, learn, update numsyls and match
  if(P$Consen){
    population$MSongs[Learners,] <- ConsensusLearning(P, population, Learners, vacancy)
  }else{
    Tutors <- ChooseTutors(P, population, Learners, vacancy)
    population$MSongs[Learners,] <- OneTutorLearning(P,population, Tutors, Learners)
  }


  population <- UpdateSongTraits(P, population, Learners)
  return(population)
}

#' Consensus Learning
#'
#' Allows birds to sample multiple tutors to create a consensus song as a template to decide what to learn.  Birds then learn.
#' @param P a list of parameters
#' @param population the population of birds
#' @param learners the indicies of birds that will attempt to learn
#' @param vacancy the indicies of dead birds
#' @keywords song-learning
#' @export
ConsensusLearning <- function(P, population, learners, vacancy){

  #Get ConNoTut number of tutors
  Tutors <- matrix(0, nrow=length(learners), ncol=P$ConNoTut)
  for(i in 1:P$ConNoTut){
    Tutors[,i] <- ChooseTutors(P, population, learners, vacancy, Tutors)
  }

  #create consensus song by combining tutor songs and testing which
  #syls will remain

  ConsensusSong <- matrix(0, nrow=length(learners), ncol=P$MaxRSize)
  AddSyllables <- vector(mode="list", length=length(learners))
  for(i in 1:nrow(ConsensusSong)){
    TutorSongs <- ListeningTest(P, population$MSongs[Tutors[i,],], P$LisThrsh)
    ConsensusSong[i,] <- colSums(TutorSongs)
    Fractional <- CalcFractional(P, ConsensusSong[i,])
    Test <- (Fractional)-runif(P$MaxRSize)
    AddSyllables[[i]] <- which(Test > 0)
    Remove <- which(population$MSongs[learners[i],]==1)
    NoLearn <- which(AddSyllables[[i]] %in% Remove)
    if(length(NoLearn) > 0){
      AddSyllables[[i]] <- AddSyllables[[i]][-NoLearn]
    }
  }

  #Learn and update song traits
  if(P$Add){
    population$MSongs[learners,] <- LearningProcess(P, matrix(population$MSongs[learners,],
                                                              nrow=length(learners)),
                                                    AddSyllables,
                                                    population$Males$Acc[learners],
                                                    population$Males$ChanceInv[learners])
  }
  if(P$Forget){
    population$MSongs[learners,] <- DropSyllables(population$Males$ChanceFor[learners],
                                                  ConsensusSong,
                                                  matrix(population$MSongs[learners,], nrow=length(learners))
                                                  )
  }
  return(population$MSongs[learners,])
}


#' Calculate Consensus Fraction
#'
#' Calculates the probability that a bird will learn a syllable depending on how many tutors that syllable was heard from.
#' @param P a list of parameters
#' @param consensusSong vector of the number of tutors that sang each syllable.
#' @keywords song-learning
#' @export
CalcFractional <- function(P, consensusSong){
  if(P$ConsenS == "Conform"){
    Conform <- consensusSong/P$ConNoTut
    return(Conform - sin(2*pi*Conform)/(2*pi))
  }else if(P$ConsenS == "AllNone"){
    return(floor(consensusSong/P$ConNoTut))
  }else{
    return(consensusSong/P$ConNoTut)
  }
}


#' Over-Learn
#'
#' Allows chicks to sample from tutors other than the father to add new syllables to their repertoire.
#' @param P a list of parameters
#' @param population the population of birds
#' @param learners the indicies of birds that will attempt to learn
#' @keywords song-learning
#' @export
OverLearn <- function(P, population, learners){
  for(i in 1:P$OLNoTut){
    Tutors <- ChooseTutors(P, population, learners, learners)
    population$MSongs[vacancy,] <- OneTutorLearning(P,population, Tutors, learners)
  }
  population <- UpdateSongTraits(P, population, learners)
  return(population)
}

#Misc
#' Female Evolve
#'
#' Replaces females that lived on the same territory as a dead male.  New female song templates are created based on fathers that are different from the father that sired the male on her territory.  Fathers must be alive and know at least one syllable.  One created, the match between these new females and their males are recalculated.
#' @param P a list of parameters
#' @param population the population of birds
#' @param vacancy indicies of territories where male chicks were born
#' @param fatherInd the indicies of the male that fathered the resident male chicks
#' @keywords song-learning
#' @export
FemaleEvolve <- function(P, population, vacancy, fatherInd){
  #when a male dies, so does his female, she is replaced
  #but a feamle with a new temaplte based on teh breeding males
  #in the population
  NotAvail <- c(vacancy,which(population$Males$SylRep == 0))
  PotentialFathers <- (1:P$numBirds)[-NotAvail]
  ProbBreed <- GetProbability(P, population, PotentialFathers)
  NewFathers <- numeric(length(vacancy))

  for(i in seq_along(NewFathers)){#get new father for territory
    OldFather <- which(PotentialFathers == fatherInd[i])
    NewFathers[i] <- sample(PotentialFathers[-OldFather],1,prob=ProbBreed[-OldFather])
  }
  population$FSongs[vacancy,] <- population$MSongs[NewFathers,]

  #update the matches where female songs have changed
  population$Males$Match[vacancy] <- TestMatch(P, population$MSongs[vacancy,],
                                                    population$FSongs[vacancy,])
  return(population)
}



#Accessory Functions (in order of appearance by cycle logic)
#' Get Learners
#'
#' Returns the indices of males that are alive, young enough to learn, and met tutor males.
#' @param P a list of parameters
#' @param population the population of birds
#' @param vacancy the indicies of dead birds
#' @keywords song-learning
#' @export
GetLearners <- function(P, population, vacancy){
  #get indicies of those who can learn, lest if they are capable
  #test whether they encouter atutor and get indicies of learners
  Alive <- (1:P$numBirds)[-vacancy]
  Capable <- TestLearningThreshold(P, population$Males[Alive,])
  population$Males[Alive[Capable], c("Age", "LrnThsh")]
  Encountered <- CheckEncouter(P, Capable)
  Learners <- Alive[Capable[Encountered]]
  return(Learners)
}


#' Test Learning Threshold
#'
#' Tests whether males are young enough to learn.
#' @param P a list of parameters
#' @param males the bird trait data.frame from the population of birds ($Males)
#' @keywords song-learning
#' @export
TestLearningThreshold <- function(P, males){
  #Get birds less than threshold
  males$LrnThsh <- ifelse(males$LrnThsh >= 1,
                          males$LrnThsh,
                          (males$LrnThsh-P$VertLrnCut)/(1-P$VertLrnCut))
  DeterminedLearner <- which(males$Age+1 <= males$LrnThsh)
  #Birds less than one year older than their threshold have a chance to learn
  ChanceLearner <- which(males$Age < males$LrnThsh & males$Age+1 > males$LrnThsh)
  Add <- which(runif(length(ChanceLearner)) < males$LrnThsh[ChanceLearner]%%1)
  if(length(Add) > 0){
    DeterminedLearner <- c(DeterminedLearner, ChanceLearner[Add])
  }
  return(DeterminedLearner)
}


#' Check Encouter
#'
#' Tests whether a learner met tutors
#' @param P a list of parameters
#' @param learners indicies of males that are alive and young enough to learn
#' @keywords song-learning
#' @export
CheckEncouter <- function(P, learners){
  #check whether males failed to meet a tutor
  ChanceEncoun <- runif(length(learners))
  return(which(ChanceEncoun < P$EnSuc))
}


#' Choose Tutors
#'
#' Randomly chooses a tutor for each learner.  Tutors must be alive, not be chicks, and must know at least one syllable.  Tutors can be chosen locally or globally.
#' @param P a list of parameters
#' @param population the population of birds
#' @param learners the indicies of birds that will attempt to learn
#' @param vacancy the indicies of  birds that are dead
#' @param misc a matrix of positions of other birds that are excluded for some reason (e.g. already for consensus tutors)
#' @keywords song-learning
#' @export
ChooseTutors <- function(P, population, learners, vacancy, misc=rep(0,length(learners))){
  #Males who cannot be tutors
  SonglessFledge <- unique(c(vacancy, which(population$Males$SylRep == 0),which(population$Males$Age == 0)))
  Common <- matrix(SonglessFledge, nrow=length(learners), ncol=length(SonglessFledge), byrow=TRUE)
  Exclude <- cbind(misc, learners, Common)
  Tutors <- numeric(length(learners))
  if(P$ScopeT){
    #A single male can tutor mutliple young males if he is local to multiple young males
    for(i in seq_along(learners)){
      #get local males, test if available, if not, look further away, then sample ot get tutor
      PotentialTutors <- LocalSearch(P,population, learners[i], Exclude[i,])
      if(length(PotentialTutors)==1){
        Tutors[i] <- PotentialTutors
      }else{
        Tutors[i] <- sample(PotentialTutors, 1)
      }
    }
  }else{#global choice, still allows one male to tutor multiple males
    Tutors <- sapply(seq_along(learners),
                     function(x) sample((1:P$numBirds)[-(Exclude[x,])], 1))
  }
  return(Tutors)
}

#One Tutor Learning core functions
#' One Tutor Learning
#'
#' Allows for birds to learn from one tutor (Add, Add/Forget, or Forget strategies).  It is also called multiple times in the OverLearn strategy, where chicks add syllables from oblique tutors.
#' @param P a list of parameters
#' @param population the population of birds
#' @param tutors the indicies of tutor paired with each learner
#' @param learners the indicies of birds that will attempt to learn
#' @keywords song-learning
#' @export
OneTutorLearning <- function(P, population, tutors, learners){
  #Set up for the Learning process
  LearnerSongs <- matrix(population$MSongs[learners,],ncol=P$MaxRSize, nrow=length(learners))
  TutorSongs <- matrix(population$MSongs[tutors,],ncol=P$MaxRSize, nrow=length(tutors))
  TutorSongs <- ListeningTest(P, TutorSongs, P$LisThrsh)
  if(P$Add){
  #Allow for a rep of up to LisThrsh syllables to learn from
    AddSyllables <- vector(mode="list",length=length(tutors))
  #Which syls do tutors know that learners do not?
    for(i in seq_along(tutors)){
      AddSyllables[[i]] <- which(TutorSongs[i,] > LearnerSongs[i,])
    }
  #Which Tutors have 1+ sylls to teach?
    Teachable <- which(sapply(AddSyllables, length) > 0)
    if(length(Teachable) > 0){
      #handles cases where only one tutor remains
      ModSongs <- matrix(LearnerSongs[Teachable,], nrow=length(Teachable), ncol=P$MaxRSize)
      LearnerSongs[Teachable,] <- LearningProcess(P, ModSongs, AddSyllables[Teachable],
                                                   population$Males$Acc[learners[Teachable]],
                                                   population$Males$ChanceInv[learners[Teachable]])
    }
  }
  if(P$Forget){
    #See DropSyllables for details
    LearnerSongs <- DropSyllables(population$Males$ChanceFor[learners], TutorSongs, LearnerSongs)
  }
  return(LearnerSongs)
}


#' Listening Test
#'
#' Tests which syllables a learner heard from his tutor.
#' @param P a list of parameters
#' @param songs a matrix of tutor syllable vectors
#' @keywords song-learning
#' @export
ListeningTest <- function(P, songs, LisThrsh){
  #Test whether repsize beyond listening threshold
  if(LisThrsh >= .999 && LisThrsh <1){#learn full rep
    return(songs)
  }
  if(LisThrsh%%1 == 0){#set number parameter
    PartialReps <- which(rowSums(songs) > LisThrsh)
    WillLearn <- rep(P$LisThrsh, nrow(songs))

  }else{
    PartialReps <- 1:nrow(songs)
    CanLearn <- rowSums(songs)
    WillLearn <- (CanLearn-P$MinLrnSyl)*LisThrsh+P$MinLrnSyl #main equation
    ChanceLearn <- ifelse(WillLearn%%1 >= runif(length(WillLearn)),1,0) #chance to learn
    WillLearn <- floor(WillLearn) + ChanceLearn
    Overflow <- which(ifelse(WillLearn > CanLearn, TRUE, FALSE))#prevent overlearning
    WillLearn[Overflow] <- CanLearn[Overflow]
    Remove <- which(WillLearn == 0)
    if (length(Remove)> 0){
      WillLearn <- WillLearn[-Remove]
      PartialReps <- PartialReps[-Remove]
    }
  }
  if(length(PartialReps) > 0 ){
    #Sample listening threshold's worth of syls and set the rest to 0
    for(i in seq_along(PartialReps)){
      Save <- sample(which(songs[PartialReps[i],] == 1), WillLearn[i])
      songs[PartialReps[i],-Save] <- 0
    }
  }
  return(songs)
}


#' Drop Syllables
#'
#' Tests whether a learner forgets a syllable that he knows, but that his tutor did not sing.
#' @param chanceFor the learners' chance to forget
#' @param tutorSongs a matrix of tutor syllable vectors
#' @param learnerSongs a matrix of learner syllable vectors
#' @keywords song-learning
#' @export
DropSyllables <- function(chanceFor, tutorSongs, learnerSongs){
  #For birds with a mimic strategy to learning, allows for a chance
  #to drop syllables if present in learner vocab and not what he heard
  #in the tutor song.  Chance to lose/forget/drop based on parameter CtF
  for(i in 1:nrow(learnerSongs)){
    LoseSyllables <- which(learnerSongs[i,] > tutorSongs[i,])
    ChancetoDrop <- runif(length(LoseSyllables))
    Drop <- which((ChancetoDrop < chanceFor[i]))
    learnerSongs[i,LoseSyllables[Drop]] <- 0
  }
  return(learnerSongs)
}


#' Update Song Traits
#'
#' Updates the SylRep and Match traits for learners post learning.
#' @param P a list of parameters
#' @param population the population of birds
#' @param learners the indicies of birds that will attempt to learn
#' @keywords song-leanring
#' @export
UpdateSongTraits <- function(P, population, learners){
  #update params for learners
  if(length(learners) > 1){
    population$Males$SylRep[learners] <- rowSums(population$MSongs[learners,])
  }else{#edge case
    population$Males$SylRep[learners] <- sum(population$MSongs[learners,])
  }
  if(P$MatPref != 0 || P$SMat){
    population$Males$Match[learners] <- TestMatch(P, matrix(population$MSongs[learners,], nrow=length(learners)),
                                                        matrix(population$FSongs[learners,], nrow=length(learners))
                                                  )
  }
  return(population)
}
