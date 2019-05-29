#Functions for doing statistics and plotting
#General Plotting

#' Quick SEM Plot
#'
#' A method that plots whatever data was saved in the path location.  It takes the column averages, so it works for Basic, Light, and Insult Sims, but not for Invasion Sims.  For trait plots, black lines are the average, dark grey is the inner 50% and light grey is 100% of the values.  For syllable plots, darker means that a syllable is more common.
#' @param P a list of parameters
#' @param path  location of a folder with simulation data
#' @param rep whether to plot repertoire size data
#' @param acc whether to plot accuracy data
#' @param lrnThsh whether to plot learning threshold data
#' @param match whether to plot matching data
#' @param chanceInv whether to plot chance to invent data
#' @param chanceFor whether to plot chance to forget data
#' @param age whether to plot age data
#' @param mSong whether to plot male song data
#' @param fSong whether to plot  female song data
#' @param autoLayout whether to allow the function to figure out the layout (TRUE) or not (FALSE)
#' @param xlab x-axis label for plot()
#' @param thin how often to sample a step of song data for the SongEvolve() plots; This is graphically intensive when there are a lot of syllables (default is 500), so ideally do not plot more than 100-200 time steps.
#' @keywords stats-plotting
#' @export
QuickSEMPlot <- function(P, path, rep=TRUE, acc=P$SAcc, lrnThsh=P$SLrn, match=P$SMat,
                      chanceInv=P$SCtI, chanceFor=P$SCtF, age=P$SAge,
                      mSong=P$SMSng, fSong=P$SFSng, autoLayout=TRUE,
                      xlab="Time Steps", thin=10){

  if(autoLayout == TRUE){#decide on plot configuration
    NumPlot <- sum(rep, acc, lrnThsh, match, chanceInv,
                   chanceFor, age, mSong, fSong)
    if(NumPlot == 1){
      Config <- c(1,1)
    }else if(NumPlot == 2){
      Config <- c(2,1)
    }else if(NumPlot %in% c(3,4)){
      Config <- c(2,2)
    }else if(NumPlot %in% c(5,6)){
      Config <- c(3,2)
    }else{Config <- c(3,3)}
    par(mfrow=Config, mgp=c(2,.5,0), mar=c(3.2,3.3,1,1), las=1)
  }


  if(rep==TRUE){
    SylRep <- read.csv(file.path(path,"SylRep.csv"))
    TraitPlot(SylRep, xlab, ylab="Average Syllable Repertoire")
  }
  if(acc==TRUE){
    Acc <- read.csv(file.path(path,"Acc.csv"))
    TraitPlot(Acc, xlab, ylab="Average Accuracy")
  }
  if(lrnThsh==TRUE){
    LrnThsh <- read.csv(file.path(path,"LrnThsh.csv"))
    TraitPlot(LrnThsh, xlab, ylab="Average Learning Threshold")
  }
  if(match==TRUE){
    Match <- read.csv(file.path(path,"Match.csv"))
    TraitPlot(Match, xlab, ylab="Average Match")
  }
  if(chanceInv==TRUE){
    ChanceInv <- read.csv(file.path(path,"ChanceInv.csv"))
    TraitPlot(ChanceInv, xlab, ylab="Average Chance to Invent")
  }
  if(chanceFor==TRUE){
    ChanceFor <- read.csv(file.path(path,"ChanceFor.csv"))
    TraitPlot(ChanceFor, xlab, ylab="Average Chance to Forget")
  }
  if(age==TRUE){
    Age <- read.csv(file.path(path,"Age.csv"))
    TraitPlot(Age, xlab, ylab="Average Age")
  }
  if(mSong==TRUE){
    MSongs <- read.csv(file.path(path,"MSong.csv"))
    SongPlot(P, MSongs, thin, TRUE)
  }
  if(fSong==TRUE){
    FSongs <- read.csv(file.path(path,"FSong.csv"))
    SongPlot(P, FSongs, thin, FALSE)
  }
}

#' Trait Plot
#'
#' Plots averages for a bird trait.  Black lines are the average, dark grey is the inner 50% and light grey is 100% of the values.  For syllable plots, darker red means that a syllable is more common.
#' @param trait a trait matrix to plot
#' @param xlab x-axis label for plot()
#' @param ylab y-axis label for plot()
#' @keywords stats-plotting
#' @export
TraitPlot <- function(trait, xlab="Time Steps", ylab){
  #processing
  Maxs <- apply(trait, 2, max)
  Mins <- apply(trait, 2, min)
  Top50 <- apply(trait, 2, quantile, probs=.75)
  Bottom50 <- apply(trait, 2, quantile, probs=.25)
  Means <- colMeans(trait)
  All <- 1:ncol(trait)
  First <- 1:(ncol(trait)-1)
  Second <- 2:ncol(trait)

  #main
  plot(0, type='n', ylab=ylab,
       xlab=xlab, font.lab=2, ylim=c(0,max(Maxs)),
       xlim = c(0, ncol(trait)),
       cex.lab=1.2)

  #100%, 50%, average
  polygon(x=c(All, rev(All)), y=c(Maxs, rev(Mins)), col="grey90", border=NA)
  polygon(x=c(All, rev(All)), y=c(Top50, rev(Bottom50)), col="grey80", border=NA)
  segments(First, Means[First], Second, Means[Second])
}


#' Song Plot
#'
#' Shows the prevalence of each syllable across time.  Darker color means that a syllable is more common.
#' @param P a list of parameters
#' @param songs male or female song data from  simulation
#' @param thin how often to sample a step of song data for the SongEvolve() plots; This is graphically intensive when there are a lot of syllables (default is 500), so ideally do not plot more than 100-200 time steps.
#' @param male whether male songs are being plotted; affects the y-axis label
#' @keywords stats-plotting
#' @export
SongPlot <- function(P, songs, thin = 10, male=TRUE){
  if(male){
    Type <- "Male"
  }else{
    Type <- "Female"
  }
  songs <- songs[,seq(1,ncol(songs),by=thin)]/P$numBirds
  plot(0, type="n",ylim=c(P$MaxRSize, 1), xlim=c(1, P$nSim/thin),
       xlab=paste0("TimeStep/",thin), ylab=paste(Type, "Syllables"), font.lab=2,
       cex.lab=1.2)
  for(i in 1:(1+ P$nSim/thin)){
    for(j in 1:P$MaxRSize){
      rect(i-1, j-1, i, j, col=rgb(0, 0, 0, songs[j,i]), border=NA)
    }
  }
}


#' Territory Heat Map
#'
#' Creates a heat map showing the magnitude of a trait in each territory for a given timestep.  Requires individual data.
#' @param P a list of parameters
#' @param index which column to plot
#' @param trait a matrix of SEM data from a Basic sim (individual data)
#' @keywords stats-plotting
#' @export
TerritoryHeatMap <- function(P, index=1, trait, max=NA){
  if(is.na(max)){
    max <- max(trait[,index])
  }
    Matter <- matrix(trait[,index]/max, nrow=P$R)
    plot(0,type="n", ylim=c(P$C,0), xlim=c(0,P$R),
         ylab="", xlab="")
    for(i in 1:P$R){#x
      for(j in 1:P$C){#y
        rect(i-1, j-1, i, j, col=rgb(0,0,0,Matter[i,j]), border=NA)
      }
    }
  }

#Plotting lineages (requires saving Name and FatherName UUIDs)
#' Family Tree Plot
#'
#' Experimental plot that shows which birds sired which offspring over time.  It starts from the tips, so all but one of the original lineage will be lost after enough time steps have passed.
#' @param path path to a fodl with 2 matricies of bird UIDs.
#' @param byGens whether to plots the y axis by generation (TRUE) or timestep (FALSE)
#' @keywords stats-plotting
#' @export
FamilyTreePlot <- function(path, byGens=TRUE){
  Name <- read.csv(file.path(path, 'Name.csv'), stringsAsFactors = FALSE)
  FatherName <- read.csv(file.path(path, 'FatherName.csv'), stringsAsFactors = FALSE)
  Gens <- ncol(Name)
  Terrs <- nrow(Name)
  FamilyTree <- as.list(rep(NA,Gens))

  #Get the chains from progeny to ancestors
  for(i in 1:Terrs){
    Son <- i
    Chain <- numeric(Gens)
    Chain[Gens] <- Son
    BookMark <- Gens
    for(j in (Gens-1):1){
      Father <- which(Name[j]==FatherName[Son,BookMark])
      if(length(Father)==0){next}
      Chain[j] <- Father
      Son <- Father
      BookMark <- j
    }
    if(byGens==TRUE){
      Chain <- Chain[which(Chain !=0)]
      YLab <- "Generations"
    }else{
      for(j in rev(seq_along(Chain))){
        if(Chain[j] == 0){Chain[j] <- Chain[j+1]}
      }
      YLab <- "TimeSteps"
    }
    FamilyTree[[i]] <- Chain
  }
  #Set up for plotting
  Founders <- numeric(length(FamilyTree))
  NumProgeny <- numeric(length(FamilyTree))
  for(i in seq_along(FamilyTree)){
    Founders[i] <- FamilyTree[[i]][1]
    NumProgeny[i] <- length(FamilyTree[[i]])
  }
  Roots <- sort(unique(Founders))

  #Main plotting
  par(bg="grey40", mar=c(3,3,1,1), mgp=c(1.5,.5,0))
  plot(0,xlim=c(1,400), ylim=c(max(NumProgeny),1), type='n',
       ylab=YLab, xlab="Bird's Matrix Position", font.lab=2)
  col <- c("#fff7f3","#fde0dd","#fcc5c0","#fa9fb5","#f768a1","#dd3497","#ae017e","#7a0177","#49006a")
  #col <- c('#d73027','#f46d43','#fdae61','#fee08b','#ffffbf','#d9ef8b','#a6d96a','#66bd63','#1a9850')
  for(i in seq_along(FamilyTree)){
    RoCol <- which(Roots == FamilyTree[[i]][1])
    ind <- length(FamilyTree[[i]])
    segments(FamilyTree[[i]][1:(ind-1)],1:(ind-1),FamilyTree[[i]][2:ind],2:ind,
             col=col[RoCol%%length(col)+1], lwd=2)
  }
}


#Cluster Plotting

#' Quick Cluster Plots
#'
#' Creates Cluster Plotss for all saved data except MaleSongs and FemaleSongs.  See ClusterPlot for info on plot interpretation.
#' @param P a list of parameters
#' @param path location of a folder with simulation data
#' @param rep whether to plot repertoire size data
#' @param acc whether to plot accuracy data
#' @param lrnThsh whether to plot learning threshold data
#' @param match whether to plot matching data
#' @param chanceInv whether to plot chance to invent data
#' @param chanceFor whether to plot chance to forget data
#' @param age whether to plot age data
#' @param autoLayout whether to allow the function to figure out the layout (TRUE) or not (FALSE)
#' @family Cluster Plots
#' @keywords stats-plotting
#' @export
QuickClusterPlot <- function(P, path, rep=TRUE, acc=P$SAcc, lrnThsh=P$SLrn, match=P$SMat,
                         chanceInv=P$SCtI, chanceFor=P$SCtF, age=P$SAge, AutoLayout=TRUE){
  if(AutoLayout == TRUE){
    numPlot <- sum(c(rep, acc, lrnThsh, match, chanceInv, chanceFor, age))
    if(numPlot == 1){
      config <- c(1,1)
    }else if(numPlot == 2){
      config <- c(2,1)
    }else if(numPlot %in% c(3,4)){
      config <- c(2,2)
    }else if(numPlot %in% c(5,6)){
      config <- c(3,2)
    }else{config <- c(3,3)}
    par(mfrow=config, mgp=c(1.5,.5,0), mar=c(2.7,2.7,1,1))
  }
  if(rep==TRUE){
    SylRep <- read.csv(file.path(path,"SylRep.csv"))
    ClusterPlot(P,SylRep)
  }
  if(acc==TRUE){
    Acc <- read.csv(file.path(path,"Acc.csv"))
    ClusterPlot(P,Acc)
  }
  if(lrnThsh==TRUE){
    LrnThsh <- read.csv(file.path(path,"LrnThsh.csv"))
    ClusterPlot(P,LrnThsh)
  }
  if(match==TRUE){
    Match <- read.csv(file.path(path,"Match.csv"))
    ClusterPlot(P,Match)
  }
  if(chanceInv==TRUE){
    ChanceInv <- read.csv(file.path(path,"ChanceInv.csv"))
    ClusterPlot(P,ChanceInv)
  }
  if(chanceFor==TRUE){
    ChanceFor <- read.csv(file.path(path,"ChanceFor.csv"))
    ClusterPlot(P,ChanceFor)
  }
  if(age==TRUE){
    Age <- read.csv(file.path(path,"Age.csv"))
    ClusterPlot(P,Age)
  }
}

#' Cluster Calculation
#'
#' Calculates the cluster score of a matrix.
#' @param P a list of parameters
#' @param matrix a saved trait from the Basic sims (requires individual data)
#' @family Cluster Plots
#' @keywords stats-plotting
#' @export
ClusterCalc <- function(P, matrix){
  #Calculate the cluster score; lower numbers = more clustered
  #trait is currently in vector format, but these calculations treat it
  #as the matrix it would be if it were reconfigured into a matrix (byrows=FALSE)
  South <- sum(abs(matrix[-(1:P$C*P$R)] - matrix[-seq(1,P$numBirds, by=P$R)])) #sum of rows 1:R-1 - sum of rows 2:R
  East <- sum(abs(matrix[-((P$R*(P$C-1)+1):P$numBirds)] - matrix[-(1:P$R)])) #sum of cols 1:C-1 - sum of cols 2:C
  ClusterScore <- South+East
  return(ClusterScore)
}


#' Cluster Plot
#'
#' Cluster Plots are normalized such that the minimal score (a smooth gradient) is zero.  The black line shows the maximal score (a checkerboard pattern) while the grey line shows the average score (no pattern).  Green line plots the score of the real data over time.  The function also prints the mean probability of getting the real values given the Min, Max, and Mean values for the matrix at each timestep.
#' @param P a list of parameters
#' @param trait a saved trait from the Basic sims (requires individual data)
#' @family Cluster Plots
#' @keywords stats-plotting
#' @export
ClusterPlot <- function(P, trait){
  time <- proc.time()
  Timesteps <- length(trait)
  Real <- numeric(Timesteps)
  Max <- Real
  Min <- Real
  UnReal <- matrix(0, ncol=5, nrow=length(trait))
  for(i in seq_along(trait)){
    MaxMat <- GetMaxMat(trait[,i], P$R, P$C)
    MinMat <- sort(trait[,i])
    Real[i] <- ClusterCalc(P,trait[,i])
    for(j in 1:5){
      UnReal[i,j] <- ClusterCalc(P,trait[sample(P$numBirds, P$numBirds),i])
    }
    Max[i] <- ClusterCalc(P,MaxMat)
    Min[i] <- ClusterCalc(P,MinMat)
  }
  #get pvalues
  UnReal <- rowMeans(UnReal)
  print(mean(ppert(Real, min=Min, mode=UnReal, max=Max)))

  #"Background subtract" the miinumum values and plot
  UnReal <- UnReal-Min
  Real <- Real-Min
  Max <- Max-Min
  plot(0,type="n", xlim=c(1,Timesteps),
       ylim=c(min(Real,Max,Min), max(Real,Max,Min)),
       xlab="TimeSteps", ylab=paste(deparse(substitute(trait)),"Score"), font.lab=2)
  segments(1:(Timesteps-1), Real[1:(Timesteps-1)],
           2:Timesteps, Real[2:Timesteps], col="grey50",
           lwd=2)
  segments(1:(Timesteps-1), Max[1:(Timesteps-1)],
           2:Timesteps, Max[2:Timesteps], col="Black",
           lwd=2)

  segments(1:(Timesteps-1), UnReal[1:(Timesteps-1)],
           2:Timesteps, UnReal[2:Timesteps], col="green",
           lwd=1)
  return(proc.time()-time)
}


#' Get Max Matrix
#'
#' Creates a matrix that has the maximum score given the trait data.
#' @param trait a saved trait from the Basic sims (requires individual data)
#' @param R the rows in the bird matrix
#' @param C the columns in the bird matrix
#' @keywords stats-plotting
#' @family Cluster Plots
#' @export
GetMaxMat <- function(trait, R, C){
  trait <- sort(trait)
  N <- length(trait)
  MaxMat <- numeric(N)

  #Center index
  CentNeed <- ((R-2)*(C-2))/2
  Upper <- floor(CentNeed)
  Lower <- N-ceiling(CentNeed)+1
  SmallCenterPieces <- 1:Upper
  LargeCenterPieces <- Lower:N
  #Edge index
  EdgeNeed <- ((R-2) + (C-2))
  if(R%%2 == 1 && C%%2 == 1){
    EdgeNeed <- c(EdgeNeed+2, EdgeNeed-2)
  }else{EdgeNeed <- c(EdgeNeed, EdgeNeed)}
  SmallEdgePieces <- (Upper+1):(Upper + EdgeNeed[1])
  LargeEdgePieces <- (Lower - EdgeNeed[2]):(Lower-1)
  #Corner index
  Remainder <-(1:N)[-c(SmallCenterPieces, LargeCenterPieces,
                       SmallEdgePieces, LargeEdgePieces)]

  #edge positions
  West <- 2:(R-1)
  South <- R*(2:(C-1))
  East <- R*C-((R-2):1)
  North <- R*(1:(C-2))+1


  #Corner Positions
  Corners <- c(1,R, R*C-R+1, R*C)
  #Center positions
  Centers <- (1:N)[-c(North, South, East, West, Corners)]

  if(R%%2 == 1 && C%%2 == 1){
    MaxMat[Corners] <- trait[rev(Remainder)]
    s <- rep(1,4)
    l <- rep(2,4)
  }else if(R%%2 == 0 && C%%2 == 0){
    MaxMat[Corners] <- trait[Remainder[c(3,1,2,4)]]
    s <- c(1,2,2,1)
    l <- c(2,1,1,2)
    #SE
  }else if(R%%2 == 1 && C%%2 == 0){
    MaxMat[Corners] <- trait[Remainder[c(3,4,1,2)]]
    s <- c(1,1,2,1)
    l <- c(2,2,1,2)
    #E
  }else{
    MaxMat[Corners] <- trait[Remainder[c(3,1,4,2)]]
    s <- c(1,2,1,1)
    l <- c(2,1,2,2)
    #S
  }

  SmallPos <- c(West[seq(s[1],length(West),2)],South[seq(s[2],length(South),2)],
                East[seq(s[3],length(East),2)],North[seq(s[4],length(North),2)])
  LargePos <- c(West[seq(l[1],length(West),2)],South[seq(l[2],length(South),2)],
                East[seq(l[3],length(East),2)],North[seq(l[4],length(North),2)])
  MaxMat[c(SmallPos, LargePos)] <- trait[c(SmallEdgePieces, LargeEdgePieces)]

  #center
  PreFlip <- matrix(sapply(1:(R-2), "+", ((R-2)*(seq(1,(C-2)-1,2)))), nrow=(C-2)/2)
  Flip <- matrix(apply(PreFlip, 1, rev), nrow=nrow(PreFlip), byrow = TRUE)
  Centers[as.vector(PreFlip)] <- as.vector(Centers[Flip])

  MaxMat[Centers[seq(1,length(Centers),2)]] <- trait[LargeCenterPieces]
  MaxMat[Centers[seq(2,length(Centers),2)]] <- trait[SmallCenterPieces]
  return(MaxMat)
}
