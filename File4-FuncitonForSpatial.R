
library("rjson")
library("tidyverse")
#the definition of nv, varF and F is found in https://github.com/boaguilar/multicell_boolean_networks/blob/master/Docs/Two_cell_model_specification.pdf

#Find only nv ----
Find_nv <- function(PCC){
  i = 0
  NodeIDPCC = NULL
  NodeNamePCC= NULL
  InputFunctionPCC = NULL
  for (i in 1:length(PCC[["Model"]][["Variables"]])){
    NodeNamePCC = append(NodeNamePCC,PCC[["Model"]][["Variables"]][[i]][["Name"]])
    InputFunctionPCC = append(InputFunctionPCC,PCC[["Model"]][["Variables"]][[i]][["Formula"]])
    NodeIDPCC = append(NodeIDPCC,PCC[["Model"]][["Variables"]][[i]][["Id"]])
  }
  InfoNodeTable = cbind(NodeIDPCC,NodeNamePCC,InputFunctionPCC)
  LinkIDPCC = NULL
  LinkFrom = NULL
  LinkTo = NULL
  for (i in 1:length(PCC[["Model"]][["Relationships"]])){
    LinkIDPCC = append(LinkIDPCC,PCC[["Model"]][["Relationships"]][[i]][["Id"]])
    LinkFrom = append(LinkFrom,PCC[["Model"]][["Relationships"]][[i]][["FromVariable"]])
    LinkTo= append(LinkTo,PCC[["Model"]][["Relationships"]][[i]][["ToVariable"]])
  }
  InfoLinkTable = cbind(LinkIDPCC,LinkFrom,LinkTo)
  NodeTable = as.data.frame(InfoNodeTable)
  LinkTable = as.data.frame(InfoLinkTable)
  collectingInput <- NULL
  collectingNodeName <- NULL
  count <- 0
  for (i in 1:nrow(NodeTable)){
    count <- 0
    for (j in 1:nrow(LinkTable)){
      if ((LinkTable$LinkTo)[j] == (as.integer(NodeTable$NodeIDPCC))[i]){
        count <- count+1
      } 
    }
    collectingInput[i] <- count
    collectingNodeName[i] <- NodeTable$NodeNamePCC[i]
  }
  
  nv <- rbind(NodeIDPCC,collectingNodeName,collectingInput)
  return(nv)
}

#Find only varF ----
Find_varF <- function(PCC){
  i = 0
  NodeIDPCC = NULL
  NodeNamePCC= NULL
  InputFunctionPCC = NULL
  for (i in 1:length(PCC[["Model"]][["Variables"]])){
    NodeNamePCC = append(NodeNamePCC,PCC[["Model"]][["Variables"]][[i]][["Name"]])
    InputFunctionPCC = append(InputFunctionPCC,PCC[["Model"]][["Variables"]][[i]][["Formula"]])
    NodeIDPCC = append(NodeIDPCC,PCC[["Model"]][["Variables"]][[i]][["Id"]])
  }
  InfoNodeTable = cbind(NodeIDPCC,NodeNamePCC,InputFunctionPCC)
  LinkIDPCC = NULL
  LinkFrom = NULL
  LinkTo = NULL
  for (i in 1:length(PCC[["Model"]][["Relationships"]])){
    LinkIDPCC = append(LinkIDPCC,PCC[["Model"]][["Relationships"]][[i]][["Id"]])
    LinkFrom = append(LinkFrom,PCC[["Model"]][["Relationships"]][[i]][["FromVariable"]])
    LinkTo= append(LinkTo,PCC[["Model"]][["Relationships"]][[i]][["ToVariable"]])
  }
  InfoLinkTable = cbind(LinkIDPCC,LinkFrom,LinkTo)
  NodeTable = as.data.frame(InfoNodeTable)
  LinkTable = as.data.frame(InfoLinkTable)
  collectingInput <- NULL
  collectingNodeName <- NULL
  collectingInteractorID <- list()
  collectingTypeInteraction <- list()
  count <- 0
  InteractorID <- NULL
  TypeInteraction <- NULL
  for (i in 1:nrow(NodeTable)){
    count <- 0
    InteractorID <- NULL
    TypeInteraction <- NULL
    for (j in 1:nrow(LinkTable)){
      if ((LinkTable$LinkTo)[j] == (as.integer(NodeTable$NodeIDPCC))[i]){
        count <- count+1
        InteractorID <- append(InteractorID,(LinkTable$LinkFrom)[j])
        TypeInteraction <- append(TypeInteraction,(LinkTable$LinkType)[j])
      } 
    }
    collectingInput[i] <- count
    collectingNodeName[i] <- NodeTable$NodeNamePCC[i]
    if (length(InteractorID) == 0){
      collectingInteractorID[[i]] <- "No Input"
    }else{
      collectingInteractorID[[i]] <- InteractorID
    }
    if (length(TypeInteraction) == 0){
      collectingTypeInteraction[[i]] <- "No Input"
    }else{
      collectingTypeInteraction[[i]] <- TypeInteraction
    }
  }
  max(collectingInput)
  varF <- matrix(data = -1,nrow = max(collectingInput), ncol = length(NodeIDPCC))
  for (i in 1:length(NodeIDPCC)){
    for (j in 1:length(collectingInteractorID[[i]])){
      if (collectingInteractorID[[i]][j] != "No Input"){
        varF[j,i] = collectingInteractorID[[i]][j]
      }
    }
  }
  return(list(varF =varF,
              collectingNodeName = collectingNodeName))
}

#Find F: 01 Prep - truth_table function and A-Z----
perm<- NULL
truth_table <- function(x) {
  vars <- unique(unlist(strsplit(x, "[^a-zA-Z]+")))
  vars <- vars[vars != ""]
  perm <- expand.grid(rep(list(c(FALSE, TRUE)), length(vars)))
  names(perm) <- vars
  perm[ , x] <- with(perm, eval(parse(text = x)))
  perm
}
#example truth_table("A | B")
#truth_table("A | B | C | D | E | F")
#and A-Z list for later
ForTT <- "A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V, W, X, Y, Z"
ForTTList <- str_split(ForTT, ",")
ForTTvec <- ForTTList[1][[1]]
#create the string function
#just construct a string of the target function from the input node interactions
#if A1 activates B and A2 activate B, we need to get the string "A1 | A2"
#if A1 activates B and A2 inhibit B, we need to get the string "A1 & !A2"
#Find F: 02 Actual Find F code ----
Find_Ftable <- function(PCC){
  i = 0
  NodeIDPCC = NULL
  NodeNamePCC= NULL
  InputFunctionPCC = NULL
  for (i in 1:length(PCC[["Model"]][["Variables"]])){
    NodeNamePCC = append(NodeNamePCC,PCC[["Model"]][["Variables"]][[i]][["Name"]])
    InputFunctionPCC = append(InputFunctionPCC,PCC[["Model"]][["Variables"]][[i]][["Formula"]])
    NodeIDPCC = append(NodeIDPCC,PCC[["Model"]][["Variables"]][[i]][["Id"]])
  }
  InfoNodeTable = cbind(NodeIDPCC,NodeNamePCC,InputFunctionPCC)
  LinkIDPCC = NULL
  LinkFrom = NULL
  LinkTo = NULL
  LinkType = NULL
  for (i in 1:length(PCC[["Model"]][["Relationships"]])){
    LinkIDPCC = append(LinkIDPCC,PCC[["Model"]][["Relationships"]][[i]][["Id"]])
    LinkFrom = append(LinkFrom,PCC[["Model"]][["Relationships"]][[i]][["FromVariable"]])
    LinkTo= append(LinkTo,PCC[["Model"]][["Relationships"]][[i]][["ToVariable"]])
    LinkType = append(LinkType,PCC[["Model"]][["Relationships"]][[i]][["Type"]])
  }
  InfoLinkTable = cbind(LinkIDPCC,LinkFrom,LinkTo,LinkType)
  NodeTable = as.data.frame(InfoNodeTable)
  LinkTable = as.data.frame(InfoLinkTable)
  collectingInput <- NULL
  collectingNodeName <- NULL
  collectingInteractorID <- list()
  collectingTypeInteraction <- list()
  count <- 0
  InteractorID <- NULL
  TypeInteraction <- NULL
  for (i in 1:nrow(NodeTable)){
    count <- 0
    InteractorID <- NULL
    TypeInteraction <- NULL
    for (j in 1:nrow(LinkTable)){
      if ((LinkTable$LinkTo)[j] == (as.integer(NodeTable$NodeIDPCC))[i]){
        count <- count+1
        InteractorID <- append(InteractorID,(LinkTable$LinkFrom)[j])
        TypeInteraction <- append(TypeInteraction,(LinkTable$LinkType)[j])
      } 
    }
    collectingInput[i] <- count
    collectingNodeName[i] <- NodeTable$NodeNamePCC[i]
    if (length(InteractorID) == 0){
      collectingInteractorID[[i]] <- "No Input"
    }else{
      collectingInteractorID[[i]] <- InteractorID
    }
    if (length(TypeInteraction) == 0){
      collectingTypeInteraction[[i]] <- "No Input"
    }else{
      collectingTypeInteraction[[i]] <- TypeInteraction
    }
  }
  collectingTypeInteraction
  max(collectingInput)
  varF <- matrix(data = -1,nrow = max(collectingInput), ncol = length(NodeIDPCC))
  interF <- matrix(data = -1,nrow = max(collectingInput), ncol = length(NodeIDPCC))
  for (i in 1:length(NodeIDPCC)){
    for (j in 1:length(collectingInteractorID[[i]])){
      if (collectingInteractorID[[i]][j] != "No Input"){
        varF[j,i] = collectingInteractorID[[i]][j]
        
      }
      if (collectingTypeInteraction[[i]][j] != "No Input"){
        interF[j,i] = collectingTypeInteraction[[i]][j]
      }
    }
  }
  varF
  interF
  F_list <- list()
  InterFun <- NULL
  char = NULL
  for (i in 1:ncol(varF)){
    smalltable <- rbind(varF[,i],interF[,i])
    for (j in 1:ncol(smalltable)){
      if(j == 1){
        if (interF[j,i] == "Activator"){
          char <- c(ForTTvec[j])
          InterFun[j] <- paste(char, collapse = "")
        }else if(interF[j,i] == "Inhibitor"){
          char <- c("!",ForTTvec[j])
          InterFun[j] <- paste(char, collapse = "")
        }else{
          InterFun[j] <- ""
        }
      }
      else{
      if (interF[j,i] == "Activator"){
        char <- c(" | ", ForTTvec[j])
        InterFun[j] <- paste(char, collapse = "")
      }else if(interF[j,i] == "Inhibitor"){
        char <- c("& !",ForTTvec[j])
        InterFun[j] <- paste(char, collapse = "")
      }else{
        InterFun[j] <- ""
      }
      }
    }
    smalltable2 <- rbind(smalltable,InterFun)
    rt = paste(InterFun, collapse = " ")
    F_list[[i]] <- truth_table(rt)
  }
  F_list
  
  
  F_table = matrix(-1,nrow = 2^max(collectingInput), ncol = length(NodeIDPCC))
  for (k in 1:length(F_list)){
    F_toFill <- as.numeric(F_list[[k]][,ncol(F_list[[k]])])
    if (length(F_toFill) != 0){
      F_table[1:length(F_toFill),k] <- F_toFill
    }
  }
  return(F_table)
}

#Checking ----
Find_nv(PCC)
Find_varF(PCC)
Find_Ftable(PCC)

check = 1 #select the node number (ID still don't start from 0,1,...)
collectingNodeName[check] #the identity of the selected node
collectingInput[check] #the number of nodes that affect the selected node (called variables)
varF[,check] #ID of the input nodes (variables), -1 is just the fillings
collectingNodeName[which(NodeIDPCC %in%  as.numeric(varF[,check]))] #Identity of the input nodes (variables)
interF[,check] #Type of interactions between the input nodes (variables) and the selected node

Find_nv(PCC)[,check] # "collectingInput" is equivalent to nv
Find_varF(PCC)[["varF"]][,check]
Find_Ftable(PCC)[,check]

#Put all three functions in one function that output all three file: Find_nv_varF_F ----
Find_nv_varF_F <- function(PCC){
  i = 0
  #1.1 Extract Variables 
  NodeIDPCC = NULL
  NodeNamePCC= NULL
  InputFunctionPCC = NULL
  for (i in 1:length(PCC[["Model"]][["Variables"]])){
    NodeNamePCC = append(NodeNamePCC,PCC[["Model"]][["Variables"]][[i]][["Name"]])
    InputFunctionPCC = append(InputFunctionPCC,PCC[["Model"]][["Variables"]][[i]][["Formula"]])
    NodeIDPCC = append(NodeIDPCC,PCC[["Model"]][["Variables"]][[i]][["Id"]])
  }
  InfoNodeTable = cbind(NodeIDPCC,NodeNamePCC,InputFunctionPCC)
  #1.2 Extract Links 
  LinkIDPCC = NULL
  LinkFrom = NULL
  LinkTo = NULL
  LinkType = NULL
  for (i in 1:length(PCC[["Model"]][["Relationships"]])){
    LinkIDPCC = append(LinkIDPCC,PCC[["Model"]][["Relationships"]][[i]][["Id"]])
    LinkFrom = append(LinkFrom,PCC[["Model"]][["Relationships"]][[i]][["FromVariable"]])
    LinkTo= append(LinkTo,PCC[["Model"]][["Relationships"]][[i]][["ToVariable"]])
    LinkType = append(LinkType,PCC[["Model"]][["Relationships"]][[i]][["Type"]])
  }
  InfoLinkTable = cbind(LinkIDPCC,LinkFrom,LinkTo,LinkType)
  
  NodeTable = as.data.frame(InfoNodeTable)
  LinkTable = as.data.frame(InfoLinkTable)
  #2. Find nv and varF
  collectingInput <- NULL
  collectingNodeName <- NULL
  collectingInteractorID <- list() #this is a list as the dim is not equal for each node
  collectingTypeInteraction <- list()
  count <- 0
  InteractorID <- NULL
  TypeInteraction <- NULL
  #2.1 for each existing node i 
  for (i in 1:nrow(NodeTable)){
    collectingNodeName[i] <- NodeTable$NodeNamePCC[i] #collect the identity (name) of node i
    count <- 0
    InteractorID <- NULL
    TypeInteraction <- NULL
    #2.1.1 going through LinkTable and see "LinkTo", if the ID match NodeIDPCC 
    for (j in 1:nrow(LinkTable)){
      if ((LinkTable$LinkTo)[j] == (as.integer(NodeTable$NodeIDPCC))[i]){ 
        count <- count+1 #nv increased by 1 each time the match found
        #collect that ID in InteractorID and interaction type in TypeInteraction
        InteractorID <- append(InteractorID,(LinkTable$LinkFrom)[j]) 
        TypeInteraction <- append(TypeInteraction,(LinkTable$LinkType)[j])
      } 
    }
    collectingInput[i] <- count #this is nv for node i
    #and collectingInput is nv for this boolean network
    #2.1.2 this is the list of the ID of input nodes for the node i
    if (length(InteractorID) == 0){
      collectingInteractorID[[i]] <- "No Input"
    }else{
      collectingInteractorID[[i]] <- InteractorID
    }
    #2.1.3 this is the list of the interaction types for the node i
    if (length(TypeInteraction) == 0){
      collectingTypeInteraction[[i]] <- "No Input"
    }else{
      collectingTypeInteraction[[i]] <- TypeInteraction
    }
  }
  #collectingInteractorID, size of this list = number of nodes
  #collectingTypeInteraction, size of this list = number of nodes
  #max(collectingInput) = the max number of inputs for all the nodes
  #these will be used to make varF and interF
  #2.2 create varF and interF (will be used later for TT)
  varF <- matrix(data = -1,nrow = max(collectingInput), ncol = length(NodeIDPCC))
  interF <- matrix(data = -1,nrow = max(collectingInput), ncol = length(NodeIDPCC))
  for (i in 1:length(NodeIDPCC)){
    for (j in 1:length(collectingInteractorID[[i]])){
      if (collectingInteractorID[[i]][j] != "No Input"){
        varF[j,i] = collectingInteractorID[[i]][j]
        #varF is varF for this boolean network
      }
      if (collectingTypeInteraction[[i]][j] != "No Input"){
        interF[j,i] = collectingTypeInteraction[[i]][j]
      }
    }
  }
  
  #3. Find F
  F_list <- list()
  InterFun <- NULL 
  char <- NULL
  #3.1 for each existing node i
  for (i in 1:ncol(varF)){
    smalltable <- rbind(varF[,i],interF[,i]) #for each node
    for (j in 1:ncol(smalltable)){
      if(j == 1){ #for the first input variables 
        if (interF[j,i] == "Activator"){ #if the type of interaction is Activator
          char <- c(ForTTvec[j]) #char is A 
          InterFun[j] <- paste(char, collapse = "") #collect it so we know what Nodes are A,B,...
        }else if(interF[j,i] == "Inhibitor"){ #if the type of interaction is Inhibitor
          char <- c("!",ForTTvec[j]) #char is !A 
          InterFun[j] <- paste(char, collapse = "")
        }else{ #if the type of interaction -1
          InterFun[j] <- "" #not placing any character
        }
      }
      else{ #for the other input variables, do similar things, but 
        if (interF[j,i] == "Activator"){ #for activator
          char <- c(" | ", ForTTvec[j]) 
          InterFun[j] <- paste(char, collapse = "") #add "|" in front of the letter
        }else if(interF[j,i] == "Inhibitor"){ #for inhibitor
          char <- c("& !",ForTTvec[j])
          InterFun[j] <- paste(char, collapse = "") #add "& !" in front of the letter
        }else{
          InterFun[j] <- ""
        }
      }
    }
    smalltable2 <- rbind(smalltable,InterFun) #just to check if needed
    #3.2 get the final string to be used with truth_table() function
    rt = paste(InterFun, collapse = " ") #rt is the string form we need e.g. A|B|C or A&!B 
    F_list[[i]] <- truth_table(rt) #call the function truth_table, feeding rt as the input 
  }
  #F_list, again , the size of this list = number of nodes
  #3.3 Combine each element in F_list into the format of F the model want
  F_table = matrix(-1,nrow = 2^max(collectingInput), ncol = length(NodeIDPCC))
  for (k in 1:length(F_list)){
    F_toFill <- as.numeric(F_list[[k]][,ncol(F_list[[k]])])
    if (length(F_toFill) != 0){
      F_table[1:length(F_toFill),k] <- F_toFill
    }
  }
  return(list(nv = collectingInput,
              varF = varF,
              FF = F_table,
              NodeTable_df = NodeTable,
              LinkTable_df = LinkTable))
}


#ReIDnode: give new ID number so that the spatial model work better at the level of output ----
#4.1 new ID formed from 0 to n-1 (n = the number of nodes)
ReIDnode<- function(Testall){
  NodeTable <- Testall[["NodeTable_df"]]
  varFori <- Testall[["varF"]]
  varF<- Testall[["varF"]]
  NodeTable["newID"] <- as.character(0:(nrow(NodeTable)-1))
  #4.2 going through varF,
  for (i in 1:nrow(varF)){
    for (j in 1:ncol(varF)){
      if (varF[i,j] != "-1"){ #if -1, skip
        for (k in 1:nrow(NodeTable)){ 
          if (NodeTable$NodeIDPCC[k] == varF[i,j]){ #find a match between the old NodeIDPCC (in a loop) to the varF[i,j]
            varF[i,j] <- NodeTable$newID[k] #if match, replace the varF[i,j] with newID[k]
          } 
        }
      }
    }
  }
  return(list(varF = varF,
              NodeTable = NodeTable,
              varFori = varFori))
}


#for any BMA output file (.JSON) ----
PCC <- fromJSON(file="/Users/panareeboonyuen/Desktop/MIB Sysbio/MIniproject/RealModel/Day5/Final_PCC_modified.json")
PSC<-fromJSON(file="/Users/panareeboonyuen/Desktop/MIB Sysbio/MIniproject/RealModel/Day5/Final_PSC_modified.json")
Mcell <- fromJSON(file = "/Users/panareeboonyuen/Desktop/MIB Sysbio/MIniproject/RealModel/Day4/FinalMcell.json")
Thcell <- fromJSON(file = "/Users/panareeboonyuen/Desktop/MIB Sysbio/MIniproject/RealModel/Day3/Final_Thcell.json")

#Writing file out function ----
OUT <- PCC
Write_nv_varF_F <- function(OUT){
  stringname <- OUT[["Model"]][["Name"]]
  namenv <- paste(c(paste0(stringname),"_nv_fn.csv"), collapse = "")
  namevarF <- paste(c(paste0(stringname),"_varF_fn.csv"), collapse = "")
  nameF <- paste(c(paste0(stringname),"_FF_fn.csv"), collapse = "")
  nameNodeTable <- paste(c(paste0(stringname),"_NodeTable_fn.csv"), collapse = "") 
  write.csv(Find_nv_varF_F(OUT)[["FF"]], paste0(nameF)) #write F  
  write.csv(ReIDnode(Find_nv_varF_F(OUT))[["varF"]] , paste0(namevarF)) #write reIDed varF
  write.csv(ReIDnode(Find_nv_varF_F(OUT))[["NodeTable"]], paste0(nameNodeTable)) #write node information
  write.csv(Find_nv_varF_F(OUT)[["nv"]],  paste0(namenv)) #write nv
}


