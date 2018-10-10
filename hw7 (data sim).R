# Preliminary variables --------------------------------------------------------
#Mammal species
mammspecs <- data.frame(as.character(seq(1:10)))
colnames(mammspecs) <- "mamm.specs"

#assign fake genera. This will be important later,
#ectos tend to be phylogenetically conserved
mammspecs$genus <- c(rep("genus1", 5), rep("genus2", 5))

#Ecto species
ectospecs <- data.frame(as.character(seq(1:12)))
colnames(ectospecs) <- "ecto.specs"
#Twelve ectoparasite species is a low-ish number, but it'll do

#Assign ectos as generalist, specialist, or neither
ectospecs$life.history <- c(rep("generalist", 4), rep("specialist", 4), 
                            rep("neither", 4))

#Assign occupancy probabilities to each ecto -------------------------------------
for(i in 1:length(ectospecs$ecto.specs)){
  occprobs <- matrix(NA, nrow = length(ectospecs$ecto.specs),
                     ncol = length(mammspecs$mamm.specs))
  
  #Assign a mammal genus to the ecto species
  genus <- sample(mammspecs$genus, size = length(ectospecs$ecto.specs),
                  replace = T)
  
  #Generalist species: can occupy all species in a genus
  if(ectospecs$life.history[i] == "generalist"){
    #Generate occupancy probs for all mamm in the genus
    for(j in 1:length(mammspecs$mamm.specs)){
      if(mammspecs$genus[j] == genus[i]){
        occprobs[i,j] <- runif(1, 0, 1)
      } else{
        occprobs[i,j] <- 0
      }
    }
    
  }
}
