# --------------------
# Imports
# --------------------

partnersData <- read.csv("data/Partners-Table 1.csv")
partnersProjects <- read.csv("data/Projects-Table 1.csv")
partnersNodes <- read.csv("data/partners [Nodes].csv")
colnames(partnersNodes)[colnames(partnersNodes)=="label"] <- "project"
partners <- merge(partnersData, partnersNodes, all.x = TRUE)

# --------------------
# Functions
# --------------------

# Prepare list of edges based on the data
get.edges <- function(partners) {
  partnerSplit <- split(partners, partners$partner)
  partnersEdges = data.frame(Source = integer(), Target = integer(), Label = character())
  for (p in names(partnerSplit)) {
    projects = partnerSplit[[p]]
    var1 = projects[, c('phase', 'id')]
    colnames(var1) <- c('phase1', 'id1')
    var2 = projects[, c('phase', 'id')]
    colnames(var2) <- c('phase2', 'id2')
    edges = merge(var1, var2)
    edges = edges[which(edges['id1'] != edges['id2']),]
    if (nrow(edges) > 0) {
      toAdd = edges[, c('id1', 'phase1', 'id2', 'phase2')]
      colnames(toAdd) <- c('Source', 'Phase Source', 'Target', 'Phase Target')
      toAdd['Label'] = p
      partnersEdges = rbind(partnersEdges, toAdd)
    }
  }
  partnersEdges['Type'] = 'Undirected'
  partnersEdges
}

# Compute degrees of connectivity
get.connectivity <- function(partnersEdges) {
  acceleratorConnectivity = data.frame(Id = integer(), Phase1 = integer(), Phase2 = integer(), Phase3 = integer(), Phase1and2 = integer(), Phase1or2 = integer(), PhaseAll = integer())
  acceleratorsEdges = partnersEdges[which(partnersEdges['Phase Source'] == 3),]
  connectivitySplit <- split(acceleratorsEdges, acceleratorsEdges$Source)
  for (s in names(connectivitySplit)) {
    connections = connectivitySplit[[s]]
    partnerSplit = split(connections, connections$Label)
    phase1and2count = 0
    phase1or2count = 0
    for (partner in names(partnerSplit)) {
      phases = partnerSplit[[partner]]
      tp = phases['Phase Target']
      if ((sum(tp == 1) > 0) && (sum(tp == 2) > 0)) phase1and2count = phase1and2count+1
      if ((sum(tp == 1) > 0) || (sum(tp == 2) > 0)) phase1or2count = phase1or2count+1
    }
    targetPhase = connections['Phase Target']
    new = data.frame(
      Id = c(s),
      Phase1 = c(sum(targetPhase == 1)),
      Phase2 = c(sum(targetPhase == 2)),
      Phase3 = c(sum(targetPhase == 3)),
      Phase1and2 = c(phase1and2count),
      Phase1or2 = c(phase1or2count),
      PhaseAll = c(nrow(targetPhase))
    )
    acceleratorConnectivity = rbind(acceleratorConnectivity, new)
  }
  acceleratorConnectivity
}

# --------------------
# Computations
# --------------------

partnersEdges = get.edges(partners)
write.csv(partnersEdges, file="output/partnersEdges.csv")

acceleratorConnectivity = get.connectivity(partnersEdges)
colnames(partnersNodes) <- c('Id', 'Accelerator')
acceleratorConnectivity = merge(acceleratorConnectivity, partnersNodes)
write.csv(acceleratorConnectivity, file="output/acceleratorConnectivity.csv")
acceleratorConnectivity = merge(acceleratorConnectivity, partnersProjects)

toAdd = acceleratorConnectivity[c('Help', 'Phase1', 'Phase2', 'Phase3', 'Phase1and2', 'Phase1or2', 'PhaseAll')]
db = merge(db, toAdd, all.x = TRUE)
remove(toAdd)
db$Phase1[is.na(db$Phase1)] <- 0
db$Phase2[is.na(db$Phase2)] <- 0
db$Phase3[is.na(db$Phase3)] <- 0
db$Phase1and2[is.na(db$Phase1and2)] <- 0
db$Phase1or2[is.na(db$Phase1or2)] <- 0
db$PhaseAll[is.na(db$PhaseAll)] <- 0
