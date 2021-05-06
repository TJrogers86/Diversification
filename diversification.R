library(ape)
library(TreeSim)
library(geiger)
library(diversitree)

my.tree <- TreeSim::sim.bd.taxa(n=300, numbsim=1, lambda=0.1, mu=0)[[1]]
plot(my.tree)
ape::ltt.plot(my.tree)
ape::ltt.plot(my.tree, log= "y")

yule.trees <- TreeSim::sim.bd.taxa(n=300, numbsim=1, lambda=0.1, mu=2, complete=FALSE)
ape::mltt.plot(yule.trees)
ape::ltt.plot(yule.trees, log= "y")

bd.trees <- TreeSim::sim.bd.taxa(n=300, numbsim=10, lambda=1.9, mu=0, complete=FALSE)
ape::mltt.plot(bd.trees, log="y", legend=FALSE)

depth.range <- range(unlist(lapply(yule.trees,ape::branching.times)), unlist(lapply(bd.trees,ape::branching.times)))
max.depth <- sum(abs(depth.range)) #ape rescales depths
plot(x=c(0, -1*max.depth), y=c(200, ape::Ntip(yule.trees[[1]])), log="y", type="n", bty="n", xlab="Time", ylab="N")
colors=c(rgb(1,0,0,0.5), rgb(0, 0, 0, 0.5))
list.of.both <- list(bd.trees, yule.trees)
for (i in sequence(2)) {
  tree.list <- list.of.both[[i]]
  for (j in sequence(length(tree.list))) {
    ape::ltt.lines(tree.list[[j]], col=colors[[i]])
  }
}
legend("topleft", legend=c("Birth Death", "Yule"), fill=colors)



speciation.rates <- c(0.1, 0.1, 0.1, 0.2) #0A, 1A, 0B, 1B
extinction.rates <- rep(0.03, 4)
transition.rates <- c(0.01,0.01,0, 0.01, 0, 0.01, 0.01,0,0.01, 0,0.01,0.01)
pars <- c(speciation.rates, extinction.rates, transition.rates)
phy <- tree.musse(pars, max.taxa=50, x0=1, include.extinct=FALSE)
