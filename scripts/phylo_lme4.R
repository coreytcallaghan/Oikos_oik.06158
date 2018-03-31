library(ape)
library(lme4)
library(Matrix)
## additional possibilities for PGLMMs ...
library(MCMCglmm)
library(MASS) ## glmmPQL + ape::corBrownian()
library(pez)
## n.b. at present need 'modular' devel branch:
## devtools::install_github("glmmTMB/glmmTMB/glmmTMB",ref="modular")
library(glmmTMB)
## utils
library(dplyr)
library(coda)
library(lattice)
library(broom) ## need install_github("bbolker/broom")
library(dotwhisker)
library(nlme)



phylo.to.Z <- function(r) {
  ntip <- length(r$tip.label)
  Zid <- Matrix(0.0,ncol=length(r$edge.length),nrow=ntip)
  nodes <- (ntip+1):max(r$edge)
  root <- nodes[!(nodes %in% r$edge[,2])]
  for (i in 1:ntip) {
    cn <- i  ## current node
    while (cn != root) {
      ce <- which(r$edge[,2]==cn)   ## find current edge
      Zid[i,ce] <- 1   ## set Zid to 1
      cn <- r$edge[ce,1]            ## find previous node
    }
  }
  Z <- t(r$edge.length * t(Zid))
  return(Z)
}

#' split a square (block) matrix into component blocks 
#' @param M square matrix
#' @param ind indices (0,n1,n2,...) giving the endpoint of each block
split_blkMat <- function(M,ind) {
  res <- list()
  if (length(ind)==1) return(list(M))
  for (i in 1:(length(ind)-1)) {
    v <- (ind[i]+1):ind[i+1]
    res[[i]] <- M[v,v]
  }
  return(res)
}

#' modify reTrms object
#' @param rt a reTrms object
#' @param phylo a phylo object (phylogenetic tree)
#' @param phylonm name of phylogenetic term in model
#' @param phyloZ Z matrix built on branch length
modify_phylo_retrms <- function(rt,phylo,phylonm,
                                phyloZ=phylo.to.Z(phylo),sp) {
  rep_phylo <- rt$Zt@Dim[2]/length(unique(sp)) ## number of column (aka same as number of obs)
  ## FIXME: better way to specify phylonm
  ## need to replace Zt, Lind, Gp, flist, Ztlist
  ## we have the same number of parameters (theta, lower),
  ##  same number of obs
  n.edge <- nrow(phylo$edge)
  phylo.pos <- which(names(rt$cnms)==phylonm)
  inds <- c(0,cumsum(sapply(rt$Ztlist,nrow)))
  ## Zt: substitute phylo Z for previous dummy (scalar-intercept) Z
  # for(i in phylo.pos){
  # repterms <- nrow(rt[["Ztlist"]][[i]])/length(unique(sp))
  # rt[["Ztlist"]][[i]] <- KhatriRao(do.call(cbind,replicate(rep_phylo,t(phyloZ),simplify = FALSE)),
  #             matrix(1
  #                    , ncol=ncol(rt[["Ztlist"]][[i]])
  #                    , nrow=repterms)
  # )
  ## reconstitute Zt from new Ztlist
  # }
  # rt[["Zt"]] <- do.call(rbind,rt[["Ztlist"]])
  ## Gp: substitute new # random effects (n.edge) for old # (n.phylo)
  Gpdiff <- diff(rt$Gp)  ## old numbers
  Gpdiff_new <- Gpdiff
  # for(i in phylo.pos){
  # Gpdiff_new[i] <- n.edge  ## replace
  # }
  rt[["Gp"]] <- as.integer(c(0,cumsum(Gpdiff_new)))          ## reconstitute
  ## Lind: replace phylo block with the same element, just more values
  Lind_list <- split(rt[["Lind"]],rep(seq_along(Gpdiff),Gpdiff))
  # for(i in phylo.pos){
  # Lind_list[[i]] <- rep(Lind_list[[i]][1],n.edge)
  # }
  # rt[["Lind"]] <- unlist(Lind_list)
  ## Lambdat: replace block-diagonal element in Lambdat with a
  ##   larger diagonal matrix
  Lambdat_list <- split_blkMat(rt[["Lambdat"]],inds)
  # for(i in phylo.pos){
  # Lambdat_list[[i]] <- KhatriRao(Diagonal(n.edge,1.0),
  #             matrix(1
  #                    , ncol=n.edge
  #                    , nrow=repterms))
  # }
  
  for(i in phylo.pos){
    repterms <- nrow(rt[["Ztlist"]][[i]])/length(unique(sp))
    #     rt[["Ztlist"]][[i]] <- KhatriRao(do.call(cbind,replicate(rep_phylo,t(phyloZ),simplify = FALSE)),
    #                 matrix(1
    #                        , ncol=ncol(rt[["Ztlist"]][[i]])
    #                        , nrow=repterms)
    #     )
    ## reconstitute Zt from new Ztlist
    rt[["Ztlist"]][[i]] <- t(KhatriRao(phyloZ,matrix(1,ncol=ncol(phyloZ),nrow=repterms))) %*% rt[["Ztlist"]][[i]]
    Gpdiff_new[i] <- n.edge  ## replace
    Lind_list[[i]] <- rep(Lind_list[[i]][1],n.edge)
    Lambdat_list[[i]] <- KhatriRao(diag(n.edge),
                                   Matrix(1
                                          , ncol=n.edge
                                          , nrow=repterms))
  }
  rt[["Zt"]] <- do.call(rbind,rt[["Ztlist"]])
  rt[["Lind"]] <- unlist(Lind_list)
  rt[["Lambdat"]] <- Matrix::.bdiag(Lambdat_list)
  ## flist: 
  rt[["flist"]] <- as.list(rt[["flist"]])
  rt[["flist"]][[phylonm]] <- factor(paste0("edge_",seq(n.edge)))
  return(rt)
}

#' 
phylo_glmm <- function(formula,data,family,phylo,phylonm,phyloZ,sp) {
  glmod <- glFormula(formula=formula,data = data, family = family)
  glmod$reTrms <- modify_phylo_retrms(glmod$reTrms,phylo,
                                      phylonm,phyloZ,sp)
  devfun <- do.call(mkGlmerDevfun, glmod)
  opt <- optimizeGlmer(devfun)
  devfun <- updateGlmerDevfun(devfun, glmod$reTrms)
  opt <- optimizeGlmer(devfun, stage=2)
  mkMerMod(environment(devfun), opt, glmod$reTrms, fr = glmod$fr)
}

phylo_lmm <- function(formula,data,phylo,phylonm,phyloZ,control,sp) {
  lmod <- lFormula(formula=formula,data = data,control=control)
  lmod$reTrms <- modify_phylo_retrms(lmod$reTrms,phylo,
                                     phylonm,phyloZ,sp)
  devfun <- do.call(mkLmerDevfun, lmod)
  opt <- optimizeLmer(devfun)
  # devfun <- updateLmerDevfun(devfun, lmod$reTrms)
  # opt <- optimizeLmer(devfun, stage=2)
  mkMerMod(environment(devfun), opt, lmod$reTrms, fr = lmod$fr)
}


if (!file.exists("data/phylo.nex")) {
  dir.create("data")
  download.file("http://mpcm-evolution.org/OPM/Chapter11_OPM/data.zip",
                dest="data/OPM_ch11_data.zip")
  setwd("data")
  untar("OPM_ch11_data.zip")
  setwd("..")
}
library(ape)
phylo <- ape::read.nexus("data/phylo.nex")
dat_pois <- read.table("data/data_pois.txt",header=TRUE)
dat_pois$obs <- factor(seq(nrow(dat_pois)))
dat_gaus <- read.table("data/data_simple.txt",header=TRUE)
dat_gaus$obs <- factor(seq(nrow(dat_gaus)))

system.time(phyloZ <- phylo.to.Z(phylo))



basic_gaus_fit <- lmer(phen~cofactor+(1|phylo),
                       data=dat_gaus,
                       control=lmerControl(check.nobs.vs.nlev="ignore",check.nobs.vs.nRE="ignore"))

basic_gaus_res <- rbind(coef(summary(basic_gaus_fit))[,1:2],
                        cbind(matrix(unlist(VarCorr(basic_gaus_fit))),NA))

phylo_gaus_fit <- phylo_lmm(phen~cofactor+(1|phylo),sp=dat_gaus$phylo,
                            data=dat_gaus,phylo=phylo,phylonm="phylo",
                            control=lmerControl(check.nobs.vs.nlev="ignore",check.nobs.vs.nRE="ignore"),
                            phyloZ=phyloZ)

lme4_gaus_res <- rbind(coef(summary(phylo_gaus_fit))[,1:2],
                       cbind(matrix(unlist(VarCorr(phylo_gaus_fit))),NA))

tt_lmer_nocor_gaus <- tidy(basic_gaus_fit)
tt_lmer_cor_gaus <- tidy(phylo_gaus_fit)

print(basic_gaus_res)
print(lme4_gaus_res)
