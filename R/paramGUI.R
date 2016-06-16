#' Calculates a matrix in which each column is a skewed Gaussian 
#' 
#' @description Calculates a matrix in which each column is a skewed Gaussian. 
#' Like \code{calcEhiergaus} from TIMP package but uses a vector not a list of parameter estimates.
#'
#' @param theta vector of parameter estimates
#' @param lambda wavelengths at which to calculate model
#'
#' @return \code{matrix} 
#' @importFrom TIMP skew l2nu
#' @export
#'
calcE <- function(theta, lambda) {
  nspec <- length(theta)/3
  nl <- length(lambda)
  npare <- 3
  spec <- matrix(nrow = nl, ncol = nspec)
  for(i in 1.:nspec) {
    ioff <- (i - 1.) * npare
    spec[, i] <- skew(theta[ioff + 1], theta[ioff + 2], theta[
      ioff + 3], l2nu(lambda), nupower=1)
  }
  spec
}

#' Spectrotemporal model
#'
#' @param sim object of class \code{dat} representing data
#' @param model object of class \code{dat} representing a model
#' @param iter integer number of iterations
#' @param kroncol object of class \code{logical} that is 
#' \code{TRUE} if the \code{kroneckcol} function should be used to formulate the model and 
#' \code{FALSE} if the standard \code{kronecker} is to be used instead
#' @param lin defines the range to plot linearly (from -\code{lin} to +\code{lin})
#' @param l_posk object of class \code{logical} indicating whether positivity constraints are enforced on the rate parameters
#'
#' @importFrom TIMP compModel
#' @importFrom stats nls nls.control 
#' @importFrom utils head tail 
#' @export
#'
spectemp <- function(sim, model, iter, kroncol=FALSE, lin=NA,l_posk=FALSE) {
  
  psisim <- as.vector(sim@psi.df)
  dummy <- as.data.frame(psisim)
  
  specpar <- unlist(model@specpar)
  kinpar <- model@kinpar
  seqmod <- model@seqmod
  irfpar <- model@irfpar
  irf <- model@irf 
  x2 <- sim@x2
  x <- sim@x
  
  ## not using the calcEhiergaus from TIMP because it requires a list of
  ## specpar, as opposed to a vector
  if (l_posk) {
    if(kroncol) {
      kronform <- psisim ~ kroneckercol(A= calcE(sp, x2),
                                        B= compModel(k=exp(k), x=x, seqmod=seqmod,
                                                     irf=irf,
                                                     irfpar=irfpar))
    } else {
      kronform <- psisim ~ kronecker(calcE(sp, x2),
                                     compModel(k=exp(k), x=x, seqmod=seqmod,
                                               irf=irf,
                                               irfpar=irfpar))
    }
    
    #
    # NB warnOnly=TRUE, to return also in case of nonconvergence
    #
    onls <- nls(kronform, control = nls.control(printEval=TRUE, warnOnly=TRUE,
                                                maxiter=iter),
                start = list(k = log(kinpar), sp = specpar, irfpar=irfpar),
                algorithm = "plinear", trace = TRUE)
  } else {
    
    if(kroncol) {
      kronform <- psisim ~ kroneckercol(A= calcE(sp, x2),
                                        B= compModel(k=k, x=x, seqmod=seqmod,
                                                     irf=irf,
                                                     irfpar=irfpar))
    } else {
      kronform <- psisim ~ kronecker(calcE(sp, x2),
                                     compModel(k=k, x=x, seqmod=seqmod,
                                               irf=irf,
                                               irfpar=irfpar))
    }
    
    #
    # NB warnOnly=TRUE, to return also in case of nonconvergence
    #
    onls <- nls(kronform, control = nls.control(printEval=TRUE, warnOnly=TRUE,maxiter=iter),
                start = list(k = kinpar, sp = specpar, irfpar=irfpar),
                algorithm = "plinear", trace = TRUE)
    
  }
  
  # sumonlskron <- summary(onls, correlation=TRUE)
  # if(kroncol) { 
  #    assign("sumonlssingle",  sumonlskron, envir=.GlobalEnv)
  # } else {
  #   assign("sumonlsmultiple",  sumonlskron, envir=.GlobalEnv)
  # }
  
  ## standardize the storage of par. est., assuming order is kinpar, specpar
  ## irfpar
  theta <- theta()
  
  if(length(model@irfpar) > 0) {
    pars <- head(onls$m$getPars(), length(onls$m$getPars())-2) 
    theta@irfpar <- tail(onls$m$getPars(),2)
  } else {
    pars <- onls$m$getPars()
  }
  theta@specpar <- list(tail(pars, length(specpar)))
  if (l_posk) {
    theta@kinpar <- exp(head(pars, length(kinpar)))
  } else {
    theta@kinpar <- head(pars, length(kinpar))
  }
  
  #plotterforGUI(modtype="spectemp", data=sim, model=model, theta=theta, result=onls, lin = lin)
  list("theta"=theta,"onls"=onls)
  
}


#' kroneckercol: column-wise kronecker product
#' 
#' @description The column-wise kronecker product is also called the Khatri–Rao product
#'
#' @param A numerical matrix
#' @param B numerical matrix
#'
#' @return column-wise kronecker product of A and B
#' @export
#'
"kroneckercol" <- function(A, B) {
  ncomp <- ncol(B)
  nt <- nrow(B)
  nl <- nrow(A)
  ar <- rep(1:nl, rep(nt, nl))
  br <- rep(1:nt, nl)
  ac <- 1:ncomp
  A[ar, ac] * B[br, ac]
}


#' Generate linlog tics for a linear-logarithmic axis
#'
#' @param x values for which to calculate a linlog axis
#' @param mu center of axis in the orginal \code{x} axis
#' @param alpha linear part 
#'
#' @return Returns matrix with new \code{x} values in first column and the corresponding labels in the second column.
#'
"linlogtics" <- function(x, mu, alpha) {
  maxorigx <- max(x)
  minorigx <- min(x)
  ticsl <- c(-alpha)
  tics <- c(-alpha)
  cntmin <- -alpha
  
  while (cntmin > minorigx) {
    cntmin <- cntmin * 10
    ticsl <- append(ticsl, cntmin)
    tics <- append(tics, -alpha - (alpha * log10(-cntmin/alpha)))
  }
  ticsl <- append(sort(ticsl), c(0, alpha))
  tics <- append(sort(tics), c(0, alpha))
  cntmax <- alpha
  while (cntmax < maxorigx) {
    cntmax <- cntmax * 10
    ticsl <- append(ticsl, cntmax)
    tics <- append(tics, alpha + (alpha * log10(cntmax/alpha)))
  }
  ## new x values as column 1
  ## new x labels as colum 2
  ret<-cbind(tics, ticsl)
  
  ret
}

#' Master plot function for paramGUI
#'
#' @param modtype either "kin", "spec" or "spectemp"
#' @param X matrix of conditionally linear parameters, if any
#' @param data object of class \code{dat} containing data
#' @param model object of class \code{dat} containing data
#' @param theta object of class \code{theta} containing parameters
#' @param result object returned by \code{fitModel} or in the case
#' \code{modtype=="spectemp"}, by \code{nls}
#' @param lin The linear range for the concentration plot
#' @param mu The center of the lin-log axis is \code{lin} is specified
#'
#' @return graphics
#' @importFrom fields image.plot
#' @importFrom colorspace diverge_hcl
#' @importFrom TIMP compModel getSpecList parEst linloglines linlogplot irfparF matlinlogplot
#' @importFrom graphics abline axis barplot image lines matlines matplot mtext par plot
#' @export
#'
"plotterforGUI" <- function(modtype="kin", X=matrix(), data, model,
                            theta=vector(), result, lin=NA, mu=0)
{
  
  
  ## note that result is the return value of fitModel if modtype=="spec"
  ## or modtype == "kin", and the return value of nls otherwise
  if(!is.null(model)) {
    ## get clp, if modtype == "kin" ||  modtype == "spec"
    if(!modtype == "spectemp") {
      X <- getSpecList(result$currModel, result$currTheta)[[1]]
      theta <- result$currTheta[[1]]
    }
  } else {
    modtype <- "kin"
  }
  nt <- data@nt
  nl <- data@nl
  
  x <- data@x
  x2 <- data@x2
  
  if(!is.null(model)) {
    if(modtype == "kin" && length(model@irfpar) > 0) 
      mu <- unlist(parEst(result,param="irfpar",dataset=1,verbose=F))[1]
    else {if(modtype == "spectemp" && length(theta@irfpar) > 0) 
      mu <- head(theta@irfpar,1)
    else
      mu <- 0 }
  }
  
  op <- par(no.readonly = TRUE)
  # CHANGE PLOT OPTIONS
  if(!is.null(model)) {
    if ((nt==1)||(nl==1))
    {par(mfrow = c(2, 2), oma=c(0,0,3,0))}
    else{  par(mfrow = c(3, 4), oma=c(0,0,3,0))}
  } else {
    par(mfrow = c(2, 3), oma=c(0,0,3,0))
  }
  
  ## get labels and time axis for linlog plots
  
  if(is.na(lin)) {
    if(!modtype == "spec") { 
      if( (!is.null(model) && length(model@irfpar) > 0) ) 
        lin <- max(pretty(abs(mu)*10))
      else 
        lin <- max(data@x)
    }
    else
      lin <- max(data@x)
  }
  
  xnew <- linloglines(x, mu=mu,alpha=lin)
  newlab <- linlogtics(x, mu=0,alpha=lin)
  
  if(!is.null(model)) {
    if(!modtype == "spectemp") {
      residuals <- matrix(nrow = nt, ncol = nl)
      residlist <- result$currModel@fit@resultlist[[1]]@resid
      for (j in 1:length(residlist)) { 
        if(modtype=="kin")
          residuals[, j] <- residlist[[j]]
        else
          residuals[j, ] <- residlist[[j]]
      }
    }
    else {
      residuals <- result$m$resid()
      dim(residuals) <- c(nt, nl)
    }
  }
  
  
  #PLOT DATA 
  
  observed <- data@psi.df
  if (nt == 1) {
    plot(x=x2, y=observed,  xlab = "wavelength (nm)",ylab="",
         main = "Data", type = "l",xlim=c(min(x2), max(x2)))
    if(!is.null(model)) {
      lines(x=x2, y=observed-residuals[1,],  col="red")
    }
    abline(0, 0)
  }
  else {
    if(nl == 1) {  
      ##    linlogplot(x=x, y=observed, mu=mu, alpha=lin, xlab = "time (ps)",ylab="",               main = "Data", type = "l", xlim=c(min(x), max(x)))
      plot(x=x, y=observed, xlab = "time (ps)",ylab="",
           main = "Data", type = "l", xlim=c(min(x), max(x)))
      if(!is.null(model)) {
        lines(x=x, y=observed-residuals[,1],  col="red")
      }
      abline(0, 0)}
    else {
      m<-par("mar")
      par(mar=c(m[1:3],3))
      image(xnew, x2, observed, ylab = "wavelength (nm)", xaxt="n",
            main = "Data", col = diverge_hcl(40, h = c(0, 120), c =
                                               60, l = c(45, 90), power = 1.2), xlab="time (ps)")
      axis(1, at=newlab[,1], labels=newlab[,2])
      image.plot(xnew, x2, observed, ylab = "wavelength (nm)", legend.only=TRUE,
                 main = "Data", col = diverge_hcl(40, h = c(0, 120), c =
                                                    60, l = c(45, 90), power = 1.2))
      par(mar=m)
      
      # PLOT SVD DATA 
      svdobserved <- svd(observed)
      plot(log10(svdobserved$d),main="log(sing val. data)",  ylab="")
      lsv1<-svdobserved$u[,1]
      #  maxlsv1<-max(lsv1)
      #  minlsv1<-min(lsv1)
      #  extrlsv1<-max(abs(maxlsv1),abs(minlsv1))
      #  if (minlsv1 > 0) minlsv1=0
      #  if (maxlsv1 < 0) maxlsv1=0
      linlogplot(x=x,y=lsv1,mu=mu, alpha=lin, main="1st LSV data",
                 xlab = "time (ps)",type = "l",  ylab="",xlim=c(min(x), max(x)),ylim=c(min(lsv1,0),max(lsv1,0)))
      abline(0,0,lty=3)
      plot(x2,svdobserved$v[,1],main="1st RSV data",
           xlab = "wavelength (nm)",type = "l",  ylab="")	
      abline(0,0,lty=3)
      
      linlogplot(x=x,y=svdobserved$u[,2],mu=mu, alpha=lin, main="2nd LSV data",
                 xlab = "time (ps)",type = "l",  ylab="",xlim=c(min(x), max(x)))
      abline(0,0,lty=3)
      plot(x2,svdobserved$v[,2],main="2nd RSV data",
           xlab = "wavelength (nm)",type = "l",  ylab="")	
      abline(0,0,lty=3)
    }
    
  }
  if(!is.null(model)) {
    if (nt > 1){  
      #PLOT CONCENTRATIONS
      
      if(!modtype == "spec")
        C <- compModel(k = theta@kinpar, x = x, irfpar = theta@irfpar, 
                       irf = model@irf, seqmod = model@seqmod)
      else { # modtype == "spec"
        C <- X
        # assign("Cest",cbind(x, C), envir = globalenv()) 
      }
      
      
      if(lin == 0)
        matplot(x, C,  xlab = "time (ps)", ylab="",
                main = "Concentrations", type="l",lty=1)
      else {
        
        matlinlogplot(x, C, mu, lin, ylab="", xlab = "time (ps)", 
                      main = "Concentrations", type="l",lty=1)
      }
      if(data@simdata)
      {   
        if((modtype == "kin")||(modtype == "spectemp"))
        {amplitudes = data@amplitudes
        ncomp<-length(amplitudes)
        aC2<-data@C2%*%diag(1/amplitudes,ncomp,ncomp)
        matlinlogplot(x=x, mu=mu, alpha=lin, y=aC2,
                      col="blue", lty=3, add=TRUE, type="l")}
        else
        {matlinlogplot(x=x, mu=mu, alpha=lin, y=data@C2,
                       col="blue", lty=3, add=TRUE, type="l")}
      }
      abline(0,0,lty=3)
    } else {barplot(X[1,], main = "Amplitudes",  ylab="",
                    xlab = "component",lty=1)
    }  
    if (nl > 1){  
      #PLOT SPECTRA
      
      if(modtype == "kin") {
        E <- X
        # assign("Eest",cbind(x2, E), envir = globalenv()) 
      }
      if(modtype == "spec")
        E <- calcEhiergaus(lambda=x2, theta=theta@specpar, nupower=1)
      if(modtype == "spectemp")
        E <- calcE(lambda=x2, theta=unlist(theta@specpar))
      
      matplot(x2, E, main = "Spectra",  ylab="",
              xlab = "wavelength (nm)", type="l",lty=1)
      abline(0,0,lty=3)
      
      if(data@simdata)
        if(modtype == "kin")
        {    aE2<-data@E2%*%diag(amplitudes,ncomp,ncomp)
        matlines(x2, aE2, lty=3, col="blue")}
      else
      {
        matlines(x2, data@E2, lty=3, col="blue")}
    } else {
      barplot(X[1,], main = "Amplitudes",  ylab="",
              xlab = "component",lty=1)
    }  
    #PLOT RESIDS 
    if (nt == 1) {
      plot(x=x2, y=residuals[1,],  xlab = "wavelength (nm)", ylab="",
           main = "Residuals", type = "l",xlim=c(min(x2), max(x2)))
      abline(0, 0)
    }
    else {
      if(nl == 1) {
        linlogplot(x=x, y=residuals[,1], mu=mu, alpha=lin, xlab = "time (ps)", ylab="",
                   main = "Residuals", type = "l",xlim=c(min(x), max(x)))
        abline(0, 0)
      } else {
        m<-par("mar")
        par(mar=c(m[1:3],3))
        image(xnew, x2, residuals, ylab = "wavelength (nm)", xaxt="n",
              main = "Residuals", col = diverge_hcl(40, h = c(0, 120), c =
                                                      60, l = c(45, 90), power = 1.2), xlab="time (ps)")
        axis(1, at=newlab[,1], labels=newlab[,2])
        image.plot(xnew, x2, residuals, ylab = "wavelength (nm)", legend.only=TRUE,
                   main = "Residuals", col = diverge_hcl(40, h = c(0, 120), c =
                                                           60, l = c(45, 90), power = 1.2))
        par(mar=m)    
        svdresid <- svd(residuals)
        logsvd<-log10(svdresid$d)
        lenlogsvd<-length(logsvd)
        nsingvalres<-lenlogsvd-dim(E)[2]
        
        plot(logsvd[1:nsingvalres], main="log(sing. val. resid.)", ylab="")
        linlogplot(x=x, y=svdresid$u[,1], mu=mu, alpha=lin, main="1st LSV resid.",
                   xlab = "time (ps)", type = "l",  ylab="",xlim=c(min(x), max(x)))
        abline(0,0,lty=3)
        plot(x2,svdresid$v[,1], main="1st RSV resid",
             xlab = "wavelength (nm)",type = "l", ylab="")
        abline(0,0,lty=3)
      }  }
    # ADD TITLE 
    if(!modtype=="spec") {
      kinest <- paste("Kin par:", toString(signif(theta@kinpar, digits=4)))
      if(length(theta@irfpar) > 0) 
        kinest <- paste(kinest, "   IRF par:",
                        toString(signif(theta@irfpar, digits=4)))
      mtext(kinest, side = 3, outer = TRUE, line = 1)
      
    }
    if(!modtype=="kin") {
      specest <- paste("Spec par:", toString(signif(unlist(theta@specpar),
                                                    digits=4)))
      mtext(specest, side = 3, outer = TRUE, line = -.5)
    }
  }
  # RESET FORMER PLOT OPTIONS
  par(op)
  # try 20111104
  #tkdestroy(winmodel)
  #this only kills one winmodel ...
  
}

#' Simulate data
#'
#' @description Calculates an object of class "kin". <TODO>
#' 
#' @author Katharine M. Mullen
#' @author Ivo H. M. van Stokkum 
#' 
#' @param kinpar vector of rate constants
#' @param tmax last time point
#' @param deltat time step
#' @param specpar vector of spectral parameters for location, width, skewness
#' @param lmin minimum wavelength (nm)
#' @param lmax maximum wavelength (nm)
#' @param deltal wavelength step
#' @param sigma noise level
#' @param irf logical for IRF usage
#' @param irfpar vector of IRF parameters for location, width
#' @param seqmod logical for sequential model
#' @param dispmu logical for dispersion of IRF location mu
#' @param nocolsums logical for <TODO>
#' @param disptau logical for dispersion of IRF width tau
#' @param parmu vector of dispersion parameters for IRF location mu
#' @param partau vector of dispersion parameters for IRF width tau
#' @param lambdac center wavelength for dispersion
#' @param fullk logical for full K matrix
#' @param kmat K matrix
#' @param jvec input vector
#' @param specfun function for spectral shape
#' @param nupow power of nu in spectral model
#' @param irffun function for IRF
#' @param kinscal vector of kinetic scaling parameters
#' @param lightregimespec <TODO>
#' @param specdisp logical for dispersion parameters of spectral parameters
#' @param specdisppar vector of dispersion parameters of spectral parameters
#' @param parmufunc <TODO>
#' @param specdispindex <TODO>
#' @param amplitudes amplitudes of components
#' @param specref <TODO>
#' @param nosiminfo logical for hiding simulation information
#'
#' @return an object of class "kin"
#' @importFrom TIMP dat kin compModel specparF calcEhiergaus
#' @importFrom stats rnorm
#' @export
#'
"simndecay_gen_paramGUI" <-
  function (kinpar, tmax, deltat, specpar=vector(), lmin, lmax, deltal, 
            sigma, irf = FALSE, irfpar = vector(), seqmod = FALSE, dispmu
            = FALSE, nocolsums=FALSE, 
            disptau = FALSE, parmu = list(), partau = vector(), lambdac = 0, 
            fullk = FALSE, kmat = matrix(), jvec = vector(), specfun = "gaus", 
            nupow = 1, irffun = "gaus", kinscal = vector(), 
            lightregimespec = list(),
            specdisp = FALSE, specdisppar=list(), parmufunc = "exp",
            specdispindex = list(), amplitudes = vector(),specref=0,nosiminfo=FALSE) 
  {
    if (tmax>0) { x <- seq(0, tmax, deltat)} else { x<-c(1) }
    nt <- length(x)
    ncomp <- length(kinpar)
    x2 <- seq(lmin, lmax, deltal)
    nl <- length(x2)
    
    if(specdisp){
      ## store all the spectra; could do it otherwise if mem. is an issue
      EList <- list()
      for (i in 1:nt) {
        sp <- specparF(specpar = specpar, 
                       xi = x[i], 
                       i = i, specref = specref, 
                       specdispindex = specdispindex, 
                       specdisppar = specdisppar,
                       parmufunc = parmufunc)
        EList[[i]] <- calcEhiergaus(sp, x2, nupow)
      }
    }
    else 
      if (lmin==lmax) {E2<-matrix(1,nrow = 1, ncol = ncomp)
      # TODO: set modType to 0?
      }else{
        E2 <- calcEhiergaus(specpar, x2, nupow)}
    
    if (!(dispmu || disptau)) {
      if (nt==1){
        C2<- matrix(amplitudes,nrow = 1, ncol = ncomp)
        # TODO: set modType to 0?
      } else {
        C2 <- compModel(k = kinpar, x = x, irfpar = irfpar, irf = irf, 
                        seqmod = seqmod, fullk = fullk, kmat = kmat,
                        jvec = jvec,amplitudes = amplitudes,
                        lightregimespec = lightregimespec,
                        nocolsums= nocolsums, kinscal = kinscal)
      }
      if(specdisp){
        psisim <- matrix(nrow = nt, ncol = nl)
        E2 <- EList[[1]]
        for (i in 1:nt) {
          psisim[i,] <- t(as.matrix(C2[i, ])) %*% t(EList[[i]])
        }
      }
      else 
        psisim <- C2 %*% t(E2)
    }
    else {
      psisim <- matrix(nrow = nt, ncol = nl)
      for (i in 1:nl) {
        irfvec <- irfparF(irfpar, lambdac, x2[i], i, dispmu, 
                          parmu, disptau, partau, "", "", "gaus")
        
        C2 <- compModel(k = kinpar, x = x, irfpar = irfpar, irf = irf, 
                        seqmod = seqmod, fullk = fullk, kmat = kmat,
                        jvec = jvec,amplitudes = amplitudes,
                        lightregimespec = lightregimespec,
                        nocolsums= nocolsums, kinscal = kinscal)
        psisim[, i] <- C2 %*% cbind(E2[i, ])
      }
    }
    dim(psisim) <- c(nt * nl, 1)
    psi.df <- psisim + sigma * rnorm(nt * nl)
    dim(psi.df) <- c(nt, nl)
    
    if (nosiminfo){
      dat(psi.df = psi.df, x = x, nt = nt, x2 = x2, nl = nl, 
          simdata = FALSE)
    }
    else{
      kin(psi.df = psi.df, x = x, nt = nt, x2 = x2, nl = nl, C2 = C2, 
          E2 = E2, kinpar = kinpar, specpar = specpar, 
          seqmod = seqmod, irf = irf, irfpar = irfpar, 
          dispmu = dispmu, disptau = disptau, parmu = parmu, partau = partau, 
          lambdac = lambdac, simdata = TRUE, fullk = fullk, kmat = kmat, 
          jvec = jvec,amplitudes = amplitudes)
    }
  }



# ".onLoad" <- function (lib, pkg)
# {
#   startGUI()
# }
