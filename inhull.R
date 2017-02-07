
inhull <- function(testpts, 
  calpts, 
  hull=convhulln(calpts), 
  tol=mean(mean(abs(calpts)))*sqrt(.Machine$double.eps)) { 
#++++++++++++++++++++ 
   require(geometry, quietly=TRUE) # for convhulln    require(MASS, quietly=TRUE) # for Null 
# ensure arguments are matrices (not data frames) and get sizes 
   calpts <- as.matrix(calpts) 
   testpts <- as.matrix(testpts) 
   p <- dim(calpts)[2] # columns in calpts    cx <- dim(testpts)[1] # rows in testpts    nt <- dim(hull)[1] # number of simplexes in hull 
# find normal vectors to each simplex 
   nrmls <- matrix(NA, nt, p) # predefine each nrml as NA, degenerate

   degenflag <- matrix(TRUE, nt, 1) 
   for (i in 1:nt) { 
    nullsp <- t(Null(t(calpts[hull[i,-1],] - matrix(calpts[hull[i,1],],p-1,p, byrow=TRUE))))

    if (dim(nullsp)[1] == 1) { 
      nrmls[i,] <- nullsp

       degenflag[i] <- FALSE
     }
   } 
# Warn of degenerate faces, and remove corresponding normals 
   if(length(degenflag[degenflag]) > 0) {
    warning(length(degenflag[degenflag])," degenerate faces in convex hull")
  }

   nrmls <- nrmls[!degenflag,] 
   nt <- dim(nrmls)[1] 
# find center point in hull, and any (1st) point in the plane of each 
# simplex

   center = colMeans(calpts)
   # apply(calpts, 2, mean) 
   a <- calpts[hull[!degenflag,1],] 
# scale normal vectors to unit length and ensure pointing inwards 
   nrmls <- nrmls/matrix(apply(nrmls, 1, function(x) {
    sqrt(sum(x^2))
  }), nt, p)    
   dp <- sign(apply((matrix(center, nt, p, byrow=TRUE)-a) * nrmls, 1, sum))    
   nrmls <- nrmls*matrix(dp, nt, p) 
# if min across all faces of dot((x - a),nrml) is 
# +ve then x is inside hull 
# 0 then x is on hull 
# -ve then x is outside hull 
# Instead of dot((x - a),nrml) use dot(x,nrml) - dot(a, nrml) 
   aN <- diag(a %*% t(nrmls)) 
   val <- apply(testpts %*% t(nrmls) - matrix(aN, cx, nt, byrow=TRUE), 1,min) 
# code values inside 'tol' to zero, return sign as integer 
   val[abs(val) < tol] <- 0 
   as.integer(sign(val)) 
} 