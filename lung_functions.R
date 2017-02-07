all_exists = function(...){
  all(file.exists(...))
}

all.exists = function(...) {
    all_exists(...)
}

human_seg = function(fname, 
    lthresh = -320,
    adder = 1025,
    hu_max = 3071,
    verbose = TRUE) {

    if (verbose) {
        message("reading in data")
    }
    file = fsl_swapdim(
        file = fname, 
        a = "RL", b = "PA", 
        c = "IS", verbose = TRUE)
    img = antsImageRead(file)

    img = img + adder
    img[img < 0] = 0
    img[img > hu_max + adder] = hu_max + adder
    if (verbose) {
        message("smoothing data")
    }    
    ximg = antsImageClone(img)
    img = smooth_image(img, sigma = 3)

    if (verbose) {
        message("running largest connected")
    }    
    human = img > (lthresh + adder)

    human = oMath(human, 
        "GetLargestComponent", 
        retfile = TRUE)
    return(
        list(
            img = ximg,
            smoothed = img,
            human = human
        )
    )
    # return(human)
}

boundary_labels = function(img) {
    labs = as.array(img)

    ############################################
    # get label from first labels
    # this is background
    ############################################
    dlabs = dim(labs)
    bad_lab = unique(
        c(labs[
            seq(dlabs[1]-5, dlabs[1]),
            seq(dlabs[2]-5, dlabs[2]),
            1:5
            ])
        )
    bad_lab = unique(
        c(bad_lab,
            labs[
            seq(dlabs[1]-5, dlabs[1]),
            1:5,
            1:5
            ])
        )    
    bad_lab = unique(
        c(bad_lab,
            labs[
            1:5,
            seq(dlabs[2]-5, dlabs[2]),
            1:5
            ])
        )   
    bad_lab = unique(
        c(bad_lab,
            labs[
            1:5,
            1:5,
            1:5
            ])
        )
    return(bad_lab)
}

remove_boundary = function(human,
    fill_size = 15) {
    inverted_human = human * -1 + 1
    
    fill_human = filler(human, 
        fill_size = fill_size)
    inverted = fill_human * -1 + 1
    # v3 = voxdim(inverted)[3]
    # d3 = dim(inverted)[3]
    # v3 = seq(d3, d3 - floor(top_mm / v3),
    #     by = -1)
    # arr = as.array(inverted)
    # arr[,,v3] = 0
    # inverted = as.antsImage(arr, 
    #     reference = human)

    # eroded = filler(inverted,
    #     fill_size = 10,
    #     dilate = FALSE)

    largest = iMath(inverted, 
        "GetLargestComponent")
    bad_lab = boundary_labels(largest)

    if (1 %in% bad_lab) {
        return(inverted_human - largest)
    } else {
        return(inverted_human)
    }
}

lung_seg2 = function(human, fill_size = 7) {
    arr = as.array(human)
    arr = replace_outside_surface(arr, 
        replace_value = -1)
    arr = arr == 0
    aimg = as.antsImage(arr, 
        reference = human)
    rm(list = "arr")
    aimg = filler(aimg, 
        fill_size = fill_size)
    for (i in 1:10) {
        gc();
    }
    aimg = iMath(aimg, 
        "GetLargestComponent")     
    return(aimg)
}

human_filler = function(human){ 

    vdim = voxdim(human)[1:2]
    vdim = min(vdim)
    filler_size = ceiling(25/vdim)
    
    human_filled = fill_2d_z(
        img = human,
        fill_size = filler_size)

}

lung_seg = function(human, 
    fill = TRUE,
    img = NULL,
    fill_size = 15) {


    inverted = human * -1 + 1
    # outside = oMath(inverted, 
    #     "GetLargestComponent", 
    #     retfile = TRUE) 
    alabs = ANTsR::labelClusters(
        inverted,
        minClusterSize = 1,
        fullyConnected = TRUE)

    ##################################
    # Look at the labels in the center of the image
    ##################################    
    idim = dim(inverted)
    vdim = voxdim(inverted)
    eg = vector(mode = "list", length = 3)
    for (i in 1:3) {
        # 5 mm on each side
        keep_cylinder = floor(idim[i]/2) + 
            floor(c(-5, 5)/ vdim[i])
        keep_cylinder = c(
            max(1, keep_cylinder[1]),
            min(idim[i], keep_cylinder[2])
            )
        inds = seq(keep_cylinder[1], 
            keep_cylinder[2])
        eg[[i]] = inds
    }
    eg = expand.grid(eg)
    eg = as.matrix(eg)
    arr = array(0, dim = idim)
    arr[eg] = 1
    max_mid_lab = max(alabs[arr == 1])

    if (max(alabs) < 3 | max_mid_lab == 1) {
        inverted = remove_boundary(human, 
            fill_size = fill_size)
        alabs = ANTsR::labelClusters(
            inverted,
            minClusterSize = 1,
            fullyConnected = TRUE)
    }
    max_labs = max(alabs)
    if (max_labs == 1) {
        warning("Only one connected component!")
    }

    # alabs = ants2oro(alabs, reference = img)
    # alabs = replace_dropped_dimensions(
    #     img = alabs,
    #     inds = dd$inds,
    #     orig.dim = dim(img))

    # convert to array for ease of use
    labs = as.array(alabs)
    bad_lab = boundary_labels(alabs)

    if (length(bad_lab ) == 0) {
        stop("don't have bad labels")
    }
    # remove this label from the image
    labs[ labs %in% bad_lab] = 0
    # make a table
    tab = table(c(as.array(labs)))
    # remove background
    tab = tab[ setdiff(names(tab), "0") ]
    keep_lab = names(tab)[which.max(tab)]
    keep_lab = as.numeric(keep_lab)

    # recreate image: binary with labeled lungs
    labs = labs == keep_lab 
    alabs = as.antsImage(labs, 
        reference = human)

    if (fill) {
        filled = filler(alabs)
        filled = ants2oro(filled, reference = img)
    } else {
        filled = ants2oro(alabs, reference = img)
    }

    return(filled)
}


img_volume = function(img, units = "cm") {
    sum(img) * voxres(img, units = units)
}

fast_ortho2 = function(x, pdim = NULL, ...) {
    is.rnifti = function(x) {
        inherits(x, "niftiImage") || 
        inherits(x, "internalImage")
    }
    if (is.rnifti(x)) {
        pdim = c(1, RNifti::pixdim(x))
        x = as(x, "array")
    }
    ortho2(x, pdim = pdim)
}