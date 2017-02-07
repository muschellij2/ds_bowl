############################################
# Segment lungs from the rest of the scan
############################################
rm(list = ls())
library(fslr)
library(extrantsr)
library(ANTsR)
library(RNifti)
library(dplyr)
library(caret)
library(ROCR)
source("lung_functions.R")

rootdir = "/dcl01/smart/data/kaggle_lung"
rda_file = file.path(rootdir, 
    "filenames.rda")
datadir = file.path(rootdir,
    "nifti")
outdir = file.path(rootdir, "lung")
houtdir = file.path(rootdir, "human")
meas_dir = file.path(rootdir, "results",
    "measures")

load(rda_file)
files = df$nii

df$outfile = file.path(outdir, 
    paste0(nii.stub(df$nii, bn = TRUE), 
        "_lung.nii.gz")
    )

df$human = file.path(houtdir, 
    paste0(nii.stub(df$nii, bn = TRUE), 
        "_human_mask.nii.gz")
    )

df$measure_rda = file.path(meas_dir,
    paste0(df$id, "_measures.rda"))

# df = df[ file.exists(df$outfile), ]

iid = 1
iid = as.numeric(Sys.getenv("SGE_TASK_ID"))
if (is.na(iid)) {
    iid = 1
}
N = nrow(df)
all_measures = vector(mode = "list",
    length = N)
# for (iid in seq(N)) {
# for (iid in seq(N)) {
    print(iid)
    id = df$id[iid]
    ## Making positive values
    adder = 1025
    hu_max = 3071
    fname = files[iid]
    outrda = df$measure_rda[iid]    
    outfile = df$outfile[iid]
    human_fname = df$human[iid]
    breaks = seq(0, hu_max + adder, 
        length.out = 20)

    if (!file.exists(outrda)) {
        rpi = rpi_orient(fname)
        img = rpi$img
        rm(list = "rpi")
        message("Reading in Lung")
        # lung = fast_readnii(outfile)
        lung = readnii(outfile)
        lung_mask = lung > 0

        human = readnii(human_fname)
        human_vals = mask_img(img, human)

        pdim = voxdim(lung)
        vres = prod(pdim) / 1000
        # lung_mask = lung > 0
    # }


        make_meas = function(img, mask, vres) {
            # # simple metrics 
            vals = img[ mask == 1 ]
            h = hist(vals, breaks = 2000, 
                plot = FALSE)
            ph = hist(vals, breaks = 256, 
                plot = FALSE)
            p = ph$counts / sum(ph$counts)

            ###########################
            # Got these from the paper
            ###########################
            entropy = - sum(p * log2(p), 
                na.rm = TRUE)
            uniformity = sum(p^2)

            ###########################
            # Largest x/y cross-section
            ###########################
            vdim = voxdim(mask)
            ind = which(mask ==1, 
                arr.ind = TRUE)
            ind[,1] = ind[,1] * vdim[1]
            ind[,2] = ind[,2] * vdim[2]
            ind = data.frame(ind)
            ind = ind %>% 
                group_by(dim3) %>% 
                summarise(d1 = diff(range(dim1)), 
                d2 = diff(range(dim2))) %>% 
                ungroup
            ind = ind %>% 
                summarise(d1 = max(d1), 
                d2 = max(d2)) %>% 
                ungroup 
            csection = unlist(ind)

            #############################
            # Quantile Images
            #############################
            q = quantile(vals, 
                probs = c(0.025, 0.25, 
                    0.5, 0.75, 0.975)
                )
            cats = cut(vals, breaks= breaks, 
                include.lowest = FALSE)
            tab_cat = table(cats)
            ptab = prop.table(tab_cat)
            tab_cat = tab_cat * vres        
            measures = list(
                # vol = img_volume(mask),
                vol = sum(mask) * vres,
                mn = mean(vals),
                mn_10 = mean(vals, trim = 0.1),
                med = median(vals),
                sd = sd(vals),
                min = min(vals),
                max = max(vals),
                mode = h$mids[which.max(h$counts)],
                cat_vol = tab_cat,
                ptab = ptab,
                entropy = entropy,
                uniformity = uniformity,
                quantiles = q,
                csection = csection
                ) 
            L = list(measures = measures, 
                hist = h)
        }

        lung_data = make_meas(lung, lung_mask, vres)
        human_data = make_meas(img, human, vres)

        meas = unlist(lung_data$measures)
        meas = as.data.frame(t(meas), 
            stringsAsFactors = FALSE)
        colnames(meas) = paste0(
            "lung.",
            colnames(meas))
        hmeas = unlist(human_data$measures)
        hmeas = as.data.frame(t(hmeas), 
            stringsAsFactors = FALSE)
        colnames(hmeas) = paste0(
            "human.",
            colnames(hmeas))
        measures = cbind(meas, hmeas)
        measures$id = id
        message("writing measure")
        save(measures, file = outrda)
    }
# }

# nulls = sapply(meas, is.null)
# meas = meas[!nulls]
# meas = do.call("rbind", meas)

# meas$y = df$cancer[!nulls]

# m = meas %>% 
#     filter(!is.na(y)) %>% 
#     mutate(y = factor(y))

# g = ggplot(aes(colour = factor(y)), data = m) + 
#     geom_density()

# inTraining <- createDataPartition(
#     m$y, p = .55, list = FALSE)

# training <- m[ inTraining,]
# testing  <- m[-inTraining,]


# fitControl <- trainControl(## 10-fold CV
#                            method = "repeatedcv",
#                            number = 10,
#                            ## repeated ten times
#                            repeats = 10)
# rf <- train(y ~ ., data = training, 
#                  method = "rf", 
#                  trControl = fitControl,
#            ## This last option is actually one
#            ## for gbm() that passes through
#                  verbose = FALSE)
# p = predict(rf, 
#     newdata = testing, 
#     type = "prob")
# p = p[,"1"]
# pred = prediction(p, testing$y)
# perf = performance(pred, "tpr", "fpr")

# gbmFit1 <- train(y ~ ., data = training, 
#                  method = "gbm", 
#                  trControl = fitControl,
#             ## This last option is actually one
#             ## for gbm() that passes through
#                  verbose = FALSE)
# gbmFit1                           

# fill_width = 20
# vres = voxres(img, units = "mm")
# nvox = ceiling(fill_width/ vres)
# filled = filler(human, fill_size = nvox)

# ortho2(img, img > -200)