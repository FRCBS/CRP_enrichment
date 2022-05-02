### Helper functions for CRP enrichment analysis

## Generalist
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
    # http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
    # Multiple plot function
    #
    # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
    # - cols:   Number of columns in layout
    # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
    #
    # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
    # then plot 1 will go in the upper left, 2 will go in the upper right, and
    # 3 will go all the way across the bottom.
    #

    library(grid)

    # Make a list from the ... arguments and plotlist
    plots <- c(list(...), plotlist)

    numPlots = length(plots)

    # If layout is NULL, then use 'cols' to determine layout
    if (is.null(layout)) {
        # Make the panel
        # ncol: Number of columns of plots
        # nrow: Number of rows needed, calculated from # of cols
        layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                         ncol = cols, nrow = ceiling(numPlots/cols))
    }

    if (numPlots==1) {
        print(plots[[1]])
    } else {
        # Set up the page
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

        # Make each plot, in the correct location
        for (i in 1:numPlots) {
            # Get the i,j matrix positions of the regions that contain this subplot
            matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

        print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                        layout.pos.col = matchidx$col))
        }
    }
}

median_by_quantile <- function(df, by = "COL1", var = "COL2", n = 4){
    data <- df[, c(by, var)]
    data_narm <- na.omit(data)
    data_ordered <- data_narm[order(data_narm[, by]), ]
    # I'll loop this because I can't figure out how to make this work with apply
    var_med <- c()
    by_med <- c()
    nr <- nrow(data_ordered)
    stepsize <- ceiling(nr/n)
    a <- 1
    b <- stepsize
    for (i in 1:n){
        var_med[i] <- median(unlist(data_ordered[a:b, var]))
        by_med[i] <- median(unlist(data_ordered[a:b, by]))
        a <- b
        b <- b + stepsize
        if (b > nr) {b <- nr}
    }
    return(data.frame(by = by_med, var = var_med))
}

mean_by_quantile <- function(df, by = "COL1", var = "COL2", n = 4){
    data <- df[, c(by, var)]
    data_narm <- na.omit(data)
    data_ordered <- data_narm[order(data_narm[, by]), ]
    # I'll loop this because I can't figure out how to make this work with apply
    var_mean <- c()
    by_mean <- c()
    nr <- nrow(data_ordered)
    stepsize <- ceiling(nr/n)
    a <- 1
    b <- stepsize
    for (i in 1:n){
        var_mean[i] <- mean(unlist(data_ordered[a:b, var]))
        by_mean[i] <- mean(unlist(data_ordered[a:b, by]))
        a <- b
        b <- b + stepsize
        if (b > nr) {b <- nr}
    }
    return(data.frame(by = by_mean, var = var_mean))
}

bootmean <- function(data, i) {
    splice <- data[i]
    return(mean(splice))
}

bootmedian <- function(data, i) {
    splice <- data[i]
    return(median(splice))
}

get_ratio <- function(cohort, var1, var2, var1_trld, var2_trld) {
    # requires dplyr
    # Answers the question:
    # How much the proportion of individuals with var2 >< var2_trld
    # increase after we filter the population with var1_trld

    # Base population: cohort
    vi <- cohort %>%
        select({{var1}}, {{var2}}) %>% # variables of interest
        drop_na()

    # Find individuals of interest (ioi, above/below of var2_trld)
    ioi <- vi %>%
        filter({{var2}} >= {{var2_trld}})

    # Get proportion
    prop <- 100 * dim(ioi)[1] / dim(vi)[1]

    # Now filter base population first using var1
    filtered <- vi %>%
        filter({{var1}} >= {{var1_trld}})

    # And find IoI again
    f_ioi <- filtered %>%
        filter({{var2}} >= {{var2_trld}})

    # Now get proportion (filtered population)
    f_prop <- 100 * dim(f_ioi)[1] / dim(filtered)[1]

    # Compute difference between proportions
    diff <- round(f_prop - prop, 2)

    # Return
    return(diff)
}

get_ratio_boot <- function(cohort, var1, var2, var1_trld, var2_trld, i) {
    # requires dplyr
    # Answers the question:
    # How much the proportion of individuals with var2 >< var2_trld
    # increase after we filter the population with var1_trld

    # Base population: cohort
    vi <- cohort[i, ] %>%
        select({{var1}}, {{var2}}) %>% # variables of interest
        drop_na()

    # Find individuals of interest (ioi, above/below of var2_trld)
    ioi <- vi %>%
        filter({{var2}} >= var2_trld)

    # Get proportion
    prop <- 100 * dim(ioi)[1] / dim(vi)[1]

    # Now filter base population first using var1
    filtered <- vi %>%
        filter({{var1}} >= var1_trld)

    # And find IoI again
    f_ioi <- filtered %>%
        filter({{var2}} >= var2_trld)

    # Now get proportion (filtered population)
    f_prop <- 100 * dim(f_ioi)[1] / dim(filtered)[1]

    # Compute difference between proportions
    diff <- round(f_prop - prop, 2)

    # Return
    return(diff)
}

get_mean_boot <- function(cohort, var1, var2, var1_trld, i) {
    # requires dplyr
    # Answers the question:
    # What is the mean of var2
    # after we filter the population with var1_trld

    # Base population: cohort
    vi <- cohort[i, ] %>%
        select({{var1}}, {{var2}}) %>% # variables of interest
        drop_na()

    # Filter base population first using var1
    filtered <- vi %>%
        filter({{var1}} >= var1_trld)

    # And find mean of interest
    moi <- filtered %>%
        summarise(across({{var2}}, mean))

    # Return
    return(round(as.numeric(moi), 2))
}
