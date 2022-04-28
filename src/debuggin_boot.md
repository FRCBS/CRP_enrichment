# Boot funcs

    bootmean <- function(data, i) {
        splice <- data[i]
        return(mean(splice, na.rm = T))
    }

    bootmedian <- function(data, i) {
        splice <- data[i]
        return(median(splice, na.rm = T))
    }

    get_ratio <- function(cohort, var1, var2, var1_trld, var2_trld) {
        # Answers the question:
        # How much the proportion of individuals with var2 >< var2_trld
        # increase after we filter the population with var1_trld

        # Base population: cohort
        vi <- na.omit(cohort[, c(var1, var2)])

        # Find individuals of interest (ioi, above/below of var2_trld)
        ioi <- vi[vi[, var2] >= var2_trld, ]

        # Get proportion
        prop <- 100 * nrow(ioi) / nrow(vi)

        # Now filter base population first using var1
        filtered <- vi[vi[, var1] >= var1_trld, ]

        # And find IoI again
        f_ioi <- filtered[filtered[, var2] >= var2_trld, ]

        # Now get proportion (filtered population)
        f_prop <- 100 * nrow(f_ioi) / nrow(filtered)

        # Compute difference between proportions
        diff <- round(f_prop - prop, 2)

        # Return
        return(diff)
    }

    get_ratio_boot <- function(cohort, var1, var2, var1_trld, var2_trld, i) {
        # Answers the question:
        # How much the proportion of individuals with var2 >< var2_trld
        # increase after we filter the population with var1_trld

        # Base population: cohort
        vi <- na.omit(cohort[i, c(var1, var2)])
        
        # Find individuals of interest (ioi, above/below of var2_trld)
        ioi <- vi[vi[, var2] >= var2_trld, ]

        # Get proportion
        prop <- 100 * nrow(ioi) / nrow(vi)

        # Now filter base population first using var1
        filtered <- vi[vi[, var1] >= var1_trld, ]

        # And find IoI again
        f_ioi <- filtered[filtered[, var2] >= var2_trld, ]

        # Now get proportion (filtered population)
        f_prop <- 100 * nrow(f_ioi) / nrow(filtered)

        # Compute difference between proportions
        diff <- round(f_prop - prop, 2)

        # Return
        return(diff)
    }

# Testing

Bootstrapping should always converge to the “population” estimate. When
we bootstrap, we consider the sample space to be our “population”. So,
if check the mean of a varible first by taking the sample mean

    mean(h2000$GP, na.rm = T)

    ## [1] 1.170217

we should get roughly the same number by using our bootmean() statistic
with boot().

    boot(h2000$GP, statistic = bootmean, R = 100)$t0

    ## [1] 1.170217

Let’s test this with the median now:

    median(h2000$GP, na.rm = T)

    ## [1] 1.16

    boot(h2000$GP, statistic = bootmedian, R = 100)$t0

    ## [1] 1.16

These seem to work fine. Next, we want to check the proportion of
certain kinds of people in a population before and after applying some
kind of variable filter. For example, the proportion of people above
median GP before and after we filter for ferritin lower bound of 5 ug/l
(so, the difference).

This is the result we got straight from our sample:

    get_ratio(h2000, var1 = "FERRITIINI", var2 = "GP", var1_trld = 5, var2_trld = 1.16)

    ## [1] 0.1

And this is what comes out of boot:

    boot(h2000, statistic = get_ratio_boot, R = 1000, var1 = "FERRITIINI", var2 = "GP", var1_trld = 5, var2_trld = 1.16)$t0

    ## [1] 0.1

With this configuration of the get\_ratio function the boot() seems to
work correctly. Can we break it in a surprising way? (We know we can,
this is just a demo).

    get_ratio_broken_index <- function(cohort, var1, var2, var1_trld, var2_trld, i) {
        
        # Base population: cohort
        vi <- cohort %>%
            select({{var1}}, {{var2}}) %>%
            drop_na()

        splice <- vi[i, ]
        
        # Find individuals of interest (ioi, above/below of var2_trld)
        ioi <- splice %>%
            filter({{var2}} >= {{var2_trld}})

        # Get proportion
        prop <- 100 * nrow(ioi) / nrow(splice)

        # Now filter base population first using var1
        filtered <- splice %>%
            filter({{var1}} >= {{var1_trld}})

        # And find IoI again
        f_ioi <- filtered %>% 
            filter({{var2}} >= {{var2_trld}})

        # Now get proportion (filtered population)
        f_prop <- 100 * nrow(f_ioi) / nrow(filtered)

        # Compute difference between proportions
        diff <- round(f_prop - prop, 2)

        # Return
        return(diff)
    }

    boot(h2000, statistic = get_ratio_broken_index, R = 1, var1 = FERRITIINI, var2 = GP, var1_trld = 5, var2_trld = 1.16)$t0

    ## [1] 8.07

Moving the index splicing after the selection and NA removal breaks the
result. I suspect this is because the length(i) = nrow(h2000), and
length(i) != nrow(h2000 %&gt;% select(var1, var2) %&gt;% drop\_na()).
Let’s see:

    get_ratio_broken_index_nodrop <- function(cohort, var1, var2, var1_trld, var2_trld, i) {

        # Base population: cohort
        vi <- cohort %>%
            select({{var1}}, {{var2}})
        
        splice <- vi[i, ]
        
        # Find individuals of interest (ioi, above/below of var2_trld)
        ioi <- splice %>%
            filter({{var2}} >= {{var2_trld}})

        # Get proportion
        prop <- 100 * nrow(ioi) / nrow(splice)

        # Now filter base population first using var1
        filtered <- splice %>%
            filter({{var1}} >= {{var1_trld}})

        # And find IoI again
        f_ioi <- filtered %>% 
            filter({{var2}} >= {{var2_trld}})

        # Now get proportion (filtered population)
        f_prop <- 100 * nrow(f_ioi) / nrow(filtered)

        # Compute difference between proportions
        diff <- round(f_prop - prop, 2)

        # Return
        return(diff)
    }

    boot(h2000, statistic = get_ratio_broken_index_nodrop, R = 1, var1 = FERRITIINI, var2 = GP, var1_trld = 5, var2_trld = 1.16)$t0

    ## [1] 1.33

Curious! It’s still wrong. Does select() change the size of the data so
that the indexing fails somehow?

    nrow(h2000)

    ## [1] 6264

    selected <- h2000 %>% select(FERRITIINI, GP)
    nrow(selected)

    ## [1] 6264

No, it doesn’t (as it shouldn’t). Let’s go back to the original working
one to see what happens with the indexing.

    get_ratio_boot_i <- function(cohort, var1, var2, var1_trld, var2_trld, i) {
        # Answers the question:
        # How much the proportion of individuals with var2 >< var2_trld
        # increase after we filter the population with var1_trld

        # Base population: cohort
        vi <- na.omit(cohort[, c(var1, var2)])
        
        vi <- vi[i, ]
        
        # Find individuals of interest (ioi, above/below of var2_trld)
        ioi <- vi[vi[, var2] >= var2_trld, ]

        # Get proportion
        prop <- 100 * nrow(ioi) / nrow(vi)

        # Now filter base population first using var1
        filtered <- vi[vi[, var1] >= var1_trld, ]

        # And find IoI again
        f_ioi <- filtered[filtered[, var2] >= var2_trld, ]

        # Now get proportion (filtered population)
        f_prop <- 100 * nrow(f_ioi) / nrow(filtered)

        # Compute difference between proportions
        diff <- round(f_prop - prop, 2)

        # Return
        return(diff)
    }

    boot(h2000, statistic = get_ratio_boot_i, R = 100, var1 = "FERRITIINI", var2 = "GP", var1_trld = 5, var2_trld = 1.16)$t0

    ## [1] 0.2

We get really close, but still incorrect. Even though our approach is
identical to the tidyverse version, we get a different result (tidy: 8,
this: 0.2, correct: 0.1). We’ll need to make this simpler to understand
what happens.

    # Let's create a dataset with controlled NA (1000 pcs)

    vi <- na.omit(h2000[, c("FERRITIINI", "GP")])
    scuffed_data <- vi
    set.seed(2022)
    scuffed_data[sample(1:nrow(vi), 1000, replace = F), ] <- NA

    # Now let's see if we get identical results from our two different implementations
    var1 <- "FERRITIINI"
    var2 <- "GP"
    var1_trld <- 5
    var2_trld <- 1.16
    ## BASE

    # Find individuals of interest (ioi, above/below of var2_trld)
    ioi_base <- scuffed_data[scuffed_data[, var2] >= var2_trld, ]
    # Get proportion
    prop_base <- 100 * nrow(ioi_base) / nrow(scuffed_data)
    # Now filter base population first using var1
    filtered_base <- scuffed_data[scuffed_data[, var1] >= var1_trld, ]
    # And find IoI again
    f_ioi_base <- filtered_base[filtered_base[, var2] >= var2_trld, ]
    # Now get proportion (filtered population)
    f_prop_base <- 100 * nrow(f_ioi_base) / nrow(filtered_base)
    # Compute difference between proportions
    diff_base <- round(f_prop_base - prop_base, 2)

    ## TIDY
    # Find individuals of interest (ioi, above/below of var2_trld)
    ioi_tidy <- scuffed_data %>%
        filter(GP >= var2_trld)

    # Get proportion
    prop_tidy <- 100 * nrow(ioi_tidy) / nrow(scuffed_data)

    # Now filter base population first using var1
    filtered_tidy <- scuffed_data %>%
        filter(FERRITIINI >= var1_trld)

    # And find IoI again
    f_ioi_tidy <- filtered_tidy %>% 
        filter(GP >= var2_trld)

    # Now get proportion (filtered population)
    f_prop_tidy <- 100 * nrow(f_ioi_tidy) / nrow(filtered_tidy)

    # Compute difference between proportions
    diff_tidy <- round(f_prop_tidy - prop_tidy, 2)


    print(c(diff_base, diff_tidy))

    ## [1] 0.19 9.42

We can now see that they produce wildly different estimates. Tidy must
handle NAs differently here. Let’s see how.

    nrow(ioi_base) 

    ## [1] 3087

    nrow(ioi_tidy)

    ## [1] 2087

We find that filter() strips NA automatically (which you can also find
by reading the documentation). This in turn affects the proportion
calculations, as it is done using nrow(). I consider this solved.
