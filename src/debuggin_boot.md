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
        vi <- cohort %>%
            select({{var1}}, {{var2}}) %>% # variables of interest
            drop_na()

        # Splice for bootstrapping
        splice <- vi[i, ]

        # Find individuals of interest (ioi, above/below of var2_trld)
        ioi <- splice %>%
            filter({{var2}} >= {{var2_trld}})

        # Get proportion
        prop <- 100 * dim(ioi)[1] / dim(splice)[1]

        # Now filter base population first using var1
        filtered <- splice %>%
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

These seem to work fine. **The question is then, why doesn’t our
proportion statistic function work with boot?** We want to check the
proportion of certain kinds of people in a population before and after
applying some kind of variable filter. For example, the proportion of
people above median GP before and after we filter for ferritin lower
bound of 5 ug/l (so, the difference).

This is the result we got straight from our sample:

    get_ratio(h2000, var1 = FERRITIINI, var2 = GP, var1_trld = 5, var2_trld = 1.16)

    ## [1] 0.1

And this is what comes out of boot:

    boot(h2000, statistic = get_ratio_boot, R = 100, var1 = FERRITIINI, var2 = GP, var1_trld = 5, var2_trld = 1.16)$t0

    ## [1] 8.07

That is an increase of 8000% in the estimate. There is no way the data
shape produces this, so I must be using the boot() function call
incorrectly.

We can do a sanity check by using our own bootstrap hack:

    res <- c()
    for (k in 1:100) {
        res[k] <- get_ratio_boot(h2000, var1 = FERRITIINI, var2 = GP, var1_trld = 5, var2_trld = 1.16, i = sample(1:5000, 5000, replace = T))
    }
    mean(res)

    ## [1] 0.1174

Our own boothack comes much more closer to the real value in the sample
space. So what is wrong with our boot()?

Let’s try dumbing down our statistic function

    dumb_ratio_boot <- function(data, i) {
        # this version does not require the passing of ... arguments to boot()
        splice <- data[i, ]
        
        # Find individuals of interest (ioi, above/below of var2_trld)
        ioi <- splice %>%
            filter(GP >= 1.16)

        # Get proportion
        prop <- 100 * dim(ioi)[1] / dim(splice)[1]

        # Now filter base population first using var1
        filtered <- splice %>%
            filter(FERRITIINI >= 5)

        # And find IoI again
        f_ioi <- filtered %>%
            filter(GP >= 1.16)

        # Now get proportion (filtered population)
        f_prop <- 100 * dim(f_ioi)[1] / dim(filtered)[1]

        # Compute difference between proportions
        diff <- round(f_prop - prop, 2)

        # Return
        return(diff)
    }

Now let’s try it

    boot(h2000, statistic = dumb_ratio_boot, R = 100)$t0

    ## [1] 1.33

We got closer somehow? What if we remove all tidy syntax?

    dumber_ratio_boot <- function(data, i) {
        # this version does not require the passing of ... arguments to boot()
        splice <- data[i, ]
        
        # Find individuals of interest (ioi, above/below of var2_trld)
        ioi <- splice[splice$GP >= 1.16, ]

        # Get proportion
        prop <- 100 * dim(ioi)[1] / dim(splice)[1]

        # Now filter base population first using var1
        filtered <- splice[splice$FERRITIINI >= 5, ]

        # And find IoI again
        f_ioi <- filtered[filtered$GP >= 1.16, ]

        # Now get proportion (filtered population)
        f_prop <- 100 * dim(f_ioi)[1] / dim(filtered)[1]

        # Compute difference between proportions
        diff <- round(f_prop - prop, 2)

        # Return
        return(diff)
    }

    boot(h2000, statistic = dumber_ratio_boot, R = 100)$t0

    ## [1] 5.55

We get a worse result :(
