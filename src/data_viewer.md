We have one cohort of blood donors, FinDonor, and two general population
cohorts, FinRisk97 and Health2000. The FinDonor cohort does not include
the variables of interest, so we’ll use data from individual donations
(by donors), which is cross-referenceable to the FinDonor set.

# Warmup | Getting to know the data

The data we have presented as CRP x Ferritin plots. The left hand side
presents data as is, the right hand side presents the data after log
transform. The positive correlation between CRP and Ferritin is much
more obvious when we log transform the data. The Health2000 data has
many values set as 0.00 (missing data?), so these values are probably
creating the points at -5 (log(0) = -Inf). The FinDonor data requires
some attention also: the CRP measurements have not been taken using a
high sensitivity method, as we were not interested in “non-elevated”
CRP. So, every CRP value under 3 is missing, and here they have been
imputed as “2.9”. Additionally, the resolution extends only to integers,
which creates visible stratification/intervals after the log
transformation of the data. The FinDonor is the only data set where the
positive correlation is thus not immediately visible.

<img src="data_viewer_files/figure-markdown_strict/crp_ferr_all_nonlog-1.png" style="display: block; margin: auto;" />

Still working with the entire cohorts (no subgroup analysis yet). We
sort the data (each cohort separately) by the ferritin measurements and
take 10 quantiles from each sorted cohort. If we then take the ferritin
and CRP medians from each of the quantiles, we can plot them to examine
the relationship in ordinal “categories”. This is raw data without
transformations. NaNs are stripped.

The positive correlation between ferritin and CRP is obvious in the
FINRISK97 and Health2000 cohorts, but as the medians never cross 3, the
FinDonor cohort shows nothing.

<img src="data_viewer_files/figure-markdown_strict/plot_quantiles-1.png" style="display: block; margin: auto;" />

Analysis of means reveals that the FinDonor CRP x Ferritin behaves
similarly.

<img src="data_viewer_files/figure-markdown_strict/plot_mean_quantiles-1.png" style="display: block; margin: auto;" />
What happens to CRP when we start filtering donors based on ferritin
levels? Let’s say that we want to set the LB of donor ferritin to 30
ug/l. To see the effect, we want to compare the CRP medians/means of
population filtered and not filtered by the ferritin LB.

<img src="data_viewer_files/figure-markdown_strict/plot_LB_diff-1.png" style="display: block; margin: auto;" />

Using ferritin LB of 30 ug/l, the share of potential donors in
inflammatory risk (e.g. vascular or heart disease, indicated by CRP
&gt;= 3 mg/l) increases from 18.52% to 20.72% in the FINRISK1997 cohort
and from 16.22% to 17.84% in the Health2000 cohort. Within the existing
donor population (indicated by the FinDonor cohort) the share goes from
14.85% to 15.85%.

Thinking in reverse, what is the maximum ferritin threshold we could set
without increasing the share of donors under inflammation risk?

<img src="data_viewer_files/figure-markdown_strict/unnamed-chunk-1-1.png" style="display: block; margin: auto;" />

These density plots indicate that dodging CRP 3 mg/l might not be
possible based on ferritin value alone. Do these distributions ever
separate?

<img src="data_viewer_files/figure-markdown_strict/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

# Actionable analysis

## Ferritin x CRP & GlycA & HbA<sub>1*C*</sub>

The plot below shows what happens to common inflammation marker means
and medians within the population when we start requiring a certain
ferritin threshold for donors. Filter value for ferritin is set to a
*lower bound* of 30 ug/l for these analyses. The CRP variable is
available in every cohort, but the GP is only found from FINRISK97 and
Health200. Finally, the glycated hemoglobin (HbA1C) is only available in
the Health2000 cohort.

    sp1 <- data.frame(median = c(frmd$t0, frfilmd$t0, h00md$t0, h00filmd$t0),
                             lower = c(frcimd$normal[2], frfilcimd$normal[2], h00cimd$normal[2], h00filcimd$normal[2]),
                             upper = c(frcimd$normal[3], frfilcimd$normal[3], h00cimd$normal[3], h00filcimd$normal[3]),
                             filter = rep(c("Unguided", "Guided"), 2),
                             cohort = c(rep("FINRISK97", 2), rep("Health2000", 2)),
                             plotorder = c(1, 1.5, 2.25, 2.75))

    ggplot(data = sp1, aes(x = median, y = plotorder)) + 
        geom_errorbarh(aes(xmin = lower, xmax = upper, color = filter), height = 0.1) +
        geom_point() +
        theme_minimal() +
        scale_y_reverse() +
        geom_text(aes(x = 0, y = plotorder, label = paste(cohort, "|", filter, "| Median:", round(median, 2))), size = 4, hjust = "inward") +
        theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), legend.position = "none") +
        labs(title = "Population CRP median by cohort and whether selected by ferritin >30 ug/l", 
             subtitle = "95 % confidence interval (10k bootstrap samples)",
             x = "Mean", 
             color = "Ferritin > 30 ug/l")

![](data_viewer_files/figure-markdown_strict/sanquin_plot1-1.png)

    sp2 <- data.frame(median = c(frGPmd$t0, frGPfilmd$t0, h00GPmd$t0, h00GPfilmd$t0),
                             lower = c(frGPcimd$normal[2], frGPfilcimd$normal[2], h00GPcimd$normal[2], h00GPfilcimd$normal[2]),
                             upper = c(frGPcimd$normal[3], frGPfilcimd$normal[3], h00GPcimd$normal[3], h00GPfilcimd$normal[3]),
                             filter = rep(c("Unguided", "Guided"), 2),
                             cohort = c(rep("FINRISK97", 2), rep("Health2000", 2)),
                             plotorder = c(1, 1.5, 2.25, 2.75))

    ggplot(data = sp2, aes(x = median, y = plotorder)) + 
        geom_errorbarh(aes(xmin = lower, xmax = upper, color = filter), height = 0.1) +
        geom_point() +
        theme_minimal() +
        scale_y_reverse() +
        geom_text(aes(x = 0, y = plotorder, label = paste(cohort, "|", filter, "| Median:", round(median, 2))), size = 4, hjust = "inward") +
        theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), legend.position = "none") +
        labs(title = "Population GlycA median by cohort and whether selected by ferritin >30 ug/l", 
             subtitle = "95 % confidence interval (10k bootstrap samples)",
             x = "Mean", 
             color = "Ferritin > 30 ug/l")

![](data_viewer_files/figure-markdown_strict/sanquin_plot2-1.png)

    sp3 <- data.frame(median = c(h00HbAmd$t0, h00HbAfilmd$t0),
                             lower = c(h00HbAmd$t0, h00HbAfilcimd$normal[2]),
                             upper = c(h00HbAmd$t0, h00HbAfilcimd$normal[3]),
                             filter = c("Unguided", "Guided"),
                             cohort = c(rep("Health2000", 2)),
                             plotorder = c(1, 1.5))

    ggplot(data = sp3, aes(x = median, y = plotorder)) + 
        geom_errorbarh(aes(xmin = lower, xmax = upper, color = filter), height = 0.1) +
        geom_point() +
        theme_minimal() +
        ylim(c(3,0)) +
        geom_text(aes(x = 0, y = plotorder, label = paste(cohort, "|", filter, "| Median:", round(median, 2))), size = 4, hjust = "inward") +
        theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), legend.position = "none") +
        labs(title = "Population HbA1C median (mmol/mol) and whether selected by ferritin >30 ug/l", 
             subtitle = "Cohort: Health2000 | 95 % confidence interval (10k bootstrap samples)",
             x = "Mean", 
             color = "Ferritin > 30 ug/l")

![](data_viewer_files/figure-markdown_strict/sanquin_plot3-1.png)

<img src="data_viewer_files/figure-markdown_strict/LBresults_multiplot-1.png" style="display: block; margin: auto;" />

<img src="data_viewer_files/figure-markdown_strict/prop_plot-1.png" style="display: block; margin: auto;" />
