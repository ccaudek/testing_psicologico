# Functions by https://github.com/JoranTiU/sleasy/tree/main/R

globalFit <- function(x) {
    chisq_scaled <- NA
    chisq_scaled <- fitMeasures(x)[["chisq.scaled"]]

    if (!is.na(chisq_scaled)) {
        df_chisq_scaled <- fitMeasures(x)[["df.scaled"]]
        pvalue_chisq_scaled <- fitMeasures(x)[["pvalue.scaled"]]
        cfi_scaled <- fitMeasures(x)[["cfi.scaled"]]
        rmsea_scaled <- fitMeasures(x)[["rmsea.scaled"]]
        rmsea_lower_scaled <- fitMeasures(x)[["rmsea.ci.lower.scaled"]]
        rmsea_upper_scaled <- fitMeasures(x)[["rmsea.ci.upper.scaled"]]
        srmr_scaled <- fitMeasures(x)[["srmr_mplus"]]
    }

    chisq <- fitMeasures(x)[["chisq"]]
    df_chisq <- fitMeasures(x)[["df"]]
    pvalue_chisq <- fitMeasures(x)[["pvalue"]]
    cfi <- fitMeasures(x)[["cfi"]]
    rmsea <- fitMeasures(x)[["rmsea"]]
    rmsea_lower <- fitMeasures(x)[["rmsea.ci.lower"]]
    rmsea_upper <- fitMeasures(x)[["rmsea.ci.upper"]]
    srmr <- fitMeasures(x)[["srmr"]]
    null_rmsea <- as.numeric(nullRMSEA(x, silent = TRUE))
    # null_rmsea <- 0.100 # For testing purposes



    # normal =====================================================================

    cat(
        "Results------------------------------------------------------------------------",
        "\n", "\n"
    )

    if (is.na(chisq_scaled)) {
        cat("Chi-Square (", df_chisq, ") = ", chisq, " with p-value = ",
            pvalue_chisq, "\n", "\n",
            "CFI = ", cfi, "\n", "\n",
            "RMSEA = ", rmsea, "; lower bound = ", rmsea_lower, "; upper bound = ",
            rmsea_upper, "\n", "\n",
            "SRMR = ", srmr, "\n", "\n",
            sep = ""
        )

        # Chi-Square-----------------------------------------------------------------
        cat(
            "Interpretations---------------------------------------------------------------",
            "\n", "\n"
        )

        if (pvalue_chisq >= 0.05) {
            cat("The hypothesis of perfect fit *is not* rejected according to the
          Chi-Square test statistics because the p-value is larger or equal
          to 0.05.", "\n", "\n")
        } else {
            cat(
                "The hypothesis of perfect fit *is* rejected according to the Chi-
          Square test statistics because the p-value is smaller than 0.05",
                "\n", "\n"
            )
        }

        # CFI  ----------------------------------------------------------------------
        if (null_rmsea < 0.158) {
            cat("The RMSEA of the null-model is less than .158. Therefore the CFI is
          likely not informative as a measure of fit for this model, and will
          not be reported", "\n", "\n")
        } else {
            if (cfi > 0.9) {
                cat("The hypothesis of approximate model fit *is not* rejected according
          to the CFI because the value is larger than 0.9.", "\n", "\n")
            } else {
                cat("The hypothesis of approximate model fit *is* rejected according to
          the CFI because the value is smaller or equal than 0.9.", "\n", "\n")
            }
        }

        # RMSEA----------------------------------------------------------------------
        if (rmsea < 0.05) {
            if (rmsea_upper < 0.05) {
                cat("The hypothesis of approximate model fit *is not* rejected according
         to the RMSEA because the point estimate and the upper-limit of it's
               95% CI are smaller than 0.05.", "\n", "\n")
            } else {
                cat("The RMSEA is smaller than 0.05, but the upper-limit of it's
               95% CI is not. Therefore the hypothesis of approximate model
               fit *might be* rejected according to the RMSEA", "\n", "\n")
            }
        } else {
            cat("The hypothesis of approximate model fit *is* rejected according
         to the RMSEA because the point estimate is larger or equal to
         0.05.", "\n", "\n")
        }

        # SRMR-----------------------------------------------------------------------
        if (srmr < 0.08) {
            cat(
                "The hypothesis of approximate model fit *is not* rejected according
          to the SRMR because the value is smaller than 0.08.",
                "\n", "\n"
            )
        } else {
            cat("The hypothesis of approximate model fit *is* rejected according
          to the SRMR because the value is larger or equal to
          0.08.", "\n", "\n")
        }
        # non-normal==================================================================
    } else {
        cat("Chi-Square (", df_chisq_scaled, ") = ", chisq_scaled, " with p-value
          = ", pvalue_chisq_scaled, "\n", "\n",
            "CFI = ", cfi_scaled, "\n", "\n",
            "RMSEA = ", rmsea_scaled, "; lower bound = ", rmsea_lower_scaled, ";
      upper bound = ", rmsea_upper_scaled, "\n", "\n",
            "SRMR = ", srmr_scaled, "\n", "\n",
            sep = ""
        )

        cat(
            "Interpretations---------------------------------------------------------------",
            "\n", "\n"
        )
        # Chi-Square-----------------------------------------------------------------
        if (pvalue_chisq_scaled >= 0.05) {
            cat("The hypothesis of perfect fit *is not* rejected according to the
          Chi-Square test statistics because the p-value is larger or equal
          to 0.05.", "\n", "\n")
        } else {
            cat(
                "The hypothesis of perfect fit *is* rejected according to the Chi-
          Square test statistics because the p-value is smaller than 0.05",
                "\n", "\n"
            )
        }

        # CFI  ----------------------------------------------------------------------
        if (null_rmsea < 0.158) {
            cat("The RMSEA of the null-model is less than .158. Therefore the CFI is
          likely not informative as a measure of fit for this model, and will
          not be reported", "\n", "\n")
        } else {
            if (cfi_scaled > 0.9) {
                cat("The hypothesis of approximate model fit *is not* rejected according
          to the CFI because the value is larger than 0.9.", "\n", "\n")
            } else {
                cat("The hypothesis of approximate model fit *is* rejected according to
          the CFI because the value is smaller or equal than 0.9.", "\n", "\n")
            }
        }

        # RMSEA----------------------------------------------------------------------
        if (rmsea_scaled < 0.05) {
            if (rmsea_upper_scaled < 0.05) {
                cat("The hypothesis of approximate model fit *is not* rejected according
         to the RMSEA because the point estimate and the upper-limit of it's
               95% CI are smaller than 0.05.", "\n", "\n")
            } else {
                cat("The RMSEA is smaller than 0.05, but the upper-limit of it's
               95% CI is not. Therefore the hypothesis of approximate model
               fit *might be* rejected according to the RMSEA", "\n", "\n")
            }
        } else {
            cat("The hypothesis of approximate model fit *is* rejected according
         to the RMSEA because the point estimate is larger or equal to
         0.05.", "\n", "\n")
        }
        # SRMR-----------------------------------------------------------------------
        if (srmr_scaled < 0.08) {
            cat(
                "The hypothesis of approximate model fit *is not* rejected according
         to the SRMR because the value is smaller than 0.08.",
                "\n", "\n"
            )
        } else {
            cat(
                "The hypothesis of approximate model fit *is* rejected according
         to the SRMR because the value is larger or equal to 0.08.",
                "\n", "\n"
            )
        }
    }
}

invarianceCheck <- function(model, data, group, estimator = "MLR",
                            intercept = FALSE, missing = FALSE, display = FALSE) {
    if (estimator == "MLR") {
        if (intercept == FALSE) {
            if (missing == FALSE) {
                cfa <- cfa(model, data,
                    std.lv = TRUE, meanstructure = TRUE,
                    group = group, estimator = "MLR"
                )

                loading_invariance <- cfa(model, data,
                    std.lv = TRUE, meanstructure = TRUE,
                    group = group,
                    group.equal = c("loadings"),
                    estimator = "MLR"
                )
            } else {
                cfa <- cfa(model, data,
                    std.lv = TRUE, missing = "fiml",
                    group = group, estimator = "MLR"
                )

                loading_invariance <- cfa(model, data,
                    std.lv = TRUE,
                    missing = "fiml", group = group,
                    group.equal = c("loadings"), estimator = "MLR"
                )
            }

            load_check <- semTools::compareFit(
                Loadings_Invariant = loading_invariance,
                Loadings_Free = cfa
            )
            summary.FitDiff(load_check)

            pvalue_chisq_diff <- as.numeric(
                anova(loading_invariance, cfa)$"Pr(>Chisq)"[2]
            )

            diff_cfi_scaled <- fitMeasures(loading_invariance)[["cfi.scaled"]] -
                fitMeasures(cfa)[["cfi.scaled"]]
            diff_rmsea_scaled <- fitMeasures(loading_invariance)[["rmsea.scaled"]] -
                fitMeasures(cfa)[["rmsea.scaled"]]
            diff_srmr_scaled <- fitMeasures(loading_invariance)[["srmr"]] -
                fitMeasures(cfa)[["srmr"]]

            # Chi-Square --------------------------------------------------------------
            cat("Loading Invariance Interpretation --------------------------------------------------------", "\n", "\n")

            if (pvalue_chisq_diff >= 0.05) {
                cat("The hypothesis of perfect loading invariance *is not* rejected according to the
      Chi-Square difference test statistics because the p-value is larger or equal to 0.05.", "\n", "\n")
            } else {
                cat("The hypothesis of perfect loading invariance *is* rejected according
           to the Chi-Square difference test statistics because the p-value is
           smaller than 0.05", "\n", "\n")
            }
            # CFI  --------------------------------------------------------------------
            if (diff_cfi_scaled <= .01) {
                cat("The hypothesis of approximate loading invariance *is not* rejected
        according to the CFI because the difference in CFI value is smaller than
        or equal to 0.01.", "\n", "\n")
            } else {
                cat("The hypothesis of approximate approximate loading invariance *is*
        rejected according to the CFI because the difference in CFI value is
        larger than 0.01.", "\n", "\n")
            }


            # RMSEA -------------------------------------------------------------------
            if (diff_rmsea_scaled <= 0.015) {
                cat("The hypothesis of approximate loading invariance *is not* rejected
       according to the RMSEA because the difference in RMSEA value is smaller
       than or equal to 0.015.", "\n", "\n")
            } else {
                cat("The hypothesis of approximate approximate loading invariance *is*
       rejected according to the RMSEA because the difference in RMSEA value is
       larger than 0.015.", "\n", "\n")
            }

            # SRMR --------------------------------------------------------------------
            if (diff_srmr_scaled < .030) {
                cat("The hypothesis of approximate loading invariance *is not* rejected
       according to the SRMR because the difference in SRMR value is smaller than
       or equal to 0.030.", "\n", "\n")
            } else {
                cat("The hypothesis of approximate approximate loading invariance *is*
       rejected according to the SRMR because the difference in SRMR value is
       larger than 0.030.", "\n", "\n")
            }

            t1 <- eval(parse(text = c(paste0(
                "dim(inspect(cfa, \"est\")$`",
                as.numeric(levels(pull(data, group))[1]), "`$lambda)"
            ))))

            loadings <- array(NA, dim = c(t1[1], t1[2], length(levels(pull(data, group)))))
            group_upd <- as.numeric(pull(data, group))

            for (i in min(group_upd):max(group_upd)) {
                eval(parse(text = c(
                    paste0(
                        "loadings[,,", i, "] <-", "inspect(cfa, \"est\")$`",
                        as.numeric(levels(pull(data, group))[i]), "`$lambda"
                    )
                )))
            }

            rowlab <- eval(parse(text = c(paste0(
                "dimnames(inspect(cfa, \"est\")$`",
                as.numeric(levels(pull(data, group))[1]), "`$lambda)[[1]]"
            ))))

            collab <- eval(parse(text = c(paste0(
                "dimnames(inspect(cfa, \"est\")$`",
                as.numeric(levels(pull(data, group))[1]), "`$lambda)[[2]]"
            ))))

            dimnames(loadings) <- list(rowlab, collab, levels(pull(data, group)))

            combis <- as.matrix(combn(unique(group_upd), 2))

            loading_diff <- array(NA, dim = c(t1[1], t1[2], ncol(combis)))
            label_diff <- rep(NA, ncol(combis))

            for (j in 1:ncol(combis)) {
                loading_diff[, , j] <- loadings[, , combis[1, j]] - loadings[, , combis[2, j]]
                label_diff[j] <- paste0(
                    levels(pull(data, group))[combis[1, j]], "-",
                    levels(pull(data, group))[combis[2, j]]
                )
            }

            dimnames(loading_diff) <- list(rowlab, collab, label_diff)
            load_diff_abs <- abs(loading_diff)
            maxind <- which(load_diff_abs == max((load_diff_abs)), arr.ind = TRUE)

            if (display == TRUE) {
                cat("Factor loadings for each group ---------------------------------------------------------", "\n", "\n")
                print(loadings)
                cat("Difference in loadings between groups --------------------------------------------------", "\n", "\n")
                print(loading_diff)

                print(paste(
                    "The largest difference in loading is that of variable",
                    dimnames(loading_diff)[[1]][maxind[1]], "on factor",
                    dimnames(loading_diff)[[2]][maxind[2]], "for group comparison",
                    dimnames(loading_diff)[[3]][maxind[3]]
                ))
            }
        } else {
            if (missing == FALSE) {
                cfa <- cfa(model, data,
                    std.lv = TRUE, meanstructure = TRUE,
                    group = group, estimator = "MLR"
                )

                loading_invariance <- cfa(model, data,
                    std.lv = TRUE, meanstructure = TRUE,
                    group = group,
                    group.equal = c("loadings"), estimator = "MLR"
                )

                intercept_invariance <- cfa(model, data,
                    std.lv = TRUE, meanstructure = TRUE,
                    group = group,
                    group.equal = c("loadings", "intercepts"),
                    estimator = "MLR"
                )
            } else {
                cfa <- cfa(model, data,
                    std.lv = TRUE, missing = "fiml",
                    group = group, estimator = "MLR"
                )

                loading_invariance <- cfa(model, data,
                    std.lv = TRUE,
                    missing = "fiml", group = group,
                    group.equal = c("loadings"), estimator = "MLR"
                )

                intercept_invariance <- cfa(model, data,
                    std.lv = TRUE,
                    missing = "fiml", group = group,
                    group.equal = c("loadings", "intercepts"),
                    estimator = "MLR"
                )
            }


            pvalue_chisq_diff1 <- as.numeric(
                anova(intercept_invariance, loading_invariance, cfa)$"Pr(>Chisq)"[2]
            )

            diff_cfi_scaled1 <- fitMeasures(loading_invariance)[["cfi.scaled"]] -
                fitMeasures(cfa)[["cfi.scaled"]]
            diff_rmsea_scaled1 <- fitMeasures(loading_invariance)[["rmsea.scaled"]] -
                fitMeasures(cfa)[["rmsea.scaled"]]
            diff_srmr_scaled1 <- fitMeasures(loading_invariance)[["srmr"]] -
                fitMeasures(cfa)[["srmr"]]

            SC1 <- sum(
                as.numeric(pvalue_chisq_diff1 > .05),
                as.numeric(diff_cfi_scaled1 <= .01),
                as.numeric(diff_rmsea_scaled1 <= 0.015),
                as.numeric(diff_srmr_scaled1 <= 0.030)
            )

            if (SC1 >= 3) {
                int_check <- semTools::compareFit(
                    Intercept_Invariant =
                        intercept_invariance, Loadings_Invariant = loading_invariance,
                    Loadings_Intercept_Free = cfa
                )

                summary.FitDiff(int_check)

                pvalue_chisq_diff2 <- as.numeric(
                    anova(intercept_invariance, loading_invariance, cfa)$"Pr(>Chisq)"[3]
                )

                diff_cfi_scaled2 <- fitMeasures(intercept_invariance)[["cfi.scaled"]] -
                    fitMeasures(loading_invariance)[["cfi.scaled"]]
                diff_rmsea_scaled2 <- fitMeasures(intercept_invariance)[["rmsea.scaled"]] -
                    fitMeasures(loading_invariance)[["rmsea.scaled"]]
                diff_srmr_scaled2 <- fitMeasures(intercept_invariance)[["srmr"]] -
                    fitMeasures(loading_invariance)[["srmr"]]

                # Chi-Square ------------------------------------------------------------
                cat("Loading Invariance Interpretation --------------------------------------------------------", "\n", "\n")

                if (pvalue_chisq_diff1 >= 0.05) {
                    cat("The hypothesis of perfect loading invariance *is not* rejected
          according to the Chi-Square difference test statistics because the p-value
          is larger or equal to 0.05.", "\n", "\n")
                } else {
                    cat("The hypothesis of perfect loading invariance *is* rejected according
          to the Chi-Square difference test statistics because the p-value is
          smaller than 0.05", "\n", "\n")
                }

                # CFI  ------------------------------------------------------------------
                if (diff_cfi_scaled1 <= .01) {
                    cat("The hypothesis of approximate loading invariance *is not* rejected
         according to the CFI because the difference in CFI value is smaller than
         or equal to 0.01.", "\n", "\n")
                } else {
                    cat("The hypothesis of approximate approximate loading invariance *is*
         rejected according to the CFI because the difference in CFI value is
         larger than 0.01.", "\n", "\n")
                }

                # RMSEA -----------------------------------------------------------------
                if (diff_rmsea_scaled1 <= 0.015) {
                    cat("The hypothesis of approximate loading invariance *is not* rejected
         according to the RMSEA because the difference in RMSEA value is smaller
         than or equal to 0.015.", "\n", "\n")
                } else {
                    cat("The hypothesis of approximate approximate loading invariance *is*
         rejected according to the RMSEA because the difference in RMSEA value is
         larger than 0.015.", "\n", "\n")
                }

                # SRMR ------------------------------------------------------------------
                if (diff_srmr_scaled1 <= .030) {
                    cat("The hypothesis of approximate loading invariance *is not* rejected
         according to the SRMR because the difference in SRMR value is smaller than
         or equal to 0.030.", "\n", "\n")
                } else {
                    cat("The hypothesis of approximate approximate loading invariance *is*
         rejected according to the SRMR because the difference in SRMR value is
         larger than 0.030.", "\n", "\n")
                }

                cat("Intercept Invariance Interpretation ------------------------------------------------------", "\n", "\n")

                if (pvalue_chisq_diff2 >= 0.05) {
                    cat("The hypothesis of perfect intercept invariance *is not* rejected
         according to the Chi-Square difference test statistics because the p-value
         is larger or equal to 0.05.", "\n", "\n")
                } else {
                    cat("The hypothesis of perfect intercept invariance *is* rejected
         according to the Chi-Square difference test statistics because the p-value
         is smaller than 0.05", "\n", "\n")
                }

                if (diff_cfi_scaled2 <= .01) {
                    cat("The hypothesis of approximate intercept invariance *is not*
         rejected according to the CFI because the difference in CFI value is
         smaller than or equal to 0.01.", "\n", "\n")
                } else {
                    cat("The hypothesis of approximate approximate intercept invariance *is*
         rejected according to the CFI because the difference in CFI value is
         larger than 0.01.", "\n", "\n")
                }

                if (diff_rmsea_scaled2 <= 0.015) {
                    cat("The hypothesis of approximate intercept invariance *is not* rejected
         according to the RMSEA because the difference in RMSEA value is smaller
         than or equal to 0.015.", "\n", "\n")
                } else {
                    cat("The hypothesis of approximate approximate intercept invariance *is*
         rejected according to the RMSEA because the difference in RMSEA value is
         larger than 0.015.", "\n", "\n")
                }

                if (diff_srmr_scaled2 <= .030) {
                    cat("The hypothesis of approximate intercept invariance *is not* rejected
         according to the SRMR because the difference in SRMR value is smaller than
         or equal to 0.030.", "\n", "\n")
                } else {
                    cat("The hypothesis of approximate approximate intercept invariance *is*
         rejected according to the SRMR because the difference in SRMR value is
         larger than 0.030.", "\n", "\n")
                }

                t1 <- eval(parse(text = c(paste0(
                    "dim(inspect(cfa, \"est\")$`",
                    as.numeric(levels(pull(data, group))[1]), "`$lambda)"
                ))))

                loadings <- array(NA, dim = c(t1[1], t1[2], length(levels(pull(data, group)))))
                group_upd <- as.numeric(pull(data, group))

                for (i in min(group_upd):max(group_upd)) {
                    eval(parse(text = c(
                        paste0(
                            "loadings[,,", i, "] <-", "inspect(cfa, \"est\")$`",
                            as.numeric(levels(pull(data, group))[i]), "`$lambda"
                        )
                    )))
                }

                rowlab <- eval(parse(text = c(paste0(
                    "dimnames(inspect(cfa, \"est\")$`",
                    as.numeric(levels(pull(data, group))[1]), "`$lambda)[[1]]"
                ))))

                collab <- eval(parse(text = c(paste0(
                    "dimnames(inspect(cfa, \"est\")$`",
                    as.numeric(levels(pull(data, group))[1]), "`$lambda)[[2]]"
                ))))

                dimnames(loadings) <- list(rowlab, collab, levels(pull(data, group)))

                combis <- as.matrix(combn(unique(group_upd), 2))

                loading_diff <- array(NA, dim = c(t1[1], t1[2], ncol(combis)))
                label_diff <- rep(NA, ncol(combis))

                for (j in 1:ncol(combis)) {
                    loading_diff[, , j] <- loadings[, , combis[1, j]] - loadings[, , combis[2, j]]
                    label_diff[j] <- paste0(
                        levels(pull(data, group))[combis[1, j]], "-",
                        levels(pull(data, group))[combis[2, j]]
                    )
                }

                dimnames(loading_diff) <- list(rowlab, collab, label_diff)


                t2 <- eval(parse(text = c(paste0(
                    "dim(inspect(cfa, \"est\")$`",
                    as.numeric(levels(pull(data, group))[1]), "`$nu)"
                ))))

                intercepts <- array(NA, dim = (c(t2[1], length(levels(pull(data, group))))))
                group_upd <- as.numeric(pull(data, group))

                for (i in min(group_upd):max(group_upd)) {
                    eval(parse(text = c(
                        paste0(
                            "intercepts[,", i, "] <-", "inspect(cfa, \"est\")$`",
                            as.numeric(levels(pull(data, group))[i]), "`$nu"
                        )
                    )))
                }

                rowlab2 <- eval(parse(text = c(paste0(
                    "dimnames(inspect(cfa, \"est\")$`",
                    as.numeric(levels(pull(data, group))[1]), "`$nu)[[1]]"
                ))))

                dimnames(intercepts) <- list(rowlab2, levels(pull(data, group)))

                combis <- as.matrix(combn(unique(group_upd), 2))

                intercept_diff <- array(NA, dim = c(t2[1], ncol(combis)))
                label_diff <- rep(NA, ncol(combis))

                for (j in 1:ncol(combis)) {
                    intercept_diff[, j] <- intercepts[, combis[1, j]] - intercepts[, combis[2, j]]
                    label_diff[j] <- paste0(
                        levels(pull(data, group))[combis[1, j]], "-",
                        levels(pull(data, group))[combis[2, j]]
                    )
                }

                dimnames(intercept_diff) <- list(rowlab2, label_diff)

                load_diff_abs <- abs(loading_diff)
                int_diff_abs <- abs(intercept_diff)
                maxind <- which(load_diff_abs == max((load_diff_abs)), arr.ind = TRUE)
                maxind2 <- which(int_diff_abs == max((int_diff_abs)), arr.ind = TRUE)

                if (display == TRUE) {
                    cat("Factor loadings for each group ---------------------------------------------------------", "\n", "\n")
                    print(loadings)
                    cat("Difference in loadings between groups --------------------------------------------------", "\n", "\n")
                    print(loading_diff)

                    print(paste(
                        "The largest difference in loading is that of variable",
                        dimnames(loading_diff)[[1]][maxind[1]],
                        "on factor", dimnames(loading_diff)[[2]][maxind[2]], "for group comparison",
                        dimnames(loading_diff)[[3]][maxind[3]]
                    ))
                    cat("\n")
                    cat("Intercepts for each group --------------------------------------------------------------", "\n", "\n")
                    print(intercepts)
                    cat("\n")
                    cat("Difference in intercepts between groups ------------------------------------------------", "\n", "\n")
                    print(intercept_diff)
                    cat("\n")
                    print(paste(
                        "The largest difference in intercept is that of variable",
                        dimnames(intercept_diff)[[1]][maxind2[1]],
                        "for group comparison", dimnames(intercept_diff)[[2]][maxind2[2]]
                    ))
                }
            } else {
                loading_invariance <- cfa(model, data,
                    std.lv = TRUE,
                    missing = "fiml", group = group,
                    group.equal = c("loadings"), estimator = "MLR"
                )

                load_check <- semTools::compareFit(
                    Loadings_Invariant = loading_invariance,
                    Loadings_Free = cfa
                )
                summary.FitDiff(load_check)

                cat("Loading Invariance Interpretation -----------------------------------------------------------", "\n", "\n")

                if (pvalue_chisq_diff1 >= 0.05) {
                    cat("The hypothesis of perfect loading invariance *is not* rejected
           according to the Chi-Square difference test statistics because the p-value
           is larger or equal to 0.05.", "\n", "\n")
                } else {
                    cat("The hypothesis of perfect loading invariance *is* rejected according
           to the Chi-Square difference test statistics because the p-value is
           smaller than 0.05", "\n", "\n")
                }

                # CFI  ----------------------------------------------------------------
                if (diff_cfi_scaled1 <= .01) {
                    cat("The hypothesis of approximate loading invariance *is not* rejected
           according to the CFI because the difference in CFI value is smaller than
           or equal to 0.01.", "\n", "\n")
                } else {
                    cat("The hypothesis of approximate approximate loading invariance *is*
           rejected according to the CFI because the difference in CFI value is
           larger than 0.01.", "\n", "\n")
                }

                # RMSEA ---------------------------------------------------------------
                if (diff_rmsea_scaled1 <= 0.015) {
                    cat("The hypothesis of approximate loading invariance *is not* rejected
           according to the RMSEA because the difference in RMSEA value is smaller
           than or equal to 0.015.", "\n", "\n")
                } else {
                    cat("The hypothesis of approximate approximate loading invariance *is*
           rejected according to the RMSEA because the difference in RMSEA value is
           larger than 0.015.", "\n", "\n")
                }

                # SRMR ----------------------------------------------------------------
                if (diff_srmr_scaled1 <= .030) {
                    cat("The hypothesis of approximate loading invariance *is not* rejected
           according to the SRMR because the difference in SRMR value is smaller than
           or equal to 0.030.", "\n", "\n")
                } else {
                    cat("The hypothesis of approximate approximate loading invariance *is*
           rejected according to the SRMR because the difference in SRMR value is
           larger than 0.030.", "\n", "\n")
                }

                t1 <- eval(parse(text = c(paste0(
                    "dim(inspect(cfa, \"est\")$`",
                    as.numeric(levels(pull(data, group))[1]), "`$lambda)"
                ))))

                loadings <- array(NA, dim = c(t1[1], t1[2], length(levels(pull(data, group)))))
                group_upd <- as.numeric(pull(data, group))

                for (i in min(group_upd):max(group_upd)) {
                    eval(parse(text = c(
                        paste0(
                            "loadings[,,", i, "] <-", "inspect(cfa, \"est\")$`",
                            as.numeric(levels(pull(data, group))[i]), "`$lambda"
                        )
                    )))
                }

                rowlab <- eval(parse(text = c(
                    paste0(
                        "dimnames(inspect(cfa, \"est\")$`",
                        as.numeric(levels(pull(data, group))[1]), "`$lambda)[[1]]"
                    )
                )))

                collab <- eval(parse(text = c(
                    paste0(
                        "dimnames(inspect(cfa, \"est\")$`",
                        as.numeric(levels(pull(data, group))[1]), "`$lambda)[[2]]"
                    )
                )))

                dimnames(loadings) <- list(rowlab, collab, levels(pull(data, group)))

                combis <- as.matrix(combn(unique(group_upd), 2))

                loading_diff <- array(NA, dim = c(t1[1], t1[2], ncol(combis)))
                label_diff <- rep(NA, ncol(combis))

                for (j in 1:ncol(combis)) {
                    loading_diff[, , j] <- loadings[, , combis[1, j]] - loadings[, , combis[2, j]]
                    label_diff[j] <- paste0(
                        levels(pull(data, group))[combis[1, j]], "-",
                        levels(pull(data, group))[combis[2, j]]
                    )
                }

                dimnames(loading_diff) <- list(rowlab, collab, label_diff)
                load_diff_abs <- abs(loading_diff)
                maxind <- which(load_diff_abs == max((load_diff_abs)), arr.ind = TRUE)

                if (display == TRUE) {
                    cat("Factor loadings for each group ---------------------------------------------------------", "\n", "\n")
                    print(loadings)
                    cat("Difference in loadings between groups --------------------------------------------------", "\n", "\n")
                    print(loading_diff)

                    print(paste(
                        "The highest loading is that of variable", dimnames(loadings)[[1]][maxind[1]],
                        "on factor", dimnames(loadings)[[2]][maxind[2]], "in group",
                        dimnames(loadings)[[3]][maxind[3]]
                    ))
                }
                cat("\n")
                cat("Intercept Invariance Interpretation ------------------------------------------------------", "\n", "\n")

                print("Before testing intercept invariance, (partial) loading invariance should hold, which is not the case.")
            }
        }
    }
    if (estimator == "ML") {
        if (intercept == FALSE) {
            if (missing == FALSE) {
                cfa <- cfa(model, data,
                    std.lv = TRUE, meanstructure = TRUE,
                    group = group, estimator = "ML"
                )

                loading_invariance <- cfa(model, data,
                    std.lv = TRUE, meanstructure = TRUE,
                    group = group,
                    group.equal = c("loadings"), estimator = "ML"
                )
            } else {
                cfa <- cfa(model, data,
                    std.lv = TRUE, missing = "fiml",
                    group = group, estimator = "ML"
                )

                loading_invariance <- cfa(model, data,
                    std.lv = TRUE,
                    missing = "fiml", group = group,
                    group.equal = c("loadings"), estimator = "ML"
                )
            }

            load_check <- semTools::compareFit(
                Loadings_Invariant = loading_invariance,
                Loadings_Free = cfa
            )

            summary.FitDiff(load_check)

            pvalue_chisq_diff <- as.numeric(
                anova(loading_invariance, cfa)$"Pr(>Chisq)"[2]
            )

            diff_cfi <- fitMeasures(loading_invariance)[["cfi"]] -
                fitMeasures(cfa)[["cfi"]]
            diff_rmsea <- fitMeasures(loading_invariance)[["rmsea"]] -
                fitMeasures(cfa)[["rmsea"]]
            diff_srmr <- fitMeasures(loading_invariance)[["srmr"]] -
                fitMeasures(cfa)[["srmr"]]

            # Chi-Square ------------------------------------------------------------
            cat("Loading Invariance Interpretations---------------------------------------------------------", "\n", "\n")

            if (pvalue_chisq_diff >= 0.05) {
                cat("The hypothesis of perfect loading invariance *is not* rejected
         according to the Chi-Square difference test statistics because the p-value
         is larger or equal to 0.05.", "\n", "\n")
            } else {
                cat("The hypothesis of perfect loading invariance *is* rejected according
         to the Chi-Square difference test statistics because the p-value is
         smaller than 0.05", "\n", "\n")
            }

            # CFI  ------------------------------------------------------------------
            if (diff_cfi <= .01) {
                cat("The hypothesis of approximate loading invariance *is not* rejected
         according to the CFI because the difference in CFI value is smaller than
         or equal to 0.01.", "\n", "\n")
            } else {
                cat("The hypothesis of approximate approximate loading invariance *is*
         rejected according to the CFI because the difference in CFI value is
         larger than 0.01.", "\n", "\n")
            }

            # RMSEA -----------------------------------------------------------------
            if (diff_rmsea <= 0.015) {
                cat("The hypothesis of approximate loading invariance *is not* rejected
         according to the RMSEA because the difference in RMSEA value is smaller
         than or equal to 0.015.", "\n", "\n")
            } else {
                cat("The hypothesis of approximate approximate loading invariance *is*
         rejected according to the RMSEA because the difference in RMSEA value is
         larger than 0.015.", "\n", "\n")
            }

            # SRMR ------------------------------------------------------------------
            if (diff_srmr < .030) {
                cat("The hypothesis of approximate loading invariance *is not* rejected
         according to the SRMR because the difference in SRMR value is smaller than
         or equal to 0.030.", "\n", "\n")
            } else {
                cat("The hypothesis of approximate approximate loading invariance *is*
         rejected according to the SRMR because the difference in SRMR value is
         larger than 0.030.", "\n", "\n")
            }

            t1 <- eval(parse(text = c(paste0(
                "dim(inspect(cfa, \"est\")$`",
                as.numeric(levels(pull(data, group))[1]), "`$lambda)"
            ))))

            loadings <- array(NA, dim = c(t1[1], t1[2], length(levels(pull(data, group)))))
            group_upd <- as.numeric(pull(data, group))

            for (i in min(group_upd):max(group_upd)) {
                eval(parse(text = c(
                    paste0(
                        "loadings[,,", i, "] <-", "inspect(cfa, \"est\")$`",
                        as.numeric(levels(pull(data, group))[i]), "`$lambda"
                    )
                )))
            }

            rowlab <- eval(parse(text = c(
                paste0(
                    "dimnames(inspect(cfa, \"est\")$`",
                    as.numeric(levels(pull(data, group))[1]), "`$lambda)[[1]]"
                )
            )))

            collab <- eval(parse(text = c(
                paste0(
                    "dimnames(inspect(cfa, \"est\")$`",
                    as.numeric(levels(pull(data, group))[1]), "`$lambda)[[2]]"
                )
            )))

            dimnames(loadings) <- list(rowlab, collab, levels(pull(data, group)))

            combis <- as.matrix(combn(unique(group_upd), 2))

            loading_diff <- array(NA, dim = c(t1[1], t1[2], ncol(combis)))
            label_diff <- rep(NA, ncol(combis))

            for (j in 1:ncol(combis)) {
                loading_diff[, , j] <- loadings[, , combis[1, j]] - loadings[, , combis[2, j]]
                label_diff[j] <- paste0(
                    levels(pull(data, group))[combis[1, j]], "-",
                    levels(pull(data, group))[combis[2, j]]
                )
            }

            dimnames(loading_diff) <- list(rowlab, collab, label_diff)
            load_diff_abs <- abs(loading_diff)
            maxind <- which(load_diff_abs == max((load_diff_abs)), arr.ind = TRUE)

            if (display == TRUE) {
                cat("Factor loadings for each group ---------------------------------------------------------", "\n", "\n")
                print(loadings)
                cat("Difference in loadings between groups --------------------------------------------------", "\n", "\n")
                print(loading_diff)

                print(paste(
                    "The largest difference in loading is that of variable",
                    dimnames(loading_diff)[[1]][maxind[1]], "on factor",
                    dimnames(loading_diff)[[2]][maxind[2]], "for group comparison",
                    dimnames(loading_diff)[[3]][maxind[3]]
                ))
            }
        } else {
            if (missing == FALSE) {
                cfa <- cfa(model, data,
                    std.lv = TRUE, meanstructure = TRUE,
                    group = group, estimator = "ML"
                )

                loading_invariance <- cfa(model, data,
                    std.lv = TRUE, meanstructure = TRUE,
                    group = group,
                    group.equal = c("loadings"), estimator = "ML"
                )

                intercept_invariance <- cfa(model, data,
                    std.lv = TRUE, meanstructure = TRUE,
                    group = group,
                    group.equal = c("loadings", "intercepts"),
                    estimator = "ML"
                )
            } else {
                cfa <- cfa(model, data,
                    std.lv = TRUE, missing = "fiml",
                    group = group, estimator = "ML"
                )

                loading_invariance <- cfa(model, data,
                    std.lv = TRUE,
                    missing = "fiml", group = group,
                    group.equal = c("loadings"), estimator = "ML"
                )

                intercept_invariance <- cfa(model, data,
                    std.lv = TRUE,
                    missing = "fiml", group = group,
                    group.equal = c("loadings", "intercepts"),
                    estimator = "ML"
                )
            }


            pvalue_chisq_diff1 <- as.numeric(
                anova(intercept_invariance, loading_invariance, cfa)$"Pr(>Chisq)"[2]
            )

            diff_cfi1 <- fitMeasures(loading_invariance)[["cfi"]] -
                fitMeasures(cfa)[["cfi"]]
            diff_rmsea1 <- fitMeasures(loading_invariance)[["rmsea"]] -
                fitMeasures(cfa)[["rmsea"]]
            diff_srmr1 <- fitMeasures(loading_invariance)[["srmr"]] -
                fitMeasures(cfa)[["srmr"]]

            SC2 <- sum(
                as.numeric(pvalue_chisq_diff1 > .05),
                as.numeric(diff_cfi_scaled1 <= .01),
                as.numeric(diff_rmsea_scaled1 <= 0.015),
                as.numeric(diff_srmr_scaled1 <= 0.030)
            )

            if (SC2 >= 3) {
                int_check <- semTools::compareFit(
                    Intercept_Invariant =
                        intercept_invariance, Loadings_Invariant = loading_invariance,
                    Loadings_Intercept_Free = cfa
                )

                summary.FitDiff(int_check)

                pvalue_chisq_diff2 <- as.numeric(
                    anova(intercept_invariance, loading_invariance, cfa)$"Pr(>Chisq)"[3]
                )

                diff_cfi2 <- fitMeasures(intercept_invariance)[["cfi"]] -
                    fitMeasures(loading_invariance)[["cfi"]]
                diff_rmsea2 <- fitMeasures(intercept_invariance)[["rmsea"]] -
                    fitMeasures(loading_invariance)[["rmsea"]]
                diff_srmr2 <- fitMeasures(intercept_invariance)[["srmr"]] -
                    fitMeasures(loading_invariance)[["srmr"]]

                # Chi-Square ----------------------------------------------------------
                cat("Interpretations-------------------------------------------------------
         --------", "\n", "\n")

                cat("Loading Invariance----------------------------------------------------
         --------", "\n", "\n")

                if (pvalue_chisq_diff1 >= 0.05) {
                    cat("The hypothesis of perfect loading invariance *is not* rejected
           according to the Chi-Square difference test statistics because the p-value
           is larger or equal to 0.05.", "\n", "\n")
                } else {
                    cat("The hypothesis of perfect loading invariance *is* rejected according
           to the Chi-Square difference test statistics because the p-value is
           smaller than 0.05", "\n", "\n")
                }

                # CFI  ----------------------------------------------------------------
                if (diff_cfi1 <= .01) {
                    cat("The hypothesis of approximate loading invariance *is not* rejected
           according to the CFI because the difference in CFI value is smaller than
           or equal to 0.01.", "\n", "\n")
                } else {
                    cat("The hypothesis of approximate approximate loading invariance *is*
           rejected according to the CFI because the difference in CFI value is
           larger than 0.01.", "\n", "\n")
                }

                # RMSEA ---------------------------------------------------------------
                if (diff_rmsea1 <= 0.015) {
                    cat("The hypothesis of approximate loading invariance *is not* rejected
           according to the RMSEA because the difference in RMSEA value is smaller
           than or equal to 0.015.", "\n", "\n")
                } else {
                    cat("The hypothesis of approximate approximate loading invariance *is*
           rejected according to the RMSEA because the difference in RMSEA value is
           larger than 0.015.", "\n", "\n")
                }

                # SRMR ----------------------------------------------------------------
                if (diff_srmr1 <= .030) {
                    cat("The hypothesis of approximate loading invariance *is not* rejected
           according to the SRMR because the difference in SRMR value is smaller than
           or equal to 0.030.", "\n", "\n")
                } else {
                    cat("The hypothesis of approximate approximate loading invariance *is*
           rejected according to the SRMR because the difference in SRMR value is
           larger than 0.030.", "\n", "\n")
                }

                cat("Intercept Invariance----------------------------------------------------
         --------", "\n", "\n")

                if (pvalue_chisq_diff2 >= 0.05) {
                    cat("The hypothesis of perfect intercept invariance *is not* rejected
           according to the Chi-Square difference test statistics because the p-value
           is larger or equal to 0.05.", "\n", "\n")
                } else {
                    cat("The hypothesis of perfect intercept invariance *is* rejected
           according to the Chi-Square difference test statistics because the p-value
           is smaller than 0.05", "\n", "\n")
                }

                if (diff_cfi2 <= .01) {
                    cat("The hypothesis of approximate intercept invariance *is not*
           rejected according to the CFI because the difference in CFI value is
           smaller than or equal to 0.01.", "\n", "\n")
                } else {
                    cat("The hypothesis of approximate approximate intercept invariance *is*
           rejected according to the CFI because the difference in CFI value is
           larger than 0.01.", "\n", "\n")
                }

                if (diff_rmsea2 <= 0.015) {
                    cat("The hypothesis of approximate intercept invariance *is not* rejected
           according to the RMSEA because the difference in RMSEA value is smaller
           than or equal to 0.015.", "\n", "\n")
                } else {
                    cat("The hypothesis of approximate approximate intercept invariance *is*
           rejected according to the RMSEA because the difference in RMSEA value is
           larger than 0.015.", "\n", "\n")
                }

                if (diff_srmr2 <= .030) {
                    cat("The hypothesis of approximate intercept invariance *is not* rejected
           according to the SRMR because the difference in SRMR value is smaller than
           or equal to 0.030.", "\n", "\n")
                } else {
                    cat("The hypothesis of approximate approximate intercept invariance *is*
           rejected according to the SRMR because the difference in SRMR value is
           larger than 0.030.", "\n", "\n")
                }

                t1 <- eval(parse(text = c(paste0(
                    "dim(inspect(cfa, \"est\")$`",
                    as.numeric(levels(pull(data, group))[1]), "`$lambda)"
                ))))

                loadings <- array(NA, dim = c(t1[1], t1[2], length(levels(pull(data, group)))))
                group_upd <- as.numeric(pull(data, group))

                for (i in min(group_upd):max(group_upd)) {
                    eval(parse(text = c(
                        paste0(
                            "loadings[,,", i, "] <-", "inspect(cfa, \"est\")$`",
                            as.numeric(levels(pull(data, group))[i]), "`$lambda"
                        )
                    )))
                }

                rowlab <- eval(parse(text = c(
                    paste0(
                        "dimnames(inspect(cfa, \"est\")$`",
                        as.numeric(levels(pull(data, group))[1]), "`$lambda)[[1]]"
                    )
                )))

                collab <- eval(parse(text = c(
                    paste0(
                        "dimnames(inspect(cfa, \"est\")$`",
                        as.numeric(levels(pull(data, group))[1]), "`$lambda)[[2]]"
                    )
                )))

                dimnames(loadings) <- list(rowlab, collab, levels(pull(data, group)))

                combis <- as.matrix(combn(unique(group_upd), 2))

                loading_diff <- array(NA, dim = c(t1[1], t1[2], ncol(combis)))
                label_diff <- rep(NA, ncol(combis))

                for (j in 1:ncol(combis)) {
                    loading_diff[, , j] <- loadings[, , combis[1, j]] - loadings[, , combis[2, j]]
                    label_diff[j] <- paste0(
                        levels(pull(data, group))[combis[1, j]], "-",
                        levels(pull(data, group))[combis[2, j]]
                    )
                }

                dimnames(loading_diff) <- list(rowlab, collab, label_diff)

                t2 <- eval(parse(text = c(paste0(
                    "dim(inspect(cfa, \"est\")$`",
                    as.numeric(levels(pull(data, group))[1]), "`$nu)"
                ))))

                intercepts <- array(NA, dim = (c(t2[1], length(levels(pull(data, group))))))
                group_upd <- as.numeric(pull(data, group))

                for (i in min(group_upd):max(group_upd)) {
                    eval(parse(text = c(
                        paste0(
                            "intercepts[,", i, "] <-", "inspect(cfa, \"est\")$`",
                            as.numeric(levels(pull(data, group))[i]), "`$nu"
                        )
                    )))
                }

                rowlab2 <- eval(parse(text = c(
                    paste0(
                        "dimnames(inspect(cfa, \"est\")$`",
                        as.numeric(levels(pull(data, group))[1]), "`$nu)[[1]]"
                    )
                )))

                dimnames(intercepts) <- list(rowlab2, levels(pull(data, group)))

                combis <- as.matrix(combn(unique(group_upd), 2))

                intercept_diff <- array(NA, dim = c(t2[1], ncol(combis)))
                label_diff <- rep(NA, ncol(combis))

                for (j in 1:ncol(combis)) {
                    intercept_diff[, j] <- intercepts[, combis[1, j]] - intercepts[, combis[2, j]]
                    label_diff[j] <- paste0(
                        levels(pull(data, group))[combis[1, j]], "-",
                        levels(pull(data, group))[combis[2, j]]
                    )
                }

                dimnames(intercept_diff) <- list(rowlab2, label_diff)

                load_diff_abs <- abs(loading_diff)
                int_diff_abs <- abs(intercept_diff)
                maxind <- which(load_diff_abs == max((load_diff_abs)), arr.ind = TRUE)
                maxind2 <- which(int_diff_abs == max((int_diff_abs)), arr.ind = TRUE)

                if (display == TRUE) {
                    cat("Factor loadings for each group ---------------------------------------------------------", "\n", "\n")
                    print(loadings)
                    cat("\n", "\n", "Difference in loadings between groups --------------------------------------", "\n", "\n")
                    print(loading_diff)

                    print(paste(
                        "The largest difference in loading is that of variable",
                        dimnames(loading_diff)[[1]][maxind[1]],
                        "on factor", dimnames(loading_diff)[[2]][maxind[2]], "for group comparison",
                        dimnames(loading_diff)[[3]][maxind[3]]
                    ))
                    cat("\n")
                    cat("Intercepts for each group --------------------------------------------------------------", "\n", "\n")
                    print(intercepts)
                    cat("\n")
                    cat("Difference in intercepts between groups ------------------------------------------------", "\n", "\n")
                    print(intercept_diff)
                    cat("\n")
                    print(paste(
                        "The largest difference in intercept is that of variable",
                        dimnames(intercept_diff)[[1]][maxind2[1]],
                        "for group comparison", dimnames(intercept_diff)[[2]][maxind2[2]]
                    ))
                }
            } else {
                load_check <- semTools::compareFit(
                    Loadings_Invariant = loading_invariance,
                    Loadings_Free = cfa
                )

                summary.FitDiff(load_check)

                cat("Loading Invariance Interpretation ------------------------------------------------------", "\n", "\n")

                if (pvalue_chisq_diff1 >= 0.05) {
                    cat("The hypothesis of perfect loading invariance *is not* rejected
             according to the Chi-Square difference test statistics because the p-value
             is larger or equal to 0.05.", "\n", "\n")
                } else {
                    cat("The hypothesis of perfect loading invariance *is* rejected according
             to the Chi-Square difference test statistics because the p-value is
             smaller than 0.05", "\n", "\n")
                }

                # CFI  --------------------------------------------------------------
                if (diff_cfi1 <= .01) {
                    cat("The hypothesis of approximate loading invariance *is not* rejected
             according to the CFI because the difference in CFI value is smaller than
             or equal to 0.01.", "\n", "\n")
                } else {
                    cat("The hypothesis of approximate approximate loading invariance *is*
             rejected according to the CFI because the difference in CFI value is
             larger than 0.01.", "\n", "\n")
                }

                # RMSEA -------------------------------------------------------------
                if (diff_rmsea1 <= 0.015) {
                    cat("The hypothesis of approximate loading invariance *is not* rejected
             according to the RMSEA because the difference in RMSEA value is smaller
             than or equal to 0.015.", "\n", "\n")
                } else {
                    cat("The hypothesis of approximate approximate loading invariance *is*
             rejected according to the RMSEA because the difference in RMSEA value is
             larger than 0.015.", "\n", "\n")
                }

                # SRMR --------------------------------------------------------------
                if (diff_srmr1 <= .030) {
                    cat("The hypothesis of approximate loading invariance *is not* rejected
             according to the SRMR because the difference in SRMR value is smaller than
             or equal to 0.030.", "\n", "\n")
                } else {
                    cat("The hypothesis of approximate approximate loading invariance *is*
             rejected according to the SRMR because the difference in SRMR value is
             larger than 0.030.", "\n", "\n")
                }

                t1 <- eval(parse(text = c(
                    paste0(
                        "dim(inspect(cfa, \"est\")$`",
                        as.numeric(levels(data$group)[1]), "`$lambda)"
                    )
                )))

                loadings <- array(NA, dim = c(t1[1], t1[2], length(levels(pull(data, group)))))
                group_upd <- as.numeric(pull(data, group))

                for (i in min(group_upd):max(group_upd)) {
                    eval(parse(text = c(
                        paste0(
                            "loadings[,,", i, "] <-", "inspect(cfa, \"est\")$`",
                            as.numeric(levels(pull(data, group))[i]), "`$lambda"
                        )
                    )))
                }

                rowlab <- eval(parse(text = c(
                    paste0(
                        "dimnames(inspect(cfa, \"est\")$`",
                        as.numeric(levels(pull(data, group))[1]), "`$lambda)[[1]]"
                    )
                )))

                collab <- eval(parse(text = c(
                    paste0(
                        "dimnames(inspect(cfa, \"est\")$`",
                        as.numeric(levels(pull(data, group))[1]), "`$lambda)[[2]]"
                    )
                )))

                dimnames(loadings) <- list(rowlab, collab, levels(pull(data, group)))

                combis <- as.matrix(combn(unique(group_upd), 2))

                loading_diff <- array(NA, dim = c(t1[1], t1[2], ncol(combis)))
                label_diff <- rep(NA, ncol(combis))

                for (j in 1:ncol(combis)) {
                    loading_diff[, , j] <- loadings[, , combis[1, j]] - loadings[, , combis[2, j]]
                    label_diff[j] <- paste0(
                        levels(pull(data, group))[combis[1, j]], "-",
                        levels(pull(data, group))[combis[2, j]]
                    )
                }

                dimnames(loading_diff) <- list(rowlab, collab, label_diff)

                load_diff_abs <- abs(loading_diff)
                maxind <- which(load_diff_abs == max((load_diff_abs)), arr.ind = TRUE)

                if (display == TRUE) {
                    cat("Factor loadings for each group ---------------------------------------------------------", "\n", "\n")
                    print(loadings)
                    cat("Difference in loadings between groups --------------------------------------------------", "\n", "\n")
                    print(loading_diff)

                    print(paste(
                        "The largest difference in loading is that of variable",
                        dimnames(loading_diff)[[1]][maxind[1]], "on factor",
                        dimnames(loading_diff)[[2]][maxind[2]], "for group comparison",
                        dimnames(loading_diff)[[3]][maxind[3]]
                    ))
                }

                cat("Intercept Invariance Interpretation --------------------------------------------------------", "\n", "\n")

                print("Before testing intercept invariance, (partial) loading invariance should hold, which is not the case.")
            }
        }
    }
}

localFit <- function(x) {
    local_misfit <- abs(inspect(x, "resid")$cov)
    max_local_misfit <- which(local_misfit == max(local_misfit), arr.ind = TRUE)[1, ]

    return(list(
        local_misfit = local_misfit,
        max_misfit = local_misfit[as.numeric(max_local_misfit[1]), as.numeric(max_local_misfit[2])]
    ))
}

sem_structural_results <- function(model, nd = 3) {
    indic <- which(inspect(model, what = "std")$beta != 0,
        arr.ind = TRUE, useNames = TRUE
    )

    result <- as.data.frame(cbind(
        colnames(inspect(model, what = "std")$beta)[indic[, 2]],
        colnames(inspect(model, what = "std")$beta)[indic[, 1]],
        round(inspect(model, what = "std")$beta[indic], digits = 3),
        round(inspect(model, what = "se")$beta[indic], digits = 3),
        round(pnorm(abs(inspect(model, what = "coef")$beta /
            inspect(model, what = "se")$beta), lower.tail = FALSE)[indic], digits = 3)
    ))
    colnames(result) <- c("outcome", "predictor", "std estimate", "se", "p-value")

    return(result)
}

summary.FitDiff <- function(object, fit.measures = "default", nd = 3, tag = "\u2020") {
    if (nrow(object@nested) > 0L) {
        cat(
            "Nested Model Comparison-------------------------------------------------------------------",
            "\n", "\n"
        )
        test.statistics <- object@nested
        if (object@model.class == "lavaan") {
            print(test.statistics, nd = nd)
        } else {
            class(test.statistics) <- c("lavaan.data.frame", "data.frame")
            stats::printCoefmat(test.statistics, P.values = TRUE, has.Pvalue = TRUE)
        }
        cat("\n")
    }


    noFit <- ncol(object@fit) == 1L && names(object@fit)[1] == "df"
    if (!noFit) {
        if (is.null(fit.measures)) fit.measures <- colnames(object@fit)
        if ("all" %in% fit.measures) fit.measures <- colnames(object@fit)
        if (length(fit.measures) == 1 && fit.measures == "default") {
            ## robust or scaled test statistics?
            if (is.null(object@fit$cfi.scaled)) {
                fit.measures <- c("chisq", "df", "pvalue", "rmsea", "cfi", "srmr")
            } else if (all(!is.na(object@fit$cfi.robust)) && !is.null(object@fit$cfi.robust)) {
                fit.measures <- c(
                    "chisq.scaled", "df.scaled", "pvalue.scaled",
                    "rmsea.robust", "cfi.robust", "srmr"
                )
            } else {
                fit.measures <- c(
                    "chisq.scaled", "df.scaled", "pvalue.scaled",
                    "rmsea.scaled", "cfi.scaled", "srmr"
                )
            }

            if ("aic" %in% colnames(object@fit)) {
                # fit.measures <- c(fit.measures, "aic", "bic")
                fit.measures <- c(fit.measures)
            }
        }


        cat(
            "Model Fit Indices ------------------------------------------------------------------------",
            "\n", "\n"
        )
        ## this is the object to return (numeric, no printed daggers)
        fit.indices <- object@fit[, fit.measures, drop = FALSE]

        ## print with daggers marking each fit index's preferred model
        ## (turns "numeric" vectors into "character")
        badness <- grepl(
            pattern = c("chisq|rmsea|ic|rmr|ecvi|fmin|hqc"),
            x = colnames(fit.indices)
        )
        goodness <- grepl(
            pattern = c("cfi|tli|rfi|nfi|ifi|rni|cn|gfi|mfi|Hat"),
            x = colnames(fit.indices)
        )
        minvalue <- badness & !goodness
        minvalue[!badness & !goodness] <- NA
        fit.integer <- grepl(
            pattern = c("df|npar|ntotal"),
            x = colnames(fit.indices)
        )
        suppressWarnings(fitTab <- as.data.frame(
            mapply(tagCharacter,
                nd = nd,
                char = tag,
                vec = fit.indices,
                minvalue = minvalue,
                print_integer = fit.integer
            ),
            stringsAsFactors = FALSE
        ))
        rownames(fitTab) <- object@name
        colnames(fitTab) <- colnames(fit.indices)
        class(fitTab) <- c("lavaan.data.frame", "data.frame")
        print(fitTab, nd = nd)
        cat("\n")


        if (nrow(object@nested) > 0L) {
            fit.diff.measures <- fit.measures[!grepl(
                pattern = "chisq|pvalue|ntotal",
                x = fit.measures
            )]
            cat(
                "Differences in Fit Indices ---------------------------------------------------------------",
                "\n", "\n"
            )
            fit.diff <- object@fit.diff[, fit.diff.measures, drop = FALSE]
            class(fit.diff) <- c("lavaan.data.frame", "data.frame")
            print(fit.diff, nd = nd)
            cat("\n")
        }
    }


    invisible(object)
}
