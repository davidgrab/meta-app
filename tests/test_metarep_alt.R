# Test script to verify metarep common.effect argument with alternative
library(meta)
library(metarep)

tryCatch(
    {
        # Create a dummy meta-analysis object
        data(Fleiss1993bin)
        m1 <- metabin(d.asp, n.asp, d.plac, n.plac, data = Fleiss1993bin, sm = "OR")

        print("Testing metarep with common.effect parameter and alternative='two.sided'...")

        # Run metarep with common.effect = TRUE and explicit alternative
        # My code currently uses alternative = "two.sided"
        res_common <- metarep(m1, u = 2, t = 0.05, report.u.max = TRUE, common.effect = TRUE, alternative = "two.sided")
        print("Success with common.effect = TRUE and alternative = 'two.sided'")
        print(res_common$r.value)
    },
    error = function(e) {
        print(paste("Error with two.sided:", e$message))
    }
)

tryCatch(
    {
        print("Testing metarep with common.effect = TRUE and alternative = 'less'...")
        res_less <- metarep(m1, u = 2, t = 0.05, report.u.max = TRUE, common.effect = TRUE, alternative = "less")
        print("Success with alternative = 'less'")
        print(res_less$r.value)
    },
    error = function(e) {
        print(paste("Error with less:", e$message))
    }
)
