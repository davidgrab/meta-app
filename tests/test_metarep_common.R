# Test script to verify metarep common.effect argument
library(meta)
library(metarep)

tryCatch(
    {
        # Create a dummy meta-analysis object
        data(Fleiss1993bin)
        m1 <- metabin(d.asp, n.asp, d.plac, n.plac, data = Fleiss1993bin, sm = "OR")

        print("Testing metarep with common.effect parameter...")

        # Run metarep with common.effect = TRUE
        res_common <- metarep(m1, u = 2, t = 0.05, report.u.max = TRUE, common.effect = TRUE)
        print("Success with common.effect = TRUE")
        print(res_common$r.value)

        # Run metarep with common.effect = FALSE
        res_random <- metarep(m1, u = 2, t = 0.05, report.u.max = TRUE, common.effect = FALSE)
        print("Success with common.effect = FALSE")
        print(res_random$r.value)
    },
    error = function(e) {
        print(paste("Error:", e$message))
    }
)
