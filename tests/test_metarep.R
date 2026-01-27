# Test script to verify metarep functionality
library(meta)
library(metarep)

tryCatch(
    {
        # Create a dummy meta-analysis object
        data(Fleiss1993bin)
        m1 <- metabin(d.asp, n.asp, d.plac, n.plac, data = Fleiss1993bin, sm = "OR")

        print("Meta-analysis object created.")

        # Run metarep
        print("Running metarep...")
        u_val <- 2
        res <- metarep(m1, u = u_val, t = 0.05, report.u.max = TRUE)

        print("Metarep result:")
        print(res)

        print("R-value:")
        print(res$r.value)

        print("Attempting forest plot...")
        png("test_forest.png")
        forest(res)
        dev.off()
        print("Forest plot created successfully.")
    },
    error = function(e) {
        print(paste("Error:", e$message))
    }
)
