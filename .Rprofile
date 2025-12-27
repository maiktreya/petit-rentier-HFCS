if (interactive() && Sys.getenv("TERM_PROGRAM") == "vscode") {
    if ("httpgd" %in% .packages(all.available = TRUE)) {
        options(vsc.rstudioapi = TRUE)
        options(vsc.use_httpgd = TRUE)
        options(vsc.plot = TRUE)
        options(device = function(...) {
            httpgd::hgd(silent = FALSE)
        })
    }
}
options(radian.auto_match = TRUE)
options(radian.auto_indentation = TRUE)
options(radian.complete_while_typing = TRUE)
# .Rprofile

# Only run this if we are in the Language Server or Interactive session
if (interactive() || nzchar(Sys.getenv("R_TEST_LOAD_SOURCE"))) {
    # Option A: Disable line length check entirely (Recommended if you hate it)
    # This tells lintr: "Use defaults, but kill the line length linter"
    options(lintr.linters = lintr::linters_with_defaults(
        line_length_linter = NULL
    ))

    # Option B: If you want to keep it but set it to 120 chars:
    # options(lintr.linters = lintr::linters_with_defaults(
    #   line_length_linter = lintr::line_length_linter(120)
    # ))
}
