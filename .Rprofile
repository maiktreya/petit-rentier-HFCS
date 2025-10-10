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
