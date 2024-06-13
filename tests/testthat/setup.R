mod_1cmt_iv <- PKPDsim::new_ode_model("pk_1cmt_iv_auc")
suppressMessages({ ## avoid message "the following objects are masked from ..."
  if (.Platform$OS.type == "windows") {
    .libPaths <- c(Sys.getenv("R_PACKAGE_DIR"), .libPaths())
  }
  if (!require("pkbusulfanmccune", character.only = TRUE)) {
    PKPDsim::install_default_literature_model("pk_busulfan_mccune")
    library(pkbusulfanmccune)
  }
})
