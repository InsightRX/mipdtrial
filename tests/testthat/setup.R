mod_1cmt_iv <- PKPDsim::new_ode_model("pk_1cmt_iv_auc")
suppressMessages({ ## avoid message "the following objects are masked from ..."
  if (.Platform$OS.type == "windows") {
    .libPaths(c("D:/a/mipdtrial/mipdtrial/check/mipdtrial.Rcheck", .libPaths()))
  }
  if (!requireNamespace("pkbusulfanmccune", quietly = TRUE)) {
    PKPDsim::install_default_literature_model("pk_busulfan_mccune")
    loadNamespace("pkbusulfanmccune")
  }
})

