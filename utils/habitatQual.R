habitatQual <- function (inputTable, agentsTable, d2Table) {
  tab.perc <- Reduce (function (...) merge (..., all = TRUE, by = "individual_id"),
                      list (inputTable [, .(den_perc = ((sum (denning, na.rm = T)) / .N) * 100), by = individual_id ],
                            inputTable [, .(rust_perc = ((sum (rust, na.rm = T)) / .N) * 100), by = individual_id ],
                            inputTable [, .(cav_perc = ((sum (cavity, na.rm = T)) / .N) * 100), by = individual_id ],
                            inputTable [, .(move_perc = ((sum (movement, na.rm = T)) / .N) * 100), by = individual_id ],
                            inputTable [, .(cwd_perc = ((sum (cwd, na.rm = T)) / .N) * 100), by = individual_id ],
                            inputTable [, .(open_perc = ((sum (open, na.rm =T)) / .N) * 100), by = individual_id])
  )
  tab.perc <- merge (tab.perc,
                     agentsTable [, c ("individual_id", "fisher_pop")],
                     by = "individual_id", all.x = T)
  # log transform the data
  tab.perc [fisher_pop == 2 & den_perc >= 0, den_perc := log (den_perc + 1)][fisher_pop == 2 & cav_perc >= 0, cavity := log (cav_perc + 1)] # sbs-wet
  tab.perc [fisher_pop == 3 & den_perc >= 0, den_perc := log (den_perc + 1)]# sbs-dry
  tab.perc [fisher_pop == 4 & rust_perc >= 0, rust_perc := log (rust_perc + 1)] # dry

  # truncate at the center
  # 1 = boreal
  # 2 = sbs-wet
  # 3 = sbs-dry
  # 4 = dry

  tab.perc [fisher_pop == 1 & den_perc > 24, den_perc := 24][fisher_pop == 1 & rust_perc > 2.2, rust_perc := 2.2][fisher_pop == 1 & cwd_perc > 17.4, cwd_perc := 17.4][fisher_pop == 1 & move_perc > 56.2, move_perc := 56.2][fisher_pop == 1 & open_perc < 31.2, open_perc := 31.2]
  tab.perc [fisher_pop == 2 & den_perc > 1.57, den_perc := 1.57][fisher_pop == 2 & rust_perc > 36.2, rust_perc := 36.2][fisher_pop == 2 & cav_perc > 0.685, cav_perc := 0.685][fisher_pop == 2 & cwd_perc > 30.38, cwd_perc := 30.38][fisher_pop == 2 & move_perc > 61.5, move_perc := 61.5][fisher_pop == 2 & open_perc < 32.7, open_perc := 32.7]
  tab.perc [fisher_pop == 3 & den_perc > 1.16, den_perc := 1.16][fisher_pop == 3 & rust_perc > 19.1, rust_perc := 19.1][fisher_pop == 3 & cav_perc > 0.45, cav_perc := 0.45][fisher_pop == 3 & cwd_perc > 12.7, cwd_perc := 12.7][fisher_pop == 3 & move_perc > 51.3, move_perc := 51.3][fisher_pop == 3 & open_perc < 37.3, open_perc := 37.3]
  tab.perc [fisher_pop == 4 & den_perc > 2.3, den_perc := 2.3][fisher_pop == 4 & rust_perc > 1.6, rust_perc := 1.6][fisher_pop == 4 & cwd_perc > 10.8, cwd_perc := 10.8][fisher_pop == 4 & move_perc > 58.1, move_perc := 58.1][fisher_pop == 4 & open_perc < 15.58, open_perc := 15.58]

  tab.perc [fisher_pop == 1, d2 := mahalanobis (tab.perc [fisher_pop == 1, c ("den_perc", "rust_perc", "cwd_perc", "move_perc", "open_perc")], c(23.98, 2.24, 17.4, 56.2, 31.2), cov = d2Table[[1]])]
  tab.perc [fisher_pop == 2, d2 := mahalanobis (tab.perc [fisher_pop == 2, c ("den_perc", "rust_perc", "cav_perc", "cwd_perc", "move_perc", "open_perc")], c(1.57, 36.2, 0.68, 30.38, 61.5, 32.72), cov = d2Table[[2]])]
  tab.perc [fisher_pop == 3, d2 := mahalanobis (tab.perc [fisher_pop == 3, c ("den_perc", "rust_perc", "cav_perc", "cwd_perc", "move_perc", "open_perc")], c(1.16, 19.1, 0.4549, 12.76, 51.25, 37.27), cov = d2Table[[3]])]
  tab.perc [fisher_pop == 4, d2 := mahalanobis (tab.perc [fisher_pop == 4, c ("den_perc", "rust_perc", "cwd_perc", "move_perc", "open_perc")], c(2.31, 1.63, 10.8, 58.1, 15.58), cov = d2Table[[4]])]

  return (tab.perc)
}
