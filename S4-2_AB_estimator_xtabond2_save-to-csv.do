// coef vector
matrix b = e(b)
matrix list b

* Show only the first 9 columns
matrix b9 = b[1,1..9]
matrix list b9

// Save coefficient estimates
mat2txt, matrix(b9) saving("data/stata/xtabond2_coef_vector.txt") replace

// Save to CSV with test statistics
esttab dynamic_model_xtabond2 using "data/stata/xtabond2_results.csv", ///
    cells("b(fmt(%9.4f)) se(fmt(%9.4f) par) t(fmt(%9.2f)) p(fmt(%9.3f) star)") ///
    star(* 0.10 ** 0.05 *** 0.01) ///
	legend ///
    csv replace ///
    stats(N N_g ar1p ar2p hansenp, ///
          labels("Observations" "Groups" "AR(1) p-value" "AR(2) p-value" "Hansen p-value") ///
          fmt(%9.0f %9.0f %9.3f %9.3f %9.3f))
