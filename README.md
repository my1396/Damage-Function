# Interactive Effects of Temperature and Precipitation on Global Economic Growth

The current repository contains the following scripts:

`1_damage_fun_estimate.R` has three regressions:

1.  baseline regression from Burke et al (2015);
2.  model with interactive effects + AFE;
3.  model with interactive effects + IFE; (Hande's Code)

`2_econ_control.R` has robustness checks for economic controls.

`3_climate_proj.R` synthesizes Global Climate Model (GCM) projections for future climates until 2100.

`4_GDP_pathway.R` calculates GDP pathways until 2100.

`5_difference_in_GDP_with-without_IE.R` calculates difference in Climate Change impacts on GDP with and without interactive terms.

`6.1_plot_optimal_curves.R` plots optimal curves with scatter points of countries representing current climate in 2019.

`6.2_plot_optimal_curves.R` plots pathways of $\eta$ as shown in eq. (8).

`6.3_plot_diff-pre.R` plots Interactive effects vs. Projected precipitation as in Fig. 2c.

`6.4_Fig_response-surface.R` plots the response surface as in Fig. 1.

`6.5_Fig_projection-2100.R` plot economic projections until 2100 as in Fig.3.

`7.1_bootstrap_regression.R` bootstraps historical observational datasets to gain a distribution of parameter estimates.

`7.2_bootstrap_path_persistent.R` based on bootstrapped regression coefficients, calculates projected economic impacts per bootstrap.

`7.3_plot_bootstrap_path_persistent.R` plots uncertainty band for economic losses.

`fun_script.R` includes user defined functions.

`/data` is the data folder.

Note that the scripts should be run in the prescribed order, as later scripts may rely on data generated by earlier ones.
All scripts are written using R version 4.3.1.


