

### Validation

The scorecard is validated based on three key parameters

* Discrimination measures the ability of the score to separate between goods and bads and is typically measured using the KS and Gini coefficient

* Accuracy measures the percentage error (PE) between the predicted and realised bad rates at all levels of the score

* Stability measures the difference in score distribution between the training and test samples and is measured by the Population Stability Index (PSI)

Rules of thumb

* A Gini of 50% or higher provides strong discrimination

* Weighted average PE should be less than 10%

* PSI should be less than 10% (PSI uses the same formula as IV except that the distribution of records are used rather than the percentage of good and bad records)

<img src="D:/Online/Personal/Training/scorecardbuilder/inst/scorecardbuilder_shiny/www/validation-discrimination.png" title="Discrimination" alt="Discrimination" width="50%" />

<img src="D:/Online/Personal/Training/scorecardbuilder/inst/scorecardbuilder_shiny/www/validation-accuracy.png" title="Accuracy" alt="Accuracy" width="50%" />

<img src="D:/Online/Personal/Training/scorecardbuilder/inst/scorecardbuilder_shiny/www/validation-stability.png" title="Stability" alt="Stability" width="50%" />
