

### Explore

This tab allows you to do exploratory analysis on your data. There are two views available in this tab.

* All Variables - This uses the *summarytools* package to quickly show the distribution of all variables in your data. The type of the variable reported by the package is detected by the functions in the package and may not match the type you have specified in the interface.

* Single Variable - Here you can choose the variable for which you want to calculate descriptive statistics. For numeric variables, a number of descriptive statistics are computed along with a density plot. The density is an estimate computed by *geom_density* in the *ggplot2* package. For character or integer variables, the number and percentage of records for each unique value is computed.
