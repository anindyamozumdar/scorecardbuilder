

### Sample

In this tab, you can divide your original data into training and test samples.

The training sample is used to bin a characteristic into attributes to achieve a desired weight of evidence (WoE) pattern and calculate the information value (IV) of a characteristic. The theoretical concepts behind WoE and IV are explained in detail in the help for the Binning tab. The test sample is used to ensure that the patterns hold in a sample separate from the one which you will use to train your model. This ensures that the scorecard you build is robust and does not overfit on a particular sample.

The good/bad flag and any stratification variable you have chosen are used to ensure that their distributions remain the same between the two samples. In real life applications, it is very common to stratify by the good/bad flag and a time period (typically month). This ensures that the monthly bad rate between the training and test samples are the same.

Once you click on the Sample button, the samples will be created. For each stratification variable, you can view the distribution in the train and test samples to verify they are roughly equal.

The slider can be adjusted to specify what percentage of the records from the original data are available in the training sample. There is no specific rule on what ratio is a good split, but 80-20, 70-30 and 50-50 splits are quite common. The training sample must have at least 20% and at most 80% of the records. Do keep in mind that the standard errors and p-values calculated in the Model tab are dependent on the sample size.
