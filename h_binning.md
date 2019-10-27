

### Binning

The general objective of binning is to obtain a strong separation of the weight of evidence (WoE) between bins and a high information value (IV) for the characteristic.

This is generally done by constructing an initial set of bins, and then splitting and combining bins to arrive at a desired pattern. For each bin j, the WoE for that bin is defined by the following formula (where N is the total number of bins).

<img src="www/binning-woe.png" title="Weight of Evidence" alt="Weight of Evidence" width="50%" />

The IV is calculated at the characteristic level. Higher the IV, the more the predictive power of this characteristic in separating goods from bads. A general rule of thumb is that to be considered in the model the IV must be at least 3%.

<img src="www/binning-iv.png" title="Information Value" alt="Information Value" width="80%" />

The following diagrams show a numeric and a character variable, after they have been binned to obtain a monotonic WoE pattern.

<img src="www/binning-numeric.png" title="Binning Numeric" alt="Binning Numeric" width="80%" />

<img src="www/binning-character.png" title="Binning Character" alt="Binning Character" width="80%" />
