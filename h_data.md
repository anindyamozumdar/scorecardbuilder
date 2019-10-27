

### Introduction

Welcome to Scorecard Builder.

This is an interface to develop credit risk scorecards. A credit risk scorecard is a tool to evaluate the level of risk associated with an application for credit. They are widely used in the banking and consumer finance industries across all products like mortgages, credit cards, auto finance and personal loans. You may have heard of the well-known FICO score used in the United States.

A scorecard comprises characteristics, attributes and score points. A characteristic is a variable which has been determined to be predictive of the risk of an application. An attribute of a characteristic is a value or a group of values valid for that characteristic. A score point is the score assigned for that characteristic and attribute value. A very simple scorecard is shown in the diagram below. In this example, an applicant whose debt to income ratio is 50% will be assigned a score of 62 for this characteristic. The scores are added across all characteristics to arrive at the total score. Each score corresponds to a probability that the customer will go bad, i.e. default on their loan.

<img src="www/data-scorecard.png" title="Simple scorecard" alt="Simple scorecard" width="50%" />

This application allows you to develop a scorecard and covers all aspects of the credit scoring process. To learn more about this topic, refer to the book 'Credit Risk Scorecards' by Naeem Siddiqui (2005).

### Hints

* If you have downloaded the simulated data, then choose *fgood* as the good/bad flag and *period* for stratification.

* Uncheck to not consider *rnd* for binning.

<img src="www/data-data.png" title="Hints" alt="Hints" width="80%" />
