# Medical Cost

The present project consisted in the analysis of a real dataset, in order to predict the individual medical costs charged by health insurance. For this purpose, in a first instance, a data treatment was carried out to allow inferring how the different predictors affect the variable of interest. Subsequently, four supervised linear regression models were performed, from which the least significant variables for the study were gradually removed. Quality assessment tests were carried out on these models through the _Validation Set Approach_ and, later, _cross validation_, using the **K-fold Cross Validation** and **Leave-One-Out Cross Validation** methods, to compare the different adjusted models. Given the amount of errors associated with the prediction of the variable of interest resulting from the adjusted models, the variable of interest was manipulated, taking a _logarithmic approach_ to it. There was a Gaussian distribution of the values of the new variable of interest, contrary to the slope to the right of the values of the previous approach. For this interpretation, two supervised linear regression models were performed, which equally removed the predictors with the lowest meaningfulness. Subsequently, the same quality assessment tests were carried out. An improvement in the quality of the new models was observed compared to the previous approach. Despite this, linear regression proved to be insufficient to successfully predict the variable of interest. For this reason, it was decided to carry out a **K-Nearest-Neighbor** regression study on the two approaches for predicting the variable of interest. 
<br>
Based on the analysis of the results, it was observed that the model capable of answering the questions of interest with greater accuracy consisted of the one that used the original variable of interest, through the K-Nearest-Neighbors regression. ([Technical report](https://github.com/13caroline/Automatic-Learning-I/blob/main/Medical_Cost_Personal.pdf))

## Quality Assessment Techniques

* Validation Set Approach
* Cross Validation
   1. K-fold Cross Validation
   2. Leave-One-Out Cross Validation
* K-Nearest-Neighbors

### Questions asked
* What variables influence the amount of individual medical costs charged by health insurance?
* Is health insurance more beneficial for large families?
* How do body mass index levels influence individual medical costs charged for insurance?
* Are smokers subject to higher costs? 


## Collaborators

| Name            	|
|-----------------	|
| [Bruno Veloso](https://github.com/brunocv)                 |
| [Carolina Cunha](https://github.com/13caroline)  	|

> <img src="https://seeklogo.com/images/U/Universidade_do_Minho-logo-CB2F98451C-seeklogo.com.png" align="left" height="48" width="48" > University of Minho, Software Engineering (4th Year).
