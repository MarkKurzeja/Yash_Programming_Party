# Programming Party with Yash


## Input requirements for R
### Required Inputs
* `x`, the input function
* `y`, the output function
* Form of the covariance matrix
* Parameters for the chosen covariance matrix
* Noise parameter for each of the resulting `x`'s

The current format will be as follows:
1. A file with the covariance specification `setup_cov_mat.txt` that contains all of the relevent information for a valid covariance matrix
2. A file with the testing inputs `setup_test_x.csv` that is a csv with the testing inputs `x`
3. A file `setup_training_data.csv` with the data input, in the form of a csv file, with a proper header and then output labeled in a column called `y` with the same column titles as `setup_test_x.csv`