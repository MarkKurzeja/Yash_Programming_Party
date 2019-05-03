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


## Design requirements for full process

1. Inputs Phase:
    * Covariance Configuration File
    * Training Data input
    * Testing Data input
2. Stan processing step
    * Stan will require a beginning processing step to get the data into a list format
3. Training step
    * R Model
    * Stan Model
4. Post processing step Stan
    * Stan will need the posterior predictive model to be processed into an output file
5. Post processing in general
    * a csv file of the input x and the output y-hat will be computed
    * Graphical models that plot training vs test, test with prediction bounds (lower and upper only) etc will be bulit


