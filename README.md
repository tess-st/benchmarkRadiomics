# benchmarkRadiomics

We provide a benchmark framework for radiomics-based survival analysis, aimed at identifying the optimal settings with respect to different radiomics data sources and various ML pipeline variations, including preprocessing techniques and learning algorithms.

Keywords: radiomics, CT imaging data, colorectal cancer, survival, machine learning, benchmark, mlr3.


## Calculation of radiomics features

[PyRadiomics](https://pyradiomics.readthedocs.io/en/latest/) is a [Python package](https://github.com/AIM-Harvard/pyradiomics) that can be used to calculate radiomics features. It includes a command-line script that can be used for both single image extraction and batch processing. For our use case we adapted the example [batchprocessing script](https://github.com/AIM-Harvard/pyradiomics/blob/master/examples/batchprocessing.py). This script can also be found in `PyRadiomics/example_radiomcsCalc.py`.


## Setup of the benchmark pipeline

Our benchmark was set up as described in the figure below, but it can be easily adapted for regression or classification tasks, including the comparison of different learners with appropriate hyperparameters, utilization of alternative metrics, customization of the preprocessing steps, etc.

![alt text](img/benchmarkPipelineSetup.png "Setup")

