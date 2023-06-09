# benchmarkRadiomics

We provide a benchmark framework for radiomics-based survival analysis, aimed at identifying the optimal settings with respect to different radiomics data sources and various ML pipeline variations, including preprocessing techniques and learning algorithms.

*Keywords*: radiomics, CT imaging data, colorectal cancer, survival, machine learning, benchmark, mlr3.


Our goal is to provide an mostly automated approach for conducting and evaluating benchmark studies that compare various pipelines for radiomics analyses. This includes the following steps:

1. [Calculation of radiomics features](#calculation-of-radiomics-features)
2. [Radiomics-based benchmark experiment](#radiomics-based-benchmark-experiment)
3. [Evaluation of the benchmark results via an LMM](#evaluation-of-the-benchmark-results-via-an-lmm)

We conducted the aforementioned analysis steps for our original study; however, due to data privacy laws, we are unable to present the study with the original radiomics data. Instead, we provide the analysis using an example dataset. For the final step, where the benchmark results are analyzed using a mixed model approach, we present our own results. Accordingly, all three analysis steps are to be considered disjoint here.


## Calculation of radiomics features

[PyRadiomics](https://pyradiomics.readthedocs.io/en/latest/) is a [Python package](https://github.com/AIM-Harvard/pyradiomics) that can be used to calculate radiomics features. It includes a command-line script that can be used for both single image extraction and batch processing. For our use case we adapted the example [batchprocessing script](https://github.com/AIM-Harvard/pyradiomics/blob/master/examples/batchprocessing.py). This script can also be found in `example_radiomcsCalc.py`.


## Radiomics-based benchmark experiment

Our benchmark was set up as described in the figure below, but it can be easily adapted for regression or classification tasks, including the comparison of different learners with appropriate hyperparameters, utilization of alternative metrics, customization of the preprocessing steps, etc.

![alt text](img/benchmarkPipelineSetup.png "Setup")

We present the benchmark experiment in `2_radiomicsBenchmark.R` for the example dataset of the [Shedden Lung Cancer Study](https://www.openml.org/search?type=data&status=any&id=1245) provided in *OpenML*. 
Adaptations made in comparison to the original study are documented in the comments within the R script.


## Evaluation of the benchmark results via an LMM

In the meta-analysis of the benchmark results, a linear mixed model (LMM) was employed to examine significant differences among the observed performances. The LMM accounted for data correlation induced by repeatedly using the same cross-validation train/test sets by incorporating a random effect. To enable inference with respect to the experimental benchmark setup of our original study, we included datasets and pipeline variations, along with their interactions, as fixed effects in the LMM. The according analysis script is provided `3_1_lmm_sirfloxBMR.R`. Of course the composition of fixed effects is dependent on the benchmark setup chosen. An adapted version of the benchmark evaluation via an LMM is provided in `3_2_lmm_lungBMR.R` for the analysis of results obtained from [Radiomics-based benchmark experiment](#radiomics-based-benchmark-experiment).   

$$ 
y_{idl} = \beta_0 + \beta_d (dataset) + \beta_l (learner) + \beta_{dl} (dataset \times learner) + b_i+ \epsilon_{idl}  			
$$

$$
\begin{align}
\text{with } 	&b_i \sim^{iid} N(0,\tau²) \text{ and } \epsilon_{idl} \sim^{iid} N(0, \sigma²) \\
&i=1, ..., 10 \text{ CV}; d=1, ...,5; l=1, ...,12 \\
&y_{idl}:	\text{ performance IBS of learner } l \text{ for the } i-\text{th fold on data set } d 
\end{align} 
$$

Considering the presence of an interaction term in the linear mixed model (LMM), we computed estimated marginal means (EMMs), also known as least-squares or predicted means. The EMMs represent the adjusted means of each predictor, accounting for the mean values of the predictors they interact with in the model. These predictions were visually presented in an interaction-style plot, which included the EMMs and their corresponding simultaneous confidence intervals (CIs).

![alt text](img/LMM_interactionplot_ci.png)
