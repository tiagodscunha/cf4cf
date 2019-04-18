# CF4CF and CF4CF-META: Collaborative Filtering algorithm selection frameworks

Source code for all experimental procedures used to empirically compare multiple these Collaborative Filtering algorithm selection frameworks. The code can be used to replicate the experiments of the contributions presented in the following research papers:

- Cunha, T., Soares, C., & de Carvalho, A. C. P. L. F. (2018). A Label Ranking approach for selecting rankings of Collaborative Filtering algorithms. In ACM Symposium on Applied Computing (pp. 1393–1395).
- Cunha, T., Soares, C., & de Carvalho, A. C. P. L. F. (2018). CF4CF: Recommending Collaborative Filtering Algorithms Using Collaborative Filtering. In Proceedings of the 12th ACM Conference on Recommender Systems (pp. 357–361). 
- Cunha, T., Soares, C., & de Carvalho, A. C. P. L. F. (2018). CF4CF-META: Hybrid Collaborative Filtering Algorithm Selection Framework. In L. Soldatova, J. Vanschoren, G. Papadopoulos, & M. Ceci (Eds.), Discovery Science (pp. 114–128). Cham: Springer International Publishing.

The current code reflects an empirical comparison of such approaches to the scope of CF algorithm selection. The results reported here will be included in the PhD thesis. The code consists on a series of R scripts, aimed at evaluating the aforementioned frameworks on multiple evaluation scenarios. In order to simplify the experiments, the scripts represent individual experiments, each with a specific scope, aimed to be executed independently. In order to organize the code, a prefix number was used to identify multiple meta-approaches:

- 1: Label Ranking approaches
- 2: CF4CF
- 3: CF4CF-META
- 4: ASLIB (more information here: https://www.coseal.net/aslib/)
- 5: ALORS (our implementation of the meta-approach presented in: https://www.sciencedirect.com/science/article/pii/S0004370216301436)

The remaining of items in this repository can be organized as follows:

Folders:
- labelrankingforests-master: source code with Label Ranking algorithms implementation and evaluation and tuning procedures (source code adapted from https://github.com/rebelosa/labelrankingforests)
- metafeatures_graph: graph-based CF metafeatures (proposed in https://arxiv.org/abs/1807.09097)
- metafeatures_landmarkers: subsampling landmarkers metafeatures (proposed in https://link.springer.com/chapter/10.1007/978-3-319-67786-6_14)
- metafeatures_statistical: statistical and information theoretical metafeatures (proposed in https://link.springer.com/chapter/10.1007/978-3-319-46227-1_25)
- results: metalevel evaluation results for all metamodels used
- targets: multicriteria metatargets (proposed in https://arxiv.org/abs/1807.09097)

Other Files:
- auxiliary.R: auxiliary functions to process metadata and metatargets
- tuningCF.R: auxiliary functions used to perform hyperparameter tuning in CF4CF and CF4CF-META
- NEW_ALS.R: override function used to retrieve factorized matrices from recommenderlab's ALS
- RF_multioutput.R: auxiliary script used to instantiate a multi-output regressor to be used in our ALORS implementation
- results_paper.R: main script which loads the results from the folder and creates the visualizations 

