\name{dosearch-package}
\alias{dosearch-package}
\alias{dosearch}
\docType{package}
\title{Causal Effect Identification from Multiple Incomplete Data Sources}

\description{
Solves causal effect identifiability problems from arbitrary observational and experimental data using a heuristic search. Allows for the presence of advanced data-generating mechanims. See the Vignette for further details.
}

\author{Santtu Tikka, Antti Hyttinen, Juha Karvanen}

\references{
J. D. Angrist, G. W. Imbens, and D. B. Rubin. Identification of causal effects using instrumental variables. \emph{Journal of the American Statistical Association}, 91(434):444--455, 1996.

E. Bareinboim and J. Pearl. Controlling selection bias in causal inference. In \emph{Proceedings of the 15th International Conference on Artificial Intelligence and Statistics}, 22:100--108, 2012a.

E. Bareinboim and J. Pearl. Causal inference by surrogate experiments: z-identifiability. In \emph{Proceedings of the 28th Conference on Uncertainty in Artificial Intelligence}, 113--120, 2012b.

E. Bareinboim and J. Pearl. A general algorithm for deciding transportability of experimental results. \emph{Journal of Causal Inference}, 1:107--134, 2013.

E. Bareinboim and J. Pearl. Transportability from multiple environments with limited experiments: Completeness results. In \emph{Proceedings of the 27th Annual Conference on Neural Information Processing Systems}, 280--288, 2014.

E. Bareinboim and J. Tian. Recovering causal effects from selection bias. In \emph{Proceedings of the 29th AAAI Conference on Artificial Intelligence}, 3475--3481, 2015.

E. Bareinboim, J. Tian, and J. Pearl. Recovering from selection bias in causal and statistical inference. In \emph{Proceedings of the 28th AAAI Conference on Neural Information Processing Systems}, 2014.

N. E. Breslow. Statistics in epidemiology: The case-control study. \emph{Journal of the American Statistical Association}, 91(433):14--28, 1996.

B. Chen, D. Kumor, and E. Bareinboim. Identification and model testing in linear structural equation models using auxiliary variables. In \emph{Proceedings of the 34th International Conference on Machine Learning}, 70:757--766, 2017.

J. Correa and E. Bareinboim. Causal effect identification by adjustment under confounding and selection biases. In \emph{Proceedings of the 31st AAAI Conference on Artificial Intelligence}, 2017.

J. Correa, J. Tian, and E. Bareinboim. Generalized adjustment under confounding and selection biases. In \emph{Proceedings of the 32nd AAAI Conference on Artificial Intelligence}, 2018.

D. Danks, C. Glymour, and R. E. Tillman. Integrating locally learned causal structures with overlapping variables. In \emph{Advances in Neural Information Processing Systems}, 1665--1672, 2009.

A. P. Dawid. Influence diagrams for causal modelling and inference. \emph{International Statistical Review}, 70(2):161--189, 2002.

D. Entner, P. Hoyer, and P. Spirtes. Data-driven covariate selection for nonparametric estimation of causal effects. In \emph{Proceedings of the 16th International Conference on Artificial Intelligence and Statistics}, 31:256--264, 2013.

S. Greenland, J. M. Robins, and J. Pearl. Confounding and collapsibility in causal inference. \emph{Statistical Science}, 14(1):29--46, 1999.

Y. Huang and M. Valtorta. Pearl's calculus of intervention is complete. In \emph{Proceedings of the 22nd Conference on Uncertainty in Artificial Intelligence}, 217--224, 2006.

A. Hyttinen, F. Eberhardt, and P. O. Hoyer. Causal discovery of linear cyclic models from multiple experimental data sets with overlapping variables. In \emph{Proceedings of the 28th Conference on Uncertainty in Artificial Intelligence}, 387--396, 2012.

A. Hyttinen, F. Eberhardt, and M. Jarvisalo. Do-calculus when the true graph is unknown. In \emph{Proceedings of the 31st Conference on Uncertainty in Artificial Intelligence}, 395--404, 2015.

A. Jaber, J. Zhang, and E. Bareinboim. Causal identification under Markov equivalence. In \emph{Proceedings of the 34th Conference on Uncertainty in Artificial Intelligence}, 978--987, 2018.

J. Karvanen. Study design in causal models. \emph{Scandinavian Journal of Statistics}, 42(2):361--377, 2015.

D. Koller and N. Friedman. \emph{Probabilistic Graphical Models: Principles and Techniques}, 2009.

S. L. Lauritzen. Causal inference from graphical models. In \emph{Complex Stochastic Systems}, 67--107, 2000.

R. J. A. Little and D. B. Rubin. \emph{Statistical Analysis with Missing Data}, 1986.

M. H. Maathuis, M. Kalisch, and P. Buhlmann. Estimating high-dimensional intervention effects from observational data. \emph{The Annals of Statistics}, 37(6A):3133--3164, 2009.

D. Malinsky and P. Spirtes. Estimating bounds on causal effects in high-dimensional and possibly confounded systems. \emph{International Journal of Approximate Reasoning}, 88:371--384, 2017.

K. Mohan and J. Pearl. Graphical models for processing missing data. 2018. Forthcoming, https://arxiv.org/abs/1801.03583.

K. Mohan, J. Pearl, and J. Tian. Graphical models for inference with missing data. In \emph{Advances in Neural Information Systems}, 26:1277--1285, 2013.

J. M. Pena and M. Bendtsen. Causal effect identification in acyclic directed mixed graphs and gated models. \emph{International Journal of Approximate Reasoning}, 90:56--75, 2017.

J. Pearl. Causal diagrams for empirical research. \emph{Biometrika}, 82(4):669--688, 1995.

J. Pearl. \emph{Causality: Models, Reasoning, and Inference}, 2009.

E. Perkovic, J. Textor, M. Kalisch, and M. Maathuis. A complete generalized adjustment criterion. In \emph{Proceedings of the 31st Conference on Uncertainty in Artificial Intelligence}, 682--691, 2015.

J. Peters, J. M. Mooij, D. Janzing, and B. Scholkopf. Causal discovery with continuous additive noise models. \emph{Journal of Machine Learning Research}, 15:2009--2053, 2014.

I. Shpitser and J. Pearl. Identification of joint interventional distributions in recursive semi-Markovian causal models. In \emph{Proceedings of the 21st National Conference on Artificial Intelligence -- Volume 2}, 1219--1226, 2006a.

I. Shpitser and J. Pearl. Identification of conditional interventional distributions. In \emph{Proceedings of the 22nd Conference on Uncertainty in Artificial Intelligence}, 437--444, 2006b.

I. Shpitser and J. Pearl. Complete identification methods for the causal hierarchy. \emph{Journal of Machine Learning Research}, 9:1941--1979, 2008.

I. Shpitser, K. Mohan, and J. Pearl. Missing data as a causal and probabilistic problem. In \emph{Proceedings of the 31st Conference on Uncertainty in Artificial Intelligence}, 802--811, 2015.

P. Spirtes, C. Glymour, and R. Scheines. \emph{Causation, Prediction, and Search}, 1993.

S. Tikka and J. Karvanen. Identifying causal effects with the R package causaleffect. \emph{Journal of Statistical Software}, 76(12):1--30, 2017a.

S. Tikka and J. Karvanen. Simplifying probabilistic expressions in causal inference. \emph{Journal of Machine Learning Research}, 18(36):1--30, 2017b.

S. Tikka and J. Karvanen. Enhancing identification of causal effects by pruning. \emph{Journal of Machine Learning Research}, 18(194):1--23, 2018a.

S. Tikka and J. Karvanen. Surrogate outcomes and transportability. Submitted manuscript, http://arxiv.org/abs/1806.07172, 2018b.

R. Tillman and P. Spirtes. Learning equivalence classes of acyclic models with latent and selection variables from multiple datasets with overlapping variables. In \emph{Proceedings of the 14th International Conference on Artificial Intelligence and Statistics}, 3--15, 2011.

S. Triantafillou and I. Tsamardinos. Constraint-based causal discovery from multiple interventions over overlapping variable sets. \emph{Journal of Machine Learning Research}, 16:2147--2205, 2015.

S. Triantafillou, I. Tsamardinos, and I. Tollis. Learning causal structure from overlapping variable sets. In \emph{Proceedings of the 13th International Conference on Artificial Intelligence and Statistics}, 860--867, 2010.

B. van der Zander and M. Liskiewicz. On searching for generalized instrumental variables. In \emph{Proceedings of the 19th International Conference on Artificial Intelligence and Statistics}, 2016.
} % END REFERENCES
