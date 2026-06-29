# Project backlog

## Ontology setup

Set up an ontology model for the project on the basis of well-defined
domain-specific concepts. Compose the specification document in *Markdown*.

## Research. Identify spectrum as a spectrum of coal

The identification of a spectrum as a spectrum of coal seems rather natural
for a starting point for *QSPR*-predictions. The next is a preliminary mental
plan for appropriate research:

1. Pick up a large and comprehensive set of coal spectra assuming it as a parent
   statistical population.
2. For all spectra in the set make all relevant physics-feasible per-channel
   spectral transformations which do not *know* anything about the analyzed
   substance (Kubelka-Munk transfromation, log-transformation, etc.).
3. Representing each spectra as 3709-component vector (i.e. as a row of a table
   with 3709 columns) use
   [PCA](https://en.wikipedia.org/wiki/Principal_component_analysis) and
   identify ~10 principle components (principle spectra) for the set along with
   appropriate loadings $a_{01}, a_{02}, \dots, a_{0n}$, where zero-index means
   that these are coefficients of the total set.
4. Decompose each $i$-th spectra into those principal spectra obtaining
   $a_{i1}, a_{i2}, \dots, a_{in}$ loadings and $\epsilon_i$ as error of $i$-th
   decomposition.
5. Find lower $a_{l1}, a_{l2}, \dots, a_{ln}$ and upper
   $a_{u1}, a_{u2}, \dots, a_{un}$ values over all
   $a_{i1}, a_{i2}, \dots, a_{in}, i \in \left [1 \dots 3709 \right]$. For a
   set of  $\epsilon_i$ consider univariate probability density function
   estimate $F_{\epsilon}$: fit [gld](https://CRAN.R-project.org/package=gld)-
   distribution.
6. For any fresh ($f$-index) spectrum make up decomposition to principal spectra using
   multivariate linear regression with $a_f$-coefficient constraints:
   $a_{l1} \leqslant a_{f1} \leqslant a_{u1}, a_{l2} \leqslant a_{f2} \leqslant a_{u2}, \dots, a_{ln} \leqslant a_{fn} \leqslant a_{un}$.
7. For this fresh spectrum get the value of $\epsilon_{f}$ and find its
   probability $p_{f}$ using $F_{\epsilon}$.
8. Consider $p_{f}$ as a probability of the fresh spectrum to be a spectrum of
   coal.

It might be better to find multivariate distribution for $a_{i1}, a_{i2}, \dots, a_{in}$
instead of its limits only and then get statistically induced boundaries for
$a_f$-coefficient constraints.
