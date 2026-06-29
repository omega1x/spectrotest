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
   identify $n \sim 10$ principle components (principle spectra) for the set along with
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

> It might be better to find multivariate distribution for
> $a_{i1}, a_{i2}, \dots, a_{in}$
> instead of its limits only and then get statistically induced boundaries for
> $a_f$-coefficient constraints.

Finally check the value of $p_{f}$ for spectra which visually certain are not
spectra of coal. For example, consider low-energy spectra obtained in
spectrometer detector fail state, spectra of DRIFT mirror and rough optical
samples.

## Research. Natural classification

Find out natural patterns in coal spectra. It is well know that an autoencoder
completely bypasses the limitations of linear
[PCA](https://en.wikipedia.org/wiki/Principal_component_analysis) by squeezing
all 3,709 dimensions down into a clean, *3D*-structure natively. This preserves
dense clusters while effectively eliminating the curse of dimensionality.

For continuous *FTIR*-spectra data where you explicitly want to preserve
Euclidean distances and encourage spherical clusters, a vanilla
*Deep Autoencoder* must be carefully adjusted. Because standard autoencoders
compress data without a geometric constraint, they often twist, bend, and warp
the latent space. This warps Euclidean distances and elongates density
structures, making clusters arbitrary in shape rather than spherical. To solve
this, you should implement a *Variational Autoencoder* (VAE) using R-package
`torch`.

### Why a VAE is perfect for FTIR-spectra?

1. **Forces Spherical Clusters**: A VAE adds a
   *Kullback-Leibler (KL) divergence* penalty to the loss function. This forces
   the latent space to structure itself like a standard Gaussian distribution,
   organically encouraging clusters to remain compact and spherical.
2. **Protects Euclidean Interpretation**: By structuring the latent space around
   a normal distribution, distances between points represent meaningful
   proximity. It actively penalizes the network for creating highly elongated or
   disjointed spaces.
3. **Handles FTIR Collinearity**: Spectra are highly continuous and smoothly
   varying. The dense layers in the encoder act like complex, adaptive smoothing
   filters across the 3,709 wavelength channels.

### VAE Architecture in R (using package `torch`)

Here is the complete implementation optimized for your 3,709-wavelength
*FTIR*-data, compressing it directly into a *3D* Euclidean space optimized for
standard [DBSCAN]( https://CRAN.R-project.org/package=dbscan).

```r
library(torch)

# 1. Dummy FTIR Spectra setup (18,000 samples x 3,709 wavelengths)
set.seed(42)
raw_spectra <- matrix(runif(18000 * 3709, 0, 2), nrow = 18000, ncol = 3709)

device <- if (cuda_is_available()) "cuda" else "cpu"
X_tensor <- torch_tensor(raw_spectra, dtype = torch_float(), device = device)

# 2. Define Variational Autoencoder (VAE)
VAE <- nn_module(
  "VAE",
  initialize = function() {
    # Shared Encoder Back-bone
    self$encoder_backbone <- nn_sequential(
      nn_linear(3709, 512),
      nn_relu(),
      nn_linear(512, 64),
      nn_relu()
    )
    
    # Latent space heads: Mean vector and Log-Variance vector (3 Dimensions)
    self$fc_mu <- nn_linear(64, 3)
    self$fc_logvar <- nn_linear(64, 3)
    
    # Decoder: Reconstructs the 3D spherical space back to 3709 wavelengths
    self$decoder <- nn_sequential(
      nn_linear(3, 64),
      nn_relu(),
      nn_linear(64, 512),
      nn_relu(),
      nn_linear(512, 3709)
    )
  },
  
  # Reparameterization trick ensures gradients can flow through random sampling
  reparameterize = function(mu, logvar) {
    std <- torch_exp(0.5 * logvar)
    eps <- torch_randn_like(std)
    return(mu + eps * std)
  },
  
  forward = function(x) {
    h <- self$encoder_backbone(x)
    mu <- self$fc_mu(h)
    logvar <- self$fc_logvar(h)
    
    z <- self$reparameterize(mu, logvar)
    reconstructed <- self$decoder(z)
    
    list(reconstructed = reconstructed, mu = mu, logvar = logvar)
  }
)

model <- VAE()
model$to(device = device)
optimizer <- optim_adam(model$parameters, lr = 0.0005) # Lower learning rate for VAE stability

# 3. VAE Loss Function: MSE (Reconstruction) + KL Divergence (Spherical Regularizer)
vae_loss_function <- function(reconstructed, target, mu, logvar, kl_weight = 0.01) {
  # MSE measures how well we reconstruct the FTIR spectrum shape
  mse_loss <- nnf_mse_loss(reconstructed, target, reduction = "mean")
  
  # KL Divergence forces the 3D space to be Gaussian/Spherical
  kl_loss <- -0.5 * torch_mean(1 + logvar - mu$pow(2) - logvar$exp())
  
  # Balance reconstruction accuracy vs cluster sphericity
  return(mse_loss + (kl_weight * kl_loss))
}

# 4. Training Loop
batch_size <- 256
epochs <- 25
num_samples <- nrow(raw_spectra)

for (epoch in 1:epochs) {
  permutation <- sample(num_samples)
  epoch_loss <- 0
  
  for (i in seq(1, num_samples, by = batch_size)) {
    optimizer$zero_grad()
    
    indices <- permutation[i:min(i + batch_size - 1, num_samples)]
    batch_x <- X_tensor[indices, ]
    
    output <- model(batch_x)
    
    loss <- vae_loss_function(output$reconstructed, batch_x, output$mu, output$logvar, kl_weight = 0.005)
    
    loss$backward()
    optimizer$step()
    epoch_loss <- epoch_loss + loss$item()
  }
  cat(sprintf("Epoch %d/%d - Total VAE Loss: %.5f\n", epoch, epochs, epoch_loss / (num_samples / batch_size)))
}

# 5. Extract Euclidean 3D Mean Vectors for DBSCAN
model$eval()
with_no_grad({
  # Pass the dataset through the backbone and grab the mu (mean) vectors
  h_all <- model$encoder_backbone(X_tensor)
  dbscan_ready_features <- model$fc_mu(h_all) 
  
  dbscan_matrix <- as.matrix(dbscan_ready_features$cpu())
})

# 'dbscan_matrix' is now a 18000 x 3 matrix ready for Euclidean DBSCAN!
```

### Tips for Applying Euclidean DBSCAN on this 3D Space

1. **Explicitly Set the Metric**: When running your clustering, ensure you pass
   `metric = "euclidean"` to your R DBSCAN package
   (like `dbscan::dbscan(dbscan_matrix, eps = ..., minPts = 6)`).
2. **Fine-Tuning Cluster Density**: In the VAE code, `kl_weight` acts as your
   knob for sphericity. If your final DBSCAN clusters still look too irregular
   or elongated, increase `kl_weight` to `0.01` or `0.02`. This forces the
   network to prioritize the spherical Euclidean constraint over perfect
   spectrum reconstruction.

### Cluster-Averaged Spectra Validation

Once you run DBSCAN on your spherical 3D matrix and identify your clusters,
average the raw 3,709-dimensional spectra within each cluster. By comparing the
mean physical spectrum of *Cluster A* versus *Cluster B*, you can evaluate the
physical features your VAE separated:

```r
# Assumes 'dbscan_result$cluster' contains the cluster assignments (0 = noise, 1, 2, 3...)
# Calculate the mean spectrum for each identified cluster
cluster_assignments <- dbscan_result$cluster

unique_clusters <- unique(cluster_assignments[cluster_assignments > 0])
mean_spectra_list <- lapply(unique_clusters, function(cl) {
  colMeans(raw_spectra[cluster_assignments == cl, , drop = FALSE])
})

# Plot these mean profiles over each other 
# Direct chemical shifts or peak intensity differences between the profiles 
# validate whether your 3D space effectively isolated expected chemical
```

