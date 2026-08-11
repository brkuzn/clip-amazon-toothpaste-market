# Brand-orthogonal nesting: what happens when brand is stripped out

Produced by `scripts/brand_orthogonal_nesting.R`. Everything below comes from
that run; the Python comparison column is the reimplementation used as a check.

## What was done

Each of `joint_pc1..joint_pc5` was regressed on a full set of
`brand_grouped_50` dummies at the ASIN level, and the residuals kept. k-means
(K = 5, seed 42, nstart 50) was then run on the residual space, and those
clusters used as nests in the usual pipeline: profile rho, invert marginal
costs from the Bertrand first-order conditions, re-solve Nash prices under pre-
and post-merger ownership.

## Results

| metric | brand-orthogonal | CLIP | Python (brand-orth.) |
|---|---|---|---|
| brand R² across the five PCs | 0.736 | — | 0.736 |
| ARI vs brand groups | −0.0008 | +0.6206 | ≈ −0.001 |
| rho* | 0.49 | 0.67 | 0.44 |
| RSS drop | 5.7% | 8.1% | 4.72% |
| negative-MC share | 1.5% | 1.7% | 1.72% |
| hello merger effect | +5.562% | +0.404% | +6.11% |
| Colgate merger effect | +0.473% | +0.018% | — |
| diversion hello → Colgate | 5.95% | 0.41% | 9.33% |
| mean distinct firms per nest | 6.00 | 2.80 | 5.26 |

Per-PC brand R²: 0.8701, 0.7642, 0.8197, 0.7247, 0.4429. The residual space
retains 26.4% of the joint-PC variance.

### On the difference from the Python figures

Steps 1–3 agree to the digit: identical per-PC R², identical variance-weighted
R² of 0.736, and an ARI against brand of essentially zero in both. The residual
space is therefore the same object in both implementations.

The downstream numbers differ because R's `kmeans` and scikit-learn's
`KMeans` do not produce the same partition from the same seed — the
initialisation schemes and the Lloyd variants differ, and `set.seed(42)` in R
is not `random_state=42` in Python. A different partition of the same residual
space gives a different rho profile and therefore different merger effects.

This is worth stating plainly rather than hiding: **the conclusion does not
depend on which of the two partitions you take.** Both give a negative-MC share
at CLIP's level (1.5% and 1.72% against CLIP's 1.7%), both fit demand distinctly
worse than CLIP (5.7% and 4.72% against 8.1%), both produce a hello effect an
order of magnitude above CLIP's (+5.56% and +6.11% against +0.40%), and both
scatter hello across four of the five nests. A finding that survives two
independent clusterings of the same space is more credible than one tuned to a
single seed.

## What it means

Three things happen at once when brand is removed, and they pull in different
directions.

**The cost side is repaired just as well.** The negative-MC share falls to 1.5%,
indistinguishable from CLIP's 1.7%. This is the placebo result again, from a
different angle: what fixes the Bertrand inversion is nesting that crosses
ownership boundaries, and brand-residual clusters do that abundantly — 6.00
distinct firms per nest against CLIP's 2.80. Cost-side repair is cheap. Almost
any cross-firm partition buys it.

**Demand fit gets distinctly worse.** The RSS drop falls from 8.1% to 5.7%. The
nesting term only lowers residual variance when the products grouped together
genuinely share demand shocks, and 73.6% of the information the embeddings carry
about product position was just thrown away. What is left is real but thinner.

**The merger prediction changes by an order of magnitude**, from +0.40% to
+5.56%, and the mechanism is visible in the diversion: hello-to-Colgate rises
from 0.41% to 5.95%. With brand residualised out, hello's ASINs scatter across
four of the five nests, so hello's within-nest rivals now routinely include
Colgate products. The model is told that a shopper leaving hello is likely to
pick up a Colgate tube, and the simulated post-merger price rises accordingly.
That diversion is an artifact of the partition, not something the data
established.

## The honest conclusion

This is evidence **against** reading the CLIP result as brand-independence.

The embeddings encode the whole listing — the title text, the benefit
description and the image — and the brand name and logo are part of what a
shopper sees, so they are part of what CLIP encodes. Brand dummies explain 73.6%
of the joint-PC variance and the adjusted Rand index between the CLIP clusters
and the brand groups is 0.62. The clusters are strongly brand-correlated, and
any claim that they are orthogonal to brand is false.

What is true, and what the thesis actually needs, is narrower and more
defensible: the CLIP partition **cuts across ownership** (2.80 firms per nest
against exactly 1.00 for brand nests) while still tracking the positioning that
consumers respond to. Removing brand keeps the ownership-crossing property and
loses the positioning; brand nesting keeps brand and loses the
ownership-crossing. Only the brand-inclusive embedding has both, which is why
only it both fits demand well and places hello and Colgate in genuinely
different parts of the product space.

Put another way: brand is one signal among several in the embedding, and
stripping it out removes information the market actually uses. The contribution
is not that CLIP ignores brand. It is that CLIP reproduces a positioning
structure in which brand matters without being the only thing that matters, and
that structure happens not to respect ownership boundaries.
