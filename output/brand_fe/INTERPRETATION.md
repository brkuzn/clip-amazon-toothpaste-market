# Brand fixed effects: are the CLIP components just proxying for brand?

Produced by `scripts/brand_fe_variants.R`. Eleven `brand_grouped_50` dummies
were added to the design matrix (base = APAGARD) and `is_other` dropped as
collinear. Each specification keeps its own rho profile, marginal-cost
inversion and merger simulation.

## Results

| specification | rho* | RSS drop | neg-MC | hello effect | Colgate effect |
|---|---|---|---|---|---|
| brand FE, no nesting | 0.00 | 0.00% | 8.59% | +1.233% | +0.104% |
| brand FE + brand nesting | 0.55 | 6.88% | **8.59%** | +1.350% | +0.105% |
| brand FE + CLIP nesting | 0.64 | 8.70% | 1.80% | +0.436% | +0.022% |

Joint-PC t statistics and loadings under brand fixed effects:

| PC | t (no nesting) | t (brand nest) | t (CLIP nest) | beta (CLIP nest) |
|---|---|---|---|---|
| joint_pc1 | −3.939 | −4.247 | −4.273 | −0.3728 |
| joint_pc2 | 9.522 | 9.883 | 9.918 | +0.8318 |
| joint_pc3 | 1.716 | 1.430 | 1.519 | +0.1124 |
| joint_pc4 | 7.755 | 7.848 | 8.058 | +0.5485 |
| joint_pc5 | 2.434 | 2.806 | 2.677 | +0.1593 |

rho*, RSS drop and the negative-MC share reproduce the Python reimplementation
exactly in all three specifications, as do the reported loadings.

## Result 1: the cost side does not move, exactly as Appendix D predicts

Brand fixed effects raise the brand-nested rho* from 0.35 to 0.55 and roughly
double the RSS drop, from 3.08% to 6.88%. The demand side clearly improves. The
negative-MC share does not move at all: **8.59% with no nesting and 8.59% with
brand nesting**, identical to plain logit.

This is Appendix D working as proved. Marginal costs are recovered from the
pre-merger first-order conditions, and pre-merger every brand nest lies wholly
inside a single firm. The proposition then fixes the multiproduct Bertrand
markup at 1/(|alpha|(1 − S_f)) regardless of rho, so no amount of fit
improvement on the demand side can move the recovered costs. Brand fixed effects
and a higher within-brand correlation change what the model says about
substitution; they cannot change what it says about markups, because the nest
partition never crosses an ownership line.

## Correction: the hello merger effect is *not* identical across those two

The expectation carried into this exercise was that the hello merger effect
would also be unchanged at +1.353% in both specifications, identical to three
decimals. **That is not what happens, and it should not.** The run gives
+1.233% without nesting and +1.350% with brand nesting.

The reason is a genuine limit on Appendix D's scope. The proposition requires
the nest partition to refine the *ownership* partition. That holds pre-merger,
which is why the cost side is untouched. It stops holding post-merger: once
hello joins Colgate, the combined firm owns products in two different brand
nests, the refinement condition fails, and the post-merger Nash markups are free
to differ from the plain-logit ones. The counterfactual therefore can move even
though the recovered costs cannot.

The baseline models already show this: M1 gives +1.23% and M2 gives +1.27% for
hello, which are close but not equal. The brand-FE variants reproduce the same
pattern with a wider gap because rho is higher (0.55 rather than 0.35).

Stating it precisely makes the Appendix D claim stronger, not weaker: the
identity is exact where it applies (pre-merger cost recovery, hence the
unchanged 8.59%) and is not claimed where it does not apply (the post-merger
counterfactual).

## Result 2: the CLIP components survive brand absorption

Adding brand fixed effects to CLIP nesting barely disturbs anything: rho* moves
0.67 → 0.64, the negative-MC share 1.72% → 1.80%, and the hello effect from
+0.40% to +0.44%. **Four of the five joint PCs remain significant at the 5%
level with all brand variation absorbed by dummies.**

That is the answer to the proxy question. If the PCs were simply encoding brand
identity, eleven brand dummies would strip them of explanatory power. They do
not. The components carry within-brand positioning information — how a
particular listing presents itself relative to others from the same brand — that
brand identity alone cannot express.

## An honest caveat about the individual loadings

The joint explanatory power survives; the individual coefficients do not.

- `joint_pc3` loses significance under brand fixed effects, its t statistic
  falling from −7.71 in the baseline to 1.519 here.
- Two loadings change sign: `joint_pc3` (−0.2854 → +0.1124) and `joint_pc5`
  (−0.1745 → +0.1593).
- `joint_pc2` keeps its sign but its loading roughly quadruples
  (+0.2103 → +0.8318).

The principal components are not invariant to absorbing brand. Once the brand
dummies take out the between-brand variation, what remains for each PC to
explain is a different object, and the components re-weight accordingly. So the
defensible claim is about their **joint** contribution, not about any one
loading being a stable structural parameter. Reading economic meaning into the
sign of an individual PC would be a mistake in either specification.
