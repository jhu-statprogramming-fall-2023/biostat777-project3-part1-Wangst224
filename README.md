# hiperGLM
`hiperGLM` is a high-performance GLM package that fits linear and generalized linear models for large datasets. This package is created by Shengtao Wang under the guidance of Professor Akihiko Nishimura, as part of the BSPH course PH.140.778, and is still in progress.

Link to Github Repo: [https://github.com/Wangst224/hiperGLM](https://github.com/Wangst224/hiperGLM)

Link to deployed website: []()

List of exported functions:
1. `hiper_glm`: the model fitting function, return a hglm object.
2. `coef`: extract coefficients from a hglm object.

An example:
```r
hglm_linear = hiper_glm(X, y, model = "linear", option = list(mle_finder = "pseudo_inv"))
coef(hglm_linear)
```

Website customization:
1. code chunk theme
2. code color
3. navigation bar layout
4. navigation bar GitHub icon
5. home page links
