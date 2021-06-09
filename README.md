# Bayesian-Inference

## JAGS
1. Install rjags
```{r}
install.packages("rjags")
library(rjags)
```
If you got error, install the package with the [link](https://sourceforge.net/projects/mcmc-jags/files/JAGS/4.x/Mac%20OS%20X/).
```{r}
library(rjags)
Error : .onLoad failed in loadNamespace() for 'rjags', details:
  call: dyn.load(file, DLLpath = DLLpath, ...)
  error: unable to load shared object '/Users/casallas/Library/R/3.0/library/rjags/libs/rjags.so':
  dlopen(/Users/casallas/Library/R/3.0/library/rjags/libs/rjags.so, 10): Library not loaded: /usr/local/lib/libjags.3.dylib
  Referenced from: /Users/casallas/Library/R/3.0/library/rjags/libs/rjags.so
  Reason: image not found
Error: package or namespace load failed for ‘rjags’
```
2. Generate model
```{r}
model = jags.model(textConnection(modelstring), data=list(y=y,I=I,t=t), n.chains=3, n.adapt=1000)
```
3. Burning
```{r}
update(model, n.iter=1000)
```
4. Thining
```{r}
mcmc.samples = coda.samples(model, variable.names=c("a","b","c",'d'), n.iter=2000, thin=10)
```
