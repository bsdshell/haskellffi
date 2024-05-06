# haskellffi

* $sp/haskellffi 

* It only need $b/AronCLibFFI.c in haskellffi.cabal

```
stack build haskellffi
```

* GHC compile `$b/AronCLibFFI.c` to `AronCLibFFI.o` under `$b/AronCLibFFI`
* NOTE: Comment out the `main` in $b/AronCLibFFI.c

