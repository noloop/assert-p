# assert-p Test

## How to test assert-p?

It's quite simple, in REPL:

```lisp
(asdf:test-system :assert-p)
```

If "T" is returned in Test result because it passed, if "nil" is returned because it failed.
