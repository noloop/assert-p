# assert-p

### _A library of assertions written in Common Lisp._

## Getting Started in assert-p

### Dependencies

:assertion-error

### Download and installation

**1 - Download assert-p system**

By quicklisp:

```
IN PROGRESS...
```

or directly from github:

```
git clone https://github.com/noloop/assert-p.git
```
**2 - Install assert-p**

By quicklisp:

```
IN PROGRESS...
```

or directly from asdf:

```lisp
(asdf:load-system :assert-p)
```

_**Note: Remember to configure asdf to find your directory where you downloaded the libraries (asdf call them "systems") above, if you do not know how to make a read at: https://common-lisp.net/project/asdf/asdf/Configuring-ASDF-to-find-your-systems.html or https://lisp-lang.org/learn/writing-libraries.**_

## t-p

Check actual eq t. Return t when actual is t, and throw assertion-error when actual is nil.

```lisp
(t-p t) => t
(t-p nil) => throw assertion-error
(t-p '()) => throw assertion-error
(t-p 5) => throw assertion-error
```

## not-t-p

Check actual not eq t. Return t when actual is nil, and throw assertion-error when actual is t.

```lisp
(not-t-p t) => throw assertion-error
(not-t-p nil) => t
(not-t-p '()) => t
(not-t-p 5) => t
```

## API

function **(t-p actual)**

function **(not-t-p actual)**

function **(zero-p actual)**

function **(not-zero-p actual)**

function **(nil-p actual)**

function **(not-nil-p actual)**

function **(null-p actual)**

function **(not-null-p actual)**

function **(eq-p actual expected)**

function **(not-eq-p actual expected)**

function **(eql-p actual expected)**

function **(not-eql-p actual expected)**

function **(equal-p actual expected)**

function **(not-equal-p actual expected)**

function **(equalp-p actual expected)**

function **(not-equalp-p actual expected)**

function **(typep-p actual expected)**

function **(not-typep-p actual expected)**

function **(values-p predicate actual expected)**

function **(not-values-p predicate actual expected)**

function **(error-p form)**

function **(not-error-p form)**

function **(condition-error-p form condition)**

function **(not-condition-error-p form condition)**

function **(custom-p test-result actual expected operator)**