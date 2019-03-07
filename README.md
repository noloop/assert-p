# assert-p

### _A library of assertions written in Common Lisp._

## Getting Started in assert-p

### Dependencies

[:assertion-error](https://github.com/noloop/assertion-error)

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

function **(t-p actual)** => t or throw assertion-error

function **(not-t-p actual)** => t or throw assertion-error

function **(zero-p actual)** => t or throw assertion-error

function **(not-zero-p actual)** => t or throw assertion-error

function **(nil-p actual)** => t or throw assertion-error

function **(not-nil-p actual)** => t or throw assertion-error

function **(null-p actual)** => t or throw assertion-error

function **(not-null-p actual)** => t or throw assertion-error

function **(eq-p actual expected)** => t or throw assertion-error

function **(not-eq-p actual expected)** => t or throw assertion-error

function **(eql-p actual expected)** => t or throw assertion-error

function **(not-eql-p actual expected)** => t or throw assertion-error

function **(equal-p actual expected)** => t or throw assertion-error

function **(not-equal-p actual expected)** => t or throw assertion-error

function **(equalp-p actual expected)** => t or throw assertion-error

function **(not-equalp-p actual expected)** => t or throw assertion-error

function **(typep-p actual expected)** => t or throw assertion-error

function **(not-typep-p actual expected)** => t or throw assertion-error

function **(values-p predicate actual expected)** => t or throw assertion-error

function **(not-values-p predicate actual expected)** => t or throw assertion-error

function **(error-p form)** => t or throw assertion-error

function **(not-error-p form)** => t or throw assertion-error

function **(condition-error-p form condition)** => t or throw assertion-error

function **(not-condition-error-p form condition)** => t or throw assertion-error

function **(custom-p test-result actual expected operator)** => t or throw assertion-error