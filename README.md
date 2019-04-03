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

## But what is a library of assertions?

An assertion library assembles is an set of assertions that throw an assertion-error when the assertion fails. An assertion library usually follows a pattern so that the Test Runners can capture the assertion-error that it throws. The default I follow is using the library [:assertion-error](https://github.com/noloop/assertion-error) of which **assert-p** is dependent. All **assert-p** assertions return `t` when the assertion passes and `throw assertion-error` when the assertion fails.

## (t-p actual)

Check actual eq t. Return t when actual is t, and throw assertion-error when actual is not t.

```lisp
(t-p t) => t
(t-p nil) => throw assertion-error
(t-p '()) => throw assertion-error
(t-p 5) => throw assertion-error
```

## (not-t-p actual)

Check actual not eq t. Return t when actual is not t, and throw assertion-error when actual is t.

```lisp
(not-t-p t) => throw assertion-error
(not-t-p nil) => t
(not-t-p '()) => t
(not-t-p 5) => t
```

## (zero-p actual)

Check actual eq 0. Return t when actual is 0, and throw assertion-error when actual is not 0.

```lisp
(zero-p 0) => t
(zero-p 1) => throw assertion-error
(zero-p '()) => throw assertion-error
(zerot-p nil) => throw assertion-error
```

## (not-zero-p actual)

Check actual not eq 0. Return t when actual is not 0, and throw assertion-error when actual is 0.

```lisp
(not-zero-p 0) => throw assertion-error
(not-zero-p 1) => t
(not-zero-p '()) => t
(not-zerot-p nil) => t
```

## (nil-p actual)

Check actual eq nil. Return t when actual is nil, and throw assertion-error when actual is not nil.

```lisp
(nil-p nil) => t
(nil-p 1) => throw assertion-error
(nil-p '()) => t
(nil-p 0) => throw assertion-error
```

## (not-nil-p actual)

Check actual not eq nil. Return t when actual is not nil, and throw assertion-error when actual is nil.

```lisp
(not-nil-p nil) => throw assertion-error
(not-nil-p 1) => t
(not-nil-p '()) => throw assertion-error
(not-nil-p 0) => t
```

## (null-p actual)

Check null actual. Equivalent to (null actual). Return t when actual is null, and throw assertion-error when actual is not null.

```lisp
(null-p nil) => t
(null-p 1) => throw assertion-error
(null-p '()) => t
(null-p 0) => throw assertion-error
```

## (not-null-p actual)

Check not null actual. Equivalent to (not (null actual)). Return t when actual is not null, and throw assertion-error when actual is null.

```lisp
(not-null-p nil) => throw assertion-error
(not-null-p 1) => t
(not-null-p '()) => throw assertion-error
(not-null-p 0) => t
```

## (eq-p actual expected)

Check actual eq expected. Return t when actual is eq expected, and throw assertion-error when actual is not eq expected.

```lisp
(eq-p nil nil) => t
(eq-p 1 3) => throw assertion-error
(eq-p 1 1) => t ; Depending on the implementation it may return T or NIL.
(eq-p nil '())) => t
(eq-p :a :a) => t
(eq-p t nil) => throw assertion-error
```

## (not-eq-p actual expected)

Check actual not eq expected. Return t when actual is not eq expected, and throw assertion-error when actual is eq expected.

```lisp
(not-eq-p nil nil) => throw assertion-error
(not-eq-p nil '())) => throw assertion-error
(not-eq-p :a :a) => throw assertion-errort
(not-eq-p t nil) => t
```

## (eql-p actual expected)

Check actual eql expected. Return t when actual is eql expected, and throw assertion-error when actual is not eql expected.

```lisp
(eql-p nil nil) => t
(eql-p 1 3) => throw assertion-error
(eql-p 1 1) => t
(eql-p nil '())) => t
(eql-p :a :a) => t
(eql-p t nil) => throw assertion-error
```

## (not-eql-p actual expected)

Check actual not eql expected. Return t when actual is not eql expected, and throw assertion-error when actual is not expected.

```lisp
(not-eql-p nil nil) => throw assertion-error
(not-eql-p 1 3) => t
(not-eql-p 1 1) => throw assertion-error
(not-eql-p nil '())) => throw assertion-error
(not-eql-p :a :a) => throw assertion-error
(not-eql-p t nil) => t
```

## (equal-p actual expected)

Check actual equal expected. Return t when actual is equal expected, and throw assertion-error when actual is not equal expected.

```lisp
(equal-p nil nil) => t
(equal-p 1 3) => throw assertion-error
(equal-p 1 1) => t
(equal-p nil '())) => t
(equal-p :a :a) => t
(equal-p t nil) => throw assertion-error
(equal-p '(1 2 3) '(1 2 3)) => t
(not-equal-p "a" "A") => throw assertion-error
```

## (not-equal-p actual expected)

Check actual not equal expected. Return t when actual is not equal expected, and throw assertion-error when actual is equal expected.

```lisp
(not-equal-p nil nil) => throw assertion-error
(not-equal-p 1 3) => t
(not-equal-p 1 1) => throw assertion-error
(not-equal-p nil '())) => throw assertion-error
(not-equal-p :a :a) => throw assertion-error
(not-equal-p t nil) => t
(not-equal-p '(1 2 3) '(1 2 3)) => throw assertion-error
(not-equal-p "a" "A") => t
```

## (equalp-p actual expected)

Check actual equalp expected. Return t when actual is equalp expected, and throw assertion-error when actual is not equalp expected.

```lisp
(equalp-p nil nil) => t
(equalp-p 1 3) => throw assertion-error
(equalp-p 1 1) => t
(equalp-p nil '())) => t
(equalp-p :a :a) => t
(equalp-p t nil) => throw assertion-error
(equalp-p '(1 2 3) '(1 2 3)) => t
(equalp-p "a" "A") => t
```

## (not-equalp-p actual expected)

Check actual not equalp expected. Return t when actual is not equalp expected, and throw assertion-error when actual is equalp expected.

```lisp
(not-equalp-p nil nil) => throw assertion-error
(not-equalp-p 1 3) => t
(not-equalp-p 1 1) => throw assertion-error
(not-equalp-p nil '())) => throw assertion-error
(not-equalp-p :a :a) => throw assertion-error
(not-equalp-p t nil) => t
(not-equalp-p '(1 2 3) '(1 2 3)) => throw assertion-error
(not-equalp-p "a" "A") => throw assertion-error
```

## (typep-p actual expected)

Check actual typep expected. Return t when actual is typep expected, and throw assertion-error when actual is not typep expected.

```lisp
(typep-p 1 'integer) => t
(typep-p "a" 'string) => t
(typep-p :a 'symbol) => t
(typep-p nil 'integer)) => throw assertion-error
(typep-p nil 'symbol) => t
```

## (not-typep-p actual expected)

Check actual not typep expected. Return t when actual is not typep expected, and throw assertion-error when actual is typep expected.

```lisp
(not-typep-p 1 'integer) => throw assertion-error
(not-typep-p "a" 'string) => throw assertion-error
(not-typep-p :a 'symbol) => throw assertion-error
(not-typep-p nil 'integer)) => t
(not-typep-p nil 'symbol) => throw assertion-error
```

## (values-p predicate actual expected)

Check actual every predicate expected. Predicate is a symbol of function, actual is a list, and expected an atom. Example: `(values-p '= '(t t t) t)` It's similar to `(every #'(lambda (i) (= t i)) '(t t t))`. Return t when actual values is <predicate> expected, and throw assertion-error when actual values is not <predicate> expected.

```lisp
(values-p '= '(1 1 1) 1) => t
(values-p 'string= '("a" "a" "a") "a") => t
(values-p 'eq '(t t t) t)) => t
(values-p 'eq '(t nil t) t)) => throw assertion-error
(values-p 'eq '(nil nil nil) t)) => throw assertion-error
```

## (not-values-p predicate actual expected)

Check actual every not predicate expected. Predicate is a symbol of function, actual is a list, and expected an atom. Example: `(values-p '= '(t t t) t)` It's similar to `(every #'(lambda (i) (not (= t i))) '(t t t))`. Return t when actual values is not <predicate> expected, and throw assertion-error when actual values is <predicate> expected.

```lisp
(not-values-p '= '(1 1 1) 1) => throw assertion-error
(not-values-p 'string= '("a" "a" "a") "a") => throw assertion-error
(not-values-p 'eq '(t t t) t)) => throw assertion-error
(not-values-p 'eq '(t nil t) t)) => throw assertion-error
(not-values-p 'eq '(nil nil nil) t)) => t
```

## (error-p fn)

Check if fn throw an error. Return t when fn throw error, and throw assertion-error when fn not throw error.

```lisp
(error-p #'(lambda () (error "Something"))) => t
(error-p #'(lambda () ())) => throw assertion-error
```

## (not-error-p fn)

Check if fn not throw an error. Return t when fn not throw error, and throw assertion-error when fn throw error.

```lisp
(not-error-p #'(lambda () (error "Something"))) => throw assertion-error
(not-error-p #'(lambda () ())) => t
```

## (condition-error-p fn condition)

Check if fn throw an specified condition error. Return t when fn throw specified condition error, and throw assertion-error when fn not throw specified condition error.

```lisp
(condition-error-p #'(lambda () (error 'division-by-zero))
                   division-by-zero) => t
(condition-error-p #'(lambda () ())
                   division-by-zero) => throw assertion-error
(condition-error-p #'(lambda () (error "Something"))
                   division-by-zero) => throw assertion-error
```

## (not-condition-error-p fn condition)

Check if fn not throw an specified condition error. Return t when fn not throw specified condition error, and throw assertion-error when fn throw specified condition error.

```lisp
(not-condition-error-p #'(lambda () (error 'division-by-zero))
                   division-by-zero) => throw assertion-error
(not-condition-error-p #'(lambda () ())
                   division-by-zero) => t
(not-condition-error-p #'(lambda () (error "Something"))
                   division-by-zero) => t
```

## (custom-p test-result actual expected operator)

Custom check for special cases. Where test-result is a form that returns t or nil, actual is value actual used in test-result, expected is value expected used in test-result, operator is operator used in test-result. Return t when test-result is t, and throw assertion-error when test-result is nil.

```lisp
(custom-p (every #'eq '(:a :b :c) '(:a :b))
          '(:a :b :c)
          '(:a :b)
          "every =") => t
(custom-p (every #'eq '(:a :b :c) '(:a :b :z))
          '(:a :b :c)
          '(:a :b)
          "string=") => throw assertion-error
(custom-p (string= "I exist?" "I exist?")
          "I exist?"
          "I exist?"
          "string=") => t
(custom-p (string= "I exist?" "I'm not sure.")
          "I exist?"
          "I'm not sure."
          "string=") => throw assertion-error
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

function **(error-p fn)** => t or throw assertion-error

function **(not-error-p fn)** => t or throw assertion-error

macro **(condition-error-p fn condition)** => t or throw assertion-error

macro **(not-condition-error-p fn condition)** => t or throw assertion-error

function **(custom-p test-result actual expected operator)** => t or throw assertion-error

### LICENSE

Copyright (C) 2019 noloop

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.

Contact author:

noloop@zoho.com

