# binaries
Arithmetic system of 2-adic numbers in scheme language

2-adic numbers are represented by infinite streams in `binaries`. All arithmetic operations provided are accurate.

Run with Chez Scheme

## Construction and display

* `binaries-zero`:

    2-adic zero.

* `binaries-one`:

    2-adic one.

* `binaries-minus-one`:

    2-adic minus-one.

* `(r-binaries r)`:

    Construct a 2-adic number from a rational number `r`.

* `(h-binaries h)`:

    Construct a 2-adic number from an infinite stream of hexes `h`.

* `(display-binaries b n)`:

    Display a 2-adic number `b` by hex digits.

    If `n` is a positive integer or `#t`, scientific notation is enabled, and `n` hex digits or infinite digits will be displayed.

    If `n` is a non-positive integer or `#f`, the fractional part will be displayed in full and the integer part will show `n` or infinite digits.

    If `n` is omitted, it will be regarded as `#f`.

#### Example

Construct and display a rational number `⁷/₁₃`.
```
> (display-binaries (r-binaries 7/13) -50)
.36726726726726726726726726726726726726726726726726...
```

Construct an infinite series

`2 + 3 × 16 + 5 × 16² + 7 × 16³ + 11 × 16⁴ + ⋯ + P(n+1) × 16ⁿ + ⋯`

where `P(k)` is the `k`-th prime. Display in scientific notation.

```
> (define primes
    (letrec ([integers (stream-cons 3
                         (stream+ (ns 1) integers))])
      (stream-cons 2
        (stream-filter
          (lambda (n)
            (let ([m (isqrt n)])
              (let loop ([ps primes])
                (cond [(> (stream-car ps) m) #t]
                      [(zero? (remainder n (stream-car ps))) #f]
                      [else (loop (stream-cdr ps))]))))
          integers))))
> (define binaries-primes (h-binaries ($stream-carry primes)))
> (display-binaries binaries-primes 50)
[1]99ABDE02478BDE0478BDE147BDE024BD0278BE0478DE028E02...
```

## Basic arithmetic operations and potential zero

* `(binaries+ b1 b2 ... bn)`:

    Construct a 2-adic number `b1 + b2 + ⋯ + bn`.

* `(binaries- b1 b2)`:

    Construct a 2-adic number `b1 - b2`.

    If `b1` is omitted, it will be regarded as zero.

* `(binaries* b1 b2)`:

    Construct a 2-adic number `b1 × b2`.

* `(binaries*r b r)`:

    Construct a 2-adic number `b × r`, where `r` is a rational number.

* `(binaries-shift b n)`:

    Construct a 2-adic number `b × 2ⁿ`, where `n` is an integer.

* `(binaries/ b1 b2)`:

    Construct a 2-adic number `b1 / b2`.

    If `b1` is omitted, it will be regarded as one.

Since all the results are represented by infinite streams, a 2-adic number that is theoretically zero cannot be calculated to be zero. In `binaries`, if a certain number of zero digits are detected, it will be judged as a **potential zero**. Functions below are provided for dealing with potential zeros.

* `(binaries-zero? b)`:

    Test if a 2-adic number `b` is zero, non-zero or potential zero.

    If `b` is zero, it will return `#t`; if `b` is non-zero, it will return `#f`.

    If `b` is a potential zero, it will return an integer `n` indicating that `b` is divisible by `2ⁿ`.

* `(binaries-eval b n)`:

    Evaluate a potential zero to the given binary bits `n` until it be determined to be non-zero.

    It will return `#t` if `b` is evaluated to be non-zero, `#f` else.

    If `n` is omitted, then there is no limit to the number of bits in the evaluation before determination. Any theoretical zero number will result in endless computations.

Potential zeros cannot be used as a divisor.

#### Example

Ordinary operations may yield potential zeros:

```
> (define pot-zero-0 (binaries- one-binaries one-binaries))
> (binaries-zero? pot-zero-0)
200
```

A construction from infinite stream may also yield potential zeros:

```
> (define pot-zero-1 (h-binaries (hexes-shift one-hexes 200)))
> (binaries-zero? pot-zero-1)
200
```

A shift operation will not change the state of zero:

```
> (define non-zero-small (binaries-shift one-binaries 800))
> (binaries-zero? non-zero-small)
#f
> (binaries-zero? (binaries-shift pot-zero-1 -800))
-600
> (binaries-zero? (binaries-shift zero-binaries -800))
#t
```

Addition of multiple parameters is recommended, as this will reduce the possibility of potential zeros appearing:

```
> (define one-plus-small (binaries+ one-binaries non-zero-small))
> (define pot-zero-2 (binaries- one-plus-small one-binaries))
> (binaries-zero? pot-zero-2)
200
> (binaries-zero? (binaries+ one-binaries one-plus-small minus-one-binaries))
#f
```

We can see that three numbers `pot-zero-1`, `non-zero-small` and `pot-zero-2` are theoretically equal. Do some more calculations involving potential zeros:

```
> (define pot-zero-3 (binaries+ pot-zero-1 pot-zero-2))
> (binaries-zero? pot-zero-3)
200
> (binaries-zero? (binaries+ non-zero-small pot-zero-2))
#f
> (binaries-zero? (binaries- non-zero-small pot-zero-2))
1000
> (define pot-zero-4 (binaries* pot-zero-3 pot-zero-3))
> (binaries-zero? pot-zero-4)
400
> (binaries-eval pot-zero-4 1000)
#f
> (binaries-eval pot-zero-4)
#t
> (display-binaries pot-zero-4 50)
[1602]10000000000000000000000000000000000000000000000000...
> (display-binaries (binaries/ pot-zero-3 non-zero-small) -50)
.20000000000000000000000000000000000000000000000000...
> (display-binaries (binaries/ non-zero-small pot-zero-3) -50)
Exception in binaries/: Division by potential zero.
> (binaries-eval pot-zero-3)
#t
> (display-binaries (binaries/ non-zero-small pot-zero-3) -50)
8.00000000000000000000000000000000000000000000000000...
```

## Advanced arithmetic operations

* `(binaries-square b)`:

    Construct a 2-adic number `b²`.

* `(binaries-expt b n)`:

    Construct a 2-adic number `bⁿ`, where `n` is an integer.

* `(binaries-sqrt? b)`:

    It will return `#t` if `b` is zero or non-zero that has square roots, `#f` else.

* `(binaries-sqrt b index)`:

    Construct a 2-adic number `√b`. If integer `index` is even, then the odd part in the scientific notation of result `≡ 1 mod 4` , else `≡ 3 mod 4`.

    If `index` is omitted, it will be regarded as `0`.

* `(binaries-root? b n)`:

    It will return `#t` if `b` is zero or non-zero that has `n`-th roots, `#f` else.

* `(binaries-root b n index)`:

    Construct a 2-adic number `ⁿ√b`, where `n` is a non-zero integer. In the case that `n` is even, if integer `index` is even, then the odd part in the scientific notation of result `≡ 1 mod 4` , else `≡ 3 mod 4`.

    If `index` is omitted, it will be regarded as `0`.

* `(binaries-exp b)`:

    Construct a 2-adic number `exp(b)`. It should hold that `b ≡ 0 mod 4`.

* `(binaries-log b)`:

    Construct a 2-adic number `log(b)`. It should hold that `b ≡ 1 mod 2`.

* `(binaries-pow b1 b2)`:

    Construct a 2-adic number `pow(b1, b2)`. It is defined to be `exp(b2 × log(b1))`.

* `(binaries-sin b)`:

    Construct a 2-adic number `sin(b)`. It should hold that `b ≡ 0 mod 4`.

* `(binaries-cos b)`:

    Construct a 2-adic number `cos(b)`. It should hold that `b ≡ 0 mod 4`.

* `(binaries-tan b)`:

    Construct a 2-adic number `tan(b)`. It should hold that `b ≡ 0 mod 2`.

* `(binaries-asin b)`:

    Construct a 2-adic number `arcsin(b)`. It should hold that `b ≡ 0 mod 4`.

* `(binaries-acos b index)`:

    Construct a 2-adic number `arccos(b)`. It should hold that `b ≡ 1 mod 8` and that `1 - b²` has square roots. If integer `index` is even, then the odd part in the scientific notation of result `≡ 1 mod 4` , else `≡ 3 mod 4`.

    If `index` is omitted, it will be regarded as `0`.

* `(binaries-atan b)`:

    Construct a 2-adic number `arctan(b)`. It should hold that `b ≡ 0 mod 2`.

* `(binaries-sinh b)`:

    Construct a 2-adic number `sinh(b)`. It should hold that `b ≡ 0 mod 4`.

* `(binaries-cosh b)`:

    Construct a 2-adic number `cosh(b)`. It should hold that `b ≡ 0 mod 4`.

* `(binaries-tanh b)`:

    Construct a 2-adic number `tanh(b)`. It should hold that `b ≡ 0 mod 2`.

* `(binaries-asinh b)`:

    Construct a 2-adic number `arcsinh(b)`. It should hold that `b ≡ 0 mod 4`.

* `(binaries-acosh b index)`:

    Construct a 2-adic number `arccosh(b)`. It should hold that `b ≡ 1 mod 8` and that `b² - 1` has square roots. If integer `index` is even, then the odd part in the scientific notation of result `≡ 1 mod 4` , else `≡ 3 mod 4`.

    If `index` is omitted, it will be regarded as `0`.

* `(binaries-atanh b)`:

    Construct a 2-adic number `arctanh(b)`. It should hold that `b ≡ 0 mod 2`.

#### Example

The following code constructs a number `α` satisfying `α⁻²⁸ + 15 = 0` and verifies that `α²³ - α⁹ / √-15 = 0`.

```
> (define r-15 (r-binaries -15))
> (define sqrt-15 (binaries-sqrt r-15))
> (display-binaries sqrt-15 -50)
.9D44EED7085922F66E2026FB6C24DF22C0523BCA43D8A7B7E5...
> (define alpha (binaries-root r-15 -28))
> (display-binaries alpha -50)
.540CE8D70746CCB36DD9D74161E23D98177655CEA52763E19A...
> (display-binaries
    (binaries-
      (binaries-expt alpha 23)
      (binaries/
        (binaries-expt alpha 9)
        sqrt-15))
    -50)
.00000000000000000000000000000000000000000000000000...
```

The following code calculates `((43 √-15 + 26 √41)² + 2⁴ + 1 + 1 + 1)⁶`.

```
> (define r+41 (r-binaries 41))
> (define sqrt+41 (binaries-sqrt r+41))
> (display-binaries sqrt+41 -50)
.DC66CC00EFB622B945B363D9C09AC784C104795047307E7C73...
> (define beta (binaries+
                 (binaries*r sqrt-15 43)
                 (binaries*r sqrt+41 26)))
> (display-binaries beta -50)
.54108CB31641E4C6C4388FC1AA2633D39EBB37391A11C0B098...
> (define x (binaries-expt
              (binaries+
                (binaries-square beta)
                (binaries-shift one-binaries 4)
                one-binaries one-binaries one-binaries)
              6))
> (display-binaries x -50)
.000966C0B552B5CC09E3112AFFFFFFFFFFFFFFFFFFFFFFFFFF...
```

From the last output we can guess that the result is an integer. So if we don't limit the number of digits for display, the result will be

```
> (display-binaries x)
.000966C0B552B5CC09E3112AF(3920)^C
break> 
```

The display function will detect consecutive occurrences of digit `0` or `F` in unlimited display mode. In that case, the number of `0` or `F` currently evaluated will be placed in the parenthesis and changes over time. We can press `Ctrl-C` to break the procedure.

The output indicates that the result of our expression is an hex integer `-0x5DEEC16F33A4DAA4F3997000`, that is `-29070743726494498752382464000`.

Recall that we have constructed a 2-adic number `binaries-primes` by infinite series before. Now we will calculate some trigonometric functions involving it and verify some identities.

```
> (define p (binaries+ binaries-primes (r-binaries 4)))
> (define 2p (binaries-shift p 1))
> (define sin+2p (binaries-sin 2p))
> (display-binaries sin+2p -50)
.CCCFB54B2A5E34A98B527FB5480B811129524A1EFFED9057DC...
> (define cos+2p (binaries-cos 2p))
> (display-binaries cos+2p -50)
.999550FF477BC9A04080262801F89A6ECE380027B2AFEB21FD...
> (define tan+2p (binaries-tan 2p))
> (display-binaries tan+2p -50)
.CAA54E78CB2101842651A3A8AC142FEE07749383649730BDBD...
> (define tan+p (binaries-tan p))
> (display-binaries tan+p -50)
.EF7B00122CF11254C3CF1E40CDB58E6706C5B53A7CEF7A31CF...
```

`sin(2p)² + cos(2p)² = 1`:

```
> (display-binaries (binaries+
                      (binaries-square sin+2p)
                      (binaries-square cos+2p))
                    -50)
.10000000000000000000000000000000000000000000000000...
```

`sin(2p) = cos(2p) × tan(2p)`:

```
> (display-binaries (binaries-
                      sin+2p
                      (binaries* cos+2p tan+2p))
                    -50)
.00000000000000000000000000000000000000000000000000...
```

`tan(2p) = 2 tan(p) / (1 - tan(p)²)`:

```
> (display-binaries (binaries-
                      tan+2p
                      (binaries-shift
                        (binaries/
                          tan+p
                          (binaries-
                            one-binaries
                            (binaries-square tan+p)))
                        1))
                    -50)
.00000000000000000000000000000000000000000000000000...
```

Then we will try inverse trigonometric functions:

```
> (display-binaries (binaries-asin sin+2p) -50)
.C6AE6B380D1E6B30D1E6B70DD6B380D638C1EA30D16B380A38...
> (display-binaries (binaries-acos cos+2p) -50)
.495194C7F2E194CF2E1948F2294C7F29C73E15CF2E94C7F5C7...
> (display-binaries (binaries-atan tan+2p) -50)
.C6AE6B380D1E6B30D1E6B70DD6B380D638C1EA30D16B380A38...
> (display-binaries (binaries-atan tan+p) -50)
.6357BD148E07BD18E07BD38E6BD1486B14E07D18E0BD140D14...
```

Notice that the result `arccos(cos(2p))` is `-2p`, because `p`'s odd part in the scientific notation `≡ 3 mod 4` . We can add the `index` parameter to get another result:

```
> (display-binaries (binaries-acos cos+2p 1) -50)
.C6AE6B380D1E6B30D1E6B70DD6B380D638C1EA30D16B380A38...
```



## License
[WTFPL](http://www.wtfpl.net/txt/copying)
