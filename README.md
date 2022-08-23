# binaries
Arithmetic system of 2-adic numbers in scheme language

Run with Chez Scheme



2-adic numbers are represented by infinite streams in `binaries`. All arithmetic operations provided are accurate.



## Construction and display

* `binaries-zero`:

    Zero.

* `binaries-one`:

    One.

* `binaries-minus-one`:

    Minus-one.

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

Construct and display a rational number ⁷/₁₃.

```
> (display-binaries (r-binaries 7/13) -50)
.36726726726726726726726726726726726726726726726726...
```

Construct and display an infinite series

2+3×16+5×16²+7×16³+11×16⁴+⋯+P(n+1)×16ⁿ+⋯

where P(k) is the k-th prime.

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



## License
[WTFPL](http://www.wtfpl.net/txt/copying)
