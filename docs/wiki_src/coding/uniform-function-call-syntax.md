# Uniform function call syntax (UFCS)

UFCS is a syntactic feature that allows you to use functions in a
different (perhaps more "natural") order.

By using the dot `.` between two terms, the first term will be applied
to the second term instead of the other way around.

Example:

``` bruijn
:test ("abc".length) (length "abc")
:test ("abc".length.inc) ((+4))
```
