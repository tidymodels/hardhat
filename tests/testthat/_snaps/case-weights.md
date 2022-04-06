# importance_weights() doesn't allow negative weights

    Code
      importance_weights(-1)
    Condition
      Error in `importance_weights()`:
      ! `x` can't contain negative weights.

# importance-weights constructor checks for double data

    Code
      new_importance_weights(1L)
    Condition
      Error in `new_importance_weights()`:
      ! `x` must be a double vector.

# can't cast importance-weights -> integer (too lenient, likely fractional weights)

    Code
      vec_cast(x, integer())
    Condition
      Error:
      ! Can't convert `x` <importance_weights> to <integer>.

# as.integer() fails (too lenient, likely fractional weights)

    Code
      as.integer(x)
    Condition
      Error in `as.integer()`:
      ! Can't convert `x` <importance_weights> to <integer>.

# frequency_weights() coerces to integer

    Code
      frequency_weights(1.5)
    Condition
      Error in `frequency_weights()`:
      ! Can't convert from `x` <double> to <integer> due to loss of precision.
      * Locations: 1

# frequency_weights() doesn't allow negative weights

    Code
      frequency_weights(-1L)
    Condition
      Error in `frequency_weights()`:
      ! `x` can't contain negative weights.

# frequency-weights constructor checks for integer data

    Code
      new_frequency_weights(1)
    Condition
      Error in `new_frequency_weights()`:
      ! `x` must be an integer vector.

# can't cast frequency-weights -> double (too lenient)

    Code
      vec_cast(x, double())
    Condition
      Error:
      ! Can't convert `x` <frequency_weights> to <double>.

# as.double() fails (too lenient)

    Code
      as.double(x)
    Condition
      Error in `as.double()`:
      ! Can't convert `x` <frequency_weights> to <double>.

