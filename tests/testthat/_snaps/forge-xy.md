# novel predictor levels are caught

    Code
      xx <- forge(new, x$blueprint)
    Condition
      Warning:
      Novel levels found in column 'f': 'e'. The levels have been removed, and values have been coerced to 'NA'.

# novel predictor levels can be ignored

    Code
      xx <- forge(new, x$blueprint)

# novel outcome levels are caught

    Code
      xx1 <- forge(new, x1$blueprint, outcomes = TRUE)
    Condition
      Warning:
      Novel levels found in column 'f': 'e'. The levels have been removed, and values have been coerced to 'NA'.

---

    Code
      xx2 <- forge(new, x2$blueprint, outcomes = TRUE)
    Condition
      Warning:
      Novel levels found in column 'f': 'e'. The levels have been removed, and values have been coerced to 'NA'.

