COMMENT propagation des constantes (conditions)

READ b

IF + 0 b <> b
  PRINT b

IF * 1 / 4 2 <> b
  PRINT b

IF / 0 b <> b
  PRINT b

IF * 0 b <> b
  PRINT b

IF / b 0 <> b
  PRINT b

IF % b 0 <> b
  PRINT b

IF / * 0 b 0 <> b
  PRINT b

IF % + 3 7 0 <> b
  PRINT b

IF * / b 0 0 <> b
  PRINT b

IF * / b 0 1 <> b
  PRINT b
