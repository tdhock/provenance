works_with_R("3.2.2", 
             data.table="1.9.6")

prices <- fread("products/hemp_seeds.csv")

## From Wikipedia Pound (mass): The pound or pound-mass
## (abbreviations: lb, lbm, lbm, ℔[1]) is a unit of mass used in the
## imperial, United States customary and other systems of
## measurement. A number of different definitions have been used, the
## most common today being the international avoirdupois pound which
## is legally defined as exactly 0.45359237 kilograms, and which is
## divided into 16 avoirdupois ounces.
kilograms.per.unit <- c(
  lb=0.45359237,
  kg=1,
  g=0.001
  )

prices[, kg := kilograms.per.unit[unit] * quantity]
prices[, CAD.per.kg := CAD/kg]
print(prices[order(CAD.per.kg),.(store,manufacturer,kg,CAD.per.kg)])

unique(prices$store)
