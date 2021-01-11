# Weapon regeX
[![Mutation testing badge](https://img.shields.io/endpoint?style=flat&url=https%3A%2F%2Fbadge-api.stryker-mutator.io%2Fgithub.com%2FNhaajt%2FWeapon-regeX%2Fmain)](https://dashboard.stryker-mutator.io/reports/github.com/Nhaajt/Weapon-regeX/main)
[![Build Status](https://github.com/Nhaajt/Weapon-regeX/workflows/Scala%20CI/badge.svg)](https://github.com/Nhaajt/Weapon-regeX/actions?query=workflow%3AScala%20CI+branch%3Amain)

<img src="images/WeaponRegeX_logo.svg" width="50%" alt="Weapon regeX Logo">

With Weapon regeX you can mutate regular expressions which can be used in mutation testing. The 
generated regular expressions cover edge cases and typos. Weapon regeX is available for both
Javascript and Scala. The Javascript version of the library is generated from Scala using [ScalaJS](https://www.scala-js.org/).

# Getting started

## Scala
Add Weapon regeX to your ```build.sbt```.
```Scala
"io.stryker-mutator" %% "weapon-regex" % "0.1.2"
```

Mutate!

```Scala
import weaponregex.WeaponRegeX

val mutants = WeaponRegeX.mutate("^abc(d+|[xyz])$")

mutants.map(mutant => println(mutant.pattern))
```

## Javascript

Install Weapon regeX with npm.

```bash
npm install weapon-regex
```

Mutate!

```Javascript
const wrx = require('weapon-regex');

var mutants = wrx.mutate("^abc(d+|[xyz])$");

mutants.forEach(mutant => {
  console.log(mutant.pattern);
});
```

# API
## Scala

The ```mutate``` function has the following signature:

```scala
def mutate(
      pattern: String,
      mutators: Seq[TokenMutator] = BuiltinMutators.all,
      mutationLevels: Seq[Int] = null
  ): Seq[Mutant]
```
The with the ```mutators``` argument you can give a select list of mutators that should be used in
the mutation process. If omitted, all builtin mutators will be used. This list will be filtered
depending on the ```mutationLevels``` argument.

A list of ```mutationLevels``` can also be passed to the function. The mutators will be filtered
based on the levels in the list. If omitted, no filtering takes place.

## Javascript

The ```mutate``` function can be called with an options object to control which mutators should be
used in the mutation process:

```Javascript
const wrx = require('weapon-regex');

val mutants = wrx.mutate("^abc(d+|[xyz])$",{
  mutators: Array.from(wrx.mutators.values()),
  mutationLevels: [1, 2, 3]
});
```

Both options can be omitted, and have the same functionality as the options described in the Scala
API section. You can get a map of mutators from the ```mutators``` attribute of the library. It is
a map from string (mutator name) to a mutator object.

# Supported mutators
Name | Levels
--- | ---
BOLRemoval | 1, 2, 3
EOLRemoval | 1, 2, 3
BOL2BOI | 2, 3
EOL2EOI | 2, 3
CharClassNegation | 1
CharClassChildRemoval | 2, 3
CharClassAnyChar | 2, 3
CharClassRangeModification | 3
PredefCharClassNegation | 1
PredefCharClassNullification | 2, 3
PredefCharClassAnyChar | 2, 3
QuantifierRemoval | 1
QuantifierNChange | 2, 3
QuantifierNOrMoreModification | 2, 3
QuantifierNOrMoreChange | 2, 3
QuantifierNMModification | 2, 3
QuantifierShortModification | 2, 3
QuantifierShortChange | 2, 3
QuantifierReluctantAddition | 3
GroupToNCGroup | 2, 3
