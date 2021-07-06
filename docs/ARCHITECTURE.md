# 📐 ARCHITECTURE

We have Spaghetti ARCHITECTURE inspired by Flying Spaghetti Monster

Tools and libraries:
* For start:
  * [x] [Summoner](https://kowainik.github.io/projects/summoner) for scaffolding fully configured batteries-included production-level Haskell projects
  * [ ] [cake-slayer](https://github.com/kowainik/cake-slayer) - Architecture of Haskell backend applications
  * [ ] [three-layer](https://github.com/Holmusk/three-layer) - Architecture of Haskell backend applications
* For static code analysis:
  * [x] [hlint](https://github.com/ndmitchell/hlint) for source code suggestions
  * [x] [stan](https://kowainik.github.io/projects/stan) for STatic ANalysis
  * [ ] [weeder](https://github.com/ocharles/weeder) for detect dead code
* For dynamic code analysis:
  * [x] [hspec](https://hspec.github.io/) for unit and integration tests
    * [x] [hspec-expectations-pretty-diff](https://github.com/unrelentingtech/hspec-expectations-pretty-diff#readme) for catchy combinators for HUnit
    * [x] [hspec-golden](https://github.com/stackbuilders/hspec-golden#readme) for golden tests
    * [x] [hspec-slow](https://github.com/SupercedeTech/hspec-slow) for find slow test cases
  * [ ] [gauge](https://github.com/vincenthz/hs-gauge) for performance measurement and analysis
  * [ ] [weigh](https://github.com/fpco/weigh#readme) for measure allocations of a Haskell functions/values
* For configuration:
  * [x] [optparse-applicative](https://github.com/pcapriotti/optparse-applicative) for parse options
  * [ ] [tomland](https://kowainik.github.io/posts/2019-01-14-tomland) for configuration from file
  * [ ] [dhall-haskell](https://github.com/dhall-lang/dhall-haskell) for configuration from file
* For patching List:
  * [x] [ilist](https://hackage.haskell.org/package/ilist) for doing index-related things
  * [x] [list-singleton](https://hackage.haskell.org/package/list-singleton) for easily and clearly create lists with only one element in them
  * [ ] [slist](https://kowainik.github.io/projects/slist) for sized list
  * [x] [split](https://hackage.haskell.org/package/split) for splitting lists
* For production code:
  * [x] [mtl](https://github.com/haskell/mtl) for IoC
  * [x] [relude](https://kowainik.github.io/projects/relude) as standard library
  * [ ] [co-log](https://github.com/kowainik/co-log) for logging
  * [ ] [mono-traversable](https://github.com/snoyberg/mono-traversable#readme) as container standard library
  * [x] [data-default](https://hackage.haskell.org/package/data-default) for default values
  * [ ] [validation-selective](https://github.com/kowainik/validation-selective) is lighweight pure data validation based on Applicative and Selective functors
  * [x] [filepath](https://github.com/haskell/filepath#readme) for manipulating FilePaths in a cross platform way.
  * [x] [pretty-simple](https://github.com/cdepillabout/pretty-simple) for data types with a 'Show' instance
