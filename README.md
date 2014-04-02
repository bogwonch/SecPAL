SecPAL
======

The SecPAL authorization logic ported to Haskell for the App Guarden project at Edinburgh University.

# Building

~~~{.sh}
cabal build secpal
~~~

This is very software under heavy development and I would *strongly* recommend running it in a sandbox.

~~~{.sh}
cabal sandbox init
cabal sandbox install --only-dependencies
cabal build
~~~

# Running 

To run either find the built binary and execute as normal or use cabal (recommended for the minute)

    $ cabal run secpal -- -h
    Usage: secpal -f FILE [options]
      -f FILE  --file=FILE  assertion context file
      -d       --debug      debug (unimplemented)
      -h       --help       show this message
      -v       --verbose    be more chatty

To see the proofs you need to enable verbose mode.
For instance:

    $ cabal run secpal -- -f AC.policy
    Loaded assertion context.
    Enter query or :h to see the help
    ? Phone says Game is-installable.
    ! Yes.
    ? :verbose
    ? Phone says Game is-installable.
    AC := { Phone says app is-installable() if app meets(NotMalware), app meets(NoInfoLeaks)., anyone says app meets(policy) if evidence shows-meets(app, policy)., Phone says NILInferer can-say 0 app meets(NoInfoLeaks)., Phone says Google can-say inf app meets(NotMalware)., Google says AVChecker can-say 0 app meets(NotMalware)., AVChecker says Game meets(NotMalware)., NILInferer says Evidence shows-meets(Game, NoInfoLeaks). }
    AC, inf [(app := Game)] |= Phone says Game is-installable().
      AC, inf [(app := Game)] |= Phone says Game meets(NotMalware).
        AC, inf [(app := Game)] |= Phone says Google can-say inf app meets(NotMalware).
        -------------------------------------------------------------------------------
        AC, inf [(app := Game)] |= Google says Game meets(NotMalware).
          AC, inf [(app := Game)] |= Google says AVChecker can-say 0 app meets(NotMalware).
          ---------------------------------------------------------------------------------
          AC, 0 |= AVChecker says Game meets(NotMalware).
          -----------------------------------------------
      AC, inf [(app := Game)] |= Phone says Game meets(NoInfoLeaks).
        AC, inf [(app := Game)] |= Phone says NILInferer can-say 0 app meets(NoInfoLeaks).
        ----------------------------------------------------------------------------------
        AC, 0 [(anyone := NILInferer), (app := Game), (policy := NoInfoLeaks)] |= NILInferer says Game meets(NoInfoLeaks).
          AC, 0 [(evidence := Evidence)] |= NILInferer says Evidence shows-meets(Game, NoInfoLeaks).
          ------------------------------------------------------------------------------------------
          AC, 0 |= True
          -------------
      AC, inf |= True
      ---------------


## Tests

To run the (minimal) test-suite:

~~~{.sh}
cabal run tests
~~~

Everything should have green ticks.

