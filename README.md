SecPAL
======

The SecPAL authorization logic ported to Haskell for the App Guarden project at Edinburgh University.


# WARNING

**This software is buggy and no longer developed.**

**It was eventually replaced with
[AppPAL](https://github.com/bogwonch/libAppPAL).** If you're interested in
SecPAL I'd recommend checking it out. It isn't bug-free but it's generally much
more reliable.


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

## Readline and MacOS X

The Haskell readline library is a little odd and needs some gentle pursuation to build.
To get round this make sure you've installed `readline` with Homebrew; then use the following to get the library installed.

~~~{.sh}
cabal install readline --extra-include-dirs=/usr/local/opt/readline/include   \
  --extra-lib-dirs=/usr/local/opt/readline/lib                                \
  --configure-option=--with-readline-includes=/usr/local/opt/readline/include \
  --configure-option=--with-readline-libraries=/usr/local/opt/readline/lib
~~~

Then build everything else as normal.

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

    $ cabal run secpal -- -f doc/AC.policy
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

