module Tests.Evaluation where

import Logic.SecPAL.Language
import Logic.SecPAL.Pretty
import Logic.SecPAL.Evaluable
import Logic.SecPAL.Context
import Tests.Testable

-- Hahahahahahah! This is crazy! I'm actually trying this!
--
-- Some entities to save typing
alice = Constant "Alice"
bob = Constant "Bob"

isCool x = Fact{subject=x, verb=Predicate{predicate="is-cool", args=[]}}
likesJazz x = Fact{subject=x, verb=Predicate{predicate="likes-jazz", args=[]}}


testEvaluationTruths = [inACTest1, condNoRename1]
testEvaluationFalsehoods = [falseInACTest1, falseCondNoRename1]

-- An assertion is true if it is in the assertion context
inACTest1 = 
  let 
    a = Assertion { who=alice
                  , says=Claim{ fact=isCool bob
                              , conditions=[]
                              , constraint=Boolean True 
                              }
                  }
    ctx = Context { ac=AC [a], d=Infinity }
  in
    Test{ description = pShow ctx ++" |= "++pShow a
        , result = test $ ctx ||- a
        }

falseInACTest1 = 
  let 
    a = Assertion { who=alice
                  , says=Claim{ fact=isCool alice
                              , conditions=[]
                              , constraint=Boolean True 
                              }
                  }
    b = Assertion { who=alice
                  , says=Claim{ fact=isCool bob
                              , conditions=[]
                              , constraint=Boolean True 
                              }
                  }
    ctx = Context { ac=AC [a], d=Infinity }
  in
    Test{ description = pShow ctx ++" |= "++pShow b
        , result = test . not $ ctx ||- b
        }

-- Can we use the cond variable without renaming
condNoRename1 =
  let 
    a = Assertion { who=alice
                  , says=Claim{ fact=isCool bob
                              , conditions=[]
                              , constraint=Boolean True 
                              }
                  }
    a' = Assertion { who=alice
                   , says=Claim{ fact=isCool bob
                               , conditions=[likesJazz bob]
                               , constraint=Boolean True
                               }
                   }
    b = Assertion { who=alice
                  , says=Claim{ fact=likesJazz bob
                              , conditions=[]
                              , constraint=Boolean True
                              }
                  }
    ctx = Context { ac=AC [a', b], d=Infinity }
  in
    Test{ description = pShow ctx ++" |= "++pShow a
        , result = test $ ctx ||- a
        }
        --

falseCondNoRename1 =
  let 
    a = Assertion { who=alice
                  , says=Claim{ fact=isCool bob
                              , conditions=[]
                              , constraint=Boolean True 
                              }
                  }
    a' = Assertion { who=alice
                   , says=Claim{ fact=isCool bob
                               , conditions=[likesJazz bob]
                               , constraint=Boolean True
                               }
                   }
    b = Assertion { who=alice
                  , says=Claim{ fact=likesJazz bob
                              , conditions=[]
                              , constraint=Boolean False
                              }
                  }
    ctx = Context { ac=AC [a', b], d=Infinity }
  in
    Test{ description = pShow ctx ++" |= "++pShow a
        , result = test . not $ ctx ||- a
        }
