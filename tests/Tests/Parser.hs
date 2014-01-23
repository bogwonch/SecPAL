module Tests.Parser where

import Logic.SecPAL.Language
import Logic.SecPAL.Parser
import Text.Parsec
import Tests.Testable
import Tests.TestResults

pT spp sp =
  let output = parse spp "" (show sp)
      result = case output of
                 (Left err) -> TestFailed (Just $ show err)
                 (Right sp') -> if sp' == sp
                                  then TestPassed
                                  else TestFailed (Just $ show sp')
  in
    Test{ description=show sp
        , result=result
        }

testParser = [ testPEVar
             , testPEConst
             , testPD0
             , testPDInf
             , testPVPPred0
             , testPVPPred1
             , testPVPPred2
             , testPVPCanSay0
             , testPVPCanSay1
             , testPFact0
             , testPFact1
             , testPFact2
             , testPFact3
             , testPFact4
             , testPFact5
             , testPValue0
             , testPValue1
             , testPValue2
             , testPValue3
             , testPValue4
             , testPValue5
             , testPValue6
             , testPEc0
             , testPEc1
             , testPEc2
             , testPEc3
             , testPEc4
             , testPEc5
             , testPEc6
             , testPEc7
             , testPEc8
             , testPEc9
             , testPEc10
             , testPEc11
             , testPEc12
             , testPEc13
             , testPEc14
             , testPEc15
             , testPEc16
             , testPC0
             , testPC1
             ]

spEVar = Variable "alice"
spEConst = Constant "Bob"

testPEVar = pT pE spEVar
testPEConst = pT pE spEConst

testPD0 = pT pD Zero
testPDInf = pT pD Infinity

spVPPred0 = Predicate{predicate="likes-jazz", args=[]}
spVPPred1 = Predicate{predicate="likes-jazz", args=[Variable "bebob"]}
spVPPred2 = Predicate{predicate="likes-jazz", args=[Variable "bebob", Constant "Scat"]}

testPVPPred0 = pT pVerbPhrase spVPPred0
testPVPPred1 = pT pVerbPhrase spVPPred1
testPVPPred2 = pT pVerbPhrase spVPPred2

spPVPCanSay0 = CanSay {delegation=Zero, what=spPFact0}
spPVPCanSay1 = CanSay {delegation=Infinity, what=spPFact0}

testPVPCanSay0 = pT pVerbPhrase spPVPCanSay0
testPVPCanSay1 = pT pVerbPhrase spPVPCanSay1

spPFact0 = Fact{subject=spEConst, verb=spVPPred0}
spPFact1 = Fact{subject=spEConst, verb=spVPPred1}
spPFact2 = Fact{subject=spEConst, verb=spVPPred2}
spPFact3 = Fact{subject=spEVar, verb=spVPPred0}
spPFact4 = Fact{subject=spEVar, verb=spVPPred1}
spPFact5 = Fact{subject=spEVar, verb=spVPPred2}

testPFact0 = pT pFact spPFact0
testPFact1 = pT pFact spPFact1
testPFact2 = pT pFact spPFact2
testPFact3 = pT pFact spPFact3
testPFact4 = pT pFact spPFact4
testPFact5 = pT pFact spPFact5

spPEc0 = Entity spEVar
spPEc1 = Entity spEConst
spPEc2 = Value spPValue0
spPEc3 = Value spPValue1
spPEc4 = Value spPValue2
spPEc5 = Value spPValue3
spPEc6 = Value spPValue4
spPEc7 = Value spPValue5
spPEc8 = Value spPValue6
spPEc9 = Apply (F "is-a-cylon") []
spPEc10 = Apply (F "is-a-cylon") [spPEc0]
spPEc11 = Apply (F "is-a-cylon") [spPEc1]
spPEc12 = Apply (F "is-a-cylon") [spPEc0, spPEc1]
spPEc13 = Apply (F "is-a-cylon") [spPEc4, spPEc6]
spPEc14 = Apply (F "is-a-cylon") [spPEc9]
spPEc15 = Apply (F "is-a-cylon") [spPEc10]
spPEc16 = Apply (F "is-a-cylon") [spPEc15]

testPEc0 = pT pEc spPEc0
testPEc1 = pT pEc spPEc1
testPEc2 = pT pEc spPEc2
testPEc3 = pT pEc spPEc3
testPEc4 = pT pEc spPEc4
testPEc5 = pT pEc spPEc5
testPEc6 = pT pEc spPEc6
testPEc7 = pT pEc spPEc7
testPEc8 = pT pEc spPEc8
testPEc9 = pT pEc spPEc9
testPEc10 = pT pEc spPEc10
testPEc11 = pT pEc spPEc11
testPEc12 = pT pEc spPEc12
testPEc13 = pT pEc spPEc13
testPEc14 = pT pEc spPEc14
testPEc15 = pT pEc spPEc15
testPEc16 = pT pEc spPEc16

spPValue0 = Int' 42
spPValue1 = Int' (negate 13)
spPValue2 = Float' 42.0 
spPValue3 = Float' 4.2 
spPValue4 = Float' (negate 13.245) 
spPValue5 = String' ""
spPValue6 = String' "MUSTARD"

testPValue0 = pT pValue spPValue0
testPValue1 = pT pValue spPValue1
testPValue2 = pT pValue spPValue2
testPValue3 = pT pValue spPValue3
testPValue4 = pT pValue spPValue4
testPValue5 = pT pValue spPValue5
testPValue6 = pT pValue spPValue6

spPC0 = Boolean True
spPC1 = Boolean False

testPC0 = pT pC spPC0
testPC1 = pT pC spPC1
