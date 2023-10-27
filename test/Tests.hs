import Test.Tasty ( defaultMain, TestTree, testGroup
                  , after, DependencyType(AllSucceed) -- used to control tests
                  )
import Test.Tasty.HUnit (testCase, Assertion)
import IC.Approx
import IC.Exact

import IC.Colour
import LSystems
import Examples

main :: IO ()
main = defaultMain tests

-- Some of the later tests depend on `angle`, `axiom` and `rules` heavily:
-- go implement them first, or the tests won't run!
tests :: TestTree
tests = testGroup "lsystems"
  [ testGroup "angle" (numberedTests angleTests)
  , testGroup "axiom" (numberedTests axiomTests)
  , testGroup "rules" (numberedTests rulesTests)
  , after AllSucceed "/rules/" $
      testGroup "lookupChar" (numberedTests lookupCharTests)
  , after AllSucceed "/rules/ || /axiom/" $
      testGroup "expandOne" (numberedTests expandOneTests)
  , after AllSucceed "/rules/ || /axiom/" $
      testGroup "expand" (numberedTests expandTests)
  , testGroup "move" (numberedTests moveTests)
  , after AllSucceed "/angle/" $
      testGroup "trace1" (numberedTests (traceTests trace1))
  , after AllSucceed "/angle/" $
      testGroup "trace2" (numberedTests (traceTests trace2))
  ]

angleTests :: [Assertion]
angleTests = [ angle cross             --> 90
             , angle (LSystem 1 "" []) --> 1
             , angle triangle          --> 90
             , angle arrowHead         --> 60
             ]

axiomTests :: [Assertion]
axiomTests = [ axiom (LSystem 0 "+" []) --> "+"
             , axiom cross              --> "M-M-M-M"
             , axiom triangle           --> "-M"
             , axiom arrowHead          --> "N"
             ]

rulesTests :: [Assertion]
rulesTests = [ rules cross                       --> [ ('M', "M-M+M+MM-M-M+M")
                                                     , ('+', "+")
                                                     , ('-', "-")
                                                     ]
             , rules (LSystem 0 "" [('M', "N")]) --> [ ('M', "N") ]
             ]

lookupCharTests :: [Assertion]
lookupCharTests =
  [ lookupChar [('X', "Yes"), ('Y', "No")] 'X' --> "Yes"
  , lookupChar [('Y', "No"), ('X', "Yes")] 'X' --> "Yes"
  , lookupChar (rules peanoGosper) 'M'         --> "M+N++N-M--MM-N+"
  , lookupChar (rules triangle) '+'            --> "+"
  ]

expandOneTests :: [Assertion]
expandOneTests =
  [ expandOne (rules triangle) (axiom triangle) --> "-M+M-M-M+M"
  , expandOne [('A', "B")] "A"                  --> "B"
  ]
  

expandTests :: [Assertion]
expandTests =
  [ expand (axiom arrowHead) 2 (rules arrowHead) --> "N+M+N-M-N-M-N+M+N"
  , expand (axiom dragon) 0 (rules dragon)       --> "MX"
  , expand (axiom dragon) 1 (rules dragon)       --> "A+MX--MY+"
  , expand (axiom dragon) 5 (rules dragon)
      -- the backslashes here are called "string gaps", allowing for writing
      -- a string across multiple lines
      --> "A+A+A+A+A+MX--MY+--A-MX++MY-+--A-A+MX--MY+++A-MX++MY--+\
          \--A-A+A+MX--MY+--A-MX++MY-+++A-A+MX--MY+++A-MX++MY---+-\
          \-A-A+A+A+MX--MY+--A-MX++MY-+--A-A+MX--MY+++A-MX++MY--++\
          \+A-A+A+MX--MY+--A-MX++MY-+++A-A+MX--MY+++A-MX++MY----+"

  , expand (axiom tree) 2 (rules tree)
      --> "NN[-N[-M][+M][NM]][+N[-M][+M][NM]][NNN[-M][+M][NM]]"
  , expand (axiom bush) 3 (rules bush)
      --> "MMMM-[[MM-[[M-[[X]+X]+M[+MX]-X]+M-[[X]+X]+M[+MX]-X]+MM[\
          \+MMM-[[X]+X]+M[+MX]-X]-M-[[X]+X]+M[+MX]-X]+MM-[[M-[[X]\
          \+X]+M[+MX]-X]+M-[[X]+X]+M[+MX]-X]+MM[+MMM-[[X]+X]+M[+M\
          \X]-X]-M-[[X]+X]+M[+MX]-X]+MMMM[+MMMMMM-[[M-[[X]+X]+M[+\
          \MX]-X]+M-[[X]+X]+M[+MX]-X]+MM[+MMM-[[X]+X]+M[+MX]-X]-M\
          \-[[X]+X]+M[+MX]-X]-MM-[[M-[[X]+X]+M[+MX]-X]+M-[[X]+X]+\
          \M[+MX]-X]+MM[+MMM-[[X]+X]+M[+MX]-X]-M-[[X]+X]+M[+MX]-X"

  ]

moveTests :: [Assertion]
moveTests = [ move L 90 ((100, 100), 90)  ~~> ((100.0,100.0),180.0)
            , move F 60 ((50, 50), 60)    ~~> ((50.5,50.866024),60.0)
            , move F 45 ((-25, 180), 180) ~~> ((-26.0,180.0),180.0)
            , move R 90 ((200, 200), 180) ~~> ((200.0, 200.0),90.0)
            , move R 45 ((-20, 50), 0)    ~~> ((-20.0, 50.0),-45.0)
            , move R 90 ((100, 100), 90)  ~~> ((100.0,100.0),0.0)
            , move R 30 ((100, 100), 90)  ~~> ((100.0,100.0),60.0)
            ]

traceTests :: ([Command] -> Float -> Colour -> [ColouredLine]) -> [Assertion]
traceTests trace
  = [ trace (expandLSystem triangle 1) (angle triangle) Blue
      ~~> [ ((0.0,0.0),(1.0,0.0),Blue)
          , ((1.0,0.0),(0.99999994,1.0),Blue)
          , ((0.99999994,1.0),(2.0,1.0),Blue)
          , ((2.0,1.0),(2.0,0.0),Blue)
          , ((2.0,0.0),(3.0,0.0),Blue)
          ]
    , trace (expandLSystem tree 1) (angle tree) Red
      ~~> [ ((0.0,0.0),(-4.371139e-8,1.0),Red)
          , ((-4.371139e-8,1.0),(0.7071067,1.7071068),Red)
          , ((-4.371139e-8,1.0),(-0.7071068,1.7071068),Red)
          , ((-4.371139e-8,1.0),(-8.742278e-8,2.0),Red)
          , ((-8.742278e-8,2.0),(-1.3113416e-7,3.0),Red)
          ]
    , trace (expandLSystem galaxy 1) (angle galaxy) Blue
      ~~> [ ((0.0,0.0),(-0.5877852,0.809017),Blue)
          , ((-0.5877852,0.809017),(0.0,1.6180341),Blue)
          , ((0.0,0.0),(-0.9510564,-0.3090172),Blue)
          , ((0.0,1.6180341),(0.5877852,0.80901706),Blue)
          , ((-0.9510564,-0.3090172),(-1.5388416,0.4999998),Blue)
          , ((-1.5388416,0.4999998),(-0.58778507,0.8090168),Blue)
          , ((0.0,0.0),(1.1924881e-8,-1.0),Blue)
          , ((1.1924881e-8,-1.0),(-0.9510564,-1.3090172),Blue)
          , ((-0.9510564,-1.3090172),(-0.9510565,-0.30901718),Blue)
          , ((0.0,0.0),(0.95105654,-0.30901694),Blue)
          , ((0.95105654,-0.30901694),(0.95105654,-1.309017),Blue)
          , ((0.95105654,-1.309017),(5.9604645e-8,-0.9999999),Blue)
          , ((0.0,0.0),(0.5877852,0.80901706),Blue)
          , ((0.5877852,0.80901706),(1.5388417,0.5000001),Blue)
          , ((1.5388417,0.5000001),(0.9510563,-0.3090167),Blue)
          ]
    , trace (expandLSystem canopy 1) (angle canopy) Blue
      ~~> [ ((0.0,0.0),(-4.371139e-8,1.0),Blue)
          , ((-4.371139e-8,1.0),(-0.5000001,1.8660254),Blue)
          , ((-0.5000001,1.8660254),(-1.0000002,2.732051),Blue)
          , ((-4.371139e-8,1.0),(0.49999994,1.8660254),Blue)
          , ((0.49999994,1.8660254),(0.9999999,2.732051),Blue)
          , ((-4.371139e-8,1.0),(-8.742278e-8,2.0),Blue)
          , ((-8.742278e-8,2.0),(0.49999988,2.8660254),Blue)
          , ((-8.742278e-8,2.0),(-0.5000001,2.8660254),Blue)
          , ((-8.742278e-8,2.0),(-1.3113416e-7,3.0),Blue)
          ]
    ]

-------------------------------------------------------------------------------
-- HELPERS
{-|
This function just matches up a bunch of assertions to a numerical naming
system, allowing us to distinguish them.

If we wanted, we could provide descriptions to them instead...
-}
numberedTests :: [Assertion] -> [TestTree]
numberedTests = zipWith (\n -> testCase ("#" ++ show n)) ([1..] :: [Integer])
