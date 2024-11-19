include "common.mc"
include "ast.mc"
include "json-debug.mc"

lang PhaseStats = Ast + AstToJson
  type StatState =
    { lastPhaseEnd : Ref Float
    , log : Bool
    , jsonDumpPhases : Set String
    }

  sem endPhaseStats : StatState -> String -> Expr -> ()
  sem endPhaseStats state phaseLabel = | e ->
    let before = deref state.lastPhaseEnd in
    let now = wallTimeMs () in

    (if state.log then
      printLn phaseLabel;
      printLn (join ["  Phase duration: ", float2string (subf now before), "ms"]);
      let preTraverse = wallTimeMs () in
      let size = countExprNodes 0 e in
      let postTraverse = wallTimeMs () in
      printLn (join ["  Ast size: ", int2string size, " (Traversal takes ~", float2string (subf postTraverse preTraverse), "ms)"])
     else ());

    (if setMem phaseLabel state.jsonDumpPhases then
      printJsonLn (JsonString (concat "Computing AST after " phaseLabel));
      let e = exprToJson e in
      printJsonLn (JsonString (concat "Printing AST after " phaseLabel));
      printJsonLn e
     else ());

    let newNow = wallTimeMs () in
    modref state.lastPhaseEnd newNow

  sem mkPhaseLogState : Set String -> Bool -> StatState
  sem mkPhaseLogState jsonDumpPhases = | log ->
    { lastPhaseEnd = ref (wallTimeMs ())
    , jsonDumpPhases = jsonDumpPhases
    , log = log
    }
end
