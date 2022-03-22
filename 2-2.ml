type team = Korea | France | Usa | Brazil | Japan | Nigeria | Cameroon
          | Poland | Portugal | Italy | Germany | Norway | Sweden | England
          | Argentina

type tourna = LEAF of team
            | NODE of tourna * tourna

let rec parenize : tourna -> string = fun tn ->
  match tn with
  | LEAF (t) -> (
    match t with
    | Korea -> "Korea"
    | France -> "France"
    | Usa -> "Usa"
    | Brazil -> "Brazil"
    | Japan -> "Japan"
    | Nigeria -> "Nigeria"
    | Cameroon -> "Cameroon"
    | Poland -> "Poland"
    | Portugal -> "Portugal"
    | Italy -> "Italy"
    | Germany -> "Germany"
    | Norway -> "Norway"
    | Sweden -> "Sweden"
    | England -> "England"
    | Argentina -> "Argentina"
  )
  | NODE (t1, t2) -> 
    let _t1 = String.concat "(" [""; (parenize t1)] in
    let t1 = String.concat _t1 [""; " "] in
    let t2 = String.concat (parenize t2) [""; ")"] in
    String.concat t1 [""; t2]