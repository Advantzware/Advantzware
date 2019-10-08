DEF INPUT PARAM selby-ce-ctrl AS CHAR NO-UNDO.
DEF INPUT PARAM selby LIKE ce-ctrl.sell-by NO-UNDO.
DEF INPUT PARAM commb LIKE sman.commbasis NO-UNDO.
DEF INPUT PARAM fcost AS DEC DECIMALS 10 NO-UNDO.
DEF INPUT PARAM ocost AS DEC DECIMALS 10 NO-UNDO.
DEF INPUT PARAM comm% AS DEC DECIMALS 10 NO-UNDO.
DEF INPUT PARAM prof% AS DEC DECIMALS 10 NO-UNDO.

DEF OUTPUT PARAM price AS DEC DECIMALS 10 NO-UNDO.
DEF OUTPUT PARAM comm$ AS DEC DECIMALS 10 NO-UNDO.

IF selby EQ "S" OR selby-ce-ctrl EQ "B" THEN
  ASSIGN
   prof% = (1 - (100 / (100 + prof%))) * 100
   selby = IF selby EQ "S" THEN "G" ELSE "N".
ELSE
IF selby EQ "B" THEN selby = "N".

CASE selby:
  WHEN "G" THEN
    ASSIGN
      price = fcost / (1 - (prof% / 100))
      comm$ = IF comm% EQ 0 THEN 0
              ELSE ((price - (IF commb EQ "G" THEN fcost ELSE 0)) * (comm% / 100)).    
  WHEN "F" THEN
      ASSIGN
        price = (fcost / (1 - (prof% / 100)) + ocost)
        comm$ = IF comm% EQ 0 THEN 0
                ELSE ((price - (IF commb EQ "G" THEN fcost ELSE 0)) * (comm% / 100)).    

  WHEN "N" THEN DO:
        IF commb EQ "G" AND comm% NE 0 THEN
          price = (((1 / (comm% / 100)) * (fcost + ocost)) - fcost) /
                  (((1 / (comm% / 100)) * (1 - (prof% / 100))) - 1).

        ELSE
          price = (fcost + ocost) / (1 - ((prof% + comm%) / 100)).

        comm$ = price - fcost - ocost - (price * (prof% / 100)).
  END.

END CASE.

IF comm% EQ 0 OR comm$ EQ ? THEN comm$ = 0.
