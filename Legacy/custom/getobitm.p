
DEF INPUT  PARAM ip-company LIKE cust-part.company NO-UNDO.
DEF INPUT  PARAM ip-i-no    LIKE cust-part.part-no NO-UNDO.
DEF INPUT  PARAM ip-ask-quest AS LOG NO-UNDO.
DEF OUTPUT PARAM op-new#    LIKE cust-part.i-no    NO-UNDO.

DEF VAR ll AS LOG NO-UNDO.

op-new# = "".

FIND FIRST cust-part
    WHERE cust-part.company EQ ip-company
      AND cust-part.part-no EQ ip-i-no
      AND cust-part.cust-no EQ FILL("*",20)
    NO-LOCK NO-ERROR.
IF AVAIL cust-part THEN DO:
  op-new# = FILL("?",30).

  IF ip-ask-quest THEN
     MESSAGE "FG " + TRIM(ip-i-no) + " is obsolete, import new FG Item#?"
         VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
         UPDATE ll.

  IF ll THEN op-new# = cust-part.i-no.
END.


