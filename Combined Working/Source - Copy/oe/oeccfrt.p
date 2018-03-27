/***************************************************************************\
*****************************************************************************
**  Program: oeccfrt.p
**       By:
** Descript:
**
*****************************************************************************
***************************************************************************/

DEF INPUT PARAM cocode AS CHAR NO-UNDO.
DEF INPUT PARAM ws_loc     AS CHAR NO-UNDO.
DEF INPUT PARAM ws_carrier LIKE carr-mtx.carrier NO-UNDO.
DEF INPUT PARAM ws_delzone LIKE carr-mtx.del-zone NO-UNDO.
DEF INPUT PARAM ws_delzip  LIKE carr-mtx.del-zip  NO-UNDO.
DEF INPUT PARAM ws_weight AS DECIMAL NO-UNDO.
DEF INPUT-OUTPUT PARAM ws_amt AS DECIMAL NO-UNDO.
DEF OUTPUT PARAM ws_cwt AS DECIMAL NO-UNDO.
DEF OUTPUT PARAM ws_minimum AS LOGICAL NO-UNDO.
def output param ws_recid as recid no-undo.


DEF VAR i AS INTEGER NO-UNDO FORMAT ">9".
DEF VAR j LIKE i LABEL "j".
DEF VAR k LIKE i LABEL "k".


FIND carrier
  WHERE carrier.company = cocode
  AND carrier.loc = ws_loc
  AND carrier.carrier = ws_carrier NO-LOCK NO-ERROR.

if not avail carrier then
FIND first carrier
  WHERE carrier.company = cocode
  /* AND carrier.loc = ws_loc 9508 CAH relax loc if not found */
  AND carrier.carrier = ws_carrier NO-LOCK NO-ERROR.

IF AVAIL carrier THEN
DO:
  FIND FIRST carr-mtx
    WHERE carr-mtx.company = carrier.company AND
    carr-mtx.carrier = carrier.carrier AND
    carr-mtx.loc     = carrier.loc
    and (if ws_delzone > '' then carr-mtx.del-zone = ws_delzone else true)
    and (if ws_delzip > '' then carr-mtx.del-zip = ws_delzip else true)
    NO-LOCK NO-ERROR.
  /* relax equality on zip to prefix match */
  if not avail carr-mtx then
  FIND FIRST carr-mtx
    WHERE carr-mtx.company = carrier.company AND
    carr-mtx.carrier = carrier.carrier AND
    carr-mtx.loc     = carrier.loc
    and (if ws_delzone > '' then carr-mtx.del-zone = ws_delzone else true)
    and (if ws_delzip > '' then carr-mtx.del-zip begins
        substring(ws_delzip,1,3) else true)
    NO-LOCK NO-ERROR.
  /* relax zip to tiered range match (table entry is upper bound 3 char */
  if not avail carr-mtx then
  FIND FIRST carr-mtx
    WHERE carr-mtx.company = carrier.company AND
    carr-mtx.carrier = carrier.carrier AND
    carr-mtx.loc     = carrier.loc
    and (if ws_delzone > '' then carr-mtx.del-zone = ws_delzone else true)
    and (if ws_delzip > '' then carr-mtx.del-zip >=
        substring(ws_delzip,1,3) else true)
    NO-LOCK NO-ERROR.
  /* relax zip and zone matching */
  if not avail carr-mtx then
  FIND FIRST carr-mtx
    WHERE carr-mtx.company = carrier.company AND
    carr-mtx.carrier = carrier.carrier AND
    carr-mtx.loc     = carrier.loc
    NO-LOCK NO-ERROR.
  IF AVAIL carr-mtx THEN DO:
    DO i = 1 TO 10:
      if carr-mtx.weight[i] gt 0 and
         ws_weight ge carr-mtx.weight[i] then leave.
    END.
    if i ge 1 and i le 10 then ws_cwt = carr-mtx.rate[i].
    IF ROUND(ws_weight / 100 * ws_cwt, 2) <=
      carr-mtx.min-rate AND ws_amt <= carr-mtx.min-rate THEN
    ASSIGN ws_amt = carr-mtx.min-rate ws_minimum = true.
    ELSE
    IF ws_weight NE 0 AND ws_cwt NE 0 THEN
    ASSIGN ws_amt = ROUND(ws_weight / 100 *
      ws_cwt, 2).
    else assign ws_amt = 0.
    assign ws_recid = recid(carr-mtx).
  END.   /* avail matrix */
  else assign ws_minimum = ?.
END.    /* avail carrier */
else assign ws_minimum = ?.
