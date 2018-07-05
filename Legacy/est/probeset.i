
DEF VAR li-rels AS INT NO-UNDO.
DEF VAR lv-time AS INT NO-UNDO.
DEFINE VARIABLE cCEBrowseSubDir AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCEBrowse AS LOGICAL NO-UNDO.

DEF BUFFER probe-board FOR reftable.
DEF BUFFER bff-probe for probe .


li-rels = IF xest.est-type EQ 2 OR xest.est-type EQ 5 OR xest.est-type EQ 6
                                THEN rels[vmcl] 
          ELSE
          IF xest.est-type EQ 1 THEN rels[k]
                                ELSE 1.

DO TRANSACTION:   

  RELEASE probe.

  lv-time = TIME.

  IF vprint THEN
  FIND FIRST probe
      WHERE probe.company    EQ xest.company
        AND probe.est-no     EQ xest.est-no
        AND probe.probe-date EQ TODAY
        AND probe.est-qty    EQ {1}
        AND probe.freight    EQ li-rels
        AND probe.probe-time EQ lv-time
      NO-ERROR.

  IF AVAIL probe THEN DO:
    FOR EACH probeit
        WHERE probeit.company EQ probe.company
          AND probeit.est-no  EQ probe.est-no
          AND probeit.line    EQ probe.line:
      DELETE probeit.
    END.

    FOR EACH est-summ
        WHERE est-summ.company EQ probe.company
          AND est-summ.est-no  EQ probe.est-no
          AND est-summ.e-num   EQ probe.line:
      DELETE est-summ.
    END.
  END.

  ELSE DO:
    v-line = 1.

    FOR EACH bff-probe
        WHERE bff-probe.company EQ xest.company
          AND bff-probe.est-no  EQ xest.est-no
        NO-LOCK
        BY bff-probe.line DESC:
      v-line = bff-probe.line + 1.
      LEAVE.
    END.
 
    RELEASE probe.

    CREATE probe.
    ASSIGN
     probe.company      = xest.company
     probe.est-no       = xest.est-no
     probe.e-num        = xest.e-num
     probe.probe-date   = IF vprint THEN TODAY ELSE ?
     probe.est-qty      = {1}
     probe.line         = v-line
     probe.freight      = li-rels
     probe.probe-user   = USERID("nosweat")
     probe.spare-char-1 = tmp-dir.
  END.
  
  ASSIGN
   probe.probe-time = lv-time
   probe.set-chg    = {2}.

    
  FIND CURRENT probe NO-LOCK NO-ERROR.

  FIND FIRST probe-board
      WHERE probe-board.reftable EQ "probe.board"
        AND probe-board.company  EQ probe.company
        AND probe-board.loc      EQ ""
        AND probe-board.code     EQ probe.est-no
        AND probe-board.code2    EQ STRING(probe.line,"9999999999")
      NO-ERROR.
  IF NOT AVAIL probe-board THEN DO:
    CREATE probe-board.
    ASSIGN
     probe-board.reftable = "probe.board"
     probe-board.company  = probe.company
     probe-board.loc      = ""
     probe-board.code     = probe.est-no
     probe-board.code2    = STRING(probe.line,"9999999999").
  END.
  probe-board.val[1] = 0.

  FIND CURRENT probe-board NO-LOCK NO-ERROR.
END.

IF vprint AND xest.est-type GE 5 AND xest.est-type LE 6 THEN
  RUN est/upestqty.p (ROWID(xest)).


RUN est/CostBuildHeaders.p (ROWID(xest), {1}, riJob). 

