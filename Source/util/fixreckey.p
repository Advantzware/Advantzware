disable triggers for load of itemfg.
disable triggers for load of oe-ord.
disable triggers for load of oe-ordl.
disable triggers for load of est.
disable triggers for load of po-ord.

def var iCnt as int.
def temp-table tt-count
 field tab-name as char format "x(30)"
 field tab-cnt as int.
DEF VAR fgcnt AS INT.
DEF VAR ordcnt AS INT.
DEF VAR ordlcnt AS INT.
DEF VAR estcnt AS INT.
DEF VAR pocnt AS INT.
DEF VAR oe-relcnt AS INT.
DEF VAR oe-rellcnt AS INT.
DEF VAR efcnt AS INT.
DEF VAR ebcnt AS INT.
DEF VAR itemcnt AS INT.

DEF VAR vlAns AS LOG NO-UNDO.

MESSAGE "All Items on Order?" view-as alert-box question
            button yes-no update vlAns.
PAUSE 0 BEFORE-HIDE.
IF vlAns THEN DO:
    
  RUN util/updRecKey.p (INPUT "itemfg", output fgcnt).
  RUN util/updRecKey.p (INPUT "item", output itemcnt).

  RUN util/updRecKey.p (INPUT "oe-ord", output ordcnt).
  RUN util/updRecKey.p (INPUT "oe-ordl", output ordlcnt).
            
  RUN util/updRecKey.p (INPUT "est", output estcnt).
  RUN util/updRecKey.p (INPUT "ef", output efcnt).
  RUN util/updRecKey.p (INPUT "eb", output ebcnt).

  RUN util/updRecKey.p (INPUT "po-ord", output pocnt).

  RUN util/updRecKey.p (INPUT "oe-rel", output oe-relcnt).
  RUN util/updRecKey.p (INPUT "oe-rell", output oe-rellcnt).

    
    MESSAGE "Summary: " SKIP(1)
        SKIP
        "Itemfg: " fgcnt SKIP
        "Item:  " itemcnt SKIP
        "Orders: " ordcnt SKIP
        "Order Lines: " ordlcnt SKIP
        "Estimates: " estcnt SKIP
        "EB:  " ebcnt SKIP
        "EF:  " efcnt SKIP
        "POs: " pocnt SKIP
        "sched rel " oe-relcnt SKIP
        "act rel" oe-rellcnt SKIP
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
END. /* Do block */
