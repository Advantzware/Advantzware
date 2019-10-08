/* fgbinrpt.p */

DEFINE VARIABLE units AS INTEGER NO-UNDO LABEL 'Units'.

OUTPUT TO 'util/fgbinrpt.log'.
FOR EACH itemfg NO-LOCK,
    EACH fg-bin OF itemfg NO-LOCK WHERE fg-bin.qty NE 0
    BREAK BY fg-bin.company BY fg-bin.i-no BY fg-bin.loc BY fg-bin.loc-bin
    WITH STREAM-IO WIDTH 132 NO-BOX:
  IF FIRST-OF(fg-bin.company) THEN
  DISPLAY SKIP (1) fg-bin.company WITH FRAME f1 STREAM-IO SIDE-LABELS NO-BOX.
  units = TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0).
  DISPLAY fg-bin.i-no WHEN FIRST-OF(fg-bin.i-no)
    fg-bin.tag FORMAT 'x(20)' LABEL 'Tag'
    fg-bin.loc LABEL 'Whse'
    fg-bin.loc-bin LABEL 'Bin'
    units
    fg-bin.case-count LABEL 'Unit Count'
    fg-bin.cases-unit COLUMN-LABEL 'Units/!Pallet'
    fg-bin.partial-count FORMAT '->>>,>>9' LABEL 'Partial'
    fg-bin.qty LABEL 'Total Qty'.
  IF LAST-OF(fg-bin.i-no) THEN
  DOWN 1.
END. /* each itemfg */
OUTPUT CLOSE.
&IF DEFINED(FWD-VERSION) > 0 &THEN
open-mime-resource "text/plain" "file:///util/fgbinrpt.log" false.
&ELSE
OS-COMMAND NO-WAIT notepad.exe util/fgbinrpt.log.
&ENDIF
