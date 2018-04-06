/* util/duptagbin.p */

DEF BUFFER bfg-bin FOR fg-bin.

DEFINE VARIABLE i AS INTEGER NO-UNDO.

OUTPUT TO 'util/duptagbin.log'.
FOR EACH fg-bin NO-LOCK WHERE fg-bin.tag NE ''
                          AND fg-bin.qty NE 0
    BY fg-bin.company BY fg-bin.tag BY fg-bin.loc BY fg-bin.loc-bin
    WITH STREAM-IO WIDTH 132:
  IF NOT CAN-FIND(FIRST bfg-bin WHERE bfg-bin.company EQ fg-bin.company
                              AND bfg-bin.tag EQ fg-bin.tag
                              AND bfg-bin.i-no EQ fg-bin.i-no
                              AND bfg-bin.qty NE 0
                              AND (bfg-bin.loc NE fg-bin.loc
                               OR bfg-bin.loc-bin NE fg-bin.loc-bin)) THEN NEXT.
  DISP
    fg-bin.company
    fg-bin.i-no
    fg-bin.tag FORMAT 'x(20)'
    fg-bin.loc LABEL 'Loc'
    fg-bin.loc-bin LABEL 'Bin'
    fg-bin.qty LABEL 'Qty'
    fg-bin.case-count
    fg-bin.cases-unit
    fg-bin.units-pallet
    fg-bin.partial-count.
  i = i + 1.
END. /* each fg-bin */
DISP 'Total Records with Issues:' STRING(i).
OUTPUT CLOSE.
