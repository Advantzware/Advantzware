/* fixCentury.p */

&SCOPED-DEFINE streamIO STREAM-IO

DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE items AS CHARACTER NO-UNDO INITIAL '~
FBIHLER0128A'.
DEFINE VARIABLE tags AS CHARACTER NO-UNDO INITIAL '~
FBIHLER0128A   00032'.

&IF '{&streamIO}' EQ '' &THEN
FUNCTION deleteRec RETURNS LOGICAL (ipTable AS CHARACTER):
  MESSAGE 'Delete' ipTable 'Record?' VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
    UPDATE deleteRec AS LOGICAL.
  RETURN deleteRec.
END FUNCTION.
&ENDIF

DO i = 1 TO NUM-ENTRIES(items):
  &IF '{&streamIO}' EQ 'STREAM-IO' &THEN
  OUTPUT TO VALUE('util/aaa.' + ENTRY(i,items) + '.log') APPEND.
  &ENDIF
  FOR EACH itemfg NO-LOCK WHERE itemfg.i-no EQ ENTRY(i,items) WITH STREAM-IO:
    DISPLAY itemfg.i-no.
    FOR EACH fg-rcpth OF itemfg,
        EACH fg-rdtlh WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
                                AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
                                AND fg-rdtlh.tag EQ ENTRY(i,tags)
        BREAK BY fg-rcpth.trans-date
              BY fg-rdtlh.trans-time
              BY fg-rcpth.r-no
        WITH FRAME f1 {&streamIO} WIDTH 132 TITLE 'fg-rcpth / fg-rdtlh':
      DISPLAY
        fg-rcpth.r-no
        fg-rcpth.trans-date LABEL 'Trans Date'
        fg-rcpth.rita-code LABEL 'Rita'
        fg-rdtlh.tag LABEL 'Tag' FORMAT 'x(20)'
        fg-rdtlh.loc LABEL 'Loc'
        fg-rdtlh.loc-bin LABEL 'Bin'
        fg-rdtlh.qty LABEL 'Qty'
        fg-rdtlh.stacks-unit.
      &IF '{&streamIO}' EQ '' &THEN
      IF deleteRec('fg-rdtlh') THEN DO:
        DELETE fg-rdtlh.
        DELETE fg-rcpth.
      END.
      ELSE
      UPDATE fg-rdtlh.qty  WITH 1 COL WIDTH 80.
      &ENDIF
    END. /* each fg-rcpth */
    FOR EACH fg-bin OF itemfg WHERE fg-bin.tag EQ ENTRY(i,tags)
        WITH {&streamIO} WIDTH 132 TITLE 'fg-bin':
      DISPLAY
        fg-bin.tag FORMAT 'x(20)'
        fg-bin.loc LABEL 'Loc'
        fg-bin.loc-bin LABEL 'Bin'
        fg-bin.qty LABEL 'Qty'
        fg-bin.case-count
        fg-bin.cases-unit
        fg-bin.units-pallet
        fg-bin.partial-count.
      &IF '{&streamIO}' EQ '' &THEN
      IF deleteRec('fg-bin') THEN DELETE fg-bin.
      ELSE
      UPDATE
        fg-bin.qty
        fg-bin.case-count
        fg-bin.cases-unit
        fg-bin.units-pallet
        fg-bin.partial-count
          WITH 1 COL WIDTH 80.
      &ENDIF
    END. /* each fg-bin */
    FOR EACH fg-rctd OF itemfg WHERE fg-rctd.tag EQ ENTRY(i,tags)
        WITH {&streamIO} WIDTH 150 TITLE 'fg-rctd':
      DISPLAY
        fg-rctd.rct-date
        fg-rctd.rita-code
        fg-rctd.tag FORMAT 'x(20)'
        fg-rctd.loc LABEL 'Loc'
        fg-rctd.loc-bin LABEL 'Bin'
        fg-rctd.partial LABEL 'Partial'
        fg-rctd.qty LABEL 'Qty'
        fg-rctd.t-qty
        fg-rctd.qty-case
        fg-rctd.cases-unit
        fg-rctd.cases
        fg-rctd.units-pallet.
      &IF '{&streamIO}' EQ '' &THEN
      IF deleteRec('fg-rctd') THEN DELETE fg-rctd.
      ELSE
      UPDATE
        fg-rctd.qty
        fg-rctd.t-qty
        fg-rctd.qty-case
        fg-rctd.cases-unit
        fg-rctd.cases
        fg-rctd.units-pallet
          WITH 1 COL WIDTH 80.
      &ENDIF
    END. /* each fg-rctd */
    FOR EACH loadtag
        WHERE loadtag.company EQ itemfg.company
          AND loadtag.i-no EQ itemfg.i-no
          AND loadtag.tag-no EQ ENTRY(i,tags)
        WITH {&streamIO} WIDTH 132 TITLE 'loadtag':
      DISPLAY
        loadtag.tag-no
        loadtag.loc LABEL 'Loc'
        loadtag.loc-bin LABEL 'Bin'
        loadtag.pallet-no LABEL 'Pallet'
        loadtag.pallet-count
        loadtag.tot-cases
        loadtag.partial
        loadtag.qty-case
        loadtag.case-bundle.
      &IF '{&streamIO}' EQ '' &THEN
      IF deleteRec('loadtag') THEN DELETE loadtag.
      ELSE
      UPDATE
        loadtag.tag-no
        loadtag.loc LABEL 'Loc'
        loadtag.loc-bin LABEL 'Bin'
        loadtag.pallet LABEL 'Pallet'
        loadtag.pallet-count
        loadtag.tot-cases
        loadtag.partial
        loadtag.qty-case
        loadtag.case-bundle
          WITH 1 COL WIDTH 80.
      &ENDIF
    END.
  END. /* each itemfg */
  &IF '{&streamIO}' EQ 'STREAM-IO' &THEN
  OUTPUT CLOSE.
  &ENDIF
END. /* do i */
