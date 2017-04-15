/* ------------------------------------------------- oe/oe-ordd.p  03/99 FWK  */
/* delete statement - order entry                                             */
/* -------------------------------------------------------------------------- */

DEF INPUT  PARAMETER v-recid AS RECID.
DEF OUTPUT PARAMETER v-continue AS LOG.

{sys/inc/var.i SHARED}

DEF BUFFER b-oe-rell FOR oe-rell.
DEF BUFFER b-oe-boll FOR oe-boll.

DEF VAR v-stat AS CHAR NO-UNDO.
DEF VAR v-msg AS CHAR NO-UNDO.


v-continue = YES.

FIND FIRST oe-ordl WHERE RECID(oe-ordl) EQ v-recid NO-LOCK NO-ERROR.

IF AVAIL oe-ordl THEN 
FIND oe-ord OF oe-ordl NO-LOCK NO-ERROR.

IF AVAIL oe-ord THEN DO:
  IF oe-ordl.t-inv-qty GT 0 THEN DO:
    MESSAGE "Quantities have been invoiced, unable to delete..."
            VIEW-AS ALERT-BOX ERROR.
    v-continue = NO.
    RETURN.
  END.

  FOR EACH oe-rel NO-LOCK
      WHERE oe-rel.company EQ oe-ordl.company
        AND oe-rel.ord-no  EQ oe-ordl.ord-no
        AND oe-rel.i-no    EQ oe-ordl.i-no
        AND oe-rel.line    EQ oe-ordl.line:

    RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT v-stat).

    IF INDEX("ABPCZ",v-stat) GT 0 THEN DO:
      ASSIGN
       v-msg = ENTRY(INDEX("ABPCZ",v-stat),
                     "n Actual Release, Backorder, Posted Release/BOL,n Invoice,n Invoice")
       v-msg = "A" + v-msg + " exists for Item/Line: " +
               TRIM(oe-ordl.i-no) + "/" + TRIM(STRING(oe-ordl.line,">99")) +
               ", cannot delete...".
      MESSAGE v-msg VIEW-AS ALERT-BOX.
      v-continue = NO.
      RETURN.
    END.
  END.
      
  /*
  for each oe-bolh where oe-bolh.company eq oe-ordl.company
                     and oe-bolh.ord-no eq oe-ordl.ord-no
                     and oe-bolh.posted 
                   no-lock:

          for each inv-head where inv-head.company eq oe-bolh.company
                             and inv-head.bol-no eq oe-bolh.bol-no 
                            no-lock:
          find first inv-line where inv-line.company eq inv-head.company
                                 and inv-line.r-no eq inv-head.r-no
                                    and inv-line.i-no eq oe-ordl.i-no 
                              no-lock no-error.
          if AVAIL inv-line THEN DO:
            MESSAGE "Unposted invoice exists, unable to delete..."
                    VIEW-AS ALERT-BOX ERROR.
            assign v-continue = no.
            return.
          end. /* avail inv-line */
        end. /* each inv-head */
  end. /* each oe-bolh */
  */

  FOR EACH oe-boll
      WHERE oe-boll.company EQ oe-ordl.company
        AND oe-boll.ord-no  EQ oe-ordl.ord-no
        AND oe-boll.i-no    EQ oe-ordl.i-no
        AND oe-boll.line    EQ oe-ordl.line:

    FOR EACH oe-rell
        WHERE oe-rell.company  EQ oe-boll.company
          AND oe-rell.ord-no   EQ oe-boll.ord-no
          AND oe-rell.line     EQ oe-boll.line
          AND oe-rell.i-no     EQ oe-boll.i-no
          AND oe-rell.r-no     EQ oe-boll.r-no
          AND oe-rell.rel-no   EQ oe-boll.rel-no
          AND oe-rell.b-ord-no EQ oe-boll.b-ord-no:

      RUN delete-oe-rell.
    END. /* each oe-rell */

    FIND FIRST oe-bolh WHERE oe-bolh.b-no EQ oe-boll.b-no NO-ERROR.
 
    IF AVAIL oe-bolh THEN DO:
      FIND FIRST b-oe-boll
          WHERE RECID(b-oe-boll) NE RECID(oe-boll) 
            AND b-oe-boll.company EQ oe-bolh.company
            AND b-oe-boll.b-no EQ oe-bolh.b-no
          NO-ERROR.
      IF NOT AVAIL b-oe-boll THEN DELETE oe-bolh.
    END.

    DELETE oe-boll.
  END. /*each oe-boll */
      
  FOR EACH oe-rell
      WHERE oe-rell.company EQ oe-ordl.company
        AND oe-rell.ord-no  EQ oe-ordl.ord-no
        AND oe-rell.i-no    EQ oe-ordl.i-no
        AND oe-rell.line    EQ oe-ordl.line:

    RUN delete-oe-rell.
  END. /* each oe-rell */

  FOR EACH eb
      WHERE eb.company EQ oe-ordl.company
        AND eb.est-no  EQ oe-ordl.est-no
        AND eb.cust-no EQ oe-ord.cust-no
        AND eb.ord-no  EQ oe-ordl.ord-no
        AND (eb.part-no  EQ oe-ordl.part-no OR
             eb.est-type EQ 2               OR
             eb.est-type EQ 6):
    eb.ord-no = 0.
  END.
END. /* avail oe-ordl */

RETURN.

PROCEDURE delete-oe-rell.
  FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-ERROR.
  IF AVAIL oe-relh THEN DO:
    FIND FIRST b-oe-rell NO-LOCK
        WHERE RECID(b-oe-rell) NE RECID(oe-rell) 
          AND b-oe-rell.r-no EQ oe-relh.r-no
        USE-INDEX r-no NO-ERROR.
    IF NOT AVAIL b-oe-rell THEN DELETE oe-relh.
  END.
                
  FIND FIRST itemfg
      WHERE itemfg.company EQ oe-ordl.company
        AND itemfg.i-no EQ oe-ordl.i-no
      NO-ERROR.
  IF AVAIL itemfg THEN itemfg.q-rel = itemfg.q-rel - oe-rell.qty.
  RUN fg/chkfgloc.p (INPUT oe-ordl.i-no, INPUT oe-rell.loc).
  FIND FIRST itemfg-loc 
      WHERE itemfg-loc.company EQ oe-ordl.company
        AND itemfg-loc.i-no    EQ oe-ordl.i-no
        AND itemfg-loc.loc     EQ oe-rell.loc
      EXCLUSIVE-LOCK NO-ERROR.
  IF AVAIL itemfg-loc THEN
    itemfg-loc.q-rel = itemfg-loc.q-rel - oe-rell.qty.
  FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
  DELETE oe-rell.
END PROCEDURE.

/* end ---------------------------------- copr. 1993  advanced software, inc. */
