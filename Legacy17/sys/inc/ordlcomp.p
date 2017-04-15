
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.

DEF VAR li AS INT NO-UNDO.

DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF BUFFER b-itemfg FOR itemfg.
DEF BUFFER b-oe-rel FOR oe-rel.
DEF VAR v-q-back LIKE itemfg.q-back NO-UNDO.
DEF VAR cLoc AS CHAR NO-UNDO.
{sys/inc/var.i SHARED}

{fg/fullset.i NEW}


{sys/inc/oeship.i}

FIND FIRST oe-ordl WHERE ROWID(oe-ordl) EQ ip-rowid NO-LOCK NO-ERROR.

IF AVAIL oe-ordl THEN
FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.

IF AVAIL oe-ord THEN
FIND FIRST itemfg OF oe-ordl NO-LOCK NO-ERROR.


IF AVAIL itemfg AND itemfg.isaset AND
   itemfg.alloc AND oeship-log    AND
   NOT oe-ordl.is-a-component     THEN
  RUN fg/fullset.p (ROWID(itemfg)).

FOR EACH b-oe-ordl
    WHERE b-oe-ordl.company        EQ oe-ordl.company
      AND b-oe-ordl.ord-no         EQ oe-ordl.ord-no
      AND b-oe-ordl.is-a-component EQ YES
      AND b-oe-ordl.set-hdr-line   EQ oe-ordl.line
      AND NOT CAN-FIND(FIRST tt-fg-set
                       WHERE tt-fg-set.part-no EQ b-oe-ordl.i-no):
  DELETE b-oe-ordl.
END.
    
FOR EACH b-oe-ordl
    WHERE b-oe-ordl.company        EQ oe-ordl.company
      AND b-oe-ordl.ord-no         EQ oe-ordl.ord-no
      AND b-oe-ordl.is-a-component EQ YES
      AND b-oe-ordl.set-hdr-line   EQ oe-ordl.line
    BREAK BY b-oe-ordl.i-no
          BY b-oe-ordl.rec_key:
  IF FIRST-OF(b-oe-ordl.i-no) AND NOT LAST-OF(b-oe-ordl.i-no) THEN
    DELETE b-oe-ordl.
END.

FOR EACH tt-fg-set WHERE tt-fg-set.part-no NE itemfg.i-no,
    FIRST b-itemfg
    WHERE b-itemfg.company EQ cocode
      AND b-itemfg.i-no    EQ tt-fg-set.part-no
    NO-LOCK
    BREAK BY tt-fg-set.part-no:

  IF FIRST-OF(tt-fg-set.part-no) THEN DO:
    FIND FIRST b-oe-ordl
        {sys/inc/ordlcomp.i b-oe-ordl oe-ordl}
          AND b-oe-ordl.i-no EQ tt-fg-set.part-no
        NO-ERROR.

    IF NOT AVAIL b-oe-ordl THEN DO:
      FOR EACH b-oe-ordl OF oe-ord NO-LOCK BY b-oe-ordl.line DESC:
        LEAVE.
      END.
      li = IF AVAIL b-oe-ordl THEN b-oe-ordl.line ELSE 0.

      CREATE b-oe-ordl.
      BUFFER-COPY oe-ordl EXCEPT rec_key line TO b-oe-ordl
      ASSIGN
       b-oe-ordl.i-no           = b-itemfg.i-no
       b-oe-ordl.line           = li + 1
       b-oe-ordl.is-a-component = YES
       b-oe-ordl.set-hdr-line   = oe-ordl.line.
    END.

    IF b-itemfg.i-name NE itemfg.i-name OR
       b-oe-ordl.i-name EQ ""           THEN
       b-oe-ordl.i-name = b-itemfg.i-name.

    IF b-itemfg.part-no NE itemfg.part-no OR
       b-oe-ordl.part-no EQ ""            THEN
       b-oe-ordl.part-no = b-itemfg.part-no.
  
    IF b-itemfg.part-dscr1 NE itemfg.part-dscr1 OR
       b-oe-ordl.part-dscr1 EQ ""               THEN
       b-oe-ordl.part-dscr1 = b-itemfg.part-dscr1.

    IF b-itemfg.part-dscr2 NE itemfg.part-dscr2 OR
       b-oe-ordl.part-dscr2 EQ ""               THEN
       b-oe-ordl.part-dscr2 = b-itemfg.part-dscr2.

    FIND FIRST eb WHERE
         eb.company EQ b-oe-ordl.company AND
         eb.est-no EQ b-oe-ordl.est-no AND
         eb.part-no EQ b-oe-ordl.part-no
         NO-LOCK NO-ERROR.

    ASSIGN
     b-oe-ordl.price      = 0
     b-oe-ordl.t-price    = 0
     b-oe-ordl.cost       = 0
     b-oe-ordl.pr-uom     = b-itemfg.sell-uom 
     b-oe-ordl.cas-cnt    = b-itemfg.case-count
     b-oe-ordl.qty        = oe-ordl.qty * 
        (IF AVAIL eb AND eb.spare-char-2 = "Y" THEN 1.0 ELSE tt-fg-set.part-qty-dec).


    IF AVAIL eb THEN
       RUN est/getcscnt.p (ROWID(eb),
                           OUTPUT b-oe-ordl.cas-cnt,
                           OUTPUT b-oe-ordl.cases-unit).
 
    FOR EACH oe-rel
        WHERE oe-rel.company EQ oe-ordl.company
          AND oe-rel.ord-no  EQ oe-ordl.ord-no
          AND oe-rel.i-no    EQ oe-ordl.i-no
          AND oe-rel.line    EQ oe-ordl.line
          AND oe-rel.link-no EQ 0:

      FIND FIRST b-oe-rel USE-INDEX seq-no NO-LOCK NO-ERROR.
      li = IF AVAIL b-oe-rel THEN b-oe-rel.r-no ELSE 0.

      CREATE b-oe-rel.
      BUFFER-COPY oe-rel EXCEPT rec_key line spare-dec-1 TO b-oe-rel
      ASSIGN
       b-oe-rel.r-no    = li + 1
       b-oe-rel.i-no    = b-oe-ordl.i-no
       b-oe-rel.line    = b-oe-ordl.line
       b-oe-rel.qty     = 0 /*oe-rel.qty * 
          (IF AVAIL eb AND eb.spare-char-2 = "Y" THEN 1.0 ELSE tt-fg-set.part-qty-dec) */
       b-oe-rel.tot-qty = b-oe-ordl.qty /*b-oe-rel.qty*/.
  
      IF LAST(tt-fg-set.part-no) THEN DO: 

       FIND FIRST itemfg-loc
           WHERE itemfg-loc.company EQ oe-rel.company
             AND itemfg-loc.i-no    EQ oe-rel.i-no
             AND itemfg-loc.loc     EQ oe-rel.spare-char-1
           EXCLUSIVE-LOCK NO-ERROR.
       FIND itemfg WHERE itemfg.company EQ oe-rel.company
            AND itemfg.i-no EQ oe-rel.i-no
           NO-LOCK NO-ERROR.
       cLoc = oe-rel.spare-char-1.
       DELETE oe-rel.

       IF AVAIL(itemfg) AND AVAIL(itemfg-loc) THEN
          RUN fg/calcqabl.p (ROWID(itemfg), cLoc, OUTPUT itemfg-loc.q-alloc, OUTPUT v-q-back).
       FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
      END.

      RUN fg/fgitmloc.p (INPUT b-oe-rel.i-no, INPUT ROWID(b-oe-rel)).

    END.
  END.
END.
