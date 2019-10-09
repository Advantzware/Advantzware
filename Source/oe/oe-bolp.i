/* --------------------------------------------------- oe/oe-bolp.i 08/98 JLF */
/* Bill Of Lading and Invoice Posting - relieve bins                          */
/* -------------------------------------------------------------------------- */

EMPTY TEMP-TABLE tt-bolh.
EMPTY TEMP-TABLE tt-boll.

CREATE tt-bolh.
BUFFER-COPY oe-bolh TO tt-bolh.

CREATE tt-boll.
BUFFER-COPY oe-boll TO tt-boll.
IF oe-boll.s-code = "I" THEN
    ASSIGN 
      tt-boll.loc-bin = ""
      tt-boll.tag     = "".
IF itemfg.isaset AND itemfg.alloc                                         AND
   NOT CAN-FIND(FIRST fg-set WHERE fg-set.company EQ itemfg.company
                               AND fg-set.set-no  EQ itemfg.i-no
                               AND fg-set.part-no EQ itemfg.i-no)         AND
   (NOT AVAIL oe-ordl OR
    NOT CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl})) THEN DO:

  RUN fg/fullset.p (ROWID(itemfg)).

  FOR EACH tt-fg-set:
    
    CREATE tt-boll.
    BUFFER-COPY oe-boll TO tt-boll
    ASSIGN
     tt-boll.i-no = tt-fg-set.part-no
     tt-boll.qty  = oe-boll.qty * tt-fg-set.part-qty-dec
     tt-boll.tag  = "".
  END.
END.

FOR EACH tt-boll:
  
  ASSIGN
   v-bol-qty = tt-boll.qty
   v-partial = tt-boll.partial
   v-fifo    = NO.

  IF v-bol-qty GT 0 THEN DO:
    
   IF AVAIL {1} THEN
      IF "{1}" EQ "oe-ordl" THEN
          
        RUN oe/oe-bolp5.p (RECID(tt-boll), RECID(oe-ordl)).

      ELSE
        RUN oe/invpost5.p (RECID(tt-boll), RECID(inv-line)).
     
    DO WHILE v-bol-qty GT 0:           /** update/create fg-bin records **/
      RELEASE fg-bin.
      
      IF NOT v-fifo THEN DO:
        IF NOT AVAIL fg-bin AND tt-boll.tag NE "" THEN 
        FIND FIRST fg-bin NO-LOCK
            WHERE fg-bin.company EQ tt-bolh.company
              AND fg-bin.tag     EQ tt-boll.tag
              AND fg-bin.i-no    EQ tt-boll.i-no
              AND fg-bin.cust-no EQ ""
              AND fg-bin.loc     EQ tt-boll.loc
              AND fg-bin.loc-bin EQ tt-boll.loc-bin
              AND fg-bin.job-no  EQ tt-boll.job-no
              AND fg-bin.job-no2 EQ tt-boll.job-no2
            USE-INDEX tag NO-ERROR.

        IF NOT AVAIL fg-bin AND tt-boll.tag NE "" THEN
        FIND FIRST fg-bin NO-LOCK
            WHERE fg-bin.company EQ tt-bolh.company
              AND fg-bin.tag     EQ tt-boll.tag
              AND fg-bin.i-no    EQ tt-boll.i-no
              AND fg-bin.cust-no EQ ""
              AND fg-bin.loc     EQ tt-boll.loc
              AND fg-bin.loc-bin EQ tt-boll.loc-bin
            USE-INDEX tag NO-ERROR.

        IF NOT AVAIL fg-bin AND tt-boll.tag NE "" THEN
        FIND FIRST fg-bin NO-LOCK
            WHERE fg-bin.company EQ tt-bolh.company
              AND fg-bin.tag     EQ tt-boll.tag
              AND fg-bin.i-no    EQ tt-boll.i-no
              AND fg-bin.cust-no EQ ""
            USE-INDEX tag NO-ERROR.

        IF NOT AVAIL fg-bin AND tt-boll.job-no ne "" AND tt-boll.s-code NE "I" THEN DO:


          FIND FIRST fg-bin NO-LOCK
              WHERE fg-bin.company EQ tt-bolh.company
                AND fg-bin.i-no    EQ tt-boll.i-no
                AND fg-bin.loc     EQ tt-boll.loc
                AND fg-bin.loc-bin EQ tt-boll.loc-bin
                AND fg-bin.tag     EQ tt-boll.tag
                AND fg-bin.job-no  EQ tt-boll.job-no
                AND fg-bin.job-no2 EQ tt-boll.job-no2
                AND fg-bin.qty     GT 0
              NO-ERROR.


          IF NOT AVAIL fg-bin THEN
          FIND FIRST fg-bin NO-LOCK
              WHERE fg-bin.company EQ tt-bolh.company
                AND fg-bin.i-no    EQ tt-boll.i-no
                AND fg-bin.loc     EQ tt-boll.loc
                AND fg-bin.loc-bin EQ tt-boll.loc-bin
                AND fg-bin.tag     EQ tt-boll.tag
                AND fg-bin.qty     GT 0
              NO-ERROR.

        END. /* not avail fg-bin ... */


      END. /* if not fifo */
      
      IF "{1}" EQ "oe-ordl" THEN
        RUN oe/oe-bolp6.p (RECID(tt-boll), RECID(oe-ordl),
                           IF AVAIL fg-bin THEN RECID(fg-bin) ELSE ?).
      ELSE
        RUN oe/invpost6.p (RECID(tt-boll), RECID(inv-line),
                           IF AVAIL fg-bin THEN RECID(fg-bin) ELSE ?).
    END. /* Do while v-bol-qty gt 0 */
  END. /* If v-bol-qty gt 0 */

  ELSE DO:
    FIND FIRST fg-bin NO-LOCK
        WHERE fg-bin.company EQ tt-boll.company
          AND fg-bin.i-no    EQ tt-boll.i-no
          AND fg-bin.loc     EQ tt-boll.loc
          AND fg-bin.loc-bin EQ tt-boll.loc-bin
          AND fg-bin.tag     EQ tt-boll.tag
          AND fg-bin.job-no  EQ tt-boll.job-no
          AND fg-bin.job-no2 EQ tt-boll.job-no2
          AND fg-bin.cust-no EQ ""
        NO-ERROR.
    
    IF "{1}" EQ "oe-ordl" THEN
      RUN oe/oe-bolp6.p (RECID(tt-boll), RECID(oe-ordl),
                         IF AVAIL fg-bin THEN RECID(fg-bin) ELSE ?).
    ELSE
      RUN oe/invpost6.p (RECID(tt-boll), RECID(inv-line),
                         IF AVAIL fg-bin THEN RECID(fg-bin) ELSE ?).
  END. /* else do */
END. /* each tt-boll */

/* end ---------------------------------- copr. 1998  advanced software, inc. */
