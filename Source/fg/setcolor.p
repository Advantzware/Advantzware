
DEF INPUT PARAM ip-rowid AS ROWID NO-UNDO.
DEF INPUT PARAM ip-type LIKE item.press-type NO-UNDO.

DEF VAR li AS INT NO-UNDO.
DEF VAR li-inks AS INT NO-UNDO.
DEF VAR li-coat AS INT NO-UNDO.
DEF VAR li-pass AS INT NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEF VAR v-side-count AS INT NO-UNDO.

DEF TEMP-TABLE w-ink NO-UNDO LIKE itemfg-ink
    FIELD mat-type LIKE item.mat-type
    FIELD seq-no   AS   INT.
  
DISABLE TRIGGERS FOR LOAD OF eb.

FIND eb WHERE ROWID(eb) EQ ip-rowid NO-ERROR.

IF AVAIL eb THEN DO:
  FOR EACH itemfg-ink
      WHERE itemfg-ink.company EQ eb.company
        AND itemfg-ink.i-no    EQ eb.stock-no
      NO-LOCK:


  
      DO li = 1 TO (IF itemfg-ink.occurs LE 0 THEN 1
                  ELSE itemfg-ink.occurs):
      CREATE w-ink.
      BUFFER-COPY itemfg-ink TO w-ink.
      ASSIGN
       li-inks      = li-inks + 1.
       w-ink.seq-no = li-inks.
    END.
  END.

  /*IF eb.est-type LE 4 THEN
  DO li = 1 TO EXTENT(eb.i-code2):
    IF eb.i-code2[li] NE ""                               AND
       CAN-FIND(FIRST itemfg-ink
                WHERE itemfg-ink.company EQ eb.company
                  AND itemfg-ink.i-no    EQ eb.stock-no
                  AND itemfg-ink.rm-i-no EQ eb.i-code2[li]
                  AND itemfg-ink.pass    EQ eb.i-ps2[li]) THEN DO:
      CREATE w-ink.
      ASSIGN
       w-ink.rm-i-no = eb.i-code2[li]
       w-ink.dscr    = eb.i-dscr2[li]
       w-ink.pass    = eb.i-ps2[li]
       w-ink.cover%  = eb.i-%2[li]
       li-inks       = li-inks + 1.
       w-ink.seq-no  = li-inks.
    END.
  END.

  ELSE
  DO li = 1 TO EXTENT(eb.i-code):
    IF eb.i-code[li] NE ""                                AND
       CAN-FIND(FIRST itemfg-ink
                WHERE itemfg-ink.company EQ eb.company
                  AND itemfg-ink.i-no    EQ eb.stock-no
                  AND itemfg-ink.rm-i-no EQ eb.i-code[li]
                  AND itemfg-ink.pass    EQ eb.i-ps[li])  THEN DO:
      CREATE w-ink.
      ASSIGN
       w-ink.rm-i-no = eb.i-code[li]
       w-ink.dscr    = eb.i-dscr[li]
       w-ink.pass    = eb.i-ps[li]
       w-ink.cover%  = eb.i-%[li]
       li-inks       = li-inks + 1.
       w-ink.seq-no  = li-inks.
    END.
  END.*/

  IF eb.est-type LE 4 THEN
    ASSIGN
     eb.i-ps2   = 0
     eb.i-code2 = ""
     eb.i-dscr2 = ""
     eb.i-%2    = 0.
  ELSE
    ASSIGN
     eb.i-ps   = 0
     eb.i-code = ""
     eb.i-dscr = ""
     eb.i-%    = 0.

  FOR EACH w-ink:
    FIND FIRST item
        WHERE item.company    EQ eb.company
          AND item.i-no       EQ w-ink.rm-i-no
          AND item.press-type EQ ip-type
        NO-LOCK NO-ERROR.
    IF AVAIL item THEN w-ink.mat-type = item.mat-type.
    ELSE DELETE w-ink.
  END.

  ASSIGN
   li      = 0
   li-inks = 0
   li-coat = 0.

  /*FOR EACH w-ink
      BREAK BY w-ink.pass
            BY w-ink.seq-no:

    ll = NO.

    IF w-ink.mat-type EQ "I" THEN
      ASSIGN
       li-inks = li-inks + 1
       ll      = li-inks LE eb.i-col AND w-ink.pass LE eb.i-pass.

    ELSE
    IF w-ink.mat-type EQ "V" THEN
      ASSIGN
       li-coat = li-coat + 1
       ll      = li-coat LE eb.i-coat AND w-ink.pass LE eb.i-coat-p.

    IF ll THEN li = li + 1.

    IF LAST-OF(w-ink.pass) THEN
      ASSIGN
       li-inks = 0
       li-coat = 0.

    IF NOT ll                                          OR
       (eb.est-type LE 4 AND li GT EXTENT(eb.i-code2)) OR
       (eb.est-type GT 4 AND li GT EXTENT(eb.i-code))  THEN DELETE w-ink.
  END.*/
      
  ASSIGN
   li          = 0
   eb.i-col    = 0
   eb.i-pass   = 0
   eb.i-coat   = 0
   eb.i-coat-p = 0.

  FOR EACH w-ink 
      BREAK BY w-ink.pass
            BY w-ink.rm-i-no
            BY w-ink.seq-no:

    li = li + 1.

    IF eb.est-type LE 4 THEN
    
    
      IF li LE EXTENT(eb.i-code2) THEN DO:
        ASSIGN
         eb.i-ps2[li]   = w-ink.pass
         eb.i-code2[li] = w-ink.rm-i-no
         eb.i-dscr2[li] = w-ink.dscr
         eb.i-%2[li]    = w-ink.cover%.

        RUN accum-inks.
      END.

    ELSE
      IF li LE EXTENT(eb.i-code) THEN DO:
        ASSIGN
         eb.i-ps[li]   = w-ink.pass
         eb.i-code[li] = w-ink.rm-i-no
         eb.i-dscr[li] = w-ink.dscr
         eb.i-%[li]    = w-ink.cover%.

        RUN accum-inks.
      END.

    IF LAST-OF(w-ink.pass) THEN
      ASSIGN
       eb.i-col    = eb.i-col + li-inks
       eb.i-pass   = eb.i-pass + (IF li-inks GT 0 THEN 1 ELSE 0)
       eb.i-coat   = eb.i-coat + li-coat
       eb.i-coat-p = eb.i-coat-p + (IF li-coat GT 0 THEN 1 ELSE 0)
       li-inks     = 0
       li-coat     = 0.
  END.

  IF eb.est-type LE 4 THEN
    DO: 
        eb.side = "".

        DO v-side-count = 1 TO 17:

              IF eb.i-code2[v-side-count] NE "" THEN
                 eb.side[v-side-count] = "F".
              ELSE
                 eb.side[v-side-count] = " ".
        END.
    END.    
END.

RETURN.

PROCEDURE accum-inks.
  IF w-ink.mat-type EQ "I" THEN li-inks = li-inks + 1.
  ELSE
  IF w-ink.mat-type EQ "V" THEN li-coat = li-coat + 1.
END PROCEDURE.
