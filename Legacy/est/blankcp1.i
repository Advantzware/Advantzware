
DEF INPUT PARAM ip-help AS LOG NO-UNDO.

DEF VAR lv-field-list AS CHAR INIT "part-no,part-dscr1,stock-no" NO-UNDO.
DEF VAR lv-rowid AS ROWID NO-UNDO.
DEF VAR lv-value  AS CHAR NO-UNDO.
DEF VAR lv-field AS CHAR NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR li1 AS INT NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.
DEF VAR ll-copy-fg AS LOG NO-UNDO.
DEF VAR li-out-item AS cha NO-UNDO .

def var v-e-num        like xeb.e-num.
def var v-est-no       like xeb.est-no.
def var v-form-no      like xeb.form-no.
def var v-cust-seq     like xeb.cust-seq.
def var v-cust-%       like xeb.cust-%.
def var v-blank-no     like xeb.blank-no.
def var v-cust-no      like xeb.cust-no.
def var v-bl-qty       like xeb.bl-qty.
def var v-yld-qty      like xeb.yld-qty.
def var v-num-up       like xeb.num-up.
def var v-yrprice      like xeb.yrprice.
def var v-comm         like xeb.comm.
def var v-ship-addr    like xeb.ship-addr.
def var v-ship-city    like xeb.ship-city.
def var v-ship-name    like xeb.ship-name.
def var v-ship-no      like xeb.ship-no.
def var v-ship-id      like xeb.ship-id.
def var v-ship-state   like xeb.ship-state.
def var v-ship-zip     like xeb.ship-zip.
def var v-sman         like xeb.sman.
def var v-est-type     like xeb.est-type.

DEF BUFFER b-eb FOR eb.
DEF BUFFER b-ef FOR ef.
DEF BUFFER blankcp1-eb FOR eb.


IF NOT CAN-FIND(FIRST itemfg WHERE ROWID(itemfg) EQ lv-copied) THEN DO:
  FIND FIRST b-eb NO-LOCK
      WHERE ROWID(b-eb)     EQ lv-copied
        AND b-eb.part-no    EQ eb.part-no:SCREEN-VALUE IN BROWSE {&browse-name}
        AND b-eb.stock-no   EQ eb.stock-no:SCREEN-VALUE IN BROWSE {&browse-name}
        AND b-eb.part-dscr1 EQ eb.part-dscr1:SCREEN-VALUE IN BROWSE {&browse-name}
      NO-ERROR.
  IF NOT AVAIL b-eb THEN lv-copied = ?.
END.

RELEASE b-ef.
RELEASE b-eb.
RELEASE itemfg.

ll-copy-fg = CAN-FIND(FIRST itemfg
                      WHERE itemfg.company EQ cocode
                        AND itemfg.i-no    EQ eb.stock-no:SCREEN-VALUE IN BROWSE {&browse-name}) AND
             FOCUS:NAME IN BROWSE {&browse-name} EQ "stock-no".

IF ((v-fg-copy AND adm-adding-record) OR ll-copy-fg OR ip-help)             AND 
    (lv-copied EQ ? /*OR CAN-FIND(FIRST b-eb WHERE ROWID(b-eb) EQ lv-copied)*/) THEN
DO WITH FRAME {&FRAME-NAME}:
  ASSIGN
   li      = LOOKUP(FOCUS:NAME IN BROWSE {&browse-name},lv-field-list)
   ll-form = NO.

  IF li EQ 0 THEN
    ASSIGN
     li       = 1
     lv-value = eb.part-no:SCREEN-VALUE IN BROWSE {&browse-name}.

  ELSE lv-value = FOCUS:SCREEN-VALUE IN BROWSE {&browse-name}.

  lv-field = ENTRY(li,lv-field-list).

  IF li EQ 0 THEN li = 1.

  IF li EQ 3 AND lv-value NE "" AND NOT ip-help THEN
  FIND FIRST itemfg NO-LOCK
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ lv-value
      NO-ERROR.

  IF AVAIL itemfg THEN DO:
    ASSIGN
     eb.part-no:SCREEN-VALUE IN BROWSE {&browse-name}    = itemfg.part-no
     eb.part-dscr1:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.i-name
     eb.stock-no:SCREEN-VALUE IN BROWSE {&browse-name}   = CAPS(itemfg.i-no)
     eb.style:SCREEN-VALUE IN BROWSE {&browse-name}      = itemfg.style
     eb.procat:SCREEN-VALUE IN BROWSE {&browse-name}     = itemfg.procat
     eb.len:SCREEN-VALUE IN BROWSE {&browse-name}        = STRING(itemfg.l-score[50])
     eb.wid:SCREEN-VALUE IN BROWSE {&browse-name}        = STRING(itemfg.w-score[50])
     eb.dep:SCREEN-VALUE IN BROWSE {&browse-name}        = STRING(itemfg.d-score[50]).
    /* Removed for task #10310608
     lv-copied                                           = ROWID(itemfg) .
    Removed for task #10310608 */
    IF itemfg.est-no NE "" THEN
    FIND FIRST b-eb
        WHERE b-eb.company  EQ cocode
          AND b-eb.est-no   EQ itemfg.est-no
          AND b-eb.stock-no EQ itemfg.i-no
          AND ROWID(b-eb)   NE ROWID(eb)
      USE-INDEX est-no NO-LOCK NO-ERROR.
  END.
 
  li1 = 0.
  IF NOT AVAIL b-eb THEN DO:
    IF (NOT ip-help) AND lv-value NE "" THEN
      IF lv-field EQ "part-no" THEN
      FOR EACH b-eb NO-LOCK
          WHERE b-eb.company EQ cocode
            AND b-eb.part-no EQ lv-value
            AND ROWID(b-eb)  NE ROWID(eb)
          USE-INDEX part
          BREAK BY b-eb.part-no:
        li1 = li1 + 1.
        IF LAST(b-eb.part-no) OR li1 GT 1 THEN LEAVE.
      END.

      ELSE
      IF lv-field EQ "part-dscr1" THEN
      FOR EACH b-eb NO-LOCK
          WHERE b-eb.company    EQ cocode
            AND b-eb.part-dscr1 EQ lv-value
            AND ROWID(b-eb)     NE ROWID(eb)
          USE-INDEX pdscr
          BREAK BY b-eb.part-dscr1:
        li1 = li1 + 1.
        IF LAST(b-eb.part-dscr1) OR li1 GT 1 THEN LEAVE.
      END.

      ELSE
      IF lv-field EQ "stock-no" THEN
      FOR EACH b-eb NO-LOCK
          WHERE b-eb.company  EQ cocode
            AND b-eb.stock-no EQ lv-value
            AND ROWID(b-eb)   NE ROWID(eb)
          USE-INDEX stock
          BREAK BY b-eb.stock-no:
        li1 = li1 + 1.
        IF LAST(b-eb.stock-no) OR li1 GT 1 THEN LEAVE.
      END.

    lv-rowid = IF adm-new-record AND NOT adm-adding-record THEN ? ELSE ROWID(eb).

    IF ip-help OR li1 GT 1 THEN DO:
      IF lv-field EQ "stock-no" THEN RUN est/l-ebstf.w (gcompany,est.est-type,"", lv-rowid, lv-value, OUTPUT li-out-item , OUTPUT lv-rowid).
      ELSE RUN est/l-eb.w (gcompany, gloc, est.est-type, lv-rowid, li + 2, lv-value, OUTPUT lv-rowid).

      FIND b-eb WHERE ROWID(b-eb) EQ lv-rowid NO-LOCK NO-ERROR.
    END.
  END.

  IF AVAIL b-eb THEN DO:
    ll = b-eb.cust-no EQ eb.cust-no:SCREEN-VALUE IN BROWSE {&browse-name}.
  
    IF NOT ll THEN
      MESSAGE "Item created for another customer, copy anyway?"
          VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
          UPDATE ll.

    IF NOT ll THEN RELEASE b-eb.

    ELSE DO:
      ASSIGN
       eb.part-no:SCREEN-VALUE IN BROWSE {&browse-name}    = b-eb.part-no
       eb.part-dscr1:SCREEN-VALUE IN BROWSE {&browse-name} = b-eb.part-dscr1
       eb.stock-no:SCREEN-VALUE IN BROWSE {&browse-name}   = b-eb.stock-no       
       eb.style:SCREEN-VALUE IN BROWSE {&browse-name}      = b-eb.style
       eb.procat:SCREEN-VALUE IN BROWSE {&browse-name}     = b-eb.procat  
       eb.len:SCREEN-VALUE IN BROWSE {&browse-name}        = STRING(b-eb.len)
       eb.wid:SCREEN-VALUE IN BROWSE {&browse-name}        = STRING(b-eb.wid)
       eb.dep:SCREEN-VALUE IN BROWSE {&browse-name}        = STRING(b-eb.dep) 
       eb.procat:SCREEN-VALUE IN BROWSE {&browse-name}     = b-eb.procat
       eb.i-col:SCREEN-VALUE IN BROWSE {&browse-name}      = STRING(b-eb.i-col)
       eb.i-coat:SCREEN-VALUE IN BROWSE {&browse-name}     = STRING(b-eb.i-coat)       
       eb.pur-man:SCREEN-VALUE IN BROWSE {&browse-name}    = STRING(b-eb.pur-man)
       lv-copied                                           = ROWID(b-eb).

      FIND FIRST b-ef
          WHERE b-ef.company eq b-eb.company
            AND b-ef.est-no  eq b-eb.est-no
            AND b-ef.form-no eq b-eb.form-no
          NO-LOCK NO-ERROR.
      IF AVAIL b-ef THEN
        ASSIGN
         ef.board:SCREEN-VALUE IN BROWSE {&browse-name} = b-ef.board
         ef.cal:SCREEN-VALUE IN BROWSE {&browse-name}   = STRING(b-ef.cal).

      IF ef.est-type GE 5                                 AND
         ef.est-type LE 8                                 AND
         CAN-FIND(FIRST blankcp1-eb
                  WHERE blankcp1-eb.company EQ ef.company
                    AND blankcp1-eb.est-no  EQ ef.est-no
                    AND blankcp1-eb.form-no EQ ef.form-no
                    AND ROWID(blankcp1-eb)  NE ROWID(eb)) THEN
          MESSAGE "Layout already exists for new blank, do you wish to override?"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE ll-form.
      ELSE ll-form = YES.
    END.
  END.
  ELSE DO:
      FIND FIRST itemfg NO-LOCK
          WHERE itemfg.company EQ cocode
          AND itemfg.i-no    EQ ENTRY(1,li-out-item)
        NO-ERROR.

      
   IF AVAIL itemfg THEN DO:
     ASSIGN
      eb.part-no:SCREEN-VALUE IN BROWSE {&browse-name}    = itemfg.part-no
      eb.part-dscr1:SCREEN-VALUE IN BROWSE {&browse-name} = itemfg.i-name
      eb.stock-no:SCREEN-VALUE IN BROWSE {&browse-name}   = CAPS(itemfg.i-no)
      eb.style:SCREEN-VALUE IN BROWSE {&browse-name}      = itemfg.style
      eb.procat:SCREEN-VALUE IN BROWSE {&browse-name}     = itemfg.procat
      eb.len:SCREEN-VALUE IN BROWSE {&browse-name}        = STRING(itemfg.l-score[50])
      eb.wid:SCREEN-VALUE IN BROWSE {&browse-name}        = STRING(itemfg.w-score[50])
      eb.dep:SCREEN-VALUE IN BROWSE {&browse-name}        = STRING(itemfg.d-score[50]).
     /* Removed for task #10310608
      lv-copied                                           = ROWID(itemfg) .
     Removed for task #10310608 */
     IF itemfg.est-no NE "" THEN
     FIND FIRST b-eb
         WHERE b-eb.company  EQ cocode
           AND b-eb.est-no   EQ itemfg.est-no
           AND b-eb.stock-no EQ itemfg.i-no
           AND ROWID(b-eb)   NE ROWID(eb)
       USE-INDEX est-no NO-LOCK NO-ERROR.
   END.

  END.
END.

