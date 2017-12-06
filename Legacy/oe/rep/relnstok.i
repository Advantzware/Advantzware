/* ---------------------------------------------- oe/rep/relnstok.i */
/* Print OE Release/Picking tickets    for NSTOCK Xprint  copy of PrimerX        */
/* -----------------------------------------------------------------*/


{oe/rep/oe-pick1.i}

{sys/FORM/r-top.i}


DEF TEMP-TABLE w-oe-rell NO-UNDO
FIELD ord-no   AS INTEGER          FORMAT "ZZZZZ9"
FIELD i-no     AS CHARACTER
FIELD qty      AS INTEGER
FIELD LINE     AS INTEGER
FIELD po-no    AS CHARACTER
FIELD company  AS CHARACTER
FIELD r-no     AS INTEGER
FIELD rel-no   AS INTEGER
FIELD b-ord-no AS INTEGER
FIELD rec_key  AS CHARACTER
FIELD tag      LIKE oe-rell.tag
FIELD loc      LIKE oe-rell.loc
FIELD loc-bin  LIKE oe-rell.loc-bin
FIELD seq      AS INTEGER
FIELD set-no   LIKE fg-set.set-no
FIELD qty-case LIKE oe-rell.qty-case
FIELD cases    LIKE oe-rell.cases
FIELD partial  LIKE oe-rell.partial
field lot-no like oe-rell.lot-no
INDEX r-no IS PRIMARY r-no i-no
INDEX idx set-no seq i-no po-no.

DEF TEMP-TABLE w-bin NO-UNDO
FIELD w-loc        LIKE fg-bin.loc
FIELD w-bin        LIKE fg-bin.loc-bin
FIELD w-tag        LIKE fg-bin.tag
FIELD w-qty        LIKE fg-bin.qty         EXTENT 2
FIELD w-unit-count AS INTEGER
FIELD w-units      AS INTEGER
FIELD w-par        LIKE oe-ordl.part-dscr1
FIELD w-i-no       AS cha
FIELD w-set-no     AS CHARACTER
FIELD w-date-time  AS CHARACTER
INDEX w-loc w-loc w-bin
INDEX w-par w-par w-date-time
INDEX w-date-time w-date-time.

DEFINE TEMP-TABLE tt-bin-file NO-UNDO 
    FIELD ord-no       AS INTEGER
    FIELD w-i-no       AS CHARACTER
    FIELD w-loc        AS CHARACTER
    FIELD w-bin        AS CHARACTER .

DEFINE TEMP-TABLE tt-item NO-UNDO
    FIELD w-i-no       AS CHARACTER
    FIELD w-ord-no     AS INTEGER .

DEF TEMP-TABLE w-bin-cons NO-UNDO
FIELD w-i-no       AS CHARACTER
FIELD w-loc        AS CHARACTER
FIELD w-bin        AS CHARACTER
FIELD w-unit-count AS INTEGER
FIELD w-units      AS INTEGER
FIELD w-qty        LIKE fg-bin.qty EXTENT 2
INDEX w-i-no w-i-no w-loc w-bin w-unit-count.

DEF BUFFER bf-w-oe-rell FOR w-oe-rell.
DEF BUFFER b-cust  FOR cust.
DEF BUFFER b-ship  FOR shipto.
DEF BUFFER b-w-bin FOR w-bin.
DEF BUFFER xitemfg FOR itemfg.
DEF BUFFER ref-lot-no FOR reftable.

DEFINE VARIABLE v-frt-pay-dscr     AS CHARACTER        FORMAT "x(11)" NO-UNDO.
DEFINE VARIABLE v-bin              AS CHARACTER        NO-UNDO.
DEFINE VARIABLE v-print            AS LOGICAL          NO-UNDO.
DEFINE VARIABLE v-part-info        LIKE itemfg.i-name    NO-UNDO.
DEFINE VARIABLE v-qty              LIKE oe-rell.qty      NO-UNDO.
DEFINE VARIABLE v-rel-qty          LIKE oe-rell.qty      NO-UNDO.
DEFINE VARIABLE v-tot-rqty         LIKE oe-rell.qty      NO-UNDO.
DEFINE VARIABLE v-tot-rqty-chk         LIKE oe-rell.qty      NO-UNDO.
DEFINE VARIABLE v-tot-count        AS DECIMAL          NO-UNDO.
DEFINE VARIABLE sw                 AS LOGICAL          NO-UNDO.
DEFINE VARIABLE v-rs               AS CHARACTER        NO-UNDO FORM "x(2)".
DEFINE VARIABLE lv-pg-num          AS INTEGER          NO-UNDO.

DEFINE VARIABLE v-zone-hdr         AS CHARACTER        FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE v-zone             LIKE shipto.dest-code NO-UNDO.
DEFINE VARIABLE v-part-dscr        LIKE oe-ordl.i-name   NO-UNDO.
DEFINE VARIABLE v-tmp-lines        AS INTEGER          NO-UNDO.
DEFINE VARIABLE v-doc-hrs          AS CHARACTER        NO-UNDO.

DEFINE VARIABLE v-csr              AS CHARACTER        NO-UNDO.
DEFINE VARIABLE v-draw-line        AS LOGICAL          NO-UNDO.

DEFINE VARIABLE v-tel              AS cha              FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-fax              AS cha              FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-contact          AS cha              FORM "x(20)" NO-UNDO .

DEFINE VARIABLE v-comp-add1        AS cha              FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add2        AS cha              FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add3        AS cha              FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add4        AS cha              FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add5        AS cha              FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-printline        AS INTEGER          NO-UNDO.
DEFINE VARIABLE v-ship-i           AS cha              FORM "x(60)" EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-cust-phono       AS CHARACTER        NO-UNDO.

DEFINE VARIABLE ll-display-comp    AS LOGICAL          NO-UNDO. /* display company address */
DEFINE VARIABLE lv-comp-name       AS cha              FORM "x(30)" NO-UNDO.
DEFINE VARIABLE lv-email           AS cha              FORM "x(30)" NO-UNDO.

DEFINE VARIABLE lv-comp-color      AS cha              NO-UNDO.
DEFINE VARIABLE lv-other-color     AS cha              INIT "BLACK" NO-UNDO.
DEFINE VARIABLE v-partial-qty      AS INT              NO-UNDO.
DEFINE VARIABLE lv-part-qty-printed AS LOG             NO-UNDO.

DEFINE SHARED VARIABLE s-print-what-item  AS CHARACTER        NO-UNDO.
DEFINE SHARED VARIABLE s-print-loc-from   AS CHARACTER        NO-UNDO.
DEFINE SHARED VARIABLE s-print-loc-to     AS CHARACTER        NO-UNDO.
DEFINE SHARED VARIABLE s-print-bin-from   AS CHARACTER        NO-UNDO.
DEFINE SHARED VARIABLE s-print-bin-to     AS CHARACTER        NO-UNDO.
DEFINE SHARED VARIABLE v-print-components AS LOGICAL          NO-UNDO.
DEFINE SHARED VARIABLE s-print-part-no    AS LOGICAL          NO-UNDO.


FORMAT w-oe-rell.ord-no          TO 6
v-bin                            AT 8    FORMAT "x(35)"
w-bin.w-par                      AT 44   FORMAT "x(25)"
w-bin.w-unit-count               TO 76   FORMAT "->>>>>"
w-bin.w-units                    TO 83   FORMAT "->>>>>"
v-tot-rqty                       TO 93   FORMAT "->>>>>>>>"
v-rs                             AT 97   FORM "x(2)"
WITH DOWN FRAME rel-mid NO-BOX no-label STREAM-IO WIDTH 98.

ASSIGN tmpstore = FILL("-",130).

FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
AND sys-ctrl.NAME    EQ "RELPRINT" NO-LOCK NO-ERROR.

ll-display-comp = AVAIL sys-ctrl AND sys-ctrl.log-fld.

FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
AND sys-ctrl.NAME    EQ "LOGOCOLR" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".

IF ll-display-comp THEN DO:

  FIND FIRST cust WHERE
  cust.company = cocode AND
  cust.active = "X"
  NO-LOCK NO-ERROR.
  
  IF AVAIL cust THEN
  ASSIGN v-comp-add1 = cust.addr[1]
  v-comp-add2 = "3900 PRODUCE ROAD"
  v-comp-add3 = "LOUISVILLE, KY  40218"
  v-comp-add4 = "Phone:  " + STRING(cust.area-code,"(999)") + STRING(cust.phone,"999-9999")
  v-comp-add5 = "Fax     :  " + STRING(cust.fax,"(999)999-9999")
  lv-email    = "Email:  " + cust.email
  lv-comp-name = cust.NAME
  v-cust-phono  = STRING(cust.area-code,"(999)") + STRING(cust.phone,"999-9999") .
END.

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.

IF v-zone-p THEN v-zone-hdr = "Route No.:".

{oe/rep/foreachr.i},
FIRST cust
WHERE cust.company EQ cocode
AND cust.cust-no EQ oe-relh.cust-no
NO-LOCK

BREAK BY {1} BY oe-relh.RELEASE#:

RUN oe/custxship.p (oe-relh.company,
oe-relh.cust-no,
oe-relh.ship-id,
BUFFER shipto).

ASSIGN
v-draw-line = NO
v-tot-qty = 0
v-weight  = 0
v-pallets = 0
v-zone    = shipto.dest-code
v-csr = oe-relh.user-id
v-doc-hrs  = shipto.dock-hour .

FIND FIRST carrier
WHERE carrier.company EQ cocode
AND carrier.carrier EQ oe-relh.carrier
NO-LOCK NO-ERROR.

ASSIGN
v-carrier   = IF avail carrier THEN carrier.dscr ELSE ""
v-frt-terms = "".

FOR EACH xoe-rell
  WHERE xoe-rell.company EQ oe-relh.company
  AND xoe-rell.r-no    EQ oe-relh.r-no
  USE-INDEX r-no NO-LOCK,
  FIRST oe-ord
  WHERE oe-ord.company EQ xoe-rell.company
  AND oe-ord.ord-no  EQ xoe-rell.ord-no
  NO-LOCK:
  
  CASE oe-ord.frt-pay:
    WHEN "P" THEN v-frt-terms = "Prepaid".
    WHEN "C" THEN v-frt-terms = "Collect".
    WHEN "B" THEN v-frt-terms = "Bill".
    WHEN "T" THEN v-frt-terms = "Third Party".
  END CASE.
  
  LEAVE.
END.

/* from relcntbx.p */
EMPTY TEMP-TABLE w-oe-rell.

FOR EACH oe-rell
  WHERE oe-rell.company EQ cocode
  AND oe-rell.r-no    EQ oe-relh.r-no,
  
  FIRST itemfg WHERE
  itemfg.company EQ cocode AND
  itemfg.i-no EQ oe-rell.i-no NO-LOCK:
  
  CREATE w-oe-rell.
  BUFFER-COPY oe-rell TO w-oe-rell
  ASSIGN
  i                = 0
  w-oe-rell.seq    = i
  w-oe-rell.set-no = oe-rell.i-no
  oe-rell.printed  = YES.
  
  /* gdm - 03230907 */
  IF v-print-components AND
  itemfg.isaset       AND
  itemfg.alloc NE YES THEN
  FOR EACH fg-set
    WHERE fg-set.company EQ cocode
    AND fg-set.set-no  EQ oe-rell.i-no
    NO-LOCK:
    
    {sys/inc/part-qty.i v-part-qty fg-set}
    
    CREATE w-oe-rell.
    BUFFER-COPY oe-rell TO w-oe-rell.
    
    ASSIGN
    i                = i + 1
    w-oe-rell.seq    = i
    w-oe-rell.set-no = oe-rell.i-no
    w-oe-rell.i-no   = fg-set.part-no
    w-oe-rell.qty    = w-oe-rell.qty * v-part-qty.
  END.
  
  v-weight = v-weight + (oe-rell.qty * itemfg.weight-100 / 100).
END.

{oe/rep/relnstok2.i}
    EMPTY TEMP-TABLE tt-bin-file.
    EMPTY TEMP-TABLE tt-item.

 /* for sorting logic  */
    
 FOR EACH w-oe-rell USE-INDEX idx,
  FIRST oe-ordl
  WHERE oe-ordl.company EQ cocode
  AND oe-ordl.ord-no  EQ w-oe-rell.ord-no
  AND oe-ordl.i-no    EQ w-oe-rell.set-no
  AND oe-ordl.LINE    EQ w-oe-rell.LINE
  NO-LOCK,
  
  FIRST itemfg WHERE
  itemfg.company EQ cocode AND
  itemfg.i-no EQ w-oe-rell.i-no
  NO-LOCK
  BREAK BY w-oe-rell.ord-no DESC
    BY w-oe-rell.loc 
    BY w-oe-rell.loc-bin 
    BY w-oe-rell.set-no
    BY w-oe-rell.seq
    BY w-oe-rell.po-no
    BY w-oe-rell.i-no  :
  

  
  IF LAST-OF(w-oe-rell.i-no) THEN DO:

     i = 0.

    for each fg-bin
               where fg-bin.company  eq cocode
                 and fg-bin.i-no     eq w-oe-rell.i-no
                 and fg-bin.qty      gt 0
               no-lock:
          
               IF NOT(
                  ((s-print-what-item = "R") OR
                   (LOOKUP(s-print-what-item,"I,S") > 0 AND
                    fg-bin.loc >= s-print-loc-from AND
                    fg-bin.loc <= s-print-loc-to AND
                    fg-bin.loc-bin >= s-print-bin-from AND
                    fg-bin.loc-bin <= s-print-bin-to))) THEN
                  NEXT.
              
               IF s-print-what-item = "R" AND
                  NOT CAN-FIND(FIRST oe-rell
                                    WHERE oe-rell.company  EQ w-oe-rell.company
                                      AND oe-rell.r-no     EQ w-oe-rell.r-no
                                      AND oe-rell.ord-no   EQ w-oe-rell.ord-no
                                      AND oe-rell.i-no     EQ w-oe-rell.i-no
                                      AND oe-rell.line     EQ w-oe-rell.line
                                      AND oe-rell.rel-no   EQ w-oe-rell.rel-no
                                      AND oe-rell.b-ord-no EQ w-oe-rell.b-ord-no
                                      AND oe-rell.po-no    EQ w-oe-rell.po-no
                                      AND oe-rell.loc      EQ fg-bin.loc
                                      AND oe-rell.loc-bin  EQ fg-bin.loc-bin
                                      AND oe-rell.tag      EQ fg-bin.tag 
                                      AND fg-bin.tag NE "" ) THEN
                  NEXT.
              
               create tt-bin-file.
               assign
                tt-bin-file.w-loc    = fg-bin.loc
                tt-bin-file.w-bin    = fg-bin.loc-bin
                tt-bin-file.w-i-no   = fg-bin.i-no
                tt-bin-file.ord-no   = w-oe-rell.ord-no .
               i        = i + 1.
              
              
           end. /*each fg-bin*/

           IF i EQ 0 THEN
           FOR EACH bf-w-oe-rell WHERE bf-w-oe-rell.po-no = w-oe-rell.po-no
               AND bf-w-oe-rell.i-no = w-oe-rell.i-no AND bf-w-oe-rell.ord-no = w-oe-rell.ord-no .
               create tt-bin-file.
               assign
                   tt-bin-file.w-loc    = w-oe-rell.loc
                   tt-bin-file.w-bin    = w-oe-rell.loc-bin
                   tt-bin-file.w-i-no   = w-oe-rell.i-no
                   tt-bin-file.ord-no   = w-oe-rell.ord-no .
               i        = i + 1.
             END.
   

    IF i = 0 THEN do:
          create tt-bin-file.
               assign
                tt-bin-file.w-loc    = ""
                tt-bin-file.w-bin    = ""
                tt-bin-file.w-i-no   = w-oe-rell.i-no
                tt-bin-file.ord-no   = w-oe-rell.ord-no .
    END.
  END. /* last-of i-no */
END. /* for each w-oe-rell */


FOR EACH tt-bin-file NO-LOCK 
    BREAK BY tt-bin-file.ord-no 
          
       BY tt-bin-file.w-loc
       BY tt-bin-file.w-bin  .
    
    FIND FIRST tt-item NO-LOCK
         WHERE tt-item.w-i-no = tt-bin-file.w-i-no AND 
              tt-item.w-ord-no = tt-bin-file.ord-no NO-ERROR.
         IF NOT AVAIL tt-item THEN do:
        CREATE tt-item.
        ASSIGN
            tt-item.w-i-no = tt-bin-file.w-i-no
            tt-item.w-ord-no = tt-bin-file.ord-no .
    END.
END. /* foR EACH tt-bin-file */

EMPTY TEMP-TABLE w-bin.
FOR EACH tt-item NO-LOCK:
 FOR EACH w-oe-rell WHERE w-oe-rell.set-no EQ tt-item.w-i-no USE-INDEX idx,
  FIRST oe-ordl
  WHERE oe-ordl.company EQ cocode
  AND oe-ordl.ord-no  EQ w-oe-rell.ord-no
  AND oe-ordl.i-no    EQ w-oe-rell.set-no
  AND oe-ordl.LINE    EQ w-oe-rell.LINE
  NO-LOCK,
  
  FIRST itemfg WHERE
  itemfg.company EQ cocode AND
  itemfg.i-no EQ w-oe-rell.i-no
  NO-LOCK
  BREAK BY w-oe-rell.ord-no DESC
    BY w-oe-rell.loc 
    BY w-oe-rell.loc-bin 
    BY w-oe-rell.set-no
    BY w-oe-rell.seq
    BY w-oe-rell.po-no
    BY w-oe-rell.i-no
     
 
   :
  
  IF FIRST-OF(w-oe-rell.i-no) THEN     
    ASSIGN v-tot-rqty = 0
           sw = NO
           v-tot-count = 0.

  ASSIGN
  v-rel-qty = v-rel-qty + w-oe-rell.qty
  v-tot-rqty = v-tot-rqty + w-oe-rell.qty.

  ASSIGN v-tot-rqty-chk = v-tot-rqty .
  
  IF LAST-OF(w-oe-rell.i-no) THEN DO:
    
    EMPTY TEMP-TABLE w-bin.
    
    i = 0.
    /*FOR EACH bf-w-oe-rell WHERE bf-w-oe-rell.po-no = w-oe-rell.po-no
        AND bf-w-oe-rell.i-no = w-oe-rell.i-no AND bf-w-oe-rell.ord-no = w-oe-rell.ord-no .
      CREATE w-bin.
      ASSIGN
      w-bin.w-tag    = bf-w-oe-rell.tag
      w-bin.w-loc    = bf-w-oe-rell.loc
      w-bin.w-bin    = bf-w-oe-rell.loc-bin
      w-bin.w-qty[1] = bf-w-oe-rell.qty
      w-bin.w-qty[2] = bf-w-oe-rell.qty
      w-bin.w-unit-count = bf-w-oe-rell.cases
      w-bin.w-units = TRUNC((bf-w-oe-rell.qty - bf-w-oe-rell.partial) / bf-w-oe-rell.cases,0)
      w-bin.w-i-no = bf-w-oe-rell.i-no
      w-bin.w-set-no = bf-w-oe-rell.set-no
      i        = i + 1.
      
    END.*/

    /*IF s-print-what-item EQ "S" THEN
      RUN consolidate-bins.*/

    for each fg-bin
               where fg-bin.company  eq cocode
                 and fg-bin.i-no     eq w-oe-rell.i-no
                 and fg-bin.qty      gt 0
               no-lock:
          
               IF NOT(
                  ((s-print-what-item = "R") OR
                   (LOOKUP(s-print-what-item,"I,S") > 0 AND
                    fg-bin.loc >= s-print-loc-from AND
                    fg-bin.loc <= s-print-loc-to AND
                    fg-bin.loc-bin >= s-print-bin-from AND
                    fg-bin.loc-bin <= s-print-bin-to))) THEN
                  NEXT.
              
               IF s-print-what-item = "R" AND
                  NOT CAN-FIND(FIRST oe-rell
                                    WHERE oe-rell.company  EQ w-oe-rell.company
                                      AND oe-rell.r-no     EQ w-oe-rell.r-no
                                      AND oe-rell.ord-no   EQ w-oe-rell.ord-no
                                      AND oe-rell.i-no     EQ w-oe-rell.i-no
                                      AND oe-rell.line     EQ w-oe-rell.line
                                      AND oe-rell.rel-no   EQ w-oe-rell.rel-no
                                      AND oe-rell.b-ord-no EQ w-oe-rell.b-ord-no
                                      AND oe-rell.po-no    EQ w-oe-rell.po-no
                                      AND oe-rell.loc      EQ fg-bin.loc
                                      AND oe-rell.loc-bin  EQ fg-bin.loc-bin
                                      AND oe-rell.tag      EQ fg-bin.tag 
                                      AND fg-bin.tag NE "" ) THEN
                  NEXT.
              
               create w-bin.
               assign
                w-bin.w-tag    = fg-bin.tag
                w-bin.w-loc    = fg-bin.loc
                w-bin.w-bin    = fg-bin.loc-bin
                w-bin.w-qty[1] = fg-bin.qty
                w-bin.w-qty[2] = fg-bin.qty
                w-bin.w-unit-count  =  w-oe-rell.cases /*fg-bin.case-count*/
                w-bin.w-units = w-oe-rell.qty-case /*TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)*/
                w-bin.w-i-no = fg-bin.i-no
                i        = i + 1.
                FIND FIRST bf-w-oe-rell NO-LOCK
                                   WHERE  bf-w-oe-rell.company  EQ w-oe-rell.company
                                      AND bf-w-oe-rell.r-no     EQ w-oe-rell.r-no
                                      AND bf-w-oe-rell.ord-no   EQ w-oe-rell.ord-no
                                      AND bf-w-oe-rell.i-no     EQ w-oe-rell.i-no
                                      AND bf-w-oe-rell.line     EQ w-oe-rell.line
                                      AND bf-w-oe-rell.rel-no   EQ w-oe-rell.rel-no
                                      AND bf-w-oe-rell.b-ord-no EQ w-oe-rell.b-ord-no
                                      AND bf-w-oe-rell.po-no    EQ w-oe-rell.po-no
                                      AND bf-w-oe-rell.loc      EQ fg-bin.loc
                                      AND bf-w-oe-rell.loc-bin  EQ fg-bin.loc-bin
                                      AND bf-w-oe-rell.tag      EQ fg-bin.tag NO-ERROR.
                IF AVAIL bf-w-oe-rell  THEN
                    ASSIGN
                    w-bin.w-unit-count  =  bf-w-oe-rell.cases /*fg-bin.case-count*/
                    w-bin.w-units  = bf-w-oe-rell.qty-case .
               RELEASE bf-w-oe-rell .
               RELEASE w-bin.
           end. /*each fg-bin*/
    
    FOR EACH w-bin
      BREAK BY w-bin.w-loc
            BY w-bin.w-bin:
      v-qty = v-qty + w-bin.w-qty[1].
      
      IF LAST-OF(w-bin.w-bin) THEN DO:
        FOR EACH b-w-bin
          WHERE b-w-bin.w-loc EQ w-bin.w-loc
          AND b-w-bin.w-bin EQ w-bin.w-bin:
          b-w-bin.w-qty[2] = v-qty.
        END.        
        v-qty = 0.
      END.
    END.

    IF i EQ 0 THEN
    FOR EACH bf-w-oe-rell WHERE bf-w-oe-rell.po-no = w-oe-rell.po-no
        AND bf-w-oe-rell.i-no = w-oe-rell.i-no AND bf-w-oe-rell.ord-no = w-oe-rell.ord-no .
      CREATE w-bin.
      ASSIGN
      w-bin.w-tag    = bf-w-oe-rell.tag
      w-bin.w-loc    = bf-w-oe-rell.loc
      w-bin.w-bin    = bf-w-oe-rell.loc-bin
      w-bin.w-qty[1] = bf-w-oe-rell.qty
      w-bin.w-qty[2] = bf-w-oe-rell.qty
      w-bin.w-unit-count  = bf-w-oe-rell.cases
      w-bin.w-units  = TRUNC((bf-w-oe-rell.qty - bf-w-oe-rell.partial) / bf-w-oe-rell.cases,0)
      w-bin.w-i-no = bf-w-oe-rell.i-no
      w-bin.w-set-no = bf-w-oe-rell.set-no
      i        = i + 1.
      
    END.
   
   
    DO i = i TO 7:
      CREATE w-bin.
      ASSIGN w-bin.w-date-time = "29991231000000".
      RELEASE w-bin.
    END.

    RUN fill-in-partno.
    
    j = 7.
    
    FOR EACH w-bin
      BREAK BY w-bin.w-date-time
      BY w-bin.w-loc
      BY w-bin.w-bin
      BY w-bin.w-tag
      BY w-bin.w-qty[2] desc
      BY w-bin.w-qty[1] desc:
      IF w-bin.w-par EQ "" AND w-bin.w-loc EQ "" AND w-bin.w-bin EQ "" THEN DELETE w-bin.
      ELSE j = j + 1.
    END.
    lv-part-qty-printed = NO.
    FOR EACH w-bin
      BREAK BY w-bin.w-date-time
      BY w-bin.w-loc
      BY w-bin.w-bin
      BY w-bin.w-tag
      BY w-bin.w-qty[2] desc
      BY w-bin.w-qty[1] desc:
      IF LAST(w-bin.w-date-time) AND (w-bin.w-loc NE "" OR w-bin.w-bin NE "") THEN
      j = j + 1.
    END.
    
    v-print = YES.
    IF avail oe-rel THEN
    DO i = 1 TO 4:
      IF oe-rel.ship-i[i] NE "" THEN DO:
        IF v-print THEN j = j + 1.
        ASSIGN
        j = j + 1
        v-print = NO.
      END.
    END.
    
    /* Do actual detail printing */
    FOR EACH w-bin
      BREAK BY w-bin.w-date-time
            BY w-bin.w-loc
            BY w-bin.w-bin 
            BY w-bin.w-tag
            /*BY w-bin.w-i-no*/
            BY w-bin.w-qty[2] desc
            BY w-bin.w-qty[1] desc:

      ASSIGN
      v-bin = TRIM(w-bin.w-loc) + "/" +
      TRIM(w-bin.w-bin) + "/" + w-bin.w-tag  .
      
      IF TRIM(v-bin) EQ "//" THEN v-bin = "".
      
      IF v-printline > 44 THEN DO:
        PAGE.
        v-printline = 0.
        {oe/rep/relnstok2.i}
      END.
      
      /*don't draw line for first item on release*/
      IF FIRST(w-bin.w-date-time) AND v-draw-line THEN
      DO:
        PUT "<C1><FROM><C80><LINE>" SKIP.
        v-printline = v-printline + 1.
      END.
      
      v-draw-line = YES.
      
      IF v-printline > 44 THEN DO:
        PAGE.
        v-printline = 0.
        {oe/rep/relnstok2.i}
      END.
      
      IF oe-ordl.whsed THEN
      ASSIGN v-rs = "RS".
      ELSE
      ASSIGN v-rs = "".
      
      IF w-bin.w-units GT 0 THEN DO: 
          IF w-bin.w-units * w-bin.w-unit-count NE v-tot-rqty THEN
            ASSIGN v-partial-qty = v-tot-rqty-chk - (w-bin.w-units * w-bin.w-unit-count)
                   v-tot-rqty-chk  = v-tot-rqty-chk - (w-bin.w-units * w-bin.w-unit-count) .
          ELSE
            ASSIGN v-partial-qty = 0.
         
      END.
       
      DISPLAY {2}
      w-oe-rell.ord-no WHEN FIRST(w-bin.w-date-time)
      v-bin
      w-bin.w-par
      w-bin.w-unit-count 
      w-bin.w-units  
      v-tot-rqty WHEN sw = NO
      v-rs WHEN sw = NO
      WITH FRAME rel-mid.
     
      IF v-partial-qty GT 0 AND w-bin.w-units EQ 0 AND lv-part-qty-printed = NO THEN DO:
        DISPLAY {2}
        1 @ w-unit-count
        v-partial-qty @ w-units
        WITH FRAME rel-mid.
        lv-part-qty-printed = YES.
      END.
      IF w-bin.w-units GT 0 THEN
          v-part-qty = 0.

      ASSIGN
      sw = YES
      v-printline = v-printline + 1.
      
      
      IF LAST(w-bin.w-date-time) THEN DO:
        IF (w-bin.w-loc NE "" OR w-bin.w-bin NE "")  THEN
        
        DOWN {2} WITH FRAME rel-mid.
        
        IF AVAIL itemfg THEN DO:
          FOR EACH notes WHERE notes.rec_key EQ itemfg.rec_key
            AND notes.note_type = "S"
            AND notes.note_code = "PT"
            NO-LOCK
            BY notes.note_code:
            
            v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 20.
            {SYS/INC/ROUNDUP.I v-tmp-lines}
            
            IF v-tmp-lines = 0 THEN
            v-tmp-lines = 1 .
            DO i = 1 TO v-tmp-lines:
              IF v-printline > 36 THEN DO:
                PAGE.
                v-printline = 0.
                {oe/rep/relnstok2.i}
              END.
              
              PUT "<c7>" SUBSTRING(NOTES.NOTE_TEXT,(1 + 20 * (i - 1)), 20) FORM "x(80)" SKIP.
              v-printline = v-printline + 1.
            END.
          END.
        END.
        
        
        IF v-printline > 44 THEN DO:
          PAGE.
          v-printline = 0.
          {oe/rep/relnstok2.i}
        END.
      END.
      
      DOWN {2} WITH FRAME rel-mid.
      
    END.  /* for eacn w-bin  */
    v-rel-qty = 0.
  END.  /* last-of(w-oe-rell.po-no) */
END. /* for each w-oe-rell */
END.


IF v-printline > 44 THEN DO:
  PAGE.
  v-printline = 0.
  {oe/rep/relnstok2.i}
END.

ASSIGN v-ship-i[1] = oe-relh.ship-i[1]
v-ship-i[2] = oe-relh.ship-i[2]
v-ship-i[3] = oe-relh.ship-i[3]
v-ship-i[4] = oe-relh.ship-i[4].

/*Leave shipping notes at bottom of  page*/
IF v-printline > 37 AND
(v-ship-i[1] NE "" OR
v-ship-i[2] NE "" OR
v-ship-i[3] NE "" OR
v-ship-i[4] NE "") THEN DO:
  PAGE.
  v-printline = 0.
  {oe/rep/relnstok2.i}
END.

PUT "<FArial><R49><C1><P12><B>     Dock Hrs: " v-doc-hrs FORMAT "x(15)"
"<R50><C1><P12><B>     Shipping Instructions: </B> <P9> " SKIP
"<R51><C1>" v-ship-i[1] AT 7
"<R52><C1>" v-ship-i[2] AT 7
"<R53><C1>" v-ship-i[3] AT 7
"<R54><C1>" v-ship-i[4] AT 7.

PUT "<R56><C1>"
"<R57><C1><FROM><C80><LINE>" SKIP
"<|10><C1><R58><#8><FROM><C80><R60><RECT> "
"<FArial><P9><=8>   Pallets                                Pulled By                               Shipped by                            Trailer#                                 Dock#" SKIP
"<R58><C16><FROM><R60><C16><Line>"
"<R58><C32><FROM><R60><C32><Line>"
"<R58><C48><FROM><R60><C48><Line>"
"<R58><C64><FROM><R60><C64><Line>".

ASSIGN  lv-pg-num = PAGE-NUM .

PAGE.

ASSIGN
v-printline = 0.
RUN oe/setRelPrinted.p (INPUT ROWID(oe-relh),YES).
END. /* for each oe-relh */

RETURN.

PROCEDURE fill-in-partno.
DEF BUFFER bf-itemfg FOR itemfg.

FOR EACH w-bin WHERE w-bin.w-par EQ ""
  BY w-bin.w-date-time
  BY w-bin.w-loc
  BY w-bin.w-bin
  BY w-bin.w-tag
  BY w-bin.w-qty[2] desc
  BY w-bin.w-qty[1] desc:
  
  w-bin.w-par = IF w-oe-rell.seq EQ 0 THEN oe-ordl.part-no
  ELSE itemfg.part-no.
  LEAVE.
END.

FOR EACH w-bin WHERE w-bin.w-par EQ ""
  BY w-bin.w-date-time
  BY w-bin.w-loc
  BY w-bin.w-bin
  BY w-bin.w-tag
  BY w-bin.w-qty[2] desc
  BY w-bin.w-qty[1] desc:
  w-bin.w-par = IF w-oe-rell.seq EQ 0 THEN oe-ordl.i-name
  ELSE itemfg.i-name.
  LEAVE.
END.

FOR EACH w-bin WHERE w-bin.w-par EQ ""
  BY w-bin.w-date-time
  BY w-bin.w-loc
  BY w-bin.w-bin
  BY w-bin.w-tag
  BY w-bin.w-qty[2] desc
  BY w-bin.w-qty[1] desc:
  w-bin.w-par = IF w-oe-rell.seq EQ 0 THEN oe-ordl.part-dscr1
  ELSE itemfg.part-dscr1.
  LEAVE.
END.

FOR EACH w-bin WHERE w-bin.w-par EQ ""
  BY w-bin.w-date-time
  BY w-bin.w-loc
  BY w-bin.w-bin
  BY w-bin.w-tag
  BY w-bin.w-qty[2] desc
  BY w-bin.w-qty[1] desc:
  w-bin.w-par = IF w-oe-rell.seq EQ 0 THEN oe-ordl.part-dscr2
  ELSE itemfg.part-dscr2.
  LEAVE.
END.

FOR EACH w-bin WHERE w-bin.w-par EQ ""
  BY w-bin.w-date-time
  BY w-bin.w-loc
  BY w-bin.w-bin
  BY w-bin.w-tag
  BY w-bin.w-qty[2] desc
  BY w-bin.w-qty[1] desc:
  w-bin.w-par = itemfg.part-dscr3.
  LEAVE.
END.

FOR EACH w-bin WHERE w-bin.w-par EQ ""
  BY w-bin.w-date-time
  BY w-bin.w-loc
  BY w-bin.w-bin
  BY w-bin.w-tag
  BY w-bin.w-qty[2] desc
  BY w-bin.w-qty[1] desc:
  IF w-oe-rell.po-no NE "" THEN w-bin.w-par = "PO#: " + w-oe-rell.po-no.
  LEAVE.
END.

FOR EACH w-bin WHERE w-bin.w-par EQ ""
  BY w-bin.w-date-time
  BY w-bin.w-loc
  BY w-bin.w-bin
  BY w-bin.w-tag
  BY w-bin.w-qty[2] desc
  BY w-bin.w-qty[1] desc:

    ASSIGN w-bin.w-par = w-oe-rell.lot-no.

  LEAVE.
END.

END PROCEDURE.
PROCEDURE consolidate-bins:
    
    /*consolidate*/
    IF s-print-what-item EQ "S" THEN
    DO:
      EMPTY TEMP-TABLE w-bin-cons.
      
      FOR EACH w-bin:
        
        FIND FIRST w-bin-cons WHERE
        w-bin-cons.w-i-no EQ w-bin.w-i-no AND
        w-bin-cons.w-loc EQ w-bin.w-loc AND
        w-bin-cons.w-bin EQ w-bin.w-bin AND
        w-bin-cons.w-unit-count EQ w-bin.w-unit-count
        NO-ERROR.
        
        IF NOT AVAIL w-bin-cons THEN
        DO:
          CREATE w-bin-cons.
          ASSIGN
          w-bin-cons.w-i-no = w-bin.w-i-no
          w-bin-cons.w-loc = w-bin.w-loc
          w-bin-cons.w-bin = w-bin.w-bin
          w-bin-cons.w-unit-count = w-bin.w-unit-count.
        END.
        ASSIGN
        w-bin-cons.w-units = w-bin-cons.w-units + w-bin.w-units
        w-bin-cons.w-qty[1] = w-bin-cons.w-qty[1] + w-bin.w-qty[1].
        
        DELETE w-bin.
      END. /*each w-bin*/
      
      i = 0.
      
      FOR EACH w-bin-cons:
        
        CREATE w-bin.
        BUFFER-COPY w-bin-cons TO w-bin
        ASSIGN
        w-bin.w-date-time = "29991201000000".
        
        DELETE w-bin-cons.
        i = i + 1.
      END.
    END. /*s-print-what-item EQ "S"*/

END PROCEDURE.
