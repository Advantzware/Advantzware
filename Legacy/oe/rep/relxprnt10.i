/* ---------------------------------------------- oe/rep/relxprnt2.i */
/* Print OE Release/Picking tickets    for PremierX Xprint          */
/* -----------------------------------------------------------------*/

{oe/rep/oe-pick1.i}

{sys/FORM/r-top.i}

DEF BUFFER ref-lot-no FOR reftable.

def TEMP-TABLE w-oe-rell NO-UNDO
   FIELD ord-no AS INT FORMAT "ZZZZZ9"
   FIELD i-no AS CHAR
   FIELD qty AS INT
   FIELD LINE AS INT
   FIELD po-no AS CHAR
   FIELD company AS CHAR
   FIELD r-no AS INT
   FIELD rel-no AS INT
   FIELD b-ord-no AS INT
   FIELD rec_key AS CHAR
   FIELD job-no AS CHAR
   FIELD job-no2 AS INT
   FIELD loc AS CHAR
   FIELD loc-bin AS CHAR
   field seq    as   int
   FIELD relseq AS INT 
   field set-no like fg-set.set-no
   INDEX r-no IS PRIMARY r-no i-no
   INDEX idx set-no seq i-no po-no.

def TEMP-TABLE w-bin NO-UNDO 
   field w-loc like fg-bin.loc
   field w-bin like fg-bin.loc-bin
   field w-tag like fg-bin.tag
   field w-qty like fg-bin.qty extent 2
   field w-unit-count AS INT
   FIELD w-units AS INT
   field w-par like oe-ordl.part-dscr1
   FIELD w-i-no AS cha
   FIELD w-date-time AS CHAR
   FIELD w-cust-qty AS INT
   FIELD w-uom  AS CHAR
   FIELD w-ord-col AS CHAR FORMAT "X(6)"
   field job like fg-bin.job-no
   field job2 like fg-bin.job-no2
   INDEX w-loc w-loc w-bin
   INDEX w-par w-par w-date-time
   INDEX w-date-time w-date-time
     .


DEF TEMP-TABLE w-bin-cons NO-UNDO
    FIELD w-i-no AS CHAR
    FIELD w-loc AS CHAR
    FIELD w-bin AS CHAR
    FIELD w-unit-count AS INT
    FIELD w-units AS INT
    field w-qty like fg-bin.qty extent 2
    FIELD w-cust-qty AS INT
    FIELD w-uom  AS CHAR 
    INDEX w-i-no w-i-no w-loc w-bin w-unit-count.

def buffer b-cust  for cust.
def buffer b-ship  for shipto.
def buffer b-w-bin for w-bin.

def var v-frt-pay-dscr as char format "x(11)" no-undo.
def var v-bin as char no-undo.
def var v-print as log no-undo.
def var v-part-info like itemfg.i-name no-undo.
def var v-qty like oe-rell.qty no-undo.
def var v-rel-qty like oe-rell.qty  NO-UNDO.
def var v-tot-rqty AS INT NO-UNDO.
DEF VAR sw  AS logi NO-UNDO. 
DEFINE VARIABLE swm AS LOGICAL NO-UNDO .
DEF VAR v-rs AS CHAR NO-UNDO FORM "x(2)".
DEF VAR v-cq AS LOG NO-UNDO.

def var v-zone-hdr as char format "x(10)" NO-UNDO.
def var v-zone like shipto.dest-code NO-UNDO.
def var v-part-dscr like oe-ordl.i-name NO-UNDO.
DEF VAR v-tmp-lines AS INT NO-UNDO.

DEF VAR v-csr AS CHAR NO-UNDO.
DEF VAR v-draw-line AS LOG NO-UNDO.
DEF VAR v-cust-value AS CHAR FORMAT "x(11)" NO-UNDO.
DEF VAR v-tot-rqty-a AS CHAR FORMAT "x(11)" NO-UNDO.
DEF VAR v-final-cust-value AS INT NO-UNDO.
DEF VAR itotrqty AS INT NO-UNDO.

DEF SHARED VAR s-print-what-item AS cha NO-UNDO.
DEF SHARED VAR s-print-loc-from AS cha NO-UNDO.
DEF SHARED VAR s-print-loc-to AS cha NO-UNDO.
DEF SHARED VAR s-print-bin-from AS cha NO-UNDO.
DEF SHARED VAR s-print-bin-to AS cha NO-UNDO.
DEFINE SHARED VARIABLE lSortRelSeq AS LOGICAL NO-UNDO .
DEFINE SHARED VARIABLE lPrintQtyUom AS LOGICAL NO-UNDO .

format w-bin.w-ord-col                  AT 1    FORMAT "x(6)"
       w-bin.w-par                      at 8    format "x(25)"
       v-bin                            at 34   format "x(35)"
       w-bin.w-units                    to 76   format "->>>>>"
       w-bin.w-unit-count               to 83   format "->>>>>"
       v-cust-value                     to 95   FORMAT "x(11)" /* format "->>>>>>>>>>" */
/*        v-rs                             AT 97   FORM "x(2)" */
    with down frame rel-mid no-box no-label STREAM-IO width 98.

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add5 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-printline AS INT NO-UNDO.
DEF VAR v-ship-i AS cha FORM "x(60)" EXTENT 4 NO-UNDO.

DEF VAR ll-display-comp AS LOG NO-UNDO.  /* display company address */
DEF VAR lv-comp-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-email AS cha FORM "x(56)" NO-UNDO.

DEF VAR lv-comp-color AS cha NO-UNDO.
DEF VAR lv-other-color AS cha INIT "BLACK" NO-UNDO.
DEF VAR lv-tot-cust-qty AS INT NO-UNDO.
DEF VAR lv-save-cust-uom AS CHAR NO-UNDO.
DEF BUFFER xitemfg FOR itemfg.
DEF SHARED VAR v-print-components AS LOG NO-UNDO.
DEF SHARED VAR s-print-part-no AS LOG NO-UNDO.
DEF VAR v-reljob AS CHAR FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE iOrdQtyCust AS INTEGER NO-UNDO.
DEFINE BUFFER bf-oe-ordl FOR oe-ordl .

DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormLogo", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).

ASSIGN ls-full-img1 = cRtnChar + ">" .

{sys/inc/oescreen.i}

ASSIGN tmpstore = fill("-",130).

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "RELPRINT" no-lock no-error.

ll-display-comp = AVAIL sys-ctrl AND sys-ctrl.log-fld.

FIND first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "LOGOCOLR" no-lock no-error.
IF AVAIL sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".

/*find first company where company.company = cocode no-lock no-error.*/

IF ll-display-comp THEN DO:
   FIND FIRST cust WHERE
        cust.company = cocode AND
        cust.active = "X"
        NO-LOCK NO-ERROR.

  IF AVAIL cust THEN
     ASSIGN v-comp-add1 = cust.addr[1]
            v-comp-add2 = "3900 PRODUCE ROAD"
            v-comp-add3 = "LOUISVILLE, KY  40218"
            v-comp-add4 = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
            v-comp-add5 = "Fax     :  " + string(cust.fax,"(999)999-9999") 
            lv-email    = "Email:  " + cust.email 
            lv-comp-name = cust.NAME.
 END.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

if v-zone-p then v-zone-hdr = "Route No.:".

    {oe/rep/foreachr.i},

        first cust
        where cust.company eq cocode
          and cust.cust-no eq oe-relh.cust-no
        no-lock

        break by {1} by oe-relh.release#:

      RUN oe/custxship.p (oe-relh.company,
                          oe-relh.cust-no,
                          oe-relh.ship-id,
                          BUFFER shipto).

      assign
       v-draw-line = NO
       v-tot-qty = 0
       v-weight  = 0
       v-pallets = 0
       v-zone    = shipto.dest-code
       v-csr = oe-relh.user-id.

      find first carrier
          where carrier.company eq cocode
            and carrier.carrier eq oe-relh.carrier
          no-lock no-error.
      
      assign
       v-carrier   = if avail carrier then carrier.dscr else ""
       v-frt-terms = "".

      FOR EACH xoe-rell
          where xoe-rell.company eq oe-relh.company
            and xoe-rell.r-no    eq oe-relh.r-no
          USE-INDEX r-no NO-LOCK,
          FIRST oe-ord
          where oe-ord.company eq xoe-rell.company
            and oe-ord.ord-no  eq xoe-rell.ord-no
          no-lock:

        case oe-ord.frt-pay:
             when "P" THEN v-frt-terms = "Prepaid".
             when "C" THEN v-frt-terms = "Collect".
             when "B" THEN v-frt-terms = "Bill".
             when "T" THEN v-frt-terms = "Third Party".
        end case.

        LEAVE.
      END.

      /* from relcntbx.p */
      EMPTY TEMP-TABLE w-oe-rell.
         
      v-reljob = " " .

      for each oe-rell
          where oe-rell.company eq cocode
            and oe-rell.r-no    eq oe-relh.r-no,

          first itemfg WHERE
                itemfg.company EQ cocode AND
                itemfg.i-no EQ oe-rell.i-no no-lock:
          
        create w-oe-rell.
        buffer-copy oe-rell to w-oe-rell
        assign
         i                = 0 
         w-oe-rell.seq    = i
         w-oe-rell.set-no = oe-rell.i-no
         oe-rell.printed  = yes.
         IF lSortRelSeq THEN
             ASSIGN w-oe-rell.relseq = itemfg.spare-int-2 .

        /* gdm - 03230907 */
        IF v-print-components AND
          itemfg.isaset       AND 
          itemfg.alloc NE YES THEN 
          for each fg-set
              where fg-set.company eq cocode
                and fg-set.set-no  eq oe-rell.i-no
              no-lock:
              
            {sys/inc/part-qty.i v-part-qty fg-set}
            
            create w-oe-rell.
            buffer-copy oe-rell to w-oe-rell.
            
            assign
             i                = i + 1
             w-oe-rell.seq    = i
             w-oe-rell.set-no = oe-rell.i-no
             w-oe-rell.i-no   = fg-set.part-no
             w-oe-rell.qty    = w-oe-rell.qty * v-part-qty.
            IF lSortRelSeq THEN
             ASSIGN w-oe-rell.relseq = itemfg.spare-int-2 .
          end.

        v-weight = v-weight + (oe-rell.qty * itemfg.weight-100 / 100).
      end.

      {oe/rep/relxprnt2.i}

      EMPTY TEMP-TABLE w-bin.
      
      for each w-oe-rell USE-INDEX idx,
          first oe-ordl
          where oe-ordl.company eq cocode
            and oe-ordl.ord-no  eq w-oe-rell.ord-no
            and oe-ordl.i-no    eq w-oe-rell.set-no
            and oe-ordl.line    eq w-oe-rell.line
          no-lock,
      
          first itemfg WHERE 
                itemfg.company EQ cocode AND
                itemfg.i-no EQ w-oe-rell.i-no
                no-lock
          break BY w-oe-rell.relseq
                by w-oe-rell.set-no
                by w-oe-rell.seq
                by w-oe-rell.i-no
                by w-oe-rell.po-no
                by w-oe-rell.ord-no:

        IF FIRST-OF(w-oe-rell.set-no) THEN
           ASSIGN v-tot-rqty = 0
                  sw = NO
                  swm = NO. 

        IF FIRST-OF(w-oe-rell.ord-no) THEN
           ASSIGN v-tot-rqty = 0
                  sw = NO
                  swm = NO.
           
        ASSIGN
           v-rel-qty = v-rel-qty + w-oe-rell.qty
           v-tot-rqty = v-tot-rqty + w-oe-rell.qty.
        
        if last-of(w-oe-rell.po-no) then do:
           EMPTY TEMP-TABLE w-bin.
          
           i = 0.

           /*v-reljob = (w-oe-rell.job-no + "-" + string(w-oe-rell.job-no2,"99"))   .*/

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
                                      AND oe-rell.tag      EQ fg-bin.tag) THEN
                  NEXT.
              
               create w-bin
                   .
               assign
                w-bin.w-tag    = fg-bin.tag
                w-bin.w-loc    = fg-bin.loc
                w-bin.w-bin    = fg-bin.loc-bin
                w-bin.w-qty[1] = fg-bin.qty
                w-bin.w-qty[2] = fg-bin.qty
                w-bin.w-unit-count = fg-bin.case-count
                w-bin.w-units  = TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
                w-bin.w-i-no   = fg-bin.i-no
                w-bin.w-cust-qty = IF oe-ordl.spare-dec-1 GT 0 THEN oe-ordl.spare-dec-1 ELSE oe-ordl.qty
                w-bin.w-uom    = IF oe-ordl.spare-char-2 NE "" THEN oe-ordl.spare-char-2 ELSE "EA" 
                w-bin.job = fg-bin.job-no
                w-bin.job2 = fg-bin.job-no2
                w-bin.w-date-time = "29991201000000".
                i        = i + 1
                 .
               ASSIGN iOrdQtyCust = 0.
               FOR EACH bf-oe-ordl NO-LOCK
                    WHERE bf-oe-ordl.company EQ oe-ordl.company
                      AND bf-oe-ordl.ord-no EQ oe-ordl.ord-no
                      AND bf-oe-ordl.i-no EQ oe-ordl.i-no 
                      AND bf-oe-ordl.po-no EQ oe-ordl.po-no :
                    iOrdQtyCust = iOrdQtyCust + IF bf-oe-ordl.spare-dec-1 GT 0 THEN bf-oe-ordl.spare-dec-1 ELSE bf-oe-ordl.qty  .
               END.
               IF iOrdQtyCust GT 0 THEN
                   ASSIGN w-bin.w-cust-qty = iOrdQtyCust .
              
               IF s-print-what-item NE "S" THEN
                  FOR EACH fg-rcpth FIELDS(r-no trans-date) WHERE
                      fg-rcpth.company eq fg-bin.company AND
                      fg-rcpth.i-no    eq fg-bin.i-no AND
                      fg-rcpth.job-no  eq fg-bin.job-no AND
                      fg-rcpth.job-no2 eq fg-bin.job-no2
                      NO-LOCK,
                      first fg-rdtlh FIELDS(trans-time) where
                      fg-rdtlh.r-no eq fg-rcpth.r-no AND
                      fg-rdtlh.loc  eq fg-bin.loc AND
                      fg-rdtlh.loc-bin eq fg-bin.loc-bin AND
                      fg-rdtlh.tag     eq fg-bin.tag AND
                      fg-rdtlh.cust-no EQ fg-bin.cust-no
                      no-lock
                      by fg-rcpth.trans-date
                      BY fg-rdtlh.trans-time
                      by fg-rcpth.r-no
                      /*by fg-rcpth.job-no
                      by fg-rcpth.job-no2
                      by fg-bin.qty*/ :
                 
                     /* w-bin.w-date-time = STRING(YEAR(fg-rcpth.trans-date),"9999")
                                        + STRING(MONTH(fg-rcpth.trans-date),"99")
                                        + STRING(DAY(fg-rcpth.trans-date),"99")
                                        + STRING(fg-rdtlh.trans-time,"999999").*/
                 
                      LEAVE.
                  END.
               ELSE
                  w-bin.w-date-time = "29991201000000".

               RELEASE w-bin.
           end. /*each fg-bin*/
          
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
                        w-bin-cons.w-cust-qty = w-bin.w-cust-qty
                        w-bin-cons.w-uom = w-bin.w-uom
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
           ASSIGN itotrqty = 0.
           FOR EACH w-bin:
                ASSIGN itotrqty = itotrqty + w-bin.w-qty[1].
           END.

           for each w-bin
               break BY w-bin.w-loc
                     by w-bin.w-bin:
               v-qty = v-qty + w-bin.w-qty[1].
                
               if last-of(w-bin.w-bin) then do:
                  for each b-w-bin
                      where b-w-bin.w-loc eq w-bin.w-loc
                        and b-w-bin.w-bin eq w-bin.w-bin:
                    b-w-bin.w-qty[2] = v-qty.
                  end.
                 
                  v-qty = 0.
               end.
           end.
           IF oe-ordl.whsed THEN 
                ASSIGN v-rs = "RS".
           ELSE 
                ASSIGN v-rs = "".
           if i eq 0 then do:
              find first b-cust
                  where b-cust.company eq cocode
                    and b-cust.active  eq "X" 
                  no-lock no-error.
              if avail b-cust then do:
                 find first b-ship
                     where b-ship.company eq cocode
                       and b-ship.cust-no eq b-cust.cust-no
                     no-lock no-error.
                 if avail b-ship then do:
                   create w-bin.
                   assign   
                    w-bin.w-loc = w-oe-rell.loc /*b-ship.loc*/
                    w-bin.w-bin = w-oe-rell.loc-bin /*b-ship.loc-bin*/
                    i     = i + 1
                    w-bin.w-date-time = "29991201000000".
                 end.   
              end.
           end.
          
           do i = 1 to 7:
              create w-bin.
              IF i EQ 1 OR i EQ 2 OR i EQ 3 THEN
              ASSIGN w-bin.w-date-time = "29991200000000".
              ELSE
                  ASSIGN w-bin.w-date-time = "29991201000000".
              RELEASE w-bin.
           end.
           for each w-bin where w-bin.w-ord-col eq "" 
               BY w-bin.w-date-time
               by w-bin.w-qty[2] desc
               by w-bin.w-qty[1] desc:  
               w-bin.w-ord-col = string(w-oe-rell.ord-no). 
               leave.
           end.  
           for each w-bin where w-bin.w-ord-col eq "" 
               BY w-bin.w-date-time
               by w-bin.w-qty[2] desc
               by w-bin.w-qty[1] desc:
               w-bin.w-ord-col = v-rs. 
               leave.
           end.  
           for each w-bin where w-bin.w-par eq "" 
               BY w-bin.w-date-time
               by w-bin.w-qty[2] desc
               by w-bin.w-qty[1] desc:  
               w-bin.w-par = if w-oe-rell.seq eq 0 then oe-ordl.part-no
                             else itemfg.part-no.
               leave.
           end.  
          
           for each w-bin where w-bin.w-par eq "" 
               BY w-bin.w-date-time
               by w-bin.w-qty[2] desc
               by w-bin.w-qty[1] desc:  
               w-bin.w-par = if w-oe-rell.seq eq 0 then oe-ordl.i-name
                             else itemfg.i-name.
               leave.
           end.
          
           for each w-bin where w-bin.w-par eq "" 
               BY w-bin.w-date-time
               by w-bin.w-qty[2] desc
               by w-bin.w-qty[1] desc:
               w-bin.w-par = if w-oe-rell.seq eq 0 then oe-ordl.part-dscr1
                             else itemfg.part-dscr1.
               leave.
           end.
          
           for each w-bin where w-bin.w-par eq "" 
               BY w-bin.w-date-time
               BY w-bin.w-qty[2] desc
               by w-bin.w-qty[1] desc:
               w-bin.w-par = if w-oe-rell.seq eq 0 then oe-ordl.part-dscr2
                             else itemfg.part-dscr2.
               leave.
           end.
           
           for each w-bin where w-bin.w-par eq "" 
               BY w-bin.w-date-time
               by w-bin.w-qty[2] DESC
               by w-bin.w-qty[1] desc:
               w-bin.w-par = w-oe-rell.i-no.
               leave.
           end.
          
           for each w-bin where w-bin.w-par eq "" 
               BY w-bin.w-date-time
               by w-bin.w-qty[2] desc
               by w-bin.w-qty[1] desc:
               if w-oe-rell.po-no ne "" then w-bin.w-par = "PO#: " + w-oe-rell.po-no.
               leave.
           end.
          
           for each w-bin where w-bin.w-par eq ""
               BY w-bin.w-date-time
               by w-bin.w-qty[2] desc
               by w-bin.w-qty[1] desc:
              
               IF AVAILABLE oe-rell THEN
               DO:
                  w-bin.w-par = oe-rell.lot-no.              
               END.
          
               leave.
           end.
           
           for each w-bin where w-bin.w-cust-qty eq 0
               BY w-bin.w-date-time
               by w-bin.w-qty[2] desc
               by w-bin.w-qty[1] desc:
                ASSIGN iOrdQtyCust = 0.
               FOR EACH bf-oe-ordl NO-LOCK
                    WHERE bf-oe-ordl.company EQ oe-ordl.company
                      AND bf-oe-ordl.ord-no EQ oe-ordl.ord-no
                      AND bf-oe-ordl.i-no EQ oe-ordl.i-no 
                      AND bf-oe-ordl.po-no EQ oe-ordl.po-no :
                    iOrdQtyCust = iOrdQtyCust + IF bf-oe-ordl.spare-dec-1 GT 0 THEN bf-oe-ordl.spare-dec-1 ELSE bf-oe-ordl.qty  .
               END.

               ASSIGN w-bin.w-cust-qty = iOrdQtyCust  .
               IF w-bin.w-cust-qty EQ 0 THEN
                 ASSIGN w-bin.w-cust-qty = IF oe-ordl.spare-dec-1 GT 0 THEN oe-ordl.spare-dec-1 ELSE oe-ordl.qty  .
               leave.
           end.

           for each w-bin where w-bin.w-uom eq ""
               BY w-bin.w-date-time
               by w-bin.w-qty[2] desc
               by w-bin.w-qty[1] desc:

               ASSIGN w-bin.w-uom = IF oe-ordl.spare-char-2 NE "" THEN oe-ordl.spare-char-2 ELSE "EA" .
               leave.
           end.
          
           j = 7.

           for each w-bin
               break BY w-bin.w-date-time
                     by w-bin.w-qty[2] desc
                     by w-bin.w-qty[1] desc:
               if w-bin.w-par eq "" and w-bin.w-loc eq "" and w-bin.w-bin eq "" then delete w-bin.
               else j = j + 1.
           end.
          
           for each w-bin
               break BY w-bin.w-date-time
                     by w-bin.w-qty[2] desc
                     by w-bin.w-qty[1] desc:
               if LAST(w-bin.w-date-time) and (w-bin.w-loc ne "" or w-bin.w-bin ne "") then
                  j = j + 1. 
           end.
           
           v-print = yes.
           if avail oe-rel then
              do i = 1 to 4:
                 if oe-rel.ship-i[i] ne "" then do:
                    if v-print then j = j + 1.
                    assign
                       j = j + 1
                       v-print = no.
                 end.
              end.

           ASSIGN v-reljob = "" .
           for each w-bin
               break BY w-bin.w-date-time
                     by w-bin.w-qty[2] desc
                     by w-bin.w-qty[1] desc:
          
             assign
                v-bin = SUBSTRING(w-bin.w-tag,16,20) + "/" +
                        trim(w-bin.w-loc) + "/" +
                        trim(w-bin.w-bin)  .
             
             v-reljob = (w-bin.job + "-" + string(w-bin.job2,"99"))   .

             IF v-reljob EQ "-00" THEN v-reljob = "" .
             if trim(v-bin) eq "//" then
                  ASSIGN 
                 v-bin = ""
                 v-reljob = " ". 
             ELSE v-bin = v-bin + "/" + TRIM(v-reljob)  .

             if trim(v-bin) eq "///" then
                  ASSIGN 
                 v-bin = "" . 

             IF v-printline > 44 THEN DO:
                PAGE.
                v-printline = 0.
                {oe/rep/relxprnt2.i}
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
                {oe/rep/relxprnt2.i}
             END.

                RUN right-just 
                  (INPUT 11, 
                  INPUT 8,
                  INPUT trim(STRING(v-tot-rqty, "->>>>>>>>")),
                  INPUT "EA",
                  OUTPUT v-tot-rqty-a).
              
              display {2}
                     w-bin.w-ord-col
/*                      w-oe-rell.ord-no when FIRST(w-bin.w-date-time) @ w-oe-rell.ord-no */
                     w-bin.w-par
                     v-bin
                     w-bin.w-units
                     w-bin.w-unit-count
                     v-tot-rqty-a WHEN sw = NO @ v-cust-value
/*                      v-rs WHEN sw = NO */
              with frame rel-mid.
              IF w-bin.w-cust-qty GT 0 THEN
                 /*ASSIGN lv-tot-cust-qty = w-bin.w-cust-qty.*/
              ASSIGN lv-tot-cust-qty = w-bin.w-units * w-bin.w-unit-count.
              IF w-bin.w-uom NE "" THEN
                lv-save-cust-uom = caps(trim(w-bin.w-uom)).

              IF NOT swm THEN
                      ASSIGN lv-tot-cust-qty = IF oescreen-log AND oe-ordl.spare-dec-1 NE 0 THEN oe-ordl.spare-dec-1 else oe-ordl.qty 
                             lv-save-cust-uom = IF oescreen-log AND oe-ordl.spare-char-2 NE "" THEN CAPS(oe-ordl.spare-char-2) ELSE "EA" .
             IF lv-tot-cust-qty GT 0 AND sw = YES /*AND v-cq = NO*/ THEN DO:
                  
                  RUN right-just (INPUT 11, 
                    INPUT 8,
                    INPUT trim(STRING(lv-tot-cust-qty, "->>>>>>>>")),
                    INPUT lv-save-cust-uom,
                    OUTPUT v-cust-value).
                  
                 IF lPrintQtyUom THEN DO:
                  DISPLAY {2}
                     v-cust-value 
                            
                     WITH FRAME rel-mid.
                 END.
            
                  ASSIGN 
                    lv-tot-cust-qty = 0
                    lv-save-cust-uom = "" 
                    v-cq = YES 
                    swm  = YES .
              END.

            ASSIGN
                sw = YES
                v-printline = v-printline + 1.


            
             if LAST(w-bin.w-date-time) then do:
                if (w-bin.w-loc ne "" or w-bin.w-bin ne "")  then
                   
                   down {2} with frame rel-mid.
                  
               IF lv-tot-cust-qty GT 0 AND sw = YES AND v-cq = NO THEN DO:
                  DOWN WITH FRAME rel-mid.
                  RUN right-just (INPUT 11, 
                    INPUT 8,
                    INPUT trim(STRING(lv-tot-cust-qty, "->>>>>>>>")),
                    INPUT lv-save-cust-uom,
                    OUTPUT v-cust-value).

                  DISPLAY {2}
                     v-cust-value
                     WITH FRAME rel-mid.
                  
                  ASSIGN 
                    lv-tot-cust-qty = 0
                    lv-save-cust-uom = ""
                    v-cq = YES.
              END.
             IF itotrqty NE 0 THEN DO:
                 PUT "<C72><FROM><C77><LINE>" SKIP.
                 PUT "<C68.7><R-1>" itotrqty SKIP.
             END.
              IF AVAIL itemfg THEN DO:
                  FOR EACH notes WHERE
                         notes.rec_key EQ itemfg.rec_key AND notes.note_type = "S"
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
                               {oe/rep/relxprnt2.i}
                            END.
                            
                            PUT "<c7>" v-tmp-lines substring(NOTES.NOTE_TEXT,(1 + 20 * (i - 1)), 20) FORM "x(80)" SKIP.
                            v-printline = v-printline + 1.
                         END.
                  END.
              END.
                      
              IF v-printline > 44 THEN DO:
                 PAGE.
                 v-printline = 0.
                 {oe/rep/relxprnt2.i}
              END.
             end.

             down {2} with frame rel-mid.
            
           END.  /* for eacn w-bin*/
           v-rel-qty = 0 .
           
        end.  /* last-of(w-oe-rell.po-no) */
      end. /* for each w-oe-rell */

 
  IF v-printline > 44 THEN DO:
     PAGE.
     v-printline = 0.
     {oe/rep/relxprnt2.i}
  END.
 
ASSIGN v-ship-i[1] = oe-relh.ship-i[1]
       v-ship-i[2] = oe-relh.ship-i[2]
       v-ship-i[3] = oe-relh.ship-i[3]
       v-ship-i[4] = oe-relh.ship-i[4].

/*Leave shipping notes at bottom of new page*/
IF v-printline > 37 AND
   (v-ship-i[1] NE "" OR
    v-ship-i[2] NE "" OR
    v-ship-i[3] NE "" OR
    v-ship-i[4] NE "") THEN DO:
   PAGE.
   v-printline = 0.
   {oe/rep/relxprnt2.i}
END.

IF v-ship-i[1] NE "" OR
   v-ship-i[2] NE "" OR
   v-ship-i[3] NE "" OR
   v-ship-i[4] NE "" THEN
   PUT "<FArial><R50><C1><P12><B>     Shipping Instructions: </B> <P9> "SKIP(1)
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
             
  PAGE.

  ASSIGN
     v-printline = 0.
  RUN oe/setRelPrinted.p (INPUT ROWID(oe-relh),YES).
    end. /* for each oe-relh */

RETURN.
