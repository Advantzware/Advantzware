/* ---------------------------------------------- oe/rep/relStClair.i */
/* Print OE Release/Picking tickets    for St Clair Xprint          */
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
   field seq    as   int
   field set-no like fg-set.set-no
   FIELD tag LIKE oe-rell.tag
   FIELD rel-recid AS RECID
    FIELD partial AS DEC
   INDEX r-no IS PRIMARY r-no i-no
   INDEX idx set-no seq i-no po-no
    .

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
   FIELD ohqty AS INT
   FIELD shipqty AS INT
   FIELD partial AS INT
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
def var v-tot-rqty like oe-rell.qty NO-UNDO.
DEF VAR sw  AS logi NO-UNDO.
DEF VAR v-rs AS CHAR NO-UNDO FORM "x(2)".

def var v-zone-hdr as char format "x(10)" NO-UNDO.
def var v-zone like shipto.dest-code NO-UNDO.
def var v-part-dscr like oe-ordl.i-name NO-UNDO.
DEF VAR v-tmp-lines AS INT NO-UNDO.

DEF VAR v-csr AS CHAR NO-UNDO.
DEF VAR v-draw-line AS LOG NO-UNDO.

DEF SHARED VAR s-print-what-item AS cha NO-UNDO.
DEF SHARED VAR s-print-loc-from AS cha NO-UNDO.
DEF SHARED VAR s-print-loc-to AS cha NO-UNDO.
DEF SHARED VAR s-print-bin-from AS cha NO-UNDO.
DEF SHARED VAR s-print-bin-to AS cha NO-UNDO.

DEF VAR s-tot-rqty AS cha FORM "x(26)" NO-UNDO.
DEF VAR vBinLinecount AS INT NO-UNDO.
/*DEF VAR v-partial LIKE oe-rell.partial NO-UNDO.*/
DEF BUFFER boerell FOR oe-rell .

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
DEF VAR lv-email AS cha FORM "x(30)" NO-UNDO.
DEF VAR ls-image1    AS CHAR NO-UNDO.
DEF VAR ls-full-img1 AS CHAR FORMAT "x(200)" NO-UNDO.
DEF VAR lvTransDate AS DATE FORM "99/99/9999" NO-UNDO.
DEF VAR ldtJobdate AS DATE NO-UNDO.
DEF VAR ldtPoDate AS DATE NO-UNDO.
DEF VAR ldtRctDate AS DATE NO-UNDO.
ASSIGN 
    ls-image1 = "images\StClairPkgLogo.jpg"
    FILE-INFO:FILE-NAME = ls-image1
    ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">" .

DEF VAR lv-comp-color AS cha NO-UNDO.
DEF VAR lv-other-color AS cha INIT "BLACK" NO-UNDO.
DEF BUFFER xitemfg FOR itemfg.
DEF SHARED VAR v-print-components AS LOG NO-UNDO.
DEF SHARED VAR s-print-part-no AS LOG NO-UNDO.
DEF VAR lvFirstPage AS LOG INIT YES NO-UNDO.
DEF VAR lvLastPage AS LOG INIT YES NO-UNDO.
DEF VAR li-ship-qty AS INT format "->>>>>>>>" NO-UNDO.
DEF VAR li-inv-qty AS INT format "->>>>>>>>" NO-UNDO.
DEF VAR lGotFGBin AS LOG NO-UNDO.
DEF VAR lOHAvail AS LOG NO-UNDO.
DEF VAR cDate AS CHAR NO-UNDO.
DEF VAR cPallet AS CHAR NO-UNDO.
DEF VAR iPallets AS INT NO-UNDO.
DEF VAR iPalletsPartial AS INT NO-UNDO.

format w-oe-rell.ord-no                 to 6
       w-bin.w-par                      at 8    format "x(30)"
       v-bin                            at 38   format "x(20)"
       cDate                            AT 59   FORMAT "x(10)"
       w-bin.w-units                    to 76   format "->>>>>"
       w-bin.w-unit-count               to 83   format "->>>>>"
       v-partial AT 89                          format "->>>>>"
       s-tot-rqty                       AT 97   FORMAT "X(26)"
       cPallet                          AT 123  format "X(20)"
       
       /*v-rs                             AT 97   FORM "x(2)"*/
      
    with down frame rel-mid no-box no-label STREAM-IO width 143.

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

  /*IF AVAIL cust THEN
     ASSIGN v-comp-add1 = cust.addr[1]
            v-comp-add2 = "3900 PRODUCE ROAD"
            v-comp-add3 = "LOUISVILLE, KY  40218"
            v-comp-add4 = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
            v-comp-add5 = "Fax     :  " + string(cust.fax,"(999)999-9999") 
            lv-email    = "Email:  " + cust.email 
            lv-comp-name = cust.NAME.*/
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
      IF FIRST-OF(oe-relh.release#) THEN lvFirstPage = YES.
      assign
       v-draw-line = NO
       v-tot-qty = 0
       v-weight  = 0
       v-pallets = 0
       v-zone    = shipto.dest-code
       v-csr = oe-relh.user-id
       v-ship-i[1] = oe-relh.ship-i[1]
       v-ship-i[2] = oe-relh.ship-i[2]
       v-ship-i[3] = oe-relh.ship-i[3]
       v-ship-i[4] = oe-relh.ship-i[4].

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
         w-oe-rell.rel-recid = RECID(oe-rell)
         oe-rell.printed  = yes.
        
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
          end.

        v-weight = v-weight + (oe-rell.qty * itemfg.weight-100 / 100).
      end.

      {oe/rep/relStClair2.i}

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
          break by w-oe-rell.set-no
                by w-oe-rell.seq
                by w-oe-rell.i-no
                by w-oe-rell.po-no
                by w-oe-rell.ord-no:

        IF FIRST-OF(w-oe-rell.set-no) THEN
           ASSIGN v-tot-rqty = 0
                  sw = NO
                  v-partial = 0 . 

        IF FIRST-OF(w-oe-rell.ord-no) THEN
           ASSIGN v-tot-rqty = 0
                  sw = NO
                  v-partial = 0.

        ASSIGN
           v-rel-qty = v-rel-qty + w-oe-rell.qty
           v-tot-rqty = v-tot-rqty + w-oe-rell.qty
           v-partial = v-partial + w-oe-rell.partial  
            .
        
        if last-of(w-oe-rell.po-no) then do:
           EMPTY TEMP-TABLE w-bin.
          
           ASSIGN i = 0
                  lGotFGBin = NO.
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
              
               create w-bin.
               assign
                w-bin.w-tag    = fg-bin.tag
                w-bin.w-loc    = fg-bin.loc
                w-bin.w-bin    = fg-bin.loc-bin
                w-bin.w-qty[1] = fg-bin.qty
                w-bin.w-qty[2] = fg-bin.qty
                w-bin.w-unit-count = fg-bin.case-count
                w-bin.w-units = TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0)
                w-bin.w-i-no = fg-bin.i-no
                w-bin.partial = fg-bin.partial-count
                i        = i + 1.
              
               IF s-print-what-item NE "S" THEN
                  FOR EACH fg-rcpth /*FIELDS(r-no trans-date)*/ WHERE
                      fg-rcpth.company eq fg-bin.company AND
                      fg-rcpth.i-no    eq fg-bin.i-no AND
                      fg-rcpth.job-no  eq fg-bin.job-no AND
                      fg-rcpth.job-no2 eq fg-bin.job-no2 AND
                      fg-rcpth.rita-code = "R"
                      NO-LOCK,
                      first fg-rdtlh FIELDS(trans-time) where
                      fg-rdtlh.r-no eq fg-rcpth.r-no AND
                      /*fg-rdtlh.loc  eq fg-bin.loc AND
                      fg-rdtlh.loc-bin eq fg-bin.loc-bin AND*/
                      fg-rdtlh.tag     eq fg-bin.tag AND
                      fg-rdtlh.cust-no EQ fg-bin.cust-no
                      no-lock
                      by fg-rcpth.trans-date
                      BY fg-rdtlh.trans-time
                      by fg-rcpth.r-no
                      /*by fg-rcpth.job-no
                      by fg-rcpth.job-no2
                      by fg-bin.qty*/ :
                      w-bin.w-date-time = STRING(YEAR(fg-rcpth.trans-date),"9999")
                                        + STRING(MONTH(fg-rcpth.trans-date),"99")
                                        + STRING(DAY(fg-rcpth.trans-date),"99")
                                        + STRING(fg-rdtlh.trans-time,"999999").
                      ldtRctDate = fg-rcpth.trans-date.
                      LEAVE.
                  END.
               ELSE
                  w-bin.w-date-time = "29991201000000".
               RELEASE w-bin.
               lGotFGBin = YES.
           end. /*each fg-bin*/
            /*IF NOT lGotFGBin THEN ldtRctDate = job.due-date.*/
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
                    w-bin.w-loc = b-ship.loc
                    w-bin.w-bin = b-ship.loc-bin
                    i     = i + 1
                    w-bin.w-date-time = "29991201000000".
                 end.   
              end.
           end.

           FIND FIRST job-hdr WHERE job-hdr.company = oe-ordl.company
                                AND job-hdr.job-no = oe-ordl.job-no
                                AND job-hdr.job-no2 = oe-ordl.job-no2 NO-LOCK NO-ERROR.
           ldtJobDate = IF AVAIL job-hdr THEN job-hdr.due-date ELSE ?.
           FIND FIRST po-ordl WHERE po-ordl.company = oe-ordl.company
                                AND po-ordl.po-no = oe-ordl.po-no-po NO-LOCK NO-ERROR.
           ldtPoDate = IF AVAIL po-ordl THEN po-ordl.due-date ELSE ?.

           do i = i to 6:
              create w-bin.
              ASSIGN w-bin.w-date-time = "29991231000000".
              RELEASE w-bin.
           end.
          
           for each w-bin where w-bin.w-par eq ""
               BY w-bin.w-date-time
               by w-bin.w-qty[2] desc
               by w-bin.w-qty[1] desc:
               w-bin.w-par = if w-oe-rell.seq eq 0 then oe-ordl.part-no
                             else itemfg.part-no.
               IF w-bin.w-par EQ "" THEN w-bin.w-par = " ".
               leave.
           end.  
          
           for each w-bin where w-bin.w-par eq ""
               BY w-bin.w-date-time
               by w-bin.w-qty[2] desc
               by w-bin.w-qty[1] desc:
               w-bin.w-par = if w-oe-rell.seq eq 0 then oe-ordl.i-name
                             else itemfg.i-name.
               IF w-bin.w-par EQ "" THEN w-bin.w-par = " ".
               leave.
           end.
          
           for each w-bin where w-bin.w-par eq ""
               BY w-bin.w-date-time
               by w-bin.w-qty[2] desc
               by w-bin.w-qty[1] desc:
               w-bin.w-par = if w-oe-rell.seq eq 0 then oe-ordl.part-dscr1
                             else itemfg.part-dscr1.
               IF w-bin.w-par EQ "" THEN w-bin.w-par = " ".
               leave.
           end.
          
           for each w-bin where w-bin.w-par eq ""
               BY w-bin.w-date-time
               BY w-bin.w-qty[2] desc
               by w-bin.w-qty[1] desc:
               w-bin.w-par = if w-oe-rell.seq eq 0 then oe-ordl.part-dscr2
                             else itemfg.part-dscr2.
               IF w-bin.w-par EQ "" THEN w-bin.w-par = " ".
               leave.
           end.
           
           for each w-bin where w-bin.w-par eq ""
               BY w-bin.w-date-time
               by w-bin.w-qty[2] DESC
               by w-bin.w-qty[1] desc:
               w-bin.w-par = w-oe-rell.i-no.
               IF w-bin.w-par EQ "" THEN w-bin.w-par = " ".
               leave.
           end.
          
           for each w-bin where w-bin.w-par eq ""
               BY w-bin.w-date-time
               by w-bin.w-qty[2] desc
               by w-bin.w-qty[1] desc:
               if w-oe-rell.po-no ne "" then do:
                   w-bin.w-par = "PO#: " + w-oe-rell.po-no .
                   /*FIND po-ord WHERE po-ord.company = cocode
                                 AND po-ord.po-no = int(w-oe-rell.po-no) NO-LOCK NO-ERROR.
                   IF AVAIL po-ord THEN
                      FIND FIRST vend WHERE vend.company EQ po-ord.company
                             AND vend.vend-no EQ po-ord.vend-no
                             USE-INDEX vend NO-LOCK NO-ERROR.
                   w-bin.w-par = "PO#: " + w-oe-rell.po-no + 
                                 " Vendor: " + IF AVAIL vend THEN substring(vend.NAME,1,10) ELSE "".
                   */              
               END.
               IF w-bin.w-par EQ "" THEN w-bin.w-par = " ".
               leave.
           end.
          
/*            for each w-bin where w-bin.w-par eq ""               */
/*                BY w-bin.w-date-time                             */
/*                by w-bin.w-qty[2] desc                           */
/*                by w-bin.w-qty[1] desc:                          */
/*                                                                 */
/*                FIND FIRST ref-lot-no WHERE                      */
/*                     ref-lot-no.reftable EQ "oe-rell.lot-no" AND */
/*                     ref-lot-no.rec_key  EQ w-oe-rell.rec_key    */
/*                     USE-INDEX rec_key                           */
/*                     NO-LOCK NO-ERROR.                           */
/*                                                                 */
/*                IF AVAILABLE ref-lot-no THEN                     */
/*                DO:                                              */
/*                   w-bin.w-par = ref-lot-no.CODE.                */
/*                   RELEASE ref-lot-no.                           */
/*                END.                                             */
/*                                                                 */
/*                leave.                                           */
/*            end.                                                 */
          
/*            j = 7.                                                                                  */
/*                                                                                                    */
/*            for each w-bin                                                                          */
/*                break BY w-bin.w-date-time                                                          */
/*                      by w-bin.w-qty[2] desc                                                        */
/*                      by w-bin.w-qty[1] desc:                                                       */
/*                if w-bin.w-par eq "" and w-bin.w-loc eq "" and w-bin.w-bin eq "" then delete w-bin. */
/*                else j = j + 1.                                                                     */
/*            end.                                                                                    */
/*                                                                                                    */
/*            for each w-bin                                                                          */
/*                break BY w-bin.w-date-time                                                          */
/*                      by w-bin.w-qty[2] desc                                                        */
/*                      by w-bin.w-qty[1] desc:                                                       */
/*                if LAST(w-bin.w-date-time) and (w-bin.w-loc ne "" or w-bin.w-bin ne "") then        */
/*                   j = j + 1.                                                                       */
/*            end.                                                                                    */
/*                                                                                                    */
/*            v-print = yes.                                                                          */
/*            if avail oe-rel then                                                                    */
/*               do i = 1 to 4:                                                                       */
/*                  if oe-rel.ship-i[i] ne "" then do:                                                */
/*                     if v-print then j = j + 1.                                                     */
/*                     assign                                                                         */
/*                        j = j + 1                                                                   */
/*                        v-print = no.                                                               */
/*                  end.                                                                              */
/*               end.                                                                                 */
           /* print ticket */
           vBinLineCount = 0.
           for each w-bin
               break BY w-bin.w-date-time
                     by w-bin.w-qty[2] desc
                     by w-bin.w-qty[1] desc:          

             assign
                v-bin = SUBSTRING(w-bin.w-tag,16,20) + "/" +
                        trim(w-bin.w-loc) + "/" +
                        trim(w-bin.w-bin).
             
             if trim(v-bin) eq "//" then v-bin = "".
          
             IF v-printline > 36 THEN DO:
                 
                 PUT unformatted "<FArial><P9><=8> <R49><C50>Page number: " PAGE-NUMBER .
                PAGE.
                lvFirstPage = YES.
                v-printline = 0.
                {oe/rep/relStClair2.i}
             END.
             /*
             /*don't draw line for first item on release*/
             IF FIRST(w-bin.w-date-time) AND v-draw-line THEN
             DO:
                PUT "<C1><FROM><C80><LINE>" SKIP.
                v-printline = v-printline + 1.
             END.

            v-draw-line = YES.
            
             IF v-printline > 36 THEN DO:
                PAGE.
                lvFirstPage = YES.
                v-printline = 0.
                {oe/rep/relStClair2.i}
             END.
            */
             IF oe-ordl.whsed THEN 
                ASSIGN v-rs = "RS".
             ELSE 
                ASSIGN v-rs = "".

             RUN oe/ordlsqty.p (ROWID(oe-ordl),
                         OUTPUT li-inv-qty, OUTPUT li-ship-qty).
        
             IF FIRST(w-bin.w-date-time) THEN DO:
                vBinLineCount = vBinLineCount + 1.
                v-printline = v-printline + 1.   /* 16 */               
                PUT {2} "<C83.5>" 
                   (IF AVAIL itemfg THEN itemfg.q-onh ELSE w-bin.w-qty[1]) FORM "->>>>>>>>" /*at 100*/ 
                    SKIP
                    /*"<C80><FROM><C93><LINE>"*/.                
             END.

             s-tot-rqty = "<B><P14>" + STRING(v-tot-rqty,"->>>>>>>>") + "</B><P10>".
/*              s-tot-rqty = "<B>" + STRING(v-tot-rqty,"->>>>>>>>") + "</B>". */

             iPalletsPartial = 0.
             iPallets = 0.
             
             IF AVAIL itemfg AND itemfg.sell-uom = "CS" THEN DO:
                 IF itemfg.est-no NE "" THEN
                     FIND FIRST eb 
                        WHERE eb.company EQ cocode
                          AND eb.est-no  EQ itemfg.est-no
                          AND eb.stock-no EQ itemfg.i-no
                          NO-LOCK NO-ERROR.
                 IF AVAIL eb AND eb.cas-pal > 0 THEN
                    ASSIGN 
                        iPallets = INT(TRUNC(v-tot-rqty / (eb.cas-pal * itemfg.case-count),0))
                        iPalletsPartial = INT(TRUNC(((v-tot-rqty MODULO (eb.cas-pal * itemfg.case-count )) / itemfg.case-count),0)).
             END.
             
             DEF VAR vqtyoh AS DECIMAL NO-UNDO.
             vqtyoh = IF AVAIL itemfg THEN itemfg.q-onh ELSE w-bin.w-qty[1].
             IF vqtyoh >= v-tot-rqty   THEN lOHAvail = YES.
                 ELSE lOHAvail = NO.
             /*IF vqtyoh >= v-tot-rqty   THEN
                ldtJobDate = oe-relh.rel-date .
             ELSE IF ldtJobDate <> ? THEN
                ldtJobDate = ldtJobDate .   /*  ldtPODate */
             ELSE
                ldtJobDate = ldtPODate .
             */

             /*FIND FIRST boerell WHERE boerell.r-no = w-oe-rell.r-no
                         /*AND boerell.tag = w-bin.w-tag*/ NO-LOCK NO-ERROR.
             v-partial = IF AVAIL boerell THEN boerell.partial ELSE 0.
             */
             v-partial = w-bin.partial.
             ASSIGN v-printline = v-printline + 1.             
             cDate = "".
             cPallet = "".
             CASE vBinLineCount:
                 WHEN 1 THEN DO:
                    IF iPallets NE 0 THEN
                        cPallet = "<C93.5>" + STRING(iPallets,">>>9") + " Pallets".
                    /* lvTransDate = /*date(
                         int(SUBSTRING(w-bin.w-date-time,5,2)),
                         int(SUBSTRING(w-bin.w-date-time,7,2)),
                         int(SUBSTRING(w-bin.w-date-time,1,4)) ) */
                        IF ldtRctDate <> ? THEN ldtRctDate ELSE ? .
                    IF lOHAvail THEN cDate = STRING(lvTransDate).
                        ELSE cDate = "".*/
                 END.
                 WHEN 2 THEN DO:
                     IF iPalletsPartial NE 0 THEN
                         cPallet = "<C93.5>" + STRING(iPalletsPartial,">>>9") + " Partial".
                     IF oe-ordl.po-no-po NE 0 /*AND v-bin = ""*/ THEN DO:
                        FIND po-ord WHERE po-ord.company = cocode
                                 AND po-ord.po-no = oe-ordl.po-no-po NO-LOCK NO-ERROR.
                       IF AVAIL po-ord THEN
                            FIND FIRST vend WHERE vend.company EQ po-ord.company
                                 AND vend.vend-no EQ po-ord.vend-no
                                 USE-INDEX vend NO-LOCK NO-ERROR. 
                        IF v-bin = "" THEN 
                           v-bin = "PO:" + string(oe-ordl.po-no-po) +
                                   " " + IF AVAIL vend THEN substring(vend.NAME,1,10) ELSE "".           .
                        IF oe-ordl.po-no-po = 0 THEN DO:
                           FIND FIRST po-ordl WHERE po-ordl.company = oe-ordl.company
                                     AND po-ordl.i-no = oe-ordl.i-no
                                     AND po-ordl.opened NO-LOCK NO-ERROR.
                           IF AVAIL po-ordl THEN DO:
                             FIND po-ord WHERE po-ord.company = cocode
                                 AND po-ord.po-no = po-ordl.po-no NO-LOCK NO-ERROR.
                             IF AVAIL po-ord THEN
                                FIND FIRST vend WHERE vend.company EQ po-ord.company
                                         AND vend.vend-no EQ po-ord.vend-no
                                         USE-INDEX vend NO-LOCK NO-ERROR.
                               v-bin = "PO:" + string(po-ordl.po-no) +
                                      " " + IF AVAIL vend THEN substring(vend.NAME,1,10) ELSE "".                         
                           END.
                        END.
                        /*cDate = string(ldtPoDate).*/                     
                     END.
                 END.
                     
                 
                 WHEN 3 THEN do:
                     lvTransDate = ?.
                     IF v-bin = "" AND lOHAvail = NO THEN DO:
                        v-bin = "Job:" + string(oe-ordl.job-no) + "-" + STRING(oe-ordl.job-no2,"99").
                        IF oe-ordl.job-no = "" THEN DO:
                            FIND FIRST job-hdr WHERE job-hdr.company = oe-ordl.company
                                AND job-hdr.i-no = oe-ordl.i-no
                                AND job-hdr.opened NO-LOCK NO-ERROR.
                            IF AVAIL job-hdr THEN 
                                ASSIGN v-bin = "Job:" + string(job-hdr.job-no) + "-" + STRING(job-hdr.job-no2,"99")
                                       ldtJobDate = job-hdr.due-date
                                       .                            
                        END.
                       /* cDate = STRING(ldtJobDate).*/
                     END.
                 END.
                 WHEN 4 THEN DO:
                     lvTransDate = ?.
                      
                 END.
             END CASE.
             IF cDate = "" AND v-bin NE "" AND SUBSTRING(w-bin.w-date-time,3,2) NE "99" THEN
                 cDate =  SUBSTRING(w-bin.w-date-time,5,2) + "/" + 
                      SUBSTRING(w-bin.w-date-time,7,2) + "/" + 
                      SUBSTRING(w-bin.w-date-time,1,4).
             IF cDate EQ "//" THEN ASSIGN cDate = "" .

             display {2}
                     w-oe-rell.ord-no when FIRST(w-bin.w-date-time)
                     w-bin.w-par
                     v-bin 
/*                      SUBSTRING(w-bin.w-date-time,5,2) + "/" + */
/*                      SUBSTRING(w-bin.w-date-time,7,2) + "/" + */
/*                      SUBSTRING(w-bin.w-date-time,1,4)         */
                     /*when FIRST(w-bin.w-date-time)*/ 
                    cDate
                     w-bin.w-units
                     w-bin.w-unit-count
                     s-tot-rqty WHEN sw = NO  
                     /*v-rs WHEN sw = NO*/
                     /*li-ship-qty  WHEN last(w-bin.w-date-time) @ v-tot-rqty*/
                    /*w-oe-rell.partial when FIRST(w-bin.w-date-time) @ v-pallet */    
                    v-partial WHEN FIRST(w-bin.w-date-time) OR v-partial <> 0
                    cPallet
                    with frame rel-mid.              
            /*MESSAGE "deug2: " 
                recid(w-bin) ":" w-bin.w-i-no ":" w-bin.w-tag ":" w-bin.w-unit-count ":" w-bin.partial ":"
                 v-partial
                  VIEW-AS ALERT-BOX INFO BUTTONS OK.*/
            ASSIGN
                sw = YES
                vBinLineCount = vBinLineCount + 1
                .
           /* 
            if LAST(w-bin.w-date-time) then do:              
                 IF v-printline >= 36 THEN DO:
                   PAGE.
                   lvFirstPage = YES.
                   v-printline = 0.
                   {oe/rep/relStClair2.i}
                END.
             end.
             */

             down {2} with frame rel-mid.
             /*vBinLineCount = vBinLineCount + 1*/.

             IF vBinLineCount >= 7 THEN do:
                /*/*IF vBinLinecount = 6 THEN */ PUT SKIP.*/
                LEAVE.
             END.

           END.  /* for eacn w-bin*/
           /*IF vBinlineCount = 6 THEN do:
              PUT " " SKIP.
              v-printline = v-printline + 1.
           END.
           ELSE*/ IF vBinlineCount < 6 THEN do:
              PUT SKIP(7 - vBinLineCount ).
              v-printline = v-printline + (7 - vBinLineCount).
              vBinLineCount = vBinLineCount + (7 - vBinLineCount).
           END.
           IF v-printline > 37 THEN DO:
               
             PUT unformatted "<FArial><P9><=8> <R49><C50>Page number: " PAGE-NUMBER .
             PAGE.
             lvFirstPage = YES.
             v-printline = 0.
             {oe/rep/relStClair2.i}
           END.
           v-rel-qty = 0.
        end.  /* last-of(w-oe-rell.po-no) */
        IF vBinlineCount < 6 THEN do:
           v-printline = v-printline + (7 - vBinLineCount).
           PUT SKIP(7 - vBinLineCount ).           
        END.
        IF v-printline > 37 THEN DO:
            
            PUT unformatted "<FArial><P9><=8> <R49><C50>Page number: " PAGE-NUMBER .
          PAGE.
          lvFirstPage = YES.
          v-printline = 0.
          {oe/rep/relStClair2.i}
        END.
  end. /* for each w-oe-rell */

 
  IF v-printline > 37 THEN DO:
      
      PUT unformatted "<FArial><P9><=8> <R49><C50>Page number: " PAGE-NUMBER .
     PAGE.
     lvFirstPage = YES.
     v-printline = 0.
     {oe/rep/relStClair2.i}
  END.
 

/*Leave shipping notes at bottom of new page*/
/*IF v-printline > 37 AND
   (v-ship-i[1] NE "" OR
    v-ship-i[2] NE "" OR
    v-ship-i[3] NE "" OR
    v-ship-i[4] NE "") THEN DO:
   PAGE.
   v-printline = 0.
   {oe/rep/relStClair2.i}
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
*/

/*IF lvLastPage THEN*/ DO:

PUT unformatted
    "<|10><R46><C1><#8><FROM><R48><C104><RECT>" 
    "<FArial><P9><=8>  Pulled By<C17>Shipped by<C33>Trailer#<C50>Dock#<C65>Pallets" SKIP
    "<R46><C16><FROM><R48><C16><Line>" 
    "<R46><C32><FROM><R48><C32><Line>" 
    "<R46><C48><FROM><R48><C48><Line>"
    "<R46><C64><FROM><R48><C64><Line>"
    "<R49><C50>Page number: " PAGE-NUMBER .
lvLastPage = NO.
END.

  PAGE.

  ASSIGN
     v-printline = 0.
  RUN oe/setRelPrinted.p (INPUT ROWID(oe-relh),YES).
    end. /* for each oe-relh */

RETURN.
