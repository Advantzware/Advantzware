/* ---------------------------------------------- oe/rep/relpchtr.i  */
/* Print OE Release/Picking tickets    for Peachtree Xprint             */
/* ----------------------------------------------------------------- */

{oe/rep/oe-pick1.i}
    
{sys/FORM/r-top.i}

DEF BUFFER ref-lot-no FOR reftable.

def TEMP-TABLE w-oe-rell like oe-rell
   field seq    as   int
   field set-no like fg-set.set-no.

def TEMP-TABLE w-bin 
   field w-loc like fg-bin.loc
   field w-bin like fg-bin.loc-bin
   field w-tag like fg-bin.tag
   field w-qty like fg-bin.qty extent 2
   field w-c-c like fg-bin.case-count
   field w-cas as   dec
   field w-pal as   dec
   field w-par /*AS CHAR*/  like oe-ordl.part-dscr1
   field w-x   as   log
   FIELD w-i-no AS cha
   FIELD w-po-no AS cha.

def buffer b-cust  for cust.
def buffer b-ship  for shipto.
def buffer b-w-bin for w-bin.

DEF VAR v-cust-part AS CHAR FORMAT "x(20)" .

def var v-frt-pay-dscr as char format "x(11)" no-undo.
def var v-bin as char no-undo.
def var v-bin2 as char no-undo.
def var v-print as log no-undo.
def var v-part-info like itemfg.i-name no-undo.
def var v-qty like oe-rell.qty no-undo.
def var v-rel-qty like v-qty NO-UNDO.

def var v-units-hdr as char format "x(10)" extent 2.
def var v-zone-hdr as char format "x(10)".
def var v-zone like shipto.dest-code.
def var v-part-dscr like oe-ordl.i-name.
/* === with xprint ====*/
DEF VAR v-term AS cha NO-UNDO.
DEF VAR v-csr AS CHAR NO-UNDO.
DEF VAR v-rel-qty-print AS CHAR FORMAT "x(8)".
ASSIGN v-rel-qty-print ="Rel Qty".
DEF SHARED VAR s-print-what-item AS cha NO-UNDO.
DEF SHARED VAR s-print-loc-from AS cha NO-UNDO.
DEF SHARED VAR s-print-loc-to AS cha NO-UNDO.
DEF SHARED VAR s-print-bin-from AS cha NO-UNDO.
DEF SHARED VAR s-print-bin-to AS cha NO-UNDO.
DEF VAR v-page-num AS INT NO-UNDO.

format w-oe-rell.ord-no                 to 6    FORMAT ">>>>>9"
       w-par                            AT 8    FORMAT "x(20)" "</B>"
       v-bin                            AT 35   format "x(24)" 
       w-x                              at 68   format "X/"
       w-pal                            TO 74   format "->>>>"
       w-cas                            to 81   format "->>>>"
       w-c-c                            to 91   format "->>>>>>>>"
       w-qty[1]                         to 103   format "->>>>>>>>"
    
    with down frame rel-mid no-box no-label STREAM-IO width 103.

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add5 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-line-total AS DEC NO-UNDO.
DEF VAR v-quo-total AS DEC NO-UNDO.
def var v-t-tax      as   dec extent 3 NO-UNDO.
def var v-bot-lab    as   char format "x(63)" extent 3 NO-UNDO.
DEF VAR v-q-no LIKE oe-ord.q-no NO-UNDO.
DEF VAR v-printline AS INT NO-UNDO.
DEF VAR v-ship-i AS cha FORM "x(60)" EXTENT 4 NO-UNDO.

DEF VAR ll-display-comp AS LOG NO-UNDO.  /* display company address */
DEF VAR ll-consol-rells AS LOG NO-UNDO.
DEF VAR lv-comp-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-email AS cha FORM "x(56)" NO-UNDO.

DEF VAR lv-comp-color AS cha NO-UNDO.
DEF VAR lv-other-color AS cha INIT "BLACK" NO-UNDO.
DEF BUFFER xitemfg FOR itemfg.
DEF VAR lv-comp-unit AS INT NO-UNDO.
DEF SHARED VAR v-print-components AS LOG NO-UNDO.
DEF SHARED VAR s-print-part-no AS LOG NO-UNDO.
ASSIGN tmpstore = fill("-",130).

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "RELPRINT" no-lock no-error.
ASSIGN
 ll-display-comp = AVAIL sys-ctrl AND sys-ctrl.log-fld
 ll-consol-rells = AVAIL sys-ctrl AND sys-ctrl.int-fld NE 0.

FIND first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "LOGOCOLR" no-lock no-error.
IF AVAIL sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".

find first company where company.company = cocode no-lock no-error.
ASSIGN v-comp-add1 = ""
       v-comp-add2 = ""
       v-comp-add3 = ""
       v-comp-add4 = ""
       v-comp-add5 = ""
       lv-email = ""
       lv-comp-name = "".

IF ll-display-comp THEN DO:
   FIND FIRST cust WHERE cust.company = cocode AND
                         cust.active = "X" NO-LOCK NO-ERROR.
  IF AVAIL cust THEN
     ASSIGN v-comp-add1 = cust.addr[1]
            v-comp-add2 = cust.addr[2]
            v-comp-add3 = cust.city + ", " + cust.state + "  " + cust.zip
            v-comp-add4 = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
            v-comp-add5 = "Fax     :  " + string(cust.fax,"(999)999-9999") 
            lv-email    = "Email:  " + cust.email 
            lv-comp-name = cust.NAME.
 END.

format
  tt-rell.ord-no
  tt-rell.po-no at 8 
  tt-rell.loc-bin  AT 23  FORM "x(5)"
  tt-rell.i-no at 29  oe-ordl.i-name at 44
  oe-ordl.qty format "->>>>>>>9" to 83
  tt-rell.qty format "->>>>>>>9" SKIP
  tt-rell.tag AT 29 FORM "x(15)"
  oe-ordl.part-dscr1 at 44 format "x(30)" skip
    oe-ordl.part-no AT 29 
    oe-ordl.part-dscr2 at 44 format "x(30)"
  with down frame relprint no-box no-label STREAM-IO width 110.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.
v-printline = 0.

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
      FOR EACH w-oe-rell:
        DELETE w-oe-rell.
      END.
         
      for each oe-rell
          where oe-rell.company eq cocode
            and oe-rell.r-no    eq oe-relh.r-no,

          first itemfg of oe-rell no-lock:
          
        create w-oe-rell.
        buffer-copy oe-rell to w-oe-rell.
        
        assign
         i                = 0 
         w-oe-rell.seq    = i
         w-oe-rell.set-no = oe-rell.i-no
         oe-rell.printed  = yes.

        /* gdm - 03230907 */
        IF v-print-components AND
          itemfg.isaset       AND 
          itemfg.alloc NE YES 
         THEN 
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

      {oe/rep/relpchtr2.i}

      for each w-bin:
          delete w-bin.
      end.

      for each w-oe-rell,
      
          first oe-ordl
          where oe-ordl.company eq cocode
            and oe-ordl.ord-no  eq w-oe-rell.ord-no
            and oe-ordl.i-no    eq w-oe-rell.set-no
            and oe-ordl.line    eq w-oe-rell.line
          no-lock,
      
          first itemfg of w-oe-rell no-lock
                
          break by w-oe-rell.set-no
                by w-oe-rell.seq
                by w-oe-rell.i-no
                BY w-oe-rell.ord-no
                by w-oe-rell.po-no:

        v-rel-qty = v-rel-qty + w-oe-rell.qty.
        
        if last-of(w-oe-rell.po-no) then do:
          for each w-bin:
            delete w-bin.
          end.
        
          i = 0.

          for each fg-bin
              where fg-bin.company  eq cocode
                and fg-bin.i-no     eq w-oe-rell.i-no
                AND ((s-print-what-item = "R" ) 
                     OR (s-print-what-item = "I" AND fg-bin.loc >= s-print-loc-from AND fg-bin.loc <= s-print-loc-to
                         AND fg-bin.loc-bin >= s-print-bin-from AND fg-bin.loc-bin <= s-print-bin-to)
                    )
                and fg-bin.qty      gt 0
              no-lock:

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
                                   AND oe-rell.tag      EQ fg-bin.tag)
            THEN NEXT.
        
            create w-bin.
            assign
             w-tag    = fg-bin.tag
             w-loc    = fg-bin.loc
             w-bin    = fg-bin.loc-bin
             w-qty[1] = fg-bin.qty
             w-qty[2] = fg-bin.qty 
             w-c-c    = fg-bin.case-count
             w-x      = CAN-FIND(FIRST oe-rell
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
                                   AND oe-rell.tag      EQ fg-bin.tag)
             w-i-no = fg-bin.i-no
             w-po-no = w-oe-rell.po-no
             i        = i + 1.

            assign
             w-pal = (if fg-bin.case-count   eq 0 then 1 else fg-bin.case-count)   *
                     (if fg-bin.cases-unit   eq 0 then 1 else fg-bin.cases-unit)   *
                     (if fg-bin.units-pallet eq 0 then 1 else fg-bin.units-pallet)
             w-pal = w-qty[1] / w-pal.

            {sys/inc/roundup.i w-pal}
          end.
          
          for each w-bin break by w-loc by w-bin:
            v-qty = v-qty + w-qty[1].
            
            if last-of(w-bin) then do:
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
                 w-loc = b-ship.loc
                 w-bin = b-ship.loc-bin
                 i     = i + 1.
              end.   
            end.
          end.
         
          do i = i to 6:
            create w-bin.
          end.
           
          for each w-bin where w-par eq "" by w-qty[2] desc by w-qty[1] desc:
              if w-oe-rell.seq eq 0 THEN
                    w-par = "<B>" + oe-ordl.part-no   .
               
              ELSE  w-par = "<B>" + itemfg.part-no .
            leave.
          end.  
        
          for each w-bin where w-par eq "" by w-qty[2] desc by w-qty[1] desc:
            w-par = if w-oe-rell.seq eq 0 THEN  oe-ordl.i-no 
                                          ELSE  itemfg.i-no.
            leave.
          end.
        
          /*for each w-bin where w-par eq "" by w-qty[2] desc by w-qty[1] desc:
            w-par = if w-oe-rell.seq eq 0 then oe-ordl.po-no
                                          else w-oe-rell.po-no .
            leave.
          end.
          
          for each w-bin where w-par eq "" by w-qty[2] desc by w-qty[1] desc:
            w-par = w-oe-rell.i-no.
            leave.
          end.*/
         
          for each w-bin where w-par eq "" by w-qty[2] desc by w-qty[1] desc:
            if w-oe-rell.po-no ne "" then w-par = "PO#:" + w-oe-rell.po-no.
            leave.
          end.
          

         /* for each w-bin where w-par eq "" by w-qty[2] desc by w-qty[1] desc:
            
              FIND FIRST ref-lot-no WHERE
                   ref-lot-no.reftable EQ "oe-rell.lot-no" AND
                   ref-lot-no.rec_key  EQ w-oe-rell.rec_key
                   USE-INDEX rec_key
                   NO-LOCK NO-ERROR.

              IF AVAILABLE ref-lot-no THEN
              DO:
                 w-par = ref-lot-no.CODE.
                 RELEASE ref-lot-no.
              END.

              leave.
          end.*/

          j = 6.
          for each w-bin break by w-qty[2] desc by w-qty[1] desc:
            if w-par eq "" and w-loc eq "" and w-bin eq "" then delete w-bin.
            else j = j + 1.
          end.

          for each w-bin break by w-qty[2] desc by w-qty[1] desc:
            if last(w-qty[2]) and (w-loc ne "" or w-bin ne "") then j = j + 1. 
          end.
          
          v-print = yes.
          if avail oe-rel then do i = 1 to 4:
            if oe-rel.ship-i[i] ne "" then do:
              if v-print then j = j + 1.
              assign
               j = j + 1
               v-print = no.
            end.
          end.
          
          for each w-bin break by w-qty[2] desc by w-qty[1] desc:
            assign
             w-cas = w-qty[1] / w-c-c
             v-bin =  trim(substr(w-tag,16,5)) + "/" +
                     trim(w-loc)              + "/" +
                     trim(w-bin) .
            
            if w-cas eq ? then w-cas = 0.
           
            if trim(v-bin) eq "//" then v-bin = "".
          
            {sys/inc/roundup.i w-cas}
              
            IF PAGE-SIZE - LINE-COUNTER < 9 /* AND first(w-qty[2]) */ THEN DO:
                PAGE .
                v-printline = 0.
               {oe/rep/relpchtr2.i}
            END.        
            if FIRST(w-qty[2]) THEN
            IF v-printline > 27 THEN DO: 
               PAGE.
               v-printline = 0.
               {oe/rep/relpchtr2.i}
            END.
          
               IF  first(w-qty[2]) THEN
                    DO:
                        PUT {2} "<R+1><C1>" w-oe-rell.ord-no            FORMAT ">>>>>9"  SPACE(1)  .
                        IF w-oe-rell.job-no NE "" THEN
                            PUT {2} "<R+1><C1>" w-oe-rell.job-no FORMAT "x(6)" "-" w-oe-rell.job-no2 FORMAT "99" SPACE(1) "<R-1>" .
                    END.

               PUT {2}
                   "<C10>"  w-par                                   FORMAT "x(20)" "</B>"
                   "<C26>" v-bin                                   format "x(20)"          
                   "<C50>" w-x                                     format "X/"             
                   "<C52>" w-pal                                   format "->>>>"          
                   "<C58>" w-cas                                   format "->>>>"          
                   "<C62>" w-c-c                                   format "->>>>>>>>"      
                   "<C72>" w-qty[1]                                format "->>>>>>>>"   SKIP.  

               v-printline = v-printline + 1.
               
            if last(w-qty[2]) then do:
              if w-loc ne "" or w-bin ne "" then down {2} with frame rel-mid.
               
              /*IF v-printline > 34 THEN DO:
                  PAGE.
                 v-printline = 0.
                 {oe/rep/relpchtr2.i}
              END. */
               
                  /*display {2}
                 "Rel Qty" @ w-c-c 
                     v-rel-qty  @ w-qty[1]   */
                      
                 PUT 
                     "<C60><B><P13>" v-rel-qty-print "<C63>" v-rel-qty "</B><P10>"  /*@ w-qty[1]*/ SKIP(0.5).
PUT "<R-1>________________________________________________________________________________________________"  . 
              
              v-printline = v-printline + 1.

            END.
            down {2} with frame rel-mid.
          end.  /* for eacn w-bin*/
          v-rel-qty = 0.
        end.  /* last-of(w-oe-rell.po-no) */
      end. /* for each w-oe-rell */

ASSIGN v-ship-i[1] = oe-relh.ship-i[1]
       v-ship-i[2] = oe-relh.ship-i[2]
       v-ship-i[3] = oe-relh.ship-i[3]
       v-ship-i[4] = oe-relh.ship-i[4].

/*Leave shipping notes at bottom of new page*/
IF v-printline > 39 AND
   (v-ship-i[1] NE "" OR
    v-ship-i[2] NE "" OR
    v-ship-i[3] NE "" OR
    v-ship-i[4] NE "") THEN DO:
   PAGE.
   v-printline = 0.
   {oe/rep/relpchtr2.i}
END.

 IF v-ship-i[1] NE "" OR
   v-ship-i[2] NE "" OR
   v-ship-i[3] NE "" OR
   v-ship-i[4] NE "" THEN
   PUT "<FArial><R52><C1><P12><B>     Shipping Instructions: </B> <P9> "SKIP(1)
       "<R53><C1>" v-ship-i[1] AT 7 
       "<R54><C1>" v-ship-i[2] AT 7 
       "<R55><C1>" v-ship-i[3] AT 7 
       "<R56><C1>" v-ship-i[4] AT 7.
       

PUT "<FArial><R58><C1><P9>"
    "__________________________________________________________________________________________________________________"  SKIP 

    "<|10><C1><R60><#8><FROM><C80><R62><RECT> " 
    "<=8> Pulled By                                         Checked By                                        # of Units                                         Total Weight/Cube" SKIP
    "<R60><C20><FROM><R62><C20><Line>" 
    "<R60><C40><FROM><R62><C40><Line>" 
    "<R60><C60><FROM><R62><C60><Line>".

   ASSIGN v-page-num = PAGE-NUM.
  
  PAGE.
  

  ASSIGN
     v-printline = 0.
  RUN oe/setRelPrinted.p (INPUT ROWID(oe-relh),YES).
    
    end. /* for each oe-relh */

RETURN.

PROCEDURE create-tt-rell.
  DEF INPUT PARAM ip-qty-case LIKE oe-rell.qty-case NO-UNDO.
  DEF INPUT PARAM ip-cases    LIKE oe-rell.cases NO-UNDO.


  IF ip-qty-case LT 0 THEN
    ASSIGN
     ip-qty-case = ip-qty-case * -1
     ip-cases    = ip-cases * -1.

  FIND FIRST tt-rell
      WHERE tt-rell.i-no     EQ xoe-rell.i-no
        AND tt-rell.po-no    EQ xoe-rell.po-no
        AND tt-rell.ord-no   EQ xoe-rell.ord-no
        AND tt-rell.line     EQ xoe-rell.line
        AND tt-rell.qty-case EQ ip-qty-case
      NO-LOCK NO-ERROR.

  IF NOT AVAIL tt-rell THEN DO:
    
    CREATE tt-rell.
    BUFFER-COPY xoe-rell EXCEPT rec_key TO tt-rell
    ASSIGN
     tt-rell.qty-case = ip-qty-case
     tt-rell.cases    = 0
     tt-rell.qty      = 0
     tt-rell.partial  = 0.
  END.

  ASSIGN
   tt-rell.cases = tt-rell.cases + ip-cases
   tt-rell.qty   = tt-rell.qty + (ip-qty-case * ip-cases).

  IF xoe-rell.p-c THEN tt-rell.p-c = YES.

END PROCEDURE.
