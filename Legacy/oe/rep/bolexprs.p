/* ----------------------------------------------- oe/rep/bolexprs.p 07/05 JLF */
/*                                                                             */
/* Print BOL when sys-ctrl.char-fld eq "Express" - O/E Module                  */
/*                                                                             */
/* --------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xoe-boll for oe-boll.
def buffer xitemfg  for itemfg.

{oe/rep/oe-lad.i}

def var v-time as char format "x(8)" no-undo.
def var v-carrier like carrier.dscr no-undo.
def var v-frt-pay-dscr as char no-undo.
def var v-tot-pkgs as int format ">>>>>9" no-undo.

def var v-bol-qty          as   int.
def var v-i-no             as   char.
def var v-qty              as   int.
def var v-i-dscr           as   char extent 5.
def var v-unit-qty         as   char.
def var v-line-lines       as   int.
def var v-dscr-lines       as   int.
def var v-po-lines         as   int.
def var v-ord-lines        as   int.
def var v-printline        as   int.
def var v-part-info        like oe-ordl.part-dscr1.
def var v                  as   int.
def var v-part-qty         as   dec.
def var v-cases            like oe-boll.cases.
def var v-qty-case         like oe-boll.qty-case.
def var v-partial          like oe-boll.partial.

def var v-cust-no          like cust.cust-no.
def var v-cust-name        like cust.name.
def var v-cust-addr        like cust.addr.
def var v-cust-city        like cust.city.
def var v-cust-state       like cust.state.
def var v-cust-zip         like cust.zip.
def var v-cust-addr3       as   char format "x(30)".
def var v-ship-id          like shipto.ship-id.
def var v-ship-name        like shipto.ship-name.
def var v-ship-addr        like shipto.ship-addr.
def var v-ship-city        like shipto.ship-city.
def var v-ship-state       like shipto.ship-state.
def var v-ship-zip         like shipto.ship-zip.
def var v-ship-addr3       as   char format "x(30)".
def var v-fob              as   char format "x(11)".
def var v-tot-sqft         like itemfg.t-sqft init 0 format ">>>,>>9.99".
def var v-page-no          as   int format ">9" no-undo.
DEF VAR li AS INT NO-UNDO.
DEF VAR v-po-assigned AS LOG NO-UNDO.
DEF VAR v-ord-assigned AS LOG NO-UNDO.
DEF VAR v-unit-displayed AS LOG NO-UNDO.

FORM v-qty             to 9    format ">>>,>>9"
     tt-boll.i-no      at 23   format "x(10)"
     v-part-info       at 35   format "x(26)"
     v-unit-qty        to 71   format "x(10)"
     tt-boll.qty       to 80   format ">>>,>>9"
    with frame detail no-attr-space no-labels no-box no-underline down STREAM-IO width 80.


find first company where company.company eq cocode no-lock no-error.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

for each report   where report.term-id eq v-term-id,
    first oe-bolh where recid(oe-bolh) eq report.rec-id
    break by oe-bolh.bol-no:

  if first-of(oe-bolh.bol-no) then do:
    find first cust
        where cust.company eq cocode
          and cust.cust-no eq oe-bolh.cust-no
        no-lock no-error.

    RUN oe/custxship.p (oe-bolh.company,
                        oe-bolh.cust-no,
                        oe-bolh.ship-id,
                        BUFFER shipto).

    find first carrier
        where carrier.company eq oe-bolh.company
          and carrier.carrier eq oe-bolh.carrier
        no-lock no-error.

    v-frt-pay-dscr = "".
    FOR EACH oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no NO-LOCK,

        first oe-ord
	    where oe-ord.company eq oe-boll.company
	      and oe-ord.ord-no  eq oe-boll.ord-no
	    NO-LOCK:    

	  assign
       v-frt-pay-dscr = if oe-ord.frt-pay eq "p" or
                           oe-ord.frt-pay eq "b" then "Prepaid" else "Collect"
       v-fob          = if oe-ord.fob-code begins "ORIG" then "Origin"
                                                         else "Destination".
	
      LEAVE.
    END.

    v-time = string(time,"hh:mm am").

    if first-of(oe-bolh.bol-no) then do:

      assign
       v-cust-no      = cust.cust-no
       v-cust-name    = cust.name
       v-cust-addr[1] = cust.addr[1]
       v-cust-addr[2] = cust.addr[2]
       v-cust-city    = cust.city
       v-cust-state   = cust.state
       v-cust-zip     = cust.zip
       v-ship-id      = shipto.ship-id
       v-ship-name    = shipto.ship-name
       v-ship-addr[1] = shipto.ship-addr[1]
       v-ship-addr[2] = shipto.ship-addr[2]
       v-ship-city    = shipto.ship-city
       v-ship-state   = shipto.ship-state
       v-ship-zip     = shipto.ship-zip.

      assign
       v-cust-addr3 = v-cust-city + ", " +
                      v-cust-state + "  " +
                      v-cust-zip
       v-ship-addr3 = v-ship-city + ", " +
                      v-ship-state + "  " +
                      v-ship-zip.

      if trim(v-cust-addr3) eq "," then v-cust-addr3 = "".
      if trim(v-ship-addr3) eq "," then v-ship-addr3 = "".

      if first(oe-bolh.bol-no) then do:
        form header
             skip(3)
             oe-bolh.bol-no        to 75
             skip(1)
             today                 to 75
             skip(7)
             v-cust-name           at 12
             v-ship-name           at 49
             v-cust-addr[1]        at 12
             v-ship-addr[1]        at 49
             v-cust-addr[2]        at 12
             v-ship-addr[2]        at 49
             v-cust-addr3          at 12
             v-ship-addr3          at 49
             skip(6)
             oe-bolh.bol-date      at 4
             carrier.dscr          at 14   format "x(19)" when avail carrier
             v-fob                 at 34
             oe-ord.terms-d        at 51   format "x(30)" when avail oe-ord
             skip(1)
             oe-ord.ord-date       at 24   when avail oe-ord
             oe-ord.sman[1]        at 40   when avail oe-ord
             v-cust-no             at 54
             skip(2)
           with frame bolhead page-top no-labels no-box no-underline STREAM-IO width 80.
      end.
    end.

    hide frame bolhead no-pause.
    view frame bolhead.

    page.

    FOR EACH tt-boll:
      DELETE tt-boll.
    END.
  end. /* first-of(oe-bolh.bol-no) */

  FOR EACH oe-boll
      WHERE oe-boll.company EQ oe-bolh.company
        AND oe-boll.b-no    EQ oe-bolh.b-no:
    IF (oe-boll.qty-case * oe-boll.cases) NE 0 THEN
      RUN create-tt-boll (oe-boll.qty-case, oe-boll.cases).

    IF oe-boll.qty - (oe-boll.qty-case * oe-boll.cases) NE 0 THEN
      RUN create-tt-boll (oe-boll.qty - (oe-boll.qty-case * oe-boll.cases), 1).

    oe-boll.printed = YES.
  END.

  IF LAST-OF(oe-bolh.bol-no) THEN DO:
    ASSIGN v-printline = 0
           li          = 0
           v-po-assigned = NO
           v-ord-assigned = NO
           v-unit-displayed = NO.

    FOR EACH tt-boll
        BREAK BY tt-boll.i-no
              BY tt-boll.po-no
              BY tt-boll.ord-no
              BY tt-boll.line
              BY tt-boll.cases DESC:

      li = li + 1.
      /*v-printline = v-printline + 1.*/
      
      find first oe-ordl
          where oe-ordl.company eq cocode
            and oe-ordl.ord-no  eq tt-boll.ord-no
            and oe-ordl.i-no    eq tt-boll.i-no
            and oe-ordl.line    eq tt-boll.line
          no-lock no-error.
      
      if avail oe-ordl then
      find first oe-ord
          where oe-ord.company eq oe-ordl.company
            and oe-ord.ord-no  eq oe-ordl.ord-no
          no-lock no-error.

      find first itemfg
          where itemfg.company eq tt-boll.company
            and itemfg.i-no    eq tt-boll.i-no
          no-lock no-error.

      if avail oe-ordl THEN ASSIGN /*v-i-dscr[1] = oe-ordl.part-no*/
                                   v-i-dscr[1] = oe-ordl.i-name
                                   v-i-dscr[2] = oe-ordl.i-dscr
                                   v-i-dscr[3] = oe-ordl.part-dscr1
                                   v-i-dscr[4] = oe-ordl.part-dscr2.
      ELSE ASSIGN /*v-i-dscr[1] = itemfg.part-no*/
                  v-i-dscr[1] = itemfg.i-name
                  v-i-dscr[2] = itemfg.i-dscr
                  v-i-dscr[3] = itemfg.part-dscr1
                  v-i-dscr[4] = itemfg.part-dscr2.

     IF v-i-dscr[1] = "" THEN 
        ASSIGN v-i-dscr[1] = v-i-dscr[2]
               v-i-dscr[2] = v-i-dscr[3]
               v-i-dscr[3] = v-i-dscr[4]
               v-i-dscr[4] = v-i-dscr[5]
               v-i-dscr[5] = "".
     IF v-i-dscr[1] = "" THEN   /*2*/
        ASSIGN v-i-dscr[1] = v-i-dscr[2]
               v-i-dscr[2] = v-i-dscr[3]
               v-i-dscr[3] = v-i-dscr[4]
               v-i-dscr[4] = v-i-dscr[5]
               v-i-dscr[5] = "".
     IF v-i-dscr[1] = "" THEN   /*3*/
        ASSIGN v-i-dscr[1] = v-i-dscr[2]
               v-i-dscr[2] = v-i-dscr[3]
               v-i-dscr[3] = v-i-dscr[4]
               v-i-dscr[4] = v-i-dscr[5]
               v-i-dscr[5] = "".
      IF v-i-dscr[1] = "" THEN   /*4*/
        ASSIGN v-i-dscr[1] = v-i-dscr[2]
               v-i-dscr[2] = v-i-dscr[3]
               v-i-dscr[3] = v-i-dscr[4]
               v-i-dscr[4] = v-i-dscr[5]
               v-i-dscr[5] = "".

     IF v-i-dscr[2] = "" THEN
        ASSIGN v-i-dscr[2] = v-i-dscr[3]
               v-i-dscr[3] = v-i-dscr[4]
               v-i-dscr[4] = v-i-dscr[5]
               v-i-dscr[5] = "" .
     IF v-i-dscr[3] = "" THEN
        ASSIGN v-i-dscr[3] = v-i-dscr[4]
               v-i-dscr[4] = v-i-dscr[5]
               v-i-dscr[5] = "".
     IF v-i-dscr[4] = "" THEN
        ASSIGN v-i-dscr[4] = v-i-dscr[5]
               v-i-dscr[5] = "".

     IF v-i-dscr[4] = "" AND tt-boll.po-no <> "" 
           THEN ASSIGN v-i-dscr[4] = "PO #: " + tt-boll.po-no.
     ELSE IF v-i-dscr[5] = "" AND tt-boll.po-no <> "" 
           THEN ASSIGN v-i-dscr[5] = "PO #: " + tt-boll.po-no.

     IF v-i-dscr[4] = "" AND tt-boll.ord-no <> 0 
           THEN ASSIGN v-i-dscr[4] = "Ord#: " + trim(string(tt-boll.ord-no,">>>>>9")).
     ELSE IF v-i-dscr[5] = "" AND tt-boll.ord-no <> 0 
           THEN ASSIGN v-i-dscr[5] = "Ord#: " + trim(string(tt-boll.ord-no,">>>>>9")).

     IF v-i-dscr[1] = "" THEN 
        ASSIGN v-i-dscr[1] = v-i-dscr[2]
               v-i-dscr[2] = v-i-dscr[3]
               v-i-dscr[3] = v-i-dscr[4]
               v-i-dscr[4] = v-i-dscr[5]
               v-i-dscr[5] = "".
     IF v-i-dscr[1] = "" THEN   /*2*/
        ASSIGN v-i-dscr[1] = v-i-dscr[2]
               v-i-dscr[2] = v-i-dscr[3]
               v-i-dscr[3] = v-i-dscr[4]
               v-i-dscr[4] = v-i-dscr[5]
               v-i-dscr[5] = "".
     IF v-i-dscr[1] = "" THEN   /*3*/
        ASSIGN v-i-dscr[1] = v-i-dscr[2]
               v-i-dscr[2] = v-i-dscr[3]
               v-i-dscr[3] = v-i-dscr[4]
               v-i-dscr[4] = v-i-dscr[5]
               v-i-dscr[5] = "".
      IF v-i-dscr[1] = "" THEN   /*4*/
        ASSIGN v-i-dscr[1] = v-i-dscr[2]
               v-i-dscr[2] = v-i-dscr[3]
               v-i-dscr[3] = v-i-dscr[4]
               v-i-dscr[4] = v-i-dscr[5]
               v-i-dscr[5] = "".

     IF v-i-dscr[2] = "" THEN
        ASSIGN v-i-dscr[2] = v-i-dscr[3]
               v-i-dscr[3] = v-i-dscr[4]
               v-i-dscr[4] = v-i-dscr[5]
               v-i-dscr[5] = "" .
     IF v-i-dscr[2] = "" THEN
        ASSIGN v-i-dscr[2] = v-i-dscr[3]
               v-i-dscr[3] = v-i-dscr[4]
               v-i-dscr[4] = v-i-dscr[5]
               v-i-dscr[5] = "" .
     IF v-i-dscr[3] = "" THEN
        ASSIGN v-i-dscr[3] = v-i-dscr[4]
               v-i-dscr[4] = v-i-dscr[5]
               v-i-dscr[5] = "".
     IF v-i-dscr[4] = "" THEN
        ASSIGN v-i-dscr[4] = v-i-dscr[5]
               v-i-dscr[5] = "".

     v-part-info = IF li LE 5 THEN v-i-dscr[li] ELSE "" .
     v-dscr-lines = 0.
     do v = 1 to 5:
       if v-i-dscr[v] ne "" then v-dscr-lines = v-dscr-lines + 1.
     end.
     v-line-lines = v-dscr-lines.
     if v-line-lines eq 0 then v-line-lines = 1. 
     IF li = 1 THEN v-printline = v-printline + v-line-lines.
     if li = 1 AND v-printline gt 28 then do:
        put skip(29 - v-printline)
            "Page"
            page-number - v-last-page format ">9"  .
        v-printline = v-line-lines.
        page.
      end.

     IF li EQ 1 THEN DO WITH FRAME detail:
        
        FOR EACH b-tt-boll
            WHERE b-tt-boll.i-no   EQ tt-boll.i-no
              AND b-tt-boll.po-no  EQ tt-boll.po-no
              AND b-tt-boll.ord-no EQ tt-boll.ord-no
              AND b-tt-boll.line   EQ tt-boll.line
              AND ROWID(b-tt-boll) NE ROWID(tt-boll):
          ASSIGN
           tt-boll.qty    = tt-boll.qty + b-tt-boll.qty
           tt-boll.weight = tt-boll.weight + b-tt-boll.weight.
        END.

        v-qty = IF AVAIL oe-ordl THEN oe-ordl.qty ELSE 0.

        v-unit-qty = trim(string(tt-boll.cases,">>>>9")) + "/" +
                     trim(string(tt-boll.qty-case,">>>9")).
        v-unit-qty = fill(" ",10 - length(trim(v-unit-qty))) + trim(v-unit-qty).

        DISPLAY v-qty       
                tt-boll.i-no v-part-info
                v-unit-qty
                tt-boll.qty .
        DOWN.     
        
     END.
     
     IF li GE 2 OR (LAST-OF(tt-boll.line) AND li le 5) THEN DO WITH FRAME detail:
        v-unit-qty = trim(string(tt-boll.cases,">>>>9")) + "/" +
                       trim(string(tt-boll.qty-case,">>>9")).
        v-unit-qty = fill(" ",10 - length(trim(v-unit-qty))) + trim(v-unit-qty).
     /*
        if v-part-info eq "" AND li LE 5 then
        if tt-boll.po-no <> "" AND NOT v-po-assigned
           THEN ASSIGN v-part-info = "PO #: " + tt-boll.po-no
                                           v-po-assigned = YES.
        else if tt-boll.ord-no <> 0 AND NOT v-ord-assigned then
             ASSIGN v-part-info = "Ord#: " + trim(string(tt-boll.ord-no,">>>>>9"))
                    v-ord-assigned = YES.
     */
        IF NOT last-of(tt-boll.line) OR li GT 5  THEN do:
           DISPLAY  v-part-info v-unit-qty .
           DOWN.
        END.
        ELSE IF li LE 5 THEN boldel: DO v = li TO 5:
            v-part-info = v-i-dscr[v].
            IF v GE 2 AND LAST-OF(tt-boll.LINE) AND v-part-info = "" THEN LEAVE  .
            IF v = 1 AND last-of(tt-boll.line) THEN next.
            DISPLAY v-part-info .
            IF li NE 1 AND last-of(tt-boll.line) AND NOT v-unit-displayed THEN do:
               DISPLAY v-unit-qty .
               v-unit-displayed = YES.
            END.            
            DOWN.
        END.
      END.
            
      /*if v-printline gt 45 then do:
        /*v-printline = v-printline - v-line-lines.*/
        put skip(46 - v-printline)
            "Page"
            page-number - v-last-page format ">9"  v-printline.
        v-printline = 0.
        page.
      end.
      */
      IF LAST-OF(tt-boll.line) THEN DO:        
        PUT SKIP(1).
        ASSIGN
         v-printline = v-printline + 1
         li           = 0
         v-po-assigned = NO
         v-ord-assigned = NO  
         v-unit-displayed = NO    .
      END.
    END. /* for each tt-boll */
  


  v-dscr-lines = 0.
  do i = 1 to 4:
    if oe-bolh.ship-i[i] ne "" then v-dscr-lines = v-dscr-lines + 1.
  end.
  v-printline = v-printline + v-dscr-lines.
  
  if v-printline gt 28 then do:
    /*v-printline = v-printline - v-dscr-lines.*/
    put skip(29 - v-printline)
          "Page"
          page-number - v-last-page format ">9" .
    v-printline = 0.
    page.
  end.

  do i = 1 to 4:
    if oe-bolh.ship-i[i] ne "" then put oe-bolh.ship-i[i] at 5 skip.
  end.
  PUT SKIP(29 - v-printline).
  display "Page"                        at 6
          page-number - v-last-page     format ">9"
          "Signed"                      at 20
          space(0)
          fill(".",30)                  format "x(30)"
          "Date"                        at 58
          space(0)
          fill(".",10)                  format "x(10)"
      
      with frame bol-tot no-labels no-box no-underline STREAM-IO width 80.

  assign
   v-last-page     = page-number.
   oe-bolh.printed = true.

  page.

  END. /*last-of*/
end. /* for each oe-bolh */

RETURN.

PROCEDURE create-tt-boll.
  DEF INPUT PARAM ip-qty-case LIKE oe-boll.qty-case NO-UNDO.
  DEF INPUT PARAM ip-cases    LIKE oe-boll.cases NO-UNDO.


  IF ip-qty-case LT 0 THEN
    ASSIGN
     ip-qty-case = ip-qty-case * -1
     ip-cases    = ip-cases * -1.

  FIND FIRST tt-boll
      WHERE tt-boll.i-no     EQ oe-boll.i-no
        AND tt-boll.po-no    EQ oe-boll.po-no
        AND tt-boll.ord-no   EQ oe-boll.ord-no
        AND tt-boll.line     EQ oe-boll.line
        AND tt-boll.qty-case EQ ip-qty-case
      NO-LOCK NO-ERROR.

  IF NOT AVAIL tt-boll THEN DO:
    CREATE tt-boll.
    BUFFER-COPY oe-boll EXCEPT rec_key TO tt-boll
    ASSIGN
     tt-boll.qty-case = ip-qty-case
     tt-boll.cases    = 0
     tt-boll.qty      = 0
     tt-boll.weight   = 0
     tt-boll.partial  = 0.
  END.

  ASSIGN
   tt-boll.cases  = tt-boll.cases + ip-cases
   tt-boll.qty    = tt-boll.qty + (ip-qty-case * ip-cases)
   tt-boll.weight = tt-boll.weight + 
                    ((ip-qty-case * ip-cases) / oe-boll.qty * oe-boll.weight).

  IF oe-boll.p-c THEN tt-boll.p-c = YES.

END PROCEDURE.

