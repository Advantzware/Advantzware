/* ----------------------------------------------- oe/rep/bol-csc.p 12/97 JLF */
/*                                                                            */
/* Print BOL when sys-ctrl.char-fld eq "ContSrvc" - O/E Module                */
/*                                                                            */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xoe-boll for oe-boll.
def buffer xitemfg  for itemfg.

{oe/rep/oe-lad.i}

def var v-time as char format "x(8)" no-undo.
def var v-carrier like carrier.dscr no-undo.
def var v-frt-pay-dscr as char no-undo.
def var v-tot-pkgs as int format ">>>>>9" no-undo.

def var v-bol-qty          as   INT no-undo.
def var v-i-no             as   CHAR no-undo.
def var v-qty              as   INT no-undo.
def var v-i-dscr           as   char extent 5 no-undo.
def var v-unit-qty         as   CHAR no-undo.
def var v-line-lines       as   INT no-undo.
def var v-dscr-lines       as   INT no-undo.
def var v-po-lines         as   INT no-undo.
def var v-ord-lines        as   INT no-undo.
def var v-printline        as   INT no-undo.
def var v-part-info        like oe-ordl.part-dscr1 no-undo.
def var v                  as   INT no-undo.
def var v-part-qty         as   DEC no-undo.
def var v-cases            like oe-boll.cases no-undo.
def var v-qty-case         like oe-boll.qty-case no-undo.
def var v-partial          like oe-boll.partial no-undo.

def var v-cust-no          like cust.cust-no no-undo.
def var v-cust-name        like cust.NAME no-undo.
def var v-cust-addr        like cust.addr no-undo.
def var v-cust-city        like cust.city no-undo.
def var v-cust-state       like cust.state no-undo.
def var v-cust-zip         like cust.zip no-undo.
def var v-cust-addr3       as   char format "x(30)" no-undo.
def var v-ship-id          like shipto.ship-id no-undo.
def var v-ship-name        like shipto.ship-name no-undo.
def var v-ship-addr        like shipto.ship-addr no-undo.
def var v-ship-city        like shipto.ship-city no-undo.
def var v-ship-state       like shipto.ship-state no-undo.
def var v-ship-zip         like shipto.ship-zip no-undo.
def var v-ship-addr3       as   char format "x(30)" no-undo.
def var v-fob              as   char format "x(11)" no-undo.
def var v-tot-sqft         like itemfg.t-sqft init 0 format ">>>,>>9.99" no-undo.
def var v-page-no          as   int format ">9" no-undo.
DEF VAR li AS INT NO-UNDO.
DEF VAR v-po-assigned AS LOG NO-UNDO.
DEF VAR v-ord-assigned AS LOG NO-UNDO.
DEF VAR v-unit-displayed AS LOG NO-UNDO.
DEF VAR v-part-dscr AS cha NO-UNDO.

FORM v-qty             to 9    format ">>>,>>9"
     tt-boll.i-no      at 23   format "x(10)"
     v-part-info       at 35   format "x(27)"
     v-unit-qty        to 71   format "x(9)"
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

    ASSIGN
       v-frt-pay-dscr = ""
       v-fob = "".

    FOR EACH oe-boll where
        oe-boll.company eq oe-bolh.company and
        oe-boll.b-no eq oe-bolh.b-no
        NO-LOCK,
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
       v-i-dscr       = ""
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
       v-ship-zip     = shipto.ship-zip
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
             oe-bolh.trailer AT 4 oe-ord.ord-date       at 24   when avail oe-ord
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

      ASSIGN v-i-dscr = ""
             li = li + 1.

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

      if avail oe-ordl THEN
         ASSIGN v-i-dscr[1] = oe-ordl.i-name
                v-i-dscr[2] = oe-ordl.i-dscr
                v-i-dscr[3] = oe-ordl.part-dscr1
                v-i-dscr[4] = oe-ordl.part-dscr2.
      ELSE
         ASSIGN v-i-dscr[1] = itemfg.i-name
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

     IF v-i-dscr[4] = "" AND tt-boll.po-no <> "" THEN
        ASSIGN v-i-dscr[4] = "PO #: " + tt-boll.po-no.
     ELSE IF v-i-dscr[5] = "" AND tt-boll.po-no <> "" THEN
        ASSIGN v-i-dscr[5] = "PO #: " + tt-boll.po-no.

     IF v-i-dscr[4] = "" AND tt-boll.ord-no <> 0 THEN
        ASSIGN v-i-dscr[4] = "Ord#: " + trim(string(tt-boll.ord-no,">>>>>9")).
     ELSE IF v-i-dscr[5] = "" AND tt-boll.ord-no <> 0 THEN
        ASSIGN v-i-dscr[5] = "Ord#: " + trim(string(tt-boll.ord-no,">>>>>9")).

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

     ASSIGN
        v-part-info = IF li LE 5 THEN v-i-dscr[li] ELSE ""
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

        v-qty = 0.
        for each oe-rell FIELDS(qty) where oe-rell.company eq cocode
            and oe-rell.r-no    eq tt-boll.r-no
            AND oe-rell.ord-no   EQ tt-boll.ord-no
            and oe-rell.i-no    eq tt-boll.i-no
            and oe-rell.line    eq tt-boll.LINE
            no-lock:
               
            v-qty = v-qty + oe-rell.qty.  
        end.

        ASSIGN
        v-unit-qty = trim(string(tt-boll.cases,">>>9")) + "/" +
                     trim(string(tt-boll.qty-case,">>>9"))
        v-unit-qty = fill(" ",9 - length(trim(v-unit-qty))) + trim(v-unit-qty).

        DISPLAY v-qty       
                tt-boll.i-no v-part-info
                v-unit-qty
                tt-boll.qty.
        DOWN.     
        
     END.
     
     IF li GE 2 OR (LAST-OF(tt-boll.line) AND li le 5) THEN DO WITH FRAME detail:

        ASSIGN
        v-unit-qty = trim(string(tt-boll.cases,">>>9")) + "/" +
                       trim(string(tt-boll.qty-case,">>>9"))
        v-unit-qty = fill(" ",9 - length(trim(v-unit-qty))) + trim(v-unit-qty).
     
        IF NOT last-of(tt-boll.line) OR li GT 5  THEN do:
           DISPLAY v-part-info v-unit-qty .
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

      IF LAST-OF(tt-boll.line) THEN DO:        
        if v-print-components AND itemfg.isaset then
      for each fg-set where fg-set.company eq cocode
	                    and fg-set.set-no  eq itemfg.i-no   no-lock:
          find first xitemfg where xitemfg.company eq cocode
	                           and xitemfg.i-no    eq fg-set.part-no no-lock no-error.
          FIND FIRST fg-bin where fg-bin.company eq cocode
                            and fg-bin.i-no    eq xitemfg.i-no
                            and fg-bin.job-no = oe-boll.job-no
                            AND fg-bin.job-no2 = oe-boll.job-no2 NO-LOCK NO-ERROR.
        
          v-part-dscr = string(fg-set.part-no,"x(16)") +
		                (if avail xitemfg then xitemfg.i-name else "").

          {sys/inc/part-qty.i v-part-qty fg-set}

          IF AVAIL fg-bin THEN DO:
             put {1}
	          v-part-dscr              at 32 format "x(39)"
              oe-boll.cases TO 81  FORM ">>>9" " @ " 
              fg-bin.case-count FORM "->>>>>z"
              skip.              
              v-printline = v-printline + 1.
              IF fg-bin.partial-count <> 0 THEN do:
                 PUT {1} "  1" TO 81  " @ " fg-bin.partial-count FORM "->>>>>z" SKIP.
                 v-printline = v-printline + 1.
              END.
          END.
          ELSE DO:
              put {1}
	             v-part-dscr              at 32 format "x(39)"   
                 skip.
              v-printline = v-printline + 1.
          END.
    end. /* isaset */

        PUT SKIP(1).
        ASSIGN
         v-printline = v-printline + 1
         li           = 0
         v-po-assigned = NO
         v-ord-assigned = NO  
         v-unit-displayed = NO.
      END.
    END. /* for each tt-boll */

  v-dscr-lines = 0.
  do i = 1 to 4:
    if oe-bolh.ship-i[i] ne "" then v-dscr-lines = v-dscr-lines + 1.
  end.
  v-printline = v-printline + v-dscr-lines.
  
  if v-printline gt 28 then do:
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

