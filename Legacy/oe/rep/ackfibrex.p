/* ----------------------------------------------- oe/rep/ackfibrex.p         */
/* ORDER ACKNOLEDGEMENT                                                       */
/* for ORDER STATUS = (R), (U)                                                */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM ip-prt-revised AS LOG NO-UNDO.

{sys/inc/var.i shared}

{oe/rep/acknowl.i}

def var v-salesman as char format "x(3)" NO-UNDO.
def var v-fob as char format "x(27)" NO-UNDO.
def var v-shipvia like carrier.dscr NO-UNDO.
def var v-addr3 as char format "x(30)" NO-UNDO.
def var v-addr4 as char format "x(30)" NO-UNDO.
def var v-sold-addr3 as char format "x(30)" NO-UNDO.
def var v-line as INT NO-UNDO.
def var v-printline as int NO-UNDO.
def var v-len as int NO-UNDO.
def var v-totord as dec format "->>,>>>,>>9.99" NO-UNDO.
def var v-totlin as dec format "->>,>>>,>>9.99".
def var v-ans as log init no NO-UNDO.
def var lcnt as int init 1 NO-UNDO.
def var v-cust-phone as char format "(999)999-9999" no-undo.
def var v-tax-rate     as dec format ">,>>9.99<<<".
def var v-frt-tax-rate like v-tax-rate.
DEF VAR ll-calc-disc-first AS LOG NO-UNDO.
/* === with xprint ====*/
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(50)" NO-UNDO.
def buffer b-ref1 for reftable.
def buffer b-ref2 for reftable.
DEF BUFFER b-custx FOR cust.
def buffer ref-lot-no for reftable.
DEF BUFFER bf-oe-rel FOR oe-rel.
DEF BUFFER bf-itemfg FOR itemfg.

ASSIGN
  ls-image1 = "images\fibrelog.bmp"
  FILE-INFO:FILE-NAME = ls-image1
  ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

DEF VAR v-tel AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-fax AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-contact AS cha FORM "x(20)" NO-UNDO .

DEF VAR v-comp-add1 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add2 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add3 AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-comp-add4 AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-first-note AS LOG NO-UNDO.
DEF VAR v-comp-add5 AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-display-comp AS LOG NO-UNDO.
DEF VAR lv-email AS cha FORM "x(30)" NO-UNDO.
DEF VAR lv-comp-name AS cha FORM "x(30)" NO-UNDO.
DEF VAR v-inst AS cha FORM "x(80)" EXTENT 4 NO-UNDO.
DEF VAR lv-line-print AS INT INIT 44 NO-UNDO.
DEF VAR ln-cnt AS INT NO-UNDO.
DEF VAR lv-is-fgnote AS LOG NO-UNDO.
DEF VAR v-lot-no AS CHAR FORM "x(20)" NO-UNDO.

find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "ACKHEAD" no-lock no-error.
IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN lv-display-comp = YES.

find first company where company.company eq cocode no-lock no-error.
{custom/notesdef.i}

 ASSIGN v-comp-add1 = ""
        v-comp-add2 = ""
        v-comp-add3 = ""
        v-comp-add4 = ""
        v-comp-add5 = ""
        lv-email = ""
        lv-comp-name = "".

 IF lv-display-comp THEN DO:
    FIND FIRST b-custx WHERE b-custx.company = cocode AND
         b-custx.active = "X" NO-LOCK NO-ERROR.
    
    IF AVAIL b-custx THEN
       ASSIGN v-comp-add1 = b-custx.addr[1]
              v-comp-add2 = b-custx.addr[2]
              v-comp-add3 = b-custx.city + ", " + b-custx.state + "  " + b-custx.zip
              lv-email    = "Email:  " + b-custx.email 
              lv-comp-name = b-custx.NAME.
 END.

  ll-calc-disc-first = NO.
  FOR EACH sys-ctrl
      WHERE sys-ctrl.company  EQ cocode
        AND sys-ctrl.name     EQ "INVPRINT"
        AND sys-ctrl.char-fld EQ "Dayton"
      NO-LOCK:
    ll-calc-disc-first = YES.
    LEAVE.
  END.

  find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.
  
  PUT "<FORMAT=LETTER>".

  FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
      FIRST oe-ord WHERE RECID(oe-ord) EQ report.rec-id BREAK BY oe-ord.ord-no:

      IF lv-display-comp THEN
      DO:
         IF oe-ord.USER-ID NE "" THEN
               FIND FIRST b-ref1 WHERE
                    b-ref1.reftable EQ "users.phone-no" AND
                    b-ref1.company EQ oe-ord.USER-ID
                    NO-LOCK NO-ERROR.
              
               IF AVAIL b-ref1 THEN
               DO:
                  IF b-ref1.CODE NE "" THEN
                     v-comp-add4 = "Phone:  " + string(b-ref1.CODE,"(999)999-9999").
                  ELSE IF AVAIL b-custx THEN
                     v-comp-add4 = "Phone:  " + string(b-custx.area-code,"(999)") + string(b-custx.phone,"999-9999").
              
                  RELEASE b-ref1.
               END.
              
               FIND FIRST b-ref1 WHERE
                    b-ref1.reftable EQ "users.fax-no" AND
                    b-ref1.company EQ oe-ord.USER-ID
                    NO-LOCK NO-ERROR.
              
               IF AVAIL b-ref1 THEN
               DO:
                  IF b-ref1.CODE NE "" THEN
                     v-comp-add5 = "Fax  :  " + string(b-ref1.CODE,"(999)999-9999").
                  ELSE IF AVAIL b-custx THEN
                     v-comp-add5 = "Fax  :  " + string(b-custx.fax,"(999)999-9999").
              
                  RELEASE b-ref1.
               END.
      END.

      IF oe-ord.sman[1] <> "" THEN DO:
         FIND FIRST sman WHERE sman.company EQ oe-ord.company
                           AND sman.sman    EQ oe-ord.sman[1] NO-LOCK NO-ERROR.
         v-salesman = IF AVAIL sman THEN sman.sname ELSE oe-ord.sman[1].
      END.
      

      if oe-ord.fob-code eq "ORIG" then
        v-fob = "Origin".
      else
        v-fob = "Destination".

      find FIRST carrier
          where carrier.company eq oe-ord.company
            and carrier.carrier eq oe-ord.carrier
          no-lock no-error.
      if avail carrier then
        v-shipvia = carrier.dscr.
      else
        v-shipvia = "".
      v-printline = 0.
      assign
       v-addr3 = oe-ord.city + ", " + oe-ord.state + "  " + oe-ord.zip
       v-sold-addr3 = oe-ord.sold-city + ", " + oe-ord.sold-state +
                      "  " + oe-ord.sold-zip
       v-line = 1.
      IF oe-ord.sold-city = "" AND oe-ord.sold-state = ""
         AND oe-ord.sold-zip = "" THEN v-sold-addr3 = v-addr3.

      find first cust
          where cust.company eq cocode
            and cust.cust-no eq oe-ord.cust-no
          no-lock no-error.
      if avail cust then v-cust-phone = cust.area-code + cust.phone.
      
      FIND first oe-ordl where oe-ordl.company eq oe-ord.company
                           and oe-ordl.ord-no  eq oe-ord.ord-no
                           NO-LOCK NO-ERROR.
      
      /* get total page-num */
      ln-cnt = 0.
      for each oe-ordl
          where oe-ordl.company eq oe-ord.company
            and oe-ordl.ord-no  eq oe-ord.ord-no AND
                oe-ordl.LINE > 0
          no-lock:
          ln-cnt = ln-cnt + 3.
          if oe-ordl.part-dscr1 ne ""        then ln-cnt = ln-cnt + 1.
          if oe-ordl.part-dscr2 ne "" then ln-cnt = ln-cnt + 1.

          FOR EACH oe-rel
              WHERE oe-rel.company EQ oe-ordl.company
                AND oe-rel.ord-no  EQ oe-ordl.ord-no
                AND oe-rel.i-no    EQ oe-ordl.i-no
                AND oe-rel.line    EQ oe-ordl.line
                AND ((oe-rel.link-no EQ 0 AND v-schrel) OR
                     (oe-rel.link-no NE 0 AND v-actrel))
              NO-LOCK BREAK BY oe-rel.link-no DESC:

              if first-of(oe-rel.link-no) then DO:
                 if oe-rel.link-no eq 0 then ln-cnt = ln-cnt + 1.
                 ELSE if first(oe-rel.link-no) then ln-cnt = ln-cnt + 1.
                 ln-cnt = ln-cnt + 1.              
              end.
              v-addr4 = "".
              if v-shipto then do:
                find first shipto where shipto.company eq cocode
                                  and shipto.cust-no eq oe-rel.cust-no
                                  and shipto.ship-id eq oe-rel.ship-id
                                  no-lock no-error.
                if avail shipto THEN do:
                    ln-cnt = ln-cnt + 1.
                    v-addr4 = shipto.ship-city + ", " +
                        shipto.ship-state + "  " + shipto.ship-zip.
                END.
                IF shipto.ship-addr[1] <> "" THEN ln-cnt = ln-cnt + 1.
                IF shipto.ship-addr[2] <> "" THEN ln-cnt = ln-cnt + 1.
                IF v-addr4 <> "" THEN ln-cnt = ln-cnt + 1.
              END.              
         
          END.
        
          IF v-prntinst THEN DO:
             find first itemfg {sys/look/itemfgrlW.i}
               and itemfg.i-no eq oe-ordl.i-no no-lock no-error.
             {custom/notesprt.i itemfg v-inst 4}
             lv-is-fgnote = NO.
             DO i = 1 TO 4:
                IF v-inst[i] <> "" THEN ASSIGN ln-cnt = ln-cnt + 1
                                             lv-is-fgnote = YES.
             END.
             IF lv-is-fgnote THEN ASSIGN ln-cnt = ln-cnt + 2
                                         lv-is-fgnote = NO.
          END.
      END.
      for each oe-ordm no-lock where oe-ordm.company eq oe-ord.company and
                           oe-ordm.ord-no eq oe-ord.ord-no break BY ord-no:
           if first(oe-ordm.ord-no) THEN ln-cnt = ln-cnt + 2.
           ln-cnt = ln-cnt + 1.
      END.
      IF oe-ord.f-bill THEN ln-cnt = ln-cnt + 1.

      {oe/rep/ackfibrex.i}
      ASSIGN v-printline = 20.

      for each oe-ordl
          where oe-ordl.company eq oe-ord.company
            and oe-ordl.ord-no  eq oe-ord.ord-no
            AND oe-ordl.LINE > 0 
          no-lock:

        RUN new-page-proc.

        PUT v-line FORM ">>>9" SPACE(3)
            oe-ordl.part-no  AT 9  SPACE(7)             
            oe-ordl.i-name SPACE(11)
            oe-ordl.qty FORM "->>,>>>,>>>,>>9" SPACE(6)
            oe-ordl.price  FORM "$->>>>,>>9.99<<<<" SPACE(3)
            oe-ordl.pr-uom
            SKIP.

        v-printline = v-printline + 1.     

        FIND FIRST bf-itemfg WHERE bf-itemfg.company = oe-ordl.company
                               AND bf-itemfg.i-no = oe-ordl.i-no
            NO-LOCK NO-ERROR.
        IF AVAIL bf-itemfg THEN DO:
           PUT bf-itemfg.i-no  AT 9 SKIP.
           v-printline = v-printline + 1.     
        END.

        ASSIGN v-lot-no = "".
        FIND FIRST bf-oe-rel
             WHERE bf-oe-rel.company EQ oe-ordl.company
               AND bf-oe-rel.ord-no  EQ oe-ordl.ord-no
               AND bf-oe-rel.i-no    EQ oe-ordl.i-no
               AND bf-oe-rel.line    EQ oe-ordl.line
               NO-LOCK NO-ERROR.

             ASSIGN v-lot-no = bf-oe-rel.lot-no. 
        RELEASE bf-oe-rel.
        
        IF v-lot-no <> ""  THEN DO:
           PUT v-lot-no  AT 9 SKIP.
           v-printline = v-printline + 1.     
        END.   

        IF oe-ordl.po-no NE "" THEN DO:
          RUN new-page-proc.
          PUT "P.O. # " + oe-ordl.po-no FORM "X(22)" AT 9 SKIP.
          v-printline = v-printline + 1.
        END.
        
        IF v-schrel THEN
        for each oe-rel
            where oe-rel.company EQ oe-ordl.company
              AND oe-rel.ord-no  EQ oe-ordl.ord-no
              AND oe-rel.i-no    EQ oe-ordl.i-no
              AND oe-rel.line    EQ oe-ordl.line
              AND oe-rel.link-no EQ 0
            no-lock break by oe-rel.rel-date:

          RUN print-rels (FIRST(oe-rel.rel-date)).
        end.   /* for each oe-rel  */

        IF v-actrel THEN
        for each oe-rel
            where oe-rel.company EQ oe-ordl.company
              AND oe-rel.ord-no  EQ oe-ordl.ord-no
              AND oe-rel.i-no    EQ oe-ordl.i-no
              AND oe-rel.line    EQ oe-ordl.line
              AND oe-rel.link-no NE 0
            no-lock break by oe-rel.rel-date:

          RUN print-rels (FIRST(oe-rel.rel-date)).
        end.   /* for each oe-rel  */

        v-line = v-line + 1.

        if oe-ordl.pr-uom begins "L" AND oe-ordl.pr-uom NE "LB" then
           assign v-totlin = oe-ordl.price * IF oe-ordl.qty LT 0 THEN -1 ELSE 1.
                             
        else
        if oe-ordl.pr-uom eq "CS" then
        do:
          find first itemfg {sys/look/itemfgrlW.i}
            and itemfg.i-no eq oe-ordl.i-no no-lock no-error.

          v-totlin = oe-ordl.qty /
                     (if oe-ordl.cas-cnt ne 0 then oe-ordl.cas-cnt else
                      if avail itemfg and itemfg.case-count ne 0
                      then itemfg.case-count else 1) *
                     oe-ordl.price.
        end.
        else
        if oe-ordl.pr-uom eq "C" then
          v-totlin = oe-ordl.qty / 100 * oe-ordl.price.

        else
        if oe-ordl.pr-uom eq "M" then
          v-totlin = oe-ordl.qty / 1000 * oe-ordl.price.

        else /** DEFAULT TO EACH **/
          v-totlin = oe-ordl.qty * oe-ordl.price.

        v-totlin = ROUND(v-totlin,2).

        IF oe-ordl.disc NE 0 THEN
           v-totlin = IF ll-calc-disc-first THEN 
                        (v-totlin - ROUND(v-totlin * oe-ordl.disc / 100,2))
                      ELSE
                        ROUND(v-totlin * (1 - (oe-ordl.disc / 100)),2).

        v-totord = v-totord + v-totlin.

     IF v-prntinst THEN DO: /* print spec notes */
        find first itemfg {sys/look/itemfgrlW.i}
             and itemfg.i-no eq oe-ordl.i-no
             no-lock no-error.

        IF AVAIL itemfg THEN DO:
           lv-first-note = yes.         
           {custom/notesprt.i itemfg v-inst 4}
           
           DO i = 1 TO 4:
              IF v-inst[i] <> "" THEN DO:
                 IF lv-first-note THEN do:
                    RUN new-page-proc.
                    PUT SKIP(1).
                    ASSIGN
                      v-printline = v-printline + 1
                      lv-first-note = NO.
                 END.
                 RUN new-page-proc.
                 PUT v-inst[i] SKIP.
                 v-printline = v-printline + 1.
              END.           
           END.
           
           IF NOT lv-first-note THEN do:
              RUN new-page-proc.
              PUT SKIP(1).
              v-printline = v-printline + 1.           
           END.
        END. 
     END. /* v-prntinst */

     RUN new-page-proc.
     if v-printline LT lv-line-print then
     DO:
        PUT SKIP(1).
        v-printline = v-printline + 1.           
     END.
   end. /* each oe-ordl */

      for each oe-ordm no-lock where
          oe-ordm.company eq oe-ord.company and
          oe-ordm.ord-no eq oe-ord.ord-no
          break BY ord-no:

        if first(oe-ordm.ord-no) then
        do:
          RUN new-page-proc.
          put "** Miscellaneous Items **" at 5 SKIP(1).
          assign v-printline = v-printline + 1.
        end.

        RUN new-page-proc.

        if oe-ordm.bill eq "N" then
           PUT v-line FORM ">>>9" SPACE(4)
               oe-ordm.charge SPACE(2)
               oe-ordm.dscr FORM "X(30)" SPACE(25)
               "1" SPACE(6)                                     
               "          N/C   EA" SKIP.          
        else
           PUT v-line FORM ">>>9" SPACE(4)
               oe-ordm.charge SPACE(2)
               oe-ordm.dscr FORM "X(30)" SPACE(25)
               "1" SPACE(6)
               oe-ordm.amt FORM "$->>>>,>>9.99" SPACE(3)
               "EA" SKIP.

        v-printline = v-printline + 1.

        IF oe-ordm.po-no NE "" THEN DO:
          RUN new-page-proc.
          PUT "P.O. # " + oe-ordm.po-no FORM "X(22)" AT 9 SKIP.
          v-printline = v-printline + 1.
        END.

        RUN new-page-proc.

        PUT SKIP(1).
        assign v-line = v-line + 1
               v-printline = v-printline + 1.
       
        if oe-ordm.bill ne "N" THEN assign v-totord = v-totord + oe-ordm.amt.
      end. /* each oe-ordm */
           
      /* print freight*/
      IF oe-ord.f-bill THEN DO:
         RUN new-page-proc.
         PUT skip(2) "Freight:" AT 9
             oe-ord.t-freight AT 93 FORM "$->>>>,>>9.99<<<<" SKIP.
         ASSIGN
           v-totord = v-totord + oe-ord.t-freight
           v-printline = v-printline + 3.
      END.
      
      v-totord = v-totord + oe-ord.tax.

      PUT
        "<FArial><P12><R57><C3><B>Acknowledgement Signature ________________________________________</B><FMS Mincho>"
        "<P12><R59><C53><B>Tax:                </B>" AT 90 oe-ord.tax FORM "$->,>>9.99"  "<P9>"

        "<P12><R61><C53><B>Total Order Value:</B>" AT 90 v-totord FORM "$>>>>,>>9.99"  "<P9>"
        "<R63><C1>" SPACE(32) "<P10><B>THIS IS A CONFIRMATION OF YOUR ORDER, NOT AN INVOICE.<P10></B>"
        "<R64><C48>QF-91 Sales Order Acknowledgement Rev. 3, 1/08" FORMAT "X(1000)".

      
      assign
       v-totord        = 0
       oe-ord.ack-prnt = YES
       v-printline = v-printline + 8.

      IF v-printline <= 66 THEN page.

    end. /* each oe-ord */

RETURN.

PROCEDURE print-rels:
    DEF INPUT PARAM ip-first AS LOG NO-UNDO.
    
    DO WITH with frame sched-rel DOWN:
       if ip-first then do:
         lcnt = 1.
         if oe-rel.link-no eq 0 then do:

           RUN new-page-proc.
           put "Scheduled Releases:" at 15 SKIP .
           v-printline = v-printline + 1.
         end.
         else do:
           RUN new-page-proc.
           put "Actual Releases:" at 15 SKIP. 
           v-printline = v-printline + 1.  
         end.
       end.
       
       {oe/rel-stat.i lv-stat}
       IF AVAIL oe-rell THEN
       FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK NO-ERROR.
       ASSIGN
         ld-date = IF AVAIL oe-relh THEN oe-relh.rel-date ELSE oe-rel.rel-date
         lcnt    = lcnt + 1.

       if v-shipto then do:
         find first shipto
             where shipto.company eq cocode
               and shipto.cust-no eq oe-rel.cust-no
               and shipto.ship-id eq oe-rel.ship-id
             no-lock no-error.

         if avail shipto THEN DO:
         
            v-addr4 = shipto.ship-city + ", " +
                      shipto.ship-state + "  " + shipto.ship-zip.
      
            RUN new-page-proc.
            put oe-rel.po-no AT 2 shipto.ship-name AT 15 FORM "x(28)" ld-date AT 59 (IF oe-rel.link-no EQ 0 THEN oe-rel.tot-qty ELSE oe-rel.qty) 
                SKIP.
            v-printline = v-printline + 1.
           
            IF shipto.ship-addr[1] <> "" THEN DO:
               RUN new-page-proc.
               PUT shipto.ship-addr[1] AT 15  SKIP.
               v-printline = v-printline + 1.
       
            END.
            IF shipto.ship-addr[2] <> "" THEN DO:
                RUN new-page-proc.
                PUT shipto.ship-addr[2] AT 15 SKIP.
                v-printline = v-printline + 1.
                
            END.
            IF v-addr4 <> "" THEN DO:
               RUN new-page-proc.
               PUT v-addr4 AT 15  SKIP.
               v-printline = v-printline + 1.
            END.
         END.
       end.

       RUN new-page-proc.
       IF v-printline LT lv-line-print THEN
       DO:
          PUT SKIP(1).
          v-printline = v-printline + 1.
       END.
    END.

END PROCEDURE.

PROCEDURE new-page-proc:

   if v-printline ge lv-line-print then
   do:
      PAGE.
      {oe/rep/ackfibrex.i}
      assign v-printline = 20.          
   end.
END PROCEDURE.

/* end ---------------------------------- copr. 2001  Advanced Software, Inc. */
