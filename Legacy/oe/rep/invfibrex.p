/* ---------------------------------------------- oe/rep/invfibrex.p  */
/* PRINT INVOICE   Fibre                                              */
/* ------------------------------------------------------------------ */

{sys/inc/var.i shared}
{oe/rep/invoice.i}
{custom/notesdef.i}

DEF VAR v-inst AS cha FORM "x(80)" EXTENT 4 NO-UNDO.
DEF VAR v-salesman AS CHAR FORMAT "X(20)" NO-UNDO.
def var v-fob as char format "x(27)" NO-UNDO.
def var v-shipvia like carrier.dscr NO-UNDO.
def var v-addr3 as char format "x(30)" NO-UNDO.
def var v-sold-addr3 as char format "x(30)" NO-UNDO.
def var v-shipto-name as char format "x(30)" NO-UNDO.
def var v-shipto-addr as char format "x(30)" extent 2 NO-UNDO.
def var v-shipto-city as char format "x(15)" NO-UNDO.
def var v-shipto-state as char format "x(2)" NO-UNDO.
def var v-shipto-zip as char format "x(10)" NO-UNDO.
def var v-printline as INT NO-UNDO.
def var v-inv-date as date initial TODAY FORM "99/99/9999" NO-UNDO.
DEF VAR v-date-ship AS DATE NO-UNDO.
DEF VAR v-p-c AS CHAR NO-UNDO.
DEF VAR v-lot-no AS CHAR NO-UNDO.
DEF VAR v-est-no AS CHAR NO-UNDO.
DEF VAR v-cnt AS CHAR NO-UNDO.
DEF VAR v-addr-2 AS LOG NO-UNDO.

DEF buffer xinv-head for inv-head .
DEF buffer xinv-line for inv-line .

def var v as INT NO-UNDO.
def var v-price as dec FORMAT "$>>,>>9.99<<" no-undo.
DEF VAR v-ship-qty AS DEC FORMAT "->,>>>,>>9" NO-UNDO.
def var v-t-price as dec format ">>>>>>9.99" no-undo.
def var v-po-no like inv-line.po-no no-undo.
def var v-price-head as char format "x(4)" no-undo.
DEF VAR v-subtot-lines AS DEC NO-UNDO.
DEF VAR v-inv-freight LIKE inv-head.t-inv-freight NO-UNDO.
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR ls-full-img1 AS cha FORM "x(200)" NO-UNDO.
DEF VAR v-terms AS CHAR NO-UNDO.

/* gdm - 04160924 */
DEF BUFFER b-rh FOR rm-rcpth.
DEF BUFFER b-rd FOR rm-rdtlh.
DEF VAR v-addr3a       AS CHAR FORMAT "x(30)"                 NO-UNDO.
DEF VAR v-sold-addr3a  AS CHAR FORMAT "x(30)"                 NO-UNDO.

  ASSIGN
     ls-image1 = "images\FWFlogoBW.jpg"
     FILE-INFO:FILE-NAME = ls-image1
     ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

  find first oe-ctrl where oe-ctrl.company = cocode no-lock no-error.

  for each report where report.term-id eq v-term-id no-lock,
      first xinv-head where recid(xinv-head) eq report.rec-id no-lock
      break by report.key-01
            by report.key-02:

    assign  v-shipto-name = xinv-head.sold-name
            v-shipto-addr[1] = xinv-head.sold-addr[1]
            v-shipto-addr[2] = xinv-head.sold-addr[2]
            v-shipto-city = xinv-head.sold-city
            v-shipto-state = xinv-head.sold-state
            v-shipto-zip = xinv-head.sold-zip
            v-date-ship = ?.

    find first oe-bolh where
         oe-bolh.company = xinv-head.company and
         oe-bolh.bol-no = xinv-head.bol-no
         use-index bol-no
         no-lock no-error.

    if avail oe-bolh then do:
      
      v-date-ship = oe-bolh.bol-date.      

      find first shipto where
           shipto.company  = oe-bolh.company and
           shipto.cust-no = oe-bolh.cust-no and
           shipto.ship-id = oe-bolh.ship-id
           no-lock no-error.

      if avail shipto then
         assign
            v-shipto-name = shipto.ship-name
            v-shipto-addr[1] = shipto.ship-addr[1]
            v-shipto-addr[2] = shipto.ship-addr[2]
            v-shipto-city = shipto.ship-city
            v-shipto-state = shipto.ship-state
            v-shipto-zip = shipto.ship-zip.
    end. /* avail oe-bolh */

    IF NOT v-reprint OR xinv-head.inv-no EQ 0 THEN
      RUN oe/get-inv#.p (ROWID(xinv-head)).

    DO TRANSACTION:
      FIND inv-head WHERE ROWID(inv-head) EQ ROWID(xinv-head).

      v-inv-date = inv-head.inv-date.

      if inv-head.fob-code begins "O" then
         v-fob = "     Origin".
      else
         v-fob = "  Destination".

      find FIRST carrier where
           carrier.company = inv-head.company and
           carrier.carrier = inv-head.carrier
           no-lock no-error.

       if avail carrier then
          v-shipvia = FILL(" ",10 - INT(LENGTH(carrier.dscr) / 2)) + carrier.dscr.
       else
          v-shipvia = "".

      assign
        v-addr3 = inv-head.city + ", " + inv-head.state + "  " + inv-head.zip
        v-sold-addr3 = v-shipto-city + ", " + v-shipto-state +  "  "
                     + v-shipto-zip
        v-printline = 0
        v-po-no = ""
        v-salesman = "" .

      FIND FIRST terms WHERE terms.t-code EQ inv-head.terms NO-LOCK NO-ERROR.
      IF AVAIL terms THEN
        v-terms = FILL(" ",9 - INT(LENGTH(terms.dscr) / 2)) + terms.dscr .
      ELSE v-terms = "" .
  
      for each xinv-line FIELDS(sman) no-lock where
          xinv-line.r-no = inv-head.r-no
          by xinv-line.i-no:

          do i = 1 to 3:
             if xinv-line.sman[i] ne "" then do:

                FIND FIRST sman WHERE
                     sman.company EQ cocode AND
                     sman.sman EQ  xinv-line.sman[i]
                     NO-LOCK NO-ERROR.

                IF AVAIL sman THEN
                DO:
                   v-salesman = FILL(" ",10 - INT(LENGTH(sman.sname) / 2)) + sman.sname.
                   RELEASE sman.
                END.

                leave.
             end.
          end.
      end. /* each xinv-line */

      find first inv-line where
           inv-line.r-no = inv-head.r-no
           no-lock no-error.

      if avail inv-line then
         ASSIGN
           v-price-head = inv-line.pr-uom
           v-po-no = FILL(" ",8 - INT(LENGTH(inv-line.po-no) / 2)) + inv-line.po-no.
      ELSE
         ASSIGN
            v-price-head = "".

      {oe/rep/invfibrex.i}

      v-subtot-lines = 0.
      
      for each inv-line where
          inv-line.r-no = inv-head.r-no
          NO-LOCK:
          
          IF v-printline > 50 THEN DO:

             PAGE.
             v-printline = 0.
             {oe/rep/invfibrex.i}
          END.
          
          ASSIGN
             v-ship-qty  = IF inv-line.ord-no EQ 0 THEN inv-line.inv-qty
                                                   ELSE inv-line.ship-qty
             v-price = inv-line.price * (1 - (inv-line.disc / 100))
             v-t-price = inv-line.t-price
             v-subtot-lines = v-subtot-lines + inv-line.t-price
             v-price-head = inv-line.pr-uom
             v-p-c = "P"
             v-lot-no = "".
          
          FOR each oe-boll WHERE
              oe-boll.b-no eq inv-line.b-no AND
              oe-boll.i-no eq inv-line.i-no AND
              oe-boll.line eq inv-line.LINE NO-LOCK:

              if oe-boll.p-c then v-p-c = "C".
          end.

          RUN get_lot_no.
        
          PUT
             "<P9><C0.2>" inv-line.qty    FORMAT "->,>>>,>>9" 
             "<C8>"       v-ship-qty      FORMAT "->,>>>,>>9"
             "<C17>"      inv-line.ord-no FORMAT ">>>>>>9"
             "<C24>"      STRING(inv-line.part-no) FORMAT "x(15)"
             "<C36.8>"      STRING(inv-line.i-name)  FORMAT "x(25)"
             "<C57>"      STRING(v-p-c,"x")
             "<C60>"      v-price FORMAT "$>>>,>>9.99<<"
             "<C70>"      STRING(v-price-head) FORMAT "x(4)"
             "<C72>"      inv-line.t-price FORMAT "$->>>,>>9.99"                                                
             SKIP.

          v-printline = v-printline + 1.

        
         /*  gdm - 04160924 - should be coming from the raw material files                   */
         /* customer lot # should come from reftable ACH 06-07-10 */
          
          IF v-lot-no EQ "" THEN
          DO:

               v-lot-no = inv-line.lot-no.

          END.

          PUT "<C8>"    STRING(inv-line.inv-qty,"->,>>>,>>9") FORMAT "X(10)"
              "<C17.75>"  TRIM(STRING(INT(inv-line.est-no),">>>>>>>"))
              "<C23.75>"  STRING(inv-line.i-no) FORMAT "x(15)".

          IF TRIM(inv-line.part-dscr1) NE "" THEN
             PUT "<C36.8>"   inv-line.part-dscr1 FORMAT "X(25)".
          ELSE 
             IF v-lot-no NE "" THEN DO:
                PUT "<C36.8>" v-lot-no FORMAT "X(25)".
                ASSIGN v-lot-no = "".
             END.

          PUT SKIP.
          ASSIGN v-printline = v-printline + 1.

          PUT "<C23.75>" inv-line.po-no FORMAT "X(15)".

          IF TRIM(inv-line.part-dscr2) NE "" THEN
             PUT "<C36.8>"   inv-line.part-dscr2 FORMAT "X(25)".
          ELSE 
             IF v-lot-no NE "" THEN
             DO:
                PUT "<C36.8>" v-lot-no FORMAT "X(25)".
                ASSIGN v-lot-no = "".
             END.

          PUT SKIP.
          ASSIGN v-printline = v-printline + 1.

          IF v-lot-no NE "" THEN DO:
             PUT "<C36.8>" v-lot-no FORMAT "X(25)" SKIP.
             v-printline = v-printline + 1.
          END.
      END.  /* each inv-line */        
       
         IF v-printline > 50 THEN DO:
            PAGE.
            v-printline = 0.
            {oe/rep/invfibrex.i}
         END.

        IF CAN-FIND(FIRST inv-misc where
           inv-misc.company = inv-head.company and
           inv-misc.r-no = inv-head.r-no and
           inv-misc.bill = "Y") THEN
           DO:
              put CHR(10)
                  "** Miscellaneous Items **" at 23 skip(1).
              v-printline = v-printline + 3.
           END.

        
        for each inv-misc FIELDS(charge dscr amt po-no ord-no) where
            inv-misc.company = inv-head.company and
            inv-misc.r-no = inv-head.r-no and
            inv-misc.bill = "Y"
            no-lock
            break by inv-misc.charge with frame detailm:
          
            IF v-printline > 50 THEN DO:
               PAGE.
               v-printline = 0.
               {oe/rep/invfibrex.i}
            END.
            
            put "<C17.4>"  TRIM(STRING(inv-misc.ord-no,">>>>>>>")) 
                "<C23.75>" inv-misc.charge FORMAT "x(20)"
                "<C38.8>"  inv-misc.dscr   FORMAT "x(30)"
                "<C71>"    inv-misc.amt    FORM "$->>>,>>9.99"
                SKIP.

            IF inv-misc.ord-no NE 0 THEN
            DO:
               v-est-no = "".

               FOR EACH oe-ordl FIELDS(est-no) WHERE
                   oe-ordl.company EQ cocode AND
                   oe-ordl.ord-no EQ inv-misc.ord-no
                   NO-LOCK:

                   /*only populate estimate # if it unique on line items*/
                   IF oe-ordl.est-no NE "" THEN
                   DO:
                      IF v-est-no EQ "" THEN
                         v-est-no = oe-ordl.est-no.
                      ELSE
                      IF v-est-no NE oe-ordl.est-no THEN
                      DO:
                         v-est-no = "".
                         LEAVE.
                      END.
                   END.
               END.
            END.

            IF v-est-no NE "" OR inv-misc.po-no NE "" THEN
            DO:
               PUT "<C17.4>" TRIM(STRING(INT(v-est-no),">>>>>>>"))
                   "<C23.75>" "PO#: " inv-misc.po-no SKIP.
               v-printline = v-printline + 1.
            END.

            ASSIGN
              v-subtot-lines = v-subtot-lines + inv-misc.amt
              v-printline = v-printline + 1.
        end. /* each inv-misc */

        PUT "<P10>".

        ASSIGN
            v-inv-freight = if inv-head.f-bill THEN inv-head.t-inv-freight ELSE 0
            v-printline = v-printline + 6.

        if v-prntinst then do:
           {custom/notesprt.i inv-head v-inst 4}
           DO i = 1 TO 4:
              IF v-inst[i] <> "" THEN DO:                
                 IF v-printline > 50 THEN DO:
                    PAGE.
                    v-printline = 0.
                    {oe/rep/invfibrex.i}
                 END.
                 PUT v-inst[i] SKIP.
                 v-printline = v-printline + 1.
              END.
           END.

           DO i = 1 TO 4:
              IF inv-head.bill-i[i] <> "" THEN DO:
                 IF v-printline > 50 THEN DO:
                    PAGE.
                    v-printline = 0.
                    {oe/rep/invfibrex.i}
                 END.

                 CASE i:
                    WHEN 1 THEN DO:
                        PUT "<R54><C1>" inv-head.bill-i[i]. 
                    END.
                    WHEN 2 THEN DO:
                        PUT "<R55><C1>" inv-head.bill-i[i]. 
                    END.
                    WHEN 3 THEN DO:
                        PUT "<R56><C1>" inv-head.bill-i[i]. 
                    END.
                    WHEN 4 THEN DO:
                        PUT "<R57><C1>" inv-head.bill-i[i]. 
                    END.
                 END CASE.

                 v-printline = v-printline + 1.
              END.
           END.
        end.
        
        assign
         inv-head.printed = yes
         inv-head.stat = "X".
      end. /* DO TRANSACTION avail inv-head */
    
    PUT "<P6><R59><C1>INTEREST AT THE RATE OF 1 1/2% PER MONTH WILL BE CHARGED ON ALL PAST DUE"
        "<R60><C1>ACCOUNTS. IN THE EVENT OF FAILURE TO PAY ANY OF THE AMOUNT DUE ON THIS"
        "<R61><C1>INVOICE, ALL COLLECTION COSTS AND/OR ATTORNEY FEES IN THE COLLECTION OF"
        "<R62><C1>ANY SUCH AMOUNT WILL BE PAID BY CUSTOMER."
        "<P12><R58><C56.5><#8><FROM><R+4><C81><RECT>" 
        "<=8><C57>SUB TOTAL $<C68>" v-subtot-lines FORMAT "-ZZZZZZZZ9.99"  
        "<=8><R+1><C57>FREIGHT   $<C68>" v-inv-freight FORMAT "-ZZZZZZZZ9.99"  
        "<=8><R+2><C57>SALES TAX $<C68>" inv-head.t-inv-tax FORMAT "-ZZZZZZZZ9.99" 
        "<=8><R+3><C57>TOTAL DUE $<C68>" inv-head.t-inv-rev FORMAT "-ZZZZZZZZ9.99"
        "<=8><R58><C66.7><FROM><C66.7><R62><RECT>"
        "<=8><R59><C56.5><FROM><C81><R59><RECT>"
        "<=8><R60><C56.5><FROM><C81><R60><RECT>"
        "<=8><R61><C56.5><FROM><C81><R61><RECT>"
        "<=8><R62><C56.5><FROM><C81><R62><RECT><P10>".

    page.
 
    end. /* each xinv-head */

/* gdm - 04160924 */
PROCEDURE get_lot_no:

    IF inv-line.job-no NE "" THEN DO:        

      RELEASE reftable.

      FIND FIRST job NO-LOCK
        WHERE job.company EQ inv-line.company
          AND job.job-no  EQ inv-line.job-no
          AND job.job-no2 EQ inv-line.job-no2 NO-ERROR.
      IF AVAIL job THEN DO:

       FIND FIRST reftable NO-LOCK
         WHERE reftable.reftable EQ "jc/jc-calc.p"
           AND reftable.company  EQ job.company
           AND reftable.loc      EQ ""
           AND reftable.code     EQ STRING(job.job,"999999999")
           AND reftable.code2    EQ inv-line.i-no
         USE-INDEX reftable NO-ERROR.
       IF NOT AVAIL reftable 
         THEN
          FIND FIRST job-hdr NO-LOCK
            WHERE job-hdr.company EQ inv-line.company
              AND job-hdr.job-no  EQ inv-line.job-no
              AND job-hdr.job-no2 EQ inv-line.job-no2
              AND job-hdr.i-no    EQ inv-line.i-no NO-ERROR.

       IF AVAIL reftable OR AVAIL job-hdr THEN DO:

         FOR EACH rm-rcpth NO-LOCK
            WHERE rm-rcpth.company   EQ inv-line.company
              AND rm-rcpth.job-no    EQ inv-line.job-no
              AND rm-rcpth.job-no2   EQ inv-line.job-no2 
              AND rm-rcpth.rita-code EQ "I" USE-INDEX job,
            EACH rm-rdtlh NO-LOCK
             WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
               AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code 
               AND rm-rdtlh.s-num     EQ (IF AVAIL reftable 
                                            THEN reftable.val[12]
                                            ELSE job-hdr.frm)
               AND rm-rdtlh.tag       NE "",
            EACH b-rd NO-LOCK
             WHERE b-rd.company   EQ rm-rdtlh.company
               AND b-rd.tag       EQ rm-rdtlh.tag
               AND b-rd.loc       EQ rm-rdtlh.loc
               AND b-rd.loc-bin   EQ rm-rdtlh.loc-bin
               AND b-rd.rita-code EQ "R"
               AND b-rd.tag2      NE "" USE-INDEX tag,
            FIRST b-rh NO-LOCK
             WHERE b-rh.r-no      EQ b-rd.r-no
               AND b-rh.rita-code EQ b-rd.rita-code
               AND b-rh.i-no      EQ rm-rcpth.i-no:
                                                 
               ASSIGN v-lot-no = b-rd.tag2.                

         END.  /* for each */
       END. 
      END.
    END. /* get lot # */
    
END PROCEDURE.

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
