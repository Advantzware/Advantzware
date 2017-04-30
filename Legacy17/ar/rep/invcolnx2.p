/* ------------------------------------------------------- ar/rep/invcolnx2.p */
/* PRINT INVOICE   Xprint form for Colonial Carton                            */
/* -------------------------------------------------------------------------- */
DEF INPUT PARAM ip-copy-title AS cha NO-UNDO.

{sys/inc/var.i shared}
{ar/rep/invoice.i}
{custom/notesdef.i}

DEF SHARED VAR v-fr-tax AS LOGICAL INITIAL NO NO-UNDO.

DEF VAR v-salesman     AS CHAR FORMAT "x(14)" NO-UNDO.  
DEF VAR v-salesname    AS CHAR FORMAT "x(30)" NO-UNDO.  
DEF VAR v-fob          AS CHAR FORMAT "x(27)" NO-UNDO.  
DEF VAR v-addr3        AS CHAR FORMAT "x(30)" NO-UNDO.  
DEF VAR v-sold-addr3   AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-shipto-id    AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR v-shipto-name  AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-shipto-addr  AS CHAR FORMAT "x(30)" NO-UNDO EXTENT 2.
DEF VAR v-shipto-city  AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v-shipto-state AS CHAR FORMAT "x(2)"  NO-UNDO.
DEF VAR v-shipto-zip   AS CHAR FORMAT "x(10)" NO-UNDO.
DEF VAR disp-frt       AS CHAR FORMAT "x(8)"  NO-UNDO INIT "Freight:".
DEF VAR v-pc           AS CHAR                NO-UNDO. /* partial or complete */
DEF VAR v-i-dscr2      AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-ord-del-hdr  AS CHAR FORMAT "x(3)"  NO-UNDO INIT "Del".
DEF VAR v-part-info    AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-i-no         AS CHAR FORMAT "x(15)" NO-UNDO.
DEF VAR v-i-dscr       AS CHAR FORMAT "x(18)" NO-UNDO.
DEF VAR v-bill-i       AS CHAR FORMAT "x(25)" NO-UNDO.
DEF VAR v-ship-i       AS CHAR FORMAT "x(25)" NO-UNDO.
DEF VAR v-price-head   AS CHAR FORMAT "x(5)"  NO-UNDO.
DEF VAR v-bot-lab      AS CHAR FORMAT "x(63)" NO-UNDO EXTENT 3.
DEF VAR v-notes        AS CHAR FORMAT "x(80)" NO-UNDO EXTENT 4 .
DEF VAR ls-image1      AS CHAR                NO-UNDO.
DEF VAR ls-full-img1   AS CHAR FORMAT "x(50)" NO-UNDO.
DEF VAR v-tel          AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-fax          AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-contact      AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR v-comp-add1    AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add2    AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add3    AS CHAR FORMAT "x(30)" NO-UNDO.
DEF VAR v-comp-add4    AS CHAR FORMAT "x(30)" NO-UNDO.

DEF VAR v-shipvia     LIKE carrier.dscr     NO-UNDO.
DEF VAR v-t-weight    LIKE ar-invl.t-weight NO-UNDO.
DEF VAR v-tax-code    LIKE stax.tax-code    NO-UNDO.
DEF VAR v-tx-rate     LIKE stax.tax-rate    NO-UNDO.
DEF VAR v-bol-cases   LIKE oe-boll.cases    NO-UNDO.
DEF VAR v-net         LIKE ar-inv.gross     NO-UNDO.
DEF VAR lv-bol-no     LIKE oe-boll.bol-no   NO-UNDO.
DEF VAR v-po-no       LIKE ar-invl.po-no    NO-UNDO.
DEF VAR v-ord-no      LIKE oe-ord.ord-no    NO-UNDO.
DEF VAR v-ord-date    LIKE oe-ord.ord-date  NO-UNDO.
DEF VAR v-rel-po-no   LIKE oe-rel.po-no     NO-UNDO.
DEF VAR v-inv-freight LIKE ar-inv.freight   NO-UNDO.

DEF VAR v-line         AS INT                 NO-UNDO.
DEF VAR v-printline    AS INT                 NO-UNDO.
DEF VAR v-inv-no       AS INT                 NO-UNDO.
DEF VAR v-tot-pallets  AS INT                 NO-UNDO.
DEF VAR v-tot-qty      AS INT                 NO-UNDO.
DEF VAR v-del-no       AS INT FORMAT ">>>>>>" NO-UNDO.
DEF VAR v-set-qty      AS INT                 NO-UNDO.
DEF VAR cnt            AS INT                 NO-UNDO.
DEF VAR minus-ship     AS INT                 NO-UNDO.
DEF VAR v-beeler-lines AS INT                 NO-UNDO.
DEF VAR v              AS INT                 NO-UNDO.
DEF VAR v-bo-qty       AS INT FORMAT "99999"  NO-UNDO.
DEF VAR v-inv-qty      AS INT FORMAT "99999"  NO-UNDO.
DEF VAR v-ship-qty     AS INT FORMAT "99999"  NO-UNDO.
DEF VAR v-lines        AS INT                 NO-UNDO.
DEF VAR v-notes-line   AS INT                 NO-UNDO.

DEF VAR v-tot-cas      AS DEC FORMAT "->>>9.9999" NO-UNDO.
DEF VAR v-tax-rate     AS DEC FORMAT "->>>.99"    NO-UNDO.
DEF VAR v-part-qty     AS DEC FORMAT "999.9999"   NO-UNDO.
DEF VAR tmp1           AS DEC                     NO-UNDO.                    
DEF VAR net1           AS DEC                     NO-UNDO. 
DEF VAR net2           AS DEC                     NO-UNDO. 
DEF VAR net3           AS DEC                     NO-UNDO. 
DEF VAR v-price        AS DEC FORMAT ">>>>9.9999" NO-UNDO.
DEF VAR v-t-price      AS DEC FORMAT ">>>>>>9.99" NO-UNDO.
DEF VAR v-subtot-lines AS DEC                     NO-UNDO.
DEF VAR v-t-tax        AS DEC                     NO-UNDO extent 3.
DEF VAR v-frt-tax      AS DEC                     NO-UNDO. 
DEF VAR v-inv-total    AS DEC                     NO-UNDO. 

def var v-billto-name as char format "x(30)" NO-UNDO.
def var v-billto-id as char format "x(10)" NO-UNDO.
def var v-billto-addr as char format "x(30)" extent 2 NO-UNDO.
def var v-billto-addr3 as char format "x(30)" NO-UNDO.
def var v-billto-city as char format "x(15)" NO-UNDO.
def var v-billto-state as char format "x(2)" NO-UNDO.
def var v-billto-zip as char format "x(10)" NO-UNDO.

DEF VAR v-inv-date  AS DATE INITIAL TODAY FORMAT "99/99/9999" NO-UNDO.
DEF VAR v-date-ship AS DATE INITIAL TODAY                     NO-UNDO.
DEF VAR tmp2        AS DATE                                   NO-UNDO.

DEF VAR v-ans AS LOGICAL INITIAL NO NO-UNDO.
def var v-job-no AS CHAR FORMAT "x(13)" no-undo.

DEF BUFFER xar-inv FOR ar-inv.

DEF TEMP-TABLE w-sman
  FIELD sman AS char FORMAT "x(4)".

DEF TEMP-TABLE w-tax
    FIELD w-dsc AS CHAR
    FIELD w-tax AS DEC.

DEF TEMP-TABLE tt-inv-line
    FIELD i-no LIKE ar-invl.i-no
    FIELD ord-no LIKE ar-invl.ord-no
    FIELD po-no LIKE v-po-no
    FIELD part-no LIKE ar-invl.part-no  
    FIELD i-dscr LIKE v-i-dscr
    FIELD ship-qty LIKE v-ship-qty
    FIELD price LIKE v-price
    FIELD total-price LIKE ar-invl.amt
    FIELD part-dscr LIKE v-i-dscr2  
    FIELD pc LIKE v-pc
    FIELD price-head LIKE v-price-head
    FIELD bol-no LIKE ar-invl.bol-no
    FIELD job-no LIKE inv-line.job-no
    FIELD job-no2 LIKE inv-line.job-no2.
.

/* === with xprint ====*/
ASSIGN ls-image1 = "images\ccci.jpg"
       FILE-INFO:FILE-NAME = ls-image1
       ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.
FOR EACH tt-inv-line NO-LOCK:
    DELETE tt-inv-line .
END.

FOR EACH report 
  WHERE report.term-id EQ v-term-id NO-LOCK,
  FIRST ar-inv WHERE RECID(ar-inv) EQ report.rec-id NO-LOCK
   break BY report.key-01
         BY report.key-02:


  FIND FIRST cust WHERE cust.company = ar-inv.company
                    AND cust.cust-no = ar-inv.cust-no NO-LOCK NO-ERROR.

  IF ar-inv.sold-name <> "" 
    THEN ASSIGN  v-shipto-name = ar-inv.sold-name
                 v-shipto-addr[1] = ar-inv.sold-addr[1]
                 v-shipto-addr[2] = ar-inv.sold-addr[2]
                 v-shipto-city = ar-inv.sold-city
                 v-shipto-state = ar-inv.sold-state
                 v-shipto-zip = ar-inv.sold-zip
                 v-shipto-id = ar-inv.sold-id.
    ELSE DO:
     FIND FIRST shipto WHERE shipto.company = ar-inv.company
                         AND shipto.cust-no = ar-inv.cust-no
                         AND shipto.ship-id = ar-inv.ship-id NO-LOCK NO-ERROR.
     IF AVAIL shipto 
       THEN ASSIGN  v-shipto-name = shipto.ship-name
                    v-shipto-addr[1] = shipto.ship-addr[1]
                    v-shipto-addr[2] = shipto.ship-addr[2]
                    v-shipto-city = shipto.ship-city
                    v-shipto-state = shipto.ship-state
                    v-shipto-zip = shipto.ship-zip
                    v-shipto-id = shipto.ship-id.                            
    END.

  ASSIGN v-del-no = 0.

  IF ar-inv.inv-date NE ? 
    THEN ASSIGN v-inv-date = ar-inv.inv-date
                v-date-ship = ar-inv.inv-date.

  IF ar-inv.fob-code BEGINS "ORIG" 
    THEN ASSIGN v-fob = "Origin".
    ELSE ASSIGN v-fob = "Destination".

  FIND FIRST carrier WHERE carrier.company = ar-inv.company 
                       AND carrier.carrier = ar-inv.carrier NO-LOCK NO-ERROR.
  IF AVAIL carrier 
   THEN ASSIGN v-shipvia = carrier.dscr.
   ELSE ASSIGN v-shipvia = "".

  ASSIGN v-addr3      = ar-inv.city + ", " + ar-inv.state + "  " + ar-inv.zip
         v-sold-addr3 = v-shipto-city + ", " + v-shipto-state +
                        "  " + v-shipto-zip
         v-line = 1
         v-printline = 0.
  FIND FIRST stax
    {sys/ref/stax1W.i}
      AND {sys/ref/taxgroup.i stax} eq ar-inv.tax-code NO-LOCK NO-ERROR.
  IF NOT AVAIL stax THEN 
    FIND FIRST stax where stax.tax-group eq ar-inv.tax-code NO-LOCK NO-ERROR.

  IF AVAIL stax 
    THEN assign v-tax-rate = stax.tax-rate[1] +
                             stax.tax-rate[2] + stax.tax-rate[3]
                v-tax-code[1] = stax.tax-code[1]
                v-tax-code[2] = stax.tax-code[2]
                v-tax-code[3] = stax.tax-code[3]
                v-tx-rate[1]  = stax.tax-rate[1]
                v-tx-rate[2]  = stax.tax-rate[2]
                v-tx-rate[3]  = stax.tax-rate[3].

  ASSIGN v-tot-pallets = 0.

  FOR EACH ar-invl NO-LOCK WHERE ar-invl.x-no EQ ar-inv.x-no 
   BREAK BY ar-invl.i-no:

   DO i = 1 to 3:
     IF ar-invl.sman[i] NE "" THEN DO:
       CREATE w-sman.
       ASSIGN w-sman.sman = ar-invl.sman[i].
     END.
   END.

   ASSIGN v-tot-qty = v-tot-qty + ar-invl.ship-qty
          v-t-weight = v-t-weight + (round(ar-invl.t-weight /
                       ar-invl.qty, 2) * ar-invl.inv-qty)
          v-tot-pallets = 0
          v-pc = "C". /* complete*/

   IF LAST-OF(ar-invl.i-no) THEN DO:

     IF ar-invl.est-no ne "" THEN DO: 

       FIND FIRST eb 
         where eb.company  = ar-invl.company 
           AND eb.est-no   = ar-invl.est-no 
           AND eb.e-num    = ar-invl.e-num 
           AND eb.form-no  = ar-invl.form-no 
           AND eb.blank-no = ar-invl.blank-no NO-LOCK NO-ERROR.
       IF ar-invl.form-no = 0 AND ar-invl.est-type = 2 
         THEN DO:

          FOR EACH fg-set NO-LOCK 
            WHERE fg-set.company = ar-invl.company
              AND fg-set.set-no = ar-invl.i-no:
            ASSIGN v-set-qty = v-set-qty + fg-set.part-qty.
          END.

          IF v-set-qty = 0 
            THEN ASSIGN v-set-qty = 1.


          FOR EACH eb NO-LOCK 
            WHERE eb.company = ar-invl.company 
              AND eb.est-no = ar-invl.est-no 
              AND eb.e-num = ar-invl.e-num 
              AND eb.form-no NE 0:

             FIND fg-set 
               WHERE fg-set.company = ar-invl.company 
                 AND fg-set.set-no = ar-invl.i-no  
                 AND fg-set.part-no = eb.stock-no NO-LOCK NO-ERROR.
             IF AVAIL fg-set AND fg-set.part-qty NE 0 
               THEN ASSIGN v-part-qty = fg-set.part-qty / v-set-qty.
               ELSE ASSIGN v-part-qty = 1 / v-set-qty.

             IF eb.cas-cnt = 0
               THEN ASSIGN v-tot-cas = ROUND((v-t-weight * v-part-qty) /
                                       eb.cas-wt, 2).
               ELSE ASSIGN v-tot-cas = ROUND((v-tot-qty * v-part-qty) /
                                       eb.cas-cnt, 2).
             IF v-bol-cases NE 0 
               THEN ASSIGN v-tot-cas = v-bol-cases.
          END.  /* each eb */
       END. /* do */
       ELSE
       IF AVAIL eb THEN DO:

         IF eb.cas-cnt = 0 
           THEN ASSIGN v-tot-cas = ROUND(v-t-weight / eb.cas-wt, 2).
           ELSE ASSIGN v-tot-cas = ROUND(v-tot-qty / eb.cas-cnt, 2).

         IF v-bol-cases NE 0 
           THEN ASSIGN v-tot-cas = v-bol-cases.
       END. /* do */
     END. /* est-no ne "" */

     ASSIGN v-t-weight = 0
            v-tot-cas = 0
            v-tot-qty = 0.
   END. /* last-of i-no */
  END. /* each ar-invl */

  /** Build Salesman Id String **/
  ASSIGN v-salesman = "".
  FOR EACH w-sman BREAK BY w-sman.sman:
    IF FIRST-OF(w-sman.sman) 
     THEN ASSIGN v-salesman = v-salesman + w-sman.sman.
      DELETE w-sman.
  END.

  ASSIGN v-po-no = ar-inv.po-no
         v-bill-i = ar-inv.bill-i[1]
         v-ord-no = ar-inv.ord-no
         v-ord-date = ar-inv.ord-date.
     /*Get Sold To*/                
        find first oe-ord where oe-ord.company eq cocode
                         and oe-ord.ord-no   eq ar-inv.ord-no
                        no-lock no-error.
        IF AVAIL oe-ord THEN 
          v-billto-id = oe-ord.sold-id.
        IF v-billto-id = "" THEN v-billto-id = ar-inv.cust-no.
        FIND FIRST soldto WHERE soldto.company = ar-inv.company
                        AND soldto.cust-no = ar-inv.cust-no 
                        AND soldto.sold-id = v-billto-id NO-LOCK NO-ERROR.
        IF AVAIL soldto THEN 
                ASSIGN  
                v-billto-name = soldto.sold-name
                v-billto-addr[1] = soldto.sold-addr[1]
                v-billto-addr[2] = soldto.sold-addr[2]
                v-billto-city = soldto.sold-city
                v-billto-state = soldto.sold-state
                v-billto-zip = soldto.sold-zip
                v-billto-id = soldto.sold-id.
        v-billto-addr3 = v-billto-city + ", " + v-billto-state +
              "  " + v-billto-zip.

  IF v-salesman = "" THEN DO:

    FIND FIRST oe-ord WHERE oe-ord.company EQ cocode
                        AND oe-ord.ord-no  EQ ar-inv.ord-no NO-LOCK NO-ERROR.
    IF AVAIL oe-ord THEN DO:

      FIND FIRST sman WHERE sman.company = cocode 
                        AND sman.sman = oe-ord.sman[1] NO-LOCK NO-ERROR.
      ASSIGN v-salesman = oe-ord.sman[1].
      IF AVAIL sman THEN v-salesname = sman.sname.
    END.

    IF v-salesman = "" THEN DO:

      ASSIGN v-salesman = cust.sman.

      FIND FIRST sman WHERE sman.company = cocode 
                        AND sman.sman = cust.sman NO-LOCK NO-ERROR.
      IF AVAIL sman THEN v-salesname = sman.sname.
    END.
  END.

  FIND FIRST ar-invl WHERE ar-invl.x-no = ar-inv.x-no NO-LOCK NO-ERROR.
  IF AVAIL ar-invl THEN DO:

    ASSIGN v-price-head = ar-invl.pr-uom
           v-po-no = ar-invl.po-no                  
           v-ord-no = ar-invl.ord-no
           lv-bol-no = ar-invl.bol-no.
  END.

  {ar/rep/invcolnx2.i}  /* xprint form */

  ASSIGN v-subtot-lines = 0
         v-t-tax = 0.

  for each ar-invl no-lock where ar-invl.x-no = ar-inv.x-no:

    FOR EACH oe-boll NO-LOCK  
      WHERE oe-boll.company = ar-invl.company
        AND oe-boll.bol-no = ar-invl.bol-no
        AND oe-boll.i-no = ar-invl.i-no /*use-index bol-no*/:

        /** Build Case Count Display Lines **/
        IF oe-boll.p-c THEN v-pc = "C". /*complete*/
    END. /* each oe-boll */
    
    IF v-printline > 45 THEN DO:
      PAGE.
      {ar/rep/invcolnx2.i}  /* xprint form */
      ASSIGN v-printline = 21.
    END.

    FIND FIRST oe-ordl WHERE oe-ordl.company = cocode 
                         AND oe-ordl.ord-no = ar-invl.ord-no 
                         AND oe-ordl.i-no = ar-invl.i-no 
                         AND oe-ordl.LINE = ar-invl.LINE NO-LOCK NO-ERROR.
    IF AVAIL oe-ordl 
      THEN ASSIGN v-bo-qty = IF (ar-invl.qty - ar-invl.ship-qty - 
                                 oe-ordl.t-ship-qty) < 0 
                               THEN 0 
                               ELSE (ar-invl.qty - ar-invl.ship-qty -
                                     oe-ordl.t-ship-qty).
      ELSE ASSIGN v-bo-qty = IF ( ar-invl.qty - ar-invl.ship-qty ) < 0
                               THEN 0 
                               ELSE ar-invl.qty - ar-invl.ship-qty.
    ASSIGN v-inv-qty = ar-invl.qty
           v-ship-qty = ar-invl.ship-qty
           v-i-no = ar-invl.i-no
           v-i-dscr = ar-invl.i-name
           v-price = ar-invl.unit-pr * (1 - (ar-invl.disc / 100))
           v-t-price = ar-invl.amt
           v-subtot-lines = v-subtot-lines + ar-invl.amt.
           
    IF ar-invl.tax AND AVAIL stax 
      THEN DO i = 1 TO 3:

       IF stax.tax-code[i] NE "" THEN DO:
         CREATE w-tax.
         ASSIGN w-dsc      = stax.tax-dscr[i]
                w-tax      = ROUND((IF stax.accum-tax 
                                      THEN v-t-price
                                      ELSE ar-invl.amt) * 
                                    stax.tax-rate[i] / 100,2)
                v-t-price  = v-t-price + w-tax
                v-t-tax[i] = v-t-tax[i] + w-tax
                v-lines    = v-lines + 1.
       END.
    END.

    IF v-t-price NE ar-invl.amt THEN DO:
      CREATE w-tax.
      ASSIGN w-dsc     = "******ITEM TOTAL:"
             w-tax     = v-t-price
             v-lines   = v-lines + 1.
    END.

    ASSIGN v-po-no  = ar-invl.po-no
           v-ord-no = ar-invl.ord-no
           v-price-head = ar-invl.pr-uom
           v-i-dscr2 = ar-invl.part-dscr1.

    IF v-i-dscr2 = "" 
      THEN v-i-dscr2 = ar-invl.i-dscr.
    
    IF v-ord-no = 0 AND v-ship-qty = 0 
      THEN v-ship-qty = v-inv-qty.

 /*   FIND FIRST tt-inv-line
        WHERE tt-inv-line.i-no EQ ar-invl.i-no              /*Task# 01071405*/
          AND tt-inv-line.ord-no EQ v-ord-no NO-ERROR.
    IF NOT AVAIL tt-inv-line THEN DO:  */  
        CREATE tt-inv-line.
        ASSIGN
            tt-inv-line.po-no = v-po-no
            tt-inv-line.part-no = ar-invl.part-no
            tt-inv-line.i-dscr = v-i-dscr
            tt-inv-line.ship-qty = v-ship-qty
            tt-inv-line.price = v-price
            tt-inv-line.total-price = ar-invl.amt
            tt-inv-line.ord-no = v-ord-no
            tt-inv-line.i-no = ar-invl.i-no
            tt-inv-line.part-dscr = v-i-dscr2
            tt-inv-line.pc = v-pc
            tt-inv-line.price-head = v-price-head
            tt-inv-line.bol-no = ar-invl.bol-no
            tt-inv-line.job-no = ar-invl.job-no
            tt-inv-line.job-no2 = ar-invl.job-no2.
  /*  END.
    ELSE
        ASSIGN 
            tt-inv-line.ship-qty = tt-inv-line.ship-qty + v-ship-qty
            tt-inv-line.total-price = tt-inv-line.total-price + ar-invl.amt. */

  END. /*each ar-invl*/
  


  FOR EACH tt-inv-line:
   v-job-no = "".
   FIND FIRST job-hdr WHERE job-hdr.company EQ cocode
       AND job-hdr.job-no EQ tt-inv-line.job-no
       AND job-hdr.job-no2 EQ tt-inv-line.job-no2
       AND job-hdr.i-no EQ tt-inv-line.i-no NO-LOCK NO-ERROR.

   IF NOT AVAIL job-hdr THEN do:
      FIND FIRST job-hdr WHERE job-hdr.company EQ cocode
       AND job-hdr.ord-no EQ tt-inv-line.ord-no
       AND job-hdr.i-no EQ tt-inv-line.i-no NO-LOCK NO-ERROR.
   END.
   
   v-job-no = fill(" ",6 - length(trim(tt-inv-line.job-no))) +
      trim(tt-inv-line.job-no) .

   IF v-job-no EQ "" AND AVAIL job-hdr THEN
       v-job-no = fill(" ",6 - length(trim(job-hdr.job-no))) +
                 trim(job-hdr.job-no) .
   IF AVAIL job-hdr THEN
       v-job-no = v-job-no + "-" + trim(string(job-hdr.frm)) + trim(string(job-hdr.blank-no)) .


       IF v-printline > 49 THEN DO:  /* task 10181309  */
           PAGE.
           {ar/rep/invcolnx2.i}  /* xprint form */
               ASSIGN v-printline = 21.
       END.     /* task 10181309  */

    PUT SPACE(1)
         tt-inv-line.po-no 
         tt-inv-line.part-no  
        SPACE(1)
         tt-inv-line.i-dscr FORM "x(30)" 
         tt-inv-line.ship-qty  FORMAT "->>>>>>9" 
        SPACE(2)
         tt-inv-line.price   FORMAT "->>,>>9.9999"                
         tt-inv-line.total-price  FORMAT "->>>,>>9.99"                
        SKIP
         SPACE(1)
         v-job-no /*tt-inv-line.ord-no*/ SPACE(2)
         tt-inv-line.i-no SPACE(1)
         tt-inv-line.part-dscr  SPACE(11)
         tt-inv-line.pc  FORM "x" SPACE(7)
         tt-inv-line.price-head 
        SKIP .

    ASSIGN v-printline = v-printline + 2.

    IF v-printline > 49 THEN DO:  /* task 10181309  */
        
      PAGE.
      {ar/rep/invcolnx2.i}  /* xprint form */
      ASSIGN v-printline = 21.
    END.     /* task 10181309  */

    FOR EACH oe-boll NO-LOCK  
      WHERE oe-boll.company = ar-inv.company
        AND oe-boll.bol-no = tt-inv-line.bol-no
        AND oe-boll.i-no = tt-inv-line.i-no
        AND oe-boll.lot-no NE ""
/*        EACH reftable NO-LOCK                           */
/*       WHERE reftable.reftable EQ "oe-boll.lot-no"      */
/*         AND reftable.rec_key EQ STRING(RECID(oe-boll)) */
/*         USE-INDEX rec_key                              */
/*         BY reftable.CODE:                              */
        BY oe-boll.lot-no:

      PUT 
          SPACE(26)
          "Lot #: "  oe-boll.lot-no FORMAT "x(15)"
/*            "Lot #: "  reftable.CODE FORMAT "x(15)" */
          SPACE (10) 
           "Qty:"  STRING(oe-boll.qty,"->>>>>>9")
          SKIP. 
      ASSIGN v-printline = v-printline + 1.
    END.

    PUT SKIP(1).
    ASSIGN v-printline = v-printline + 1.

 
    IF v-printline > 49 THEN DO:  /* task 10181309  */
      PAGE.
      {ar/rep/invcolnx2.i}  /* xprint form */
      ASSIGN v-printline = 21.
    END.     /* task 10181309  */

  END. /* each ar-invl */
  FOR EACH tt-inv-line NO-LOCK:
    DELETE tt-inv-line .
  END.

  IF v-prntinst THEN DO:

    DO i = 1 to 4:

      IF ar-inv.bill-i[i] ne "" THEN DO:

        PUT ar-inv.bill-i[i] AT 10 SKIP.
        ASSIGN v-printline = v-printline + 1.
      END.
    END. /* 1 to 4 */
  END.

  
  ASSIGN v-notes = ""
         v-notes-line = 0
         lv-line-chars = 80.

  {custom/notesprtA.i ar-inv v-notes 4}

  IF v-printline > 49 THEN DO:
    PAGE.
    {ar/rep/invcolnx2.i}  /* xprint form */
    ASSIGN v-printline = 21.
  END.                                 

  IF v-notes[1] NE "" THEN DO: PUT "<C2>" v-notes[1] SKIP.
  ASSIGN v-printline = v-printline + 1.
  IF v-printline > 49 THEN DO:
    PAGE.
    {ar/rep/invcolnx2.i}  /* xprint form */
    ASSIGN v-printline = 21.
  END. 
  END.
  IF v-notes[2] NE "" THEN DO: PUT "<C2>" v-notes[2] SKIP.
  ASSIGN v-printline = v-printline + 1.
  IF v-printline > 49 THEN DO:
    PAGE.
    {ar/rep/invcolnx2.i}  /* xprint form */
    ASSIGN v-printline = 21.
  END. 
  END.
  IF v-notes[3] NE "" THEN DO: PUT "<C2>" v-notes[3] SKIP.
  ASSIGN v-printline = v-printline + 1.
  IF v-printline > 49 THEN DO:
    PAGE.
    {ar/rep/invcolnx2.i}  /* xprint form */
    ASSIGN v-printline = 21.
  END. 
  END.
  IF v-notes[4] NE "" THEN DO: PUT "<C2>" v-notes[4] SKIP.
  ASSIGN v-printline = v-printline + 1.
  IF v-printline > 49 THEN DO:
    PAGE.
    {ar/rep/invcolnx2.i}  /* xprint form */
    ASSIGN v-printline = 21.
  END. 
  END.
   

  ASSIGN v-frt-tax = ar-inv.freight
         v-inv-freight = IF ar-inv.f-bill 
                           THEN ar-inv.freight
                           ELSE 0.
        
  IF ar-inv.tax-code <> "" AND
     ar-inv.f-bill AND ar-inv.freight <> 0 AND AVAIL stax 
    THEN DO i = 1 TO 3:

      IF stax.tax-code[i] NE "" THEN DO:
        CREATE w-tax.
        ASSIGN w-dsc      = stax.tax-dscr[i]
               w-tax      = round((IF stax.company EQ "yes" 
                                     THEN v-frt-tax
                                     ELSE ar-inv.freight) *
                                   stax.tax-rate[i] / 100,2)                 
               v-frt-tax  = v-frt-tax + w-tax
               v-t-tax[i] = v-t-tax[i] + w-tax
               v-lines    = v-lines + 1.
      END.
  END.

  DO i = 1 TO 3:

    ASSIGN v-bot-lab[i] = IF v-t-tax[i] NE 0 
                            THEN ((IF AVAIL stax 
                                     THEN STRING(CAPS(stax.tax-code[i]),"x(5)")
                                     ELSE FILL(" ",5) ) +
                                          fill(" ",6) + ":" +
                                          STRING(v-t-tax[i],"->>>>>9.99")) 
                            ELSE "".
  END.

  ASSIGN v-inv-total = v-subtot-lines + v-t-tax[1] + 
                       v-t-tax[2] + v-t-tax[3] + v-inv-freight
         tmp1 = 0 
         tmp2 = ?
         v-net = ar-inv.net.
  
  FIND FIRST terms WHERE terms.t-code EQ ar-inv.terms NO-LOCK NO-ERROR.
  IF AVAIL terms 
    THEN ASSIGN tmp1 = v-net * (ROUND(terms.disc-rate / 100, 2))
                tmp2 = TODAY + terms.disc-days.

  PUT "<R57><C1><#7><FROM><C+80><LINE>"
      "<=7><C31><FROM><R+2.4><LINE>"
      "<=7><C40><FROM><R+2.4><LINE>"
      "<=7><C50><FROM><R+2.4><LINE>"
      "<=7><C60><FROM><R+2.4><LINE>"
      "<=7><C70><FROM><R+2.4><LINE>"
      "<=7><C81><FROM><R+2.4><LINE>"
      "<=7><C31>" v-inv-freight FORM ">>,>>9.99" v-net "  " tmp1 "    " tmp2 " " v-inv-total  FORM "->>>,>>9.99"
      "<=7><R+1.2><C31><FROM><C+50><LINE>"        
      "<=7><R+1.2><C34><P6> FREIGHT<C40> NET AMOUNT SUBJECT    CASH DISCOUNT        IF PAID BY         INVOICE AMOUNT"
      "<=7><R+1.7><C40><P6> TO CASH DISCOUNT          AMOUNT            THIS DATE  "
      "<=7><R+2.4><C31><FROM><C+50><LINE>"
      "<P9><R58><C1><#8><FROM><R+4><C+29><RECT> " 
      "<=8><R+.5>  Finance Charge of 1.5% per month"
      "<=8><R+1.5>  (18% APR) may be charged after"
      "<=8><R+2.5>  30 days from date of invoice."
      .


  PUT "<FArial><R61><C63><#9><P12><B> THANK YOU. </B> <P9> " SKIP .
     
     

  ASSIGN v-printline = v-printline + 6.

  IF v-printline <= 66 THEN page. /*PUT SKIP(74 - v-printline). */
     
    DO TRANSACTION:
      FIND FIRST xar-inv WHERE RECID(xar-inv) = RECID(ar-inv).
      ASSIGN xar-inv.printed = yes.
             xar-inv.stat = "X".
    END. /* DO TRANSACTION avail ar-inv */ 

END.  /* each ar-inv */

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */
