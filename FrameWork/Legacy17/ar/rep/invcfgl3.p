/* ------------------------------------------------------- ar/rep/invcfgl3.p */

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
DEF VAR v-net2        LIKE ar-inv.freight   NO-UNDO.

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

DEF VAR v-tailgate     AS CHAR NO-UNDO FORM "x(30)".
DEF VAR v-tail-price   AS CHAR NO-UNDO FORM "x(10)".
DEF VAR v-ship-qty1    AS CHAR FORMAT "x(10)"  NO-UNDO.
DEF VAR v-ship-qty1i   AS INT  NO-UNDO.
DEF VAR fg-uom-list    AS CHAR NO-UNDO.
DEF VAR v-price2       AS DECI NO-UNDO.

DEF VAR v-inv-date  AS DATE INITIAL TODAY FORMAT "99/99/9999" NO-UNDO.
DEF VAR v-date-ship AS DATE INITIAL TODAY                     NO-UNDO.
DEF VAR tmp2        AS DATE                                   NO-UNDO.

DEF VAR v-ans AS LOGICAL INITIAL NO NO-UNDO.

DEF BUFFER xar-inv FOR ar-inv.
DEF BUFFER b-oe-rel FOR oe-rel.
DEF BUFFER b-oe-rell FOR oe-rell.
DEF BUFFER ref-sell-price FOR reftable.

DEF TEMP-TABLE w-sman NO-UNDO
  FIELD sman AS char FORMAT "x(4)".

DEF TEMP-TABLE w-tax NO-UNDO
    FIELD w-dsc AS CHAR
    FIELD w-tax AS DEC.

RUN sys/ref/uom-ea.p (OUTPUT fg-uom-list).

/* === with xprint ====*/
ASSIGN ls-image1 = "images\ccci.jpg"
       FILE-INFO:FILE-NAME = ls-image1
       ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".

FIND FIRST company WHERE company.company EQ cocode NO-LOCK NO-ERROR.

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.

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

  ASSIGN /*v-po-no = ar-inv.po-no*/
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

  {ar/rep/invcfgl3.i}  /* xprint form */

  ASSIGN v-subtot-lines = 0
         v-t-tax = 0.

  for each ar-invl no-lock where ar-invl.x-no = ar-inv.x-no:

    v-po-no = ar-invl.lot-no.

    FOR EACH oe-boll NO-LOCK  
      WHERE oe-boll.company = ar-invl.company
        AND oe-boll.bol-no = ar-invl.bol-no
        AND oe-boll.i-no = ar-invl.i-no /*use-index bol-no*/:

        /** Build Case Count Display Lines **/
        IF oe-boll.p-c THEN v-pc = "C". /*complete*/
    END. /* each oe-boll */
    
    IF v-printline > 45 THEN DO:
      PAGE.
      {ar/rep/invcfgl3.i}  /* xprint form */
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
           v-price-head = fill(" ",5) + ar-invl.pr-uom
           v-i-dscr2 = ar-invl.part-dscr1
           v-tail-price = ""                         
           v-tailgate   = ""
           v-ship-qty1  = ""
           v-ship-qty1i = 0
           v-price2     = 0.

    FIND FIRST itemfg WHERE
         itemfg.company = ar-invl.company AND 
         itemfg.i-no    = ar-invl.i-no
         NO-LOCK NO-ERROR.

    IF AVAIL itemfg THEN
       FOR EACH fg-rcpth NO-LOCK OF itemfg 
           WHERE (IF AVAIL oe-ordl THEN fg-rcpth.job-no = oe-ordl.job-no ELSE TRUE) 
             AND (IF AVAIL oe-ordl THEN fg-rcpth.job-no2 = oe-ordl.job-no2 ELSE TRUE),
           EACH fg-rdtlh  WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no      
            AND fg-rdtlh.rita-code            EQ fg-rcpth.rita-code 
            AND fg-rdtlh.stack-code BEGINS "TAIL"
           NO-LOCK:

           ASSIGN v-tailgate = fg-rdtlh.stack-code.

           FOR EACH oe-boll NO-LOCK 
              WHERE oe-boll.company EQ ar-invl.company
                AND oe-boll.bol-no  EQ ar-invl.bol-no
                AND oe-boll.i-no    EQ ar-invl.i-no USE-INDEX bol-no:

               FIND FIRST b-oe-rell  
                 WHERE b-oe-rell.company  EQ oe-boll.company  
                   AND b-oe-rell.ord-no   EQ oe-boll.ord-no    
                   AND b-oe-rell.i-no     EQ oe-boll.i-no      
                   AND b-oe-rell.LINE     EQ oe-boll.line      
                   AND b-oe-rell.r-no     EQ oe-boll.r-no      
                   AND b-oe-rell.rel-no   EQ oe-boll.rel-no    
                   AND b-oe-rell.b-ord-no EQ oe-boll.b-ord-no  
                   AND b-oe-rell.po-no    EQ oe-boll.po-no     
                 USE-INDEX ord-no NO-LOCK NO-ERROR.
               IF AVAIL b-oe-rell THEN DO:
                  FIND b-oe-rel NO-LOCK WHERE 
                       b-oe-rel.r-no EQ b-oe-rell.link-no
                       USE-INDEX seq-no NO-ERROR.
                  IF NOT AVAIL b-oe-rel THEN
                    FIND FIRST b-oe-rel NO-LOCK
                        WHERE b-oe-rel.company  EQ b-oe-rell.company
                          AND b-oe-rel.link-no  EQ b-oe-rell.r-no
                          AND b-oe-rel.ord-no   EQ b-oe-rell.ord-no
                          AND b-oe-rel.i-no     EQ b-oe-rell.i-no
                          AND b-oe-rel.line     EQ b-oe-rell.line
                          AND b-oe-rel.rel-no   EQ b-oe-rell.rel-no
                          AND b-oe-rel.b-ord-no EQ b-oe-rell.b-ord-no
                          AND b-oe-rel.po-no    EQ b-oe-rell.po-no
                        USE-INDEX link NO-ERROR.
                    IF NOT AVAIL b-oe-rel THEN
                    FIND FIRST b-oe-rel NO-LOCK
                        WHERE b-oe-rel.company  EQ b-oe-rell.company
                          AND b-oe-rel.ord-no   EQ b-oe-rell.ord-no
                          AND b-oe-rel.i-no     EQ b-oe-rell.i-no
                          AND b-oe-rel.line     EQ b-oe-rell.line
                          AND b-oe-rel.rel-no   EQ b-oe-rell.rel-no
                          AND b-oe-rel.b-ord-no EQ b-oe-rell.b-ord-no
                          AND b-oe-rel.po-no    EQ b-oe-rell.po-no
                        USE-INDEX ord-item NO-ERROR.
                  IF AVAIL b-oe-rel THEN DO:
                     FIND FIRST ref-sell-price WHERE
                                ref-sell-price.reftable EQ "oe-rel.sell-price" AND
                                ref-sell-price.company  EQ STRING(b-oe-rel.r-no,"9999999999")
                     NO-LOCK NO-ERROR.
                     IF AVAIL ref-sell-price THEN DO:
                        ASSIGN v-price-head = string(ref-sell-price.val[1],">>>9.9999")
                               v-tail-price = string((ref-sell-price.val[1] * fg-rdtlh.qty),">>,>>9.99").
                     END.
                     RELEASE ref-sell-price.
    
                     ASSIGN v-ship-qty1 = STRING(fg-rdtlh.qty,">>>>>>>9")
                            v-ship-qty1i = fg-rdtlh.qty.
                  END. /* avail b-oe-rel */
               END. /* avail b-oe-rell */
               LEAVE.
           END. /* each oe-boll */
    END. /* avail itemfg */ 

    IF v-ship-qty1i > 0 THEN DO:
        RUN compute-ext-price (recid(ar-invl), v-ship-qty1i, OUTPUT v-price2).
        ASSIGN v-net2 = v-net2 + v-price2. 
    END.
    ELSE
        ASSIGN v-net2 = v-net2 + ar-invl.amt.

    IF v-i-dscr2 = "" THEN
       v-i-dscr2 = ar-invl.i-dscr.
    
    IF v-ord-no = 0 AND v-ship-qty = 0 THEN
       v-ship-qty = v-inv-qty.

    PUT SPACE(1)
         v-po-no 
         ar-invl.part-no  
        SPACE(1)
         v-i-dscr FORM "x(30)" 
         (IF v-ship-qty1i > 0 THEN v-ship-qty - v-ship-qty1i ELSE v-ship-qty)  FORMAT "->>>>>>9" 
        SPACE(2)
         v-price  FORMAT "->>,>>9.9999"                
         (IF v-ship-qty1i > 0 THEN v-price2 ELSE ar-invl.amt)   FORMAT "->>>,>>9.99"                
        SKIP
         v-ord-no SPACE(10)
         ar-invl.i-no SPACE(1)
         (IF v-tailgate <> "" THEN v-i-dscr2 ELSE "") FORM "x(30)"
         v-ship-qty1 
         SPACE(2)
         v-pc  FORM "x"
         v-price-head FORM "x(9)" SPACE(2)
         v-tail-price FORM "x(10)"
        SKIP .

    ASSIGN v-printline = v-printline + 2.

    IF v-tailgate <> "" THEN DO:
       PUT SPACE(40) v-tailgate SKIP.
       ASSIGN v-printline = v-printline + 1.
    END.

    /*
    FOR EACH oe-boll NO-LOCK  
      WHERE oe-boll.company = ar-invl.company
        AND oe-boll.bol-no = ar-invl.bol-no
        AND oe-boll.i-no = ar-invl.i-no,
       EACH reftable NO-LOCK
      WHERE reftable.reftable EQ "oe-boll.lot-no" 
        AND reftable.rec_key EQ STRING(RECID(oe-boll))
        BY reftable.CODE:

      PUT 
          SPACE(26)
           "Lot #: "  reftable.CODE FORMAT "x(15)"
          SPACE (10) 
           "Qty:"  STRING(oe-boll.qty,"->>>>>>9")
          SKIP.           
    END. */

    PUT SKIP(1).

    ASSIGN v-printline = v-printline + 1.
  END. /* each ar-invl */

  IF v-prntinst THEN DO:

    DO i = 1 to 4:

      IF ar-inv.bill-i[i] ne "" THEN DO:

        PUT ar-inv.bill-i[i] AT 10 SKIP.
        ASSIGN v-printline = v-printline + 1.
      END.
    END. /* 1 to 4 */
  END.

  IF v-printline > 45 THEN DO:
    PAGE.
    {ar/rep/invcfgl3.i}  /* xprint form */
    ASSIGN v-printline = 21.
  END.
  
  ASSIGN v-notes = ""
         v-notes-line = 0
         lv-line-chars = 80.

  {custom/notesprtA.i ar-inv v-notes 4}
        
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
  
  IF v-net2 <> ar-inv.gross THEN
     v-net = v-net2 - ar-inv.tax-amt.

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
      "<=7><C31>" v-inv-freight FORM ">>,>>9.99" v-net "->,>>>,>>9.99" " " tmp1 "    " tmp2 " " (IF v-net2 <> ar-inv.gross THEN v-net2 ELSE ar-inv.gross) FORMAT "->>>>,>>9.99"
      "<=7><R+1.2><C31><FROM><C+50><LINE>"        
      "<=7><R+1.2><C34><P6> FREIGHT<C40> NET AMOUNT SUBJECT    CASH DISCOUNT        IF PAID BY         INVOICE AMOUNT"
      "<=7><R+1.7><C40><P6> TO CASH DISCOUNT          AMOUNT            THIS DATE  "
      "<=7><R+2.4><C31><FROM><C+50><LINE>"
      "<P9><R58><C1><#8><FROM><R+4><C+29><RECT> " 
      "<=8><R+.5>  Finance Charge of 1.5% per month"
      "<=8><R+1.5>  (18% APR) may be charged after"
      "<=8><R+2.5>  30 days from date of invoice."
      .

  PUT "<FArial><R61><C63><#9><P12><B> THANK YOU. </B> <P9> " SKIP
      "<=9><R-6>" v-notes[1]
      "<=9><R-5>" v-notes[2]
      "<=9><R-4>" v-notes[3]
      "<=9><R-3>" v-notes[4]
      .

  ASSIGN v-printline = v-printline + 6.

  IF v-printline <= 66 THEN page. /*PUT SKIP(74 - v-printline). */
     
    DO TRANSACTION:
      FIND FIRST xar-inv WHERE RECID(xar-inv) = RECID(ar-inv).
      ASSIGN xar-inv.printed = yes.
             xar-inv.stat = "X".
    END. /* DO TRANSACTION avail ar-inv */ 

END.  /* each ar-inv */

PROCEDURE compute-ext-price.
   DEFINE INPUT PARAM in-recid AS RECID.
   DEFINE INPUT PARAM in-qty AS INTE NO-UNDO.
   DEFINE OUTPUT PARAM out-price AS DECI NO-UNDO.

   DEF BUFFER bf-ar-invl FOR ar-invl.

   FIND bf-ar-invl WHERE RECID(bf-ar-invl) = in-recid NO-LOCK NO-ERROR.
   IF AVAIL bf-ar-invl THEN DO:

     ASSIGN out-price = (bf-ar-invl.ship-qty - in-qty) / 1000 * bf-ar-invl.unit-pr.

     IF bf-ar-invl.pr-uom BEGINS "L" AND bf-ar-invl.pr-uom NE "LB" THEN
        out-price = bf-ar-invl.unit-pr *
                    IF (bf-ar-invl.ship-qty - in-qty) LT 0 THEN -1 ELSE 1.
     ELSE IF bf-ar-invl.pr-uom EQ "CS" THEN
        out-price = (bf-ar-invl.ship-qty - in-qty) /
                    (IF bf-ar-invl.cas-cnt NE 0 THEN
                        bf-ar-invl.cas-cnt
                     ELSE
                     IF itemfg.case-count NE 0 THEN
                        itemfg.case-count ELSE 1) *
                     bf-ar-invl.unit-pr.
     ELSE IF LOOKUP(bf-ar-invl.pr-uom,fg-uom-list) GT 0 THEN
        out-price = (bf-ar-invl.ship-qty - in-qty) * bf-ar-invl.unit-pr.
     ELSE
     FOR EACH uom
        WHERE uom.uom  EQ bf-ar-invl.pr-uom
          AND uom.mult NE 0
        NO-LOCK:
        out-price = (bf-ar-invl.ship-qty - in-qty) / uom.mult * bf-ar-invl.unit-pr.
        LEAVE.
     END.
     out-price = ROUND(out-price,2).

     IF bf-ar-invl.disc NE 0 THEN
       out-price = ROUND(out-price * (1 - (bf-ar-invl.disc / 100)),2).
   END.
   ELSE
       out-price = bf-ar-invl.amt.

END PROCEDURE.
