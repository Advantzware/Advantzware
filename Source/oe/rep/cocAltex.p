/* ------------------------------------------------ oe/rep/cocAltex.p */
/* Print Altex COC (Certificate of Compliance)                        */
/* -------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ip-bol-recid AS RECID NO-UNDO.

{sys/inc/var.i shared}
{sys/form/s-top.f}

{oe/rep/oe-lad.i}

def var v-cust-addr3 as char format "x(30)".
DEF VAR ls-image1 AS cha NO-UNDO.
DEF VAR v-manuf-date AS DATE INIT 12/31/2999 NO-UNDO.
def var v-bol-qty    AS INTEGER NO-UNDO.
DEF VAR v-type AS CHAR NO-UNDO.
DEF VAR v-rel-date AS DATE INIT 12/31/2999 NO-UNDO.
DEF VAR v-part-desc AS CHAR FORMAT "X(30)" NO-UNDO.
DEF VAR v-cad LIKE eb.cad-no NO-UNDO.
DEF VAR v-style LIKE eb.style NO-UNDO.
DEF VAR v-flute LIKE eb.flute NO-UNDO.
DEF VAR v-test LIKE eb.test NO-UNDO.
DEF VAR ls-image-s AS cha NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
def var v-ship-name  like shipto.ship-name NO-UNDO.
def var v-ship-addr  like shipto.ship-addr NO-UNDO.
def var v-ship-city  like shipto.ship-city NO-UNDO.
def var v-ship-state like shipto.ship-state NO-UNDO.
def var v-ship-zip   like shipto.ship-zip NO-UNDO.
def var v-ship-addr3 as   char format "x(30)" NO-UNDO.
DEF VAR ls-full-img-s AS CHARACTER FORMAT "x(200)" NO-UNDO.
DEF VAR ls-full-img1 AS CHARACTER FORMAT "x(200)" NO-UNDO.
DEFINE VARIABLE cOrderNo AS CHARACTER INIT "" NO-UNDO.
DEFINE VARIABLE cBoard         AS CHARACTER NO-UNDO.
DEFINE VARIABLE lValid         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.

 {sa/sa-sls01.i}
for each report where report.term-id eq v-term-id,
    first oe-bolh where recid(oe-bolh) eq report.rec-id no-lock:

    IF ip-bol-recid NE ? AND
       ip-bol-recid NE report.rec-id THEN
       NEXT.

    for each oe-boll
        where oe-boll.company eq cocode
          and oe-boll.b-no    eq oe-bolh.b-no
          AND oe-boll.qty NE 0
        no-lock:
   
      create xreport.
      assign
       xreport.term-id = v-term-id
       xreport.key-01  = report.key-01
       xreport.key-02  = report.key-02
       xreport.key-03  = report.key-03
       xreport.key-04  = report.key-04
       xreport.key-06  = oe-boll.i-no
       xreport.rec-id  = recid(oe-boll).
    end.
   
    delete report.
end.
IF NOT lPerBolLine THEN DO:
for each report where report.term-id eq v-term-id NO-LOCK,
    first oe-boll where recid(oe-boll) eq report.rec-id no-lock,
    first itemfg
    where itemfg.company eq cocode
      and itemfg.i-no    eq oe-boll.i-no
    no-lock,
    first oe-bolh where oe-bolh.b-no   eq oe-boll.b-no no-lock,
    first cust    where cust.cust-no   eq oe-bolh.cust-no no-lock

    break by oe-boll.i-no
          by oe-boll.ord-no
          BY oe-boll.line
          BY oe-boll.po-no
          BY oe-boll.job-no
          BY oe-boll.job-no2:

    RUN FileSys_GetBusinessFormLogo(cocode, oe-bolh.cust-no, oe-boll.loc, OUTPUT cRtnChar, OUTPUT lValid, OUTPUT cMessage).
            	      
    IF NOT lValid THEN
    DO:
        MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
    END.
    ASSIGN ls-full-img1 = cRtnChar + ">" .

    v-bol-qty = v-bol-qty + oe-boll.qty.
   
    if LAST-OF(oe-boll.ord-no) then do:
      
      FIND FIRST oe-ord NO-LOCK
                     WHERE oe-ord.company EQ oe-boll.company
                       AND oe-ord.ord-no  EQ oe-boll.ord-no
                     NO-ERROR.
      
       find first oe-ordl WHERE
            oe-ordl.company eq cocode AND
            oe-ordl.ord-no  eq oe-boll.ord-no AND
            oe-ordl.i-no    eq oe-boll.i-no AND
            oe-ordl.line    eq oe-boll.line
            no-lock no-error.

       RUN oe/custxship.p (oe-bolh.company,
                        oe-bolh.cust-no,
                        oe-bolh.ship-id,
                        BUFFER shipto).

       assign
           v-ship-name    = shipto.ship-name
           v-ship-addr[1] = shipto.ship-addr[1]
           v-ship-addr[2] = shipto.ship-addr[2]
           v-ship-addr3   = shipto.ship-city + ", " +
                            shipto.ship-state + "  " +
                            shipto.ship-zip.
      
       v-cust-addr3 = cust.city + ", " +
                      cust.state + "  " +
                      cust.zip. 
      
       if trim(v-cust-addr3) eq "," then
          v-cust-addr3 = "".

       if v-ship-addr[2] eq "" then
      assign
       v-ship-addr[2] = v-ship-addr3
       v-ship-addr3   = "".

    if trim(v-ship-addr3) eq "," then v-ship-addr3 = "".
      
       {oe\rep\cocAltex.i}
        
        FOR EACH fg-rcpth FIELDS(r-no rita-code post-date) NO-LOCK
              WHERE fg-rcpth.company  EQ cocode AND
                   fg-rcpth.job-no    EQ oe-ordl.job-no AND
                   fg-rcpth.job-no2   EQ oe-ordl.job-no2 AND
                   fg-rcpth.i-no      EQ oe-ordl.i-no AND
                   fg-rcpth.rita-code EQ "R" BY fg-rcpth.post-date DESC :

                   v-manuf-date = fg-rcpth.post-date .
                   LEAVE.
        END.

        IF oe-ordl.po-no-po NE 0 AND v-manuf-date EQ 12/31/2999 THEN
        FOR EACH fg-rcpth FIELDS(r-no rita-code post-date) NO-LOCK
              WHERE fg-rcpth.company   EQ cocode AND
                   fg-rcpth.po-no     EQ string(oe-ordl.po-no-po) AND
                   fg-rcpth.i-no      EQ oe-ordl.i-no AND
                   fg-rcpth.rita-code EQ "R" BY fg-rcpth.post-date DESC :
          
                   v-manuf-date = fg-rcpth.post-date .
                   LEAVE.
        END.
         
       IF AVAIL oe-ordl THEN
       DO:
          FIND FIRST eb WHERE
               eb.company EQ cocode AND
               eb.est-no EQ oe-ordl.est-no AND
               eb.form-no EQ oe-ordl.form-no AND
               eb.blank-no EQ oe-ordl.blank-no
               NO-LOCK NO-ERROR.
      
          for each oe-rel no-lock
              where oe-rel.company   eq oe-ordl.company
                and oe-rel.ord-no    eq oe-ordl.ord-no
                and oe-rel.i-no      eq oe-ordl.i-no
                and oe-rel.line      eq oe-ordl.line:
         
              RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT v-type).
      
              IF index("A,B,P",v-type) > 0 THEN
              DO:
                 IF oe-rel.rel-date LT v-rel-date THEN
                    v-rel-date = oe-rel.rel-date.
              END.
          END.
       END.

       FIND FIRST eb WHERE
            eb.company EQ cocode AND
            eb.est-no EQ oe-ordl.est-no AND
            eb.form-no EQ oe-ordl.form-no AND
            eb.blank-no EQ oe-ordl.blank-no
            NO-LOCK NO-ERROR.

       IF AVAILABLE eb THEN
            FIND FIRST ef NO-LOCK
                 WHERE ef.company EQ eb.company
                   AND ef.est-no  EQ eb.est-no 
                   AND ef.form-no EQ eb.form-no
                 NO-ERROR.
                 
       cBoard = IF AVAILABLE ef THEN ef.board ELSE "" .
            
       IF AVAIL oe-ordl THEN
          v-part-desc = oe-ordl.i-name.

       IF AVAIL eb AND eb.cad-no NE "" THEN
          v-cad = eb.cad-no.
       ELSE
          v-cad = itemfg.cad-no.
      IF AVAIL eb THEN
          ASSIGN
          v-style = eb.style
          v-flute = eb.flute
          v-test  = eb.test .
      ELSE
          ASSIGN v-style = ""
                 v-flute = ""
                 v-test  = "" .
      
          IF AVAIL oe-ordl AND oe-ordl.ord-no NE 0 THEN
              ASSIGN 
              cOrderNo = STRING(oe-ordl.ord-no).
          ELSE
              ASSIGN 
              cOrderNo = "" .
       PUT "<FArial>"
           "<C8><B>CUSTOMER:</B>"          "<C25>" cust.NAME SKIP(1)
           "<C8><B>ITEM DESC:</B>"         "<C25>" v-part-desc SKIP(1)
           "<C8><B>PART NO:</B>"           "<C25>" itemfg.part-no SKIP(1)
           "<C8><B>SIZE:</B>"              "<C25>" STRING(itemfg.l-score[50]) + " X " + STRING(itemfg.w-score[50]) + " X " + STRING(itemfg.d-score[50]) FORMAT "x(36)"  SKIP(1)
           "<C8><B>COLOR:</B>"             "<C25>" SKIP(1)
           "<C8><B>STYLE:</B>"             "<C25>" v-style SKIP(1)
           "<C8><B>STOCK:</B>"             "<C25>" cBoard FORMAT "x(10)" SKIP(1)
           "<C8><B>ORDER NO:</B>"          "<C25>" cOrderNo SKIP(1)
           "<C8><B>PO NO:</B>"             "<C25>" oe-boll.po-no SKIP(1)
           "<C8><B>MFG DATE:</B>"          "<C25>" (IF AVAIL oe-ord THEN STRING(oe-ord.due-date,"99/99/9999") ELSE "") SKIP(1)
           "<C8><B>QTY SHIPPED:</B>"       "<C25>" trim(STRING(v-bol-qty,"->>>>>>9")) FORMAT "x(10)" SKIP(1)
           "<C8><B>NO. OF CASES:</B>"      "<C25>" STRING(oe-boll.cases) SKIP(1)
           "<C8><B>NO. OF PALLETS:</B>"    "<C25>" STRING(oe-boll.tot-pallets) SKIP(1)
           SKIP(1)
           .
       PUT 
           "<FCourier New><P9>"
           "<C5>Altex Packaging lnc. certifies that our Order No. <B>" TRIM(cOrderNo) "</B> being shipped to <B>" cust.NAME "</B>" SKIP
           "<C5>under Part No. <B>" itemfg.part-no "</B> complies with all the requirements indicated in the reference" SKIP
           "<C5>specification."
           SKIP(1)
           "<C5>We have performed the necessary tests and procedures that guarantee integrity in conformance" SKIP
           "<C5>with the specifications received from <B>" cust.NAME
           "</B><P10>" SKIP(2)
           .     
      
       PUT  
           "<C5>        Certified by: __________________________________________________" SKIP
           "<C30>Quality Assurance Representative" SKIP(1)
           "<C5>                Date: __________________________________________________" SKIP(1).
              
      FIND FIRST users 
          WHERE users.user_id = USERID("nosweat")
          NO-LOCK NO-ERROR.
     ASSIGN
          v-bol-qty = 0
          v-rel-date = 12/31/2999
          v-manuf-date = 12/31/2999
          v-part-desc = ""
          v-cad = "".
       PAGE.
    end.
END.
END.
ELSE DO:
    for each report where report.term-id eq v-term-id NO-LOCK,
    first oe-boll where recid(oe-boll) eq report.rec-id no-lock,
    first itemfg
    where itemfg.company eq cocode
      and itemfg.i-no    eq oe-boll.i-no
    no-lock,
    first oe-bolh where oe-bolh.b-no   eq oe-boll.b-no no-lock,
    first cust    where cust.cust-no   eq oe-bolh.cust-no no-lock

    break by report.key-01
          by report.key-02
          by report.key-03
          by report.key-04
          by report.key-06 DESC:

    v-bol-qty = v-bol-qty + oe-boll.qty.
   
    /*if LAST-OF(report.key-06) then do:*/
      FIND FIRST oe-ord NO-LOCK
                     WHERE oe-ord.company EQ oe-boll.company
                       AND oe-ord.ord-no  EQ oe-boll.ord-no
                     NO-ERROR.
      
      
       find first oe-ordl WHERE
            oe-ordl.company eq cocode AND
            oe-ordl.ord-no  eq oe-boll.ord-no AND
            oe-ordl.i-no    eq oe-boll.i-no AND
            oe-ordl.line    eq oe-boll.line
            no-lock no-error.

       RUN oe/custxship.p (oe-bolh.company,
                        oe-bolh.cust-no,
                        oe-bolh.ship-id,
                        BUFFER shipto).

       assign
           v-ship-name    = shipto.ship-name
           v-ship-addr[1] = shipto.ship-addr[1]
           v-ship-addr[2] = shipto.ship-addr[2]
           v-ship-addr3   = shipto.ship-city + ", " +
                            shipto.ship-state + "  " +
                            shipto.ship-zip.
      
       v-cust-addr3 = cust.city + ", " +
                      cust.state + "  " +
                      cust.zip. 
      
       if trim(v-cust-addr3) eq "," then
          v-cust-addr3 = "".

       if v-ship-addr[2] eq "" then
      assign
       v-ship-addr[2] = v-ship-addr3
       v-ship-addr3   = "".

    if trim(v-ship-addr3) eq "," then v-ship-addr3 = "".
      
       {oe\rep\cocAltex.i}

       
        FOR EACH fg-rcpth FIELDS(r-no rita-code post-date) NO-LOCK
              WHERE fg-rcpth.company  EQ cocode AND
                   fg-rcpth.job-no    EQ oe-ordl.job-no AND
                   fg-rcpth.job-no2   EQ oe-ordl.job-no2 AND
                   fg-rcpth.i-no      EQ oe-ordl.i-no AND
                   fg-rcpth.rita-code EQ "R"  ,
              EACH fg-rdtlh WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
                          AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code 
                          AND fg-rdtlh.tag EQ oe-boll.tag NO-LOCK
               BY fg-rcpth.post-date DESC :
          
                   v-manuf-date = fg-rcpth.post-date .
                   LEAVE.
        END.

        IF oe-ordl.po-no-po NE 0 AND v-manuf-date EQ 12/31/2999 THEN
        FOR EACH fg-rcpth FIELDS(r-no rita-code post-date) NO-LOCK
              WHERE fg-rcpth.company   EQ cocode AND
                   fg-rcpth.po-no     EQ string(oe-ordl.po-no-po) AND
                   fg-rcpth.i-no      EQ oe-ordl.i-no AND
                   fg-rcpth.rita-code EQ "R" ,
               EACH fg-rdtlh WHERE fg-rdtlh.r-no EQ fg-rcpth.r-no
                          AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code 
                          AND fg-rdtlh.tag EQ oe-boll.tag NO-LOCK
               BY fg-rcpth.post-date DESC :
          
                   v-manuf-date = fg-rcpth.post-date .
                   LEAVE.
        END.
         
       IF AVAIL oe-ordl THEN
       DO:
          FIND FIRST eb WHERE
               eb.company EQ cocode AND
               eb.est-no EQ oe-ordl.est-no AND
               eb.form-no EQ oe-ordl.form-no AND
               eb.blank-no EQ oe-ordl.blank-no
               NO-LOCK NO-ERROR.
      
          for each oe-rel no-lock
              where oe-rel.company   eq oe-ordl.company
                and oe-rel.ord-no    eq oe-ordl.ord-no
                and oe-rel.i-no      eq oe-ordl.i-no
                and oe-rel.line      eq oe-ordl.line:
         
              RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT v-type).
      
              IF index("A,B,P",v-type) > 0 THEN
              DO:
                 IF oe-rel.rel-date LT v-rel-date THEN
                    v-rel-date = oe-rel.rel-date.
              END.
          END.
       END.

       FIND FIRST eb WHERE
            eb.company EQ cocode AND
            eb.est-no EQ oe-ordl.est-no AND
            eb.form-no EQ oe-ordl.form-no AND
            eb.blank-no EQ oe-ordl.blank-no
            NO-LOCK NO-ERROR.
        
       IF AVAILABLE eb THEN
            FIND FIRST ef NO-LOCK
                 WHERE ef.company EQ eb.company
                   AND ef.est-no  EQ eb.est-no 
                   AND ef.form-no EQ eb.form-no
                 NO-ERROR.
                 
       cBoard = IF AVAILABLE ef THEN ef.board ELSE "" .
                 
       IF AVAIL oe-ordl THEN
          v-part-desc = oe-ordl.i-name.

       IF AVAIL eb AND eb.cad-no NE "" THEN
          v-cad = eb.cad-no.
       ELSE
          v-cad = itemfg.cad-no.
      IF AVAIL eb THEN
          ASSIGN
          v-style = eb.style
          v-flute = eb.flute
          v-test  = eb.test .
      ELSE
          ASSIGN v-style = ""
                 v-flute = ""
                 v-test  = "" .
      
          IF AVAIL oe-ordl AND oe-ordl.ord-no NE 0 THEN
              ASSIGN 
              cOrderNo = STRING(oe-ordl.ord-no).
          ELSE
              ASSIGN 
              cOrderNo = "" .
       
       PUT "<FArial>"
           "<C8><B>CUSTOMER:</B>"          "<C25>" cust.NAME SKIP(1)
           "<C8><B>ITEM DESC:</B>"         "<C25>" v-part-desc SKIP(1)
           "<C8><B>PART NO:</B>"           "<C25>" itemfg.part-no SKIP(1)
           "<C8><B>SIZE:</B>"              "<C25>" STRING(itemfg.l-score[50]) + " X " + STRING(itemfg.w-score[50]) + " X " + STRING(itemfg.d-score[50])  FORMAT "x(36)" SKIP(1)
           "<C8><B>COLOR:</B>"             "<C25>" SKIP(1)
           "<C8><B>STYLE:</B>"             "<C25>" v-style SKIP(1)
           "<C8><B>STOCK:</B>"             "<C25>" cBoard FORMAT "x(10)" SKIP(1)
           "<C8><B>ORDER NO:</B>"          "<C25>" cOrderNo SKIP(1)
           "<C8><B>PO NO:</B>"             "<C25>" oe-boll.po-no SKIP(1)
           "<C8><B>MFG DATE:</B>"          "<C25>" (IF AVAIL oe-ord THEN STRING(oe-ord.due-date,"99/99/9999") ELSE "") SKIP(1)
           "<C8><B>QTY SHIPPED:</B>"       "<C25>" trim(STRING(v-bol-qty,"->>>>>>9")) FORMAT "x(10)" SKIP(1)
           "<C8><B>NO. OF CASES:</B>"      "<C25>" STRING(oe-boll.cases) SKIP(1)
           "<C8><B>NO. OF PALLETS:</B>"    "<C25>" STRING(oe-boll.tot-pallets) SKIP(1)
           SKIP(1)
           .
       PUT 
           "<FCourier New><P9>"
           "<C5>Altex Packaging lnc. certifies that our Order No. <B>" TRIM(cOrderNo) "</B> being shipped to <B>" cust.NAME "</B>" SKIP
           "<C5>under Part No. <B>" itemfg.part-no "</B> complies with all the requirements indicated in the reference" SKIP
           "<C5>specification."
           SKIP(1)
           "<C5>We have performed the necessary tests and procedures that guarantee integrity in conformance" SKIP
           "<C5>with the specifications received from <B>" cust.NAME
           "</B><P10>" SKIP(2)
           .
      
       PUT  
           "<C5>        Certified by: __________________________________________________" SKIP
           "<C30>Quality Assurance Representative" SKIP(1)
           "<C5>                Date: __________________________________________________" SKIP(1).
              
      FIND FIRST users 
          WHERE users.user_id = USERID("nosweat")
          NO-LOCK NO-ERROR.
     ASSIGN
          v-bol-qty = 0
          v-rel-date = 12/31/2999
          v-manuf-date = 12/31/2999
          v-part-desc = ""
          v-cad = "".
       PAGE.
   /* end.*/
END.
END.
/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */
