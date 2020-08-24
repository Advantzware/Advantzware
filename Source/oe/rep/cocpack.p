/* ---------------------------------------------- oe/rep/cocprempkg.p */
/* Print Premiere COC (Certificate of Compliance)                     */
/* -------------------------------------------------------------------*/

{sys/inc/var.i shared}
{sys/form/s-top.f} 

{oe/rep/oe-lad.i}


DEFINE VARIABLE v-comp-add1     AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add2     AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add3     AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add4     AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add5     AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE lv-display-comp AS LOG       NO-UNDO.
DEFINE VARIABLE lv-email        AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE lv-comp-name    AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE lv-comp-color   AS cha       NO-UNDO.
DEFINE VARIABLE lv-other-color  AS cha       INIT "BLACK" NO-UNDO.

DEFINE VARIABLE v-cust-addr3    AS CHARACTER FORMAT "x(30)".
DEFINE VARIABLE ls-image1       AS cha       NO-UNDO.
DEFINE VARIABLE v-manuf-date    AS DATE      INIT 12/31/2999 NO-UNDO.

DEFINE VARIABLE v-type          AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-rel-date      AS DATE      INIT 12/31/2999 NO-UNDO.
DEFINE VARIABLE v-part-desc     AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE v-cad           LIKE eb.cad-no NO-UNDO.
DEFINE VARIABLE v-style         LIKE eb.style NO-UNDO.
DEFINE VARIABLE v-flute         LIKE eb.flute NO-UNDO.
DEFINE VARIABLE v-test          LIKE eb.test NO-UNDO.
DEFINE VARIABLE ls-image-s      AS cha       NO-UNDO.
DEFINE VARIABLE v-ship-name     LIKE shipto.ship-name NO-UNDO.
DEFINE VARIABLE v-ship-addr     LIKE shipto.ship-addr NO-UNDO.
DEFINE VARIABLE v-ship-city     LIKE shipto.ship-city NO-UNDO.
DEFINE VARIABLE v-ship-state    LIKE shipto.ship-state NO-UNDO.
DEFINE VARIABLE v-ship-zip      LIKE shipto.ship-zip NO-UNDO.
DEFINE VARIABLE v-ship-addr3    AS CHARACTER FORMAT "x(30)" NO-UNDO.

DEFINE VARIABLE v-job-no        AS CHARACTER INIT "" NO-UNDO.
DEFINE VARIABLE lValid          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage        AS CHARACTER NO-UNDO.
DEFINE VARIABLE iOrderTotal     AS INTEGER   NO-UNDO.
DEFINE VARIABLE iShippedTotal   AS INTEGER   NO-UNDO.
DEFINE VARIABLE cCustomerPo     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iLineCount      AS INTEGER   NO-UNDO.
DEFINE BUFFER bf-oe-boll FOR oe-boll .


{sa/sa-sls01.i}
 
FIND FIRST company WHERE company.company = cocode NO-LOCK NO-ERROR.
 
FIND FIRST cust WHERE cust.company = cocode AND
    cust.active = "X" NO-LOCK NO-ERROR.
IF AVAILABLE cust THEN
    ASSIGN v-comp-add1  = cust.addr[1]
        v-comp-add2  = cust.addr[2]
        v-comp-add3  = cust.city + ", " + cust.state + "  " + cust.zip
        v-comp-add4  = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
        v-comp-add5  = "Fax     :  " + string(cust.fax,"(999)999-9999") 
        lv-email     = "Email:  " + cust.email 
        lv-comp-name = cust.NAME   
        . 
          
FOR EACH report WHERE report.term-id EQ v-term-id,
    FIRST oe-bolh WHERE RECID(oe-bolh) EQ report.rec-id NO-LOCK:

    
    FOR EACH oe-boll
        WHERE oe-boll.company EQ cocode
        AND oe-boll.b-no    EQ oe-bolh.b-no
        AND oe-boll.qty NE 0
        NO-LOCK BREAK BY oe-boll.LINE:
           
        CREATE xreport.
        ASSIGN
            xreport.term-id = v-term-id
            xreport.key-01  = report.key-01
            xreport.key-02  = report.key-02
            xreport.key-03  = report.key-03
            xreport.key-04  = report.key-04
            xreport.key-06  = oe-boll.i-no
            xreport.rec-id  = RECID(oe-boll).
    END.
   
    DELETE report.
END.

FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
    FIRST oe-boll WHERE RECID(oe-boll) EQ report.rec-id NO-LOCK,
    FIRST itemfg
    WHERE itemfg.company EQ cocode
    AND itemfg.i-no    EQ oe-boll.i-no
    NO-LOCK,
    FIRST oe-bolh WHERE oe-bolh.b-no   EQ oe-boll.b-no NO-LOCK,
    FIRST cust    WHERE cust.cust-no   EQ oe-bolh.cust-no NO-LOCK

    BREAK BY oe-boll.LINE 
    BY oe-boll.i-no: 
          
    FIND FIRST oe-ordl WHERE
        oe-ordl.company EQ cocode AND
        oe-ordl.ord-no  EQ oe-boll.ord-no AND
        oe-ordl.i-no    EQ oe-boll.i-no AND
        oe-ordl.line    EQ oe-boll.line
        NO-LOCK NO-ERROR.    
          
    cCustomerPo = oe-boll.po-no .  
          
    RUN oe/custxship.p (oe-bolh.company,
        oe-bolh.cust-no,
        oe-bolh.ship-id,
        BUFFER shipto).

    ASSIGN
        v-ship-name    = shipto.ship-name
        v-ship-addr[1] = shipto.ship-addr[1]
        v-ship-addr[2] = shipto.ship-addr[2]
        v-ship-addr3   = shipto.ship-city + ", " +
                            shipto.ship-state + "  " +
                            shipto.ship-zip.
      
    v-cust-addr3 = cust.city + ", " +
        cust.state + "  " +
        cust.zip. 
      
    IF TRIM(v-cust-addr3) EQ "," THEN
        v-cust-addr3 = "".

    IF v-ship-addr[2] EQ "" THEN
        ASSIGN
            v-ship-addr[2] = v-ship-addr3
            v-ship-addr3   = "".

    IF TRIM(v-ship-addr3) EQ "," THEN v-ship-addr3 = "".
    {oe\rep\cocpack.i}   
    LEAVE .          
END.
     
iLineCount = 0.
FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
    FIRST oe-boll WHERE RECID(oe-boll) EQ report.rec-id NO-LOCK,
    FIRST itemfg
    WHERE itemfg.company EQ cocode
    AND itemfg.i-no    EQ oe-boll.i-no
    NO-LOCK,
    FIRST oe-bolh WHERE oe-bolh.b-no   EQ oe-boll.b-no NO-LOCK,
    FIRST cust    WHERE cust.cust-no   EQ oe-bolh.cust-no NO-LOCK

    BREAK BY oe-boll.LINE 
    BY oe-boll.i-no:       
                     
    FIND FIRST oe-ordl WHERE
        oe-ordl.company EQ cocode AND
        oe-ordl.ord-no  EQ oe-boll.ord-no AND
        oe-ordl.i-no    EQ oe-boll.i-no AND
        oe-ordl.line    EQ oe-boll.line
        NO-LOCK NO-ERROR.
     FIND FIRST bf-oe-boll NO-LOCK
          WHERE bf-oe-boll.company EQ oe-boll.company
          AND bf-oe-boll.b-no EQ oe-boll.b-no 
          AND bf-oe-boll.po-no NE "" NO-ERROR .
    cCustomerPo = IF AVAILABLE bf-oe-boll THEN bf-oe-boll.po-no ELSE "".     

    RUN oe/custxship.p (oe-bolh.company,
        oe-bolh.cust-no,
        oe-bolh.ship-id,
        BUFFER shipto).

    ASSIGN
        v-ship-name    = shipto.ship-name
        v-ship-addr[1] = shipto.ship-addr[1]
        v-ship-addr[2] = shipto.ship-addr[2]
        v-ship-addr3   = shipto.ship-city + ", " +
                            shipto.ship-state + "  " +
                            shipto.ship-zip.
      
    v-cust-addr3 = cust.city + ", " +
        cust.state + "  " +
        cust.zip. 
      
    IF TRIM(v-cust-addr3) EQ "," THEN
        v-cust-addr3 = "".

    IF v-ship-addr[2] EQ "" THEN
        ASSIGN
            v-ship-addr[2] = v-ship-addr3
            v-ship-addr3   = "".

    IF TRIM(v-ship-addr3) EQ "," THEN v-ship-addr3 = "".            
         
    IF AVAILABLE oe-ordl THEN
    DO:
        FIND FIRST eb WHERE
            eb.company EQ cocode AND
            eb.est-no EQ oe-ordl.est-no AND
            eb.form-no EQ oe-ordl.form-no AND
            eb.blank-no EQ oe-ordl.blank-no
            NO-LOCK NO-ERROR.
      
        FOR EACH oe-rel NO-LOCK
            WHERE oe-rel.company   EQ oe-ordl.company
            AND oe-rel.ord-no    EQ oe-ordl.ord-no
            AND oe-rel.i-no      EQ oe-ordl.i-no
            AND oe-rel.line      EQ oe-ordl.line:
         
            RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT v-type).
      
            IF INDEX("A,B,P",v-type) > 0 THEN
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

    IF AVAILABLE eb AND eb.part-dscr1 NE "" THEN
        v-part-desc = eb.part-dscr1.
    ELSE IF AVAILABLE oe-ordl THEN
            v-part-desc = oe-ordl.i-name.

    IF AVAILABLE eb AND eb.cad-no NE "" THEN
        v-cad = eb.cad-no.
    ELSE
        v-cad = itemfg.cad-no.
    IF AVAILABLE eb THEN
        ASSIGN
            v-style = eb.style
            v-flute = eb.flute
            v-test  = eb.test .
    ELSE
        ASSIGN v-style = ""
            v-flute = ""
            v-test  = "" .
      
    IF AVAILABLE oe-ordl AND oe-ordl.ord-no NE 0 THEN
        ASSIGN 
            v-job-no = STRING(oe-ordl.ord-no).
    ELSE
        ASSIGN 
            v-job-no = "" .
                     
              
    PUT "<C1>" oe-boll.LINE FORMAT ">>>" SPACE(3) 
        itemfg.part-no FORMAT "x(15)"  SPACE(5)
        oe-boll.i-no  FORMAT "x(15)"  SPACE(2)
        itemfg.i-name  FORMAT "x(28)"  SPACE(1)
        "EA" SPACE(1)
        (IF AVAILABLE oe-ordl THEN oe-ordl.qty ELSE 0) FORMAT "->,>>>,>>9" SPACE(1)
        oe-boll.qty  FORMAT "->,>>>,>>9"  SKIP
        .
    iLineCount = iLineCount + 1.
           
    IF FIRST-OF(oe-boll.i-no) THEN
        iOrderTotal = iOrderTotal + (IF AVAILABLE oe-ordl THEN oe-ordl.qty ELSE 0) .             
    iShippedTotal = iShippedTotal +  oe-boll.qty . 
           
    IF iLineCount GT 31 THEN
    DO:
        iLineCount = 0 . 
        PAGE.
        {oe\rep\cocpack.i}      
    END.
         
     
    ASSIGN          
        v-rel-date   = 12/31/2999
        v-manuf-date = 12/31/2999
        v-part-desc  = ""
        v-cad        = "".
    IF LAST(oe-boll.LINE) THEN 
    DO:
        PUT "<R58><C63.8>" iOrderTotal FORMAT "->,>>>,>>9" "<C72.2>" iShippedTotal FORMAT "->,>>>,>>9" .
        PAGE.
    END.
          
      
/* end.*/
END.

/* END ---------------------------------- copr. 1998  Advanced Software, Inc. */
