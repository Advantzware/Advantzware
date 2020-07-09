/* ---------------------------------------------- oe/rep/bolpacifp 10/02 YSK *
*    Program Name  : oe/rep/bolxprt.p                                        *
      Author       :                                                         *
      Purpose      :   PRINT Empire BOL                                      *
      Date         :                                                         *
      Modify By    : Aj 06/24/2008 Frieght Terms was not printing correctly  *
                     Prior it was from customer now its from oe-bolh table   *       
* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}


DEFINE        VARIABLE v-salesman       AS CHARACTER FORMAT "x(26)".
DEFINE        VARIABLE v-fob            AS CHARACTER FORMAT "x(12)".   
DEFINE        VARIABLE v-tot-palls      AS INTEGER   FORMAT "->,>>>,>>9".
DEFINE        VARIABLE v-tot-wt         AS DECIMAL   FORMAT "->>,>>>,>>9".
DEFINE        VARIABLE v-ord-no         LIKE oe-boll.ord-no.    
DEFINE        VARIABLE v-job-no         AS CHARACTER FORMAT "x(9)" NO-UNDO.
DEFINE        VARIABLE v-phone-num      AS CHARACTER FORMAT "x(13)" NO-UNDO.
DEFINE        VARIABLE v-ship-phone     AS CHARACTER FORMAT "X(13)" NO-UNDO.
              
DEFINE        VARIABLE v-ship-name      LIKE shipto.ship-name.
DEFINE        VARIABLE v-ship-addr      LIKE shipto.ship-addr.
DEFINE        VARIABLE v-ship-city      LIKE shipto.ship-city.
DEFINE        VARIABLE v-ship-state     LIKE shipto.ship-state.
DEFINE        VARIABLE v-ship-zip       LIKE shipto.ship-zip.
DEFINE        VARIABLE v-ship-addr3     AS CHARACTER FORMAT "x(30)".
DEFINE        VARIABLE v-comp-name      LIKE company.name.
DEFINE        VARIABLE v-comp-addr      LIKE company.addr.
DEFINE        VARIABLE v-comp-city      LIKE company.city.
DEFINE        VARIABLE v-comp-state     LIKE company.state.
DEFINE        VARIABLE v-comp-zip       LIKE company.zip.
DEFINE        VARIABLE v-comp-addr3     AS CHARACTER FORMAT "x(30)".
DEFINE        VARIABLE v-cust-addr3     AS CHARACTER FORMAT "x(30)".
DEFINE        VARIABLE v-1              LIKE oe-boll.cases INIT 1 NO-UNDO. 
DEFINE        VARIABLE v-terms          LIKE oe-ord.terms-d NO-UNDO.
DEFINE        VARIABLE v-frt-terms      AS CHARACTER FORMAT "x(12)" NO-UNDO.
DEFINE        VARIABLE v-zone           LIKE carr-mtx.del-zone NO-UNDO.


DEFINE        VARIABLE v-comp-add1      AS cha       FORM "x(30)" NO-UNDO.
DEFINE        VARIABLE v-comp-add2      AS cha       FORM "x(30)" NO-UNDO.
DEFINE        VARIABLE v-comp-add3      AS cha       FORM "x(30)" NO-UNDO.
DEFINE        VARIABLE v-comp-add4      AS cha       FORM "x(30)" NO-UNDO.
DEFINE        VARIABLE v-comp-add5      AS cha       FORM "x(30)" NO-UNDO.

DEFINE        VARIABLE v-printline      AS INTEGER   NO-UNDO.
DEFINE        VARIABLE lv-display-comp  AS LOG       NO-UNDO.  /* display company address */
DEFINE        VARIABLE lv-cust-no       AS cha       FORM "x(30)" NO-UNDO.
DEFINE        VARIABLE lv-comp-name     AS cha       FORM "x(30)" NO-UNDO.
DEFINE        VARIABLE lv-email         AS cha       FORM "x(40)" NO-UNDO.
DEFINE        VARIABLE v-cusx-add1      AS cha       NO-UNDO.
DEFINE        VARIABLE v-cusx-add2      AS cha       NO-UNDO.
DEFINE        VARIABLE v-cusx-add3      AS cha       NO-UNDO.
DEFINE        VARIABLE v-cusx-add4      AS cha       NO-UNDO.
DEFINE        VARIABLE v-cusx-add5      AS cha       NO-UNDO.
DEFINE        VARIABLE v-cusx-email     AS cha       NO-UNDO.
DEFINE        VARIABLE v-cusx-name      AS cha       NO-UNDO.

DEFINE        VARIABLE lv-comp-color    AS cha       NO-UNDO.
DEFINE        VARIABLE lv-other-color   AS cha       INIT "BLACK" NO-UNDO.
DEFINE        VARIABLE lv-bolfmt-int    AS INTEGER   NO-UNDO.
DEFINE        VARIABLE v-weight         LIKE oe-boll.weight NO-UNDO.
DEFINE        VARIABLE v-job-po         AS CHARACTER NO-UNDO.
DEFINE        VARIABLE v-phone          AS cha       NO-UNDO.
DEFINE        VARIABLE v-shipto-contact LIKE shipto.contact NO-UNDO.
DEFINE        VARIABLE v-ship-i         AS cha       EXTENT 4 FORM "x(60)" NO-UNDO.
DEFINE        VARIABLE v-tmp-lines      AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE v-print-barTag   AS LOG       NO-UNDO.
   
DEFINE SHARED VARIABLE cShipTO          AS CHARACTER NO-UNDO .
{fg/ttTransBins.i shared}  

DEFINE VARIABLE ls-image1      AS cha       NO-UNDO.
DEFINE VARIABLE ls-full-img1   AS cha       FORM "x(200)" NO-UNDO.
DEFINE VARIABLE lv-net-wt      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-receipt-date AS DATE      FORM "99/99/99" NO-UNDO.
DEFINE VARIABLE v-ship-from    AS CHARACTER NO-UNDO.
DEFINE VARIABLE iBolno         AS INTEGER   NO-UNDO .
DEFINE VARIABLE v-tot-pallets  AS DECIMAL   NO-UNDO .
DEFINE VARIABLE cCarrierDscr   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cRtnChar       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lValid         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdFileSysProcs AS HANDLE    NO-UNDO.
                 
RUN GetPrintBarTag IN SOURCE-PROCEDURE (OUTPUT v-Print-BarTag) NO-ERROR.

ASSIGN 
    tmpstore = FILL("-",130).

FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "BOLFMTTran" NO-LOCK NO-ERROR.
IF AVAILABLE sys-ctrl THEN 
    ASSIGN lv-display-comp = sys-ctrl.log-fld 
        lv-bolfmt-int   = sys-ctrl.int-fld.
ELSE ASSIGN lv-display-comp = NO
        lv-bolfmt-int   = 0.
            
RUN system/FileSysProcs.p PERSISTENT SET hdFileSysProcs.

RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormLogo", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound AND cRtnChar NE "" THEN 
DO:
    cRtnChar = DYNAMIC-FUNCTION (
        "fFormatFilePath" IN hdFileSysProcs,
        cRtnChar
        ).
                   
    /* Validate the N-K-1 BusinessFormLogo image file */
    RUN FileSys_ValidateFile IN hdFileSysProcs (
        INPUT  cRtnChar,
        OUTPUT lValid,
        OUTPUT cMessage
        ) NO-ERROR.

    IF NOT lValid THEN 
    DO:
        MESSAGE "Unable to find image file '" + cRtnChar + "' in N-K-1 setting for BusinessFormLogo"
            VIEW-AS ALERT-BOX ERROR.
    END.
END.

ASSIGN 
    ls-full-img1 = cRtnChar + ">" .            

FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "LOGOCOLR" NO-LOCK NO-ERROR.
IF AVAILABLE sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".

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
        lv-cust-no   = cust.cust-no 
        lv-comp-name = cust.NAME   
        v-cusx-add1  = v-comp-add1
        v-cusx-add2  = v-comp-add2
        v-cusx-add3  = v-comp-add3
        v-cusx-add4  = v-comp-add4
        v-cusx-add5  = v-comp-add5
        v-cusx-email = lv-email
        v-cusx-name  = lv-comp-name
        v-ship-from  = cust.loc
        .

                    
FOR EACH ttTransBin WHERE ttTransBin.IS-SELECTED NO-LOCK  :
                     
    RUN oe/pallcalc2.p (INPUT cocode,
        INPUT ttTransBin.i-no,
        INPUT ttTransBin.job-no,
        INPUT INT(ttTransBin.job-no2),
        INPUT ttTransBin.loc,
        INPUT ttTransBin.loc-bin,
        INPUT ttTransBin.tag,
        INPUT ttTransBin.cust-no,
        INPUT INT(ttTransBin.partial-count),
        INPUT INT(ttTransBin.qty),
        INPUT INT(ttTransBin.cases),
        OUTPUT v-tot-pallets).
    FIND FIRST itemfg NO-LOCK 
        WHERE itemfg.company EQ cocode 
        AND itemfg.i-no EQ ttTransBin.i-no NO-ERROR.
    

    ASSIGN 
        v-tot-wt          = v-tot-wt +  (((ttTransBin.case-count * ttTransBin.cases ) + ttTransBin.partial-count) * itemfg.weight-100 / 100)
        ttTransBin.tot-wt = (((ttTransBin.case-count * ttTransBin.cases ) + ttTransBin.partial-count) * itemfg.weight-100 / 100)
        v-tot-palls       = v-tot-palls + v-tot-pallets   .
/*IF ttTransBin.bol-no <> 0 THEN
    iBolno =  ttTransBin.bol-no .*/
END.

FIND FIRST ttTransBin NO-LOCK NO-ERROR .
IF NOT AVAILABLE ttTransBin THEN 
DO:   
    RETURN.
END.
IF iBolno EQ 0 THEN 
DO:
    RUN oe/oe-bolno.p (cocode, OUTPUT iBolno).
END.

{sa/sa-sls01.i}

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK.
v-printline = 0.

FOR EACH ttTransBin  WHERE ttTransBin.IS-SELECTED
    BREAK BY ttTransBin.blank-cust:
    
    FIND FIRST cust WHERE cust.company = cocode AND
        cust.active = "X" NO-LOCK NO-ERROR.
     
    ASSIGN 
        ttTransBin.bol-no = iBolno .        
  
    IF FIRST-OF(ttTransBin.blank-cust) THEN 
    DO:
  
        RUN oe/custxship.p (cust.company,
            lv-cust-no,
            cShipTO,
            BUFFER shipto).

        ASSIGN
            v-ship-name      = shipto.ship-name
            v-ship-addr[1]   = shipto.ship-addr[1]
            v-ship-addr[2]   = shipto.ship-addr[2]
            v-ship-addr3     = shipto.ship-city + ", " +
                      shipto.ship-state + "  " +
                      shipto.ship-zip
            v-phone-num      = cust.area-code + cust.phone
            v-ship-phone     = IF shipto.area-code + shipto.phone <> "" THEN
                      "(" + shipto.area-code + ")" + string(shipto.phone,"xxx-xxxx")
                      ELSE ""
    
            v-shipto-contact = shipto.contact  .

        IF v-phone = "" THEN v-phone = "(" + shipto.area-code + ")" + string(shipto.phone,"xxx-xxxx").
        IF v-shipto-contact = "" THEN v-shipto-contact = shipto.contact.

        IF shipto.broker THEN 
        DO:
            ASSIGN 
                v-comp-add1  = cust.addr[1]
                v-comp-add2  = cust.addr[2]
                v-comp-add3  = cust.city + ", " +
                        cust.state + "  " +
                        cust.zip
                v-comp-add4  = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
                v-comp-add5  = "Fax     :  " + string(cust.fax,"(999)999-9999") 
                lv-email     = "Email:  " + cust.email   
                lv-comp-name = cust.NAME .
       
        END.
        ELSE ASSIGN v-comp-add1  = v-cusx-add1
                v-comp-add2  = v-cusx-add2    
                v-comp-add3  = v-cusx-add3    
                v-comp-add4  = v-cusx-add4                
                v-comp-add5  = v-cusx-add5
                lv-email     = v-cusx-email
                lv-comp-name = v-cusx-name.
        ASSIGN
            v-comp-name    = cust.name
            v-comp-addr[1] = cust.addr[1]
            v-comp-addr[2] = cust.addr[2]
            v-comp-addr3   = cust.city + ", " +
                        cust.state + "  " +
                        cust.zip.

        IF TRIM(v-comp-addr3) EQ "," THEN v-comp-addr3 = "".
              
        IF v-comp-addr[2] EQ "" THEN
            ASSIGN
                v-comp-addr[2] = v-comp-addr3
                v-comp-addr3   = "".
        IF v-ship-addr[2] EQ "" THEN
            ASSIGN
                v-ship-addr[2] = v-ship-addr3
                v-ship-addr3   = "".

        IF TRIM(v-ship-addr3) EQ "," THEN v-ship-addr3 = "".
        IF TRIM(v-cust-addr3) EQ "," THEN v-cust-addr3 = "".

        ASSIGN
            v-salesman = ""
            v-fob      = ""
            v-terms    = "" .
        .
   
        IF NOT AVAILABLE carrier THEN
            FIND FIRST carrier WHERE carrier.company = cust.company
                AND carrier.carrier = shipto.carrier NO-LOCK NO-ERROR.

        v-salesman = cust.sman .
        cCarrierDscr = IF AVAILABLE carrier THEN carrier.dscr ELSE "" .
      
        ASSIGN 
            v-terms     = cust.terms
            v-frt-terms = IF cust.frt-pay EQ "P" THEN "Prepaid"
                           ELSE IF cust.frt-pay EQ "B" THEN "Bill"
                           ELSE IF cust.frt-pay EQ "C" THEN "Collect"
                           ELSE IF cust.frt-pay EQ "T" THEN "Third Party"
                           ELSE ""
            v-zone      = cust.del-zone.
             
        IF v-terms NE "" THEN
        DO:
            FIND FIRST terms WHERE terms.t-code EQ cust.terms NO-LOCK NO-ERROR.
            IF AVAILABLE terms THEN
                ASSIGN v-terms = terms.dscr.
        END.
      
        v-salesman = TRIM(v-salesman).
        v-job-no = IF ttTransBin.job-no = "" THEN "" ELSE (ttTransBin.job-no + "-" + STRING(ttTransBin.job-no2,">>")).
        IF v-salesman GT '' THEN
            IF substr(v-salesman,LENGTH(TRIM(v-salesman)),1) EQ "," THEN
                substr(v-salesman,LENGTH(TRIM(v-salesman)),1) = "".

        v-fob = IF cust.fob-code BEGINS "ORIG" THEN "Origin" ELSE "Destination".
  
        {fg/rep/boltxten.i}

    END. /* first-of(cust.cust-no) */
  
 
    PUT SPACE(2) ttTransBin.i-no FORMAT "x(15)" SPACE(7)
        ttTransBin.tag FORMAT "x(30)" SPACE(5)
        (ttTransBin.case-count * ttTransBin.cases ) + ttTransBin.partial-count SPACE(10)
        ttTransBin.tot-wt SKIP .
    
    v-printline = v-printline + 1.

    IF v-printline >= 40 THEN 
    DO: 
        MESSAGE "page " VIEW-AS ALERT-BOX ERROR .
        v-printline = 0.
        PAGE {1}.
        {fg/rep/boltxten.i}
    END.
  
  
    IF LAST-OF(ttTransBin.blank-cust) THEN 
    DO:
        IF v-comp-addr[2] = "" THEN
            ASSIGN v-comp-addr[2] = v-comp-addr3
                v-comp-addr3   = "".
        IF v-ship-addr[2] = "" THEN
            ASSIGN v-ship-addr[2] = v-ship-addr3
                v-ship-addr3   = "".
     
     
        /*{oe/rep/bolxprnt.i}*/
    

        /*IF oe-bolh.tot-pallets NE 0 THEN v-tot-palls = oe-bolh.tot-pallets.*/

        PUT "<R54><C50><#8><FROM><R+4><C+30><RECT> " 
            "<=8><R+1> Total Pallets       :" v-tot-palls 
            "<=8><R+3> Total Weight        :" v-tot-wt /*fORM ">>,>>9.99"*/ .
    
        ASSIGN 
            v-ship-i = "".
        /*IF v-print-shipnotes THEN
           ASSIGN v-ship-i[1] = oe-bolh.ship-i[1]
                  v-ship-i[2] = oe-bolh.ship-i[2]
                  v-ship-i[3] = oe-bolh.ship-i[3]
                  v-ship-i[4] = oe-bolh.ship-i[4].*/

        PUT "<FArial><R51><C1><P12><B>     Shipping Instructions: </B> <P9> " 
            "<R53><C1>" v-ship-i[1] AT 7 
            "<R54><C1>" v-ship-i[2] AT 7 
            "<R55><C1>" v-ship-i[3] AT 7 
            "<R56><C1>" v-ship-i[4] AT 7 
            "<R58><C1>"
            "__________________________________________________________________________________________________________________" 
            "<R59><C1>" "<B>  Signature of Receipt </B>" 
            "<R60><C7>" "Customer ________________________________________                       Carrier _______________________________________" 
            "<R62><C7>" "Date ____________________________________________                       Date _________________________________________"     
            .

        v-printline = v-printline + 14.
 
        /*  IF v-printline < 45 THEN PUT SKIP(60 - v-printline). */
        PAGE.
        v-printline = 0.
  
    END.  /* last-of*/

/*oe-bolh.printed = yes.*/
END. /* for each w-fg-rctd  */



