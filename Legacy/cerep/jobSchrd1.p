/* ----------------------------------------------- cerep/jobSLdee.p 01/09 gdm */
/*  Schedule Labels for DEE                                                   */
/* -------------------------------------------------------------------------- */

/************* Parameters ******************/
DEFINE INPUT PARAMETER ip-multi-faxout   AS LOG  NO-UNDO.
DEFINE INPUT PARAMETER ip-lines-per-page AS INTEGER  NO-UNDO.
DEFINE INPUT PARAMETER icBegJobNo        AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iiBegJobNo2       AS INTEGER  NO-UNDO.
DEFINE INPUT PARAMETER icEndJobNo        AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER iiEndJobNo2       AS INTEGER  NO-UNDO.
DEFINE INPUT PARAMETER icBegMach         AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER icEndMach         AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER icBegForm         AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER icEndForm         AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER icBegBlnk         AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER icEndBlnk         AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER icBegOrder        AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER icEndOrder        AS CHARACTER NO-UNDO.

DEFINE        VARIABLE v_due          AS CHARACTER FORMAT "x(8)" NO-UNDO.
DEFINE        VARIABLE v_due2         AS CHARACTER FORMAT "x(8)" NO-UNDO.
DEFINE        VARIABLE tmp-dir        AS character NO-UNDO.
DEFINE SHARED VARIABLE list-name      AS character NO-UNDO.
DEFINE        VARIABLE init-dir       AS character NO-UNDO.
DEFINE        VARIABLE lines-per-page AS INTEGER   NO-UNDO.
DEFINE        VARIABLE v_shtsize      AS CHARACTER FORMAT "x(20)" NO-UNDO.
DEFINE        VARIABLE v-job-qty      AS INTEGER   NO-UNDO.
DEFINE        VARIABLE v-job-itm      AS INTEGER   NO-UNDO.
DEFINE        VARIABLE cUserName      AS CHARACTER NO-UNDO .
DEFINE        VARIABLE cDie           AS CHARACTER FORMAT "x(20)" NO-UNDO.
DEFINE        VARIABLE cDieDescr      AS CHARACTER FORMAT "x(20)" NO-UNDO.

DEFINE        VARIABLE v_lblcnt       AS INTEGER   NO-UNDO.
DEFINE        VARIABLE v_lpcnt        AS INTEGER   NO-UNDO.
DEFINE        VARIABLE iColorCount    AS INTEGER   NO-UNDO .
DEFINE        VARIABLE iFormCount     AS INTEGER   NO-UNDO .
DEFINE        VARIABLE iSheetsReq     AS INTEGER   EXTENT 20 NO-UNDO.
DEFINE        VARIABLE iSheetsCount   AS INTEGER   EXTENT 20 NO-UNDO.
DEFINE        VARIABLE iSheetTot      AS INTEGER   NO-UNDO .
DEFINE BUFFER bf-ef FOR ef .
DEFINE BUFFER bf-eb FOR eb .
{sys/inc/var.i shared}

{jcrep/r-ticket.i "shared"}

{cec/msfcalc.i}

ASSIGN 
    v_due  = STRING(TODAY,"99/99/99")
    v_due2 = STRING(TODAY,"99/99/99").

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

FIND FIRST users NO-LOCK WHERE 
    users.user_id EQ USERID(LDBNAME(1)) 
    NO-ERROR.
cUserName = IF AVAILABLE users THEN users.user_name ELSE "" .

PUT "<PREVIEW>" "</PROGRESS>".

FOR EACH oe-ord
    WHERE oe-ord.company EQ cocode 
    AND oe-ord.ord-no  GE INTEGER(icBegOrder)
    AND oe-ord.ord-no  LE integer(icEndOrder)
    no-lock, 

    each oe-ordl
    where oe-ordl.company    eq oe-ord.company
    and oe-ordl.ord-no     eq oe-ord.ord-no
    AND oe-ordl.job-no  GE icBegJobNo
    AND oe-ordl.job-no  LE icEndJobNo
    AND oe-ordl.job-no2  GE INTEGER(iiBegJobNo2)
    AND oe-ordl.job-no2  LE integer(iiEndJobNo2)
    NO-LOCK BREAK BY oe-ord.ord-no :
    
    FIND FIRST eb  NO-LOCK 
        WHERE  eb.company EQ cocode
        AND eb.est-no EQ oe-ordl.est-no
        AND eb.form-no NE 0
        NO-ERROR .

    FIND FIRST ef NO-LOCK
        WHERE  eb.company EQ cocode
        AND ef.est-no EQ eb.est-no
        AND ef.form-no EQ eb.form-no NO-ERROR .

    IF FIRST-OF(oe-ord.ord-no) THEN 
    DO:
       
        FIND FIRST cust NO-LOCK 
            WHERE cust.company EQ cocode 
            AND cust.cust-no EQ oe-ord.cust-no NO-ERROR .
        i = 0 .
        iSheetTot = 0 .
        FOR EACH bf-eb NO-LOCK
            WHERE bf-eb.company EQ cocode
            AND bf-eb.est-no EQ eb.est-no BREAK BY bf-eb.form-no:
           
            iColorCount = iColorCount + bf-eb.i-col .

            FIND FIRST job NO-LOCK 
                WHERE job.company EQ cocode
                AND job.job-no EQ oe-ordl.job-no 
                AND job.job-no2 EQ oe-ordl.job-no2 NO-ERROR .

            IF AVAILABLE job THEN
                for each job-mat
                 where job-mat.company eq cocode
                 AND job-mat.job     eq job.job
                 AND job-mat.frm     EQ bf-eb.form-no
                use-index job NO-LOCK BREAK BY job-mat.frm:
                    find item where item.company eq cocode and
                        item.i-no    eq job-mat.i-no
                        no-lock no-error.

                    FIND FIRST style NO-LOCK 
                        where style.company = cocode and
                              style.style = bf-eb.style
                            no-error. 
                    IF avail style and style.type = "f" AND AVAIL ITEM 
                        AND item.mat-type >= "1" and item.mat-type <= "4" then DO:
                        
                        i = i + 1.
                        
                        ASSIGN
                            iSheetsReq[i]   = job-mat.qty 
                            iSheetsCount[i] = bf-eb.form-no 
                            iSheetTot       = iSheetTot + job-mat.qty .
                    END.
                    ELSE IF AVAIL ITEM AND 
                        (item.mat-type = 'B' or item.mat-type = 'P' or item.mat-type = 'R') THEN do: 
                            i = i + 1.
                        ASSIGN
                            iSheetsReq[i]   = job-mat.qty 
                            iSheetsCount[i] = bf-eb.form-no 
                            iSheetTot       = iSheetTot + job-mat.qty .
                    END.
                END.
        END.
        FOR EACH bf-ef NO-LOCK
            WHERE bf-ef.company EQ cocode
            AND bf-ef.est-no EQ ef.est-no :
            iFormCount = iFormCount + 1 .
        END.
        
        ASSIGN
            cDie      = "" 
            cDieDescr = "".
        IF AVAILABLE eb THEN 
        DO:
            FIND FIRST prep NO-LOCK
                WHERE prep.company EQ eb.company
                AND prep.code    EQ eb.die-no NO-ERROR.
            ASSIGN
                cDie = IF eb.die-no NE "" THEN string(eb.die-no) ELSE "".
            cDieDescr = IF AVAILABLE prep THEN string(prep.dscr) ELSE "".
        END.

       
        PUT  "<FArial>".
        PUT  "<C+25><#1>".
        PUT  "<=1>" SKIP.
        PUT  "<C1><#2>".

      
        PUT UNFORMATTED 
            "<r2.8><#1><UNITS=INCHES><C63><FROM><c79><r4.9><BARCODE,TYPE=39,CHECKSUM=NONE,VALUE="
            oe-ordl.ord-no  ">".

        PUT "<AT=0.4,3.3><angle=270>" "----------------------------------------------------------------------------------" "</angle>" SKIP .


        PUT  "<R4><C4.5><FROM><R4><c28><LINE>" SKIP .
        PUT "<FArial><R4.3><B><C5><p11>" iSheetsCount[1] "<c19> " iSheetsReq[1]  "</B>" SKIP.
        PUT  "<R5.5><C4.5><FROM><R5.5><c28><LINE>" SKIP .
        IF iSheetsReq[2] NE 0 THEN
            PUT "<FArial><R5.8><B><C5><p11>" iSheetsCount[2] "<c19> "  iSheetsReq[2] "</B>" SKIP.
        PUT  "<R7><C4.5><FROM><R7><c28><LINE>" SKIP .
        IF iSheetsReq[3] NE 0 THEN
            PUT "<FArial><R7.3><B><C5><p11>" iSheetsCount[3] "<c19> "  iSheetsReq[3] "</B>" SKIP.
        PUT  "<R8.5><C4.5><FROM><R8.5><c28><LINE>" SKIP .
        IF iSheetsReq[4] NE 0 THEN
            PUT "<FArial><R8.8><B><C5><p11>" iSheetsCount[4] "<c19> "  iSheetsReq[4] "</B>" SKIP.
        PUT  "<R10><C4.5><FROM><R10><c28><LINE>" SKIP .
        IF iSheetsReq[5] NE 0 THEN
            PUT "<FArial><R10.3><B><C5><p11>" iSheetsCount[5] "<c19> " iSheetsReq[5]  "</B>" SKIP.
        PUT  "<R11.5><C4.5><FROM><R11.5><c28><LINE>" SKIP .
        IF iSheetsReq[6] NE 0 THEN
            PUT "<FArial><R11.8><B><C5><p11>" iSheetsCount[6] "<c19> " iSheetsReq[6]  "</B>" SKIP.
        PUT  "<R13><C4.5><FROM><R13><c28><LINE>" SKIP .
        IF iSheetsReq[7] NE 0 THEN
            PUT "<FArial><R13.3><B><C5><p11>" iSheetsCount[7] "<c19> " iSheetsReq[7]  "</B>" SKIP.
        PUT  "<R14.5><C4.5><FROM><R14.5><c28><LINE>" SKIP .
        IF iSheetsReq[8] NE 0 THEN
            PUT "<FArial><R14.8><B><C5><p11>" iSheetsCount[8] "<c19> " iSheetsReq[8]  "</B>" SKIP.
        PUT  "<R16><C4.5><FROM><R16><c28><LINE>" SKIP .
        IF iSheetsReq[9] NE 0 THEN
            PUT "<FArial><R16.3><B><C5><p11>" iSheetsCount[9] "<c19> " iSheetsReq[9]  "</B>" SKIP.
        PUT  "<R17.5><C4.5><FROM><R17.5><c28><LINE>" SKIP .
        IF iSheetsReq[10] NE 0 THEN
            PUT "<FArial><R17.8><B><C5><p11>" iSheetsCount[10] "<c19> " iSheetsReq[10]  "</B>" SKIP.
        PUT  "<R19><C4.5><FROM><R19><c28><LINE>" SKIP .
        IF iSheetsReq[11] NE 0 THEN
            PUT "<FArial><R19.3><B><C5><p11>" iSheetsCount[11] "<c19> " iSheetsReq[11]  "</B>" SKIP.
        PUT  "<R20.5><C4.5><FROM><R20.5><c28><LINE>" SKIP .
        IF iSheetsReq[12] NE 0 THEN
            PUT "<FArial><R20.8><B><C5><p11>" iSheetsCount[12] "<c19> " iSheetsReq[12]  "</B>" SKIP.
        PUT  "<R22><C4.5><FROM><R22><c28><LINE>" SKIP .
        IF iSheetsReq[13] NE 0 THEN
            PUT "<FArial><R22.3><B><C5><p11>" iSheetsCount[13] "<c19> " iSheetsReq[13]  "</B>" SKIP.
        PUT  "<R23.5><C4.5><FROM><R23.5><c28><LINE>" SKIP .
        IF iSheetsReq[14] NE 0 THEN
            PUT "<FArial><R23.8><B><C5><p11>" iSheetsCount[14] "<c19> " iSheetsReq[14]  "</B>" SKIP.
        PUT "<R24.5><B><C5><p11> Total sheets:   " iSheetTot "</B>" SKIP.

        PUT "<FArial><R2.7><B><C6><p11> Form        Sheets Required   </B>" SKIP.

        PUT "<FArial><R3><C35><p10> Max # of Colors: <B>" string(iColorCount) "</B>" "  # of Forms: <B>" STRING(iFormCount) "</B>" SKIP.
        PUT "<FArial><R4.5><C35.8><p10> Customer: <B>" IF AVAILABLE cust THEN cust.NAME ELSE "" FORMAT "x(30)"  "</B>" SKIP.
        PUT "<FArial><R5><C65.8><p10> Order # : <B>" oe-ordl.ord-no "</B>" skip  .
        PUT "<FArial><R6><C37.9><p10> Carton: <B>" cDieDescr "</B>" SKIP.

        PUT "<FArial><R7.5><C34.8><p10> Order Date: <B>" oe-ord.ord-date "</B>" SKIP.
        PUT "<FArial><R7.5><C60><p10> Delivery Date: <B>" oe-ord.due-date "</B>" SKIP.
        PUT "<FArial><R9><C35><p10> Sheet Size: <B>" IF AVAILABLE ef THEN string(ef.lsh-wid) + " X " + STRING(ef.lsh-len)  ELSE "" FORMAT  "x(37)" "</B>"   "Over/Under %: <B>" trim(string(oe-ord.over-pct,">>9")) + " / " + trim(string(oe-ord.under-pct,">>9")) "</B>" SKIP.
        PUT "<FArial><R10.5><C38.4><p10> Stock: <B>" IF AVAILABLE ef THEN ef.board ELSE "" FORMAT "x(10)" "</B>" SKIP.
        PUT "<FArial><R10.5><C64><p10> Caliper:  <B>" IF AVAILABLE ef THEN ef.cal ELSE 0 FORMAT  "->9.9999" "</B>" SKIP.

        PUT "<FArial><R12><B><C38><p10> Roll Size: "  "</B>" SKIP.
        PUT  "<R13><C46><FROM><R13><c50.5><LINE>" SKIP .
        PUT "<FArial><R13.5><C50><p10> Sheets Ordered: "  SKIP.
        PUT  "<R14.5><C62><FROM><R14.5><c78.5><LINE>" SKIP .

        PUT "<FArial><R15><C46><p10> Received/Press Count: "  SKIP.
        PUT  "<R16><C62><FROM><R16><c78.5><LINE>" SKIP .

        PUT "<FArial><R16.5><B><C51><p10> Stock Arrival: "  "</B>" SKIP.
        PUT  "<R17.5><C62><FROM><R17.5><c78.5><LINE>" SKIP .

        PUT "<FArial><R19><B><C35><p10> Materials   </B>    Artwork     Plates      Ink          Stock  " SKIP.
        PUT  "<R20><C35><FROM><R20><c41.5><LINE>" SKIP .
        PUT "<FArial><R20.5><B><C35><p10> Awaiting   </B>   " SKIP.
        PUT "<FArial><R22.3><B><C35><p10> On Hand   </B>   " SKIP.

        PUT "<||><R20.5><C45><#4><FROM><R21.8><C47.2><RECT>" SKIP.
        PUT "<||><R20.5><C51><#4><FROM><R21.8><C53.2><RECT>" SKIP.
        PUT "<||><R20.5><C57><#4><FROM><R21.8><C59.2><RECT>" SKIP.
        PUT "<||><R20.5><C63><#4><FROM><R21.8><C65.2><RECT>" SKIP.

        PUT "<||><R22.3><C45><#4><FROM><R23.6><C47.2><RECT>" SKIP.
        PUT "<||><R22.3><C51><#4><FROM><R23.6><C53.2><RECT>" SKIP.
        PUT "<||><R22.3><C57><#4><FROM><R23.6><C59.2><RECT>" SKIP.
        PUT "<||><R22.3><C63><#4><FROM><R23.6><C65.2><RECT>" SKIP.
      
        PUT "<FArial><R24.5><C35><p10> # up:    <B>" IF AVAILABLE eb THEN string(eb.num-up,">>>>") ELSE "0" "</B>" SKIP.
        PUT "<FArial><R24.5><C50><p10> L#:   <B>" cDie "</B>" SKIP.

        PUT "<FArial><R26><C35><p10> Corg.: <B>" IF AVAILABLE eb THEN STRING(eb.cas-len) + " X " + STRING(eb.cas-wid) + " X " + STRING(eb.cas-dep) ELSE "" FORMAT "x(50)" "</B>" SKIP.
        PUT "<FArial><R26><C50><p10> Qty : <B>" IF AVAILABLE eb THEN STRING(eb.cas-cnt * eb.cas-pal) ELSE ""  "</B>" SKIP.

        PUT "<FArial><R27.7><C35><p10> HSC " SKIP.
        PUT "<||><R27.5><C39><#4><FROM><R28.8><C41.2><RECT>" SKIP.
        PUT "<FArial><R27.7><C44><p10> RSC " SKIP.
        PUT "<||><R27.5><C50><#4><FROM><R28.8><C52.2><RECT>" SKIP.
        PUT "<FArial><R27.7><C54><p10> Add: " SKIP.
        PUT "<FArial><R29.5><C35><p10> <b>CSR: </b>" cUserName FORMAT "x(30)" SKIP.

        PUT "<FCourier New>".

    END.  /* first-of (oe-ord.ord-no) */
END.
