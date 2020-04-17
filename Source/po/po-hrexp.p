/* -------------------------------------------------- po/po-hrexp.p 02/02 JLF */
/*                                                                            */
/* Harry Rhode's export PO                                                    */
/*                                                                            */
/* -------------------------------------------------------------------------- */

DEFINE INPUT PARAMETER v-format AS CHARACTER NO-UNDO.

{sys/inc/var.i shared}
{sys/form/s-top.f}

DEFINE BUFFER xjob-mat FOR job-mat.
DEFINE BUFFER xitem FOR item.
DEFINE BUFFER b-ref1  FOR reftable.
DEFINE BUFFER b-ref2  FOR reftable.
DEFINE BUFFER bpo-ord FOR po-ord.
DEFINE VARIABLE cWinScpXmlLog AS CHARACTER.
DEFINE STREAM sReadLog.
DEFINE STREAM sLogFileTest.
{po/po-print.i}
{po/getPoAdders.i}

DEFINE VARIABLE v-sname LIKE shipto.ship-name.
DEFINE VARIABLE v-saddr LIKE shipto.ship-addr.
DEFINE VARIABLE v-scity LIKE shipto.ship-city.
DEFINE VARIABLE v-sstate LIKE shipto.ship-state.
DEFINE VARIABLE v-szip LIKE shipto.ship-zip.
DEFINE VARIABLE v-adder LIKE item.i-no EXTENT 7 NO-UNDO.
DEFINE VARIABLE xg-flag AS LOG INIT NO NO-UNDO.
DEFINE VARIABLE v-instr AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-ord-qty LIKE po-ordl.ord-qty EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-ord-cst LIKE po-ordl.cost NO-UNDO.
DEFINE VARIABLE v-setup LIKE e-item-vend.setup NO-UNDO.
DEFINE VARIABLE v-outfile AS CHARACTER EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-mach AS CHARACTER EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-line AS CHARACTER NO-UNDO.
DEFINE VARIABLE iNumericAdder AS INTEGER NO-UNDO.
DEFINE VARIABLE cAssignedCustId AS CHARACTER NO-UNDO.
DEFINE VARIABLE iAdderCount AS INTEGER NO-UNDO.
DEFINE VARIABLE iTempAdder AS INTEGER NO-UNDO.
DEFINE VARIABLE vCarrierDesc AS CHAR NO-UNDO.
DEFINE VARIABLE vFobCode AS CHAR NO-UNDO.
DEFINE VARIABLE vCustCityState AS CHAR NO-UNDO.
DEFINE VARIABLE vShipCityState AS CHAR NO-UNDO.
DEFINE VARIABLE vDueDate AS CHAR NO-UNDO.
DEFINE VARIABLE vWidthInt AS CHAR NO-UNDO.
DEFINE VARIABLE vWidthNum AS CHAR NO-UNDO.
DEFINE VARIABLE vWidthDen AS CHAR NO-UNDO.
DEFINE VARIABLE vWidth AS CHAR NO-UNDO.
DEFINE VARIABLE vLengthInt AS CHAR NO-UNDO.
DEFINE VARIABLE vLengthNum AS CHAR NO-UNDO.
DEFINE VARIABLE vLengthDen AS CHAR NO-UNDO.
DEFINE VARIABLE vLength AS CHAR NO-UNDO.
DEFINE VARIABLE vStyleCode AS INTEGER NO-UNDO.
DEFINE VARIABLE vStyleDesc AS CHAR NO-UNDO.
DEFINE VARIABLE vScore AS CHAR NO-UNDO.
DEFINE VARIABLE vOutWidth AS CHAR NO-UNDO.
DEFINE VARIABLE vBlankWidth AS CHAR NO-UNDO.
DEFINE VARIABLE vScore2 AS CHAR NO-UNDO.
DEFINE VARIABLE v-jobno AS CHAR NO-UNDO.
DEFINE VARIABLE v-form AS CHAR NO-UNDO.

DEFINE TEMP-TABLE tt-eiv NO-UNDO
    FIELD run-qty AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD setups AS DECIMAL DECIMALS 2 EXTENT 20.

{sys/inc/hrms.i}

FIND FIRST po-ctrl NO-LOCK WHERE 
    po-ctrl.company EQ cocode.
FIND FIRST company NO-LOCK WHERE 
    company.company EQ cocode.
FIND FIRST cust NO-LOCK WHERE 
    cust.company EQ cocode AND 
    cust.active  EQ "X"
    NO-ERROR.

ASSIGN 
    cAssignedCustId = "00105".  /* Default value */ 

IF AVAILABLE cust 
AND hrms-log AND hrms-dir NE "" THEN
print-po-blok:
FOR EACH report NO-LOCK WHERE 
    report.term-id EQ v-term-id,
    FIRST po-ord NO-LOCK WHERE 
        RECID(po-ord) EQ report.rec-id AND 
        CAN-FIND(FIRST po-ordl WHERE 
            po-ordl.company   EQ po-ord.company AND 
            po-ordl.po-no     EQ po-ord.po-no AND 
            po-ordl.item-type EQ YES AND 
            (v-printde-po OR NOT po-ordl.deleted)),
    FIRST vend NO-LOCK WHERE 
        vend.company EQ po-ord.company AND 
        vend.vend-no EQ po-ord.vend-no AND 
        (vend.po-export EQ "HRMS" OR (poexport-cha  EQ "HRMS" AND vend.an-edi-vend))
    BY po-ord.po-no:
  
    /* Cross-reference vendor to vendors customer number */  
    FIND FIRST sys-ctrl-shipto NO-LOCK WHERE 
        sys-ctrl-shipto.company EQ po-ord.company AND 
        sys-ctrl-shipto.cust-vend = FALSE AND 
        sys-ctrl-shipto.cust-vend-no EQ po-ord.vend-no
        NO-ERROR.
    IF AVAILABLE sys-ctrl-shipto THEN ASSIGN  
        cAssignedCustId = sys-ctrl-shipto.char-fld.

    ASSIGN 
        hrms-dir        = TRIM(hrms-dir,"/")
        hrms-dir        = TRIM(hrms-dir,"\")
        v-outfile[1]    = TRIM(hrms-dir) + "\dataxfer\in\"
        v-outfile[2]    = v-outfile[1] + STRING(TIME,"99999999")
        v-outfile[3]    = "po_" + TRIM(REPLACE(v-format, " ","")) + vend.vend-no +
                            SUBSTRING(STRING(YEAR(TODAY),"9999"),3,2) +
                            STRING(MONTH(TODAY),"99") +
                            STRING(DAY(TODAY),"99") +
                            SUBSTRING(STRING(TIME,"HH:MM:SS"),1,2) +
                            SUBSTRING(STRING(TIME,"HH:MM:SS"),4,2) +
                            SUBSTRING(STRING(TIME,"HH:MM:SS"),7,2) + ".dat"
        v-outfile[4]    = v-outfile[1] + v-outfile[3].
   
    FILE-INFO:FILE-NAME = v-outfile[1].
    IF FILE-INFO:FILE-TYPE EQ ?  THEN        
        OS-COMMAND SILENT  "mkdir " + VALUE(v-outfile[1]).
    OUTPUT TO VALUE(v-outfile[2]).

    IF po-ord.stat EQ "N" THEN DO:
        FIND bpo-ord EXCLUSIVE WHERE 
            ROWID(bpo-ord) = ROWID(po-ord).
        ASSIGN 
            bpo-ord.stat = "O".
    END.

    ASSIGN
        v-sname    = cust.name
        v-saddr[1] = cust.addr[1]
        v-saddr[2] = cust.addr[2]
        v-scity    = cust.city
        v-sstate   = cust.state
        v-szip     = cust.zip.
 
    IF po-ord.type EQ "D" THEN ASSIGN
        v-sname    = po-ord.ship-name
        v-saddr[1] = po-ord.ship-addr[1]
        v-saddr[2] = po-ord.ship-addr[2]
        v-scity    = po-ord.ship-city
        v-sstate   = po-ord.ship-state
        v-szip     = po-ord.ship-zip.

    FIND FIRST carrier
        WHERE carrier.company EQ po-ord.company
        AND carrier.carrier EQ po-ord.carrier
        NO-LOCK NO-ERROR.
    ASSIGN 
        vCarrierDesc    = IF AVAIL carrier THEN carrier.dscr ELSE po-ord.carrier 
        vFobCode        = IF po-ord.fob-code EQ "DEST" THEN "DESTINATION" ELSE "ORIGIN"
        vCustCityState  = TRIM(cust.city) + " " + TRIM(cust.state)
        vShipCityState  = TRIM(v-scity) + " " + TRIM(v-sstate).
         
    /* Order Download Specification */
    /* H1 */
    PUT 
        cAssignedCustId     FORMAT "x(5)"       /* CUSTOMER # */
        "01"                FORMAT "x(2)"       /* 01 */
        "000000"            FORMAT "x(6)"       /* 000000 */
        cust.area-code      FORMAT "999"        /* CUSTOMER PHONE # (pt 1) */
        "-"                 FORMAT "x"          /* CUSTOMER PHONE # (pt 2) */
        cust.phone          FORMAT "999-9999"   /* CUSTOMER PHONE # (pt 3) */
        cust.zip            FORMAT "x(12)"      /* CUSTOMER BILLING ZIP */
        vCarrierDesc        FORMAT "x(15)"      /* SHIP VIA */
        vFobCode            FORMAT "x(15)"      /* FREIGHT */
        po-ord.po-date      FORMAT "99999999"   /* P.O. DATE */
        FILL(" ",3)         FORMAT "x(3)"       /* 000 */
        cAssignedCustId     FORMAT "x(5)"       /* CUST # */
        FILL(" ",45)        FORMAT "x(45)"      /* 45 blank spaces */
        SKIP.

    /* H2 */
    PUT 
        cust.name           FORMAT "x(30)"      /* CUST BILLING NAME */
        cust.addr[1]        FORMAT "x(30)"      /* CUST BILLING ADDRESS */
        cust.addr[2]        FORMAT "x(30)"      /* CUST BILLING ADDRESS */
        vCustCityState      FORMAT "x(30)"      /* CUST BILLING CITY, ST */
        FILL(" ",8)         FORMAT "x(8)"       /* 8 blank spaces */   
        SKIP. 

    /* H3 */
    PUT 
        v-sname             FORMAT "x(30)"      /* CUST SHIP TO NAME */
        v-saddr[1]          FORMAT "x(30)"      /* CUST SHIP TO ADDRESS */
        v-saddr[2]          FORMAT "x(30)"      /* CUST SHIP TO ADDRESS */
        vShipCityState      FORMAT "x(30)"      /* CUST SHIP TO CITY, ST */
        FILL(" ",8)         FORMAT "x(8)"       /* 8 blank spaces */    
        SKIP.

    FOR EACH po-ordl NO-LOCK WHERE  
        po-ordl.company   EQ po-ord.company AND 
        po-ordl.po-no     EQ po-ord.po-no AND 
        po-ordl.item-type EQ YES AND 
        (v-printde-po OR NOT po-ordl.deleted),
        FIRST ITEM NO-LOCK WHERE 
            item.company  EQ cocode AND 
            item.i-no     EQ po-ordl.i-no AND 
            item.mat-type EQ "B"
        BY po-ordl.line:
  
        ASSIGN
            xg-flag = NO
            v-adder = "".

        FIND FIRST job NO-LOCK WHERE 
            job.company EQ cocode AND 
            job.job-no  EQ FILL(" ",6 - LENGTH(TRIM(po-ordl.job-no))) + TRIM(po-ordl.job-no) AND 
            job.job-no2 EQ po-ordl.job-no2
            NO-ERROR.
        
        IF AVAILABLE job THEN DO:
            FIND FIRST est NO-LOCK WHERE 
                est.company EQ job.company AND 
                est.est-no  EQ job.est-no
                NO-ERROR.
      
            FOR EACH job-mat NO-LOCK WHERE 
                job-mat.company  EQ cocode AND 
                job-mat.job      EQ job.job AND 
                job-mat.job-no   EQ job.job-no AND 
                job-mat.job-no2  EQ job.job-no2 AND 
                job-mat.i-no     EQ po-ordl.i-no AND 
                job-mat.frm      EQ po-ordl.s-num
                USE-INDEX job
                BREAK BY job-mat.blank-no DESCENDING:
                IF LAST(job-mat.blank-no)
                OR job-mat.blank-no EQ po-ordl.b-num THEN LEAVE.
            END.

            IF AVAILABLE job-mat THEN DO:
                FIND FIRST ef NO-LOCK WHERE 
                    ef.e-num   EQ job.e-num AND 
                    ef.form-no EQ job-mat.frm
                    NO-ERROR.
   
                ASSIGN
                    xg-flag = AVAILABLE ef AND (ef.xgrain EQ "S" OR ef.xgrain EQ "B")
                    i       = 1.
     
                FIND FIRST reftable
                    WHERE reftable.reftable EQ "util/b-hrms-x.w"
                    AND reftable.company  EQ po-ordl.company
                    AND reftable.code2    EQ po-ordl.i-no
                    NO-LOCK NO-ERROR.
                IF AVAIL reftable THEN ASSIGN 
                    v-adder[i] = STRING(INT(reftable.code),"9999").
                ELSE ASSIGN 
                    v-adder[1] = STRING("0000","x(4)").

                FOR EACH xjob-mat NO-LOCK WHERE 
                    xjob-mat.company  EQ cocode AND 
                    xjob-mat.job      EQ job-mat.job AND 
                    xjob-mat.job-no   EQ job-mat.job-no AND 
                    xjob-mat.job-no2  EQ job-mat.job-no2 AND 
                    xjob-mat.frm      EQ job-mat.frm AND 
                    xjob-mat.blank-no EQ job-mat.blank-no AND 
                    xjob-mat.i-no     NE job-mat.i-no,
                    FIRST xitem NO-LOCK WHERE 
                        xitem.company  EQ cocode AND 
                        xitem.i-no     EQ xjob-mat.i-no AND 
                        xitem.mat-type EQ "A",
                    FIRST reftable NO-LOCK WHERE 
                        reftable.reftable EQ "util/b-hrms-x.w" AND 
                        reftable.company  EQ xitem.company AND 
                        reftable.code2    EQ xitem.i-no:          
                    ASSIGN
                        i = i + 1
                        v-form = STRING(job-mat.frm,"99").
          
                    iTempAdder = INT(reftable.code) NO-ERROR.
                    IF NOT ERROR-STATUS:ERROR THEN 
                        v-adder[i] = STRING(INT(reftable.code),"9999").
                    ELSE ASSIGN 
                        v-adder[i] = STRING("0000","9999").
             
                    IF i GE 6 THEN LEAVE.
                END.
            END.
        END.
        
        ASSIGN 
            vDueDate    = SUBSTRING(STRING(YEAR(po-ordl.due-date),"9999"),3,2)  +
                          STRING(MONTH(po-ordl.due-date),"99") +
                          STRING(DAY(po-ordl.due-date),"99")
            vWidthInt   = STRING(TRUNC(po-ordl.s-wid,0),"999999")
            vWidthNum   = STRING((po-ordl.s-wid - trunc(po-ordl.s-wid,0)) * 16,"99")
            vWidthDen   = "16"
            vWidth      = STRING(TRUNC(po-ordl.s-wid,0),"999") + ":" + STRING((po-ordl.s-wid - trunc(po-ordl.s-wid,0)) * 16,"99")  
            vLengthInt  = STRING(TRUNC(po-ordl.s-len,0),"999999")
            vLengthNum  = STRING((po-ordl.s-len - trunc(po-ordl.s-len,0)) * 16,"99")
            vLengthDen  = "16"
            vLength     = STRING(TRUNC(po-ordl.s-len,0),"999") + ":" + STRING((po-ordl.s-len - trunc(po-ordl.s-len,0)) * 16,"99")
            .   

        /* STYLE CODE */
        RUN po/po-ordls.p (RECID(po-ordl)).
        {po/po-ordls.i}
        ASSIGN  
            vStyleCode = IF AVAILABLE b-ref1 OR AVAILABLE b-ref2 THEN 1 ELSE 2
            vStyleDesc = (IF vStyleCode EQ 1 THEN "SCORED" ELSE "TRIMMED") + " SHEET".
            
        /* QUANTITY SHEETS */
        ASSIGN 
            v-ord-qty[1] = po-ordl.ord-qty.
        IF po-ordl.pr-qty-uom NE "EA" THEN
            RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "EA",
                item.basis-w, po-ordl.s-len,
                po-ordl.s-wid, item.s-dep,
                v-ord-qty[1], OUTPUT v-ord-qty[1]).
        IF v-ord-qty[1] - trunc(v-ord-qty[1],0) GT 0 THEN
            v-ord-qty[1] = trunc(v-ord-qty[1],0) + 1.
        ASSIGN 
            v-ord-qty[2] = v-ord-qty[1].
        IF v-ord-qty[1] GT 99999999 THEN v-ord-qty[1] = 99999999.

        /* PRICE PER MSF */
        ASSIGN 
            v-ord-cst = po-ordl.cost.
        IF po-ordl.pr-uom NE "MSF" THEN
            RUN sys/ref/convcuom.p(po-ordl.pr-uom, "MSF",
                item.basis-w, po-ordl.s-len,
                po-ordl.s-wid, item.s-dep,
                v-ord-cst, OUTPUT v-ord-cst).
        IF v-ord-cst GT 9999.99 THEN v-ord-cst = 9999.99.
        /* END PRICE PER MSF */
                       
        /* SETUP CHARGE */
        RELEASE e-item.
        RELEASE e-item-vend.
        ASSIGN 
            v-setup = 0.
        FIND FIRST e-item OF item NO-LOCK NO-ERROR.
        IF AVAILABLE e-item THEN FIND FIRST e-item-vend OF e-item WHERE 
                e-item-vend.vend-no   EQ po-ord.vend-no AND 
                e-item-vend.item-type EQ YES
                NO-LOCK NO-ERROR.
        IF AVAILABLE e-item-vend THEN 
        DO:
            v-ord-qty[3] = po-ordl.ord-qty.
            IF po-ordl.pr-qty-uom NE e-item.std-uom THEN
                RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, e-item.std-uom,
                    item.basis-w, po-ordl.s-len,
                    po-ordl.s-wid, item.s-dep,
                    v-ord-qty[3], OUTPUT v-ord-qty[3]).
            EMPTY TEMP-TABLE tt-eiv.
            CREATE tt-eiv.
            DO i = 1 TO 10:
                ASSIGN
                    tt-eiv.run-qty[i] = e-item-vend.run-qty[i]
                    tt-eiv.setups[i] = e-item-vend.setups[i].
            END.
            IF AVAILABLE e-item-vend THEN 
            DO:
                DO i = 1 TO 10:
                    ASSIGN
                        tt-eiv.run-qty[i + 10] = e-item-vend.runQtyXtra[i]
                        tt-eiv.setups[i + 10] = e-item-vend.setupsXtra[i].
                END.
            END.
            DO i = 1 TO 20:
                IF v-ord-qty[3] LE tt-eiv.run-qty[i] THEN 
                DO:
                    v-setup = tt-eiv.setups[i].
                    LEAVE.
                END.
            END.
        END.
        IF v-setup GT 999.99 THEN v-setup = 999.99.
        /* END SETUP CHARGE */

        /* ORDERED SF */
        ASSIGN 
            v-ord-qty[3] = po-ordl.ord-qty.
        IF po-ordl.pr-qty-uom NE "SF" THEN
            RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "SF",
                item.basis-w, po-ordl.s-len,
                po-ordl.s-wid, item.s-dep,
                v-ord-qty[3], OUTPUT v-ord-qty[3]).
        IF v-ord-qty[3] - trunc(v-ord-qty[3],0) GT 0 THEN
            v-ord-qty[3] = trunc(v-ord-qty[3],0) + 1.
        ASSIGN 
            v-ord-qty[4] = v-ord-qty[3].
        IF v-ord-qty[3] GT 9999999 THEN v-ord-qty[3] = 9999999.
        /* END ORDERED SF */

        /* SCORE */
        ASSIGN 
            vScore = "".
        DO i = 1 TO 9:
            IF AVAILABLE b-ref1 AND b-ref1.val[i] NE 0 THEN ASSIGN 
                    vScore = vScore + STRING(TRUNC(b-ref1.val[i],0), ">>>") +
                                  ":" +
                                  STRING((b-ref1.val[i] - trunc(b-ref1.val[i],0)) * 100,"99") +
                                  substr(b-ref1.dscr,i,1).
        
            ELSE ASSIGN 
                    vScore = vScore + "       ".
        END.
        /* END SCORE */
        
        /* Additional Scores */
        ASSIGN 
            vscore2 = "".
        DO i = 10 TO 12:
            IF AVAILABLE b-ref1 AND b-ref1.val[i] NE 0 THEN ASSIGN 
                    vScore2 = vScore2 + STRING(TRUNC(b-ref1.val[i],0), ">>>") +
                                  ":" +
                                  STRING((b-ref1.val[i] - trunc(b-ref1.val[i],0)) * 100,"99") +
                                  substr(b-ref1.dscr,i,1).
        
            ELSE ASSIGN 
                    vScore2 = vScore2 + "       ".
        END.
        IF AVAILABLE b-ref2 THEN DO i = 1 TO 9:
            IF AVAILABLE b-ref2 AND b-ref2.val[i] NE 0 THEN ASSIGN 
                    vScore2 = vScore2 + STRING(TRUNC(b-ref2.val[i],0), ">>>") +
                                  ":" +
                                  STRING((b-ref2.val[i] - trunc(b-ref2.val[i],0)) * 100,"99") +
                                  substr(b-ref2.dscr,i,1).
        
            ELSE ASSIGN 
                    vScore2 = vScore2 + "       ".
        END.

        /* Blank/Out widths */
        ASSIGN 
            vOutWidth = STRING(TRUNC(po-ordl.s-wid,0),"999") + 
                        ":" + STRING((po-ordl.s-wid - trunc(po-ordl.s-wid,0)) * 16,"99")
            vBlankWidth = STRING(trunc(po-ordl.s-wid,0),"999") + 
                        ":" + STRING((po-ordl.s-wid - trunc(po-ordl.s-wid,0)) * 16,"99").
        /* End Blank/Out widths */

        /* (SF OR SM) PER M */
        ASSIGN 
            v-ord-qty[2] = v-ord-qty[4] / (v-ord-qty[2] / 1000).
        IF v-ord-qty[2] GT 99999999 THEN v-ord-qty[2] = 99999999.

        /* EXT'D (SF OR SM) ORDERED */
        IF v-ord-qty[4] GT 99999999 THEN v-ord-qty[4] = 99999999.

        IF hrms-int EQ 0 THEN 
        DO:
            /* Use actual i-no for board code and adders */
            iAdderCount  = 1.
            /* Board code */
            iNumericAdder = INTEGER(po-ordl.i-no) NO-ERROR.
            IF NOT ERROR-STATUS:ERROR THEN ASSIGN 
                    v-adder[iAdderCount] = STRING(iNumericAdder, "9999")
                    iAdderCount = iAdderCount + 1.
            /* Adder codes */
            EMPTY TEMP-TABLE ttPoAdders.
            RUN po/getPoAdders.p (INPUT ROWID(po-ordl), INPUT table ttPoAdders BY-REFERENCE).
            FOR EACH ttPoAdders i = 1 TO 6:
                iNumericAdder = INTEGER(ttPoAdders.adderCode) NO-ERROR.
                IF NOT ERROR-STATUS:ERROR THEN ASSIGN 
                        v-adder[iAdderCount] = STRING(iNumericAdder, "9999")
                        iAdderCount = iAdderCount + 1.
            END.
        END.
        ELSE IF hrms-int EQ 1 THEN 
            DO:
            /* Use HRMS cross-reference table (already done in lines 243-267 */
            END.

        /* SPECIAL INSTRUCTIONS */
        v-instr = "".

        FOR EACH notes WHERE notes.rec_key EQ po-ordl.rec_key NO-LOCK:
            v-instr = v-instr + " " + trim(notes.note_text).
        END.

        FOR EACH notes WHERE notes.rec_key EQ po-ord.rec_key NO-LOCK:
            v-instr = v-instr + " " + trim(notes.note_text).
        END.

        /* Job# */
        IF po-ordl.job-no NE "" THEN DO:
            IF hrms-int = 1 THEN ASSIGN 
                v-jobno = STRING(po-ordl.job-no,"x(6)") + "-" +
                          STRING(po-ordl.job-no2,"99") + "-" +
                          v-form.
            ELSE ASSIGN 
                v-jobno = STRING(po-ordl.job-no,"x(6)") + "-" +
                          STRING(po-ordl.job-no2,"99").
        END.
        ELSE ASSIGN 
            v-jobno = "         ".

        /* D1 */
        PUT 
            cAssignedCustId     FORMAT "x(5)"       /* CUSTOMER # */
            "01"                FORMAT "x(2)"       /* 01 */
            po-ord.po-no        FORMAT "999999"     /* PURCHASE ORDER # */
            "A"                 FORMAT "x(1)"       /* A */
            po-ordl.line        FORMAT "99"         /* PURCHASE ORDER # */
            po-ord.po-no        FORMAT "999999"     /* PURCHASE ORDER # */
            "A"                 FORMAT "x(1)"       /* A */
            po-ordl.line        FORMAT "99"         /* PURCHASE ORDER # */
            FILL(" ",2)         FORMAT "xx"         /* MESSAGE CODE #1 */
            FILL(" ",2)         FORMAT "xx"         /* MESSAGE CODE #2 */
            FILL(" ",2)         FORMAT "xx"         /* MESSAGE CODE #3 */
            FILL(" ",7)         FORMAT "x(7)"       /* 7 blank spaces */
            "BY"                FORMAT "x(5)"       /* BY */
            vDueDate            FORMAT "x(6)"       /* DUE DATE */
            po-ord.over-pct     FORMAT "99"         /* OVERRUN PERCENTAGE */
            po-ord.under-pct    FORMAT "99"         /* UNDERRUN PERCENTAGE */
            vWidthInt           FORMAT "999999"     /* INTEGER OF WIDTH */
            vWidthNum           FORMAT "99"         /* NUMERATOR OF WIDTH */
            vWidthDen           FORMAT "99"         /* DENOMINATOR OF WIDTH */
            vWidth              FORMAT "x(6)"       /* WIDTH */
            vLengthInt          FORMAT "999999"     /* INTEGER OF LENGTH */
            vLengthNum          FORMAT "99"         /* NUMERATOR OF LENGTH */
            vLengthDen          FORMAT "99"         /* DENOMINATOR OF LENGTH */
            vLength             FORMAT "x(6)"       /* LENGTH */
            vStyleCode          FORMAT "9999"       /* STYLE */
            vStyleDesc          FORMAT "x(14)"      /* STYLE DESCRIPTION */
            item.basis-w        FORMAT "9999"       /* WEIGHT OF BOARD */
            v-ord-qty[1]        FORMAT "99999999"   /* ORDER QUANTITY */
            FILL(" ",13)        FORMAT "x(13)"      /* 13 blank spaces */     
            SKIP.



        /* D2 */
        PUT 
            item.flute          FORMAT "x(3)"       /* ITEM FLUTE */
            v-ord-cst           FORMAT "9999.99"    /* ORDER COST */
            v-setup             FORMAT "999.99"     /* SETUP CHARGE */
            1                   FORMAT "999.99"     /* "001.00" */
            0                   FORMAT "99999999.9999"  /* "00000000.0000" */
            v-ord-qty[3]        FORMAT "9999999"    /* ORDERED SF */
            FILL(" ",46)        FORMAT "x(46)"      /* 46 blank spaces */
            "X"                 FORMAT "x"          /* "X" */
            "X"                 FORMAT "x"          /* "X" */
            po-ordl.i-name      FORMAT "x(30)"      /* DESCRIPTION TEXT */
            FILL(" ",8)         FORMAT "x(8)"       /* 8 blank spaces */       
            SKIP.  

        /* D3 */
        PUT 
            po-ordl.i-name      FORMAT "x(128)"     /* ITEM NAME */
            SKIP.
        
        /* D4 */
        PUT 
            vScore              FORMAT "x(63)"      /* SCORE */
            1                   FORMAT "999.99"     /* NUMBER UP */
            FILL(" ",2)         FORMAT "xx"         /* TRIM */
            vOutWidth           FORMAT "x(6)"       /* 1 OUT WIDTH, NO TRIM */
            vBlankWidth         FORMAT "x(6)"       /* BLANK WIDTH (MULT OUT) */
            v-ord-qty[2]        FORMAT "99999999"   /* (SF OR SM) PER M */
            v-ord-qty[4]        FORMAT "99999999"   /* EXT'D (SF OR SM) ORDERED */
            v-adder[1]          FORMAT "x(4)"       /* Board code */
            v-adder[2]          FORMAT "x(4)"       /* Adders */
            v-adder[3]          FORMAT "x(4)"       /* Adders */
            v-adder[4]          FORMAT "x(4)"       /* Adders */
            v-adder[5]          FORMAT "x(4)"       /* Adders */
            v-adder[6]          FORMAT "x(4)"       /* Adders */
            v-adder[7]          FORMAT "x(4)"       /* Adders */
            FILL(" ",1)         FORMAT "x(1)"       /* Empty space */
            SKIP.

        /* D4 Second instance */
        PUT 
            vscore2             FORMAT "x(84)"      /* Additional Scores */
            FILL(" ",1)         FORMAT "x(1)"       /* Blank space */       
            SKIP.
  
        /* D5 */
        /* Tri-lakes / EFI */
        IF hrms-int EQ 1 THEN PUT 
            v-instr             FORMAT "x(64)"      /* Notes */
            v-jobno             FORMAT "x(12)"       /* Job No */
            FILL(" ",52)        FORMAT "x(55)"       
            SKIP.
        /* Valley */
        ELSE PUT 
            v-instr             FORMAT "x(64)"      /* Notes */
            v-jobno             FORMAT "x(9)"       /* Job No */
            FILL(" ",55)        FORMAT "x(55)"       
            SKIP.

    END. /* for each po-ordl record */

    FIND bpo-ord EXCLUSIVE WHERE 
        ROWID(bpo-ord) = ROWID(po-ord).
    ASSIGN 
        bpo-ord.printed = YES.

    IF SEARCH(v-outfile[2]) NE ? THEN 
    DO:
        OUTPUT close.

        OS-COPY VALUE(v-outfile[2]) VALUE(v-outfile[4]).
        OS-DELETE VALUE(v-outfile[2]).
  
        RUN po/ftppo.p (v-outfile[4],"HRMS"). 
        MESSAGE "HRMS file:" TRIM(v-outfile[3]) "has been created" 
            VIEW-AS ALERT-BOX.
        cWinScpXmlLog = v-outfile[4] + ".xml".
        RUN checkXmlLogResult.            
    END.

    PAUSE 1 NO-MESSAGE.
END. /* for each po-ord record */

PROCEDURE checkXmlLogResult:
    DEFINE VARIABLE cLogLine AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lTransferSuccess AS LOGICAL NO-UNDO.

    lTransferSuccess = FALSE.
    // To ensure process has finished
    PAUSE 1. 
    IF SEARCH(cWinscpXmlLog) NE ? THEN DO:
        lTransferSuccess = FALSE.
        INPUT STREAM sReadLog FROM VALUE(SEARCH(cWinscpXmlLog)).
        REPEAT:
            cLogLine = "".
            IMPORT STREAM sReadLog UNFORMATTED cLogLine.
            IF INDEX(cLogLine, "Result") GT 0 AND INDEX(cLogLine, "success") GT 0 THEN 
            DO:
                IF INDEX(cLogLine, "true") GT 0 THEN 
                DO:
                    lTransferSuccess = TRUE.
                END.
            END. 
        END. /* repeat */
        INPUT STREAM sReadLog CLOSE.
    END.
    OUTPUT stream sLogFileTest to value("n:\environments\prod\custfiles\logs\TestXmlLog.txt") append.
    PUT STREAM sLogFileTest UNFORMATTED cWinScpXmlLog " " STRING(lTransferSuccess) SKIP.
    
    IF NOT lTransferSuccess THEN 
        MESSAGE "Warning: The purchase order was not transmitted."
            VIEW-AS ALERT-BOX.
    OUTPUT stream sLogFileTest close.
END PROCEDURE.

