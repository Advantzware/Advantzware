/* -------------------------------------------------- po/po-ccexp.p */
/*                                                                  */
/* iPaper export PO (copy of iPaper)                              */
/*  3 header line and 5 detail lines                                */
/* -----------------------------------------------------------------*/

DEFINE INPUT PARAMETER v-format AS CHARACTER NO-UNDO.

{sys/inc/var.i shared}
{sys/form/s-top.f}

DEFINE BUFFER xjob-mat FOR job-mat.
DEFINE BUFFER xitem    FOR item.
DEFINE BUFFER b-ref1   FOR reftable.
DEFINE BUFFER b-ref2   FOR reftable.

{po/po-print.i}
{methods/getExecutableFileName.i quoter}

DEFINE VARIABLE v-sname   LIKE shipto.ship-name.
DEFINE VARIABLE v-saddr   LIKE shipto.ship-addr.
DEFINE VARIABLE v-scity   LIKE shipto.ship-city.
DEFINE VARIABLE v-sstate  LIKE shipto.ship-state.
DEFINE VARIABLE v-szip    LIKE shipto.ship-zip.
DEFINE VARIABLE v-adder   LIKE item.i-no EXTENT 6 NO-UNDO.
DEFINE VARIABLE xg-flag   AS LOG       INIT NO NO-UNDO.
DEFINE VARIABLE v-instr   AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-ord-qty LIKE po-ordl.ord-qty EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-ord-cst LIKE po-ordl.cost NO-UNDO.
DEFINE VARIABLE v-setup   LIKE e-item-vend.setup NO-UNDO.
DEFINE VARIABLE v-outfile AS CHARACTER EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-mach    AS CHARACTER EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-line    AS CHARACTER NO-UNDO.
DEFINE VARIABLE li-style  AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-mch-cod AS CHARACTER NO-UNDO.


DEFINE TEMP-TABLE tt-eiv NO-UNDO
    FIELD run-qty AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD setups  AS DECIMAL DECIMALS 2 EXTENT 20.

DEFINE VARIABLE v-rtn-char  AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-rec-found AS LOG       NO-UNDO.
DEFINE VARIABLE iPaper-dir  LIKE sys-ctrl.descrip NO-UNDO.
DEFINE VARIABLE iPaper-log  LIKE sys-ctrl.log-fld NO-UNDO.

RUN sys/ref/nk1look.p (cocode, "iPaper", "L", NO, NO, "", "", 
    OUTPUT v-rtn-char, OUTPUT v-rec-found).

IF v-rec-found THEN
    iPaper-log = LOGICAL(v-rtn-char) NO-ERROR.

RUN sys/ref/nk1look.p (cocode, "iPaper", "DS", NO, NO, "", "", 
    OUTPUT v-rtn-char, OUTPUT v-rec-found).
    
IF v-rec-found THEN
    iPaper-dir = v-rtn-char NO-ERROR.
    
{sys/inc/poexport.i}


FIND FIRST po-ctrl NO-LOCK WHERE po-ctrl.company EQ cocode .

FIND FIRST company NO-LOCK WHERE company.company EQ cocode .

FIND FIRST cust NO-LOCK
    WHERE cust.company EQ cocode
    AND cust.active  EQ "X"
    NO-ERROR.

IF AVAILABLE cust AND iPaper-log AND iPaper-dir NE "" THEN
    print-po-blok:
    FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,

        FIRST po-ord
        WHERE RECID(po-ord) EQ report.rec-id
        AND CAN-FIND(FIRST po-ordl
        WHERE po-ordl.company   EQ po-ord.company
        AND po-ordl.po-no     EQ po-ord.po-no
        AND po-ordl.item-type EQ YES
        AND (v-printde-po OR NOT po-ordl.deleted)),

        FIRST vend NO-LOCK
        WHERE vend.company EQ po-ord.company
        AND vend.vend-no EQ po-ord.vend-no
        AND (vend.po-export EQ "iPaper" OR
        (poexport-cha  EQ "iPaper" AND vend.an-edi-vend))
        
                   
        BY po-ord.po-no.

        IF OPSYS EQ "UNIX" AND substr(iPaper-dir,1,1) NE v-slash THEN
            iPaper-dir = v-slash + iPaper-dir.

        IF substr(iPaper-dir,LENGTH(iPaper-dir),1) EQ v-slash THEN
            substr(iPaper-dir,LENGTH(iPaper-dir),1) = "".
    
        ASSIGN
            v-outfile[1] = TRIM(iPaper-dir) + v-slash + "dataxfer" +
                  v-slash + "in" + v-slash
            v-outfile[2] = v-outfile[1] + string(TIME,"99999999")
            v-outfile[3] = "po_" + trim(v-format) + "iPaper" +
                  substr(STRING(YEAR(TODAY),"9999"),3,2) +
                  string(MONTH(TODAY),"99") +
                  string(DAY(TODAY),"99") +
                  substr(STRING(TIME,"HH:MM:SS"),1,2) +
                  substr(STRING(TIME,"HH:MM:SS"),4,2) +
                  substr(STRING(TIME,"HH:MM:SS"),7,2) + ".dat"
            v-outfile[4] = v-outfile[1] + v-outfile[3].

        OUTPUT to value(v-outfile[2]).

        IF po-ord.stat EQ "N" THEN po-ord.stat = "O".

        ASSIGN
            v-sname    = cust.name
            v-saddr[1] = cust.addr[1]
            v-saddr[2] = cust.addr[2]
            v-scity    = cust.city
            v-sstate   = cust.state
            v-szip     = cust.zip.
 
        IF po-ord.type EQ "D" THEN
            ASSIGN
                v-sname    = po-ord.ship-name
                v-saddr[1] = po-ord.ship-addr[1]
                v-saddr[2] = po-ord.ship-addr[2]
                v-scity    = po-ord.ship-city
                v-sstate   = po-ord.ship-state
                v-szip     = po-ord.ship-zip.

        FIND FIRST carrier NO-LOCK
            WHERE carrier.company EQ po-ord.company
            AND carrier.carrier EQ po-ord.carrier
            NO-ERROR.

        /* Order Download Specification */

        /* H1 */

        /* CUSTOMER # */
        PUT "20500"                                     FORMAT "x(5)".

        /* 01 */
        PUT "01"                                        FORMAT "x(2)".

        /* 000000 */
        PUT "000000"                                    FORMAT "x(6)".

        /* CUSTOMER PHONE # */
        PUT cust.area-code                              FORMAT "999"
            "-"                                         FORMAT "x"
            cust.phone                                  FORMAT "999-9999".

        /* CUSTOMER BILLING ZIP */
        PUT cust.zip                                    FORMAT "x(12)".

        /* SHIP VIA */
        PUT STRING(IF AVAILABLE carrier THEN carrier.dscr ELSE po-ord.carrier)
            FORMAT "x(15)".

        /* FREIGHT */
        PUT IF po-ord.fob-code EQ "DEST" THEN "DESTINATION" ELSE "ORIGIN"
            FORMAT "x(15)".

        /* P.O. DATE */
        PUT po-ord.po-date                              FORMAT "99999999".

        /* 000 */
        PUT FILL(" ",3)                                 FORMAT "x(3)".

        /* CUST # */
        PUT "20500" /*cust.cust-no*/                    FORMAT "x(5)".

        /* 45 blank spaces */
        PUT FILL(" ",45)                                FORMAT "x(45)"      SKIP.

        /* H2 */
    
        /* CUST BILLING NAME */
        PUT cust.name                                   FORMAT "x(30)".
    
        /* CUST BILLING ADDRESS */
        PUT cust.addr[1]                                FORMAT "x(30)".
    
        /* CUST BILLING ADDRESS */
        PUT cust.addr[2]                                FORMAT "x(30)".
    
        /* CUST BILLING CITY, ST */
        PUT TRIM(cust.city) + " " + trim(cust.state)    FORMAT "x(30)".

        /* 8 blank spaces */
        PUT FILL(" ",8)                                 FORMAT "x(8)"       SKIP.

        /* H3 */
    
        /* CUST SHIP TO NAME */
        PUT v-sname                                     FORMAT "x(30)".
    
        /* CUST SHIP TO ADDRESS */
        PUT v-saddr[1]                                  FORMAT "x(30)".
    
        /* CUST SHIP TO ADDRESS */
        PUT v-saddr[2]                                  FORMAT "x(30)".
    
        /* CUST SHIP TO CITY, ST */
        PUT TRIM(v-scity) + " " + trim(v-sstate)        FORMAT "x(30)".

        /* 8 blank spaces */
        PUT FILL(" ",8)                                 FORMAT "x(8)"       SKIP.

        FOR EACH po-ordl
            WHERE po-ordl.company   EQ po-ord.company
            AND po-ordl.po-no     EQ po-ord.po-no
            AND po-ordl.item-type EQ YES
            AND (v-printde-po OR NOT po-ordl.deleted),
      
            FIRST ITEM NO-LOCK
            WHERE item.company  EQ cocode
            AND item.i-no     EQ po-ordl.i-no
            AND item.mat-type EQ "B"
            BY po-ordl.line:
      
            ASSIGN
                xg-flag   = NO
                v-adder   = ""
                v-mch-cod = "".

            FOR EACH job-mch NO-LOCK WHERE job-mch.company EQ cocode
                AND job-mch.job-no EQ po-ordl.job-no
                AND job-mch.job-no2 EQ po-ordl.job-no2
                AND job-mch.frm EQ po-ordl.s-num USE-INDEX line-idx :

                ASSIGN 
                    v-mch-cod = job-mch.m-code .
                LEAVE.
            END.

            FIND FIRST job NO-LOCK
                WHERE job.company EQ cocode
                AND job.job-no  EQ fill(" ",6 - length(TRIM(po-ordl.job-no))) +
                trim(po-ordl.job-no)
                AND job.job-no2 EQ po-ordl.job-no2
                NO-ERROR.
        
            IF AVAILABLE job THEN 
            DO:
                FIND FIRST est NO-LOCK
                    WHERE est.company EQ job.company
                    AND est.est-no  EQ job.est-no
                    NO-ERROR.
      
                FOR EACH job-mat NO-LOCK
                    WHERE job-mat.company  EQ cocode
                    AND job-mat.job      EQ job.job
                    AND job-mat.job-no   EQ job.job-no
                    AND job-mat.job-no2  EQ job.job-no2
                    AND job-mat.i-no     EQ po-ordl.i-no
                    AND job-mat.frm      EQ po-ordl.s-num
                    USE-INDEX job 
                    BREAK BY job-mat.blank-no DESCENDING:
                    IF LAST(job-mat.blank-no)            OR
                        job-mat.blank-no EQ po-ordl.b-num THEN LEAVE.
                END.

                IF AVAILABLE job-mat THEN 
                DO:
                    FIND FIRST ef NO-LOCK
                        WHERE ef.e-num   EQ job.e-num
                        AND ef.form-no EQ job-mat.frm
                        NO-ERROR.
   
                    ASSIGN
                        xg-flag = AVAILABLE ef AND (ef.xgrain EQ "S" OR ef.xgrain EQ "B")
                        i       = 0.
         
                    FOR EACH xjob-mat NO-LOCK
                        WHERE xjob-mat.company  EQ cocode
                        AND xjob-mat.job      EQ job-mat.job
                        AND xjob-mat.job-no   EQ job-mat.job-no
                        AND xjob-mat.job-no2  EQ job-mat.job-no2
                        AND xjob-mat.frm      EQ job-mat.frm
                        AND xjob-mat.blank-no EQ job-mat.blank-no
                        AND xjob-mat.i-no     NE job-mat.i-no,
              
                        FIRST xitem NO-LOCK
                        WHERE xitem.company  EQ cocode 
                        AND xitem.i-no     EQ xjob-mat.i-no
                        AND xitem.mat-type EQ "A"                        
            
                        /*,
                        
                        FIRST reftable
                        WHERE reftable.reftable EQ "util/b-hrms-x.w"
                          AND reftable.company  EQ xitem.company
                          AND reftable.code2    EQ xitem.i-no
                        NO-LOCK*/   :
              
                        ASSIGN
                            i          = i + 1
                            v-adder[i] = /*STRING(INT(reftable.code),"9999").*/
                        xitem.i-no. 
             
                        IF i GE 6 THEN LEAVE.
                    END.
                END.
            END.

            /* Order Download Specification */
    
            /* D1 */

            /* CUSTOMER # */
            PUT "20500" /*cust.cust-no*/                    FORMAT "x(5)".

            /* 01 */
            PUT "01"                                        FORMAT "x(2)".
    
            /* PURCHASE ORDER # */
            PUT po-ord.po-no                                FORMAT "999999".

            /* A */
            PUT "A"                                         FORMAT "x(1)".
    
            /* PURCHASE ORDER # */
            PUT po-ordl.line                                FORMAT "99".
    
            /* PURCHASE ORDER # */
            PUT po-ord.po-no                                FORMAT "999999".

            /* A */
            PUT "A"                                         FORMAT "x(1)".
    
            /* PURCHASE ORDER # */
            PUT po-ordl.line                                FORMAT "99".
    
            /* MESSAGE CODE #1 */
            PUT FILL(" ",2)                                 FORMAT "xx".
    
            /* MESSAGE CODE #2 */
            PUT FILL(" ",2)                                 FORMAT "xx".
    
            /* MESSAGE CODE #3 */
            PUT FILL(" ",2)                                 FORMAT "xx".
    
            /* 7 blank spaces */
            PUT FILL(" ",7)                                 FORMAT "x(7)".
    
            /* BY */
            PUT "BY"                                        FORMAT "x(5)".
    
            /* DUE DATE */
            PUT substr(STRING(YEAR(po-ordl.due-date),"9999"),3,2)  +
                string(MONTH(po-ordl.due-date),"99") +
                string(DAY(po-ordl.due-date),"99")          FORMAT "x(6)".
    
            /* OVERRUN PERCENTAGE */
            PUT po-ord.over-pct                             FORMAT "99".
    
            /* UNDERRUN PERCENTAGE */
            PUT po-ord.under-pct                            FORMAT "99".
        
            /* INTEGER OF WIDTH */
            PUT trunc(po-ordl.s-wid,0)                      FORMAT "999999".

            /* NUMERATOR OF WIDTH */
            PUT (po-ordl.s-wid - trunc(po-ordl.s-wid,0)) * 16
                FORMAT "99".
            /* DENOMINATOR OF WIDTH */
            PUT 16                                          FORMAT "99".
        
            /* WIDTH */
            PUT trunc(po-ordl.s-wid,0)                      FORMAT "999"
                ":"                                         FORMAT "x"
                (po-ordl.s-wid - trunc(po-ordl.s-wid,0)) * 16
                FORMAT "99".
        
            /* INTEGER OF LENGTH */
            PUT trunc(po-ordl.s-len,0)                      FORMAT "999999".

            /* NUMERATOR OF LENGTH */
            PUT (po-ordl.s-len - trunc(po-ordl.s-len,0)) * 16
                FORMAT "99".
            /* DENOMINATOR OF LENGTH */
            PUT 16                                          FORMAT "99".
        
            /* LENGTH */
            PUT trunc(po-ordl.s-len,0)                      FORMAT "999"
                ":"                                         FORMAT "x"
                (po-ordl.s-len - trunc(po-ordl.s-len,0)) * 16
                FORMAT "99".
    
            /* STYLE NUMBER */
            RUN po/po-ordls.p (RECID(po-ordl)).
    
            {po/po-ordls.i}
 
            li-style = IF AVAILABLE b-ref1 OR AVAILABLE b-ref2 THEN 1 ELSE 2.

            PUT li-style                                    FORMAT "9999".
    
            /* STYLE DESCRIPTION */
            PUT (IF li-style EQ 1 THEN "SCORED" ELSE "TRIMMED") + " SHEET"
                FORMAT "x(14)".
    
            /* WEIGHT OF BOARD */
            PUT item.basis-w                                FORMAT "9999".
        
            /* QUANTITY SHEETS */
            v-ord-qty[1] = po-ordl.ord-qty.
    
            IF po-ordl.pr-qty-uom NE "EA" THEN
                RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "EA",
                    item.basis-w, po-ordl.s-len,
                    po-ordl.s-wid, item.s-dep,
                    v-ord-qty[1], OUTPUT v-ord-qty[1]).
                           
            IF v-ord-qty[1] - trunc(v-ord-qty[1],0) GT 0 THEN
                v-ord-qty[1] = trunc(v-ord-qty[1],0) + 1.

            v-ord-qty[2] = v-ord-qty[1].

            IF v-ord-qty[1] GT 99999999 THEN v-ord-qty[1] = 99999999.
    
            PUT v-ord-qty[1]                                FORMAT "99999999".

            /* 13 blank spaces */
            PUT STRING(po-ordl.s-num,"99") FORM "99"   /* positiom 116 - 117 */
                FILL(" ",11)                                FORMAT "x(11)"  /* pos 118 - 128 */    SKIP.
    
            /*=== D2 ===*/
    
            /* FLUTE */
            PUT item.flute                                  FORMAT "x(3)".
    
            /* PRICE PER MSF */
            v-ord-cst = po-ordl.cost.
    
            IF po-ordl.pr-uom NE "MSF" THEN
                RUN sys/ref/convcuom.p(po-ordl.pr-uom, "MSF",
                    item.basis-w, po-ordl.s-len,
                    po-ordl.s-wid, item.s-dep,
                    v-ord-cst, OUTPUT v-ord-cst).

            IF v-ord-cst GT 9999.99 THEN v-ord-cst = 9999.99.
                           
            PUT v-ord-cst                                   FORMAT "9999.99".
        
            /* SETUP CHARGE */
            v-setup = 0.

            RELEASE e-item.
            RELEASE e-item-vend.

            FIND FIRST e-item NO-LOCK OF item  NO-ERROR.

            IF AVAILABLE e-item THEN
                FIND FIRST e-item-vend NO-LOCK OF e-item
                    WHERE e-item-vend.vend-no   EQ po-ord.vend-no
                    AND e-item-vend.item-type EQ YES
                    NO-ERROR.
    
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
                        tt-eiv.setups[i]  = e-item-vend.setups[i].
                END.
     
                      
                IF AVAILABLE e-item-vend THEN
                DO:
                    
                    DO i = 1 TO 10:
                        ASSIGN
                            tt-eiv.run-qty[i + 10] = e-item-vend.runQtyXtra[i]
                            tt-eiv.setups[i + 10]  = e-item-vend.setupsXtra[i].
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
    
            PUT v-setup                                     FORMAT "999.99".

            /* "001.00" */
            PUT 1                                           FORMAT "999.99".

            /* "00000000.0000" */
            PUT 0                                           FORMAT "99999999.9999".
        
            /* ORDERED SF */
            v-ord-qty[3] = po-ordl.ord-qty.
    
            IF po-ordl.pr-qty-uom NE "SF" THEN
                RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "SF",
                    item.basis-w, po-ordl.s-len,
                    po-ordl.s-wid, item.s-dep,
                    v-ord-qty[3], OUTPUT v-ord-qty[3]).
                           
            IF v-ord-qty[3] - trunc(v-ord-qty[3],0) GT 0 THEN
                v-ord-qty[3] = trunc(v-ord-qty[3],0) + 1.

            v-ord-qty[4] = v-ord-qty[3].

            IF v-ord-qty[3] GT 9999999 THEN v-ord-qty[3] = 9999999.
    
            PUT v-ord-qty[3]                                FORMAT "9999999".

            /* 46 blank spaces */
            PUT FILL(" ",46)                                FORMAT "x(46)".

            /* "X" */
            PUT "X"                                         FORMAT "x".

            /* "X" */
            PUT "X"                                         FORMAT "x".

            /* DESCRIPTION TEXT */
            PUT po-ordl.i-name                              FORMAT "x(30)".

            /* 8 blank spaces */
            PUT FILL(" ",8)                                 FORMAT "x(8)"       SKIP.
    
            /* D3 */
    
            PUT po-ordl.i-no  FORMAT "x(70)".  /* board/Grade code */
            PUT FILL(" ",58)                               FORMAT "x(58)"     SKIP.
    
            /* D4 */
    
            /* SCORE */
            DO i = 1 TO 12:
                IF AVAILABLE b-ref1 AND b-ref1.val[i] NE 0 THEN 
                    PUT trunc(b-ref1.val[i],0)                  FORMAT ">>>"
                        ":"                                     FORMAT "x"
                        (b-ref1.val[i] - trunc(b-ref1.val[i],0)) * 100
                        FORMAT "99"
                        substr(b-ref1.dscr,i,1)                 FORMAT "x".
            
                ELSE PUT "       "                            FORMAT "x(7)".
            END.
            DO i = 1 TO 12:
                IF AVAILABLE b-ref2 AND b-ref2.val[i] NE 0 THEN 
                    PUT trunc(b-ref2.val[i],0)                  FORMAT ">>>"
                        ":"                                     FORMAT "x"
                        (b-ref2.val[i] - trunc(b-ref2.val[i],0)) * 100
                        FORMAT "99"
                        substr(b-ref2.dscr,i,1)                 FORMAT "x".
            
                ELSE PUT "       "                            FORMAT "x(7)".
            END.
            /* NUMBER UP */
            PUT 1                                           FORMAT "999.99".

            /* TRIM */
            PUT FILL(" ",2)                                 FORMAT "xx".

            /* 1 OUT WIDTH, NO TRIM */
            PUT trunc(po-ordl.s-wid,0)                      FORMAT "999"
                ":"                                         FORMAT "x"
                (po-ordl.s-wid - trunc(po-ordl.s-wid,0)) * 16
                FORMAT "99".

            /* BLANK WIDTH (MULT OUT) */
            PUT trunc(po-ordl.s-wid,0)                      FORMAT "999"
                ":"                                         FORMAT "x"
                (po-ordl.s-wid - trunc(po-ordl.s-wid,0)) * 16
                FORMAT "99".

            /* (SF OR SM) PER M */
            v-ord-qty[2] = v-ord-qty[4] / (v-ord-qty[2] / 1000).

            IF v-ord-qty[2] GT 99999999 THEN v-ord-qty[2] = 99999999.

            PUT v-ord-qty[2]                                FORMAT "99999999".

            /* EXT'D (SF OR SM) ORDERED */
            IF v-ord-qty[4] GT 99999999 THEN v-ord-qty[4] = 99999999.

            PUT v-ord-qty[4]                                FORMAT "99999999".

            /* BASE BOARD GRADE CODE */
            /*   no more HRMS board code 05031108
            FIND FIRST reftable
                WHERE reftable.reftable EQ "util/b-hrms-x.w"
                  AND reftable.company  EQ po-ordl.company
                  AND reftable.code2    EQ po-ordl.i-no
                NO-LOCK NO-ERROR.
            if avail reftable then
              put int(reftable.code)                        format "9999".
            else */
            PUT 0                                         FORMAT "9999".
    
            /* ADDERS */ 
            DO i = 1 TO 6:
                PUT v-adder[i]                                FORMAT "x(4)".
            END.

            /* 1 blank space */
            PUT FILL(" ",1)                                 FORMAT "x(1)"       SKIP.
    
            /*D5 adder new line */
            /* ADDERS */
            DO i = 1 TO 6:
                PUT v-adder[i]                                FORMAT "x(10)".
            END.
            PUT v-mch-cod                                   FORMAT "x(6)".
            PUT SKIP.

            /* D6 */

            /* SPECIAL INSTRUCTIONS */
            v-instr = "".

            FOR EACH notes NO-LOCK WHERE notes.rec_key EQ po-ordl.rec_key :
                v-instr = v-instr + " " + trim(notes.note_text).
            END.

            FOR EACH notes NO-LOCK WHERE notes.rec_key EQ po-ord.rec_key :
                v-instr = v-instr + " " + trim(notes.note_text).
            END.

            DO i = 1 TO 64:
                IF ASC(SUBSTRING(v-instr,i,1)) = 10 OR
                    asc(SUBSTRING(v-instr,i,1)) = 13
                    THEN SUBSTRING(v-instr,i,1) = "".
            END.
            PUT v-instr                                     FORMAT "x(64)" .
        

            /* Job# */
            IF po-ordl.job-no NE "" THEN
                PUT STRING(po-ordl.job-no,"x(6)") + "-" +
                    string(po-ordl.job-no2,"99")              FORMAT "x(9)".
            ELSE
                PUT FILL(" ",9)                               FORMAT "x(9)".

            /* 64 blank spaces */
            PUT FILL(" ",55)                                FORMAT "x(55)"       SKIP
                FILL(" ",128) FORM "x(128)"  SKIP
                FILL(" ",128) FORM "x(128)"  SKIP
                FILL(" ",128) FORM "x(128)"  SKIP
                FILL(" ",128) FORM "x(128)"  SKIP
                .
        END. /* for each po-ordl record */

        po-ord.printed = YES.
  
        IF SEARCH(v-outfile[2]) NE ? THEN 
        DO:
            OUTPUT close.
    
            ASSIGN 
                cQuoterCommandString = cQuoterFullPathName + " -c 1-3000 " + v-outfile[2] + " > " + v-outFile[2] + ".quo".
            OS-COMMAND SILENT VALUE(cQuoterCommandString).
                                   
            INPUT from value(v-outfile[2] + ".quo").
    
            OUTPUT to value(v-outfile[4]).
    
            REPEAT:
                v-line = FILL(" ",228).
                IMPORT v-line.
                IF LENGTH(v-line) LT 228 THEN 
                    PUT v-line FORMAT "x(228)" SKIP.
                ELSE
                    PUT UNFORMATTED v-line SKIP.
            END.
    
            OUTPUT close.
    
            INPUT close.
    
            IF OPSYS EQ "unix" THEN
                UNIX SILENT rm VALUE(v-outfile[2] + "*.*").
            ELSE
                DOS SILENT DEL VALUE(v-outfile[2] + "*.*").
      
            RUN po/ftppo.p (v-outfile[4],"iPaper"). 
            MESSAGE "iPaper file:" TRIM(v-outfile[3]) "has been created" 
                VIEW-AS ALERT-BOX.
        END.

        PAUSE 1 NO-MESSAGE.
    END. /* for each po-ord record */

/* end ----------------------------------- Copr. 2004  Advanced Software Inc. */
