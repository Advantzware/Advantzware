/* -------------------------------------------------- po/po-hrexp.p 02/02 JLF */
/*                                                                            */
/* Liberty for Sheets export PO                                                    */
/*                                                                            */
/* -------------------------------------------------------------------------- */

DEFINE INPUT PARAMETER v-format AS CHARACTER NO-UNDO.

{sys/inc/var.i shared}
{sys/form/s-top.f}

DEFINE BUFFER xjob-mat FOR job-mat.
DEFINE BUFFER xitem    FOR item.
DEFINE BUFFER b-ref1   FOR reftable.
DEFINE BUFFER b-ref2   FOR reftable.

{po/po-print.i}
{po/getPoAdders.i}

DEFINE VARIABLE v-sname         LIKE shipto.ship-name.
DEFINE VARIABLE v-saddr         LIKE shipto.ship-addr.
DEFINE VARIABLE v-scity         LIKE shipto.ship-city.
DEFINE VARIABLE v-sstate        LIKE shipto.ship-state.
DEFINE VARIABLE v-szip          LIKE shipto.ship-zip.
DEFINE VARIABLE v-adder         LIKE item.i-no EXTENT 7 NO-UNDO.
DEFINE VARIABLE xg-flag         AS LOG       INIT NO NO-UNDO.
DEFINE VARIABLE v-instr         AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-ord-qty       LIKE po-ordl.ord-qty EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-ord-cst       LIKE po-ordl.cost NO-UNDO.
DEFINE VARIABLE v-unit-cost     LIKE po-ordl.cost NO-UNDO.
DEFINE VARIABLE v-setup         LIKE e-item-vend.setup NO-UNDO.
DEFINE VARIABLE v-outfile       AS CHARACTER EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-mach          AS CHARACTER EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-line          AS CHARACTER NO-UNDO.
DEFINE VARIABLE li-style        AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-freight-dscr  AS CHARACTER NO-UNDO.
DEFINE VARIABLE k_frac          AS DECIMAL   INIT 6.25 NO-UNDO.
DEFINE VARIABLE v-cecscrn-char  AS CHARACTER NO-UNDO. /* make sure doesn't equal 'Decimal' */
DEFINE VARIABLE cOutLine        AS CHARACTER NO-UNDO.
DEFINE VARIABLE iNumericAdder   AS INTEGER   NO-UNDO.
DEFINE VARIABLE cAssignedCustId AS CHARACTER NO-UNDO.
DEFINE VARIABLE iAdderCount     AS INTEGER NO-UNDO.
DEFINE VARIABLE iSequence       AS INTEGER NO-UNDO.
DEFINE VARIABLE iTempAdder AS INTEGER NO-UNDO.
DEFINE VARIABLE v-mch-cod AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDimensions     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFormattedScore AS CHARACTER NO-UNDO.
DEFINE VARIABLE len-score AS CHARACTER.
DEFINE VARIABLE v-test-scr AS LOG.
DEFINE VARIABLE v-space    AS LOG. 
DEFINE VARIABLE cSenderID AS CHARACTER INIT "1244037".
DEFINE BUFFER b-qty   FOR reftable.
DEFINE BUFFER b-setup FOR reftable.
DEFINE STREAM sEDIPOH.
DEFINE STREAM sEDIPOD.
DEFINE STREAM sEDIPOITEM.

DEFINE TEMP-TABLE tt-eiv NO-UNDO
    FIELD run-qty AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD setups  AS DECIMAL DECIMALS 2 EXTENT 20.

DEF VAR liberty-dir LIKE sys-ctrl.descrip NO-UNDO.
DEF VAR liberty-log LIKE sys-ctrl.log-fld NO-UNDO.


{sys/inc/poexport.i}

RELEASE sys-ctrl.

find first sys-ctrl
    where sys-ctrl.company eq cocode
    and sys-ctrl.name    eq "Liberty"
    no-lock no-error.
if not avail sys-ctrl then 
do:
    create sys-ctrl.
    assign
        sys-ctrl.company = cocode
        sys-ctrl.name    = "Liberty".
end.
assign
    liberty-dir = sys-ctrl.descrip
    liberty-log = sys-ctrl.log-fld.
RELEASE sys-ctrl.
/* ************************  Function Prototypes ********************** */

FUNCTION fFormScore RETURNS CHARACTER 
	(INPUT ipdDim AS DECIMAL ) FORWARD.

FUNCTION fInsText RETURNS LOGICAL 
    (INPUT ipiStartPos AS INTEGER,
    INPUT ipiLen AS INTEGER, INPUT ipcText AS CHARACTER) FORWARD.

FUNCTION fnRJust RETURNS CHARACTER 
    (INPUT ipcText AS CHARACTER, INPUT ipiLen AS INTEGER) FORWARD.

FUNCTION fTime RETURNS CHARACTER 
    (INPUT ipiTime AS INTEGER  ) FORWARD.
    
FIND FIRST po-ctrl WHERE po-ctrl.company EQ cocode NO-LOCK.

FIND FIRST company WHERE company.company EQ cocode NO-LOCK.
FIND FIRST EDPOTran EXCLUSIVE-LOCK WHERE EDPOTran.Partner EQ "Liberty" NO-ERROR.
IF NOT AVAILABLE EDPOTran THEN DO:
    CREATE EDPOTran.
    ASSIGN EDPOTran.Seq = 1
           EDPOTran.Partner = "Liberty".
END.

/* Using this to get next unique number */
EDPOTran.Last-line = last-line + 1.
iSequence = EDPOTran.Last-line.
RELEASE EDPOTran.

FIND FIRST cust
    WHERE cust.company EQ cocode
    AND cust.active  EQ "X"
    NO-LOCK NO-ERROR.
cAssignedCustId = "00105". 

IF AVAILABLE cust AND liberty-log AND liberty-dir NE "" THEN
    print-po-blok:
    FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,

        FIRST po-ord
        WHERE RECID(po-ord) EQ report.rec-id
          AND CAN-FIND(FIRST po-ordl
        WHERE po-ordl.company   EQ po-ord.company
          AND po-ordl.po-no     EQ po-ord.po-no
          AND po-ordl.item-type EQ YES
          AND (v-printde-po OR NOT po-ordl.deleted)),

        FIRST vend
           WHERE vend.company EQ po-ord.company
             AND vend.vend-no EQ po-ord.vend-no
             AND (vend.po-export EQ "Liberty" OR
               (poexport-cha  EQ "Liberty" AND vend.an-edi-vend))
        NO-LOCK
        BY po-ord.po-no.

        iSequence = 1.
          
        /* Cross-reference vendor to vendors customer number */  
        FIND FIRST sys-ctrl-shipto NO-LOCK  
            WHERE sys-ctrl-shipto.company EQ po-ord.company
              AND sys-ctrl-shipto.cust-vend = FALSE 
              AND sys-ctrl-shipto.cust-vend-no EQ po-ord.vend-no
            NO-ERROR.
        IF AVAILABLE sys-ctrl-shipto THEN 
            cAssignedCustId = sys-ctrl-shipto.char-fld.

        IF OPSYS EQ "UNIX" AND substr(liberty-dir,1,1) NE v-slash THEN
            liberty-dir = v-slash + liberty-dir.

        IF substr(liberty-dir,LENGTH(liberty-dir),1) EQ v-slash THEN
            substr(liberty-dir,LENGTH(liberty-dir),1) = "".
    
        ASSIGN
            v-outfile[1] = TRIM(liberty-dir) + v-slash + "dataxfer" +
                  v-slash + "in" + v-slash
            v-outfile[2] = "EDIPOH" + cSenderID /* Edi Sender ID */ + USERID("asi") + "_"
            v-outfile[3] = 
                  substr(STRING(YEAR(TODAY),"9999"),3,2) +
                  string(MONTH(TODAY),"99") +
                  string(DAY(TODAY),"99") + "_" +
                  substr(STRING(TIME,"HH:MM:SS"),1,2) +
                  substr(STRING(TIME,"HH:MM:SS"),4,2) +
                  substr(STRING(TIME,"HH:MM:SS"),7,2) + "_" +
                  cSenderID + ".DAT"
            v-outfile[4] = v-outfile[1] + v-outfile[2] + v-outfile[3].
   
       
        OUTPUT STREAM sEDIPOH to value(v-outfile[4]).
        
        v-outfile[2] = "EDIPOD" + cSenderID /* Edi Sender ID */ + USERID("asi").
        v-outfile[4] = v-outfile[1] + v-outfile[2] + v-outfile[3].
        OUTPUT STREAM sEDIPOD to value(v-outfile[4]).
        
        v-outfile[2] = "EDIITEM" + cSenderID /* Edi Sender ID */ + USERID("asi").
        v-outfile[4] = v-outfile[1] + v-outfile[2] + v-outfile[3].
        OUTPUT STREAM sEDIPOITEM to value(v-outfile[4]).
        
        IF po-ord.stat EQ "N" THEN po-ord.stat = "O".
        DEFINE VARIABLE v-sphone AS CHARACTER.
        ASSIGN
            v-sname    = cust.name
            v-saddr[1] = cust.addr[1]
            v-saddr[2] = cust.addr[2]
            v-scity    = cust.city
            v-sstate   = cust.state
            v-szip     = cust.zip
            v-sphone   = cust.phone
            .
 
        IF po-ord.type EQ "D" THEN
            ASSIGN
                v-sname    = po-ord.ship-name
                v-saddr[1] = po-ord.ship-addr[1]
                v-saddr[2] = po-ord.ship-addr[2]
                v-scity    = po-ord.ship-city
                v-sstate   = po-ord.ship-state
                v-szip     = po-ord.ship-zip
                v-sphone   = ""
                .
        IF po-ord.frt-pay EQ "P" 
            THEN ASSIGN v-freight-dscr = "Prepaid".
        ELSE 
            IF po-ord.frt-pay EQ "C" 
                THEN ASSIGN v-freight-dscr = "Collect".
            ELSE ASSIGN v-freight-dscr = "Bill".                
            
        cOutLine = "".        
        fInsText(    1,    8, "<EDIPOH>"       ).
        fInsText(   10,   10, STRING(TODAY, "99/99/9999")  ).
        fInsText(   21,   35, cust.addr[1]   ).
        fInsText(   57,   35, cust.addr[2]   ).
        fInsText(   93,   30, ""             ). /* not used */
        fInsText(  124,   19, TRIM(cust.city)).
        fInsText(  144,   17, SUBSTRING(cust.name, 1, 17) ).
        fInsText(  162,   35, cust.name       ).
        fInsText(  198,    2, ""). /* not used */
        fInsText(  201,    3, TRIM(cust.state)).
        fInsText(  205,    9, cust.zip        ).
        fInsText(  215,   35, po-ord.contact). /* buyer name */
        fInsText(  251,   25, cust.area-code + "-" + cust.phone      ).
        fInsText(  277,    1, "N"             ). /* Is this a Change PO? */
        fInsText(  279,    1, ""              ). /* not used */
        fInsText(  281,    1, "S"              ). /* not used */
        fInsText(  283,   15, ""              ). /* not used */
        fInsText(  299,   15, cSenderID           ). /* assigned ID */
        fInsText(  315,    2, ""              ). /* not used */
        fInsText(  318,    7, fnRJust(STRING(iSequence), 7)). /* sequential number */
        fInsText(  326,   10, STRING(po-ord.due-date, "99/99/9999")). /* due date */
        fInsText(  337,   10, STRING(TODAY, "99/99/9999")   ).
        fInsText(  348,   10, fTime(TIME)     ).
        fInsText(  359,    1, ""              ). /* not used */
        fInsText(  361,   25, "NW"            ). /* po status */
        fInsText(  387,   25, "N"             ). /* po type */
        fInsText(  413,    6, "AMC"           ). /* sheet plant abbreviation */
        fInsText(  420,   22, STRING(po-ord.po-no, "999999")).
        fInsText(  443,   10, STRING(po-ord.po-date, "99/99/9999")).
        fInsText(  454,    1, "T"             ). /* process stat */
        fInsText(  456,   10, STRING(po-ord.due-date, "99/99/9999")).
        fInsText(  467,   30, ""              ) . /* not used */
        fInsText(  498,   35, ""              ). /* not used */
        fInsText(  534,   10, STRING(po-ord.due-date, "99/99/9999")).
        fInsText(  545,    2, ""              ). /* freight terms - can be blank */
        fInsText(  548,   35, v-saddr[1]      ).
        fInsText(  584,   35, v-saddr[2]      ).
        fInsText(  620,   30, po-ord.contact  ). /* ship to contact */
        fInsText(  651,   19, v-scity         ).
        fInsText(  671,   17, ""              ). /* not used */
        fInsText(  689,   35, cust.name       ). /* ship to company name */
        fInsText(  725,    2, ""              ). /* not used */
        fInsText(  728,    3, v-sstate        ). /* ship to state */
        fInsText(  732,    9, v-szip          ). /* ship to zip */
        fInsText(  742,   15, cSenderID       ). /* cust sender id if consolidated */
        fInsText(  758,    2, ""              ). /* not used */
        fInsText(  761,    1, "P"             ). /* test or prod */
        fInsText(  763,    1, ""              ). /* not used */
        fInsText(  765,   20, ""              ). /* not used */
        fInsText(  786,   30, po-ord.contact  ). /* user */
        fInsText(  817,   10, fnRJust(STRING(po-ord.t-cost), 10)).
        fInsText(  828,   10, STRING(po-ord.po-date, "99/99/9999")).
        cOutLine = TRIM(cOutLine, ",").
        PUT STREAM sEDIPOH UNFORMATTED cOutLine SKIP.

        FIND FIRST carrier
            WHERE carrier.company EQ po-ord.company
              AND carrier.carrier EQ po-ord.carrier
            NO-LOCK NO-ERROR.

        iSequence = 1.

  
        FOR EACH po-ordl
            WHERE po-ordl.company   EQ po-ord.company
              AND po-ordl.po-no     EQ po-ord.po-no
              AND po-ordl.item-type EQ YES
              AND (v-printde-po OR NOT po-ordl.deleted),
      
            FIRST item
            WHERE item.company  EQ cocode
              AND item.i-no     EQ po-ordl.i-no
              AND item.mat-type EQ "B"
            NO-LOCK
      
            BY po-ordl.line:
      
            ASSIGN
                xg-flag = NO
                v-adder = "".
    
            FIND FIRST job
                WHERE job.company EQ cocode
                  AND job.job-no  EQ fill(" ",6 - length(TRIM(po-ordl.job-no))) +
                      trim(po-ordl.job-no)
                  AND job.job-no2 EQ po-ordl.job-no2
                NO-LOCK NO-ERROR.
        
            IF AVAILABLE job THEN 
            DO:
                FIND FIRST est
                    WHERE est.company EQ job.company
                      AND est.est-no  EQ job.est-no
                    NO-LOCK NO-ERROR.
      
                FOR EACH job-mat
                    WHERE job-mat.company  EQ cocode
                      AND job-mat.job      EQ job.job
                      AND job-mat.job-no   EQ job.job-no
                      AND job-mat.job-no2  EQ job.job-no2
                      AND job-mat.i-no     EQ po-ordl.i-no
                      AND job-mat.frm      EQ po-ordl.s-num
                    USE-INDEX job NO-LOCK
                    BREAK BY job-mat.blank-no DESCENDING:
                    IF LAST(job-mat.blank-no)            OR
                        job-mat.blank-no EQ po-ordl.b-num THEN LEAVE.
                END.

                IF AVAILABLE job-mat THEN 
                DO:
                    FIND FIRST ef
                        WHERE ef.e-num   EQ job.e-num
                          AND ef.form-no EQ job-mat.frm
                        NO-LOCK NO-ERROR.
   
                    ASSIGN
                        xg-flag = AVAILABLE ef AND (ef.xgrain EQ "S" OR ef.xgrain EQ "B")
                        i       = 0.
         
                    FOR EACH xjob-mat
                        WHERE xjob-mat.company  EQ cocode
                          AND xjob-mat.job      EQ job-mat.job
                          AND xjob-mat.job-no   EQ job-mat.job-no
                          AND xjob-mat.job-no2  EQ job-mat.job-no2
                          AND xjob-mat.frm      EQ job-mat.frm
                          AND xjob-mat.blank-no EQ job-mat.blank-no
                          AND xjob-mat.i-no     NE job-mat.i-no
                        NO-LOCK,
              
                        FIRST xitem
                        WHERE xitem.company  EQ cocode 
                          AND xitem.i-no     EQ xjob-mat.i-no
                          AND xitem.mat-type EQ "A"
                        NO-LOCK,
            
                        FIRST reftable
                        WHERE reftable.reftable EQ "util/b-hrms-x.w"
                          AND reftable.company  EQ xitem.company
                          AND reftable.code2    EQ xitem.i-no
                        NO-LOCK:
                        
                        iTempAdder = INT(reftable.code) NO-ERROR.
                        IF NOT ERROR-STATUS:ERROR THEN 
                            v-adder[i] = STRING(INT(reftable.code),"9999").

                        ASSIGN
                            i = i + 1
                            .
             
                        IF i GE 6 THEN LEAVE.
                    END.
                END.
            END.

            RUN po/po-ordls.p (RECID(po-ordl)).
    
            {po/po-ordls.i}


            IF AVAIL b-ref1 OR AVAIL b-ref2 THEN 
            DO:
                ASSIGN
                    lv-val = 0
                    lv-typ = "".

                IF AVAIL b-ref1 THEN
                DO x = 1 TO 12:
                    ASSIGN
                        lv-val[x] = b-ref1.val[x]
                        lv-typ[x] = SUBSTR(b-ref1.dscr,x,1).
                END.

                IF AVAIL b-ref2 THEN
                DO x = 1 TO 8:
                    ASSIGN
                        lv-val[x + 12] = b-ref2.val[x]
                        lv-typ[x + 12] = SUBSTR(b-ref2.dscr,x,1).
                END.

                DO lv-int = 0 TO 1:
                    ASSIGN
                        v-lscore-c = ""
                        len-score  = "".

                    DO x = 1 TO 10:
                        IF lv-val[(lv-int * 10) + x] NE 0 THEN
                            v-lscore-c = v-lscore-c + TRIM(IF lv-val[(lv-int * 10) + x] GT 9999 THEN
                                STRING(lv-val[(lv-int * 10) + x],">>>>>")
                                ELSE
                                IF lv-val[(lv-int * 10) + x] GT 999 THEN
                                STRING(lv-val[(lv-int * 10) + x],">>>>")
                                ELSE 
                                STRING(lv-val[(lv-int * 10) + x],">>>.99")).

                        /* print score type for Premier */
                        IF v-score-types AND lv-typ[(lv-int * 10) + x] NE "" THEN 
                            v-lscore-c = v-lscore-c + lv-typ[(lv-int * 10) + x] + " ".     
                        ELSE v-lscore-c = v-lscore-c + " ".
                    END.
 
                    IF v-lscore-c NE "" THEN 
                    DO:
                        v-space = NO.

                        DO x = 1 TO LENGTH(v-lscore-c):
                            IF SUBSTR(v-lscore-c,x,1) NE " " THEN
                                ASSIGN
                                    len-score = len-score + SUBSTR(v-lscore-c,x,1)
                                    v-space   = YES.
         
                            ELSE
                                IF v-space THEN
                                    ASSIGN
                                        len-score = len-score + "  "
                                        v-space   = NO.
                        END.

                        v-test-scr = YES.
               
                        DO x = 1 TO LENGTH(TRIM(len-score)):
                            IF SUBSTR(TRIM(len-score),x,1) EQ " " THEN v-test-scr = NO.
                        END.
                    END.
                END.
            END.

            li-style = IF AVAILABLE b-ref1 OR AVAILABLE b-ref2 THEN 1 ELSE 2.

            /* PUT li-style                                    FORMAT "9999". */
    
            /* STYLE DESCRIPTION */
/*            PUT (IF li-style EQ 1 THEN "SCORED" ELSE "TRIMMED") + " SHEET"*/
/*                FORMAT "x(14)".                                           */

            IF po-ordl.pr-qty-uom NE "EA" THEN
                RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "EA",
                    item.basis-w, po-ordl.s-len,
                    po-ordl.s-wid, item.s-dep,
                    v-ord-qty[1], OUTPUT v-ord-qty[1]).
                           
            IF v-ord-qty[1] - trunc(v-ord-qty[1],0) GT 0 THEN
                v-ord-qty[1] = trunc(v-ord-qty[1],0) + 1.

            v-ord-qty[2] = v-ord-qty[1].

            IF v-ord-qty[1] GT 99999999 THEN v-ord-qty[1] = 99999999.
      
            /* PRICE PER MSF */
            v-ord-cst = po-ordl.cost.


              
            IF AVAILABLE b-ref1 THEN DO:
                IF b-ref1.val[3] GT 0 THEN 
                    cDimensions = TRIM(STRING(b-ref1.val[1], ">>>>.99")) + " x " + TRIM(STRING(b-ref1.val[2], ">>>>.99")) + " x " + TRIM(STRING(b-ref1.val[3], ">>>>.99")).
                ELSE
                    cDimensions = TRIM(STRING(b-ref1.val[2], ">>>>.99")).       
                             
                IF b-ref1.val[2] GT 0 THEN 
                DO:
                    cFormattedScore = fFormScore(b-ref1.val[1]) + fFormScore(b-ref1.val[2]).
                    IF b-ref1.val[3] GT 0 THEN 
                        cFormattedScore =  cFormattedScore + fFormScore(b-ref1.val[3]).
                END.
                ELSE
                    cFormattedScore =  fFormScore(b-ref1.val[1]).                
            END.
            ELSE DO:
                /* Formatted Dimensions */
                IF ITEM.s-dep GT 0 THEN 
                    cDimensions = TRIM(STRING(ITEM.s-len, ">>>>.99")) + " x " + TRIM(STRING(ITEM.s-wid, ">>>>.99")) + " x " + TRIM(STRING(ITEM.s-dep, ">>>>.99")).
                ELSE
                    cDimensions = TRIM(STRING(ITEM.s-wid, ">>>>.99")).
                IF ITEM.s-dep GT 0 THEN DO:
                    cFormattedScore = fFormScore(ITEM.s-len) + fFormScore(ITEM.s-wid).
                    IF ITEM.s-dep GT 0 THEN 
                        cFormattedScore =  cFormattedScore + fFormScore(ITEM.s-dep).
                END.
                ELSE
                        cFormattedScore =  fFormScore(ITEM.s-wid).
            END.
            IF po-ordl.pr-uom NE "MSF" THEN
                RUN sys/ref/convcuom.p(po-ordl.pr-uom, "MSF",
                    item.basis-w, po-ordl.s-len,
                    po-ordl.s-wid, item.s-dep,
                    v-ord-cst, OUTPUT v-ord-cst).

            IF v-ord-cst GT 9999.99 THEN v-ord-cst = 9999.99.
      
            v-unit-cost = po-ordl.cost.

            IF po-ordl.pr-uom NE "EA" THEN
                RUN sys/ref/convcuom.p(po-ordl.pr-uom, "EA",
                    item.basis-w, po-ordl.s-len,
                    po-ordl.s-wid, item.s-dep,
                    v-unit-cost, OUTPUT v-unit-cost).
              
            /* SETUP CHARGE */
            v-setup = 0.

            RELEASE e-item.
            RELEASE e-item-vend.

            FIND FIRST e-item OF item NO-LOCK NO-ERROR.

            IF AVAILABLE e-item THEN
                FIND FIRST e-item-vend OF e-item
                    WHERE e-item-vend.vend-no   EQ po-ord.vend-no
                    AND e-item-vend.item-type EQ YES
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
                        tt-eiv.setups[i]  = e-item-vend.setups[i].
                END.

                FIND FIRST b-qty WHERE
                    b-qty.reftable = "vend-qty" AND
                    b-qty.company = e-item-vend.company AND
                    b-qty.CODE    = e-item-vend.i-no AND
                    b-qty.code2   = e-item-vend.vend-no
                    NO-LOCK NO-ERROR.
      
                IF AVAILABLE b-qty THEN
                DO:
                    FIND FIRST b-setup WHERE
                        b-setup.reftable = "vend-setup" AND
                        b-setup.company = e-item-vend.company AND
                        b-setup.CODE    = e-item-vend.i-no AND
                        b-setup.code2   = e-item-vend.vend-no
                        NO-LOCK NO-ERROR.
      
                    DO i = 1 TO 10:
                        ASSIGN
                            tt-eiv.run-qty[i + 10] = b-qty.val[i]
                            tt-eiv.setups[i + 10]  = b-setup.val[i].
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

            /* ORDERED SF */
            v-ord-qty[3] = po-ordl.ord-qty.
            v-ord-qty[1] = po-ordl.ord-qty.
    
            IF po-ordl.pr-qty-uom NE "SF" THEN
                RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "SF",
                    item.basis-w, po-ordl.s-len,
                    po-ordl.s-wid, item.s-dep,
                    v-ord-qty[3], OUTPUT v-ord-qty[3]).
                           
            IF v-ord-qty[3] - trunc(v-ord-qty[3],0) GT 0 THEN
                v-ord-qty[3] = trunc(v-ord-qty[3],0) + 1.

            v-ord-qty[4] = v-ord-qty[3].

            IF v-ord-qty[3] GT 9999999 THEN v-ord-qty[3] = 9999999.

            /* SCORE */
/*            DO i = 1 TO 9:                                                  */
/*                IF AVAILABLE b-ref1 AND b-ref1.val[i] NE 0 THEN             */
/*                    PUT trunc(b-ref1.val[i],0)                  FORMAT ">>>"*/
/*                        ":"                                     FORMAT "x"  */
/*                        (b-ref1.val[i] - trunc(b-ref1.val[i],0)) * 100      */
/*                        FORMAT "99"                                         */
/*                        substr(b-ref1.dscr,i,1)                 FORMAT "x". */
/*                                                                            */
/*                ELSE PUT "       "                            FORMAT "x(7)".*/
/*            END.                                                            */
    
            /* SPECIAL INSTRUCTIONS */
            v-instr = "".

            FOR EACH notes WHERE notes.rec_key EQ po-ordl.rec_key NO-LOCK:
                v-instr = v-instr + " " + trim(notes.note_text).
            END.

            FOR EACH notes WHERE notes.rec_key EQ po-ord.rec_key NO-LOCK:
                v-instr = v-instr + " " + trim(notes.note_text).
            END.
            iAdderCount  = 1.
            iNumericAdder = INTEGER(po-ordl.i-no) NO-ERROR.
            IF NOT ERROR-STATUS:ERROR THEN 
                ASSIGN v-adder[iAdderCount] = STRING(iNumericAdder, "9999")
                    iAdderCount          = iAdderCount + 1
                    .

            /* Get adder codes */
            EMPTY TEMP-TABLE ttPoAdders.
            RUN po/getPoAdders.p (INPUT ROWID(po-ordl), INPUT table ttPoAdders BY-REFERENCE).
            FOR EACH ttPoAdders i = 1 TO 6:
                iNumericAdder = INTEGER(ttPoAdders.adderCode) NO-ERROR.
                IF NOT ERROR-STATUS:ERROR THEN 
                    ASSIGN v-adder[iAdderCount] = STRING(iNumericAdder, "999")
                        iAdderCount          = iAdderCount + 1
                        .
            END.
            v-mch-cod = "" .
            FOR EACH job-mch WHERE job-mch.company EQ cocode
                AND job-mch.job-no EQ po-ordl.job-no
                AND job-mch.job-no2 EQ po-ordl.job-no2
                AND job-mch.frm EQ po-ordl.s-num use-index line-idx NO-LOCK:

                ASSIGN 
                    v-mch-cod = job-mch.m-code .
                LEAVE.
            END.
            
            cOutLine = "".
            /* Order Detail */
            fInsText(    1,    8, "<EDIPOD>"   ).
            fInsText(   10,   35, v-saddr[1] ).
            fInsText(   46,   35, v-saddr[2] ).
            fInsText(   82,   19, v-scity    ).
            fInsText(  102,   17, "American Carton &"). /* Bill-to code */
            fInsText(  120,   35, v-sname    ).
            fInsText(  156,    2, ""   ). /* Bill-to qualifier - not used*/
            fInsText(  159,    3, v-sstate       ). 
            fInsText(  163,    9, v-szip     ).
            fInsText(  173,   35, po-ord.buyer ). /* Buyer Name */
            fInsText(  209,   25, "999-999-9999" ). /* Buyer Phone */
            fInsText(  235,   11, ""         ). /* not used */
            fInsText(  247,   11, ""         ). /* not used */
            fInsText(  259,   11, ""         ). /* not used */
            fInsText(  271,   50, ""         ). /* not used */
            fInsText(  322,    1, ""         ). /* not used */
            fInsText(  324,   15, ""         ). /* not used */
            fInsText(  340,   30, po-ordl.i-no ). /* base board grade */
            fInsText(  371,   15, cSenderID  ). /* sheet plant's cusotmer no. in corrugators's database */
            fInsText(  387,    2, ""         ). /* EDI Interchange qualifier */
            fInsText(  390,    7, fnRJust(STRING(iSequence), 7)). /* Unique Sequential number */
            fInsText(  398,   11, STRING(po-ordl.line)). 
            fInsText(  410,    9, " "        ) . /* not used */
            fInsText(  420,    9, ""         ). /* not used */
            fInsText(  430,    9, ""         ). /* not used */
            fInsText(  440,    9, ""         ). /* not used */
            fInsText(  450,    2, ""         ). /* not used */
            fInsText(  453,    2, ""         ). /* not used */
            fInsText(  456,    2, ""         ). /* not used */
            fInsText(  459,    2, ""         ). /* not used */
            fInsText(  462,   60, ""         ). /* not used */
            fInsText(  523,    1, ""         ). /* not used */
            fInsText(  525,   11, fnRJust(STRING(v-ord-qty[1]), 11)).
            fInsText(  537,    9, STRING(po-ordl.ord-no) ). /* Order # for associated sales ord */
            fInsText(  547,    6, "AMC"         ). /* sheet plant abbreviation */
            fInsText(  554,   22,  STRING(po-ordl.po-no, "999999")).
            fInsText(  577,   30, ""         ). /* not used */
            fInsText(  608,    2, "EA"       ). /* Price UOM */
            fInsText(  611,   30, ""         ). /* not used */
            fInsText(  642,   35, v-saddr[1] ).
            fInsText(  678,   35, v-saddr[2] ).
            fInsText(  714,   19, v-scity    ).
            fInsText(  734,   17, "American Carton &"    ).
            fInsText(  752,   35, v-sname    ).
            fInsText(  788,    2, ""         ). /* not used */
            fInsText(  791,    3, v-sstate   ).
            fInsText(  795,    9, v-szip     ).
            fInsText(  805,    1, ""         ). /* not used */
            fInsText(  807,   20, ""         ). /* not used */
            fInsText(  828,    2, "EA"       ).
            fInsText(  831,   12, ""         ). /* not used */
            fInsText(  844,   13, fnRJust(STRING(po-ord.t-cost), 13)).
            fInsText(  858,   30, ""         ). /* not used */
            fInsText(  889,   10, string(po-ord.due-date, "99/99/9999")). /* not used */
            cOutLine = TRIM(cOutLine, ",").
            PUT STREAM sEDIPOD UNFORMATTED cOutLine SKIP.
                
            cOutLine = "".
            /* Order Download Specification */
            fInsText( 1, 11,"<EDIPOITEM>").
            fInsText( 13, 8,STRING({sys/inc/k16.i po-ordl.s-wid}, ">>>>.9999")). 
            fInsText( 22, 8,STRING({sys/inc/k16.i po-ordl.s-len}, ">>>>.9999")).
            fInsText( 31, 12, po-ordl.i-no  ). /* base board grade */
            fInsText( 44, 1,  "R"           ).  /* adhesive code */
            fInsText( 46, 3,  "1"           ).  /* plant number */
            fInsText( 50, 254, cDimensions  ). /* mil score */
            fInsText( 305, 3, v-adder[1]    ). /* first board adder */
            fInsText( 309, 3, v-adder[2]    ). /* 2nd board adder */
            fInsText( 313, 3, v-adder[3]    ). /* 3rd board adder */
            fInsText( 318, 3, v-adder[4]    ). /* 4th board adder */
            fInsText( 321, 3, v-adder[5]    ). /* 5th board adder */
            fInsText( 325, 3, v-adder[6]    ). /* 6th board adder */
            fInsText( 329, 3, v-adder[7]    ). /* 7th board adder */
            fInsText( 333, 3, ""            ). /* 8th board adder */    
            fInsText( 336, 22, STRING(po-ord.po-no, "999999") ). /* po # */
            fInsText( 360, 11, STRING(po-ordl.line) ). /* po line # */
            fInsText( 372, 10, "0"        ). /* combo msf 3 decimals */
            fInsText( 383, 11, STRING(po-ordl.ord-qty - (po-ord.under-pct * po-ordl.ord-qty / 100)    )). /* PO min qty */
            fInsText( 395, 11, STRING(po-ordl.ord-qty + (po-ord.over-pct * po-ordl.ord-qty / 100)   )). /* po max qty */
            fInsText( 407, 15, v-mch-cod    ). /* first machine from routing */
            fInsText( 423, 252, cFormattedScore ). /* formatted scoring */
            fInsText( 676, 254, v-instr  ). /* po comments */
            cOutLine = TRIM(cOutLine, ",").
            PUT STREAM sEDIPOITEM UNFORMATTED cOutLine SKIP.
            iSequence = iSequence + 1.
            
        END. /* for each po-ordl record */      

            RUN po/ftppo.p (v-outfile[4],"Liberty"). 
            MESSAGE "Liberty file:" TRIM(v-outfile[3]) "has been created" 
                VIEW-AS ALERT-BOX.
      

        PAUSE 1 NO-MESSAGE.
    END. /* for each po-ord record */

/* end ----------------------------------- Copr. 2004  Advanced Software Inc. */





/* ************************  Function Implementations ***************** */

FUNCTION fFormScore RETURNS CHARACTER 
	(INPUT ipdDim AS DECIMAL  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	

		DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
		DEFINE VARIABLE iIntValue AS INTEGER NO-UNDO.
		
		iIntValue = TRUNCATE(ipdDim, 0).
        cResult =   STRING(iIntValue, "9999") 
                  + SUBSTRING(STRING(ipdDim - iIntValue, ".99"), 2)
                  + "x     "
                  .
		RETURN cResult.


		
END FUNCTION.

FUNCTION fInsText RETURNS LOGICAL 
    (INPUT ipiStartPos AS INTEGER,  
    INPUT ipiLen AS INTEGER, INPUT ipcText AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	

    DEFINE VARIABLE lResult AS LOGICAL NO-UNDO.
    IF ipcText NE ? THEN DO:
        ipcText = REPLACE(ipcText, ",", ""). 
        ipctext = TRIM(ipcText).
        ipcText = ipcText + FILL(" ", ipiLen - LENGTH(ipcText)).
        ipcText = ipcText + ",".
        SUBSTRING(cOutLine, ipiStartPos, ipiLen + 1) = ipcText NO-ERROR.      
    END.
    lResult = ERROR-STATUS:ERROR. 
    RETURN lResult.


		
END FUNCTION.

FUNCTION fnRJust RETURNS CHARACTER 
	(INPUT ipcText AS CHARACTER, INPUT ipiLen AS INTEGER):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	

		DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
        cResult = FILL(" ", ipiLen - LENGTH(ipcText)) + ipcText.
		RETURN cResult.


		
END FUNCTION.

FUNCTION fTime RETURNS CHARACTER 
	(INPUT ipiTime AS INTEGER  ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	

		DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
        cResult = STRING(ipiTime, "HH:MM:SS").
        cResult = SUBSTRING(cResult, 1, 2) + 
                  SUBSTRING(cResult, 4, 2) + 
                  substring(cResult, 7, 2).
		RETURN cResult.


		
END FUNCTION.
