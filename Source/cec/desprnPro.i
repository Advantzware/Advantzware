/* -------------------------------------------------- cec/desprnPro.i 04/97 JLF */
/* Box Design Print                                                           */
/* -------------------------------------------------------------------------- */

DEFINE INPUT PARAMETER v-est-id AS RECID NO-UNDO.
DEFINE            VARIABLE li-num-of-line   AS INTEGER NO-UNDO.
DEFINE {2} SHARED VARIABLE v-out1-id        AS RECID   NO-UNDO.
DEFINE {2} SHARED VARIABLE v-out2-id        AS RECID   NO-UNDO.
DEFINE            VARIABLE v-start-compress AS cha     NO-UNDO.
DEFINE            VARIABLE v-end-compress   AS cha     NO-UNDO.

/*{sys/inc/var.i shared}
{sys/form/s-top.f}
*/
{cecrep/jobtick2.i "shared"}

{cec/descalc.i new}
DEFINE VARIABLE K_FRAC      AS DECIMAL   INIT 6.25 NO-UNDO.
DEFINE VARIABLE v-line-text LIKE box-design-line.line-text.
DEFINE VARIABLE v-int       AS INTEGER.
DEFINE VARIABLE v-char      AS CHARACTER.
DEFINE VARIABLE v-first     AS INTEGER.
DEFINE VARIABLE v-last      AS INTEGER.
DEFINE VARIABLE v-frst-col  LIKE box-design-line.line-no EXTENT 500.
DEFINE VARIABLE v-last-col  LIKE box-design-line.line-no EXTENT 500.
DEFINE VARIABLE v-triad     LIKE sys-ctrl.log-fld.
DEFINE VARIABLE v-hdr       AS CHARACTER FORMAT "x(80)".

DEFINE BUFFER b-bdl FOR box-design-line.
/* for metric display */
DEFINE VARIABLE lv-tmp                AS cha       NO-UNDO.
DEFINE VARIABLE lv-tmp-val            AS cha       NO-UNDO.
DEFINE VARIABLE lv-met-lsc            AS cha       NO-UNDO.
DEFINE VARIABLE lv-met-lcum           AS cha       NO-UNDO.
DEFINE VARIABLE ld-tmp-scr            AS DECIMAL   NO-UNDO.
DEFINE VARIABLE li-num-space          AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-boxln               AS INTEGER   NO-UNDO.
DEFINE VARIABLE cRtnChar              AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cJobCardPrntScor-Log  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cJobCardPrntScor-Char AS CHARACTER NO-UNDO.
DEFINE VARIABLE cStyle                AS CHARACTER NO-UNDO.
DEFINE VARIABLE lJobCardPrntScorLog   AS LOGICAL   NO-UNDO .

DEFINE BUFFER bf-box-design-hdr FOR box-design-hdr .

RUN sys/ref/nk1look.p (INPUT cocode, "JobCardPrintScores", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    cJobCardPrntScor-Log = LOGICAL(cRtnChar) NO-ERROR. 

RUN sys/ref/nk1look.p (INPUT cocode, "JobCardPrintScores", "C" /* Character */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    cJobCardPrntScor-Char = cRtnChar NO-ERROR. 

FORM w-box-design-line.wcum-score-c             AT 1
    w-box-design-line.wscore-c
    v-line-text           FORMAT "x(65)"  AT 23              SKIP
    WITH DOWN FRAME f1 NO-LABELS NO-BOX WIDTH 150.

FORM w-box-design-line.wscore-c                 AT 1
    v-line-text                     FORMAT "x(65)"
    w-box-design-line.wcum-score-c                                         SKIP
    WITH DOWN FRAME f2 NO-LABELS NO-BOX WIDTH 120.

{sys/inc/f16to32.i}
{sys/inc/jobcard.i C}
v-triad = sys-ctrl.char-fld EQ "Triad".

FIND est WHERE RECID(est) EQ v-est-id NO-LOCK.
cStyle = "".
FOR EACH ef
    WHERE ef.company = est.company 
    AND ef.est-no    EQ est.est-no
    AND (v-ef-recid EQ ? OR
    v-ef-recid EQ recid(ef))
    NO-LOCK,
    
    EACH eb
    WHERE eb.company = ef.company
    AND eb.est-no   EQ ef.est-no
    AND eb.form-no EQ ef.form-no
    AND (v-out1-id = ? OR RECID(eb) = v-out1-id)
    NO-LOCK:

    FOR EACH w-box-design-line:
        DELETE w-box-design-line.
    END.

    ASSIGN 
        cStyle = eb.style.

    FIND FIRST item
        {sys/look/itemgsW.i}
        AND item.i-no EQ eb.adhesive
    NO-LOCK NO-ERROR.
  
    FIND box-design-hdr
        WHERE box-design-hdr.design-no EQ 0
        AND box-design-hdr.company = eb.company 
        AND box-design-hdr.est-no    EQ eb.est-no
        AND box-design-hdr.form-no   EQ eb.form-no
        AND box-design-hdr.blank-no  EQ eb.blank-no
        NO-LOCK NO-ERROR.

    /*if avail eb then*/
    FIND FIRST style
        WHERE style.company EQ eb.company
        AND style.style   EQ eb.style
        NO-LOCK NO-ERROR.

    FIND FIRST bf-box-design-hdr NO-LOCK
        WHERE bf-box-design-hdr.design-no EQ style.design-no
        NO-ERROR.

    IF NOT cJobCardPrntScor-Log AND AVAILABLE bf-box-design-hdr AND
        AVAILABLE box-design-hdr AND box-design-hdr.design-no EQ 0  THEN 
    DO:
        IF bf-box-design-hdr.lscore NE  box-design-hdr.lscore OR
            bf-box-design-hdr.lcum-score NE box-design-hdr.lcum-score THEN
    /* lJobCardPrntScorLog = YES .*/
    END.

  
    IF (NOT AVAILABLE box-design-hdr) AND AVAILABLE style THEN
        FIND box-design-hdr
            WHERE box-design-hdr.design-no EQ style.design-no
            NO-LOCK NO-ERROR.

    IF AVAILABLE box-design-hdr          AND
        box-design-hdr.design-no EQ 0 THEN 
    DO:

        ASSIGN
            v-lscore-c     = box-design-hdr.lscore
            v-lcum-score-c = box-design-hdr.lcum-score.
    
        IF est.metric THEN 
        DO:
            ASSIGN 
                lv-tmp      = ""
                lv-tmp-val  = ""
                lv-met-lsc  = ""
                lv-met-lcum = "".

            DO i = 1 TO LENGTH(box-design-hdr.lscore):
                lv-tmp-val = SUBSTRING(box-design-hdr.lscore,i,1).
                IF lv-tmp-val <> " " THEN lv-tmp = lv-tmp + lv-tmp-val.
                ELSE 
                DO:
                    IF lv-tmp <> "" THEN 
                    DO:
                        ld-tmp-scr = ({sys/inc/k16bv.i dec(lv-tmp)}) * 25.4.
                        lv-tmp = "".
                        li-num-space = 0.
                    END.
                    li-num-space = li-num-space + 1.
                    lv-met-lsc = lv-met-lsc + (IF li-num-space <= 7 THEN lv-tmp-val ELSE "") +
                        IF ld-tmp-scr = 0 THEN "" ELSE TRIM(STRING(ld-tmp-scr)).
                    ld-tmp-scr = 0.
                END.                                    
            END. 
            IF lv-tmp <> "" THEN 
            DO:
                ld-tmp-scr = ({sys/inc/k16bv.i dec(lv-tmp)}) * 25.4.
                lv-met-lsc = lv-met-lsc + string(ld-tmp-scr).
            END.
            ASSIGN 
                lv-tmp       = ""
                lv-tmp-val   = ""
                lv-met-lcum  = ""
                ld-tmp-scr   = 0
                li-num-space = 0.

            DO i = 1 TO LENGTH(box-design-hdr.lcum-score):
                lv-tmp-val = SUBSTRING(box-design-hdr.lcum-score,i,1).
                IF lv-tmp-val <> " " THEN lv-tmp = lv-tmp + lv-tmp-val.
                ELSE 
                DO:
                    IF lv-tmp <> "" THEN 
                    DO:
                        ld-tmp-scr = ({sys/inc/k16bv.i dec(lv-tmp)}) * 25.4.
                        lv-tmp = "".
                        li-num-space = 0.
                    END.
                    li-num-space = li-num-space + 1.
                    lv-met-lcum = lv-met-lcum + (IF li-num-space <= 7 THEN lv-tmp-val ELSE "") +
                        IF ld-tmp-scr = 0 THEN "" ELSE TRIM(STRING(ld-tmp-scr)).
                    ld-tmp-scr = 0.
                END.                   
            END.
            IF lv-tmp <> "" THEN 
            DO:
                ld-tmp-scr = ({sys/inc/k16bv.i dec(lv-tmp)}) * 25.4.
                lv-met-lcum = lv-met-lcum + string(ld-tmp-scr).
            END.

            ASSIGN 
                v-lscore-c     = lv-met-lsc
                v-lcum-score-c = lv-met-lcum.
        END. /* for metric display */

        i = 0.
        FOR EACH box-design-line OF box-design-hdr
            WHERE box-design-line.wcum-score NE ""
            NO-LOCK:

            i = i + 1.
            CREATE w-box-design-line.
            ASSIGN
                w-box-design-line.line-no      = box-design-line.line-no
                w-box-design-line.wscore-c     = IF est.metric THEN STRING(({sys/inc/k16bv.i dec(box-design-line.wscore)}) * 25.4) ELSE box-design-line.wscore
                w-box-design-line.wcum-score-c = IF est.metric THEN STRING(({sys/inc/k16bv.i dec(box-design-line.wscore)}) * 25.4) ELSE box-design-line.wcum-score.
        END.
    END.

    ELSE 
    DO:
        RUN cec/descalc.p (RECID(est), RECID(eb)).
        IF v-lscore-c BEGINS "No Design" THEN RETURN.
    END.
  

    ASSIGN
        v-frst-col     = 0
        v-last-col     = 0
        li-num-of-line = 0.
 
    FOR EACH b-bdl OF box-design-hdr
        NO-LOCK
        BREAK BY b-bdl.design-no:

        v-line-text = b-bdl.line-text.
        IF v-line-text <> "" THEN li-num-of-line = li-num-of-line + 1.   /* ysk*/
    
        IF v-line-text NE "" THEN
        DO v-int = 1 TO LENGTH(v-line-text):
            IF substr(v-line-text,v-int,1) NE "" THEN 
            DO:
                v-last-col[v-int] = b-bdl.line-no.
                IF v-frst-col[v-int] EQ 0 THEN v-frst-col[v-int] = b-bdl.line-no.
            END.
        END.
    END.
  
    IF AVAILABLE eb AND eb.cad-no = "" THEN
        v-hdr = /*(if v-triad then "           " else "Totals  Score     ") +  */
            "                  " + 
            "Design #: " +
            trim(STRING(IF AVAILABLE style AND box-design-hdr.design-no EQ 0 THEN
            style.design-no ELSE box-design-hdr.design-no,">>>")) +
            "   " + box-design-hdr.DESCRIPTION + "    CorrDir:"  +
            IF ef.xgrain = "N" THEN "Vertical" ELSE "Horizontal" .
    ELSE v-hdr = /*(if v-triad then "           " else "Totals  Score     ") +  */
            "                  " + 
            "Design #: " +
            trim(STRING(IF AVAILABLE style AND box-design-hdr.design-no EQ 0 THEN
            style.design-no ELSE box-design-hdr.design-no,">>>")) +
            "   " + box-design-hdr.DESCRIPTION.

    PUT {1} v-hdr SKIP.

    IF v-triad THEN
        PUT {1} SPACE(6) v-lcum-score-c SPACE(2) "Totals" SKIP.

    IF (cJobCardPrntScor-Log AND (LOOKUP(cStyle,cJobCardPrntScor-Char) NE 0 OR cJobCardPrntScor-Char EQ ""))
        OR (NOT cJobCardPrntScor-Log AND lJobCardPrntScorLog) THEN 
    DO: 
        IF est.metric THEN 
        DO:
            IF LENGTH(v-lscore-c) <= 90 THEN 
            DO:
                IF v-triad THEN
                    PUT {1} /*"Score"*/ "         "  v-lscore-c    SKIP.
                ELSE
                    PUT {1} SPACE(7) "           "  v-lscore-c  FORM "x(100)"  SKIP
                        SPACE(7) "           " v-lcum-score-c FORM "x(100)"  SKIP.
            END.
            ELSE 
            DO:
                IF v-triad THEN
                    PUT {1} /*"Score"*/ "         "  v-lscore-c    SKIP.
                ELSE
                    PUT {1} "<P9>" SPACE(7) "           "  v-lscore-c  FORM "x(135)"  SKIP
                        SPACE(7) "           " v-lcum-score-c FORM "x(135)"  "<P12>" SKIP.
            END.
        END.
        ELSE 
        DO:
            IF LENGTH(v-lscore-c) <= 90 THEN 
            DO:
                IF v-triad THEN
                    PUT {1} /*space(13) "     " */ v-lscore-c    SKIP.
                ELSE
                    PUT {1} SPACE(3) /* "     " */  v-lscore-c  FORM "x(90)"  SKIP
                        SPACE(3) /*"      " */ v-lcum-score-c FORM "x(90)"  SKIP.
            END.
            ELSE 
            DO:
                IF v-triad THEN PUT {1} /*space(13) "     " */ v-lscore-c    SKIP.
                ELSE
                    PUT {1} "<P9>" SPACE(3) /* "     " */  v-lscore-c  FORM "x(135)"  SKIP
                        SPACE(3) /*"      " */ v-lcum-score-c FORM "x(135)" "<P12>" SKIP.
            END.
        END.
    END.
    v-lines = v-lines + 4.
    PUT SKIP(1).

    IF box-design-hdr.box-image = "" THEN 
    DO:

        FOR EACH b-bdl OF box-design-hdr
            NO-LOCK
            WHERE b-bdl.line-text <> ""
            BREAK BY b-bdl.design-no:
  

            FIND FIRST w-box-design-line
                WHERE w-box-design-line.line-no EQ b-bdl.line-no
                NO-ERROR.

            ASSIGN
                v-line-text = b-bdl.line-text
                v-first     = 0
                v-last      = 3000.
            /* xprint bug : if print "-" more than 3 in a row, xprint draw rectangular and line*/
            DO i = 1 TO LENGTH(v-line-text):
                IF SUBSTRING(v-line-text,i,1) = "-" THEN SUBSTRING(v-line-text,i,1) = "_".       
            END.
            IF v-triad THEN 
            DO:
                IF (cJobCardPrntScor-Log AND (LOOKUP(cStyle,cJobCardPrntScor-Char) NE 0 OR cJobCardPrntScor-Char EQ ""))
                    OR (NOT cJobCardPrntScor-Log AND lJobCardPrntScorLog) THEN  
                    DISPLAY 
                        (w-box-design-line.wscore-c)           WHEN AVAILABLE w-box-design-line AT 8
                        v-line-text 
                        (w-box-design-line.wcum-score-c) WHEN AVAILABLE w-box-design-line
                        @ w-box-design-line.wcum-score-c
                        WITH FRAME f2 STREAM-IO NO-BOX NO-LABELS .
                DOWN  WITH FRAME f2 .
            END.

            ELSE 
            DO:
                IF (cJobCardPrntScor-Log AND (LOOKUP(cStyle,cJobCardPrntScor-Char) NE 0  OR cJobCardPrntScor-Char EQ ""))
                    OR (NOT cJobCardPrntScor-Log AND lJobCardPrntScorLog) THEN  
                    DISPLAY 
                        TRIM(w-box-design-line.wcum-score-c) WHEN AVAILABLE w-box-design-line
                        @ w-box-design-line.wcum-score-c FORM "x(6)" AT 8
                        TRIM(w-box-design-line.wscore-c)     WHEN AVAILABLE w-box-design-line               
                        @ w-box-design-line.wscore-c   FORM "x(6)"
                        v-line-text  /*AT 14 */ FORM "x(65)"
                        WITH FRAME f1 STREAM-IO NO-BOX NO-LABELS NO-ATTR-SPACE.
                DOWN  WITH FRAME f1.
            END.

            v-lines = v-lines + 1.
    
  
        END. /* for each b-bdl */
        PAGE.
    END. /* no box image */
    ELSE 
    DO:     

        FILE-INFO:FILE-NAME = box-design-hdr.box-image.
     
        PUT UNFORMATTED 
            "<C3><#30><R+40><C+85><IMAGE#30=" FILE-INFO:FULL-PATHNAME ">" .
        PUT UNFORMATTED 
            "<=30>" SKIP.
        FOR EACH box-design-line OF box-design-hdr NO-LOCK:
            IF (cJobCardPrntScor-Log AND (LOOKUP(cStyle,cJobCardPrntScor-Char) NE 0 OR cJobCardPrntScor-Char EQ "" ))
                OR (NOT cJobCardPrntScor-Log AND lJobCardPrntScorLog)  THEN  
                PUT  /*space(6)  */
                    "<C87>"
                    IF est.metric AND box-design-line.wscore <> "" THEN STRING( ({sys/inc/k16bv.i dec(box-design-line.wscore)}) * 25.4) ELSE box-design-line.wscore " " 
                    IF est.metric AND box-design-line.wcum-score <> "" THEN STRING( ({sys/inc/k16bv.i dec(box-design-line.wcum-score)}) * 25.4) ELSE box-design-line.wcum-score SKIP.
        END.
        .
    END. /* else box image */
    PUT "<P10>" .
    PAGE.
END.
/* end ---------------------------------- copr. 1997  advanced software, inc. */
