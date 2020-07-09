
DEFINE INPUT PARAMETER ip-rowid1 AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ip-rowid2 AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipiType  AS INTEGER NO-UNDO.  /* 1 is inks,
                                              2 is inks & units,
                                              3 is packing
                                              4 is freight
                                              ? is all */
DEFINE INPUT PARAMETER iplPrompt  AS LOGICAL NO-UNDO.
DEFINE BUFFER b-eb  FOR eb.
DEFINE BUFFER b-eb1 FOR eb.
DEFINE BUFFER b-ef  FOR ef.


DEFINE VARIABLE ll            AS LOG       NO-UNDO.
DEFINE VARIABLE lj            AS INTEGER   NO-UNDO.
DEFINE VARIABLE li            AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-side-count  AS INTEGER   NO-UNDO.

DEFINE VARIABLE lFound     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lUpdateFGItem AS LOGICAL   NO-UNDO. 
DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCriteria AS CHARACTER NO-UNDO.
DEFINE VARIABLE lMatchFGItem AS LOGICAL NO-UNDO.
DEFINE VARIABLE lMatchPart AS LOGICAL NO-UNDO.
DEFINE VARIABLE lMatchCad AS LOGICAL NO-UNDO.
DEFINE VARIABLE lUpdInks AS LOGICAL NO-UNDO.
DEFINE VARIABLE lUpdPack AS LOGICAL NO-UNDO.
DEFINE VARIABLE lUpdFreight AS LOGICAL NO-UNDO.


FIND eb WHERE ROWID(eb) EQ ip-rowid1 NO-LOCK NO-ERROR.

IF AVAILABLE eb THEN 
DO:

    RUN sys/ref/nk1look.p (INPUT eb.company, "CEUpdate", "L" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturn, OUTPUT lFound).
    lUpdateFGItem = cReturn EQ "YES".

    RUN sys/ref/nk1look.p (INPUT eb.company, "CEUpdate", "C" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cCriteria, OUTPUT lFound).
    IF NOT lFound OR cCriteria EQ "" OR cCriteria EQ "None" THEN 
        cCriteria = "FG Item #".

    IF LOOKUP("FG Item #", cCriteria) > 0 THEN 
        lMatchFGItem = YES.
    IF LOOKUP("Customer Part #", cCriteria) > 0 THEN 
        lMatchPart = YES.
    IF LOOKUP("CAD #", cCriteria) > 0 THEN 
        lMatchCad = YES.

    ASSIGN 
        lUpdInks = (ipiType LE 2 OR ipiType EQ ?)
        lUpdPack = (ipiType EQ 3 OR ipiType EQ ?)
        lUpdFreight = (ipiType EQ 4 OR ipiType EQ ?)
        .
    RELEASE b-eb.
    IF lMatchFGItem OR lMatchPart OR lMatchCad THEN 
        FIND FIRST b-eb NO-LOCK 
            WHERE b-eb.company EQ eb.company
            AND ((b-eb.stock-no EQ eb.stock-no AND eb.stock-no NE "" AND lMatchFGItem)
            OR (b-eb.cad-no EQ eb.cad-no AND eb.cad-no NE "" AND  lMatchCad)
            OR (b-eb.part-no EQ eb.part-no AND eb.part-no NE "" AND lMatchPart) )
            AND ROWID(b-eb)   NE ip-rowid1
            AND ROWID(b-eb)   NE ip-rowid2
            NO-ERROR.
        
    IF AVAILABLE b-eb THEN DO: 
            IF iplPrompt THEN
            RUN est\dUpdEst.w (cCriteria, INPUT-OUTPUT lUpdInks, INPUT-OUTPUT lUpdPack, INPUT-OUTPUT lUpdFreight).               
        IF lUpdInks OR lUpdPack OR lUpdFreight THEN 
            FOR EACH b-eb 
                WHERE b-eb.company EQ eb.company
                AND ((b-eb.stock-no EQ eb.stock-no AND eb.stock-no NE "" AND lMatchFGItem)
                OR (b-eb.cad-no EQ eb.cad-no AND eb.cad-no NE "" AND lMatchCad)
                OR (b-eb.part-no EQ eb.part-no AND eb.part-no NE "" AND  lMatchPart) )
                AND ROWID(b-eb)   NE ip-rowid1
                AND ROWID(b-eb)   NE ip-rowid2
                :
                IF lUpdInks THEN DO:
                    {est/copyinks.i}
                END.
                IF lUpdPack THEN DO:  
                    {est/copypack.i}
                    RUN est/CopyEnhancedPack.p(ROWID(eb),ROWID(b-eb)) .
                END.            
                IF lUpdFreight THEN DO:
                    {est/copyfrat.i}
                END.
            END.
    END.
    IF lUpdPack AND lUpdateFGItem AND eb.stock-no NE "" THEN DO:
        FIND FIRST itemfg EXCLUSIVE-LOCK 
            WHERE itemfg.company EQ eb.company
            AND itemfg.i-no EQ eb.stock-no
            NO-ERROR.
        IF AVAILABLE itemfg THEN 
            ASSIGN 
                itemfg.case-count = eb.cas-cnt
                itemfg.case-pall = eb.cas-pal
                itemfg.trno = eb.tr-no
                itemfg.spare-char-4 = eb.dest-code 
                .
    END.
        
END.  /* if avail eb */


