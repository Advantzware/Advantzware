
/*------------------------------------------------------------------------
    File        : CalcLayout.p
    Purpose     : replaces the calc-layout procedure from the various b-estitm.w

    Syntax      :  Accepts ROWID for eb, ef, or est

    Description : Calculates the ef values for the Layout Tab

    Author(s)   : BV
    Created     : Sun Jun 25 17:45:54 EDT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipcIndustry AS CHARACTER NO-UNDO. 
DEFINE INPUT PARAMETER ipriEf AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER ipriEb AS ROWID NO-UNDO.
DEFINE INPUT PARAMETER iplNew AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER iplPromptForReset AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER iplCalcDim AS LOGICAL NO-UNDO. 

DEFINE BUFFER bf-eb FOR eb.

DEFINE VARIABLE lNew AS LOGICAL NO-UNDO.
DEFINE VARIABLE cDefaultMachine AS CHARACTER NO-UNDO.
DEFINE VARIABLE iMaxWidth AS INTEGER NO-UNDO.

/*Refactor*/
DEFINE SHARED BUFFER xef FOR ef.
DEFINE SHARED BUFFER xeb FOR eb.
DEFINE SHARED BUFFER xest FOR est.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
FIND ef WHERE ROWID(ef) EQ ipriEf NO-ERROR.
FIND eb WHERE ROWID(eb) EQ ipriEb NO-ERROR.


RUN pGetCEROUTESettings(eb.company, ipcIndustry,
                        OUTPUT cDefaultMachine,
                        OUTPUT iMaxWidth).

lNew = iplNew 
    AND NOT CAN-FIND(FIRST bf-eb
    WHERE bf-eb.company EQ eb.company
    AND bf-eb.est-no  EQ eb.est-no
    AND bf-eb.form-no EQ eb.form-no
    AND ROWID(bf-eb)  NE ROWID(eb)) /*not a combo*/.

IF NOT lNew THEN
DO:
    IF iplPromptForReset THEN
        MESSAGE "Do you wish to reset layout screen?"
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
            UPDATE lNew.
    ELSE
        lNew = YES.
END.

IF lNew THEN 
DO:
    IF iplCalcDim THEN 
    DO:
        IF ef.m-code EQ ''THEN 
            ef.m-code = cDefaultMachine.

        FIND FIRST mach NO-LOCK 
            WHERE mach.company EQ ef.company
            AND mach.m-code EQ ef.m-code
            NO-ERROR.
        IF AVAILABLE mach THEN 
        DO:
            ASSIGN
                ef.m-dscr   = mach.m-dscr
                ef.roll     = mach.p-type EQ "R"
                ef.lam-dscr = "S"
                ef.lsh-wid  = mach.max-len
                ef.lsh-len  = mach.max-wid.

            FIND FIRST ITEM NO-LOCK 
                WHERE item.company EQ ef.company
                AND item.i-no EQ ef.board
                NO-ERROR.
            
            IF NOT AVAILABLE item OR item.i-code EQ "E" THEN
                ASSIGN
                    ef.gsh-wid = TRUNCATE(mach.max-len / eb.t-wid,0) *
                         eb.t-wid + (mach.min-triml * 2)
                    ef.gsh-len = TRUNCATE(mach.max-wid / eb.t-len,0) *
                         eb.t-len + (mach.min-trimw * 2).
            ELSE
                ASSIGN
                    ef.gsh-wid = item.s-wid
                    ef.gsh-len = item.s-len.

            ASSIGN
                ef.nsh-wid  = ef.gsh-wid
                ef.nsh-len  = ef.gsh-len
                ef.n-out    = 1
                ef.n-out-l  = 1
                ef.trim-w   = ef.nsh-wid - (mach.min-triml * 2)
                ef.trim-l   = ef.nsh-len - (mach.min-trimw * 2)
                eb.num-wid = trunc(ef.trim-w / eb.t-len,0)
                eb.num-len = trunc(ef.trim-l / eb.t-wid,0)
                eb.num-up  = eb.num-wid * eb.num-len.
        END.
        /*Refactor*/
        FIND xeb WHERE ROWID(xeb) EQ ROWID(eb).
        FIND xef WHERE ROWID(xef) EQ ROWID(ef).
        RUN cec/calc-dim.p.
    END.
END.

IF cDefaultMachine NE "" THEN 
DO:
    FIND FIRST mach
        WHERE mach.company EQ ef.company
        AND mach.loc     EQ ef.loc
        AND mach.m-code  EQ cDefaultMachine
        AND mach.dept[1] EQ "CR"
        NO-LOCK NO-ERROR.
    IF AVAILABLE mach THEN 
    DO:
        ASSIGN
            ef.m-code   = cDefaultMachine
            ef.lsh-lock = NO
            eb.num-wid  = 1
            eb.num-len  = 1.
        
        /*Refactor*/
        RUN cec/calc-dim1.p NO-ERROR.

        ASSIGN
            ef.gsh-len = ef.gsh-len - (ef.nsh-len * ef.n-out-l)
            ef.n-out-l = 1
            ef.gsh-len = ef.gsh-len + (ef.nsh-len * ef.n-out-l).

        IF iMaxWidth NE 0 AND iMaxWidth LT ef.gsh-wid THEN
            ASSIGN
                ef.n-out   = TRUNC(iMaxWidth / ef.nsh-wid,0)
                ef.gsh-wid = ef.n-out * ef.nsh-wid + (mach.min-trimw * 2).
    END.
END.




/* **********************  Internal Procedures  *********************** */

PROCEDURE pGetCERouteSettings:
/*------------------------------------------------------------------------------
 Purpose:  Returns the variables based on the 
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcIndustry AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcDefaultMachine AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiMaxWidth AS INTEGER NO-UNDO.
    
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO. 
    
    RUN sys\ref\nk1look.p (ipcCompany,
        'CEROUTE' + ipcIndustry,
        'C',
        NO,
        NO,
        '',
        '', 
        OUTPUT cReturn,
        OUTPUT lFound).
    
    IF lFound THEN DO: 
        opcDefaultMachine = cReturn.
        RUN sys\ref\nk1look.p (ipcCompany,
            'CEROUTE' + ipcIndustry,
            'I',
            NO,
            NO,
            '',
            '', 
            OUTPUT cReturn,
            OUTPUT lFound).
        opiMaxWidth = INTEGER(cReturn).
        
    END.    

END PROCEDURE.
    
