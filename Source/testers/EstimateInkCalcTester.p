
/*------------------------------------------------------------------------
    File        : EstimateInkCalcTester.p.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : BV
    Created     : Thu Jan 24 16:45:11 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


DEFINE VARIABLE ghSession       AS HANDLE.
DEFINE VARIABLE ghEstimateProcs AS HANDLE.

DEFINE VARIABLE lError          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage        AS CHARACTER NO-UNDO.

DEFINE VARIABLE cCompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE cEstNo AS CHARACTER NO-UNDO.
DEFINE VARIABLE dQty AS DECIMAL NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */

/* ***************************  Main Block  *************************** */


ASSIGN
    cCompany = "001"
    cEstNo = "  101483" //101483,101604
    dQty = 5000.

RUN system\session.p PERSISTENT SET ghSession.
SESSION:ADD-SUPER-PROCEDURE (ghSession).
RUN est\EstimateProcs.p PERSISTENT SET ghEstimateProcs.
THIS-PROCEDURE:ADD-SUPER-PROCEDURE (ghEstimateProcs).


RUN pUpdateUnitNoForEBBuffer (cCompany, cEstNo).

//RUN pCalcInkCount(cCompany, cEstNo, dQty).


/* **********************  Internal Procedures  *********************** */
PROCEDURE pUpdateUnitNoForEBBuffer:  
      
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstNo   AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE iCnt    AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cList   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iUnitNo AS INTEGER   NO-UNDO.

    DEFINE BUFFER bfEX-eb FOR eb.

    FOR EACH eb NO-LOCK
        WHERE eb.company = ipcCompany
        AND eb.est-no EQ ipcEstNo 
        AND eb.form-no = 1
        AND eb.blank-no = 2
        :
      
        DO iCnt = 1 to 17:
            RUN est/GetInksUnitNo.p (BUFFER eb, iCnt, eb.i-code2[iCnt], OUTPUT iUnitNo). 
            
            cList = cList + CHR(10) + eb.i-code2[iCnt] + " - " + STRING(iUnitNo).
        
            IF iUnitNo <> 0 THEN
            Do TRANSACTION:
                FIND FIRST bfEX-eb EXCLUSIVE-LOCK
                    WHERE ROWID(bfEX-eb) = ROWID(eb) NO-ERROR.
        
                IF AVAILABLE bfEX-eb THEN
                    bfEX-eb.unitno[iCnt] = iUnitNo.
                    
                RELEASE bfEX-eb.
            END.
            
        END. /* DO iCnt = 1 to 10: */
    
        MESSAGE 
            cList
            VIEW-AS ALERT-BOX.
    
    END. /* FOR FIRST eb NO-LOCK */

END.

PROCEDURE pCalcInkCount:
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstNo   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipdQty     AS DECIMAL NO-UNDO.
    
    DEFINE VARIABLE iNum-col    AS INTEGER NO-UNDO.
    DEFINE VARIABLE iNum-Varn   AS INTEGER NO-UNDO.
    
    
    FOR EACH est-op NO-LOCK
        WHERE est-op.company = ipcCompany
        AND est-op.est-no eq ipcEstNo
        AND est-op.dept = "PR"
        BY est-op.line desc:
            
            MESSAGE "est-op.m-code" est-op.m-code skip
            est-op.qty skip
            "est-op.s-num" est-op.s-num skip
            "est-op.b-num" est-op.b-num
            VIEW-AS ALERT-BOX.
        
        RUN Estimate_CalcInkUsingUnitNo in ghEstimateProcs 
        (est-op.company, 
         est-op.est-no, 
         est-op.s-num, 
         est-op.b-num,
         est-op.op-pass, 
         OUTPUT iNum-col,
         OUTPUT iNum-Varn).
         
         MESSAGE
         "iNum-col" iNum-col skip
         "iNum-Varn" iNum-Varn
          
         VIEW-AS ALERT-BOX.
    END.
    
END.
    