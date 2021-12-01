 
 /*------------------------------------------------------------------------
    File        : EstimateCalcProc_Test.p 
    Syntax      : 
    Author(s)   : a.pandianraj
    Created     : Fri Nov 12 05:33:57 EST 2021
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING OpenEdge.Core.Assert FROM PROPATH.


BLOCK-LEVEL ON ERROR UNDO, THROW.

DEFINE VARIABLE hprocHandle AS HANDLE NO-UNDO.
DEFINE VARIABLE ghSession AS HANDLE NO-UNDO.

@Before.
PROCEDURE setUpBeforeProcedure:
    /*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONNECT D:\asigui\Databases\Test\TESTDEVELd.db -H localhost -S 2826 -ld asi NO-ERROR.
    CONNECT D:\asigui\Databases\Test\TESTDEVELa.db -H localhost -S 2836 -ld audit NO-ERROR.
END PROCEDURE. 

@Setup.
PROCEDURE setUp:
    /*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    RUN system\session.p PERSISTENT SET ghSession.
    SESSION:ADD-SUPER-PROCEDURE (ghSession).
    RUN EstimateCalcProcs.p PERSISTENT SET hprochandle.
    

END PROCEDURE.  

@TearDown.
PROCEDURE tearDown:
    /*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DELETE PROCEDURE THIS-PROCEDURE.
END PROCEDURE. 

@After.
PROCEDURE tearDownAfterProcedure: 
    /*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    
END PROCEDURE. 

@Test.  
PROCEDURE TestpCalcEstimate: 
    /*------------------------------------------------------------------------------
            Purpose:                                                                      
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE oRes AS INT64 NO-UNDO.
    DEFINE VARIABLE iestHeaderSeq AS INT64 NO-UNDO.
    DEFINE VARIABLE lRes AS LOGICAL NO-UNDO.
    
    /*For test case*/
    DEFINE VARIABLE quantityMaster       AS DECIMAL NO-UNDO.
    DEFINE VARIABLE marginpct            AS DECIMAL NO-UNDO.
    DEFINE VARIABLE handingchargepct     AS DECIMAL NO-UNDO.
    DEFINE VARIABLE weightNet            AS DECIMAL NO-UNDO FORMAT "->>>>9.99".
    DEFINE VARIABLE weightToatal         AS DECIMAL NO-UNDO FORMAT "->>>>9.99".
    DEFINE VARIABLE dcostTotalBoard      AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dcostTotalMaterial   AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dcostTotalFactory    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dcostTotalNonFactory AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dcostTotalFull       AS DECIMAL NO-UNDO.
    DEFINE VARIABLE dcostTotalLabor      AS DECIMAL NO-UNDO.
    
    DEF VAR lResult AS LOG NO-UNDO.
    /*    iestHeaderSeq = CURRENT-VALUE(estCostHeaderID_seq) + 1.*/
    DO TRANSACTION:
        RUN pCalcEstimate IN hprocHandle 
            (INPUT "001",
            INPUT "  100800",
            INPUT "",
            INPUT 0,
            INPUT 0,
            INPUT NO,
            INPUT NO,
            OUTPUT oRes).
        FOR FIRST asi.estCostHeader WHERE asi.estCostHeader.estCostHeaderID = oRes NO-LOCK:
            ASSIGN 
                quantityMaster   = asi.estCostHeader.quantityMaster
                marginpct        = asi.estCostHeader.marginpct
                handingchargepct = asi.estCostHeader.handlingchargepct
                weightNet        = ROUND(asi.estCostHeader.weightNet,2)
                weightToatal     = ROUND(asi.estCostHeader.weighttotal,2)
                dcostTotalBoard      = asi.estCostHeader.costTotalBoard     
                dcostTotalMaterial   = asi.estCostHeader.costTotalMaterial  
                dcostTotalFactory    = asi.estCostHeader.costTotalFactory   
                dcostTotalNonFactory = asi.estCostHeader.costTotalNonFactory
                dcostTotalFull      = asi.estCostHeader.costTotalFull       
                dcostTotalLabor     = asi.estCostHeader.costTotalLabor          
                .
        END. 
        MESSAGE "Undo" VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE lRes.
        IF lRes THEN 
            UNDO.
    END.
    IF quantityMaster        = 340000.00     AND 
        marginpct            = 10.00         AND 
        handingchargepct     = 0.05          AND
        weightNet            = 54634.41      AND 
        weightToatal         = 54634.41      AND 
        dcostTotalBoard      = 47346.07515   AND
        dcostTotalMaterial   = 54470.780167  AND
        dcostTotalFactory    = 109180.751817 AND
        dcostTotalNonFactory = 27215.065422  AND
        dcostTotalFull       = 136395.817239 AND
        dcostTotalLabor      = 8903.20591    THEN 
        lResult = YES.
        
    Assert:IsTrue(lResult).
    
END PROCEDURE.
