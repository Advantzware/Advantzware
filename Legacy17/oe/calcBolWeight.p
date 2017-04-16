
/*------------------------------------------------------------------------
    File        : calcBolWeight.p
    Purpose     : 

    Syntax      :

    Description : Passing in rowid if oe-bolh or oe-relh returns weight 
                  for all lines on BOl. Passing in rowid of oe-boll returns
                  just the weight of that oe-boll

    Author(s)   : 
    Created     : Wed Oct 14 10:26:18 EDT 2015
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT  PARAMETER iprBOL AS ROWID NO-UNDO.
DEFINE OUTPUT PARAMETER opdWeight AS DECIMAL NO-UNDO.

DEFINE VARIABLE dTotWgt       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dLineWgt      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dReturnWeight AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lRecFound     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cNK1Value     AS CHARACTER NO-UNDO.
DEFINE VARIABLE BolWeight-log AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cInputType    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCompany      AS CHARACTER NO-UNDO.
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION wgtFromItemfg RETURNS DECIMAL 
    (ipdCases AS DECIMAL, ipcItem AS CHAR  ) FORWARD.

FUNCTION wgtFromLoadtag RETURNS DECIMAL 
    (ipdCases AS DECIMAL  ) FORWARD.


/* ***************************  Main Block  *************************** */

FIND FIRST oe-boll WHERE ROWID(oe-boll) EQ iprBol NO-LOCK NO-ERROR.
IF AVAIL oe-boll THEN 
    ASSIGN cInputType = "oe-boll" cCompany   = oe-boll.company. 
ELSE 
DO:
    
    FIND FIRST oe-bolh WHERE ROWID(oe-bolh) EQ iprBol NO-LOCK NO-ERROR.
    IF AVAIL oe-bolh THEN 
        ASSIGN cInputType = "oe-bolh" cCompany   = oe-bolh.company.
    ELSE 
    DO:
        
        FIND FIRST oe-relh WHERE ROWID(oe-relh) EQ iprBol NO-LOCK NO-ERROR.
        IF AVAIL oe-relh THEN 
            ASSIGN cInputType = "oe-relh" cCompany   = oe-relh.company.
        ELSE DO:
            FIND FIRST oe-rell WHERE ROWID(oe-rell) EQ iprBol NO-LOCK NO-ERROR.
            IF AVAIL oe-rell THEN 
                ASSIGN cInputType = "oe-rell" cCompany   = oe-rell.company.             
        END.
    END.
    
END.

IF cCompany GT "" THEN 
DO:
    RUN sys/ref/nk1look.p (INPUT cCompany, "BolWeight", "L" /* Char */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cNk1Value, OUTPUT lRecFound).
    
    IF lRecFound THEN
        bolWeight-log = LOGICAL(cNk1Value) NO-ERROR.
END.
/* ************************  Function Implementations ***************** */


FUNCTION wgtFromItemfg RETURNS DECIMAL 
    (ipdCases AS DECIMAL, ipcItem AS CHAR  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	

    DEFINE VARIABLE opdWeight AS DECIMAL NO-UNDO.
    FIND FIRST itemfg WHERE itemfg.company EQ cCompany
        AND itemfg.i-no EQ ipcItem
        NO-LOCK NO-ERROR.
    IF AVAIL itemfg THEN 
        opdWeight = ipdCases / 100 * itemfg.weight-100.
    RETURN opdWeight.


		
END FUNCTION.

FUNCTION wgtFromLoadtag RETURNS DECIMAL 
    (ipdCases AS DECIMAL  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	

    DEFINE VARIABLE opdWeight AS DECIMAL NO-UNDO.
    opdWeight = ( (ipdCases * loadtag.misc-dec[1] ) +
        loadtag.misc-dec[3] ).
    RETURN opdWeight.


		
END FUNCTION.


CASE cInputType:
    WHEN "oe-boll" THEN 
        DO:
            
            
            IF AVAIL oe-boll THEN 
            DO:
                
                
                FIND FIRST loadtag NO-LOCK 
                    WHERE loadtag.company EQ oe-boll.company
                    AND loadtag.item-type EQ NO
                    AND loadtag.tag-no  EQ oe-boll.tag NO-ERROR.
                    
                IF bolWeight-log AND AVAIL loadtag           
                    THEN ASSIGN dLineWgt = wgtFromLoadtag(oe-boll.cases).
                    
                IF dLineWgt EQ 0 THEN
                    ASSIGN dLineWgt = wgtFromItemfg(oe-boll.qty, oe-boll.i-no).
                    
                    
            END. /* avail oe-boll */
            
            dReturnWeight = dLineWgt.
            
            
        END. /* when oe-boll */

    WHEN "oe-rell" THEN 
        DO:
            
            
            IF AVAIL oe-rell THEN 
            DO:
                
                
                FIND FIRST loadtag NO-LOCK 
                    WHERE loadtag.company EQ oe-rell.company
                    AND loadtag.item-type EQ NO
                    AND loadtag.tag-no  EQ oe-rell.tag NO-ERROR.
                    
                IF bolWeight-log AND AVAIL loadtag           
                    THEN ASSIGN dLineWgt = wgtFromLoadtag(oe-rell.cases).
                    
                IF dLineWgt EQ 0 THEN
                    ASSIGN dLineWgt = wgtFromItemfg(oe-rell.qty, oe-rell.i-no).
                    
                    
            END. /* avail oe-rell */
            
            dReturnWeight = dLineWgt.
            
            
        END. /* when oe-rell */        
        
    WHEN "oe-bolh" THEN 
        DO:
            
            
            
            IF AVAIL oe-bolh THEN 
            DO:
        
                dTotWgt = 0.
                FOR EACH oe-boll NO-LOCK WHERE oe-boll.company EQ oe-bolh.company
                    AND oe-boll.bol-no EQ oe-bolh.bol-no:
                        
                        
                    IF oe-boll.weight GT 0 THEN 
                        dTotWgt = dTotWgt + oe-boll.weight.
                    ELSE 
                    DO:
                        
                        dLineWgt = 0.
                        IF oe-boll.tag GT "" THEN 
                            FIND FIRST loadtag NO-LOCK 
                                WHERE loadtag.company = oe-boll.company
                                AND loadtag.item-type = NO
                                AND loadtag.tag-no  = oe-boll.tag NO-ERROR.
                        IF bolWeight-log AND AVAIL loadtag           
                            THEN ASSIGN dLineWgt = wgtFromLoadtag(oe-boll.cases).
                        IF dLineWgt EQ 0 THEN
                            ASSIGN dLineWgt = wgtFromItemfg(oe-boll.qty, oe-boll.i-no).
                        dTotWgt = dTotWgt + dLineWgt.
                        
                        
                    END. /* If weight ne 0 */
                     
                END. /* each oe-boll */
        
            END. /* avail oe-bolh */
            
            dReturnWeight = dTotWgt.
            
            
            
            
        END. /* when oe-bolh */
        
        
    WHEN "oe-relh" THEN 
        DO:
            IF AVAIL oe-relh THEN 
            DO:
            

                    
                dTotWgt = 0.
                FOR EACH oe-rell NO-LOCK
                    WHERE oe-rell.company EQ oe-relh.company
                    AND oe-rell.r-no    EQ oe-relh.r-no                     
                    USE-INDEX r-no ,
                    
                    FIRST oe-bolh NO-LOCK 
                    WHERE  oe-bolh.company EQ oe-relh.company
                    AND oe-bolh.release# EQ oe-relh.release#,
                    
                    FIRST oe-boll NO-LOCK
                    WHERE oe-boll.company EQ oe-bolh.company
                    AND oe-boll.b-no    EQ oe-bolh.b-no
                    AND oe-boll.i-no    EQ oe-rell.i-no
                    AND oe-boll.ord-no  EQ oe-rell.ord-no
                    AND oe-boll.tag     EQ oe-rell.tag
                    USE-INDEX b-no:                

                       
                       
                    IF AVAIL oe-boll THEN 
                    DO:    
                            
                            
                        dLineWgt = 0.
                            
                        IF oe-rell.tag GT "" THEN 
                            FIND FIRST loadtag NO-LOCK 
                                WHERE loadtag.company = cCompany
                                AND LOADtag.item-type = NO
                                AND loadtag.tag-no  = oe-rell.tag NO-ERROR.
                             
                        IF bolWeight-log AND AVAIL loadtag           
                            THEN ASSIGN dLineWgt = wgtFromLoadtag(oe-rell.cases).
                            
                        IF dLineWgt EQ 0 THEN
                            ASSIGN dLineWgt = wgtFromItemfg(oe-rell.qty, oe-rell.i-no).
                            
                            
                        dTotWgt = dTotWgt + dLineWgt.
                            
                            
                    END. /* Avail oe-boll */   
                    
                    
                    
                END. /* each oe-rell */
                                
            
            END. /* Avail oe-relh */
            
            dReturnWeight = dTotWgt.

            
        END. /* when oe-relh */
        
    OTHERWISE
    RETURN.
    
    
END CASE.
opdWeight = dReturnWeight.
