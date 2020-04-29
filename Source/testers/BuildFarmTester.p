
/*------------------------------------------------------------------------
    File        : BuildFarmTester.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Sun Apr 26 22:29:30 EDT 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE ghSession AS HANDLE.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
RUN system\session.p PERSISTENT SET ghSession.
SESSION:ADD-SUPER-PROCEDURE (ghSession).
FIND FIRST eb NO-LOCK 
    WHERE eb.company EQ '001'
    AND eb.est-no EQ '  100366'
    NO-ERROR.
FOR EACH vendItemCost NO-LOCK 
    WHERE vendItemCost.company EQ eb.company
    AND vendItemCost.estimateNo EQ eb.est-no
    ,
    EACH vendItemCostLevel  
        WHERE vendItemCostLevel.vendItemCostID EQ vendItemCost.vendItemCostID:
     DELETE vendItemCostLevel.
 END. 

IF AVAILABLE eb THEN     
RUN est\BuildFarmForLogistics.p (ROWID(eb)).
FOR EACH vendItemCost NO-LOCK 
    WHERE vendItemCost.company EQ eb.company
    AND vendItemCost.estimateNo EQ eb.est-no
    ,
    EACH vendItemCostLevel NO-LOCK 
        WHERE vendItemCostLevel.vendItemCostID EQ vendItemCost.vendItemCostID:
     DISPLAY vendItemCostLevel.
 END. 