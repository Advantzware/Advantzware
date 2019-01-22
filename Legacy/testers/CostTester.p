
/*------------------------------------------------------------------------
    File        : CostTester.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : BV
    Created     : Tue Jan 22 00:06:51 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE hCostProcs AS HANDLE.
DEFINE NEW SHARED VARIABLE cocode AS CHARACTER NO-UNDO INIT '001'.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
RUN system\CostProcs.p PERSISTENT SET hCostProcs.

DEFINE VARIABLE dResultNew AS DECIMAL.
DEFINE VARIABLE dResultOld AS DECIMAL.

DEFINE VARIABLE cFromUOM AS CHARACTER INIT "LF".
DEFINE VARIABLE cToUOM AS CHARACTER INIT "MSF".
DEFINE VARIABLE dBasis AS DECIMAL INIT .24.
DEFINE VARIABLE dLen AS DECIMAL INIT 12.
DEFINE VARIABLE dWid AS DECIMAL INIT 22.
DEFINE VARIABLE dDep AS DECIMAL INIT 2.
DEFINE VARIABLE dValueToConvert AS DECIMAL INIT 100.


dResultNew = DYNAMIC-FUNCTION('fConvert' IN hCostProcs,cFromUOM,cToUOM,dBasis,dLen,dWid,dDep,dValueToConvert).
RUN rm/convcuom.p(cFromUOM, cToUOM, dBasis, dLen, dWid, dDep, dValueToConvert, OUTPUT dResultOld).
MESSAGE "New: " dResultNew SKIP 
"Old: " dResultOld
VIEW-AS ALERT-BOX.