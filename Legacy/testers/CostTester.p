
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
DEFINE VARIABLE hdCostProcs AS HANDLE.
DEFINE NEW SHARED VARIABLE cocode AS CHARACTER NO-UNDO INIT '001'.
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
RUN system\CostProcs.p PERSISTENT SET hdCostProcs.

DEFINE VARIABLE dResultNew AS DECIMAL.
DEFINE VARIABLE dResultOld AS DECIMAL.

DEFINE VARIABLE cFromUOM AS CHARACTER INIT "LF".
DEFINE VARIABLE cToUOM AS CHARACTER INIT "MSF".
DEFINE VARIABLE dBasis AS DECIMAL INIT .24.
DEFINE VARIABLE dLen AS DECIMAL INIT 12.
DEFINE VARIABLE dWid AS DECIMAL INIT 22.
DEFINE VARIABLE dDep AS DECIMAL INIT 2.
DEFINE VARIABLE dValueToConvert AS DECIMAL INIT 100.
DEFINE VARIABLE dCostPerUOM AS DECIMAL.
DEFINE VARIABLE cCostUOM AS CHARACTER.
DEFINE VARIABLE dCostFreight AS DECIMAL.


/*dResultNew = DYNAMIC-FUNCTION('fConvert' IN hdCostProcs,cFromUOM,cToUOM,dBasis,dLen,dWid,dDep,dValueToConvert).*/
/*RUN rm/convcuom.p(cFromUOM, cToUOM, dBasis, dLen, dWid, dDep, dValueToConvert, OUTPUT dResultOld).            */
/*MESSAGE "New: " dResultNew SKIP                                                                               */
/*"Old: " dResultOld                                                                                            */
/*VIEW-AS ALERT-BOX.                                                                                            */

RUN GetCostForPOLine IN hdCostProcs (cocode, 104012, 1, OUTPUT dCostPerUOM, OUTPUT cCostUOM, OUTPUT dCostFreight).

MESSAGE 1 "Cost: " dCostPerUOM SKIP 
"UOM: " cCostUOM SKIP 
"Cost Per UOM Freight:" dCostFreight
VIEW-AS ALERT-BOX .

RUN GetCostForPOLine IN hdCostProcs (cocode, 104012, 2, OUTPUT dCostPerUOM, OUTPUT cCostUOM, OUTPUT dCostFreight).

MESSAGE 2 "Cost: " dCostPerUOM SKIP 
"UOM: " cCostUOM SKIP 
"Cost Per UOM Freight:" dCostFreight
VIEW-AS ALERT-BOX .

RUN GetCostForPOLine IN hdCostProcs (cocode, 104012, 3, OUTPUT dCostPerUOM, OUTPUT cCostUOM, OUTPUT dCostFreight).

MESSAGE 3 "Cost: " dCostPerUOM SKIP 
"UOM: " cCostUOM SKIP 
"Cost Per UOM Freight:" dCostFreight
VIEW-AS ALERT-BOX .
