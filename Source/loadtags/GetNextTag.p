
/*------------------------------------------------------------------------
    File        : GetNextTag.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : BV
    Created     : Wed Nov 27 12:10:58 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcItemNo AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcFullTag AS CHARACTER NO-UNDO.

DEFINE VARIABLE iNextTagNum AS INTEGER NO-UNDO.


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

RUN GetNextLoadtagNumber (ipcCompany, ipcItemNo, OUTPUT iNextTagNum).
IF iNextTagNum GT 99999 THEN 
  opcFullTag = STRING(CAPS(ipcItemNo),"x(14)") + STRING(iNextTagNum,"999999").
ELSE 
  opcFullTag = STRING(CAPS(ipcItemNo),"x(15)") + STRING(iNextTagNum,"99999").

PROCEDURE GetNextLoadtagNumber PRIVATE :

DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcFGItemID AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opiNextTag AS INTEGER NO-UNDO.

DEFINE VARIABLE iLastFGTag AS INTEGER.
DEFINE VARIABLE iLastRMTag AS INTEGER.

FIND LAST loadtag NO-LOCK
      WHERE loadtag.company     EQ ipcCompany
        AND loadtag.item-type   EQ NO
        AND loadtag.is-case-tag EQ NO
        AND loadtag.tag-no      BEGINS ipcFGItemID 
        AND SUBSTR(loadtag.tag-no,1,15) EQ ipcFGItemID
      USE-INDEX tag NO-ERROR.
  iLastFGTag = (IF AVAIL loadtag THEN INT(SUBSTR(loadtag.tag-no,16,5)) ELSE 0) + 1.
  
IF LENGTH(TRIM(ipcFGItemID)) LT 15 AND iLastFGTag GE 100000 THEN DO: 
    FIND LAST loadtag NO-LOCK
          WHERE loadtag.company     EQ ipcCompany
            AND loadtag.item-type   EQ NO
            AND loadtag.is-case-tag EQ NO
            AND loadtag.tag-no      BEGINS ipcFGItemID 
            AND SUBSTR(loadtag.tag-no,1,14) EQ ipcFGItemID
          USE-INDEX tag NO-ERROR.
    iLastFGTag = (IF AVAIL loadtag THEN INT(SUBSTR(loadtag.tag-no,15,6)) ELSE 0) + 1.          
END.
  
FIND LAST loadtag NO-LOCK
      WHERE loadtag.company     EQ ipcCompany
        AND loadtag.item-type   EQ YES
        AND loadtag.is-case-tag EQ NO
        AND loadtag.tag-no      BEGINS ipcFGItemID 
        AND SUBSTR(loadtag.tag-no,1,15) EQ ipcFGItemID
      USE-INDEX tag NO-ERROR.
      
      
  iLastRMTag = (IF AVAIL loadtag THEN INT(SUBSTR(loadtag.tag-no,16,5)) ELSE 0) + 1.
 IF LENGTH(TRIM(ipcFGItemID)) LT 15 AND iLastRMTag GE 100000 THEN DO: 
    FIND LAST loadtag NO-LOCK
          WHERE loadtag.company     EQ ipcCompany
            AND loadtag.item-type   EQ YES
            AND loadtag.is-case-tag EQ NO
            AND loadtag.tag-no      BEGINS ipcFGItemID 
            AND SUBSTR(loadtag.tag-no,1,14) EQ ipcFGItemID
          USE-INDEX tag NO-ERROR.
    iLastRMTag = (IF AVAIL loadtag THEN INT(SUBSTR(loadtag.tag-no,15,6)) ELSE 0) + 1.          
END. 
  opiNextTag = MAX(iLastFGTag, iLastRMTag).

END PROCEDURE.