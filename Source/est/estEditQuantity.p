
/*------------------------------------------------------------------------
    File        : estEditQuantity.p
    Purpose     : To edit the estimate quantities before calculation

    Syntax      :

    Description : Estimate Edit Quantity.

    Author(s)   : 
    Created     : 05/25/2022
    Notes       :
  ----------------------------------------------------------------------*/

/* ***********************Parameter Definitions  ********************* */
DEFINE INPUT PARAMETER ipriEB AS RECID NO-UNDO.

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE griEstQty AS RECID NO-UNDO.
DEFINE VARIABLE giEQty LIKE est-qty.eqty NO-UNDO.
DEFINE VARIABLE char-val AS CHARACTER NO-UNDO. 
DEFINE VARIABLE char-val2 AS CHARACTER NO-UNDO.        
DEFINE VARIABLE date-val AS CHARACTER NO-UNDO.
DEFINE VARIABLE date-val2 AS CHARACTER NO-UNDO.
DEFINE BUFFER buf-eb FOR eb.
DEFINE BUFFER buf-ef FOR ef.
DEFINE BUFFER buf-est FOR est.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
RUN pCallUIToUpdate.


/* **********************  Internal Procedures  *********************** */

PROCEDURE pCallUIToUpdate PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Call UI for updating quantities for estimate
 Notes:
------------------------------------------------------------------------------*/
FIND FIRST buf-eb NO-LOCK 
    WHERE RECID(buf-eb) EQ ipriEB NO-ERROR.

IF buf-eb.est-type LE 6 THEN 
DO:
    
FIND FIRST est-qty NO-LOCK 
WHERE est-qty.company EQ buf-eb.company  
  AND est-qty.est-no EQ  buf-eb.est-no NO-ERROR.
   
griEstQty = IF AVAIL est-qty THEN RECID(est-qty) ELSE ?.

RUN est/estqtyd.w (griEstQty, ipriEB, STRING (est-qty.eqty), OUTPUT char-val, OUTPUT char-val2, OUTPUT date-val, OUTPUT date-val2).
FIND FIRST buf-est NO-LOCK 
    WHERE buf-est.company EQ buf-eb.company
      AND buf-est.est-no EQ buf-eb.est-no NO-ERROR.
      
    IF char-val NE "?" OR char-val2 NE "?" THEN 
    DO:
        FIND CURRENT buf-est NO-ERROR.
        ASSIGN
            buf-est.est-qty[1] = INT(ENTRY(1,char-val))
            buf-est.est-qty[2] = INT(ENTRY(2,char-val))
            buf-est.est-qty[3] = INT(ENTRY(3,char-val))
            buf-est.est-qty[4] = INT(ENTRY(4,char-val)).
        FIND CURRENT buf-est NO-LOCK NO-ERROR.
    
        giEQty = est-qty.eqty.
        FIND CURRENT est-qty NO-ERROR.
        est-qty.eqty = INT(ENTRY(1,char-val)).
        FIND CURRENT est-qty NO-LOCK NO-ERROR.
    
        FOR EACH buf-eb
            WHERE buf-eb.company EQ buf-est.company
            AND buf-eb.est-no  EQ buf-est.est-no
            AND buf-eb.eqty    EQ giEQty
            AND buf-eb.form-no NE 0:
            buf-eb.eqty = INT(ENTRY(1,char-val)).
        END.
     
        FOR EACH buf-ef
            WHERE buf-ef.company EQ buf-est.company
            AND buf-ef.est-no  EQ buf-est.est-no
            AND buf-ef.eqty    EQ giEQty:
            buf-ef.eqty = INT(ENTRY(1,char-val)).
        END.
    END. 
END. 
ELSE 
MESSAGE "Edit-Quantity is not applicable for combo estimates"
VIEW-AS ALERT-BOX. 
 
END PROCEDURE.
