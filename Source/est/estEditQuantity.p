
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






/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
RUN pCallUIToUpdate.


/* **********************  Internal Procedures  *********************** */

PROCEDURE pCallUIToUpdate PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Call UI for updating quantities for estimate
 Notes:
------------------------------------------------------------------------------*/
DEF VAR lv-estqty-recid AS RECID NO-UNDO.
DEF VAR lv-hld-eqty LIKE est-qty.eqty NO-UNDO.
DEF VAR char-val AS CHAR NO-UNDO. 
DEF VAR char-val2 AS CHAR NO-UNDO.        
DEF VAR date-val AS CHAR NO-UNDO.
DEF VAR date-val2 AS CHAR NO-UNDO.
DEFINE BUFFER buf-eb FOR eb.
DEFINE BUFFER buf-ef FOR ef.
DEFINE BUFFER buf-est FOR est.

FIND FIRST buf-eb NO-LOCK 
    WHERE RECID(buf-eb) EQ ipriEB NO-ERROR.

FIND FIRST est-qty NO-LOCK 
WHERE est-qty.company EQ buf-eb.company  
  AND est-qty.est-no EQ  buf-eb.est-no NO-ERROR.
   
lv-estqty-recid = IF AVAIL est-qty THEN RECID(est-qty) ELSE ?.

RUN est/estqtyd.w (lv-estqty-recid, ipriEB, STRING (est-qty.eqty), OUTPUT char-val, OUTPUT char-val2, OUTPUT date-val, OUTPUT date-val2).
FIND FIRST buf-est NO-LOCK 
    WHERE buf-est.company EQ buf-eb.company
      AND buf-est.est-no EQ buf-eb.est-no NO-ERROR.
      
      MESSAGE buf-est.est-qty[1] SKIP buf-est.est-qty[2] SKIP buf-est.est-qty[3] SKIP buf-est.est-qty[4]
      VIEW-AS ALERT-BOX. 

END PROCEDURE.





