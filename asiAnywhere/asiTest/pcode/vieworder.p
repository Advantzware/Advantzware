
/*------------------------------------------------------------------------
    File        : vieworder.p
    Purpose     : vieworder

    Syntax      :

    Description : Return a Dataset of all order Inquiry

    Author(s)   : Sewa Singh
    Created     : Sep 12 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{vieworder.i}

DEFINE INPUT PARAMETER prmUser AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum AS char NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsvieworder.

DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.

IF prmUser = ? THEN ASSIGN prmUser = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".

DEF VAR prmComp AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

/* ********************  Preprocessor Definitions  ******************** */
RUN build-qry IN THIS-PROCEDURE (OUTPUT v-qry-string).
 
 ASSIGN
     v-qry-handle = QUERY q-vieworderQuery:HANDLE.
         
 v-qry-handle:QUERY-PREPARE(v-qry-string).
 
 DATASET dsvieworder:FILL().
 FOR EACH ttVorder :
    FIND FIRST oe-ord WHERE oe-ord.company = ttVorder.company
         AND oe-ord.ord-no = ttVorder.ord-no
         NO-LOCK NO-ERROR.
     IF NOT AVAILABLE oe-ord THEN NEXT.
    
        /* ttVorder.frt-pay-dscr = oe-ord.frt-pay
         ttVorder.fob-dscr     = oe-ord.fob-code*/
         
     IF  oe-ord.frt-pay  = "P" THEN
         ttVorder.frt-pay-dscr        = "Prepaid".
         ELSE IF oe-ord.frt-pay  = "C" THEN 
             ttVorder.frt-pay-dscr        = "Collect".
             ELSE IF oe-ord.frt-pay  = "B" THEN
                 ttVorder.frt-pay-dscr        = "Bill".
                 ELSE IF oe-ord.frt-pay  = "T" THEN
                     ttVorder.frt-pay-dscr        = "3rd Party".
     IF  oe-ord.fob-code  = "DEST" THEN
         ttVorder.fob-dscr        = "Destination".
         ELSE IF oe-ord.fob-code  = "ORIG" THEN 
             ttVorder.fob-dscr        = "Origin".
         
         FIND FIRST carrier WHERE carrier.company = oe-ord.company AND carrier.loc = oe-ord.loc
             AND carrier.carrier = oe-ord.carrier NO-LOCK NO-ERROR.
             assign                                     
                  ttVorder.Carrdscr       = carrier.dscr.  

     IF  oe-ord.type  = "O" THEN
         ttVorder.type        = "Original".
         ELSE IF oe-ord.type  = "R" THEN 
             ttVorder.type        = "Repeat".
         ELSE IF oe-ord.type  = "C" THEN 
             ttVorder.type        = "Repeat with Change".
         ELSE IF oe-ord.type  = "T" THEN 
             ttVorder.type        = "Transfer".
         
        
        RUN oe/getStatusDesc.p( INPUT oe-ord.stat, OUTPUT ttVorder.stat) .


            
 END.  /*FOR EACH ttVorder :*/
/* ***************************  Main Block  *************************** */

/* ***************************  Procedures  *************************** */
PROCEDURE build-qry:
    DEFINE OUTPUT PARAMETER prm-query AS CHARACTER NO-UNDO.
    IF prmOrderNum <> "" THEN
       ASSIGN
          prm-query = "FOR EACH oe-ord WHERE oe-ord.company eq " + QUOTER(prmComp) 
                    + " AND oe-ord.ord-no = " + STRING(prmOrderNum) + " NO-LOCK".
                                               
END PROCEDURE.
