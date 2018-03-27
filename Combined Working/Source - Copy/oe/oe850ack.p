
/*------------------------------------------------------------------------
    File        : oe/oe850ack.p
    Purpose     : 

    Syntax      :

    Description : Generate Order Acknowledgement to send SPS Commerce in XML format

    Author(s)   : 
    Created     : Wed Nov 09 19:32:08 EST 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


     
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

DEFINE input param ipOrderNumber AS INTEGER NO-UNDO.

DEFINE VARIABLE cPartner AS CHARACTER NO-UNDO.
DEFINE VARIABLE iSeq AS INTEGER  NO-UNDO.
DEFINE VARIABLE lReturn AS LOGICAL NO-UNDO.

FIND FIRST EDDoc where EDDoc.Unique-Order-No = ipOrderNumber NO-LOCK NO-ERROR.
IF NOT AVAILABLE EDDoc THEN DO:
  IF program-name(2) MATCHES "*edpo*" then  
     MESSAGE "EDI order is not available for sales order: " ipOrderNumber
       VIEW-AS ALERT-BOX ERROR.
   lReturn = NO. 
END.      
ELSE DO:
/*   IF NOT can-find(EDPOTran OF EDDoc WHERE EDPOTran.ack-date = ?) THEN DO:*/
/*      MESSAGE "ACK already generated. Do you want to re-generate it?"     */
/*      UPDATE llans AS log                                                 */
/*      VIEW-AS ALERT-BOX.                                                  */
/*      IF NOT llans THEN RETURN.                                           */
/*   END.                                                                   */
    
   ASSIGN cPartner = EDDoc.partner
          iSeq = EDDoc.seq
          .
   RUN edi/sp855xml.p (ipOrderNumber, cPartner, iSeq, OUTPUT lReturn).
   DO TRANSACTION:
      FIND EDPOTran OF EDDoc EXCLUSIVE-LOCK.
      ASSIGN EDPOTran.ack-date = TODAY. 
   END.
   
END.