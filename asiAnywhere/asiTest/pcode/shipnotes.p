


/*------------------------------------------------------------------------
    File        : shipnotes.p
    Purpose     : OrderItem

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : Aug 27 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{shipnotes.i}

DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum as Character no-undo.
DEFINE INPUT PARAMETER prmItemNum as Character no-undo.
DEFINE INPUT PARAMETER prmNote1 as Character no-undo.
DEFINE INPUT PARAMETER prmNote2 as Character no-undo.
DEFINE INPUT PARAMETER prmNote3 as Character no-undo.
DEFINE INPUT PARAMETER prmNote4 as Character no-undo.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsshipnotes.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.

IF prmUser     = ? THEN ASSIGN prmUser     = "".
IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".
IF prmItemNum = ? THEN ASSIGN prmItemNum = "".
IF prmNote1 = ? THEN ASSIGN prmNote1 = "".
IF prmNote2 = ? THEN ASSIGN prmNote2 = "".
IF prmNote3 = ? THEN ASSIGN prmNote3 = "".
IF prmNote4 = ? THEN ASSIGN prmNote4 = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

/* ********************  Preprocessor Definitions  ******************** */
CASE prmAction :
   WHEN "update" THEN DO:
      
      FOR EACH oe-ordl where
          oe-ordl.company EQ prmComp AND
          oe-ordl.ord-no = int(prmOrderNum) AND
          oe-ordl.i-no = prmItemNum
          NO-LOCK,
          FIRST oe-rel WHERE oe-rel.company = oe-ordl.company 
              AND oe-rel.i-no = oe-ordl.i-no
              AND oe-rel.cust-no = oe-ordl.cust-no
                  EXCLUSIVE-LOCK:
            ASSIGN
                oe-rel.ship-i[1]  = prmNote1 
                oe-rel.ship-i[2]  = prmNote2
                oe-rel.ship-i[3]  = prmNote3
                oe-rel.ship-i[4]  = prmNote4 .
      END.
     
     END.
END CASE.
/*************************************************************************/
FOR EACH oe-ordl where
    oe-ordl.company EQ prmComp AND
    oe-ordl.ord-no = int(prmOrderNum) AND
    oe-ordl.i-no = prmItemNum
    NO-LOCK,
    FIRST oe-rel WHERE oe-rel.company = oe-ordl.company 
                   AND oe-rel.i-no = oe-ordl.i-no
                   AND oe-rel.cust-no = oe-ordl.cust-no
                   NO-LOCK:
    
        create ttshipnotes.
        assign 
            ttshipnotes.i-no         = oe-ordl.i-no
            ttshipnotes.i-name       = oe-ordl.i-name
            ttshipnotes.po-no        = oe-rel.po-no
            ttshipnotes.rel-date     = oe-rel.rel-date
            ttshipnotes.SNote1       = oe-rel.ship-i[1] 
            ttshipnotes.SNote2       = oe-rel.ship-i[2]
            ttshipnotes.SNote3       = oe-rel.ship-i[3]
            ttshipnotes.SNote4       = oe-rel.ship-i[4]
  .
END.   /*FOR EACH oe-ordl*/

FIND FIRST ttshipnotes NO-LOCK NO-ERROR.
IF NOT AVAIL ttshipnotes THEN
    CREATE ttshipnotes .

    




