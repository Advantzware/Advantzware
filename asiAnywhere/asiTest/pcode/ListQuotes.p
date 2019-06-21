




/*------------------------------------------------------------------------
    File        : ListQuotes.p
    Purpose     : Quote Maintenance

    Syntax      :

    Description : Return a Dataset of Quote Maintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{ListQuotes.i}
DEFINE INPUT PARAMETER prmUser      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmQuote     as INTEGER  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsListQuotes.

DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
 DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.
DEFINE VARIABLE prmComp        AS CHARACTER NO-UNDO.
DEFINE VARIABLE q-noValue1  AS INT NO-UNDO.
DEFINE VARIABLE q-noValue2  AS INT NO-UNDO.
IF prmUser = ? THEN prmUser = "".
IF prmAction = ? THEN ASSIGN prmAction = "".
IF prmAction = "" THEN ASSIGN prmAction = "Select".
IF prmQuote = ? THEN ASSIGN prmQuote = 0.
/* ********************  Preprocessor Definitions  ******************** */


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = "001".
 /*prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".*/
MESSAGE "jyoti" prmAction prmComp prmQuote .
IF prmAction = "Select" THEN DO:
   FIND FIRST quotehd WHERE quotehd.company = prmComp AND quotehd.q-no = prmQuote NO-LOCK NO-ERROR.
  FOR EACH quoteitm WHERE quoteitm.company = quotehd.company AND quoteitm.q-no = quotehd.q-no  NO-LOCK :
       FOR EACH ar-invl WHERE ar-invl.company = quotehd.company AND ar-invl.part-no = quoteitm.part-no AND ar-invl.inv-qty = quoteitm.qty NO-LOCK :
           FOR EACH itemfg WHERE itemfg.company = ar-invl.company
                             AND itemfg.i-no    = ar-invl.i-no NO-LOCK :
                CREATE ttListQuotes.
                ASSIGN
                    ttListQuotes.vQuote        = quotehd.q-no
                    ttListQuotes.vPart         = quoteitm.part-no
                    ttListQuotes.vUnit         = ar-invl.unit-pr 
                    ttListQuotes.vQty          = ar-invl.inv-qty                      
                    ttListQuotes.vUom          = ar-invl.pr-qty-uom                   
                    ttListQuotes.vitem         = itemfg.i-no
                    ttListQuotes.vName         = itemfg.i-name                        
                    ttListQuotes.vdscr         = itemfg.i-dscr
                    ttListQuotes.Style         = itemfg.style
                    ttListQuotes.vLen          = STRING(itemfg.l-score[50]) +  "x" +

                                                     STRING(itemfg.w-score[50]) +  "x" +
                                                     STRING(itemfg.d-score[50]) .
            MESSAGE "ttListQuotes" ttListQuotes.vUnit.
           END.
           END.
     END.
END.   /*IF prmAction = "select" THEN DO:*/

/*********************************************************************************/



