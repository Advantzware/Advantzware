            /*------------------------------------------------------------------------
    File        : ItemHistory.p
    Purpose     : Cust Part

    Syntax      :

    Description : Return a Dataset of all item

    Author(s)   : 
    Created     : Feb 23 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttItemHistDisplay NO-UNDO 
    FIELD hisItem       AS CHAR
    FIELD hisDate       AS DATE
    FIELD hisName       AS CHAR
    FIELD hisDesc       AS CHAR
    FIELD hisCost       AS DECIMAL
    FIELD hisSell       AS DECIMAL
    FIELD hisUom        AS CHAR
    FIELD hisQuantity       AS INT
    FIELD hisCust       AS CHAR .

DEFINE DATASET dsItemHistDisplay FOR ttItemHistDisplay.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmRowId     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsItemHistDisplay.
       
DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-hide-cost AS LOG NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction  = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmRowId = ? THEN ASSIGN prmRowId = "".
IF prmText = ? THEN ASSIGN prmText = "".



FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
def NEW SHARED var cocode     as   char  format "x(3)"  no-undo.
def NEW SHARED var locode     as   char  format "x(5)"  no-undo.
ASSIGN
    cocode = prmComp
    locode = "MAIN" .
DO TRANSACTION:
   {sys/inc/fgsecur.i}
END.

IF fgsecurity-log THEN
DO:
   FIND FIRST usergrps WHERE
        usergrps.usergrps = fgsecurity-char
        NO-LOCK NO-ERROR.

   IF AVAIL usergrps AND
      (NOT CAN-DO(usergrps.users,USERID("NOSWEAT")) AND
       TRIM(usergrps.users) NE "*") THEN
      ASSIGN
         v-hide-cost = YES.
END.
MESSAGE "v-hide-cost" v-hide-cost.

FUNCTION f-cost RETURNS DECIMAL
  ( /* parameter-definitions */ ) :
  DEF VAR f-cost AS DECIMAL NO-UNDO.

  IF v-hide-cost = YES THEN
     f-cost = oe-ordl.cost .

  RETURN f-cost.

END FUNCTION.

if prmAction = "History" then do:

    FIND FIRST oe-ordl WHERE oe-ordl.rec_key  EQ prmRowId NO-LOCK NO-ERROR.
    IF AVAIL oe-ordl THEN DO:
        FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.
    IF AVAIL oe-ord THEN
        FIND FIRST cust OF oe-ord NO-LOCK NO-ERROR.
        IF AVAIL cust THEN DO:
             FOR EACH oe-ord OF cust NO-LOCK,
                 EACH oe-ordl OF oe-ord NO-LOCK
                    WHERE oe-ordl.qty NE 0 BREAK BY oe-ordl.i-no BY oe-ord.ord-date BY oe-ord.ord-no:
                   IF LAST-OF(oe-ordl.i-no) THEN DO:
                    create ttItemHistDisplay.
                    assign                                         
                        ttItemHistDisplay.hisItem         =  oe-ordl.i-no  
                        ttItemHistDisplay.hisDate         =  oe-ord.ord-date
                        ttItemHistDisplay.hisName         =  oe-ordl.i-name  
                        ttItemHistDisplay.hisDesc         =  oe-ordl.part-dscr1
                        ttItemHistDisplay.hisCost         =  f-cost()   
                        ttItemHistDisplay.hisSell         =  oe-ordl.price
                        ttItemHistDisplay.hisUom          =  oe-ordl.pr-uom  
                        ttItemHistDisplay.hisQuantity     =  oe-ordl.qty
                        ttItemHistDisplay.hisCust     =  oe-ord.cust-no
               
                 .
                   END.
        END.
        
    END.	 /* FOR EACH item */  
    END.  /*avail ord*/
    
END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "search" then do:
    FIND FIRST oe-ordl WHERE oe-ordl.rec_key  EQ prmRowId NO-LOCK NO-ERROR.
    IF AVAIL oe-ordl THEN DO:
        FIND FIRST oe-ord OF oe-ordl NO-LOCK NO-ERROR.
    IF AVAIL oe-ord THEN
        FIND FIRST cust OF oe-ord NO-LOCK NO-ERROR.
        IF AVAIL cust THEN DO:
             FOR EACH oe-ord OF cust NO-LOCK,
                 EACH oe-ordl OF oe-ord NO-LOCK
                    WHERE oe-ordl.qty NE 0 AND (oe-ordl.i-no BEGINS prmText OR prmText = "" ) BREAK BY oe-ordl.i-no BY oe-ord.ord-date BY oe-ord.ord-no:
                   IF LAST-OF(oe-ordl.i-no) THEN DO:
                    create ttItemHistDisplay.
                    assign                                         
                        ttItemHistDisplay.hisItem         =  oe-ordl.i-no  
                        ttItemHistDisplay.hisDate         =  oe-ord.ord-date
                        ttItemHistDisplay.hisName         =  oe-ordl.i-name  
                        ttItemHistDisplay.hisDesc         =  oe-ordl.part-dscr1
                        ttItemHistDisplay.hisCost         =  f-cost()   
                        ttItemHistDisplay.hisSell         =  oe-ordl.price
                        ttItemHistDisplay.hisUom          =  oe-ordl.pr-uom  
                        ttItemHistDisplay.hisQuantity     =  oe-ordl.qty
                        ttItemHistDisplay.hisCust     =  oe-ord.cust-no
               
                 .
                   END.
        END.
        
    END.	 /* FOR EACH item */   
    END.  /*avail oe-ordl*/
   
END.  /* IF prmAction = search then do: */



