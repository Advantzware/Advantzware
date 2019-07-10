/*------------------------------------------------------------------------
    File        : itemcust_look.p
    Purpose     : 
    Syntax      :       
    Description : Return a Dataset of UserMaintenance
    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttItemCustomerLookup NO-UNDO 
    FIELD ino            AS CHAR
    FIELD jobno          AS CHAR
    FIELD loc            AS CHAR
    FIELD locbin         AS CHAR
    FIELD tag            AS CHAR
    FIELD custno         AS CHAR
    FIELD selct          AS CHAR
    FIELD casout         AS INT
    FIELD cascnt         AS INT
    FIELD casunit        AS INT
    FIELD partial        AS INT
    FIELD qtyout         AS INT
    FIELD extra           AS CHAR
    .

DEFINE DATASET dsItemCustomerLookup FOR ttItemCustomerLookup .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmcust      AS CHARACTER  NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsItemCustomerLookup.
 
DEF VAR prmComp AS CHAR NO-UNDO.


IF prmAction    = ? THEN ASSIGN prmAction    = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmField     = ? THEN ASSIGN prmField     = "".
IF prmcust      = ? THEN ASSIGN prmcust      = "".


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

DEF TEMP-TABLE tt-binsel NO-UNDO LIKE fg-bin
    FIELD job# AS CHAR
    FIELD cases-out LIKE fg-bin.cases
    FIELD partial-out LIKE fg-bin.partial-count
    FIELD qty-out LIKE fg-bin.qty
    FIELD row-id AS ROWID
    FIELD tt-select AS LOG.

FIND oe-relh WHERE oe-relh.cust-no EQ prmcust NO-LOCK NO-ERROR.

  IF AVAIL oe-relh THEN DO:
    

    FOR EACH oe-ord NO-LOCK
        WHERE oe-ord.company EQ oe-relh.company
          AND oe-ord.cust-no EQ oe-relh.cust-no
          AND oe-ord.opened  EQ YES,
        EACH oe-ordl NO-LOCK
        WHERE oe-ordl.company  EQ oe-ord.company
          AND oe-ordl.ord-no   EQ oe-ord.ord-no
          AND oe-ordl.i-no     NE ""
          AND oe-ordl.qty      GT 0:

      IF NOT CAN-FIND(FIRST tt-binsel
                      WHERE tt-binsel.company EQ oe-ordl.company
                        AND tt-binsel.i-no    EQ oe-ordl.i-no) THEN
      FOR EACH fg-bin NO-LOCK
          WHERE fg-bin.company EQ oe-ordl.company
            AND fg-bin.i-no    EQ oe-ordl.i-no
            AND fg-bin.loc     NE ""
            AND fg-bin.loc-bin NE ""
            AND fg-bin.qty     GT 0:

        CREATE tt-binsel.
        BUFFER-COPY fg-bin TO tt-binsel
        ASSIGN
         tt-binsel.row-id      = ROWID(fg-bin)
         tt-binsel.job#        = TRIM(fg-bin.job-no) + "-" +
                                 STRING(fg-bin.job-no2,"99")
         tt-binsel.cases       = TRUNC((fg-bin.qty - fg-bin.partial-count) /
                                       fg-bin.case-count,0)
         tt-binsel.cases-out   = tt-binsel.cases
         tt-binsel.partial-out = tt-binsel.partial-count
         tt-binsel.qty-out     = tt-binsel.qty.

        IF TRIM(tt-binsel.job#) EQ "-00" THEN tt-binsel.job# = "".
      END.
    END.
  END.

if prmAction = "Select" then do:

    FOR EACH tt-binsel NO-LOCK:

        create ttItemCustomerLookup.
        assign
            ttItemCustomerLookup.ino        = tt-binsel.i-no          
            ttItemCustomerLookup.jobno      = tt-binsel.job#          
            ttItemCustomerLookup.loc        = tt-binsel.loc           
            ttItemCustomerLookup.locbin     = tt-binsel.loc-bin       
            ttItemCustomerLookup.tag        = tt-binsel.tag           
            ttItemCustomerLookup.custno     = tt-binsel.cust-no       
            ttItemCustomerLookup.casout     = tt-binsel.cases-out     
            ttItemCustomerLookup.cascnt     = tt-binsel.case-count    
            ttItemCustomerLookup.casunit    = tt-binsel.cases-unit    
            ttItemCustomerLookup.partial    = tt-binsel.partial-out   
            ttItemCustomerLookup.qtyout     = tt-binsel.qty-out     .
             

    END.  /*FOR EACH tt-binsel*/
END.  /*ifif prmAction <> "search" */

/******************Search***********************************/

IF prmAction = "Search" then do:
     if prmField = "item"  then do:
         if prmCondition = "EQUAL" then do:
              FOR EACH tt-binsel NO-LOCK:

                    create ttItemCustomerLookup.
                    assign
                        ttItemCustomerLookup.ino        = tt-binsel.i-no          
                        ttItemCustomerLookup.jobno      = tt-binsel.job#          
                        ttItemCustomerLookup.loc        = tt-binsel.loc           
                        ttItemCustomerLookup.locbin     = tt-binsel.loc-bin       
                        ttItemCustomerLookup.tag        = tt-binsel.tag           
                        ttItemCustomerLookup.custno     = tt-binsel.cust-no       
                        ttItemCustomerLookup.casout     = tt-binsel.cases-out     
                        ttItemCustomerLookup.cascnt     = tt-binsel.case-count    
                        ttItemCustomerLookup.casunit    = tt-binsel.cases-unit    
                        ttItemCustomerLookup.partial    = tt-binsel.partial-out   
                        ttItemCustomerLookup.qtyout     = tt-binsel.qty-out     .
                         

              END.  /*FOR EACH tt-binsel*/
         END. 
     END .  /* if prmField = state  */
END.  /* IF prmAction = search then do: */
