/*------------------------------------------------------------------------
    File        : Lpofglook.p
    Purpose     : PO#
    Syntax      :       
    Description : Return a Dataset of UserMaintenance
    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttLpofgLookup NO-UNDO 
    FIELD vPoNo           AS INT
    FIELD vOrdQty         AS DEC
    FIELD vINo            AS CHARACTER
    FIELD vIName          AS CHARACTER
    FIELD vJobNo          AS CHARACTER
    FIELD vJob2           AS INT
    FIELD vVendINo        AS CHARACTER 
    FIELD vVendNo         AS CHARACTER
    FIELD vSWid           AS DEC
    FIELD vSLen           AS DEC
    FIELD vDefLoc         AS CHARACTER
    FIELD vDefLocBin      AS CHAR
    FIELD vCaseCount      AS INT
    FIELD vIFgINo         AS CHARACTER
    FIELD vIFgIName       AS CHAR
    FIELD vIFgDefLoc      AS CHAR
    FIELD vIFgDefLocBin   AS CHARACTER
    FIELD vIFgAvgCost     AS INT
    FIELD vIFGProdUom     AS CHAR
    FIELD vPrUom          AS CHAR
    FIELD vStdCost        AS DEC
    FIELD lpo AS CHAR
    FIELD totqty    AS CHAR
    FIELD extcost   AS DECIMAL
    FIELD unit      AS CHAR .

DEFINE DATASET dsLpofgLookup FOR ttLpofgLookup .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsLpofgLookup.
   
DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR lv-cost AS DEC DECIMALS 4 NO-UNDO.
DEF VAR lv-loc AS CHAR NO-UNDO.
DEF VAR lv-loc-bin AS CHAR NO-UNDO.
DEF VAR lv-std-cost AS CHAR NO-UNDO.
DEF VAR v-cost AS DEC DECIMALS 10 NO-UNDO.
DEF VAR ll-ea AS LOG NO-UNDO.
DEF VAR lv-cost-uom AS CHAR NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction    = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmField     = ? THEN ASSIGN prmField     = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction = "PoSelect" then do:


    FOR EACH po-ordl WHERE po-ordl.company eq prmComp AND
        po-ordl.deleted = no and 
        po-ordl.opened eq yes and 
        po-ordl.item-type = no NO-LOCK, 
    FIRST po-ord WHERE po-ord.company eq po-ordl.company AND
        po-ord.po-no eq po-ordl.po-no AND
        po-ord.stat ne "H" NO-LOCK:

    lv-cost = po-ordl.cost * (IF po-ordl.disc NE 0 THEN (1 - (po-ordl.disc / 100)) ELSE 1).  

              create ttLpofgLookup.
                      assign
                           ttLpofgLookup.vPoNo      = po-ordl.po-no
                           ttLpofgLookup.vOrdQty    = po-ordl.ord-qty
                           ttLpofgLookup.vINo       = po-ordl.i-no
                           ttLpofgLookup.vIName     = po-ordl.i-name
                           ttLpofgLookup.vJobNo     = po-ordl.job-no
                           ttLpofgLookup.vJob2      = po-ordl.job-no2
                           ttLpofgLookup.vVendINo   = po-ordl.vend-i-no
                           ttLpofgLookup.vVendNo    = po-ordl.vend-no
                           ttLpofgLookup.vSWid      = po-ordl.s-wid
                           ttLpofgLookup.vSLen      = po-ordl.s-len
                           ttLpofgLookup.vPrUom     = po-ordl.pr-uom
                           ttLpofgLookup.totqty     = string(po-ordl.ord-qty)
                           ttLpofgLookup.vStdCost   = lv-cost .

        find first itemfg where itemfg.company = po-ordl.company and
                                        itemfg.i-no = po-ordl.i-no
                                        no-lock no-error.
        IF AVAIL itemfg THEN
        ASSIGN 
                 ttLpofgLookup.vDefLoc      = itemfg.def-loc
                 ttLpofgLookup.vDefLocBin   = itemfg.def-loc-bin
                 ttLpofgLookup.vCaseCount   = itemfg.case-count .

         ASSIGN ttLpofgLookup.unit     = string(TRUNC(((po-ordl.ord-qty) / ttLpofgLookup.vCaseCount),0))
                ttLpofgLookup.extcost = po-ordl.ord-qty - (INT(ttLpofgLookup.unit) * INT (ttLpofgLookup.vCaseCount))  .

    END.  /*FOR EACH po-ordl*/
END.  /*ifif prmAction <> "search" */

/******************Search***********************************/
 
IF prmAction = "PoSearch" then do:
     if prmField = "PO#"  then do:
         if prmCondition = "EQUAL" then do:
             
             FOR EACH po-ordl WHERE po-ordl.company = prmComp AND
                      po-ordl.deleted = no and 
                      po-ordl.opened eq yes and 
                      po-ordl.item-type = NO AND po-ordl.po-no = INT(prmText) NO-LOCK:
                 lv-cost = po-ordl.cost * (IF po-ordl.disc NE 0 THEN (1 - (po-ordl.disc / 100)) ELSE 1).  
                 create ttLpofgLookup.
                      assign
                           ttLpofgLookup.vPoNo      = po-ordl.po-no
                           ttLpofgLookup.vOrdQty    = po-ordl.ord-qty
                           ttLpofgLookup.vINo       = po-ordl.i-no
                           ttLpofgLookup.vIName     = po-ordl.i-name
                           ttLpofgLookup.vJobNo     = po-ordl.job-no
                           ttLpofgLookup.vJob2      = po-ordl.job-no2
                           ttLpofgLookup.vVendINo   = po-ordl.vend-i-no
                           ttLpofgLookup.vVendNo    = po-ordl.vend-no
                           ttLpofgLookup.vSWid      = po-ordl.s-wid
                           ttLpofgLookup.vSLen      = po-ordl.s-len 
                           ttLpofgLookup.vPrUom     = po-ordl.pr-uom
                           ttLpofgLookup.totqty     = string(po-ordl.ord-qty)
                           ttLpofgLookup.vStdCost   = lv-cost .

        find first itemfg where itemfg.company = po-ordl.company and
                                        itemfg.i-no = po-ordl.i-no
                                        no-lock no-error.
        IF AVAIL itemfg THEN
        ASSIGN 
                 ttLpofgLookup.vDefLoc      = itemfg.def-loc
                 ttLpofgLookup.vDefLocBin   = itemfg.def-loc-bin
                 ttLpofgLookup.vCaseCount   = itemfg.case-count.

        ASSIGN ttLpofgLookup.unit     = string(TRUNC(((po-ordl.ord-qty) / ttLpofgLookup.vCaseCount),0))
                ttLpofgLookup.extcost = po-ordl.ord-qty - (INT(ttLpofgLookup.unit) * INT (ttLpofgLookup.vCaseCount))  .
             END.
         END. /*FOR EACH state*/

          
     END .  /* if prmField = state  */


     if prmField = "Item"  then do:
          if prmCondition = "EQUAL" then do:
              FOR EACH po-ordl WHERE po-ordl.company = prmComp 
                  AND po-ordl.deleted = no 
                  AND po-ordl.opened eq yes 
                  AND po-ordl.item-type = NO 
                  AND po-ordl.i-no = prmText  NO-LOCK :
                    lv-cost = po-ordl.cost * (IF po-ordl.disc NE 0 THEN (1 - (po-ordl.disc / 100)) ELSE 1).

                  create ttLpofgLookup.
                      assign
                           ttLpofgLookup.vPoNo      = po-ordl.po-no
                           ttLpofgLookup.vOrdQty    = po-ordl.ord-qty
                           ttLpofgLookup.vINo       = po-ordl.i-no
                           ttLpofgLookup.vIName     = po-ordl.i-name
                           ttLpofgLookup.vJobNo     = po-ordl.job-no
                           ttLpofgLookup.vJob2      = po-ordl.job-no2
                           ttLpofgLookup.vVendINo   = po-ordl.vend-i-no
                           ttLpofgLookup.vVendNo    = po-ordl.vend-no
                           ttLpofgLookup.vSWid      = po-ordl.s-wid
                           ttLpofgLookup.vSLen      = po-ordl.s-len  
                           ttLpofgLookup.vPrUom     = po-ordl.pr-uom
                           ttLpofgLookup.totqty     = string(po-ordl.ord-qty)
                           ttLpofgLookup.vStdCost   = lv-cost .

        find first itemfg where itemfg.company = po-ordl.company and
                                        itemfg.i-no = po-ordl.i-no
                                        no-lock no-error.
        IF AVAIL itemfg THEN
        ASSIGN 
                 ttLpofgLookup.vDefLoc      = itemfg.def-loc
                 ttLpofgLookup.vDefLocBin   = itemfg.def-loc-bin
                 ttLpofgLookup.vCaseCount   = itemfg.case-count.

                ASSIGN ttLpofgLookup.unit     = string(TRUNC(((po-ordl.ord-qty) / ttLpofgLookup.vCaseCount),0))
                ttLpofgLookup.extcost = po-ordl.ord-qty - (INT(ttLpofgLookup.unit) * INT (ttLpofgLookup.vCaseCount))  .

              END. /*FOR EACH po-ordl */
          END. /*FOR EACH prmcondition*/

          IF prmCondition = "BEGIN" then do:
              FOR EACH po-ordl WHERE po-ordl.company = prmComp   AND
                  po-ordl.deleted = no and 
                  po-ordl.opened eq yes and 
                   po-ordl.item-type = NO AND
                 po-ordl.i-no BEGINS prmText NO-LOCK :

                  lv-cost = po-ordl.cost * (IF po-ordl.disc NE 0 THEN (1 - (po-ordl.disc / 100)) ELSE 1).
                  create ttLpofgLookup.
                      assign
                           ttLpofgLookup.vPoNo      = po-ordl.po-no
                           ttLpofgLookup.vOrdQty    = po-ordl.ord-qty
                           ttLpofgLookup.vINo       = po-ordl.i-no
                           ttLpofgLookup.vIName     = po-ordl.i-name
                           ttLpofgLookup.vJobNo     = po-ordl.job-no
                           ttLpofgLookup.vJob2      = po-ordl.job-no2
                           ttLpofgLookup.vVendINo   = po-ordl.vend-i-no
                           ttLpofgLookup.vVendNo    = po-ordl.vend-no
                           ttLpofgLookup.vSWid      = po-ordl.s-wid
                           ttLpofgLookup.vSLen      = po-ordl.s-len 
                           ttLpofgLookup.vPrUom     = po-ordl.pr-uom
                           ttLpofgLookup.totqty     = string(po-ordl.ord-qty)
                           ttLpofgLookup.vStdCost   = lv-cost .


        find first itemfg where itemfg.company = po-ordl.company and
                                        itemfg.i-no = po-ordl.i-no
                                        no-lock no-error.
        IF AVAIL itemfg THEN
        ASSIGN 
                 ttLpofgLookup.vDefLoc      = itemfg.def-loc
                 ttLpofgLookup.vDefLocBin   = itemfg.def-loc-bin
                 ttLpofgLookup.vCaseCount   = itemfg.case-count.

                ASSIGN ttLpofgLookup.unit     = string(TRUNC(((po-ordl.ord-qty) / ttLpofgLookup.vCaseCount),0))
                ttLpofgLookup.extcost = po-ordl.ord-qty - (INT(ttLpofgLookup.unit) * INT (ttLpofgLookup.vCaseCount))  .

              END.  /*FOR EACH po-ordl*/
          END.    /*if prmCondition = BEGIN*/    
     END.  /* if prmField = state  */
END.  /* IF prmAction = search then do: */

DEF VAR ll-new-file AS LOG NO-UNDO.
def VAR ip-cust-no like itemfg.cust-no no-undo.
DEF VAR lv-cust-no LIKE itemfg.cust-no NO-UNDO.
DEF VAR lv-part-no LIKE itemfg.part-no NO-UNDO.

ll-new-file = CAN-FIND(FIRST asi._file WHERE asi._file._file-name EQ "cust-part").

PROCEDURE get-cust-part :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR cp-part-no LIKE itemfg.part-no NO-UNDO.
  DEF VAR cp-rowid AS ROWID NO-UNDO.


  IF AVAIL itemfg AND ll-new-file AND ip-cust-no NE "" THEN DO:
    ASSIGN
     cp-part-no = ""
     cp-rowid   = ROWID(itemfg).

    RUN custom/getcpart.p (prmComp, ip-cust-no,
                           INPUT-OUTPUT cp-part-no, INPUT-OUTPUT cp-rowid).
    IF cp-part-no NE "" THEN
      ASSIGN
       lv-part-no = cp-part-no
       lv-cust-no = ip-cust-no.
  END.

END PROCEDURE.

FUNCTION get-cust RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  
  lv-cust-no = itemfg.cust-no.

  RUN get-cust-part.

  RETURN lv-cust-no.            /* Function return value. */

END FUNCTION.

FUNCTION get-part RETURNS CHARACTER
  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  lv-part-no = itemfg.part-no.

  RUN get-cust-part.

  RETURN lv-part-no.            /* Function return value. */

END FUNCTION.


if prmAction = "ItemSelect" then do:

    FOR EACH itemfg WHERE itemfg.company = prmComp NO-LOCK: 
     
        
              create ttLpofgLookup.
                      assign
                           ttLpofgLookup.vIFgINo             = itemfg.i-no
                           ttLpofgLookup.vIFgIName           = itemfg.i-name
                           ttLpofgLookup.vIFgDefLoc          = itemfg.def-loc
                           ttLpofgLookup.vIFgDefLocBin       = itemfg.def-loc-bin
                           ttLpofgLookup.vIFgAvgCost         = itemfg.avg-cost
                           ttLpofgLookup.vIFGProdUom         = itemfg.prod-uom
                           ttLpofgLookup.vCaseCount   = itemfg.case-count
                           ttLpofgLookup.vINo                = get-cust () 
                           ttLpofgLookup.vIName              = get-part () 
                            .
                      IF ttLpofgLookup.vIFgDefLoc = "" THEN do:
                          RUN fg/autopost.p (ROWID(itemfg),"",INT(0),
                                             OUTPUT lv-loc, OUTPUT lv-loc-bin).
                            ASSIGN ttLpofgLookup.vIFgDefLoc          = lv-loc
                             ttLpofgLookup.vIFgDefLocBin       = lv-loc-bin .
                      END.
        

    END.  /*FOR EACH itemfg*/
END.  /*ifif prmAction <> "Itemsearch" */
MESSAGE "prmAction  " prmAction prmField prmCondition prmText .

IF prmAction = "ItemSearch" then do:
     if prmField = "Item"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH itemfg WHERE itemfg.company = prmComp  AND itemfg.i-no = prmText NO-LOCK:
                 create ttLpofgLookup.
                      assign
                           ttLpofgLookup.vIFgINo             = itemfg.i-no
                           ttLpofgLookup.vIFgIName           = itemfg.i-name
                           ttLpofgLookup.vIFgDefLoc          = itemfg.def-loc
                           ttLpofgLookup.vIFgDefLocBin       = itemfg.def-loc-bin
                           ttLpofgLookup.vIFgAvgCost         = itemfg.avg-cost
                           ttLpofgLookup.vIFGProdUom         = itemfg.prod-uom
                           ttLpofgLookup.vCaseCount   = itemfg.case-count
                             lv-std-cost               = string(itemfg.total-std-cost)
                           ttLpofgLookup.vINo                = get-cust () 
                           ttLpofgLookup.vIName              = get-part () .

                       IF  itemfg.isaset THEN DO:
                           RUN fg/costset.p (ROWID(itemfg), OUTPUT v-cost).

                           IF lv-cost-uom NE "M" THEN DO:
                               RUN sys/ref/ea-um-fg.p (lv-cost-uom, OUTPUT ll-ea).
                               IF ll-ea THEN lv-cost-uom = "EA".
                               RUN sys/ref/convcuom.p("M", lv-cost-uom,
                                                      0, 0, 0, 0, v-cost, OUTPUT v-cost).
                               IF ll-ea THEN lv-cost-uom = itemfg.prod-uom.
                             END.

                             lv-std-cost = STRING(v-cost).
                        END.

                        ASSIGN ttLpofgLookup.vStdCost  = dec(lv-std-cost) .

                      IF ttLpofgLookup.vIFgDefLoc = "" THEN do:
                          RUN fg/autopost.p (ROWID(itemfg),"",INT(0),
                                             OUTPUT lv-loc, OUTPUT lv-loc-bin).
                            ASSIGN ttLpofgLookup.vIFgDefLoc          = lv-loc
                             ttLpofgLookup.vIFgDefLocBin       = lv-loc-bin .
                      END.
             END.
         END. /*FOR EACH state*/

         IF prmCondition = "BEGIN" then do:
             FOR EACH itemfg WHERE itemfg.company = prmComp  AND itemfg.i-no BEGINS prmText  NO-LOCK :
                 create ttLpofgLookup.
                      assign
                           ttLpofgLookup.vIFgINo             = itemfg.i-no
                           ttLpofgLookup.vIFgIName           = itemfg.i-name
                           ttLpofgLookup.vIFgDefLoc          = itemfg.def-loc
                           ttLpofgLookup.vIFgDefLocBin       = itemfg.def-loc-bin
                           ttLpofgLookup.vIFgAvgCost         = itemfg.avg-cost
                           ttLpofgLookup.vIFGProdUom         = itemfg.prod-uom
                           ttLpofgLookup.vCaseCount   = itemfg.case-count
                           ttLpofgLookup.vINo                = get-cust () 
                           ttLpofgLookup.vIName              = get-part () .

                      IF ttLpofgLookup.vIFgDefLoc = "" THEN do:
                          RUN fg/autopost.p (ROWID(itemfg),"",INT(0),
                                             OUTPUT lv-loc, OUTPUT lv-loc-bin).
                            ASSIGN ttLpofgLookup.vIFgDefLoc          = lv-loc
                             ttLpofgLookup.vIFgDefLocBin       = lv-loc-bin .
                      END.

             END .  /*FOR EACH itemfg*/
         END .    /*if prmCondition = BEGIN*/    
     END .  /* if prmField = state  */


     if prmField = "Name"  then do:
          if prmCondition = "EQUAL" then do:
              FOR EACH itemfg WHERE itemfg.company = prmComp  AND itemfg.i-no = prmText  NO-LOCK :
                  create ttLpofgLookup.
                      assign
                           ttLpofgLookup.vIFgINo             = itemfg.i-no
                           ttLpofgLookup.vIFgIName           = itemfg.i-name
                           ttLpofgLookup.vIFgDefLoc          = itemfg.def-loc
                           ttLpofgLookup.vIFgDefLocBin       = itemfg.def-loc-bin
                           ttLpofgLookup.vIFgAvgCost         = itemfg.avg-cost
                           ttLpofgLookup.vIFGProdUom         = itemfg.prod-uom
                           ttLpofgLookup.vCaseCount   = itemfg.case-count
                           ttLpofgLookup.vINo                = get-cust () 
                           ttLpofgLookup.vIName              = get-part ()
                                .
                      IF ttLpofgLookup.vIFgDefLoc = "" THEN do:
                          RUN fg/autopost.p (ROWID(itemfg),"",INT(0),
                                             OUTPUT lv-loc, OUTPUT lv-loc-bin).
                            ASSIGN ttLpofgLookup.vIFgDefLoc          = lv-loc
                             ttLpofgLookup.vIFgDefLocBin       = lv-loc-bin .
                      END.

              END. /*FOR EACH Itemfg */
          END. /*FOR EACH prmcondition*/

          IF prmCondition = "BEGIN" then do:
              FOR EACH itemfg WHERE itemfg.company = prmComp  AND itemfg.i-no BEGINS prmText NO-LOCK :
                  create ttLpofgLookup.
                      assign
                           ttLpofgLookup.vIFgINo             = itemfg.i-no
                           ttLpofgLookup.vIFgIName           = itemfg.i-name
                           ttLpofgLookup.vIFgDefLoc          = itemfg.def-loc
                           ttLpofgLookup.vIFgDefLocBin       = itemfg.def-loc-bin
                           ttLpofgLookup.vIFgAvgCost         = itemfg.avg-cost
                           ttLpofgLookup.vIFGProdUom         = itemfg.prod-uom
                           ttLpofgLookup.vCaseCount   = itemfg.case-count
                           ttLpofgLookup.vINo                = get-cust () 
                           ttLpofgLookup.vIName              = get-part () .

                            IF ttLpofgLookup.vIFgDefLoc = "" THEN do:
                              RUN fg/autopost.p (ROWID(itemfg),"",INT(0),
                                             OUTPUT lv-loc, OUTPUT lv-loc-bin).
                            ASSIGN ttLpofgLookup.vIFgDefLoc          = lv-loc
                             ttLpofgLookup.vIFgDefLocBin       = lv-loc-bin .
                           END.

              END.  /*FOR EACH itemfg*/
          END.    /*if prmCondition = BEGIN*/    
     END.  /* if prmField = state  */
END.  /* IF prmAction = Itemsearch then do: */
