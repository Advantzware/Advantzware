

/*------------------------------------------------------------------------
    File        : BrwsCEstimate.p
    Purpose     : Corrugated Box
    Syntax      :

    Description : Return a Dataset of Estimate Corrugated box
    Author(s)   : 
    Created     : 14 Jan 2009 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttBroFGrcpt NO-UNDO
        FIELD vRno            AS INT 
        FIELD vDate           AS CHAR  
        FIELD vTransTime       AS CHAR   
        FIELD vTag            AS CHAR FORMAT "x(20)"
        FIELD vPono          AS CHAR FORMAT "x(9)"
        FIELD vJobno         AS CHAR FORMAT "x(6)"  
        FIELD vJobno2         AS INT
        FIELD vItem          AS CHAR FORMAT "x(15)"
        FIELD vItemName       AS CHAR FORMAT "x(30)"
        FIELD vLoc          AS CHAR  FORMAT "x(5)" 
        FIELD vLocBin          AS CHAR  FORMAT "x(8)"  
        FIELD vCases        AS INT 
        FIELD vQtyCas         AS INT 
        FIELD vCasUnit          AS INT    
        FIELD vPartial          AS INT    
        FIELD vStdCost         AS DEC
        FIELD vCostUom          AS CHAR FORMAT "x(3)"
        FIELD vTQty         AS DEC
        FIELD vFrtCost        AS DEC
        FIELD vExtCost        AS DEC
        FIELD vStackCode         AS CHAR FORMAT "x(20)"   
        FIELD vCreatedBy      AS CHAR   FORMAT "x(9)"
        FIELD vCreate2         AS CHAR  FORMAT "x(9)"
        FIELD vTotWt         AS DEC
        FIELD vRecKey      AS CHAR 
        .
DEF VAR lv-frst-rno AS INT NO-UNDO.
DEF VAR lv-linker LIKE fg-rcpts.linker NO-UNDO.
DEF VAR lv-setprt AS LOG NO-UNDO.

DEFINE DATASET dsBroFGrcpt FOR ttBroFGrcpt .

DEFINE INPUT PARAMETER prmUser        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmFgItem      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmJobno       AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmPono        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmSeqno       AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmRcptDate    AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmTagno       AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER prmRecKey      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmllSetParts   AS CHAR        NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsBroFGrcpt.

DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
DEF NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEF NEW SHARED VAR locode AS CHAR NO-UNDO.

IF prmUser        = ? THEN ASSIGN prmUser     = "".
IF prmAction      = ? THEN ASSIGN prmAction   = "Select".
IF prmFgItem      = ? THEN ASSIGN prmFgItem   = "".
IF prmJobno       = ? THEN ASSIGN prmJobno    = "".  
IF prmPono        = ? THEN ASSIGN prmPono     = "".
IF prmSeqno       = ? THEN ASSIGN prmSeqno    = "".
IF prmRcptDate    = ? THEN ASSIGN prmRcptDate = "".
IF prmTagno       = ? THEN ASSIGN prmTagno    = "".
IF prmRecKey      = ? THEN ASSIGN prmRecKey   = "".
IF prmllSetParts  = ? THEN ASSIGN prmllSetParts = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

ASSIGN
    cocode = prmComp .

ASSIGN lv-setprt = (IF prmllSetParts = "Yes" THEN TRUE ELSE FALSE).

   
   FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .

       END. /*FOR EACH usercust*/ 

RUN get-first-r-no.
RUN get-linker.                                                   

 IF prmAction = "Select" THEN DO:
  
 
    FOR EACH fg-rctd WHERE 
       fg-rctd.company eq cocode and 
           fg-rctd.r-no ge lv-frst-rno and 
           (fg-rctd.rita-code eq "R" or fg-rctd.rita-code eq "E") 
           use-index fg-rctd NO-LOCK, 
       FIRST reftable WHERE reftable.reftable EQ "fg-rctd.user-id" AND 
             reftable.company  EQ fg-rctd.company AND 
             reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999") AND 
             ((reftable.dscr    EQ lv-linker AND reftable.dscr begins "fg-rctd: ") OR 
             (not lv-setprt and not reftable.dscr begins "fg-rctd: ")) NO-LOCK BY fg-rctd.r-no DESC: 
             

            create ttBroFGrcpt.
            assign
                ttBroFGrcpt.vRno            = fg-rctd.r-no
                ttBroFGrcpt.vDate           = string(fg-rctd.rct-date)
                ttBroFGrcpt.vTransTime      = STRING(fg-rctd.trans-time,'HH:MM')
                ttBroFGrcpt.vTag            = fg-rctd.tag
                ttBroFGrcpt.vPono           = fg-rctd.po-no
                ttBroFGrcpt.vJobno          = fg-rctd.job-no
                ttBroFGrcpt.vJobno2         = fg-rctd.job-no2
                ttBroFGrcpt.vItem           = fg-rctd.i-no
                ttBroFGrcpt.vItemName       = fg-rctd.i-name
                ttBroFGrcpt.vLoc            = fg-rctd.loc 
                ttBroFGrcpt.vLocBin         = fg-rctd.loc-bin
                ttBroFGrcpt.vCases          = fg-rctd.cases 
                ttBroFGrcpt.vQtyCas         = fg-rctd.qty-case 
                ttBroFGrcpt.vCasUnit        = fg-rctd.cases-unit  
                ttBroFGrcpt.vPartial        = fg-rctd.partial
                ttBroFGrcpt.vStdCost        = fg-rctd.std-cost
                ttBroFGrcpt.vCostUom        = fg-rctd.cost-uom
                ttBroFGrcpt.vTQty           = fg-rctd.t-qty
                ttBroFGrcpt.vFrtCost        = fg-rctd.frt-cost
                ttBroFGrcpt.vExtCost        = fg-rctd.ext-cost
                ttBroFGrcpt.vStackCode      = fg-rctd.stack-code 
                ttBroFGrcpt.vCreatedBy      = reftable.code
                ttBroFGrcpt.vCreate2        = reftable.code2  
                ttBroFGrcpt.vTotWt          = fg-rctd.tot-wt 
                ttBroFGrcpt.vRecKey         = fg-rctd.rec_key .
            IF lv-setprt THEN do:
                ttBroFGrcpt.vPartial = (INT(fg-rctd.t-qty) - (fg-rctd.cases * DEC(fg-rctd.qty-case))).
            END.


                      
       END. /*FOR EACH vend-whse-trans*/
 /*END.  /*end of usercust*/*/


   
  END.   /*IF prmAction = "select" THEN DO:*/
/*********************************************************************************/
  
 IF prmAction = "Search" THEN DO:

 
    FOR EACH fg-rctd WHERE 
       fg-rctd.company eq cocode and 
           fg-rctd.r-no ge lv-frst-rno and 
           (fg-rctd.rita-code eq "R" or fg-rctd.rita-code eq "E") AND
           (fg-rctd.r-no = int(prmSeqno) OR prmSeqno = "" ) AND
           (fg-rctd.i-no BEGINS prmFgItem OR prmFgItem = "" ) AND
           (fg-rctd.job-no = prmJobno OR prmJobno = "" ) AND
           (fg-rctd.po-no = prmPono OR prmPono = "" ) AND
           (fg-rctd.rct-date = DATE(prmRcptDate) OR prmRcptDate = "" ) AND
           (fg-rctd.tag BEGINS prmTagno OR prmTagno = "" )
           use-index fg-rctd NO-LOCK, 
       FIRST reftable WHERE reftable.reftable EQ "fg-rctd.user-id" AND 
             reftable.company  EQ fg-rctd.company AND 
             reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999") AND 
             ((reftable.dscr    EQ lv-linker AND reftable.dscr begins "fg-rctd: ") OR 
             (not lv-setprt and not reftable.dscr begins "fg-rctd: ")) NO-LOCK: 
             

            create ttBroFGrcpt.
            assign
                ttBroFGrcpt.vRno            = fg-rctd.r-no
                ttBroFGrcpt.vDate           = string(fg-rctd.rct-date)
                ttBroFGrcpt.vTransTime      = STRING(fg-rctd.trans-time,'HH:MM')
                ttBroFGrcpt.vTag            = fg-rctd.tag
                ttBroFGrcpt.vPono           = fg-rctd.po-no
                ttBroFGrcpt.vJobno          = fg-rctd.job-no
                ttBroFGrcpt.vJobno2         = fg-rctd.job-no2
                ttBroFGrcpt.vItem           = fg-rctd.i-no
                ttBroFGrcpt.vItemName       = fg-rctd.i-name
                ttBroFGrcpt.vLoc            = fg-rctd.loc 
                ttBroFGrcpt.vLocBin         = fg-rctd.loc-bin
                ttBroFGrcpt.vCases          = fg-rctd.cases 
                ttBroFGrcpt.vQtyCas         = fg-rctd.qty-case 
                ttBroFGrcpt.vCasUnit        = fg-rctd.cases-unit  
                ttBroFGrcpt.vPartial        = fg-rctd.partial
                ttBroFGrcpt.vStdCost        = fg-rctd.std-cost
                ttBroFGrcpt.vCostUom        = fg-rctd.cost-uom
                ttBroFGrcpt.vTQty           = fg-rctd.t-qty
                ttBroFGrcpt.vFrtCost        = fg-rctd.frt-cost
                ttBroFGrcpt.vExtCost        = fg-rctd.ext-cost
                ttBroFGrcpt.vStackCode      = fg-rctd.stack-code 
                ttBroFGrcpt.vCreatedBy      = reftable.code
                ttBroFGrcpt.vCreate2        = reftable.code2  
                ttBroFGrcpt.vTotWt          = fg-rctd.tot-wt 
                ttBroFGrcpt.vRecKey         = fg-rctd.rec_key .
                  
                END.   /* end of for loop*/
            
            
 END. /* end search */


PROCEDURE get-first-r-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bq-fg-rctd FOR fg-rctd.

  lv-frst-rno = 999999999.

  FOR EACH bq-fg-rctd FIELDS(r-no)
      WHERE bq-fg-rctd.company   EQ cocode
        AND bq-fg-rctd.rita-code EQ "R"
        AND bq-fg-rctd.r-no      LT lv-frst-rno
      USE-INDEX rita-code NO-LOCK
      BY bq-fg-rctd.r-no:
    lv-frst-rno = bq-fg-rctd.r-no.
    LEAVE.
  END.
  RELEASE bq-fg-rctd.

  FOR EACH bq-fg-rctd FIELDS(r-no)
      WHERE bq-fg-rctd.company   EQ cocode
        AND bq-fg-rctd.rita-code EQ "E"
        AND bq-fg-rctd.r-no      LT lv-frst-rno
      USE-INDEX rita-code NO-LOCK
      BY bq-fg-rctd.r-no:
    lv-frst-rno = bq-fg-rctd.r-no.
    LEAVE.
  END.
  RELEASE bq-fg-rctd.

END PROCEDURE.


PROCEDURE get-linker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*DEF OUTPUT PARAM op-linker LIKE lv-linker NO-UNDO.*/

    FOR EACH fg-rctd WHERE fg-rctd.company eq cocode and 
           fg-rctd.r-no EQ int(prmRecKey)  and 
           (fg-rctd.rita-code eq "R" or fg-rctd.rita-code eq "E") NO-LOCK:

  lv-linker = IF AVAIL fg-rctd                 AND
                 CAN-FIND(FIRST itemfg
                          WHERE itemfg.company EQ fg-rctd.company
                            AND itemfg.i-no    EQ fg-rctd.i-no
                            AND itemfg.isaset) THEN
                "fg-rctd: " + STRING(fg-rctd.r-no,"9999999999")
              ELSE "".
    END.

END PROCEDURE.
