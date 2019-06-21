
/*------------------------------------------------------------------------
    File         : CorBoardLook
    Purpose     :  Board Lookup

    Syntax      :

    Description : Return a Dataset of Corrugated Box

    Author(s)   : 
    Created     : Feb 2 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCorBoardLook NO-UNDO 
        FIELD vItemNum   AS CHARACTER
        FIELD vItemName  AS CHARACTER
        FIELD vER        AS CHARACTER
        FIELD vCaliper   AS DECIMAL
        FIELD vWidth     AS DECIMAL
        FIELD vLength    AS DECIMAL
        FIELD vQtyOnHand AS DECIMAL
        FIELD vComm      AS DECIMAL
        FIELD vAvail     AS DECIMAL
        
    .

DEFINE DATASET dsCorBoardLook FOR ttCorBoardLook.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmEstimate        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmStyle           AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCorBoardLook.

DEF VAR prmComp AS CHAR NO-UNDO.

IF prmAction     = ? THEN ASSIGN    prmAction     = "".
IF prmUser         = ? THEN ASSIGN  prmUser       = "".
IF prmField        = ? THEN ASSIGN  prmField      = ""   .
IF prmCondition    = ? THEN ASSIGN  prmCondition  = ""  .
IF prmText         = ? THEN ASSIGN  prmText       = "" .
IF prmEstimate     = ? THEN ASSIGN  prmEstimate   = "".
IF prmStyle        = ? THEN ASSIGN  prmStyle      = "" .




FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
FIND FIRST eb WHERE eb.company = prmComp AND eb.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) NO-LOCK NO-ERROR.
IF AVAIL eb THEN
            find style where style.company = prmComp and
                            style.style = eb.style
                            no-lock no-error. 
      
     if avail style and style.type = "f" then  DO:
     FOR EACH item WHERE item.company = prmComp and (item.industry = style.industry or style.industry = "") 
      AND item.mat-type >= "1" and item.mat-type <= "4" NO-LOCK :
      IF AVAIL ITEM  THEN DO:
          CREATE ttCorBoardLook.
          ASSIGN
              ttCorBoardLook.vItemNum         = ITEM.i-no
              ttCorBoardLook.vItemName        = ITEM.i-name
              ttCorBoardLook.vER              = ITEM.i-code
              ttCorBoardLook.vCaliper         = ITEM.cal
              ttCorBoardLook.vWidth           = IF item.r-wid EQ 0 THEN item.s-wid ELSE item.r-wid
              ttCorBoardLook.vLength          = IF item.r-wid EQ 0 THEN item.s-len ELSE 12
              ttCorBoardLook.vQtyOnHand       = ITEM.q-onh
              ttCorBoardLook.vComm            = ITEM.q-comm
              ttCorBoardLook.vAvail           = ITEM.q-avail
              .
      END. /*IF AVAIL ITEM  THEN DO:*/
  END. /*FOR EACH item*/
  END. /*if avail style and style.type = "f" then  DO:*/

  ELSE DO:
      FIND FIRST eb WHERE eb.company = prmComp AND eb.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) NO-LOCK NO-ERROR.
IF AVAIL eb THEN
            find style where style.company = prmComp and
                            style.style = eb.style
                            no-lock no-error. 
      if avail style then
          FOR EACH item WHERE item.company = prmComp and (item.industry = style.industry or style.industry = "") 
                        and (item.mat-type = 'B' or item.mat-type = 'P' or item.mat-type = 'R') NO-LOCK:
  IF AVAIL ITEM  THEN DO:
          CREATE ttCorBoardLook.
          ASSIGN
              ttCorBoardLook.vItemNum         = ITEM.i-no
              ttCorBoardLook.vItemName        = ITEM.i-name
              ttCorBoardLook.vER              = ITEM.i-code
              ttCorBoardLook.vCaliper         = ITEM.cal
              ttCorBoardLook.vWidth           = IF item.r-wid EQ 0 THEN item.s-wid ELSE item.r-wid
              ttCorBoardLook.vLength          = IF item.r-wid EQ 0 THEN item.s-len ELSE 12
              ttCorBoardLook.vQtyOnHand       = ITEM.q-onh
              ttCorBoardLook.vComm            = ITEM.q-comm
              ttCorBoardLook.vAvail           = ITEM.q-avail
              .
      END. /*IF AVAIL ITEM  THEN DO:*/
  END. /*FOR EACH item*/
  END. /*Else  DO:*/
  END. /*if prmAction <> "search"*/


  IF prmAction = "search" THEN DO:
   if prmField = "i-no"  then do:
        if prmCondition = "EQUAL" then do:

           FIND FIRST eb WHERE eb.company = prmComp AND eb.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) NO-LOCK NO-ERROR.
IF AVAIL eb THEN
            find style where style.company = prmComp and
                            style.style = eb.style
                            no-lock no-error. 
     if avail style and style.type = "f" then  DO:
     FOR EACH item WHERE item.company = prmComp and (item.industry = style.industry or style.industry = "") 
      AND item.mat-type >= "1" and item.mat-type <= "4" AND (ITEM.i-no = prmText) NO-LOCK:
      IF AVAIL ITEM  THEN DO:
          CREATE ttCorBoardLook.
          ASSIGN
              ttCorBoardLook.vItemNum         = ITEM.i-no
              ttCorBoardLook.vItemName        = ITEM.i-name
              ttCorBoardLook.vER              = ITEM.i-code
              ttCorBoardLook.vCaliper         = ITEM.cal
              ttCorBoardLook.vWidth           = IF item.r-wid EQ 0 THEN item.s-wid ELSE item.r-wid
              ttCorBoardLook.vLength          = IF item.r-wid EQ 0 THEN item.s-len ELSE 12
              ttCorBoardLook.vQtyOnHand       = ITEM.q-onh
              ttCorBoardLook.vComm            = ITEM.q-comm
              ttCorBoardLook.vAvail           = ITEM.q-avail
              .
      END. /*IF AVAIL ITEM  THEN DO:*/
  END. /*FOR EACH item*/
  END. /*if avail style and style.type = "f" then  DO:*/

  ELSE DO:
      FIND FIRST eb WHERE eb.company = prmComp AND eb.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) NO-LOCK NO-ERROR.
IF AVAIL eb THEN
            find style where style.company = prmComp and
                            style.style = eb.style
                            no-lock no-error. 
      if avail style then
          FOR EACH item WHERE item.company = prmComp and (item.industry = style.industry or style.industry = "") 
                        and (item.mat-type = 'B' or item.mat-type = 'P' or item.mat-type = 'R')
                        AND (ITEM.i-no = prmText) NO-LOCK:
  IF AVAIL ITEM  THEN DO:
          CREATE ttCorBoardLook.
          ASSIGN
              ttCorBoardLook.vItemNum         = ITEM.i-no
              ttCorBoardLook.vItemName        = ITEM.i-name
              ttCorBoardLook.vER              = ITEM.i-code
              ttCorBoardLook.vCaliper         = ITEM.cal
              ttCorBoardLook.vWidth           = IF item.r-wid EQ 0 THEN item.s-wid ELSE item.r-wid
              ttCorBoardLook.vLength          = IF item.r-wid EQ 0 THEN item.s-len ELSE 12
              ttCorBoardLook.vQtyOnHand       = ITEM.q-onh
              ttCorBoardLook.vComm            = ITEM.q-comm
              ttCorBoardLook.vAvail           = ITEM.q-avail
              .
      END. /*IF AVAIL ITEM  THEN DO:*/
  END. /*FOR EACH item*/
  END. /*Else  DO:*/
  END. /*if prmCondition = "EQUAL" then do:*/

  if prmCondition = "Begin" then do:
      FIND FIRST eb WHERE eb.company = prmComp AND eb.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) NO-LOCK NO-ERROR.
IF AVAIL eb THEN
            find style where style.company = prmComp and
                            style.style = eb.style
                            no-lock no-error. 
      if avail style and style.type = "f" then  DO:
          FOR EACH item WHERE item.company = prmComp and (item.industry = style.industry or style.industry = "") 
      AND item.mat-type >= "1" and item.mat-type <= "4" AND (ITEM.i-no BEGINS prmText) NO-LOCK:
      IF AVAIL ITEM  THEN DO:
          CREATE ttCorBoardLook.
          ASSIGN
              ttCorBoardLook.vItemNum         = ITEM.i-no
              ttCorBoardLook.vItemName        = ITEM.i-name
              ttCorBoardLook.vER              = ITEM.i-code
              ttCorBoardLook.vCaliper         = ITEM.cal
              ttCorBoardLook.vWidth           = IF item.r-wid EQ 0 THEN item.s-wid ELSE item.r-wid
              ttCorBoardLook.vLength          = IF item.r-wid EQ 0 THEN item.s-len ELSE 12
              ttCorBoardLook.vQtyOnHand       = ITEM.q-onh
              ttCorBoardLook.vComm            = ITEM.q-comm
              ttCorBoardLook.vAvail           = ITEM.q-avail
              .
      END. /*IF AVAIL ITEM  THEN DO:*/
  END. /*FOR EACH item*/
  END. /*if avail style and style.type = "f" then  DO:*/

  ELSE DO:
      FIND FIRST eb WHERE eb.company = prmComp AND eb.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) NO-LOCK NO-ERROR.
IF AVAIL eb THEN
            find style where style.company = prmComp and
                            style.style = eb.style
                            no-lock no-error. 
      if avail style then
          FOR EACH item WHERE item.company = prmComp and (item.industry = style.industry or style.industry = "") 
                        and (item.mat-type = 'B' or item.mat-type = 'P' or item.mat-type = 'R')
                        AND (ITEM.i-no BEGINS prmText) NO-LOCK:
  IF AVAIL ITEM  THEN DO:
          CREATE ttCorBoardLook.
          ASSIGN
              ttCorBoardLook.vItemNum         = ITEM.i-no
              ttCorBoardLook.vItemName        = ITEM.i-name
              ttCorBoardLook.vER              = ITEM.i-code
              ttCorBoardLook.vCaliper         = ITEM.cal
              ttCorBoardLook.vWidth           = IF item.r-wid EQ 0 THEN item.s-wid ELSE item.r-wid
              ttCorBoardLook.vLength          = IF item.r-wid EQ 0 THEN item.s-len ELSE 12
              ttCorBoardLook.vQtyOnHand       = ITEM.q-onh
              ttCorBoardLook.vComm            = ITEM.q-comm
              ttCorBoardLook.vAvail           = ITEM.q-avail
              .
      END. /*IF AVAIL ITEM  THEN DO:*/
  END. /*FOR EACH item*/
  END. /*Else  DO:*/
  END. /*if prmCondition = "EQUAL" then do*/



   END. /*if prmField = "i-no" then do:*/
  END.
