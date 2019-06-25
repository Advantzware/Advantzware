
/*------------------------------------------------------------------------
    File         : CorLayoutBoardLook
    Purpose     :  LayoutBoard Lookup

    Syntax      :

    Description : Return a Dataset of Corrugated Layout Board

    Author(s)   : 
    Created     : Feb 19 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCorLayoutBoardLook NO-UNDO 
        FIELD vItemNum   AS CHARACTER
        FIELD vItemName  AS CHARACTER
        FIELD vER        AS CHARACTER
        FIELD vCaliper   AS DECIMAL
        FIELD vWidth     AS DECIMAL
        FIELD vLength    AS DECIMAL
        FIELD vQtyOnHand AS DECIMAL
        FIELD vComm      AS DECIMAL
        FIELD vAvail     AS DECIMAL
        FIELD vTest      AS CHARACTER
        FIELD vReal      AS CHARACTER
        FIELD vFlute     AS CHARACTER
        FIELD vItemNum2  AS CHAR
        FIELD vItemName2 AS CHAR
    .

DEFINE DATASET dsCorLayoutBoardLook FOR ttCorLayoutBoardLook.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmIndustry      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmEstimate        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmStyle           AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCorLayoutBoardLook.

DEF VAR prmComp AS CHAR NO-UNDO.

IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmIndustry     = ? THEN ASSIGN prmIndustry     = "".
IF prmEstimate    = ? THEN ASSIGN prmEstimate    = "".
IF prmAction    = ? THEN ASSIGN prmAction    = "as".
IF prmStyle     = ? THEN ASSIGN prmStyle     = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
 
MESSAGE "val" prmComp  prmAction prmEstimate .
if prmAction <> "search" then do:
    IF prmStyle <> "" THEN DO:
                MESSAGE "test" .
            find style where style.company = prmComp and
                            style.style = prmStyle
                            no-lock no-error.   
           if avail style then prmIndustry = style.industry.
           else prmIndustry = "". 
         
  if avail style and style.type = "f" then  DO:
         MESSAGE "style" style.TYPE prmIndustry  .
     FOR EACH item WHERE item.company = prmComp and (item.industry = prmIndustry or prmIndustry = "") 
      AND item.mat-type >= "1" and item.mat-type <= "4" NO-LOCK :
      IF AVAIL ITEM  THEN DO:
          CREATE ttCorLayoutBoardLook.
          ASSIGN
              ttCorLayoutBoardLook.vItemNum         = ITEM.i-no
              ttCorLayoutBoardLook.vItemName        = ITEM.i-name
              ttCorLayoutBoardLook.vER              = ITEM.i-code
              ttCorLayoutBoardLook.vCaliper         = ITEM.cal
              ttCorLayoutBoardLook.vWidth           = IF item.r-wid EQ 0 THEN item.s-wid ELSE item.r-wid
              ttCorLayoutBoardLook.vLength          = IF item.r-wid EQ 0 THEN item.s-len ELSE 12
              ttCorLayoutBoardLook.vQtyOnHand       = ITEM.q-onh
              ttCorLayoutBoardLook.vComm            = ITEM.q-comm
              ttCorLayoutBoardLook.vAvail           = ITEM.q-avail
              ttCorLayoutBoardLook.vReal            = ITEM.i-code
              ttCorLayoutBoardLook.vFlute           = ITEM.flute  
              ttCorLayoutBoardLook.vItemNum2        = ITEM.i-no
              ttCorLayoutBoardLook.vItemName2       = ITEM.i-name  
              ttCorLayoutBoardLook.vTest            = ITEM.reg-no
              .

            /*FIND FIRST stack-flute WHERE stack-flute.company = prmComp AND (stack-flute.code = ITEM.flute ) NO-LOCK NO-ERROR.
            ASSIGN
                ttCorLayoutBoardLook.vTest            = stack-flute.row-value[1].
              */

        END. /*IF AVAIL ITEM  THEN DO:*/
     END. /*FOR EACH item*/
  END. /*if avail style and style.type = "f" then  DO:*/

  ELSE DO:
      FOR EACH item WHERE item.company = prmComp and (item.industry = prmIndustry or prmIndustry = "") 
          and (item.mat-type = 'B' or item.mat-type = 'P' or item.mat-type = 'R') NO-LOCK:
                IF AVAIL ITEM  THEN DO:
                CREATE ttCorLayoutBoardLook.
                  ASSIGN
                    ttCorLayoutBoardLook.vItemNum         = ITEM.i-no
                    ttCorLayoutBoardLook.vItemName        = ITEM.i-name
                    ttCorLayoutBoardLook.vER              = ITEM.i-code
                    ttCorLayoutBoardLook.vCaliper         = ITEM.cal
                    ttCorLayoutBoardLook.vWidth           = IF item.r-wid EQ 0 THEN item.s-wid ELSE item.r-wid
                    ttCorLayoutBoardLook.vLength          = IF item.r-wid EQ 0 THEN item.s-len ELSE 12
                    ttCorLayoutBoardLook.vQtyOnHand       = ITEM.q-onh
                    ttCorLayoutBoardLook.vComm            = ITEM.q-comm
                    ttCorLayoutBoardLook.vAvail           = ITEM.q-avail
                    ttCorLayoutBoardLook.vReal            = ITEM.i-code
                    ttCorLayoutBoardLook.vFlute           = ITEM.flute
                    ttCorLayoutBoardLook.vItemNum2        = ITEM.i-no
                    ttCorLayoutBoardLook.vItemName2       = ITEM.i-name 
                    ttCorLayoutBoardLook.vTest            = ITEM.reg-no
                .
                
                /*FIND FIRST stack-flute WHERE stack-flute.company = prmComp AND (stack-flute.code = ITEM.flute ) NO-LOCK NO-ERROR.
                   ASSIGN
                        ttCorLayoutBoardLook.vTest            = stack-flute.row-value[1].                  
                 */

      END. /*IF AVAIL ITEM  THEN DO:*/
  END. /*FOR EACH item*/
  END. /*Else  DO:*/
  END.  /* if prmStyle <> ""*/
  END. /*if prmAction <> "search"*/


  IF prmAction = "search" THEN DO:
   if prmField = "i-no"  then do:
        if prmCondition = "EQUAL" then do:
        IF prmStyle <> "" THEN DO:
            find style where style.company = prmComp and
                            style.style = prmStyle
                            no-lock no-error.   
           if avail style then prmIndustry = style.industry.
           else prmIndustry = "". 
   
    if avail style and style.type = "f" then  DO:
     FOR EACH item WHERE item.company = prmComp and (item.industry = prmIndustry or prmIndustry = "") 
      AND item.mat-type >= "1" and item.mat-type <= "4" AND (ITEM.i-no = prmText) NO-LOCK:
      IF AVAIL ITEM  THEN DO:
          CREATE ttCorLayoutBoardLook.
          ASSIGN
              ttCorLayoutBoardLook.vItemNum         = ITEM.i-no
              ttCorLayoutBoardLook.vItemName        = ITEM.i-name
              ttCorLayoutBoardLook.vER              = ITEM.i-code
              ttCorLayoutBoardLook.vCaliper         = ITEM.cal
              ttCorLayoutBoardLook.vWidth           = IF item.r-wid EQ 0 THEN item.s-wid ELSE item.r-wid
              ttCorLayoutBoardLook.vLength          = IF item.r-wid EQ 0 THEN item.s-len ELSE 12
              ttCorLayoutBoardLook.vQtyOnHand       = ITEM.q-onh
              ttCorLayoutBoardLook.vComm            = ITEM.q-comm
              ttCorLayoutBoardLook.vAvail           = ITEM.q-avail
              ttCorLayoutBoardLook.vReal            = ITEM.i-code
              ttCorLayoutBoardLook.vFlute           = ITEM.flute
              ttCorLayoutBoardLook.vItemNum2        = ITEM.i-no
              ttCorLayoutBoardLook.vItemName2       = ITEM.i-name
              ttCorLayoutBoardLook.vTest            = ITEM.reg-no
              .

            /* FIND FIRST stack-flute WHERE stack-flute.company = prmComp AND (stack-flute.code = ITEM.flute ) NO-LOCK NO-ERROR.
            ASSIGN
                ttCorLayoutBoardLook.vTest            = stack-flute.row-value[1].
            */

      END. /*IF AVAIL ITEM  THEN DO:*/
  END. /*FOR EACH item*/
  END. /*if avail style and style.type = "f" then  DO:*/

  ELSE DO:
    FOR EACH item WHERE item.company = prmComp and (item.industry = prmIndustry or prmIndustry = "") 
                        and (item.mat-type = 'B' or item.mat-type = 'P' or item.mat-type = 'R')
                        AND (ITEM.i-no = prmText) NO-LOCK:
  IF AVAIL ITEM  THEN DO:
          CREATE ttCorLayoutBoardLook.
          ASSIGN
              ttCorLayoutBoardLook.vItemNum         = ITEM.i-no
              ttCorLayoutBoardLook.vItemName        = ITEM.i-name
              ttCorLayoutBoardLook.vER              = ITEM.i-code
              ttCorLayoutBoardLook.vCaliper         = ITEM.cal
              ttCorLayoutBoardLook.vWidth           = IF item.r-wid EQ 0 THEN item.s-wid ELSE item.r-wid
              ttCorLayoutBoardLook.vLength          = IF item.r-wid EQ 0 THEN item.s-len ELSE 12
              ttCorLayoutBoardLook.vQtyOnHand       = ITEM.q-onh
              ttCorLayoutBoardLook.vComm            = ITEM.q-comm
              ttCorLayoutBoardLook.vAvail           = ITEM.q-avail
              ttCorLayoutBoardLook.vReal            = ITEM.i-code
              ttCorLayoutBoardLook.vFlute           = ITEM.flute
              ttCorLayoutBoardLook.vItemNum2        = ITEM.i-no
              ttCorLayoutBoardLook.vItemName2       = ITEM.i-name
              ttCorLayoutBoardLook.vTest            = ITEM.reg-no
              .
    /*FIND FIRST stack-flute WHERE stack-flute.company = prmComp AND (stack-flute.code = ITEM.flute ) NO-LOCK NO-ERROR.
    ASSIGN        
        ttCorLayoutBoardLook.vTest            = stack-flute.row-value[1].*/

      END. /*IF AVAIL ITEM  THEN DO:*/
  END. /*FOR EACH item*/
  END. /*Else  DO:*/
   END.
  END. /*if prmCondition = "EQUAL" then do:*/

  if prmCondition = "Begin" then do:
     IF prmStyle <> "" THEN DO:
            find style where style.company = prmComp and
                            style.style = prmStyle
                            no-lock no-error.   
           if avail style then prmIndustry = style.industry.
           else prmIndustry = "". 
   
   if avail style and style.type = "f" then  DO:
          FOR EACH item WHERE item.company = prmComp and (item.industry = prmIndustry or prmIndustry = "") 
      AND item.mat-type >= "1" and item.mat-type <= "4" AND (ITEM.i-no BEGINS prmText) NO-LOCK:
      IF AVAIL ITEM  THEN DO:
          CREATE ttCorLayoutBoardLook.
          ASSIGN
              ttCorLayoutBoardLook.vItemNum         = ITEM.i-no
              ttCorLayoutBoardLook.vItemName        = ITEM.i-name
              ttCorLayoutBoardLook.vER              = ITEM.i-code
              ttCorLayoutBoardLook.vCaliper         = ITEM.cal
              ttCorLayoutBoardLook.vWidth           = IF item.r-wid EQ 0 THEN item.s-wid ELSE item.r-wid
              ttCorLayoutBoardLook.vLength          = IF item.r-wid EQ 0 THEN item.s-len ELSE 12
              ttCorLayoutBoardLook.vQtyOnHand       = ITEM.q-onh
              ttCorLayoutBoardLook.vComm            = ITEM.q-comm
              ttCorLayoutBoardLook.vAvail           = ITEM.q-avail
              ttCorLayoutBoardLook.vReal            = ITEM.i-code
              ttCorLayoutBoardLook.vFlute           = ITEM.flute
              ttCorLayoutBoardLook.vItemNum2        = ITEM.i-no
              ttCorLayoutBoardLook.vItemName2       = ITEM.i-name
              ttCorLayoutBoardLook.vTest            = ITEM.reg-no              
              .
            /* FIND FIRST stack-flute WHERE stack-flute.company = prmComp AND (stack-flute.code = ITEM.flute ) NO-LOCK NO-ERROR.
            ASSIGN
                ttCorLayoutBoardLook.vTest            = stack-flute.row-value[1].
             */

      END. /*IF AVAIL ITEM  THEN DO:*/
  END. /*FOR EACH item*/
  END. /*if avail style and style.type = "f" then  DO:*/

  ELSE DO:
     FOR EACH item WHERE item.company = prmComp and (item.industry = prmIndustry or prmIndustry = "") 
                        and (item.mat-type = 'B' or item.mat-type = 'P' or item.mat-type = 'R')
                        AND (ITEM.i-no BEGINS prmText) NO-LOCK:
  IF AVAIL ITEM  THEN DO:
          CREATE ttCorLayoutBoardLook.
          ASSIGN
              ttCorLayoutBoardLook.vItemNum         = ITEM.i-no
              ttCorLayoutBoardLook.vItemName        = ITEM.i-name
              ttCorLayoutBoardLook.vER              = ITEM.i-code
              ttCorLayoutBoardLook.vCaliper         = ITEM.cal
              ttCorLayoutBoardLook.vWidth           = IF item.r-wid EQ 0 THEN item.s-wid ELSE item.r-wid
              ttCorLayoutBoardLook.vLength          = IF item.r-wid EQ 0 THEN item.s-len ELSE 12
              ttCorLayoutBoardLook.vQtyOnHand       = ITEM.q-onh
              ttCorLayoutBoardLook.vComm            = ITEM.q-comm
              ttCorLayoutBoardLook.vAvail           = ITEM.q-avail
              ttCorLayoutBoardLook.vReal            = ITEM.i-code
              ttCorLayoutBoardLook.vFlute           = ITEM.flute
              ttCorLayoutBoardLook.vItemNum2        = ITEM.i-no
              ttCorLayoutBoardLook.vItemName2       = ITEM.i-name
              ttCorLayoutBoardLook.vTest            = ITEM.reg-no
              .
                
             /*  FIND FIRST stack-flute WHERE stack-flute.company = prmComp AND (stack-flute.code = ITEM.flute ) NO-LOCK NO-ERROR.
            ASSIGN
                ttCorLayoutBoardLook.vTest            = stack-flute.row-value[1].
             */

      END. /*IF AVAIL ITEM  THEN DO:*/
  END. /*FOR EACH item*/
  END. /*Else  DO:*/
   END.  /*prmStyle <> "" */
  END. /*if prmCondition = "EQUAL" then do*/

   END. /*if prmField = "i-no" then do:*/
  END.


  FOR EACH ttCorLayoutBoardLook NO-LOCK:
       IF INDEX(ttCorLayoutBoardLook.vItemNum2 ,'"',1) > 0 THEN ASSIGN
            ttCorLayoutBoardLook.vItemNum2  = REPLACE(ttCorLayoutBoardLook.vItemNum2 ,'"',":").
        IF INDEX(ttCorLayoutBoardLook.vItemName2 ,'"',1) > 0 THEN ASSIGN
            ttCorLayoutBoardLook.vItemName2  = REPLACE(ttCorLayoutBoardLook.vItemName2 ,'"',":").

  END.
