


/*------------------------------------------------------------------------
    File        : BoardLook.p
    Purpose     : Cust Part

    Syntax      :

    Description : Return a Dataset of all PartLook

    Author(s)   : Jyoti
    Created     : Feb 23 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttBoardLook NO-UNDO 
    FIELD QINum   AS CHAR
    FIELD QIName  AS CHAR
    FIELD QCode   AS CHAR
    FIELD QCal    AS DECIMAL
    FIELD QWid    AS DECIMAL
    FIELD QLen    AS DECIMAL
    FIELD QOnh    AS DECIMAL
    FIELD QComm   AS DECIMAL
    FIELD QAvail  AS DECIMAL
    .


DEFINE DATASET dsBoardLook FOR ttBoardLook.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmIndustry  AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsBoardLook.

DEF VAR prmComp AS CHAR NO-UNDO.

IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition      = ? THEN ASSIGN prmCondition      = "".
IF prmText      = ? THEN ASSIGN prmText      = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
    FOR EACH item WHERE item.company = prmComp /*AND (item.industry = prmIndustry or prmIndustry = "") */
                                                  AND (item.mat-type = 'B' ) NO-LOCK :
        create ttBoardLook.
            assign
                ttBoardLook.QINum   =  item.i-no
                ttBoardLook.QIName  =  item.i-name
                ttBoardLook.QCode   =  item.i-code
                ttBoardLook.QCal    =  item.cal
                ttBoardLook.QWid    =  item.r-wid
                ttBoardLook.QLen    =  item.s-len
                ttBoardLook.QOnh    =  item.q-onh
                ttBoardLook.QComm   = item.q-comm
                ttBoardLook.QAvail  = item.q-avail
               .
    END.	 /* FOR EACH eb */     
    
END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "search" then do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:
            FOR EACH item WHERE item.company = prmComp /*AND (item.industry = prmIndustry or prmIndustry = '') */
                                                          AND (item.mat-type = 'B')
                                                          AND (item.i-no = prmText OR item.i-name = prmText OR 
                                                               item.i-code = prmText OR item.cal = DECIMAL(prmText) OR
                                                               item.r-wid = DECIMAL(prmText) OR item.s-len = DECIMAL(prmText) OR
                                                               item.q-onh = DECIMAL(prmText) OR item.q-comm = DECIMAL(prmText) OR
                                                               item.q-avail = DECIMAL( prmText) ) no-lock:
                create ttBoardLook.
                assign
                    ttBoardLook.QINum   =  ITEM.i-no
                    ttBoardLook.QIName  =  ITEM.i-name
                    ttBoardLook.QCode   =  item.i-code
                    ttBoardLook.QCal    =  item.cal
                    ttBoardLook.QWid    =  item.r-wid
                    ttBoardLook.QLen    =  item.s-len
                    ttBoardLook.QOnh    =  item.q-onh
                    ttBoardLook.QComm   = item.q-comm
                    ttBoardLook.QAvail  = item.q-avail.
                
            END. /*FOR EACH cust where cust.cust-no = prmText*/
        END.  /*IF prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
           FOR EACH item WHERE item.company = prmComp /*AND (item.industry = prmIndustry or prmIndustry = '') */
                                                          AND (item.mat-type = 'B')
                                                          AND (item.i-no BEGINS prmText OR item.i-name BEGINS prmText OR 
                                                               item.i-code BEGINS prmText OR item.cal = DECIMAL(prmText) OR
                                                               item.r-wid = DECIMAL(prmText) OR item.s-len = DECIMAL(prmText) OR
                                                               item.q-onh = DECIMAL(prmText) OR item.q-comm = DECIMAL(prmText) OR
                                                               item.q-avail = DECIMAL(prmText) ) no-lock:
                create ttBoardLook.
                assign
                    ttBoardLook.QINum   =  ITEM.i-no
                    ttBoardLook.QIName  =  ITEM.i-name
                    ttBoardLook.QCode   =  item.i-code
                    ttBoardLook.QCal    =  item.cal
                    ttBoardLook.QWid    =  item.r-wid
                    ttBoardLook.QLen    =  item.s-len
                    ttBoardLook.QOnh    =  item.q-onh
                    ttBoardLook.QComm   = item.q-comm
                    ttBoardLook.QAvail  = item.q-avail.

            END.  /*FOR EACH eb where */         
         END. /*IF prmCondition = BEGIN then do:*/  
    END. /*IF prmField = ANY*/  
    if prmField = "i-no"  then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH item WHERE item.company = prmComp /*AND (item.industry = prmIndustry or prmIndustry = '') */
                                                          AND (item.mat-type = 'B')
                                                          AND (item.i-no = prmText ) NO-LOCK:
                create ttBoardLook.
                assign
                    ttBoardLook.QINum   =  ITEM.i-no
                    ttBoardLook.QIName  =  ITEM.i-name
                    ttBoardLook.QCode   =  item.i-code
                    ttBoardLook.QCal    =  item.cal
                    ttBoardLook.QWid    =  item.r-wid
                    ttBoardLook.QLen    =  item.s-len
                    ttBoardLook.QOnh    =  item.q-onh
                    ttBoardLook.QComm   = item.q-comm
                    ttBoardLook.QAvail  = item.q-avail.
            END. /*FOR EACH eb where*/
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
            FOR EACH item WHERE item.company = prmComp /* AND (item.industry = prmIndustry or prmIndustry = '') */
                                                          AND (item.mat-type = 'B' )
                                                          AND (item.i-no BEGINS prmText) NO-LOCK:
                create ttBoardLook.
                assign
                    ttBoardLook.QINum   =  ITEM.i-no
                    ttBoardLook.QIName  =  ITEM.i-name
                    ttBoardLook.QCode   =  item.i-code
                    ttBoardLook.QCal    =  item.cal
                    ttBoardLook.QWid    =  item.r-wid
                    ttBoardLook.QLen    =  item.s-len
                    ttBoardLook.QOnh    =  item.q-onh
                    ttBoardLook.QComm   = item.q-comm
                    ttBoardLook.QAvail  = item.q-avail .
            end.  /*FOR EACH eb wher*/
        end.    /*if prmCondition = BEGIN*/    
     end.  /* if prmField = est  */
       IF prmField = "i-name" then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH item WHERE item.company = prmComp /*AND (item.industry = prmIndustry or prmIndustry = '') */
                                                          AND (item.mat-type = 'B')
                                                          AND (item.i-name = prmText)  no-lock:
                 create ttBoardLook.
                 assign
                     ttBoardLook.QINum   =  ITEM.i-no
                     ttBoardLook.QIName  =  ITEM.i-name
                     ttBoardLook.QCode   =  item.i-code
                     ttBoardLook.QCal    =  item.cal
                     ttBoardLook.QWid    =  item.r-wid
                     ttBoardLook.QLen    =  item.s-len
                     ttBoardLook.QOnh    =  item.q-onh
                     ttBoardLook.QComm   = item.q-comm
                     ttBoardLook.QAvail  = item.q-avail.
             END. /*FOR EACH eb where*/
         END. /*if prmCondition = EQUAL*/
         IF prmCondition = "BEGIN" then do:
             FOR EACH item WHERE item.company = prmComp /*AND (item.industry = prmIndustry or prmIndustry = '') */
                                                          AND (item.mat-type = 'B')
                                                          AND ( item.i-name BEGINS prmText)  no-lock:
                 create ttBoardLook.
                 assign
                     ttBoardLook.QINum   =  ITEM.i-no
                     ttBoardLook.QIName  =  ITEM.i-name
                     ttBoardLook.QCode   =  item.i-code
                     ttBoardLook.QCal    =  item.cal
                     ttBoardLook.QWid    =  item.r-wid
                     ttBoardLook.QLen    =  item.s-len
                     ttBoardLook.QOnh    =  item.q-onh
                     ttBoardLook.QComm   = item.q-comm
                     ttBoardLook.QAvail  = item.q-avail                 
                     .
             END. /*FOR EACH eb where*/
         END.  /*if prmCondition = BEGIN*/
     END.  /*IF prmField = est-no */
     IF prmField = "i-code" then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH item WHERE item.company = prmComp AND
                item.mat-type = 'B' AND
                item.i-code = prmText
                no-lock:

                /*IF NOT (prmIndustry = '' OR item.industry = prmIndustry) THEN NEXT.*/

                create ttBoardLook.
                assign
                    ttBoardLook.QINum   =  ITEM.i-no
                    ttBoardLook.QIName  =  ITEM.i-name
                    ttBoardLook.QCode   =  item.i-code
                    ttBoardLook.QCal    =  item.cal
                    ttBoardLook.QWid    =  item.r-wid
                    ttBoardLook.QLen    =  item.s-len
                    ttBoardLook.QOnh    =  item.q-onh
                    ttBoardLook.QComm   = item.q-comm
                    ttBoardLook.QAvail  = item.q-avail.
            END. /*FOR EACH eb where*/
        END. /*if prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
            FOR EACH item WHERE item.company = prmComp /*AND (item.industry = prmIndustry or prmIndustry = '') */
                                                         AND (item.mat-type = 'B')
                                                         AND ( item.i-code BEGINS prmText)  no-lock:
                create ttBoardLook.
                assign
                    ttBoardLook.QINum   =  ITEM.i-no
                    ttBoardLook.QIName  =  ITEM.i-name
                    ttBoardLook.QCode   =  item.i-code
                    ttBoardLook.QCal    =  item.cal
                    ttBoardLook.QWid    =  item.r-wid
                    ttBoardLook.QLen    =  item.s-len
                    ttBoardLook.QOnh    =  item.q-onh
                    ttBoardLook.QComm   = item.q-comm
                    ttBoardLook.QAvail  = item.q-avail                 
                    .
            END. /*FOR EACH eb where*/
        END.  /*if prmCondition = BEGIN*/
    END.  /*IF prmField = est-no */


    
END.  /* IF prmAction = search then do: */



