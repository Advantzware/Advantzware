/*------------------------------------------------------------------------
    File        : TopEstItem.p
    Purpose     : Est Item

    Syntax      :

    Description : Return a Dataset of all FG Item

    Author(s)   : 
    Created     : aug 5 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
MESSAGE "enter" .
DEFINE TEMP-TABLE ttTopEstFGItem NO-UNDO 
    FIELD vItem      AS CHAR 
    FIELD vName      AS CHAR
    FIELD vReckey    AS CHAR 
    FIELD bjjklll    AS CHAR
     .

DEFINE DATASET dsTopEstFGItem FOR ttTopEstFGItem.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmEst       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmItem      AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsTopEstFGItem.
DEFINE OUTPUT PARAMETER cError AS CHAR NO-UNDO.


    IF prmAction = ?  THEN ASSIGN prmAction = "".
    IF prmUser   = ?  THEN ASSIGN prmUser   = "".
    IF prmEst = ?  THEN ASSIGN prmEst = "".
    IF prmItem = ? THEN  ASSIGN  prmItem = "".
DEFINE VAR vEstimate AS CHAR NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
vEstimate =  FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst).

/*************************Select****************************************/
 IF prmAction = "Select" THEN DO:
     find first est where (est.est-no = vEstimate OR est.est-no = prmEst) AND est.company = prmComp no-lock no-error.

    if avail est then do:
       for each eb where eb.company = est.company and
                       eb.est-no = est.est-no no-lock:
         IF eb.stock-no <> "" THEN DO:
             CREATE ttTopEstFGItem.
             ASSIGN
                 ttTopEstFGItem.vItem = eb.stock-no
                 ttTopEstFGItem.vName = eb.part-dscr1 
                 .

             find itemfg where itemfg.company = est.company 
                      and itemfg.i-no = eb.stock-no
                      no-lock no-error.
                    ttTopEstFGItem.vReckey  = if avail itemfg then itemfg.rec_key else est.rec_key .
            END.  
            IF eb.stock-no = "" THEN DO:
                ASSIGN
                     cError =  "No FG Item entered. " .
                RETURN .

            END.
       end. 
    END.

    
    IF NOT AVAIL est  THEN DO:
       FIND FIRST ITEM WHERE ITEM.company = prmComp 
                          AND (ITEM.i-no = prmItem AND ITEM.i-no <> "") NO-LOCK NO-ERROR.
        IF AVAIL ITEM  THEN DO:
            CREATE ttTopEstFGItem .
            ASSIGN
                ttTopEstFGItem.vItem = ITEM.i-no
                ttTopEstFGItem.vName = ITEM.i-name 
                ttTopEstFGItem.vReckey = ITEM.rec_key
                 .
            MESSAGE "item" .
          
        END.
        IF NOT AVAIL ITEM THEN DO:
             FIND FIRST ITEMfg WHERE ITEMfg.company = prmComp 
                          AND (ITEMfg.i-no = prmItem AND  ITEMfg.i-no <> "" ) NO-LOCK NO-ERROR.
            IF AVAIL itemfg THEN DO:
                     CREATE ttTopEstFGItem .
                     ASSIGN
                         ttTopEstFGItem.vItem = itemfg.i-no
                         ttTopEstFGItem.vName = itemfg.i-name 
                         ttTopEstFGItem.vReckey = ITEMfg.rec_key
                         .
                     MESSAGE "fgitem" .
            END.
             
            IF NOT AVAIL itemfg  THEN DO:
                MESSAGE "test2".
                cError =  "No FG Item entered. " .
                RETURN .
            END.
        END.
    END.
   .
     /* FIND FIRST ttTopEstFGItem  NO-LOCK NO-ERROR.   
       IF ttTopEstFGItem.vItem = "" THEN DO:
           ASSIGN
               cError =  "No FG Item entered. " .
           RETURN .
       END.*/
       
       
 END.

/*************************End Select***********************************/

 
