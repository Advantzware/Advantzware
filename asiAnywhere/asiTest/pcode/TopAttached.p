/*------------------------------------------------------------------------
    File        : TopAttached.p
    Purpose     : Attached file

    Syntax      :

    Description : Return a Dataset of all attachment

    Author(s)   : 
    Created     : Feb 23 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttTopAttachment NO-UNDO 
    FIELD vAttFile     AS CHAR
    FIELD vEstimate    AS CHAR
    FIELD vFGitem      AS CHAR 
    FIELD vDate        AS DATE 
    FIELD vOpenWith    AS CHAR 
    FIELD vReckey      AS CHAR
    FIELD ttuuuu       AS CHAR 
    .
DEFINE DATASET dsTopAttachment FOR ttTopAttachment.

DEFINE INPUT PARAMETER prmAction      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmRecKey      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAttFile     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmEst         AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmFgitem      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDate        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmOpenWith    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmSerchEst    AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsTopAttachment.
DEFINE OUTPUT PARAMETER cError AS CHAR NO-UNDO.


    IF prmAction    = ?  THEN ASSIGN prmAction      = "".
    IF prmUser      = ?  THEN ASSIGN prmUser        = "".
    IF prmRecKey    = ?  THEN ASSIGN prmRecKey      = "".
    IF prmAttFile   = ?  THEN ASSIGN prmAttFile     = "".
    IF prmEst       = ?  THEN ASSIGN prmEst         = "".
    IF prmFgitem    = ?  THEN ASSIGN prmFgitem      = "".
    IF prmOpenWith  = ?  THEN ASSIGN prmOpenWith    = "".
    IF prmDate      = ?  THEN ASSIGN prmDate        = "".
    IF prmSerchEst  = ?  THEN ASSIGN prmSerchEst    = "".
   
DEFINE VAR vEstval AS CHAR NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

vEstval = FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst).

/*************************Select****************************************/
 IF prmAction = "Select" THEN DO:
     FOR EACH attach WHERE  (attach.company = prmComp and 
                             (prmEst <> "" and trim(attach.est-no) = trim(prmEst)) or 
                             (prmFgitem <> "" and index(prmFgitem,attach.i-no) > 0)) NO-LOCK :
         CREATE ttTopAttachment.
         ASSIGN
             ttTopAttachment.vAttFile   = attach.attach-file
             ttTopAttachment.vEstimate  = attach.est-no
             ttTopAttachment.vFGitem    = attach.i-no
             ttTopAttachment.vDate      = attach.creat-date 
             ttTopAttachment.vReckey      = attach.rec_key
             .
     END.
 END.

/*************************Search**************************************/

    IF prmAction = "Search" THEN DO:
        FOR EACH attach WHERE  (attach.company = prmComp and 
                             (prmEst <> "" and trim(attach.est-no) = trim(prmEst)) or 
                             (prmFgitem <> "" and index(prmFgitem,attach.i-no) > 0)) AND (attach.creat-date = DATE(prmDate) OR prmDate = "")
                              AND (trim(attach.est-no) = prmSerchEst OR prmSerchEst = "")  NO-LOCK :
        CREATE ttTopAttachment.
    ASSIGN
             ttTopAttachment.vAttFile   = attach.attach-file
             ttTopAttachment.vEstimate  = attach.est-no
             ttTopAttachment.vFGitem    = attach.i-no
             ttTopAttachment.vDate      = attach.creat-date
             ttTopAttachment.vReckey      = attach.rec_key .
     END.
    END.
    
 /*********************************Add validation*********************************/
    IF prmAction = "Add" THEN DO:
        IF prmEst = "" AND  prmFgitem = "" THEN DO:
         ASSIGN
             cError = "Estimate# and FG Item# cannot blank. Enter either or both." .
         RETURN.
      END.
MESSAGE "est" vEstval .
  IF prmEst <> "" THEN DO:
        FIND FIRST est WHERE est.company = prmComp AND est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst) NO-LOCK NO-ERROR .
        IF NOT AVAIL est THEN DO:
            ASSIGN
                cError = "Invalid Estimate#. Try Help." .
                RETURN .
        END.
    END.

    IF prmFgitem <> "" THEN DO:
           FIND FIRST itemfg WHERE itemfg.company = prmComp  AND 
               itemfg.i-no = prmFgitem NO-LOCK NO-ERROR.
            IF NOT AVAIL itemfg THEN DO:
                cError =  "Invalid FG Item#. Try Help." .
                RETURN .
             END.
      END.

    END.

 /************************************Add**********************/
    IF prmAction = "Add" THEN DO:
        MESSAGE "addfile"  prmAttFile prmComp .
        CREATE ATTACH.
        ASSIGN
            ATTACH.company         = prmComp 
            attach.attach-file     = prmAttFile
            attach.est-no          = prmEst
            attach.i-no            = prmFgitem
            attach.creat-date      = TODAY
            attach.run-application = prmOpenWith
           .

ASSIGN 
        prmRecKey = ATTACH.rec_key 
        prmAction = "View"
        .

    END.

 /*************************************update validtion******************/

  IF prmAction = "Update" THEN DO:
     IF prmEst = "" AND  prmFgitem = "" THEN DO:
         ASSIGN
             cError = "Estimate# and FG Item# cannot blank. Enter either or both." .
         RETURN.
      END.

  IF prmEst <> "" THEN DO:
        FIND FIRST est WHERE est.company = prmComp AND est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst) NO-LOCK NO-ERROR .
        IF NOT AVAIL est THEN DO:
            ASSIGN
                cError = "Invalid Estimate#. Try Help." .
                RETURN .
        END.
    END.

    IF prmFgitem <> "" THEN DO:
           FIND  FIRST itemfg WHERE itemfg.company = prmComp  AND 
               itemfg.i-no = prmFgitem NO-LOCK NO-ERROR.
            IF NOT AVAIL itemfg THEN DO:
                cError =  "Invalid FG Item#. Try Help." .
                RETURN .
             END.
      END.
  END.

/*********************************Update*********************************/
  IF prmAction = "Update" THEN DO:

    
 FIND FIRST attach WHERE attach.company = prmComp and 
                              ATTACH.rec_key = prmRecKey  EXCLUSIVE-LOCK NO-ERROR.
        ASSIGN
            attach.attach-file     = prmAttFile
            attach.run-application = prmOpenWith
            attach.est-no          = prmEst
            attach.i-no            = prmFgitem .

        ASSIGN
             prmAction = "View" .
      END.


/*********************************Delete*************************************/
    IF prmAction = "Delete" THEN DO:
        FIND FIRST attach WHERE  attach.company = prmComp  
             AND ATTACH.rec_key = prmRecKey  EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL ATTACH THEN  DELETE ATTACH.
        MESSAGE "del" prmEst prmFgitem .
        FIND LAST attach WHERE  (attach.company = prmComp and 
                             (prmEst <> "" and trim(attach.est-no) = trim(prmEst)) or 
                             (prmFgitem <> "" and index(prmFgitem,attach.i-no) > 0))  NO-LOCK NO-ERROR.
        IF AVAIL ATTACH  THEN DO:
            ASSIGN
                prmRecKey = ATTACH.rec_key
                prmAction = "View" .
            MESSAGE "delete" ATTACH.rec_key .
        END.
          
    END.

/*********************************View*********************************/
 IF prmAction = "View" THEN DO:
     
    FIND FIRST attach WHERE  attach.company = prmComp   AND ATTACH.rec_key = prmRecKey  NO-LOCK NO-ERROR.
         CREATE ttTopAttachment.
    ASSIGN
             ttTopAttachment.vAttFile   = attach.attach-file
             ttTopAttachment.vEstimate  = attach.est-no
             ttTopAttachment.vFGitem    = attach.i-no
             ttTopAttachment.vDate      = attach.creat-date
             ttTopAttachment.vOpenWith  = attach.run-application
             ttTopAttachment.vReckey      = attach.rec_key
                 .
      
      
  END.
/*********************************End Select*********************************/

