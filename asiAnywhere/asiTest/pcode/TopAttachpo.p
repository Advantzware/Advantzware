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
DEFINE TEMP-TABLE ttTopAttachpoitem NO-UNDO 
    FIELD vAttFile     AS CHAR
    FIELD vPoNo        AS CHAR
    FIELD vFGitem      AS CHAR 
    FIELD vDate        AS DATE 
    FIELD vOpenWith    AS CHAR 
    FIELD vReckey      AS CHAR
    FIELD attachhh     AS CHAR 
    .
DEFINE DATASET dsTopAttachpoitem FOR ttTopAttachpoitem.

DEFINE INPUT PARAMETER prmAction      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmRecKey      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAttFile     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmPoNo         AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmFgitem      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDate        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmOpenWith    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmSerchEst    AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsTopAttachpoitem.
DEFINE OUTPUT PARAMETER cError AS CHAR NO-UNDO.


    IF prmAction    = ?  THEN ASSIGN prmAction      = "".
    IF prmUser      = ?  THEN ASSIGN prmUser        = "".
    IF prmRecKey    = ?  THEN ASSIGN prmRecKey      = "".
    IF prmAttFile   = ?  THEN ASSIGN prmAttFile     = "".
    IF prmPoNo       = ?  THEN ASSIGN prmPoNo         = "".
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

vEstval = FILL(" ",8 - LENGTH(TRIM(prmPoNo))) + TRIM(prmPoNo).

/*************************Select****************************************/
 IF prmAction = "Select" THEN DO:
     FOR EACH attach WHERE  attach.company = prmComp and 
                             attach.rec_key eq prmRecKey AND
                             (prmPoNo <> "" and trim(attach.est-no) = trim(prmPoNo))  NO-LOCK :
         CREATE ttTopAttachpoitem.
         ASSIGN
             ttTopAttachpoitem.vAttFile   = attach.attach-file
             ttTopAttachpoitem.vPoNo  = attach.est-no
             ttTopAttachpoitem.vFGitem    = attach.i-no
             ttTopAttachpoitem.vDate      = attach.creat-date 
             ttTopAttachpoitem.vReckey      = STRING(rowid(attach))
             .
     END.
 END.

/*************************Search**************************************/

    IF prmAction = "Search" THEN DO:
        FOR EACH attach WHERE  attach.company = prmComp and 
                             prmPoNo <> "" and trim(attach.est-no) = trim(prmPoNo) AND 
                              attach.rec_key eq prmRecKey AND (attach.creat-date = DATE(prmDate) OR prmDate = "")
                              AND (trim(attach.attach-file) BEGINS prmSerchEst OR prmSerchEst = "")  NO-LOCK :
        CREATE ttTopAttachpoitem.
    ASSIGN
             ttTopAttachpoitem.vAttFile   = attach.attach-file
             ttTopAttachpoitem.vPoNo  = attach.est-no
             ttTopAttachpoitem.vFGitem    = attach.i-no
             ttTopAttachpoitem.vDate      = attach.creat-date
             ttTopAttachpoitem.vReckey      = STRING(rowid(attach))  .
     END.
    END.
    
 /*********************************Add validation*********************************/
    IF prmAction = "Add" THEN DO:
        IF prmPoNo = ""  THEN DO:
         ASSIGN
             cError = "Po#  cannot blank. . ." .
         RETURN.
      END.

  IF prmPoNo <> "" THEN DO:
        FIND FIRST po-ord WHERE po-ord.company = prmComp AND po-ord.po-no = INT(prmPoNo) NO-LOCK NO-ERROR .
        IF NOT AVAIL po-ord THEN DO:
            ASSIGN
                cError = "Invalid Po#. Try Help." .
                RETURN .
        END.
    END.

    
    END.

 /************************************Add**********************/
    IF prmAction = "Add" THEN DO:
       
        CREATE ATTACH.
        ASSIGN
            ATTACH.company         = prmComp 
            attach.attach-file     = prmAttFile
            attach.est-no          = prmPoNo
            attach.creat-date      = TODAY
            attach.run-application = prmOpenWith
            ATTACH.rec_key         = prmFgitem
           .
        FIND FIRST po-ord WHERE po-ord.company = prmComp AND po-ord.po-no = INT(prmPoNo) NO-LOCK NO-ERROR .
        IF AVAIL po-ord THEN 
           ASSIGN ATTACH.rec_key   = po-ord.rec_key .
            ELSE
            ATTACH.rec_key = prmFgitem .

ASSIGN 
        prmRecKey = string(rowid(ATTACH))
        prmAction = "View"
        .

    END.

 /*************************************update validtion******************/

  IF prmAction = "Update" THEN DO:
     IF prmPoNo = ""  THEN DO:
         ASSIGN
             cError = "Po# cannot blank..." .
         RETURN.
      END.

  IF prmPoNo <> "" THEN DO:
        FIND FIRST po-ord WHERE po-ord.company = prmComp AND po-ord.po-no = INT(prmPoNo) NO-LOCK NO-ERROR .
        IF NOT AVAIL po-ord THEN DO:
            ASSIGN
                cError = "Invalid PoNo#. Try Help." .
                RETURN .
        END.
    END.
  END.

/*********************************Update*********************************/
  IF prmAction = "Update" THEN DO:

    
 FIND FIRST attach WHERE attach.company = prmComp and 
                              rowid(ATTACH) = to-rowid(prmRecKey)  EXCLUSIVE-LOCK NO-ERROR.
        ASSIGN
            attach.attach-file     = prmAttFile
            attach.run-application = prmOpenWith
            attach.est-no          = prmPoNo
           .

        ASSIGN
             prmAction = "View" .
      END.


/*********************************Delete*************************************/
    IF prmAction = "Delete" THEN DO:
        FIND FIRST attach WHERE  attach.company = prmComp  
             AND rowid(ATTACH) = to-rowid(prmRecKey)  EXCLUSIVE-LOCK NO-ERROR.

        IF AVAIL ATTACH THEN  DELETE ATTACH.
        

       FIND LAST attach WHERE  attach.company = prmComp and 
                             (prmPoNo <> "" and trim(attach.est-no) = trim(prmPoNo))  NO-LOCK NO-ERROR.
        IF AVAIL ATTACH  THEN DO:
            ASSIGN
                prmRecKey = string(rowid(ATTACH))
                prmAction = "View" .
            
        END.
          
    END.

/*********************************View*********************************/
 IF prmAction = "View" THEN DO:
     
    FIND FIRST attach WHERE  attach.company = prmComp   AND rowid(ATTACH) = to-rowid(prmRecKey)  NO-LOCK NO-ERROR.
         CREATE ttTopAttachpoitem.
    ASSIGN
             ttTopAttachpoitem.vAttFile   = attach.attach-file
             ttTopAttachpoitem.vPoNo  = attach.est-no
             ttTopAttachpoitem.vFGitem    = attach.i-no
             ttTopAttachpoitem.vDate      = attach.creat-date
             ttTopAttachpoitem.vOpenWith  = attach.run-application
             ttTopAttachpoitem.vReckey      = STRING(rowid(attach))
                 .
      
      
  END.
/*********************************End Select*********************************/

