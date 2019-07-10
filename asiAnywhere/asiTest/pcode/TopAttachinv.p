/*------------------------------------------------------------------------
    File        : TopAttachedinv.p
    Purpose     : Attached file

    Syntax      :

    Description : Return a Dataset of all attachment

    Author(s)   : 
    Created     : Feb 23 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttTopAttachpoinvoice NO-UNDO 
    FIELD vAttFile     AS CHAR
    FIELD vPoNo        AS CHAR
    FIELD vFGitem      AS CHAR 
    FIELD vDate        AS DATE 
    FIELD vOpenWith    AS CHAR 
    FIELD vReckey      AS CHAR
    FIELD invattac     AS CHAR 
    FIELD invvv        AS CHAR
    .
DEFINE DATASET dsTopAttachpoinvoice FOR ttTopAttachpoinvoice.

DEFINE INPUT PARAMETER prmAction      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmRecKey      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAttFile     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmPoNo         AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmFgitem      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmDate        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmOpenWith    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmSerchEst    AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsTopAttachpoinvoice.
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

prmPoNo = STRING(prmPoNo,"X(12)") + "APINV".


/*************************Select****************************************/
 IF prmAction = "Select" THEN DO:
     
     FOR EACH attach WHERE  attach.rec_key eq prmRecKey 
                          AND (if prmPoNo ne "" then (attach.est-no eq prmPoNo or attach.est-no eq "") 
                                 else attach.est-no eq "") NO-LOCK :
         CREATE ttTopAttachpoinvoice.
         ASSIGN
             ttTopAttachpoinvoice.vAttFile   = attach.attach-file
             ttTopAttachpoinvoice.vPoNo  =  STRING(attach.est-no,"X(12)")
             ttTopAttachpoinvoice.vFGitem    = attach.file-type
             ttTopAttachpoinvoice.vDate      = attach.creat-date 
             ttTopAttachpoinvoice.vReckey      = STRING(rowid(attach))
             .
     END.
 END.

/*************************Search**************************************/

    IF prmAction = "Search" THEN DO:
       FOR EACH attach WHERE  attach.rec_key eq prmRecKey AND
                             (if prmPoNo ne "" then (attach.est-no eq prmPoNo or attach.est-no eq "") 
                                 else attach.est-no eq "")
                              AND (trim(attach.attach-file) BEGINS prmAttFile OR prmAttFile = "")
                              AND (attach.FILE-TYPE BEGINS prmFgitem OR prmFgitem = "")
                              AND (attach.est-no BEGINS prmPoNo OR prmPoNo = "")
                              AND (attach.creat-date EQ date(prmDate) OR prmDate = "")  NO-LOCK :
        CREATE ttTopAttachpoinvoice.
         ASSIGN
             ttTopAttachpoinvoice.vAttFile   = attach.attach-file
             ttTopAttachpoinvoice.vPoNo  = STRING(attach.est-no,"X(12)")
             ttTopAttachpoinvoice.vFGitem    = attach.file-type
             ttTopAttachpoinvoice.vDate      = attach.creat-date 
             ttTopAttachpoinvoice.vReckey      = STRING(rowid(attach))
             .
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
            ATTACH.rec_key         = prmSerchEst
            ATTACH.FILE-TYPE       = prmFgitem
           .
        
ASSIGN 
        prmRecKey = string(rowid(ATTACH))
        prmAction = "View"
        .

    END.

 
/*********************************Update*********************************/
  IF prmAction = "Update" THEN DO:

    
 FIND FIRST attach WHERE attach.company = prmComp  
             AND rowid(ATTACH) = to-rowid(prmRecKey)  EXCLUSIVE-LOCK NO-ERROR.
        
        ASSIGN
            attach.attach-file     = prmAttFile
            attach.run-application = prmOpenWith
            attach.est-no          = prmPoNo
            ATTACH.FILE-TYPE       = prmFgitem
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
                              ATTACH.rec_key   = prmSerchEst AND 
                             (if prmPoNo ne "" then (attach.est-no eq prmPoNo or attach.est-no eq "") 
                                 else attach.est-no eq "")  NO-LOCK NO-ERROR.
        IF AVAIL ATTACH  THEN DO:
            ASSIGN
                prmRecKey = string(rowid(ATTACH))
                prmAction = "View" .
            
        END.
          
    END.

/*********************************View*********************************/
 IF prmAction = "View" THEN DO:
     
    FIND FIRST attach WHERE  attach.company = prmComp   AND rowid(ATTACH) = to-rowid(prmRecKey)  NO-LOCK NO-ERROR.
         CREATE ttTopAttachpoinvoice.
    ASSIGN
             ttTopAttachpoinvoice.vAttFile   = attach.attach-file
             ttTopAttachpoinvoice.vPoNo  = STRING(attach.est-no,"X(12)")
             ttTopAttachpoinvoice.vFGitem    = attach.file-type
             ttTopAttachpoinvoice.vDate      = attach.creat-date 
             ttTopAttachpoinvoice.vOpenWith  = attach.run-application
             ttTopAttachpoinvoice.vReckey      = STRING(rowid(attach))
                 .
      
      
  END.
/*********************************End Select*********************************/

