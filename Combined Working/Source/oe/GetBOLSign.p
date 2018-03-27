&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : oe/GetBolSign.p
    Purpose     : Access BOlSign web service to retrieve the BOL signature
                and update BOL Header "Signed" flag

    Syntax      :

    Description :

    Author(s)   :  BV
    Created     :  11/4/2013
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .P file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipcCocode AS CHAR NO-UNDO.  /*Company*/
DEFINE INPUT PARAMETER ipriBolHeader AS ROWID NO-UNDO. /*ROW-ID of BOL Header to test (for single BOL mode*/
DEFINE INPUT PARAMETER iplExecuteFTP AS LOG NO-UNDO.  /*YES -> FTP to service*/
                                                      /*NO -> Check local image file only*/
                                                      /*Only applies to single BOL check*/
DEFINE INPUT PARAMETER iplBuildFTPFile AS LOG NO-UNDO. /*Create the FTP file on the fly vs. specify ftp instruction*/
DEFINE OUTPUT PARAMETER opcImageLocation AS CHAR NO-UNDO. /*Primary output*/

{oe/BolSign.i} /*CONSTANTS*/

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME


/* ************************  Function Prototypes ********************** */

&IF DEFINED(EXCLUDE-CheckForSignature) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CheckForSignature Procedure 
FUNCTION CheckForSignature RETURNS LOGICAL
  ( ipcFileLocation AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CheckSlash) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD CheckSlash Procedure 
FUNCTION CheckSlash RETURNS CHARACTER
  ( ipcPath AS CHAR, iplSlash AS LOG)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetImageDir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetImageDir Procedure 
FUNCTION GetImageDir RETURNS CHARACTER
  ( ipcCocode AS CHAR, iplSlash AS LOG )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetSignatureFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetSignatureFile Procedure 
FUNCTION GetSignatureFile RETURNS CHARACTER
  ( ipiBolNo AS INT, ipcImageDir AS CHAR )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF


/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY COMPILE
 */
&ANALYZE-RESUME _END-PROCEDURE-SETTINGS

/* *************************  Create Window  ************************** */

&ANALYZE-SUSPEND _CREATE-WINDOW
/* DESIGN Window definition (used by the UIB) 
  CREATE WINDOW Procedure ASSIGN
         HEIGHT             = 15.05
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

IF ipriBOLHeader NE ? THEN /*ipriBOLHeader ne ? (one BOL at a time)*/
   RUN GetSigSingleBOL.
ELSE 
    RUN GetSigAll.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-ExecuteFTPBatchGet) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExecuteFTPBatchGet Procedure 
PROCEDURE ExecuteFTPBatchGet :
/*------------------------------------------------------------------------------
  Purpose:    Execute the FTP to pull data from Trucker app web server.
            This will dump all images not already in the Signed folder 
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcImageDir AS CHAR.

IF iplBuildFTPFile THEN DO:
    /*Build FTP instruction file*/
    OUTPUT TO VALUE(".\oe\BolSignGetBatch.txt").    /* ftp text file */
    ipcImageDir = CheckSlash(ipcImageDir,NO).    
    PUT UNFORMATTED 
      "open " {&FTP-SERVER} SKIP   /* ftp server ip address */
      {&FTP-USERID} SKIP   /* userid */
      {&FTP-USERPASS} SKIP   /* password */
      "bin" SKIP /*binary file transfer vs. ASCII*/
      'lcd "' ipcImageDir '"' SKIP /*prep the local directory for fetch*/
      "mget *." + {&IMG-TYPE} SKIP   /* get BOL sign */
      "quit" .
    OUTPUT CLOSE.
END.
IF iplExecuteFTP THEN
    OS-COMMAND SILENT VALUE("ftp -v -i -s:.\oe\BolSignGetBatch.txt").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ExecuteFTPBatchRename) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExecuteFTPBatchRename Procedure 
PROCEDURE ExecuteFTPBatchRename :
/*------------------------------------------------------------------------------
  Purpose:    Execute the FTP to move the image files into the signed folder on
            on the server.
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcBOLList AS CHAR.

DEFINE VARIABLE cEntry AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iEntry AS INTEGER     NO-UNDO.

/*Build FTP instruction file*/
OUTPUT TO VALUE(".\oe\BolSignRenameBatch.txt").    /* ftp text file */
PUT UNFORMATTED 
  "open " {&FTP-SERVER}  SKIP   /* ftp server ip address */
  {&FTP-USERID} SKIP   /* userid */
  {&FTP-USERPASS} SKIP   /* password */
  .
DO iEntry = 1 TO NUM-ENTRIES(ipcBOLList):
    cEntry = ENTRY(iEntry,ipcBOLList).
    PUT UNFORMATTED 
        "rename " cEntry "." + {&IMG-TYPE} + " " {&FTP-SIGNED-DIR} cEntry "." + {&IMG-TYPE} SKIP.
END.
PUT UNFORMATTED 'quit' .
OUTPUT CLOSE.

IF iplExecuteFTP AND cEntry NE "" THEN
    OS-COMMAND SILENT VALUE("ftp -v -i -s:.\oe\BolSignRenameBatch.txt").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ExecuteFTPGetRenameSingle) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExecuteFTPGetRenameSingle Procedure 
PROCEDURE ExecuteFTPGetRenameSingle :
/*------------------------------------------------------------------------------
  Purpose:    Execute the FTP to send data file to Trucker app web server 
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipiBolNo AS INTEGER.
DEFINE INPUT PARAMETER ipcImageDir AS CHAR.

/*Build FTP instruction file*/
OUTPUT TO VALUE(".\oe\BolSignGet.txt").    /* ftp text file */
ipcImageDir = CheckSlash(ipcImageDir,YES).
PUT UNFORMATTED 
  "open " {&FTP-SERVER} SKIP   /* ftp server ip address */
  {&FTP-USERID} SKIP   /* userid */
  {&FTP-USERPASS} SKIP   /* password */
  "bin" SKIP /*binary file transfer vs. ASCII*/
  "get " STRING(ipiBOlNo) "." + {&IMG-TYPE} + " " ipcImageDir STRING(ipiBOlNo) "." + {&IMG-TYPE} SKIP   /* get BOL sign */
  "rename " STRING(ipiBOlNo) "." + {&IMG-TYPE} + " " {&FTP-SIGNED-DIR} STRING(ipiBOlNo) "." + {&IMG-TYPE} SKIP
  "quit" .
OUTPUT CLOSE.

IF iplExecuteFTP AND ipiBOLno GT 0 THEN
    OS-COMMAND SILENT VALUE("ftp -v -i -s:.\oe\BolSignGet.txt").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetSigAll) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSigAll Procedure 
PROCEDURE GetSigAll :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE BUFFER bf-oe-bolh FOR oe-bolh.
DEFINE VARIABLE cBOLList AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cImageDir AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cImageFile AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lSigned AS LOGICAL     NO-UNDO.

cImageDir = GetImageDir(INPUT ipcCocode,
                        INPUT NO).
RUN ExecuteFTPBatchGet(INPUT cImageDir).
FOR EACH bf-oe-bolh NO-LOCK
    WHERE bf-oe-bolh.company EQ ipcCocode
      AND bf-oe-bolh.posted EQ NO
      AND bf-oe-bolh.bol-no GT 0
      AND bf-oe-bolh.spare-int-1 NE 1 /*not signed*/
    :
    cImageFile = GetSignatureFile(INPUT bf-oe-bolh.bol-no,
                                  INPUT cImageDir). /*set image file path*/
    RUN UpdateBolHeader(BUFFER bf-oe-bolh, 
                        INPUT cImageFile,
                        OUTPUT lSigned). /*Check to see if image already exists*/
    IF lSigned THEN
        cBOLList = cBOLList + STRING(bf-oe-bolh.bol-no) + ",".
END.
cBOLList = SUBSTRING(cBOLList,1,LENGTH(cBOLList) - 1,"CHARACTER").
RUN ExecuteFTPBatchRename(INPUT cBOLList).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetSigSingleBOL) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE GetSigSingleBOL Procedure 
PROCEDURE GetSigSingleBOL :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE BUFFER bf-oe-bolh FOR oe-bolh.
DEFINE VARIABLE cImageDir AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cImageFile AS CHARACTER   NO-UNDO.
DEFINE VARIABLE lSigned AS LOGICAL     NO-UNDO.

FIND FIRST bf-oe-bolh 
    WHERE ROWID(bf-oe-bolh) EQ ipriBOLHeader 
    NO-LOCK NO-ERROR. /*get bol header*/
IF AVAIL bf-oe-bolh THEN DO:
    cImageDir = GetImageDir(INPUT bf-oe-bolh.company,
                            INPUT YES /*slash*/). /*Set image file directory*/
    cImageFile = GetSignatureFile(INPUT bf-oe-bolh.bol-no,
                                  INPUT cImageDir). /*set image file path*/
    RUN UpdateBolHeader(BUFFER bf-oe-bolh,
                        INPUT cImageFile,
                        OUTPUT lSigned). /*Check to see if image already exists*/
    IF NOT lSigned AND iplExecuteFTP THEN DO:
        RUN ExecuteFTPGetRenameSingle(INPUT bf-oe-bolh.bol-no,
                                      INPUT cImageDir).  /*Get the image from the web service*/
        RUN UpdateBolHeader(BUFFER bf-oe-bolh,
                            INPUT cImageFile,
                            OUTPUT lSigned). /*Check again to see if image exists*/
    END.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-UpdateBOLHeader) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE UpdateBOLHeader Procedure 
PROCEDURE UpdateBOLHeader :
/*------------------------------------------------------------------------------
  Purpose:    Check for Signature image file, update the BOL header record,
            and set procedure output 
  Parameters:  Buffer for oe-bolh
  Notes:       
------------------------------------------------------------------------------*/
DEFINE PARAMETER BUFFER ipbf-oe-bolh FOR oe-bolh.
DEFINE INPUT PARAMETER ipcImageFile AS CHAR.
DEFINE OUTPUT PARAMETER oplSigned AS LOG.

oplSigned = CheckForSignature(ipcImageFile).
IF oplSigned THEN DO:
    IF ipbf-oe-bolh.spare-int-1 NE 1 THEN DO:
        FIND CURRENT ipbf-oe-bolh EXCLUSIVE-LOCK.
        ipbf-oe-bolh.spare-int-1 = 1.
        FIND CURRENT ipbf-oe-bolh NO-LOCK.
    END.
    opcImageLocation = ipcImageFile.
END.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-CheckForSignature) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CheckForSignature Procedure 
FUNCTION CheckForSignature RETURNS LOGICAL
  ( ipcFileLocation AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE cFile AS CHARACTER   NO-UNDO.

cFile = SEARCH(ipcFileLocation).
RETURN cFile NE ?.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-CheckSlash) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION CheckSlash Procedure 
FUNCTION CheckSlash RETURNS CHARACTER
  ( ipcPath AS CHAR, iplSlash AS LOG) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE cPathOut AS CHAR NO-UNDO.

IF SUBSTRING(ipcPath,LENGTH(ipcPath), 1, "CHARACTER") NE "\" THEN
    IF iplSlash  
        THEN cPathOut = ipcPath + "\".
        ELSE cPathOut = ipcPath.
ELSE
    IF iplSlash  
        THEN cPathOut = ipcPath.
        ELSE cPathOut = SUBSTRING(ipcPath,1,LENGTH(ipcPath) - 1,"CHARACTER").


RETURN cPathOut.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetImageDir) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetImageDir Procedure 
FUNCTION GetImageDir RETURNS CHARACTER
  ( ipcCocode AS CHAR, iplSlash AS LOG ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cImageDir AS CHAR NO-UNDO.
    DEFINE VARIABLE lFound AS LOG NO-UNDO.

    RUN sys\ref\nk1look.p(INPUT ipcCocode,
                          INPUT "BOLSIGN",
                          INPUT "C",
                          INPUT NO,
                          INPUT NO,
                          INPUT "",
                          INPUT "",
                          OUTPUT cImageDir,
                          OUTPUT lFound).
    
    IF NOT lFound THEN
        cImageDir = {&DEFAULT-DIR} .
    
    cImageDir = CheckSlash(cImageDir, iplSlash).
    RETURN cImageDir.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-GetSignatureFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetSignatureFile Procedure 
FUNCTION GetSignatureFile RETURNS CHARACTER
  ( ipiBolNo AS INT, ipcImageDir AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Return the location of of the BOL signature image file
    Notes:  
------------------------------------------------------------------------------*/
DEFINE VARIABLE cOutputFile AS CHAR NO-UNDO.

IF ipcImageDir EQ "" THEN
    ipcImageDir = CheckSlash({&DEFAULT-DIR},YES).
ELSE
    ipcImageDir = CheckSlash(ipcImageDir,YES).

cOutputFile = ipcImageDir + STRING(ipiBolNo) +  "." + {&IMG-TYPE}.

RETURN cOutputFile.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

