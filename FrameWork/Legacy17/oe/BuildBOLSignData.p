&ANALYZE-SUSPEND _VERSION-NUMBER AB_v10r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : oe/BuildBolSignData.p
    Purpose     : Create an XML or JSON file of the current BOLs in the
                  system that a pending (based on criteria).

    Syntax      :

    Description :

    Author(s)   :  BV
    Created     :  10/15/2013
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER ipcCocode AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ipcOutputType AS CHAR NO-UNDO. /*XML or JSON*/
DEFINE INPUT PARAMETER iplPrinted AS LOG NO-UNDO.
DEFINE INPUT PARAMETER iplExecuteFTP AS LOG NO-UNDO.
DEFINE INPUT PARAMETER iplBuildFTPFile AS LOG NO-UNDO.

DEFINE TEMP-TABLE ttOEBol
    FIELD BolNo LIKE oe-bolh.bol-no
    FIELD Trailer LIKE oe-bolh.trailer
    FIELD ShipName LIKE shipto.ship-name
    FIELD ShipAdd AS CHAR
    FIELD ShipCity LIKE shipto.ship-city
    FIELD ShipState LIKE shipto.ship-state
    FIELD ShipZip LIKE shipto.ship-zip
    FIELD AreaCode LIKE shipto.area-code
    FIELD Phone LIKE shipto.phone
    FIELD Contact LIKE shipto.contact
    FIELD Notes AS CHAR.
    .
DEFINE TEMP-TABLE ttOEBolLine
    FIELD BolNo LIKE oe-boll.bol-no
    FIELD INo LIKE oe-boll.i-no
    FIELD BolDate LIKE oe-boll.bol-date
    FIELD PartNo LIKE itemfg.part-no
    FIELD QtyCase LIKE oe-boll.qty-case
    FIELD OrdNo LIKE oe-boll.ord-no
    FIELD TotQty LIKE oe-boll.tot-qty
    FIELD Qty LIKE oe-boll.qty
    FIELD Weight LIKE oe-boll.weight
    FIELD PoNo LIKE oe-boll.po-no
    FIELD TotPallets LIKE oe-boll.tot-pallets
    FIELD JobNo LIKE oe-boll.job-no
    FIELD JobNo2 LIKE oe-boll.job-no2
    .
  
DEFINE VARIABLE gcOutputFileBOL AS CHAR NO-UNDO.
DEFINE VARIABLE gcOutputFileBOLLine AS CHAR NO-UNDO.

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

&IF DEFINED(EXCLUDE-GetOutputFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD GetOutputFile Procedure 
FUNCTION GetOutputFile RETURNS CHARACTER
  ( ipcData AS CHAR, ipcExtension AS CHAR )  FORWARD.

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
         HEIGHT             = 15
         WIDTH              = 60.
/* END WINDOW DEFINITION */
                                                                        */
&ANALYZE-RESUME

 


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK Procedure 


/* ***************************  Main Block  *************************** */

RUN BuildTempTables.

IF ipcOutputType EQ "XML" THEN
    RUN WriteBOLXML.
ELSE
    RUN WriteBOLJSON.

RUN ExecuteFTP.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&IF DEFINED(EXCLUDE-BuildTempTables) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE BuildTempTables Procedure 
PROCEDURE BuildTempTables :
/*------------------------------------------------------------------------------
  Purpose:     Generate ttOEBol and ttOEBOlLine
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE VARIABLE xBolNo LIKE oe-bolh.bol-no NO-UNDO.
DEFINE VARIABLE xINo LIKE oe-boll.i-no NO-UNDO.
DEFINE VARIABLE lLineExists AS LOG NO-UNDO.

DEFINE BUFFER bf-oe-bolh FOR oe-bolh.
DEFINE BUFFER bf-oe-boll FOR oe-boll.
DEFINE BUFFER bf-shipto FOR shipto.
DEFINE BUFFER bf-itemfg FOR itemfg.

FOR EACH bf-oe-bolh 
    WHERE bf-oe-bolh.company EQ ipcCocode
      AND bf-oe-bolh.posted EQ NO
      AND (iplPrinted EQ ? OR bf-oe-bolh.printed EQ iplPrinted)
      AND bf-oe-bolh.bol-no GT 0
      AND bf-oe-bolh.spare-int-1 NE 1 /*not signed*/
    NO-LOCK,
    EACH bf-oe-boll
        WHERE bf-oe-boll.company EQ bf-oe-bolh.company
          AND bf-oe-boll.b-no EQ bf-oe-bolh.b-no
        NO-LOCK:
    xBolNo = bf-oe-bolh.bol-no.

    xINo = bf-oe-boll.i-no.
    FIND FIRST ttOEBolLine 
        WHERE ttOEBolLine.BolNo EQ xBolNo 
          AND ttOEBolLine.INo EQ xINo
        NO-LOCK NO-ERROR.
    IF AVAIL ttOEBolLine THEN DO:
        ASSIGN 
            ttOEBolLine.QtyCase = ttOEBolLine.QtyCase + bf-oe-boll.qty-case
            ttOEBolLine.TotQty = ttOEBolLine.TotQty + bf-oe-boll.tot-qty
            ttOEBolLine.Qty = ttOEBolLine.Qty + bf-oe-boll.qty
            ttOEBOlLine.Weight = ttOEBOlLine.Weight + bf-oe-boll.weight
            ttOEBolLine.TotPallets = ttOEBolLine.TotPallets + bf-oe-boll.tot-pallets
            .
    END. /*ttOEBolLine Exists*/
    ELSE DO:
        FIND FIRST bf-itemfg
            WHERE bf-itemfg.company EQ ipcCocode
              AND bf-itemfg.i-no EQ xINo
            NO-LOCK NO-ERROR.
        CREATE ttOEBolLine.
        ASSIGN 
            ttOEBolLine.BolNo = xBolNo
            ttOEBolLine.INo = xINo
            ttOEBolLine.BolDate = bf-oe-boll.bol-date
            ttOEBolLine.OrdNo = bf-oe-boll.ord-no
            ttOEBolLine.PoNo = bf-oe-boll.po-no
            ttOEBolLine.JobNo = bf-oe-boll.job-no
            ttOEBolLine.JobNo2 = bf-oe-boll.job-no2
            ttOEBolLine.QtyCase = bf-oe-boll.qty-case
            ttOEBolLine.TotQty = bf-oe-boll.tot-qty
            ttOEBolLine.Qty = bf-oe-boll.qty
            ttOEBOlLine.Weight = bf-oe-boll.weight
            ttOEBolLine.TotPallets = bf-oe-boll.tot-pallets
            .
        IF AVAIL bf-itemfg THEN ttOEBolLine.PartNo = bf-itemfg.part-no.
    END. /*ttOEBolLine Doesn't Exist*/
    FIND FIRST ttOEBol
        WHERE ttOEBOL.BolNo EQ xBolNo
        NO-LOCK NO-ERROR.
    IF NOT AVAIL ttOEBol THEN DO:
        FIND FIRST bf-shipto 
            WHERE bf-shipto.company EQ ipcCocode
              AND bf-shipto.cust-no EQ bf-oe-bolh.cust-no
              AND bf-shipto.ship-id EQ bf-oe-bolh.ship-id
            NO-LOCK NO-ERROR.
        
        CREATE ttOEBol.
        ASSIGN 
            ttOEBol.BolNo = xBolNo
            ttOEBol.Trailer = bf-oe-bolh.trailer
            ttOEBol.Notes = bf-oe-bolh.ship-i[1] + " " + 
                            bf-oe-bolh.ship-i[2] + " " +
                            bf-oe-bolh.ship-i[3] + " " +
                            bf-oe-bolh.ship-i[4]
            .
        IF AVAIL bf-shipto THEN
            ASSIGN 
                ttOEBol.ShipName = bf-shipto.ship-name
                ttOEBol.ShipAdd = bf-shipto.ship-addr[1] + ", " + bf-shipto.ship-addr[2]
                ttOEBol.ShipCity = bf-shipto.ship-city
                ttOEBol.ShipState = bf-shipto.ship-state
                ttOEBol.ShipZip = bf-shipto.ship-zip
                ttOEBol.AreaCode = bf-shipto.area-code
                ttOEBol.Phone = bf-shipto.phone
                ttOEBol.Contact = bf-shipto.contact
                .
    END. /*not avail ttOEBol*/
END. /*each oe-bolh, oe-boll*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-ExecuteFTP) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ExecuteFTP Procedure 
PROCEDURE ExecuteFTP :
/*------------------------------------------------------------------------------
  Purpose:    Execute the FTP to send data file to Trucker app web server 
  Parameters:  
  Notes:       
------------------------------------------------------------------------------*/

IF iplBuildFTPFile THEN DO:
    /*Build FTP instruction file*/
    OUTPUT TO VALUE(".\oe\BolSignFTP.txt").    /* ftp text file */   
    PUT UNFORMATTED 
      "open " {&FTP-SERVER} SKIP   /* ftp server ip address */
      {&FTP-USERID} SKIP   /* userid */
      {&FTP-USERPASS} SKIP   /* password */
      "put " gcOutputFileBol SKIP   /* transfer BOL file */
      "put " gcOutputFileBolLine SKIP   /* transfer BOL Line File */
      "quit" .
    OUTPUT CLOSE.
END.

IF iplExecuteFTP THEN
    OS-COMMAND SILENT VALUE("ftp -v -i -s:.\oe\BolSignFTP.txt").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WriteBOLJSON) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WriteBOLJSON Procedure 
PROCEDURE WriteBOLJSON :
/*------------------------------------------------------------------------------
  Purpose:     Output temp tables into JSON
  Parameters:  <none>
  Notes:       WRITE-JSON only supported in Progress 10.2
------------------------------------------------------------------------------*/

DEFINE VARIABLE cTargetType     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFormatted      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lRetOK          AS LOGICAL   NO-UNDO. 

ASSIGN  
    cTargetType     = "file"  
    lFormatted      = TRUE  .

gcOutputFileBOL = GetOutputFile("Headers","json"). 
/* lRetOK = TEMP-TABLE ttOEBOL:WRITE-JSON(cTargetType,       */
/*                                          gcOutputFileBOL, */
/*                                          lFormatted).     */

gcOutputFileBOLLine = GetOutputFile("Lines","json").
/* lRetOK = TEMP-TABLE ttOEBOLLine:WRITE-JSON(cTargetType,        */
/*                                          gcOutputFileBOLLine,  */
/*                                          lFormatted).          */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

&IF DEFINED(EXCLUDE-WriteBOLXML) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE WriteBOLXML Procedure 
PROCEDURE WriteBOLXML :
/*------------------------------------------------------------------------------
  Purpose:     Output temp tables into XML
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

DEFINE VARIABLE cTargetType     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFile           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFormatted      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cEncoding       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSchemaLocation AS CHARACTER NO-UNDO.
DEFINE VARIABLE lWriteSchema    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lMinSchema      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lRetOK          AS LOGICAL   NO-UNDO. 

ASSIGN  
    cTargetType     = "file"  
    lFormatted      = TRUE  
    cEncoding       = ?  
    cSchemaLocation = ?  
    lWriteSchema    = FALSE  
    lMinSchema      = FALSE. 

gcOutputFileBOL = GetOutputFile("Headers","xml"). 
lRetOK = TEMP-TABLE ttOEBOL:WRITE-XML(cTargetType, 
                                         gcOutputFileBOL, 
                                         lFormatted, 
                                         cEncoding,  
                                         cSchemaLocation, 
                                         lWriteSchema, 
                                         lMinSchema).
gcOutputFileBOLLine = GetOutputFile("Lines","xml").
lRetOK = TEMP-TABLE ttOEBOLLine:WRITE-XML(cTargetType, 
                                         gcOutputFileBOLLine, 
                                         lFormatted, 
                                         cEncoding,  
                                         cSchemaLocation, 
                                         lWriteSchema, 
                                         lMinSchema).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

/* ************************  Function Implementations ***************** */

&IF DEFINED(EXCLUDE-GetOutputFile) = 0 &THEN

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION GetOutputFile Procedure 
FUNCTION GetOutputFile RETURNS CHARACTER
  ( ipcData AS CHAR, ipcExtension AS CHAR ) :
/*------------------------------------------------------------------------------
  Purpose:  Return the location of the file for TT serialization
    Notes:  
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cOutputFile AS CHAR NO-UNDO.

    cOutputFile = {&DEFAULT-DIR-TMP} + "BOLSign" + ipcData + "." + ipcExtension.

  RETURN cOutputFile.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ENDIF

