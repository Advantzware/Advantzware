&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v9r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Procedure 
/*------------------------------------------------------------------------
    File        : 
    Purpose     :

    Syntax      :

    Description :

    Author(s)   :
    Created     :
    Notes       :
  ----------------------------------------------------------------------*/
/*          This .W file was created with the Progress AppBuilder.      */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE chPdfAvDocMaster      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chPdfPdDocMaster      AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chPdfAvDocSection     AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chPdfPdDocSection     AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chPdfApp              AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE cMasterFile           AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cSectionFile          AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iMasterNumPages       AS INTEGER    NO-UNDO.
DEFINE VARIABLE iSectionNumPages      AS INTEGER    NO-UNDO.
DEFINE VARIABLE cDefaultPrinter       AS CHARACTER  NO-UNDO.
DEFINE VARIABLE cOutputPrinter        AS CHARACTER  NO-UNDO.
DEFINE VARIABLE iRet                  AS INTEGER    NO-UNDO.

/*
** Win 32 API call to set default printer.
** Win 2000 XP
*/

PROCEDURE SetDefaultPrinterA EXTERNAL "WINSPOOL.DRV":
  DEFINE INPUT  PARAMETER pPrinterName  AS CHAR.
  DEFINE RETURN PARAMETER lReturnVal    AS LONG.
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-PREPROCESSOR-BLOCK 

/* ********************  Preprocessor Definitions  ******************** */

&Scoped-define PROCEDURE-TYPE Procedure
&Scoped-define DB-AWARE no



/* _UIB-PREPROCESSOR-BLOCK-END */
&ANALYZE-RESUME



/* *********************** Procedure Settings ************************ */

&ANALYZE-SUSPEND _PROCEDURE-SETTINGS
/* Settings for THIS-PROCEDURE
   Type: Procedure
   Allow: 
   Frames: 0
   Add Fields to: Neither
   Other Settings: CODE-ONLY
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
/*
** Note this demo lacks proper error handling and most likely proper cleanup.
*/


CREATE WIDGET-POOL.

ASSIGN
cSectionFile = "C:\temp\test2.pdf"
cMasterFile  = "C:\temp\test1.pdf".


/*
** Create the Acrobat Application.
*/
CREATE "AcroExch.App"   chPdfApp          NO-ERROR. 
/*
** Create the Master Acrobat View Document.
*/
CREATE "AcroExch.AVDoc" chPdfAvDocMaster  NO-ERROR.
/*
** Create the Section to be Merged View Document.
*/
CREATE "AcroExch.AVDoc" chPdfAvDocSection NO-ERROR.

MESSAGE VALID-HANDLE(chpdfapp) VALID-HANDLE(chpdfavdocmaster)
       VALID-HANDLE(chpdfavdocsection)
    VIEW-AS ALERT-BOX.

/*
** Open the master document in the acrobat view.
*/  
chPdfAvDocMaster:OPEN(cMasterFile,"Master").

/*
** Display the view in the application window.
*/
chPDFApp:SHOW().

/*
** Open the Merge Section.
*/
chPDfAvDocSection:OPEN(cSectionFile,"Section").
/*
** Get the PDF Representaion objects from the Acrobat View objects
*/
chPdfPdDocMaster  = chPdfAvDocMaster:GetPDDoc().
chPdfPDDocSection = chPdfAvDocSection:GetPDDoc().


/*
** Get the number of pages of each document for the merge.
*/
iMasterNumPages   = chPdfPdDocMaster:GetNumPages().
iSectionNumPages  = chPdfPdDocSection:GetNumPages().

/*
** Merge the section into the bottom of the master.
** Parameter1 Zero Based Master Document Last Page.
** Parameter2 Pdf Section Com Handle
** Parameter3 start page of section - zero based
** Parameter4 number of pages to merge
** Parameter5 0x1000 = PDInsertAll       = 4096 
              0x0001 = PDInsertBookmarks = 1
              0x0002 = PDInsertTHreads   = 2 
*/
chPdfPdDocMaster:InsertPages(iMasterNumPages - 1, 
                             chPdfPdDocSection,
                             0,
                             iSectionNumPages,
                             4096 ).
/*
** Close the section acrobat view and pdf representation.
*/

chPdfPdDocSection:CLOSE().
chPdfAvDocSection:CLOSE(1 /* 1 = TRUE = NO SAVE */).

/*
** Get the new master number of pages for printing.
*/
iMasterNumPages   = chPdfPdDocMaster:GetNumPages().


/*
** Change the windows default printer if it is different than the 
** selected output.
** Note we should probably make a win32 call to get the current 
** default printer because some prior progress program could have
** changed the session printer.
*/
ASSIGN
cDefaultPrinter = SESSION:PRINTER-NAME

cOutputPrinter  = "Midvale2". /* Your Printer Name */

IF cDefaultPrinter <> cOutputPrinter THEN
DO:

  RUN SetDefaultPrinterA
    (INPUT cOutputPrinter,
     OUTPUT iRet) NO-ERROR.

  IF ERROR-STATUS:ERROR OR iRet = 0 THEN
    MESSAGE ERROR-STATUS:GET-MESSAGE(1) VIEW-AS ALERT-BOX.

END.

/*
** Print the master.
** Paramter  1 Start Page
** Parameter 2 Number of Pages
** Parameter 3 Post Script Type 1 or 2
** Parameter 4 Include Binary Post Script Data 
** Parameter 5 Shirnk/Streach to Fit Page
*/
chPdfAvDocMaster:PrintPagesSilent(0,
                                  iMasterNumPages,
                                  2,
                                  FALSE,
                                  TRUE).


/*
** Change the default printer back.
*/
IF cDefaultPrinter <> cOutputPrinter THEN
DO:

  RUN SetDefaultPrinterA
    (INPUT cDefaultPrinter,
     OUTPUT iRet) NO-ERROR.
  IF ERROR-STATUS:ERROR OR iRet = 0 THEN
    MESSAGE ERROR-STATUS:GET-MESSAGE(1) VIEW-AS ALERT-BOX.

END.
/*
** Save the new PDF 
Parameter 1 Flags 0x01 = 1   = PDSaveFull
                  0x20 = 32  = PDSaveCollectGarbage
Parameter 2 FileName
*/

chPdfPdDocMaster:SAVE(33,"C:\New.pdf").
/*
** Close the master acrobat view and pdf representation.
*/
chPdfPdDocMaster:CLOSE().
chPdfAvDocMaster:CLOSE(1).

/*
** Close the Acrobat Application.
*/
chPDFApp:EXIT().

/*
** Clean Up.
*/
RELEASE OBJECT chPDFApp.
RELEASE OBJECT chPdfAvDocMaster.
RELEASE OBJECT chPdfAvDocSection.

DELETE WIDGET-POOL.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


