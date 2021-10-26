
/*------------------------------------------------------------------------
    File        : ArtiosConvertToJPGTester.p
    Purpose     : (run through editor)

    Syntax      :

    Description : Test the ArtiosProcs.p procedure and debug

    Author(s)   : BV
    Created     : Thu Oct 21 16:08:24 EDT 2021
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE cCompany         AS CHARACTER NO-UNDO INITIAL "001".
DEFINE VARIABLE cEstimateNo      AS CHARACTER NO-UNDO INITIAL "    2544".

DEFINE VARIABLE cJPGDirectory    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCADDirectory    AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCadFile         AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCadFileFullPath AS CHARACTER NO-UNDO.
DEFINE VARIABLE cJpgFileFullPath AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdArtios         AS HANDLE    NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

RUN est\ArtiosProcs.p PERSISTENT SET hdArtios.

RUN pGetCADFile(cCompany, cEstimateNo, OUTPUT cCadFile).
RUN pGetCADDirectory(cCompany, OUTPUT cCADDirectory).
RUN pPromptForCadFile(cCadDirectory, OUTPUT cCadFileFullPath).
RUN pGetJPGDirectory(cCompany, OUTPUT cJPGDirectory).
cJpgFileFullPath = cJPGDirectory + "\" + cCadFile + ".jpg".
RUN Artios_ConvertARDToJPG IN hdArtios (cCadFileFullPath, cJpgFileFullPath).
    
MESSAGE "Cad: " cCadFile SKIP 
    "Cad Path: " cCadDirectory SKIP
    "JPG Path: " cJPGDirectory SKIP
    "Full CADFile:  " cCadFileFullPath SKIP
    "Full JPGFile: " cJpgFileFullPath
    VIEW-AS ALERT-BOX.


/* **********************  Internal Procedures  *********************** */

PROCEDURE pGetCadFile PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Given an estimate, get the CadFile from the first form/blank
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstimateNo AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCadFile AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-box-design-hdr FOR box-design-hdr.
    
    FIND FIRST eb NO-LOCK
        WHERE eb.company EQ ipcCompany
        AND eb.est-no EQ ipcEstimateNo
        AND eb.form-no EQ 1
        AND eb.blank-no EQ 1
        NO-ERROR.
    FIND FIRST bf-box-design-hdr NO-LOCK 
        WHERE bf-box-design-hdr.design-no EQ 0 
        AND bf-box-design-hdr.company   EQ eb.company 
        AND bf-box-design-hdr.est-no    EQ eb.est-no     
        AND bf-box-design-hdr.form-no   EQ eb.form-no
        AND bf-box-design-hdr.blank-no  EQ eb.blank-no 
        NO-ERROR.
    IF AVAILABLE eb AND AVAILABLE bf-box-design-hdr THEN 
        opcCadFile = eb.cad-no.

END PROCEDURE.

PROCEDURE pGetJPGDirectory PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Gets the CADFILE character value
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFilePath AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.
    
    RUN sys/ref/nk1look.p (
        INPUT ipcCompany,         /* Company Code */ 
        INPUT "CADFILE",   /* sys-ctrl name */
        INPUT "C",                /* Output return value */
        INPUT NO,                 /* Use ship-to */
        INPUT NO,                 /* ship-to vendor */
        INPUT "",                 /* ship-to vendor value */
        INPUT "",                 /* shi-id value */
        OUTPUT cReturn, 
        OUTPUT lFound
        ).    
    IF lFound THEN
        opcFilePath = RIGHT-TRIM(RIGHT-TRIM(cReturn,"\"),"/").
        
    IF opcFilePath NE "" THEN 
        opcFilePath = opcFilePath + "\".
        
END PROCEDURE.

PROCEDURE pGetCADDirectory PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:  Gets the CADFILE character value
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFilePath AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.
    
    RUN sys/ref/nk1look.p (
        INPUT ipcCompany,         /* Company Code */ 
        INPUT "ArtiosCAD",   /* sys-ctrl name */
        INPUT "C",                /* Output return value */
        INPUT NO,                 /* Use ship-to */
        INPUT NO,                 /* ship-to vendor */
        INPUT "",                 /* ship-to vendor value */
        INPUT "",                 /* shi-id value */
        OUTPUT cReturn, 
        OUTPUT lFound
        ).    
    IF lFound THEN
        opcFilePath = RIGHT-TRIM(RIGHT-TRIM(cReturn,"\"),"/").
        
    IF opcFilePath NE "" THEN 
        opcFilePath = opcFilePath + "\".
        
END PROCEDURE.

PROCEDURE pPromptForCadFile PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/ 
    DEFINE INPUT PARAMETER ipcInitDirectory AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFullCadFile AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lOKClicked     AS LOGICAL NO-UNDO.
    DEFINE VARIABLE iInitialFilter AS INTEGER NO-UNDO.
  
    
    iInitialFilter = 2.
     
    SYSTEM-DIALOG GET-FILE opcFullCadFile 
        TITLE 'Select Image File to insert'
        FILTERS 'JPG Files    (*.jpg)' '*.jpg',
        'ARD Files    (*.ARD)' '*.ARD',
        'Bitmap files (*.bmp)' '*.bmp',
        'JPEG Files   (*.jpeg)' '*.jpeg',
        'TIF Files    (*.tif)' '*.tif',
        'All Files    (*.*) ' '*.*'  
        INITIAL-FILTER iInitialFilter
        INITIAL-DIR ipcInitDirectory                      
        MUST-EXIST USE-FILENAME UPDATE lOKClicked.      

END PROCEDURE.

