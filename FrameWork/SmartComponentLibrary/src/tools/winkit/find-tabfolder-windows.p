/*------------------------------------------------------------------------
    File        : find-tabfolder-windows.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Feb 08 11:29:11 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE VARIABLE cCLSID    AS CHARACTER NO-UNDO 
    INIT "~{1EFB6596-857C-11D1-B16A-00C0F0283628~}":U .
DEFINE VARIABLE cFormType AS CHARACTER NO-UNDO 
    INIT "&global-define WinKitFormType Consultingwerk.WindowIntegrationKit.Forms.EmbeddedWindowTabFolderForm".

{Consultingwerk/Util/TempTables/ttFileNames.i}

DEFINE VARIABLE lcFile AS LONGCHAR NO-UNDO.
DEFINE VARIABLE i      AS INTEGER  NO-UNDO.

DEFINE VARIABLE iStart  AS INTEGER NO-UNDO.
DEFINE VARIABLE iEnd    AS INTEGER NO-UNDO.
DEFINE VARIABLE lcBlock AS LONGCHAR NO-UNDO.

DEFINE STREAM filenames . 

/* ***************************  Main Block  *************************** */

Consultingwerk.Util.FileHelper:GetFileList (System.IO.Directory:GetCurrentDirectory(),
                                            "*.w":U,
                                            OUTPUT TABLE ttFileNames BY-REFERENCE) .

OUTPUT STREAM filenames TO tabwindows.txt .

FOR EACH ttFileNames:
    COPY-LOB FROM FILE ttFileNames.FileName TO lcFile .
    
    IF INDEX (lcFile, cCLSID) > 0 THEN . 
    ELSE NEXT .     

    PUT STREAM filenames UNFORMATTED ttFileNames.FileName SKIP . 
    
    
    i = i + 1 .
    
    DISPL i ttFileNames.FileName FORMAT "x(50)":U WITH DOWN . 
/*    PAUSE 0 .*/
    PROCESS EVENTS . 
    
    /* When WinKitFormType not yet set, add it */
    IF INDEX (lcFile, cFormType) = 0 THEN DO:
        
        DISPL "FT" FORMAT "x(2)" WITH DOWN .
/*        PAUSE 0 .*/
        
        iStart = INDEX (lcFile, "&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS":U) .
        iEnd   = INDEX (lcFile, "/* _UIB-CODE-BLOCK-END */":U, iStart) .
        
        lcBlock = SUBSTRING (lcFile, iStart, iEnd - iStart) .
        
        lcBlock = lcBlock + 
                  System.Environment:NewLine +
                  cFormType + 
                  System.Environment:NewLine .  
                  
        SUBSTRING (lcFile, iStart, iEnd - iStart) = lcBlock .
    
        COPY-LOB FROM lcFile TO FILE ttFileNames.FileName  .
    END.    
    
    RUN src/tools/winkit/refactor-unnest-frames.p 
            (ttFileNames.FileName) .    
    
END.
