/**********************************************************************
 * Copyright (C) 2006-2013 by Consultingwerk Ltd. ("CW") -            *
 * www.consultingwerk.de and other contributors as listed             *
 * below.  All Rights Reserved.                                       *
 *                                                                    *
 *  Software is distributed on an "AS IS", WITHOUT WARRANTY OF ANY    *
 *   KIND, either express or implied.                                 *
 *                                                                    *
 *  Contributors:                                                     *
 *                                                                    *
 **********************************************************************/
/*------------------------------------------------------------------------
    File        : LlHelper
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Oct 23 06:57:17 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i} 

USING Consultingwerk.Framework.Collections.* FROM PROPATH . 
USING Consultingwerk.Windows.ListAndLabel.*  FROM PROPATH .
USING Consultingwerk.Util.*                  FROM PROPATH .   
USING Progress.Lang.*                        FROM PROPATH .

CLASS Consultingwerk.Windows.ListAndLabel.LlHelper: 

    DEFINE STATIC VARIABLE oTempFiles AS CharacterList NO-UNDO .

    PROCEDURE GetTempPathA EXTERNAL "KERNEL32.DLL":
        DEFINE INPUT        PARAMETER intSize     AS LONG.
        DEFINE INPUT-OUTPUT PARAMETER chrBuffer   AS MEMPTR.
        DEFINE RETURN       PARAMETER intRC       AS LONG.
    END PROCEDURE.
    
    PROCEDURE GetTempFileNameA EXTERNAL "KERNEL32.DLL":
        DEFINE INPUT        PARAMETER chrpath     AS CHARACTER.
        DEFINE INPUT        PARAMETER chrPrefix   AS CHARACTER.
        DEFINE INPUT        PARAMETER intUnique   AS LONG.
        DEFINE INPUT-OUTPUT PARAMETER chrTempFile AS MEMPTR.
        DEFINE RETURN       PARAMETER intRC       AS LONG.
    END PROCEDURE.

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation
        Notes:   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE LlHelper ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Cleans up prevouly created temporary file names 
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID CleanupTempFiles ():
    
        IF VALID-OBJECT (oTempFiles) THEN DO:
            {Consultingwerk/foreachPrimitiveList.i Character cFile in oTempFiles}
                OS-DELETE VALUE (cFile) NO-ERROR .
            END.
        END . 
                
        FINALLY:
            GarbageCollectorHelper:DeleteObject(oTempFiles) .         
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Builds a temporay file name use for images (BLOB) fields
        Notes:   Keeps a list of temp-files for clean up 
        @param pcExtension The extension to use for the temp-file
        @return The temporary file name
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER GetTempFileName (pcExtension AS CHARACTER):
        
        DEFINE VARIABLE cFileName AS CHARACTER   NO-UNDO.
        
        DEFINE VARIABLE mPath     AS MEMPTR      NO-UNDO.
        DEFINE VARIABLE mFileName AS MEMPTR      NO-UNDO.
        DEFINE VARIABLE intRC     AS INTEGER     NO-UNDO.
        DEFINE VARIABLE chrBuffer AS CHARACTER   NO-UNDO.

        SET-SIZE(mPath) = 260.
        SET-SIZE(mFileName) = 260.
        
        PUT-STRING(mPath, 1)     = FILL(" ":U, 259).
        PUT-STRING(mFileName, 1) = FILL(" ":U, 259).
        
        RUN GetTempPathA(INPUT 128, INPUT-OUTPUT mPath, OUTPUT intRC).
        
        ASSIGN chrBuffer = GET-STRING(mPath,1) .

        RUN GetTempFileNameA (INPUT  chrBuffer,
                              INPUT  'll':U,
                              INPUT  0,
                              INPUT-OUTPUT mFileName,
                              OUTPUT intRC).

        IF intRc = 0 THEN DO:
            ASSIGN cFileName = ? .
            
            RETURN SUBSTITUTE ("&1&2.&3":U,
                               SESSION:TEMP-DIRECTORY,
                               STRING(RANDOM(1,99999999), "99999999":U),
                               pcExtension) .
        END.

        ASSIGN cFileName = GET-STRING(mFileName,1).

        /* Mike Fechner, Consultingwerk Ltd. 23.10.2012
           Windows creates a file to test and lock the temp-file name
           We can just delete that file here. */
        OS-DELETE VALUE(cFileName) NO-ERROR .

        IF NOT VALID-OBJECT (oTempFiles) THEN 
            oTempFiles = NEW CharacterList () .
    
        oTempFiles:Add (cFileName) . 

        RETURN REPLACE(cFileName, ".tmp":U, ".":U + pcExtension).  

        FINALLY:
            SET-SIZE(mPath) = 0.
            SET-SIZE(mFileName) = 0.                    
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Uses the List & Label Select File Dialog to prompt for a file name 
        Notes:   
        @param piJob List & Label job handle
        @param pcTitle Title for the dialog
        @param piWindowParent The parent Hwnd for the Dialog
        @param pcFileName INPUT-OUTPUT The report file name
        @return Logical value indicating if the user selected Ok
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC LOGICAL PrompfForReportFileName (piJob AS INTEGER,
                                                          pcTitle AS CHARACTER,
                                                          piWindowParent AS INTEGER,  
                                                          INPUT-OUTPUT pcFileName AS CHARACTER):
        
        DEFINE VARIABLE psFileName AS MEMPTR  NO-UNDO.
        DEFINE VARIABLE iRet       AS INTEGER NO-UNDO.

        IF Consultingwerk.CharacterType:IsNullOrEmpty (pcTitle) THEN 
            ASSIGN pcTitle = "Select Report Design"{&TRAN} . 

        IF Consultingwerk.CharacterType:IsNotNullOrEmpty (pcFileName) THEN DO:
            FILE-INFORMATION:FILE-NAME = pcFileName .
            pcFileName = FILE-INFORMATION:FULL-PATHNAME .
        END.
            
        SET-SIZE(psFilename) = 255.
        
        IF Consultingwerk.CharacterType:IsNotNullOrEmpty (pcFileName) THEN DO:
            PUT-STRING (psFileName, 1) = pcFileName .    
            PUT-BYTE (psFileName, LENGTH (pcFileName) + 1) = 0 .            
        END.
        ELSE 
            PUT-BYTE(psFilename, 1) = 0.
            
        iRet = LlApi:SelectFileDlgTitleEx (piJob, 
                                           piWindowParent,
                                           pcTitle,
                                           LlConst:LL_PROJECT_LIST + 
                                           LlConst:LL_FILE_ALSONEW,
                                           INPUT-OUTPUT psFilename, 
                                           255,
                                           0).
                                                     
        IF iRet = LlConst:LL_ERR_USER_ABORTED THEN 
            RETURN FALSE . 

        pcFileName = GET-STRING (psFilename, 1) . 

        RETURN TRUE . 

        FINALLY:
            SET-SIZE (psFileName) = 0 .         
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Sets the List & Label License Info, LL_OPTIONSTR_LICENSINGINFO
        Notes:   Uses ILlLicenseProvider service implementation to return the actual 
                 license info. Use preferably direct after preferably directly after 
                 LlJobOpen()/LlJobOpenLCID()
        @param piJob List & Label job handle
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID SetLicenseInfo (piJob AS INTEGER):
        
        DEFINE VARIABLE oLicenseProvider AS ILlLicenseProvider NO-UNDO . 
        DEFINE VARIABLE cLicenseString   AS CHARACTER          NO-UNDO .
        
        ASSIGN oLicenseProvider = {Consultingwerk/get-service.i Consultingwerk.Windows.ListAndLabel.ILlLicenseProvider} .

        /* Mike Fechner, Consultingwerk Ltd. 23.10.2012
           Don't set License without License Provider */
        IF NOT VALID-OBJECT (oLicenseProvider) THEN 
            RETURN . 

        ASSIGN cLicenseString = oLicenseProvider:GetLicenseInfo () .

        /* Mike Fechner, Consultingwerk Ltd. 23.10.2012
           Don't set License when no license string was returned */
        IF Consultingwerk.CharacterType:IsNullOrEmpty (cLicenseString) THEN 
            RETURN .

        LlApi:SetOptionString (piJob, 
                               LlConst:LL_OPTIONSTR_LICENSINGINFO,
                               cLicenseString) .

    END METHOD .

END CLASS.
