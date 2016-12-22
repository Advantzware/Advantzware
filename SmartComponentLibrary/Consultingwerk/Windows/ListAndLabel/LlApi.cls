/*********************************************************************
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
    File        : ListAndLabelApi
    Purpose     : Class based wrapper to List & Label routines
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Oct 12 19:21:34 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Windows.ListAndLabel.* FROM PROPATH .  
USING Progress.Lang.*                       FROM PROPATH .

CLASS Consultingwerk.Windows.ListAndLabel.LlApi: 

    &IF DEFINED (ListAndLabelVersion) EQ 0 &THEN
    &GLOBAL-DEFINE ListAndLabelVersion 18
    &ENDIF

    {Consultingwerk/Windows/ListAndLabel/cmLL{&ListAndLabelVersion}.i}

    /*------------------------------------------------------------------------------
        Purpose: Private constructor for the LlApi class
        Notes:   Disallow instance creation
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE LlApi ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Defines a list field and it's content
        Notes:   
        @param hLlJob The List & Label Job reference
        @param pszVarName The Field Name
        @param lpbufContents The Field Value 
        @return Error code
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC INTEGER DefineField (hLlJob AS INTEGER, 
                                              pszVarName AS CHARACTER,
                                              lpbufContents AS CHARACTER):
        
        DEFINE VARIABLE rc AS INTEGER NO-UNDO.

        RUN LLDefineField (OUTPUT rc, hLlJob, pszVarName, lpbufContents) .
        
        RETURN rc . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Defines a list field and it's content
        Notes:   
        @param hLlJob The List & Label Job reference
        @param pszVarName The Field Name
        @param lpbufContents The Field Value 
        @param lPara The type of the field
        @param lpPtr Reserved for future enhancements
        @return Error code
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC INTEGER DefineFieldExt (hLlJob AS INTEGER, 
                                                 pszVarName AS CHARACTER,
                                                 lpbufContents AS CHARACTER,
                                                 lPara AS INTEGER, 
                                                 lpPtr AS INTEGER):
        
        DEFINE VARIABLE rc AS INTEGER NO-UNDO.

        RUN LLDefineFieldExt (OUTPUT rc, hLlJob, pszVarName, lpbufContents, lPara, lpPtr) .
        
        RETURN rc . 

    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Clears the internal field buffer to clear outdated definitions 
        Notes:   
        @param hLlJob The List & Label Job reference
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID DefineFieldStart (hLlJob AS INTEGER):
        
        RUN LlDefineFieldStart (hLlJob).
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Calls the interactive Designer that will be displayed as a modal 
                 pop-up window overlapping your application window.
        Notes:   The window handle is used to deactivate the calling program.
                 If this is not desired or possible, NULL can also be passed. In this 
                 case the calling program is responsible for closing the layout editor, s
                 hould the user abort the main program. This is not recommended.
                 When the List & Label layout Designer is minimized, the calling program 
                 is also automatically minimized; when the Designer is subsequently restored, 
                 List & Label is also restored.
        @param hLlJob List & Label job handle
        @param hWnd Handle of the application window which will be disabled while the Designer is being displayed.
        @param pszTitle Window title
        @param nObjType Project type
        @param pszObjName The name of the layout file
        @return Error code 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC INTEGER DefineLayout (hLlJob AS INTEGER, 
                                               hWnd AS INTEGER,
                                               pszTitle AS CHARACTER,
                                               nObjType AS INTEGER, 
                                               pszObjName AS CHARACTER):
                                                
        DEFINE VARIABLE rc                AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cWorkingDirectory AS CHARACTER NO-UNDO.
        
        ASSIGN cWorkingDirectory = System.Environment:CurrentDirectory .

        RUN LlDefineLayout (OUTPUT rc, 
                            hLlJob, 
                            hWnd, 
                            pszTitle,
                            nObjType,
                            pszObjName).
        
        RETURN rc . 

        FINALLY:
            System.Environment:CurrentDirectory = cWorkingDirectory .       
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Defines a List and Label variable
        Notes:   
        @param hLlJob The List & Label Job reference
        @param pszVarName The Field Name
        @param lpbufContents The Field Value 
        @return Error code
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC INTEGER DefineVariable (hLlJob AS INTEGER, 
                                                 pszVarName AS CHARACTER,
                                                 lpbufContents AS CHARACTER):
        
        DEFINE VARIABLE rc AS INTEGER NO-UNDO.

        RUN LLDefineVariable (OUTPUT rc, hLlJob, pszVarName, lpbufContents) .
        
        RETURN rc . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Defines an extended variable
        Notes:   
        @param hLlJob The List & Label Job reference
        @param pszVarName The Field Name
        @param lpbufContents The Field Value 
        @param lPara The type of the variable
        @param lpPtr Reserved for future enhancements
        @return Error code
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC INTEGER DefineVariableExt (hLlJob AS INTEGER, 
                                                    pszVarName AS CHARACTER,
                                                    lpbufContents AS CHARACTER,
                                                    lPara AS INTEGER, 
                                                    lpPtr AS INTEGER):
        
        DEFINE VARIABLE rc AS INTEGER NO-UNDO.

        RUN LLDefineVariableExt (OUTPUT rc, hLlJob, pszVarName, lpbufContents, lPara, lpPtr) .
        
        RETURN rc . 

    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Clears the internal variable buffer to clear outdated definitions
        Notes:   
        @param hLlJob The List & Label Job reference
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID DefineVariableStart (hLlJob AS INTEGER):
        
        RUN LlDefineVariableStart (hLlJob).

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Closes a List & Label job
        Notes:   
        @param hLlJob The List & Label Job reference
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID JobClose (hLlJob AS INTEGER):
        
        RUN LlJobClose (hLlJob) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Opens a List & Label Job
        Notes:   Almost all List & Label API's need the return value of this method
                 as a parameter   
        @param nLanguage The language for the List & Lable UI
        @return The Job handle or job number
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC INTEGER JobOpen (nLanguage AS INTEGER):
        
        DEFINE VARIABLE rc AS INTEGER NO-UNDO.

        RUN LLJobOpen (OUTPUT rc, nLanguage) .
        
        RETURN rc . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Starts the preview window.
        Notes:   
        @param hLlJob List & Label job handle
        @param pszObjName Valid file name without path name!
        @param pszPath Valid path of the preview files ending with a backslash "\".
        @param hWnd Window handle of the calling program
        @return Error code 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC INTEGER PreviewDisplay (hLlJob AS INTEGER,
	                                             pszObjName AS CHARACTER,
	                                             pszPath AS CHARACTER,
	                                             hWnd AS INTEGER):  
	                                             
        DEFINE VARIABLE rc AS INTEGER NO-UNDO .      

        RUN LlPreviewDisplay (OUTPUT rc, 
                              hLlJob,
                              pszObjName,
                              pszPath,
                              hWnd) .

        RETURN rc . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Deletes the temporary file(s) which have been created by the preview print.
        Notes:   
        @param hLlJob List & Label job handle
        @param pszObjName Valid file name without path name!
        @param pszPath Valid path of the preview files ending with a backslash "\".
        @return Error code 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC INTEGER PreviewDeleteFiles (hLlJob AS INTEGER,
                                                 pszObjName AS CHARACTER,
                                                 pszPath AS CHARACTER):  
                                                 
        DEFINE VARIABLE rc AS INTEGER NO-UNDO .      

        RUN LlPreviewDeleteFiles (OUTPUT rc, 
                                  hLlJob,
                                  pszObjName,
                                  pszPath) .

        RETURN rc . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Output of all objects on the printer.
        Notes:   Normal objects and the header of a table object (see option 
                 LL_OPTION_DELAYTABLEHEADER) are printed. A table object has to be 
                 filled with calls of LlPrintFields() afterwards. LlPrint is responsible 
                 for a page break.
                 Label/card projects: As long as LlPrint() returns LL_WRN_REPEAT_DATA, 
                 LlPrint() must be called again, so objects that have caused a page break 
                 must be printed again on the next label /page.
                 This function is described explicitly in the chapter "Further Programming 
                 Basics"
        @param hLlJob List & Label job handle
        @return Error code
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC INTEGER Print (hLlJob AS INTEGER):
        
        DEFINE VARIABLE rc AS INTEGER   NO-UNDO.

        RUN LlPrint (OUTPUT rc,
                     hLlJob) . 

        RETURN rc . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Ends the print job
        Notes:   
        @param hLlJob List & Label job handle
        @param nPages Number of empty pages desired after the print
        @return Error code
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC INTEGER PrintEnd (hLlJob AS INTEGER,
	                                       nPages AS INTEGER):
		
        DEFINE VARIABLE rc AS INTEGER   NO-UNDO.
		
		RUN LlPrintEnd (OUTPUT rc,
		                hLlJob,
		                nPages) .

        RETURN rc . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Output of a table line
        Notes:   With the return value LL_WRN_REPEAT_DATA, List & Label informs you 
                 that you have to start a new page for the entry. With the corresponding 
                 LlPrint() on the next page the record pointer should not be moved to 
                 the next record. 
                 If more tables are added via LlDbAddTable() the return value also can 
                 be LL_WRN_TABLECHANGE. Please refer to chapter "5.5. Printing 
                 Relational Data" for further information.
        @param hLlJob List & Label job handle
        @return Error code or command
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC INTEGER PrintFields (hLlJob AS INTEGER):
		
        DEFINE VARIABLE rc AS INTEGER   NO-UNDO.

        RUN LlPrintFields (OUTPUT rc,
                           hLlJob) . 

        RETURN rc . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns the various print options which are set by the user in the 
                 LlPrintOptionsDialog() 
        Notes:   
        @param hLlJob List & Label job handle
        @param nIndex LL_PRNOPTSTR_EXPORT, LL_PRNOPTSTR_ ISSUERANGES, LL_PRNOPTSTR_ PAGERANGES, LL_PRNOPTSTR_PRINTDST_FILENAME, LL_PRNOPTSTR_PRINTJOBNAME,    
        @return Setting chosen by the user
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC INTEGER PrintGetOption (hLlJob AS INTEGER, 
	                                             nIndex AS INTEGER):
		
        DEFINE VARIABLE rc AS INTEGER   NO-UNDO.

		RUN LlGetOption (OUTPUT rc,
		                 hLlJob,
		                 nIndex) .
		                 
        RETURN rc . 		                 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Returns various print option string settings.
        Notes:   
        @param hLlJob List & Label job handle
        @param nIndex     
        @return The Option Value 
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC CHARACTER PrintGetOptionString (hLlJob AS INTEGER, 
                                                         nIndex AS INTEGER):
		
        DEFINE VARIABLE m  AS MEMPTR  NO-UNDO.
        DEFINE VARIABLE rc AS INTEGER NO-UNDO.
		
		SET-SIZE (m) = 256 .
		
		RUN LlPrintGetOptionString (OUTPUT rc,
		                            hLlJob, 
		                            nIndex, 
		                            INPUT-OUTPUT m,
		                            256) .

        RETURN TRIM (GET-STRING (m, 1)) .

        FINALLY:
            SET-SIZE (m) = 0 . 		
        END FINALLY.

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Calls a print option selection window and enables the user to select 
                 print-specific settings.
        Notes:   
        @param hLlJob List & Label job handle
        @param hWnd Window handle of the calling program
        @param lpszText Text to be passed in the dialog, e.g. 'Only 55 labels will be printed'
        @return Error code
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC INTEGER PrintOptionsDialog (hLlJob AS INTEGER,   
                                                     hWnd AS INTEGER,     
                                                     lpszText AS CHARACTER):

        DEFINE VARIABLE rc AS INTEGER   NO-UNDO.
        
        RUN LlPrintOptionsDialog (OUTPUT rc, 
                                  hLlJob,
                                  hWnd,
                                  lpszText) .

        RETURN rc . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Calls a print option selection window and enables the user to select 
                 print-specific settings.
        Notes:   
        @param hLlJob List & Label job handle
        @param hWnd Window handle of the calling program
        @param lpszTitle Dialog title
        @param lpszText Text to be passed in the dialog, e.g. 'Only 55 labels will be printed'
        @return Error code
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC INTEGER PrintOptionsDialogTitle (hLlJob AS INTEGER,   
                                                          hWnd AS INTEGER,     
                                                          lpszTitle AS CHARACTER, 
                                                          lpszText AS CHARACTER):

        DEFINE VARIABLE rc AS INTEGER   NO-UNDO.
        
        RUN LlPrintOptionsDialogTitle (OUTPUT rc, 
                                       hLlJob,
                                       hWnd,
                                       lpszTitle,
                                       lpszText) .

        RETURN rc . 

    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: 
        Notes:   
        @param hLlJob List & Label job handle
        @param lpszText Text which should appear in the box
        @param nPercantage Progress percentage
        @return Error code
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC INTEGER PrintSetBoxText (hLlJob AS INTEGER,
	                                              lpszText AS CHARACTER,
	                                              nPercantage AS INTEGER):
		
        DEFINE VARIABLE rc AS INTEGER   NO-UNDO.

        RUN LlPrintSetBoxText (OUTPUT rc, 
                               hLlJob,
                               lpszText,
                               nPercantage) .

        RETURN rc . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Sets various print options for the print job or the print options 
                 dialog, for example to preset the number of copies required.
        Notes:   
        @param hLlJob List & Label job handle
        @param nIndex LL_PRNOPTSTR_EXPORT, LL_PRNOPTSTR_ ISSUERANGES, LL_PRNOPTSTR_ PAGERANGES, LL_PRNOPTSTR_PRINTDST_FILENAME, LL_PRNOPTSTR_PRINTJOBNAME,    
        @param nValue Sets the option corresponding to the nIndex
        @return Error code
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC INTEGER PrintSetOption (hLlJob AS INTEGER,
		                                         nIndex AS INTEGER,
                                                 nValue AS INTEGER).
        
        DEFINE VARIABLE rc AS INTEGER   NO-UNDO.
        
        RUN LlPrintSetOption (OUTPUT rc,
                              hLlJob,
                              nIndex,
                              nValue) .

        RETURN rc . 

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Sets various print options for List & Label
        Notes:   
        @param hLlJob List & Label job handle
        @param nIndex LL_PRNOPT_COPIES, LL_PRNOPT_PAGE, LL_PRNOPT_OFFSET, LL_PRNOPT_FIRSTPAGE, LL_PRNOPT_LASTPAGE, LL_PRNOPT_PRINTDLG_ONLYPRINTERCOPIES, LL_PRNOPT_UNITS  
        @param pszBuffer The new value
        @return Error code
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC INTEGER PrintSetOptionString (hLlJob AS INTEGER,
                                                       nIndex AS INTEGER,
                                                       pszBuffer AS CHARACTER).
        
        DEFINE VARIABLE rc AS INTEGER   NO-UNDO.
        
        RUN LlPrintSetOptionString (OUTPUT rc,
                                    hLlJob,
                                    nIndex,
                                    pszBuffer) .

        RETURN rc . 

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Starts the print job and opens the project file. Supports an abort 
                 window.
        Notes:   
        @param hLlJob Job handle   
        @param nObjType LL_PROJECT_LABEL, LL_PROJECT_LIST or LL_PROJECT_CARD
        @param pszObjName The file name of the project
        @param nPrintOptions Print options, like LL_PRINT_NORMAL, LL_PRINT_PREVIEW, LL_PRINT_FILE, LL_PRINT_EXPORT 
        @param nBoxType The dialog type
        @param hWnd Window handle of the calling program (used as parent of the dialog box)
        @param pszTitle Title of the abort dialog box, also appears as text in the print manager
        @return Error code
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC INTEGER PrintWithBoxStart (hLlJob AS INTEGER,   
                                                    nObjType AS INTEGER,
                                                    pszObjName AS CHARACTER,
                                                    nPrintOptions AS INTEGER, 
                                                    nBoxType AS INTEGER,
                                                    hWnd AS INTEGER,
                                                    pszTitle AS CHARACTER):

        DEFINE VARIABLE rc AS INTEGER   NO-UNDO.
		
        RUN LlPrintWithBoxStart (OUTPUT rc,
                                 hLlJob,
                                 nObjType,
                                 pszObjName,
                                 nPrintOptions,
                                 nBoxType,
                                 hWnd,
                                 pszTitle) . 
		
		RETURN rc .

	END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Opens a file select dialog with an integrated preview window
        Notes:   
        @param hLlJob Job handle   
        @param hWnd The handle of the parent window
        @param pszTitle The title for the dialog
        @param nObjType The list and label object type, LL_PROJECT_LABEL, LL_PROJECT_CARD, LL_PROJECT_LIST
        @param pszObjName INPUT-OUTPUT The object name to select
        @param nBufSize The size of the object name buffer
        @param pReserved Reserved for future use
        @return Error code
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC INTEGER SelectFileDlgTitleEx (hLlJob AS INTEGER,
                                                       hWnd AS INTEGER,
                                                       pszTitle AS CHARACTER,
                                                       nObjType AS INTEGER,
                                                       INPUT-OUTPUT pszObjName AS MEMPTR,
                                                       nBufSize AS INTEGER,
                                                       pReserved AS INTEGER):
        
        DEFINE VARIABLE rc                AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cWorkingDirectory AS CHARACTER NO-UNDO.
        
        ASSIGN cWorkingDirectory = System.Environment:CurrentDirectory .

        RUN LlSelectFileDlgTitleEx (OUTPUT rc, 
                                    hLlJob, 
                                    hWnd, 
                                    pszTitle, 
                                    nObjType, 
                                    INPUT-OUTPUT pszObjName, 
                                    nBufSize, 
                                    pReserved).
        
        RETURN rc . 
        
        FINALLY:
            System.Environment:CurrentDirectory = cWorkingDirectory .         
        END FINALLY.
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Sets diverse options in List & Label
        Notes:   Please call this function before LlDefineLayout() and before the 
                 functions LlPrint...Start(), preferably directly after LlJobOpen()/
                 LlJobOpenLCID(). 
        @param hLlJob List & Label job handle
        @param nMode Mode index
        @param nValue New value
        @return Error code
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC INTEGER SetOption (hLlJob AS INTEGER,
                                            nMode AS INTEGER,
                                            nValue AS INTEGER):
        
        DEFINE VARIABLE rc AS INTEGER NO-UNDO.

        RUN LlSetOption (OUTPUT rc, hLlJob, nMode, nValue) .
        
        RETURN rc . 
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Sets string options in List & Label
        Notes:   Most of the options need to be set before LlDefineLayout()and before 
                 the functions LlPrint...Start(), preferably directly after LlJobOpen()/
                 LlJobOpenLCID(). If an option needs to be set at a different time, 
                 this will be stated in that option's description. 
        @param hLlJob List & Label job handle
        @param nMode Mode index
        @param pcValue New value
        @return Error code
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC INTEGER SetOptionString (hLlJob AS INTEGER,
                                                  nMode AS INTEGER,
                                                  pcValue AS CHARACTER):
        
        DEFINE VARIABLE rc AS INTEGER NO-UNDO.

        RUN LlSetOptionString (OUTPUT rc, hLlJob, nMode, pcValue) .
        
        RETURN rc . 
        
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets parameters from a specific extension module
        Notes:   
        @param hLlJob List & Label job handle
        @param nExtensionType Type of extension, LL_LLX_EXTENSIONTYPE_EXPORT, LL_LLX_EXTENSIONTYPE_BARCODE
        @param pszExtensionName Name of the extension ("HTML", "RTF", "PDF417", ...)
        @param pszKey Name of the parameter
        @return The value of the parameter 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC CHARACTER XGetParameter (hLlJob AS INTEGER,
                                                  nExtensionType AS INTEGER,
                                                  pszExtensionName AS CHARACTER,
                                                  pszKey AS CHARACTER):

        DEFINE VARIABLE m  AS MEMPTR  NO-UNDO.
        DEFINE VARIABLE rc AS INTEGER NO-UNDO.
        
        SET-SIZE (m) = 256 .
                
        RUN LlXGetParameter (OUTPUT rc,
                             hLlJob,
                             nExtensionType,
                             pszExtensionName,
                             pszKey,
                             INPUT-OUTPUT m,
                             256) .

        RETURN TRIM (GET-STRING (m, 1)) .

        FINALLY:
            SET-SIZE (m) = 0 .      
        END FINALLY.

    END METHOD .
    
    /*------------------------------------------------------------------------------
        Purpose: Sets parameters in a specific extension module.
        Notes:   
        @param hLlJob List & Label job handle
        @param nExtensionType Type of extension, LL_LLX_EXTENSIONTYPE_EXPORT, LL_LLX_EXTENSIONTYPE_BARCODE
        @param pszExtensionName Name of the extension ("HTML", "RTF", "PDF417", ...)
        @param pszKey Name of the parameter
        @param pszValue Value of the parameter
        @return Error code
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC STATIC INTEGER XSetParameter (hLlJob AS INTEGER,
	                                            nExtensionType AS INTEGER,
	                                            pszExtensionName AS CHARACTER,
	                                            pszKey AS CHARACTER,
	                                            pszValue AS CHARACTER):
		
        DEFINE VARIABLE rc AS INTEGER NO-UNDO.

        RUN LlXSetParameter (OUTPUT rc,
                             hLlJob,
                             nExtensionType,
                             pszExtensionName,
                             pszKey,
                             pszValue) .
        
        RETURN rc . 

	END METHOD .

END CLASS.
