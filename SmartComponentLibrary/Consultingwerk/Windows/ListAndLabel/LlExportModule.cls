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
    File        : LlExportModule
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consulitingwerk Ltd.
    Created     : Mon Oct 29 22:23:48 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                      FROM PROPATH . 
USING Consultingwerk.Windows.ListAndLabel.* FROM PROPATH .  
USING Progress.Lang.*                       FROM PROPATH .

CLASS Consultingwerk.Windows.ListAndLabel.LlExportModule ABSTRACT: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the key of the Export Module
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC ABSTRACT PROPERTY ExportModule AS CHARACTER NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the list of the export module properties
        Notes:   Required due to the lack of reflection in the ABL
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ExportProperties AS CHARACTER NO-UNDO INIT "Export_Path,Export_File,Export_Quiet,Export_ShowResult,Export_ShowResultAvailable":U
    GET.
    PROTECTED SET . 

    /*------------------------------------------------------------------------------
        Purpose: Path where the exported files should be saved. 
        Notes:   If this option is empty, a file selection dialog will always be displayed.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Export_Path AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: File name of the document.
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Export_File AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Use this option to configure the possibility of exporting without user interaction.
        Notes:   0 Export with user interaction (dialogs) (default)
                 1 No dialogs or message boxes - even overwrite warnings - will be displayed (only if Export.Path was specified).
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Export_Quiet AS CHARACTER NO-UNDO INIT "0":U
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Specifies whether the export result will be displayed automatically. 
                 The program that displays the result will be determined by the 
                 registered file extension.
        Notes:   0: Result will not be displayed automatically (default)
                 1: Calls ShellExecute() with Export.File.
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Export_ShowResult AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Enables you to hide the respective checkbox in the dialog.
        Notes:   0 Checkbox will be hidden (default)
                 1 Checkbox will be available
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Export_ShowResultAvailable AS CHARACTER NO-UNDO INIT "0":U
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the type of the Extension Module
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY ExtensionType AS INTEGER NO-UNDO 
	GET.
	PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Sets the properties for the Export Module
        Notes:   
        @param piJob The List & Label job handle
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetExportProperties (piJob AS INTEGER):

        DEFINE VARIABLE cProperty AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cKey      AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cValue    AS CHARACTER NO-UNDO .

        DEFINE VARIABLE i         AS INTEGER   NO-UNDO .

        DO i = 1 TO NUM-ENTRIES (THIS-OBJECT:ExportProperties):
            
            ASSIGN cProperty = ENTRY (i, THIS-OBJECT:ExportProperties) 
                   cKey      = REPLACE (cProperty, "_":U, ".":U ) 
                   cValue    = DYNAMIC-PROPERTY (THIS-OBJECT, cProperty) .
            
            IF CharacterType:IsNotNullOrEmpty (cValue) THEN 
                LlApi:XSetParameter (piJob,
                                     THIS-OBJECT:ExtensionType,
                                     THIS-OBJECT:ExportModule,
                                     cKey,
                                     cValue) .
        END.

    END METHOD . 

END CLASS.
