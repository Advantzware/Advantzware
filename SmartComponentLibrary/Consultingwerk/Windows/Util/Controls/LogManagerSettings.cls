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
    File        : LogManagerSettings
    Purpose     : Allows settings the LOG-MANAGER:LOG-ENTRY-TYPES by a
                  user
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Aug 09 23:19:09 CEST 2008
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*             FROM PROPATH .
USING Consultingwerk.Support.*               FROM ASSEMBLY .
USING Consultingwerk.Util.*                  FROM PROPATH .
USING Consultingwerk.Windows.Session.*       FROM PROPATH .
USING Consultingwerk.Windows.Util.Controls.* FROM PROPATH .
USING Consultingwerk.Windows.Util.Forms.*    FROM PROPATH .
USING Progress.Lang.*                        FROM PROPATH .
USING System.Windows.Forms.*                 FROM ASSEMBLY .

{Consultingwerk/products.i}

CLASS Consultingwerk.Windows.Util.Controls.LogManagerSettings
    INHERITS LogManagerSettingsDesigner:

    /* PropertyTable for System Information display */
    DEFINE VARIABLE myPropertyTable AS PropertyTable NO-UNDO.

    {Consultingwerk/Windows/Util/Controls/ttLogManagerSettings.i PRIVATE}

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the LogManagerSettings class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC LogManagerSettings ():
        SUPER ().

        myPropertyTable = NEW PropertyTable () .

        ImportLogEntryTypesTable() .
        InitializeLogManagerSettings() .
        DisplayLogFileName() .

        THIS-OBJECT:ultraToolbarsManager1:ToolClick:SUBSCRIBE(THIS-OBJECT:ultraToolbarsManager1_ToolClick).
        THIS-OBJECT:fiLogFileName:EditorButtonClick:SUBSCRIBE(THIS-OBJECT:fiLogFileName_EditorButtonClick).

        myPropertyTable:SetValue:Subscribe(PropertyValueChanged) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Imports the message descriptions
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ImportLogEntryTypesTable ():

        DEFINE VARIABLE oData AS Consultingwerk.Windows.Util.Controls.Data.LogManagerDescriptions NO-UNDO .

        oData = NEW Consultingwerk.Windows.Util.Controls.Data.LogManagerDescriptions () .

        oData:GetData (OUTPUT TABLE ttLogManagerSettings) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Initializes the data for the property grid
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID InitializeLogManagerSettings():

        DEFINE VARIABLE i            AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cEntries     AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cEntry       AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cGroup       AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cDescription AS CHARACTER NO-UNDO.

        cEntries = LogManager:EntryTypesList .

        DO i = 1 TO NUM-ENTRIES(cEntries):
            cEntry = ENTRY(i, cEntries) .

            IF NUM-ENTRIES(cEntry, ".":U) > 1 THEN
                ASSIGN cGroup = ENTRY(1, cEntry, ".":U) .
            ELSE ASSIGN cGroup = "default":U.

            FIND ttLogManagerSettings WHERE ttLogManagerSettings.LogFileEntry = cEntry NO-LOCK NO-ERROR .

            IF AVAILABLE ttLogManagerSettings THEN
                ASSIGN cDescription = ttLogManagerSettings.Description +
                                      System.Environment:NewLine +
                                      System.Environment:NewLine +
                                      "Executables: ":U + ttLogManagerSettings.Clients .
            ELSE ASSIGN cDescription = "":U .

            myPropertyTable:AddPropertySpec (cEntry, "System.Int16":U,
                                             cGroup,
                                             cDescription) .
        END.

        DisplayLoggingLevels() .

        propertyGrid1:SelectedObject = myPropertyTable .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Opens the current logfile in the Windows Explorer
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OpenLogFile ():

        DEFINE VARIABLE cExplorerExe AS CHARACTER                           NO-UNDO.
        DEFINE VARIABLE cFileName    AS CHARACTER                           NO-UNDO.
        DEFINE VARIABLE oProcess     AS System.Diagnostics.ProcessStartInfo NO-UNDO .

        IF NOT LogManager:LogFileIsOpen() THEN DO:
            MessageFormHelper:ShowMessage ("The is no logfile open!"{&TRAN}, "LOG-MANAGER":U) .

            RETURN .
        END.

        ASSIGN cFileName = LogManager:LogFileName .

        IF cFileName > "":U THEN DO:

            ASSIGN cExplorerExe = SUBSTITUTE ("&1~\explorer.exe":U,
                                              System.Environment:GetEnvironmentVariable ("windir":U)) .

            oProcess = NEW System.Diagnostics.ProcessStartInfo (cExplorerExe) .

            oProcess:Arguments = "/select,":U + cFileName .

            System.Diagnostics.Process:Start (oProcess) .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Opens the current logfile with the windows default application
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ViewLogFile ():

        DEFINE VARIABLE cFileName    AS CHARACTER                           NO-UNDO .

        IF NOT LogManager:LogFileIsOpen() THEN DO:
            MessageFormHelper:ShowMessage ("There is no logfile open!"{&TRAN},
                                           "LOG-MANAGER":U) .

            RETURN .
        END.

        ASSIGN cFileName = LogManager:LogFileName .

        IF cFileName > "":U THEN
            Consultingwerk.Windows.API.Win32:ShellExecute ("open":U,
                                                           cFileName).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Sets the SessionManager property ServerLogEntryTypes
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID ServerLogEntryTypes ():

        DEFINE VARIABLE oSettingsForm AS AppServerLoggingSettingsForm NO-UNDO .

        oSettingsForm = NEW AppServerLoggingSettingsForm () .
        oSettingsForm:Show () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the ToolClick event of the ultraToolbarsManager1
        Notes:
        @param sender The object that raised the event
        @param e The ToolClickEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraToolbarsManager1_ToolClick (sender AS System.Object,
                                                         e AS Infragistics.Win.UltraWinToolbars.ToolClickEventArgs):

        CASE e:Tool:Key:
            WHEN "message":U THEN
                THIS-OBJECT:WriteMessageToLogFile () .
            WHEN "clear":U THEN
                THIS-OBJECT:ClearLogFile () .
            WHEN "new":U THEN
                THIS-OBJECT:NewLogFile () .
            WHEN "open":U THEN
                THIS-OBJECT:OpenLogFile () .
            WHEN "OpenFile":U THEN
                THIS-OBJECT:ViewLogFile () .
            WHEN "server":U THEN
                THIS-OBJECT:ServerLogEntryTypes () .
            WHEN "WriteServerMessage":U THEN
                THIS-OBJECT:WriteServerMessage () .
        END CASE.

        CATCH err AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Prompts for a message and writes the message to the log file
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID WriteMessageToLogFile ():

        TextInputPrompt:Input ("Output to Logfile"{&TRAN}, "Please enter the text that you want to output to the application log file."{&TRAN}) .

        IF Progress.Util.EnumHelper:AreEqual(TextInputPrompt:Result,
                                             DialogResult:OK) THEN

            IF NOT LogManager:WriteMessage (TextInputPrompt:Response, "USER":U) THEN

                MESSAGE "Message not written to logfile!"{&TRAN}
                    VIEW-AS ALERT-BOX ERROR TITLE "Logging Settings"{&TRAN} .

        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Clears the current logfile
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ClearLogFile( ):

        DisplayLogFileName () .

        IF NOT LogManager:ClearLogFile () THEN
            MESSAGE "The logfile could not be truncated!"{&TRAN}
                VIEW-AS ALERT-BOX ERROR TITLE "Logging Settings"{&TRAN} .

        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Prompts the user for a new logfile name
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID NewLogFile ():

        DEFINE VARIABLE oDialogResult AS DialogResult NO-UNDO .
        DEFINE VARIABLE oForm         AS Form         NO-UNDO .

        ASSIGN oForm = THIS-OBJECT:FindForm () .

        WAIT-FOR saveFileDialog1:ShowDialog (oForm) SET oDialogResult .

        IF Progress.Util.EnumHelper:AreEqual (oDialogResult, DialogResult:Ok) THEN
            LogManager:LogFileName = saveFileDialog1:FILENAME .

        CATCH e AS Progress.Lang.Error :
            MESSAGE "Error opening new logfile:"{&TRAN} SKIP(1)
                    e:GetMessage(1)
                    VIEW-AS ALERT-BOX ERROR TITLE "Logging Settings"{&TRAN} .
        END CATCH.

        FINALLY:
            DisplayLogFileName () .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Displays the current logfile name in the text box
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID DisplayLogFileName () :

        IF LogManager:LogFileIsOpen() THEN
            THIS-OBJECT:fiLogFileName:Text = LogManager:LogFileName .
        ELSE
            THIS-OBJECT:fiLogFileName:Text = "no logfile"{&TRAN} .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the EditorButtonClick event of the fiLogFileName
        Notes:
        @param sender The object that raised the event
        @param e The EditorButtonEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID fiLogFileName_EditorButtonClick (sender AS System.Object,
                                                         e AS Infragistics.Win.UltraWinEditors.EditorButtonEventArgs):

        FILE-INFORMATION:FILE-NAME = LogManager:LogFileName .

        CLIPBOARD:VALUE = FILE-INFORMATION:FULL-PATHNAME .

        RETURN.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Displays all current logging levels in the property grid
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID DisplayLoggingLevels ():

        DEFINE VARIABLE cEntries AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cEntry   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE i        AS INTEGER   NO-UNDO.

        cEntries = LogManager:EntryTypesList .

        DO i = 1 TO NUM-ENTRIES(cEntries):
            ASSIGN cEntry = ENTRY(i, cEntries) .

            myPropertyTable:SetPropertyValue (cEntry, STRING(LogManager:GetLoggingLevel(cEntry))) .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the PropertyValueChanged event of the property table
        Notes:
        @param sender The reference to the object that raised the event
        @param e The PropertySpecEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID PropertyValueChanged  (sender AS System.Object,
                                               e AS Flobbster.Windows.Forms.PropertySpecEventArgs) :

        DEFINE VARIABLE iOldValue AS INTEGER NO-UNDO.
        DEFINE VARIABLE iNewValue AS INTEGER NO-UNDO.

        ASSIGN iOldValue = LogManager:GetLoggingLevel(e:Property:Name)
               iNewValue = INTEGER(e:Value:ToString()).

        IF iOldValue = ? THEN
            ASSIGN iOldValue = 0 .

        IF iNewValue >= 0 AND iNewValue < 5 THEN .
        ELSE DO:
            MESSAGE "Only values between 0 and 4 are valid."{&TRAN}
                VIEW-AS ALERT-BOX ERROR TITLE "Logging Settings"{&TRAN} .

            RETURN .
        END.

        IF NOT ListHelper:EntryIsInList (e:Property:Name, LogManager:EntryTypesList) THEN DO:
            MESSAGE SUBSTITUTE("Property &1 is not supported by this runtime."{&TRAN}, e:Property:Name)
                VIEW-AS ALERT-BOX ERROR TITLE "Logging Settings"{&TRAN} .

            RETURN .
        END.

        LogManager:SetLoggingLevel(e:Property:Name, iNewValue) .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage (err,
                                          "Error setting logging values:"{&TRAN}) .
        END CATCH.

        FINALLY:
            DisplayLoggingLevels () .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Writes a message to the AppServer logfile
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID WriteServerMessage ():

&IF NOT PROVERSION BEGINS "10.2":U &THEN
        DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

        IF InputPromptHelper:PromptForCharacterValue ("Message to Server Logfile"{&TRAN},
                                                      "Please enter the message that shall be written to the AppServer logfile."{&TRAN},
                                                      INPUT-OUTPUT cMessage) THEN .
        ELSE
            RETURN .

        LogManager:WriteToServerLogfile (cMessage) .
&ELSE
        UNDO, THROW NEW Consultingwerk.Exceptions.NotSupportedException () .
&ENDIF

    END METHOD .

END CLASS.
