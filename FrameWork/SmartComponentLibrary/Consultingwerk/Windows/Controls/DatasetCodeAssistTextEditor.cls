/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : DatasetCodeAssistTextEditor
    Purpose     : WinForm TextBox derived Control that provides content
                  assist for ProDataset buffer and field names
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed May 04 15:22:02 CEST 2016
    Notes       : The handle of the Dataset that provides the source schema
                  for the content assist needs to be assigned to DatasetHandle
                  property of the control instance
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Util.*               FROM PROPATH.
USING Consultingwerk.Windows.Controls.*   FROM PROPATH .
USING Infragistics.Win.UltraWinEditors.*  FROM ASSEMBLY .
USING Infragistics.Win.UltraWinListView.* FROM ASSEMBLY.
USING Progress.Lang.*                     FROM PROPATH .
USING Progress.Util.*                     FROM ASSEMBLY.
USING System.Windows.Forms.*              FROM ASSEMBLY.

CLASS Consultingwerk.Windows.Controls.DatasetCodeAssistTextEditor
    INHERITS TextBox:

    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraPopupControlContainer1 AS Infragistics.Win.Misc.UltraPopupControlContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE ultraListView1 AS Infragistics.Win.UltraWinListView.UltraListView NO-UNDO.

    DEFINE VARIABLE oField AS System.Drawing.Image NO-UNDO .
    DEFINE VARIABLE oTable AS System.Drawing.Image NO-UNDO .

    DEFINE VARIABLE iReplaceStart AS INTEGER NO-UNDO.
    DEFINE VARIABLE iReplaceEnd   AS INTEGER NO-UNDO.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the handle of the Dataset used for code-completion
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DatasetHandle AS HANDLE NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DatasetCodeAssistTextEditor class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DatasetCodeAssistTextEditor ():

        SUPER().

        InitializeComponent().

        ASSIGN oField = ImageHelper:Load ("Consultingwerk/Windows/Controls/Images/data_table.png":U)
               oTable = ImageHelper:Load ("Consultingwerk/Windows/Controls/Images/table2_selection_column.png":U)
                .

    END CONSTRUCTOR.

    /**
     * Purpose: Initializes the Visual Design
     * Notes:
     */
    @InitializeComponent.
    METHOD PRIVATE VOID InitializeComponent ():

        /* NOTE: The following method is automatically generated.

        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.

        Modifying the contents of this method using a code editor will invalidate any support for this file. */
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        THIS-OBJECT:ultraPopupControlContainer1 = NEW Infragistics.Win.Misc.UltraPopupControlContainer(THIS-OBJECT:components).
        THIS-OBJECT:ultraListView1 = NEW Infragistics.Win.UltraWinListView.UltraListView().
        CAST(THIS-OBJECT:ultraListView1, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ultraPopupControlContainer1 */
        /*  */
        THIS-OBJECT:ultraPopupControlContainer1:PopupControl = THIS-OBJECT:ultraListView1.
        /*  */
        /* ultraListView1 */
        /*  */
        THIS-OBJECT:ultraListView1:Location = NEW System.Drawing.Point(0, 0).
        THIS-OBJECT:ultraListView1:Name = "ultraListView1":U.
        THIS-OBJECT:ultraListView1:Size = NEW System.Drawing.Size(160, 200).
        THIS-OBJECT:ultraListView1:TabIndex = 0.
        THIS-OBJECT:ultraListView1:Text = "ultraListView1":U.
        THIS-OBJECT:ultraListView1:View = Infragistics.Win.UltraWinListView.UltraListViewStyle:List.
        THIS-OBJECT:ultraListView1:ViewSettingsList:MultiColumn = FALSE.
        THIS-OBJECT:ultraListView1:Visible = FALSE.
        THIS-OBJECT:ultraListView1:KeyDown:Subscribe(THIS-OBJECT:ultraListView1_KeyDown).
        CAST(THIS-OBJECT:ultraListView1, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Raises the KeyDown event
       Notes:
       @param args The KeyEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnKeyDown (args AS KeyEventArgs):

        DEFINE VARIABLE oLocation AS System.Drawing.Point    NO-UNDO .
        DEFINE VARIABLE cText     AS CHARACTER               NO-UNDO .
        DEFINE VARIABLE iPos      AS INTEGER                 NO-UNDO .
        DEFINE VARIABLE cWord     AS CHARACTER               NO-UNDO .

        SUPER:OnKeyDown(args) .

        IF NOT VALID-HANDLE (THIS-OBJECT:DatasetHandle) THEN
            RETURN .

        /* CRTL-A */
        IF (args:Control = TRUE AND args:KeyValue = 65) THEN
            THIS-OBJECT:SelectAll () .

        IF (args:Control = TRUE  AND args:Shift = FALSE AND args:Alt = FALSE AND args:KeyValue = 32) OR
           (args:Control = FALSE AND args:Shift = FALSE AND args:Alt = FALSE AND args:KeyValue = 190) THEN DO:

            ASSIGN cText = REPLACE (REPLACE (REPLACE (THIS-OBJECT:Text, CHR(13), " ":U), CHR(10), " ":U), CHR(9), " ":U)
                   cText = TRIM (SUBSTRING (cText, 1, THIS-OBJECT:SelectionStart + 1))
                   iPos  = R-INDEX (cText, " ":U)
                   cWord = SUBSTRING (cText, iPos + 1) NO-ERROR .

            IF ERROR-STATUS:ERROR THEN
                RETURN .

            IF args:KeyValue = 190 THEN
                ASSIGN iReplaceStart = THIS-OBJECT:SelectionStart + 1
                       iReplaceEnd   = THIS-OBJECT:SelectionStart + 1 .
            ELSE DO:
                IF NUM-ENTRIES (cWord, ".":U) > 1 THEN
                    ASSIGN iReplaceStart = THIS-OBJECT:SelectionStart - LENGTH (ENTRY (2, cWord, ".":U))
                           iReplaceEnd   = THIS-OBJECT:SelectionStart .
                ELSE
                    ASSIGN iReplaceStart = THIS-OBJECT:SelectionStart - LENGTH (cWord)
                           iReplaceEnd   = THIS-OBJECT:SelectionStart .
            END.

            IF THIS-OBJECT:PopulateListBox (cWord, (IF args:KeyValue = 190 THEN TRUE ELSE FALSE)) THEN DO:
                oLocation = THIS-OBJECT:GetPositionFromCharIndex (IF THIS-OBJECT:SelectionStart > 0
                                                                        THEN THIS-OBJECT:SelectionStart - 1
                                                                        ELSE 0) .
                oLocation:Offset(6, THIS-OBJECT:Font:Height).
                ultraPopupControlContainer1:Show(THIS-OBJECT:PointToScreen(oLocation)) .

                ultraListView1:Focus() .
            END.

            args:Handled = TRUE .

            IF args:KeyValue = 32 THEN
                args:SuppressKeyPress = TRUE .
        END.

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Populates the the list with the fields of a buffer
        Notes:
        @param phBufferHandle The handle of the buffer to add fields to the list
        @param pcFieldName The pattern that buffer fields need to match (BEGINS)
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID PopulateBufferFields (phBufferHandle AS HANDLE,
                                                pcFieldName AS CHARACTER):

        DEFINE VARIABLE hField AS HANDLE            NO-UNDO .
        DEFINE VARIABLE i      AS INTEGER           NO-UNDO .
        DEFINE VARIABLE oItem  AS UltraListViewItem NO-UNDO .

        DO i = 1 TO phBufferHandle:NUM-FIELDS:
            ASSIGN hField = phBufferHandle:BUFFER-FIELD (i) .

            IF hField:NAME BEGINS pcFieldName THEN DO:
                oItem = ultraListView1:Items:Add (GUID, hField:NAME) .
                oItem:Appearance:Image = oField .
            END.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Populates the the list with the fields of a buffer
        Notes:
        @param pcWord The current word entered in the text editor when the content assist was opened
        @param plPeriod Logical value indicating if the period was pressed
        @return Logical value indicating if the list was populated
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED LOGICAL PopulateListBox (pcWord AS CHARACTER,
                                              plPeriod AS LOGICAL):

        DEFINE VARIABLE cTableName AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cFieldName AS CHARACTER NO-UNDO.

        ultraListView1:Items:Clear () .

        ASSIGN pcWord = TRIM (pcWord) .

        IF plPeriod THEN
            ASSIGN cTableName = pcWord .
        ELSE
            ASSIGN cTableName = ENTRY (1, pcWord, ".":U)
                   cFieldName = ListHelper:EntryWhenAvailable(2, pcWord, ".":U) .

        IF pcWord > "":U AND SUBSTRING (pcWord, LENGTH (pcWord), 1) = ".":U THEN
            ASSIGN plPeriod = TRUE .

        IF plPeriod OR cFieldName > "":U THEN DO:
            IF DatasetHelper:HasBuffer(THIS-OBJECT:DatasetHandle, cTableName) THEN DO:
                THIS-OBJECT:PopulateBufferFields (THIS-OBJECT:DatasetHandle:GET-BUFFER-HANDLE(cTableName), cFieldName) .
                RETURN TRUE .
            END.
        END.
        ELSE DO:
            THIS-OBJECT:PopulateTables (cTableName) .
            RETURN TRUE .
        END.

        RETURN FALSE .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Populates the the list with the names of matching buffers
        Notes:
        @param pcTableName The pattern that buffer names need to match (BEGINS)
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID PopulateTables (pcTableName AS CHARACTER):

        DEFINE VARIABLE hBuffer AS HANDLE            NO-UNDO .
        DEFINE VARIABLE i       AS INTEGER           NO-UNDO .
        DEFINE VARIABLE oItem   AS UltraListViewItem NO-UNDO .

        DO i = 1 TO THIS-OBJECT:DatasetHandle:NUM-BUFFERS:

            ASSIGN hBuffer = THIS-OBJECT:DatasetHandle:GET-BUFFER-HANDLE(i) .

            IF hBuffer:NAME BEGINS pcTableName THEN DO:
                oItem = ultraListView1:Items:Add (GUID, hBuffer:NAME) .
                oItem:Appearance:Image = oTable .
            END.
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
       Purpose: Event handler for the KeyDown event of the ultraListView1
       Notes:
       @param sender The reference to the object that raised the event
       @param e The KeyEventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ultraListView1_KeyDown (sender AS System.Object,
                                                e AS System.Windows.Forms.KeyEventArgs ):

        DEFINE VARIABLE cPasteText AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iPos       AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cText      AS CHARACTER NO-UNDO.

        IF EnumHelper:AreEqual(e:KeyCode, Keys:Escape) THEN
            ultraPopupControlContainer1:Close() .
        ELSE IF EnumHelper:AreEqual(e:KeyCode, Keys:Enter) OR
                EnumHelper:AreEqual(e:KeyCode, Keys:Return)
                THEN DO:

            IF ultraListView1:SelectedItems:Count > 0 THEN DO:
                ASSIGN iPos       = iReplaceStart
                       cPasteText = ultraListView1:SelectedItems:First:Text .

                /* 10.2B does not allow SUBSTRING () assignment on properties */
                ASSIGN cText = THIS-OBJECT:Text
                       SUBSTRING (cText, iReplaceStart + 1, iReplaceEnd - iReplaceStart) = cPasteText
                       THIS-OBJECT:Text = cText .

                THIS-OBJECT:SelectionStart = iPos + LENGTH (cPasteText) .
                THIS-OBJECT:SelectionLength = 0 .
            END.

            ultraPopupControlContainer1:Close() .
        END.
        ELSE IF EnumHelper:AreEqual(e:KeyCode, Keys:Space) OR
                e:KeyValue >= 65 AND e:KeyValue <= 90  THEN
            ultraPopupControlContainer1:Close() .

        CATCH err AS Progress.Lang.Error:
            ErrorHelper:ShowErrorMessage(err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the DatasetCodeAssistTextEditor class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC DatasetCodeAssistTextEditor ( ):

        IF VALID-OBJECT(components) THEN DO:
            CAST(components, System.IDisposable):Dispose().
        END.

    END DESTRUCTOR.

END CLASS.
