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
    File        : OERABusinessEntityTesterFilterDialog
    Purpose     : Provides and user interface to enter a QueryString for
                  a temp-table
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Wed Apr 08 11:20:28 CEST 2009
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Tools.OERABusinessEntityTester.OERABusinessEntityTesterFilterDialogDesigner FROM PROPATH .
USING Consultingwerk.Util.*                                                                                      FROM PROPATH .
USING Progress.Lang.*                                                                                            FROM ASSEMBLY .
USING Consultingwerk.Framework.* FROM PROPATH.

CLASS Consultingwerk.SmartComponents.Tools.OERABusinessEntityTester.OERABusinessEntityTesterFilterDialog
    INHERITS OERABusinessEntityTesterFilterDialogDesigner :

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the Business Entity
        Notes:   Used to determine the registry key for the LRU
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DatasetHandle AS HANDLE NO-UNDO
    GET:
        RETURN textBox1:DatasetHandle .
    END GET .
    SET (arg AS HANDLE):
        ASSIGN textBox1:DatasetHandle = arg .
    END SET .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the Business Entity
        Notes:   Used to determine the registry key for the LRU
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EntityName AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the Business Entity Table
        Notes:   Used to determine the registry key for the LRU
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EntityTable AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current QueryString
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY QueryString AS CHARACTER NO-UNDO
    GET:
        RETURN THIS-OBJECT:textBox1:Text.
    END GET.
    SET(arg AS CHARACTER):
        THIS-OBJECT:textBox1:Text = arg.

        /* OK Button disabled at start, will be enabled when user changes value */
        THIS-OBJECT:Button2:Enabled = FALSE .

        IF arg MATCHES "*. = " THEN
            THIS-OBJECT:textBox1:SelectionStart = LENGTH (arg) - 3.
        ELSE
            THIS-OBJECT:textBox1:SelectAll() .

    END SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the OERABusinessEntityTesterFilterDialog class
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC OERABusinessEntityTesterFilterDialog ():
        SUPER ().

        /* OK Button disabled at start, will be enabled when user changes value */
        THIS-OBJECT:Button2:Enabled = FALSE .
        THIS-OBJECT:ActiveControl = THIS-OBJECT:textBox1.

        THIS-OBJECT:textBox1:TextChanged:Subscribe (TextChangedHandler) .
        THIS-OBJECT:cbRecentItems:SelectedIndexChanged:Subscribe (SelectedIndexChangedHandler) .
        THIS-OBJECT:button2:Click:Subscribe (OkButtonClickHandler) .

    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the Click event of the Button2
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID OkButtonClickHandler (sender AS System.Object,
                                              e AS System.EventArgs):

        DEFINE VARIABLE iItem AS INTEGER NO-UNDO.

        DEFINE VARIABLE oSettingsService AS ISettingsService NO-UNDO .

        ASSIGN oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

        IF textBox1:Text > "":U THEN DO:
            IF cbRecentItems:Items:Contains (BOX (textBox1:Text)) THEN
                cbRecentItems:Items:Remove (BOX (textBox1:Text)) .

            cbRecentItems:Items:Insert (1, BOX (textBox1:Text)) .

        END.

        DO iItem = 1 TO 10:
            IF cbRecentItems:Items:Count - 1 < iItem THEN
                RETURN .

            oSettingsService:StoreSetting ("FilterLRU~\":U + THIS-OBJECT:EntityName + "~\":U + THIS-OBJECT:EntityTable,
                                           STRING (iItem, "99":U),
                                           UNBOX (cbRecentItems:Items[iItem])) .
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the Shown event
        Notes:
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED OVERRIDE VOID OnShown (e AS System.EventArgs):

        SUPER:OnShown (e) .

        UserInterfaceHelper:ProcessEvents() .

        THIS-OBJECT:PopulateRecentItems () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Populates the Combo-Box of recent queries
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID PopulateRecentItems ():

        DEFINE VARIABLE iItem            AS INTEGER          NO-UNDO .
        DEFINE VARIABLE oSettingsService AS ISettingsService NO-UNDO .
        DEFINE VARIABLE cQuery           AS CHARACTER        NO-UNDO .

        ASSIGN oSettingsService = {Consultingwerk/get-service.i Consultingwerk.Framework.ISettingsService} .

        DO iItem = 1 TO 10:
            cQuery = oSettingsService:GetSetting ("FilterLRU~\":U + THIS-OBJECT:EntityName + "~\":U + THIS-OBJECT:EntityTable,
                                           STRING (iItem, "99":U)) .

            IF cQuery > "":U THEN DO:
                cbRecentItems:Items:Add (BOX (cQuery)) .
                THIS-OBJECT:cbRecentItems:Enabled = TRUE .
            END.
            ELSE
                LEAVE .
        END.

        THIS-OBJECT:cbRecentItems:SelectedIndex = 0 .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the SelectedIndexChanged event of the cbRecentItems
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID SelectedIndexChangedHandler (sender AS System.Object,
                                                     e AS System.EventArgs):

        IF cbRecentItems:SelectedIndex > 0 THEN DO:
            textBox1:Text = UNBOX (cbRecentItems:Items [cbRecentItems:SelectedIndex]) .

            THIS-OBJECT:Button2:Enabled = TRUE.
        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the TextChanged event of the textBox1 instance
        Notes:
        @param sender The reference to the object that raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID TextChangedHandler (sender AS System.Object,
                                              e AS System.EventArgs):

        THIS-OBJECT:Button2:Enabled = TRUE.

    END METHOD.

END CLASS.
