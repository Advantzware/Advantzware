/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SmartUserAndGroupSelectionForm
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Wed Jul 30 10:56:27 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Framework.*                          FROM PROPATH .
USING Consultingwerk.Framework.Collections.*              FROM PROPATH .
USING Consultingwerk.Framework.Session.*                  FROM PROPATH.
USING Consultingwerk.OERA.*                               FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.SmartWindowForm FROM PROPATH .
USING Consultingwerk.Util.*                               FROM PROPATH .
USING Consultingwerk.Windows.Framework.Authorization.*    FROM PROPATH .
USING Progress.Lang.*                                     FROM PROPATH .

CLASS Consultingwerk.Windows.Framework.Authorization.SmartUserAndGroupSelectionForm
    INHERITS SmartWindowForm:

    DEFINE PRIVATE VARIABLE ButtonOk AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE ButtonCancel AS System.Windows.Forms.Button NO-UNDO.
    DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
    DEFINE PRIVATE VARIABLE multipleItemPickerControl1 AS Consultingwerk.Windows.Controls.MultipleItemPickerControl NO-UNDO.
    DEFINE PRIVATE VARIABLE panel1 AS System.Windows.Forms.Panel NO-UNDO.

    {Consultingwerk/SmartFramework/Authentication/dsUser.i}
    {Consultingwerk/SmartFramework/Authorization/dsGroup.i}

    DEFINE TEMP-TABLE ttUserAndGroups NO-UNDO
        LIKE eSmartUser
        FIELD ImageKey AS CHARACTER .

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the list of the selected Users and Groups
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SelectedUserAndGroups AS CharacterDictionary NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Initializes a new instance of the SmartUserAndGroupSelectionForm
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartUserAndGroupSelectionForm ():
        SUPER ().
        InitializeComponent ().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
        &ENDIF

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Assign the list of selected items (Users/Groups) to the
                 SelectedUsersAndGroups property to have it available even after the
                 dialog was closed.
        Notes:
        @param sender The source of the event
        @param e An EventArgs that contains no event data
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID ButtonOk_Click (sender AS System.Object,
                                        e AS System.EventArgs):
        DEFINE VARIABLE oSelectedUserAndGroups AS CharacterList NO-UNDO.

        oSelectedUserAndGroups = multipleItemPickerControl1:GetSelectedItems () .
        IF NOT VALID-OBJECT (SelectedUserAndGroups) THEN
            ASSIGN
                SelectedUserAndGroups = NEW CharacterDictionary ("":U, ",":U, "":U, CHR (1))
                .

        SelectedUserAndGroups:Clear ().

        {Consultingwerk/foreachPrimitiveList.i Character cValue in oSelectedUserAndGroups}

            SelectedUserAndGroups:Add (cValue, GetNameOfItemKey (cValue)).

        END.

        CATCH ple AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple).
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Gets the Name of a SecurityItem identified by it's key
        Notes:
        @param pcKeyValue The key of the SecurityItem to return the Name for
        @return The Name of the SecurityItem
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE CHARACTER GetNameOfItemKey (pcKeyValue AS CHARACTER):

        FIND FIRST ttUserAndGroups WHERE ttUserAndGroups.UserGuid = pcKeyValue NO-ERROR.

        IF AVAILABLE ttUserAndGroups THEN
            RETURN ttUserAndGroups.UserFullName.
        ELSE
            UNDO, THROW NEW AppError ("No User / Group record available!":U, 0).

        RETURN "":U.

    END METHOD .

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
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Authorization.SmartUserAndGroupSelectionForm":U).
        THIS-OBJECT:ButtonOk = NEW System.Windows.Forms.Button().
        THIS-OBJECT:ButtonCancel = NEW System.Windows.Forms.Button().
        THIS-OBJECT:multipleItemPickerControl1 = NEW Consultingwerk.Windows.Controls.MultipleItemPickerControl().
        THIS-OBJECT:panel1 = NEW System.Windows.Forms.Panel().
        THIS-OBJECT:panel1:SuspendLayout().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* ButtonOk */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ButtonOk, "ButtonOk":U).
        THIS-OBJECT:ButtonOk:DialogResult = System.Windows.Forms.DialogResult:OK.
        THIS-OBJECT:ButtonOk:Name = "ButtonOk":U.
        THIS-OBJECT:ButtonOk:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:ButtonOk:UseVisualStyleBackColor = TRUE.
        THIS-OBJECT:ButtonOk:Click:Subscribe(THIS-OBJECT:ButtonOk_Click).
        /*  */
        /* ButtonCancel */
        /*  */
        resources:ApplyResources(THIS-OBJECT:ButtonCancel, "ButtonCancel":U).
        THIS-OBJECT:ButtonCancel:DialogResult = System.Windows.Forms.DialogResult:Cancel.
        THIS-OBJECT:ButtonCancel:Name = "ButtonCancel":U.
        THIS-OBJECT:ButtonCancel:UseCompatibleTextRendering = TRUE.
        THIS-OBJECT:ButtonCancel:UseVisualStyleBackColor = TRUE.
        /*  */
        /* multipleItemPickerControl1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:multipleItemPickerControl1, "multipleItemPickerControl1":U).
        THIS-OBJECT:multipleItemPickerControl1:Name = "multipleItemPickerControl1":U.
        /*  */
        /* panel1 */
        /*  */
        resources:ApplyResources(THIS-OBJECT:panel1, "panel1":U).
        THIS-OBJECT:panel1:BackColor = System.Drawing.Color:White.
        THIS-OBJECT:panel1:Controls:Add(THIS-OBJECT:multipleItemPickerControl1).
        THIS-OBJECT:panel1:Name = "panel1":U.
        /*  */
        /* SmartUserAndGroupSelectionForm */
        /*  */
        THIS-OBJECT:AcceptButton = THIS-OBJECT:ButtonOk.
        THIS-OBJECT:CancelButton = THIS-OBJECT:ButtonCancel.
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ButtonOk).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:ButtonCancel).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:panel1).
        THIS-OBJECT:MinimizeBox = FALSE.
        THIS-OBJECT:Name = "SmartUserAndGroupSelectionForm":U.
        THIS-OBJECT:SizeGripStyle = System.Windows.Forms.SizeGripStyle:Show.
        THIS-OBJECT:panel1:ResumeLayout(FALSE).
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Populate the Selection Control
        Notes:
        @param e An EventArgs that contains no event data
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PROTECTED VOID OnShown (e AS System.EventArgs):

        DEFINE VARIABLE oRequest AS FetchDataRequest NO-UNDO.
        DEFINE VARIABLE oItems   AS CharacterList    NO-UNDO .

        SUPER:OnShown (INPUT e).

        PROCESS EVENTS .

        IF SessionManager:LoginCompanyKey > "":U THEN
            oRequest = NEW FetchDataRequest ("eSmartUser":U,
                                             SUBSTITUTE ("for each eSmartUser where eSmartUser.LoginCompanyGuid = &1 or eSmartUser.LoginCompanyGuid = &2":U,
                                                         QUOTER (SessionManager:LoginCompanyKey),
                                                         QUOTER ("":U))) .
        ELSE
            oRequest = NEW FetchDataRequest ("eSmartUser":U) .

        FrameworkSettings:ServiceAdapter:RetrieveData ("":U,
                                                       "Consultingwerk.SmartFramework.Authentication.UserBusinessEntity":U,
                                                       oRequest,
                                                       OUTPUT DATASET dsUser) .

        oRequest = NEW FetchDataRequest ("eSmartGroup":U) .

        FrameworkSettings:ServiceAdapter:RetrieveData ("":U,
                                                       "Consultingwerk.SmartFramework.Authorization.GroupBusinessEntity":U,
                                                       oRequest,
                                                       OUTPUT DATASET dsGroup) .

        IF VALID-OBJECT (THIS-OBJECT:SelectedUserAndGroups) THEN
            oItems = NEW CharacterList (THIS-OBJECT:SelectedUserAndGroups:Keys,
                                        THIS-OBJECT:SelectedUserAndGroups:KeyDelimiter) .
        ELSE
            oItems = NEW CharacterList () .

        FOR EACH eSmartUser:
            CREATE ttUserAndGroups.
            BUFFER-COPY eSmartUser TO ttUserAndGroups
                ASSIGN ttUserAndGroups.ImageKey = "Consultingwerk/Windows/Framework/Images/user_16.png":U .

        END.

        FOR EACH eSmartGroup:
            CREATE ttUserAndGroups.
            ASSIGN ttUserAndGroups.UserGuid = eSmartGroup.GroupGuid
                   ttUserAndGroups.UserName = eSmartGroup.GroupGuid
                   ttUserAndGroups.UserFullName = eSmartGroup.GroupName
                   ttUserAndGroups.ImageKey = "Consultingwerk/Windows/Framework/Images/users_16.png":U .
        END.

        FOR EACH ttUserAndGroups WHERE ttUserAndGroups.UserFullName = ? OR ttUserAndGroups.UserFullName = "":U:
            ASSIGN ttUserAndGroups.UserFullName = ttUserAndGroups.UserName .
        END.

        multipleItemPickerControl1:SetListItems (TEMP-TABLE ttUserAndGroups:HANDLE,
                                                 oItems) .

        CATCH err AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartUserAndGroupSelectionForm class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartUserAndGroupSelectionForm ():

    END DESTRUCTOR .

END CLASS.
