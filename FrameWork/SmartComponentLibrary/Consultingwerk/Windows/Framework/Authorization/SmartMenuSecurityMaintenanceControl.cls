/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : SmartMenuSecurityMaintenanceControl
    Purpose     : User Control for the assignment of SMF Smart Menu Function
                  authurization to multiple users or groups
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Thu May 22 13:20:21 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Assertion.*                          FROM PROPATH .
USING Consultingwerk.Framework.*                          FROM PROPATH .
USING Consultingwerk.Framework.Collections.*              FROM PROPATH .
USING Consultingwerk.Framework.Enum.*                     FROM PROPATH .
USING Consultingwerk.Framework.Session.*                  FROM PROPATH .
USING Consultingwerk.OERA.*                               FROM PROPATH .
USING Consultingwerk.SmartComponents.Base.*               FROM PROPATH .
USING Consultingwerk.SmartFramework.Authorization.*       FROM PROPATH .
USING Consultingwerk.Util.*                               FROM PROPATH .
USING Consultingwerk.Windows.Framework.Authorization.*    FROM PROPATH .
USING Consultingwerk.Windows.Util.*                       FROM PROPATH .
USING Infragistics.Win.*                                  FROM ASSEMBLY .
USING Infragistics.Win.UltraWinTree.*                     FROM ASSEMBLY .
USING Progress.Lang.*                                     FROM PROPATH .

{Consultingwerk/products.i}

CLASS Consultingwerk.Windows.Framework.Authorization.SmartMenuSecurityMaintenanceControl
    INHERITS SmartUserControl:

    DEFINE PRIVATE VARIABLE components                                                      AS System.ComponentModel.IContainer                                         NO-UNDO.
    DEFINE PRIVATE VARIABLE MenuAdapter                                                     AS Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter NO-UNDO.
    DEFINE PRIVATE VARIABLE MenuTree                                                        AS Infragistics.Win.UltraWinTree.UltraTree                                  NO-UNDO.

    /* Marko Rüterbories, Consultingwerk Ltd. 08.12.2012
       RootNode added to the Tree even if there is no Data in the SmartMenu table */
    DEFINE PRIVATE VARIABLE oRootNode AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.

    DEFINE VARIABLE oTreeHelper AS UltraTreeHelper NO-UNDO.

    DEFINE VARIABLE oMenuDatasetController AS Consultingwerk.SmartFramework.Menu.MenuDatasetController NO-UNDO.

    {Consultingwerk/SmartFramework/Authorization/dsAuthorizationMaintenance.i}
    {Consultingwerk/SmartFramework/Authentication/dsUser.i}

    DEFINE VARIABLE oRealmValueListRestricted    AS Infragistics.Win.ValueList NO-UNDO.
    DEFINE VARIABLE oRealmValueListNotRestricted AS Infragistics.Win.ValueList NO-UNDO.
    DEFINE VARIABLE oAllValueListRestricted      AS Infragistics.Win.ValueList NO-UNDO.
    DEFINE VARIABLE oAllValueListNotRestricted   AS Infragistics.Win.ValueList NO-UNDO.
    DEFINE VARIABLE oGroupValueListRestricted    AS Infragistics.Win.ValueList NO-UNDO.
    DEFINE VARIABLE oGroupValueListNotRestricted AS Infragistics.Win.ValueList NO-UNDO.

    DEFINE VARIABLE cSecurityRealmGuid AS CHARACTER NO-UNDO.
    DEFINE VARIABLE oOwnerList         AS CharacterDictionary NO-UNDO.
    DEFINE VARIABLE lRefreshHeaderInfo AS LOGICAL   NO-UNDO.

    DEFINE VARIABLE lSynchronizing AS LOGICAL NO-UNDO.

    DEFINE VARIABLE oUserImage  AS System.Drawing.Bitmap NO-UNDO .
    DEFINE VARIABLE oGroupImage AS System.Drawing.Bitmap NO-UNDO .

    /*------------------------------------------------------------------------------
        Purpose: Raised when the HasPendingChanges property has changed
        Notes:
        @param sender The object that raised the HasPendingChangesChanged event
        @param e The Consultingwerk.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC EVENT HasPendingChangesChanged SIGNATURE VOID (sender AS Progress.Lang.Object,
                                                                 e AS Consultingwerk.EventArgs).

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the alternative LoginCompanyKey to be used to retrieve
                 and store authorization data
        Notes:   Lets empty (default) to work with the current login company
                 (SessionManager)
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AlternativeLoginCompanyKey AS CHARACTER NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns if the Controls has pending changes that require save or
                 can could be cancelled
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY HasPendingChanges AS LOGICAL INITIAL FALSE NO-UNDO
    GET.
    PROTECTED SET (arg AS LOGICAL):
        DEFINE VARIABLE lChanged AS LOGICAL NO-UNDO.

        ASSIGN lChanged = arg <> THIS-OBJECT:HasPendingChanges .

        ASSIGN THIS-OBJECT:HasPendingChanges = arg .

        IF lChanged THEN
           THIS-OBJECT:OnHasPendingChangesChanged (Consultingwerk.EventArgs:Empty) .
    END.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Business Entity Name used to retrieve the menu
                 structure
        Notes:   SCL-891: Enabels customers to use customized versions of the menu
                 Busienss Entity to retrieve the menu structure. Business Entity
                 should be designed around a compatible dataset structure
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MenuBusinessEntityName AS CHARACTER NO-UNDO
    GET:
        RETURN MenuAdapter:EntityName .
    END GET.
    SET (arg AS CHARACTER):
        Assert:NotNullOrEmpty (arg, "MenuBusinessEntityName":U) .

        ASSIGN MenuAdapter:EntityName = arg .
    END SET.

    /*------------------------------------------------------------------------------
        Purpose: Initializes a new instance of the SmartMenuSecurityMaintenanceControl
        Notes:
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartMenuSecurityMaintenanceControl ():

        DEFINE VARIABLE oSecurityRealm AS SecurityRealmDatasetModel NO-UNDO.

        SUPER ().
        InitializeComponent ().

        &IF NOT PROVERSION BEGINS "10.2":U AND NOT PROVERSION BEGINS "11.0" &THEN
        THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
        &ENDIF

        oSecurityRealm = NEW SecurityRealmDatasetModel ().
        oSecurityRealm:SmartSecurityRealm:Filter:SecurityRealmCode:EQ ("SMF":U):Run ().

        IF NOT oSecurityRealm:SmartSecurityRealm:Available THEN
            UNDO, THROW NEW AppError ("SecurityRealm 'SMF' does not exist!"{&TRAN}, 0).

        ASSIGN cSecurityRealmGuid = oSecurityRealm:SmartSecurityRealm:SecurityRealmGuid

               oUserImage         = ImageHelper:Load ("Consultingwerk/Windows/Framework/Images/user_16.png":U).
               oGroupImage        = ImageHelper:Load ("Consultingwerk/Windows/Framework/Images/users_16.png":U) .

        THIS-OBJECT:FetchUsers () .

        CreateValueLists ().

        SetupNodeLevelLayout ().

        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

        FINALLY:
            Consultingwerk.Util.GarbageCollectorHelper:DeleteObject (oSecurityRealm).
        END FINALLY.
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Cancels pending changes
        Notes:   Reloads current security settinsg
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Cancel ():

        IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
            FrameworkSettings:WaitStateManager:SetWaitState ("Cancelling Security Settings":U,
                                                             THIS-OBJECT) .

        THIS-OBJECT:GetAuthorizationData ().

        MenuAdapter:RetrieveData () .

        PROCESS EVENTS.

        MenuTree:ExpandAll ().

        FINALLY:
            IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
                FrameworkSettings:WaitStateManager:ClearWaitState ("Cancelling Security Settings":U,
                                                                   THIS-OBJECT) .

            THIS-OBJECT:HasPendingChanges = FALSE .
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Add a dummy ChildNode to the given ParentNode
        Notes:
        @param poParentNode Object reference to the parent UltraTreeNode
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID CreateDummyChildNode (poParentNode AS Infragistics.Win.UltraWinTree.UltraTreeNode):

        DEFINE VARIABLE oNode      AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.

        oNode = NEW Infragistics.Win.UltraWinTree.UltraTreeNode ("DummyNode":U + GUID, "":U).

        poParentNode:Nodes:Add (oNode).

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Adds a RootNode to the MenuTree
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID CreateRootNode ():

        DEFINE VARIABLE oColumnSet AS UltraTreeColumnSet NO-UNDO.

        oColumnSet        = oTreeHelper:GetOrCreateNodeLevelBasedColumnSet (0).

        oRootNode = NEW Infragistics.Win.UltraWinTree.UltraTreeNode (GUID).

        MenuTree:Nodes:Add (oRootNode).

        PopulateNodeCells (oColumnSet, oRootNode).

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Prepares all needed ValueLists for displaying the security settings
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID CreateValueLists ():

        oRealmValueListRestricted    = NEW Infragistics.Win.ValueList ().
        oRealmValueListRestricted:ValueListItems:Add ("Restricted":U, "Restricted":U).
        oRealmValueListRestricted:ValueListItems:Add ("Unrestricted":U, "Unrestricted":U).
        oRealmValueListRestricted:ValueListItems:Add ("Default (RESTRICTED-BY-REALM)":U, "Default (RESTRICTED-BY-REALM)":U).

        oRealmValueListNotRestricted = NEW Infragistics.Win.ValueList ().
        oRealmValueListNotRestricted:ValueListItems:Add ("Restricted":U, "Restricted":U).
        oRealmValueListNotRestricted:ValueListItems:Add ("Unrestricted":U, "Unrestricted":U).
        oRealmValueListNotRestricted:ValueListItems:Add ("Default (NOTRESTRICTED-BY-REALM)":U, "Default (NOTRESTRICTED-BY-REALM)":U).

        oAllValueListRestricted      = NEW Infragistics.Win.ValueList ().
        oAllValueListRestricted:ValueListItems:Add ("Restricted":U, "Restricted":U).
        oAllValueListRestricted:ValueListItems:Add ("Unrestricted":U, "Unrestricted":U).
        oAllValueListRestricted:ValueListItems:Add ("Default (RESTRICTED-FOR-ALL)":U, "Default (RESTRICTED-FOR-ALL)":U).

        oAllValueListNotRestricted   = NEW Infragistics.Win.ValueList ().
        oAllValueListNotRestricted:ValueListItems:Add ("Restricted":U, "Restricted":U).
        oAllValueListNotRestricted:ValueListItems:Add ("Unrestricted":U, "Unrestricted":U).
        oAllValueListNotRestricted:ValueListItems:Add ("Default (NOTRESTRICTED-FOR-ALL)":U, "Default (NOTRESTRICTED-FOR-ALL)":U).

        oGroupValueListRestricted    = NEW Infragistics.Win.ValueList ().
        oGroupValueListRestricted:ValueListItems:Add ("Restricted":U, "Restricted":U).
        oGroupValueListRestricted:ValueListItems:Add ("Unrestricted":U, "Unrestricted":U).
        oGroupValueListRestricted:ValueListItems:Add ("Default (RESTRICTED-BY-GROUP)":U, "Default (RESTRICTED-BY-GROUP)":U).

        oGroupValueListNotRestricted = NEW Infragistics.Win.ValueList ().
        oGroupValueListNotRestricted:ValueListItems:Add ("Restricted":U, "Restricted":U).
        oGroupValueListNotRestricted:ValueListItems:Add ("Unrestricted":U, "Unrestricted":U).
        oGroupValueListNotRestricted:ValueListItems:Add ("Default (NOTRESTRICTED-BY-GROUP)":U, "Default (NOTRESTRICTED-BY-GROUP)":U).

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Fetches the SmartUser's from the Backend
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID FetchUsers ():

        DEFINE VARIABLE oRequest AS FetchDataRequest NO-UNDO .

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

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Get Authorization data for the selected Users and Groups
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID GetAuthorizationData ():

        DEFINE VARIABLE oParameter      AS AuthorizationMaintenanceServiceParameter NO-UNDO .
        DEFINE VARIABLE oServiceAdapter AS IServiceAdapterWithInvokeTask            NO-UNDO .

        ObjectAssert:IsValid (FrameworkSettings:ServiceAdapter).
        ObjectAssert:TypeOf (FrameworkSettings:ServiceAdapter, "Consultingwerk.OERA.IServiceAdapterWithInvokeTask":U).

        oServiceAdapter = CAST (FrameworkSettings:ServiceAdapter, IServiceAdapterWithInvokeTask).

        oParameter = NEW AuthorizationMaintenanceServiceParameter (cSecurityRealmGuid,
                                                                   oOwnerList:Keys,
                                                                   THIS-OBJECT:AlternativeLoginCompanyKey) .

        DATASET dsAuthorizationMaintenance:EMPTY-DATASET ().

        oServiceAdapter:InvokeTask ("":U,
                                    "Consultingwerk.SmartFramework.Authorization.AuthorizationMaintenanceService":U,
                                    "GetAuthorization":U,
                                    INPUT-OUTPUT DATASET dsAuthorizationMaintenance,
                                    oParameter,
                                    SessionManager:ContextDataset).

        CATCH err AS Progress.Lang.Error :
            ErrorHelper:ShowErrorMessage (err) .
        END CATCH.

        FINALLY:
            GarbageCollectorHelper:DeleteObject (oParameter).
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Get the data of a SecurityItem
        Notes:
        @param pcMenuGuid Id of the requested menu item
        @param piEntryIndex Index within the extent fields of the Temp-Table
        @param pcTagValue The default level of a SecurityItem
        @return The cell data of the currently populated cell
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE CHARACTER GetSecurityItemData (pcMenuGuid AS CHARACTER,
                                                  piEntryIndex AS INTEGER,
                                                  OUTPUT pcTagValue AS CHARACTER):

        DEFINE VARIABLE cResult       AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cFunctionGuid AS CHARACTER NO-UNDO.

        cFunctionGuid = oMenuDatasetController:GetFunctionGuidOfMenuItem (pcMenuGuid).

        FIND FIRST ttAuthorizationMaintenance WHERE ttAuthorizationMaintenance.SecurityItemGuid = cFunctionGuid NO-LOCK NO-ERROR .

        IF AVAILABLE ttAuthorizationMaintenance THEN DO:

            pcTagValue = ttAuthorizationMaintenance.DefaultLevel[piEntryIndex].

            IF ttAuthorizationMaintenance.Restricted[piEntryIndex] NE ? THEN
                IF ttAuthorizationMaintenance.Restricted[piEntryIndex] THEN
                    cResult = "Restricted":U.
                ELSE
                    cResult = "Unrestricted":U.
            ELSE
                cResult = SUBSTITUTE ("Default (&1)":U, pcTagValue).
        END.

        RETURN cResult.

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
        THIS-OBJECT:components = NEW System.ComponentModel.Container().
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE resources AS Progress.Util.ResourceManager NO-UNDO.
        resources = NEW Progress.Util.ResourceManager("Consultingwerk.Windows.Framework.Authorization.SmartMenuSecurityMaintenanceControl":U).
        @VisualDesigner.FormMember (NeedsInitialize="true":U).
        DEFINE VARIABLE m_override1 AS Infragistics.Win.UltraWinTree.Override NO-UNDO.
        m_override1 = NEW Infragistics.Win.UltraWinTree.Override().
        THIS-OBJECT:MenuTree = NEW Infragistics.Win.UltraWinTree.UltraTree().
        THIS-OBJECT:MenuAdapter = NEW Consultingwerk.SmartComponents.Implementation.SmartBusinessEntityAdapter(THIS-OBJECT:components).
        CAST(THIS-OBJECT:MenuTree, System.ComponentModel.ISupportInitialize):BeginInit().
        CAST(THIS-OBJECT:MenuAdapter, System.ComponentModel.ISupportInitialize):BeginInit().
        THIS-OBJECT:SuspendLayout().
        /*  */
        /* MenuTree */
        /*  */
        THIS-OBJECT:MenuTree:AllowDrop = TRUE.
        THIS-OBJECT:MenuTree:DisplayStyle = Infragistics.Win.UltraWinTree.UltraTreeDisplayStyle:WindowsVista.
        resources:ApplyResources(THIS-OBJECT:MenuTree, "MenuTree":U).
        THIS-OBJECT:MenuTree:HideSelection = FALSE.
        THIS-OBJECT:MenuTree:Name = "MenuTree":U.
        m_override1:SelectionType = Infragistics.Win.UltraWinTree.SelectType:Single.
        THIS-OBJECT:MenuTree:Override = m_override1.
        THIS-OBJECT:MenuTree:BeforeExpand:Subscribe(THIS-OBJECT:MenuTree_BeforeExpand).
        THIS-OBJECT:MenuTree:BeforeCellActivate:Subscribe(THIS-OBJECT:MenuTree_BeforeCellActivate).
        THIS-OBJECT:MenuTree:CellValueChanged:Subscribe(THIS-OBJECT:MenuTree_CellValueChanged).
        /*  */
        /* MenuAdapter */
        /*  */
        THIS-OBJECT:MenuAdapter:BindingSourceExcludeFields = "ParentMenuGuid,MenuSequence":U.
        THIS-OBJECT:MenuAdapter:DatasetControllerType = "Consultingwerk.SmartFramework.Menu.MenuDatasetController":U.
        THIS-OBJECT:MenuAdapter:EntityJoin = "YES":U.
        THIS-OBJECT:MenuAdapter:EntityName = "Consultingwerk.SmartFramework.Menu.MenuBusinessEntity":U.
        THIS-OBJECT:MenuAdapter:EntityTable = "eSmartMenu":U.
        THIS-OBJECT:MenuAdapter:EntityView = "eSmartFunction":U.
        THIS-OBJECT:MenuAdapter:ForeignFields = "":U.
        THIS-OBJECT:MenuAdapter:LinkCommitSource = ?.
        THIS-OBJECT:MenuAdapter:LinkDataSource = ?.
        THIS-OBJECT:MenuAdapter:LinkNavigationSource = ?.
        THIS-OBJECT:MenuAdapter:NonUpdatableColumns = "":U.
        THIS-OBJECT:MenuAdapter:QuerySort = "":U.
        THIS-OBJECT:MenuAdapter:ThrowErrorsFromRetrieveData = FALSE.
        THIS-OBJECT:MenuAdapter:AfterRetrieveData:Subscribe(THIS-OBJECT:MenuAdapter_AfterRetrieveData).
        /*  */
        /* SmartMenuSecurityMaintenanceControl */
        /*  */
        resources:ApplyResources(THIS-OBJECT, "$this":U).
        THIS-OBJECT:Controls:Add(THIS-OBJECT:MenuTree).
        THIS-OBJECT:Name = "SmartMenuSecurityMaintenanceControl":U.
        CAST(THIS-OBJECT:MenuTree, System.ComponentModel.ISupportInitialize):EndInit().
        CAST(THIS-OBJECT:MenuAdapter, System.ComponentModel.ISupportInitialize):EndInit().
        THIS-OBJECT:ResumeLayout(FALSE).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Set the type of editor before activating the cell
        Notes:
        @param sender The source of the event
        @param e An EventArgs that contains no event data
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID MenuTree_BeforeCellActivate (sender AS System.Object,
                                                     e AS Infragistics.Win.UltraWinTree.BeforeCellActivateEventArgs):

        DEFINE VARIABLE oValueList   AS Infragistics.Win.ValueList                        NO-UNDO.
        DEFINE VARIABLE oComboEditor AS Infragistics.Win.UltraWinEditors.UltraComboEditor NO-UNDO.

        Consultingwerk.Assertion.ListAssert:EntryIsInList (UNBOX (e:Cell:Tag),
                                                           SUBSTITUTE ("Label,&1,&2,&3,&4,&5,&6":U,
                                                                       RestrictedDefaultLevel:NOTRESTRICTED-BY-REALM,
                                                                       RestrictedDefaultLevel:RESTRICTED-BY-REALM,
                                                                       RestrictedDefaultLevel:NOTRESTRICTED-FOR-ALL,
                                                                       RestrictedDefaultLevel:RESTRICTED-FOR-ALL,
                                                                       RestrictedDefaultLevel:NOTRESTRICTED-BY-GROUP,
                                                                       RestrictedDefaultLevel:RESTRICTED-BY-GROUP)) .

        CASE STRING (UNBOX (e:Cell:Tag)) :
            WHEN RestrictedDefaultLevel:NOTRESTRICTED-BY-REALM THEN oValueList = oRealmValueListNotRestricted.
            WHEN RestrictedDefaultLevel:RESTRICTED-BY-REALM    THEN oValueList = oRealmValueListRestricted.
            WHEN RestrictedDefaultLevel:NOTRESTRICTED-FOR-ALL  THEN oValueList = oAllValueListNotRestricted.
            WHEN RestrictedDefaultLevel:RESTRICTED-FOR-ALL     THEN oValueList = oAllValueListRestricted.
            WHEN RestrictedDefaultLevel:NOTRESTRICTED-BY-GROUP THEN oValueList = oGroupValueListNotRestricted.
            WHEN RestrictedDefaultLevel:RESTRICTED-BY-GROUP    THEN oValueList = oGroupValueListRestricted.
            OTHERWISE DO:
/*                MESSAGE STRING (UNBOX (e:Cell:Tag))*/
/*                    VIEW-AS ALERT-BOX.             */
                RETURN.
            END.
        END CASE.

        oComboEditor = NEW Infragistics.Win.UltraWinEditors.UltraComboEditor ().

        oComboEditor:DropDownStyle = Infragistics.Win.DropDownStyle:DropDownList.
        oComboEditor:ValueList = oValueList.

        e:Cell:EditorControl = oComboEditor.

        CATCH ple AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple).
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Write back changed Data to the Dataset
        Notes:
        @param sender The source of the event
        @param e An EventArgs that contains no event data
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID MenuTree_CellValueChanged (sender AS System.Object,
                                                   e AS Infragistics.Win.UltraWinTree.CellValueChangedEventArgs):

        DEFINE VARIABLE cFunctionGuid AS CHARACTER NO-UNDO.
        DEFINE VARIABLE iEntryIndex   AS INTEGER   NO-UNDO.

        IF lSynchronizing THEN
            RETURN.

        cFunctionGuid = oMenuDatasetController:GetFunctionGuidOfMenuItem (e:Node:Key).

        FIND FIRST ttAuthorizationMaintenance WHERE ttAuthorizationMaintenance.SecurityItemGuid = cFunctionGuid NO-LOCK NO-ERROR .

        iEntryIndex = e:Column:Index.

        IF AVAILABLE ttAuthorizationMaintenance THEN DO:
            CASE e:CurrentValue:ToString () :
                WHEN "Restricted":U THEN ttAuthorizationMaintenance.Restricted[iEntryIndex] = TRUE.
                WHEN "Unrestricted":U THEN ttAuthorizationMaintenance.Restricted[iEntryIndex] = FALSE.

                OTHERWISE ttAuthorizationMaintenance.Restricted[iEntryIndex] = ? .
            END CASE.
        END.

        lSynchronizing = TRUE.

        SynchronizeItems (MenuTree:Nodes, cFunctionGuid, e).

        CATCH ple AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple).
        END CATCH.

        FINALLY:
            lSynchronizing = FALSE.

            THIS-OBJECT:HasPendingChanges = TRUE .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Invokes the population of the tree after retrieving data.
        Notes:
        @param sender The source of the event
        @param e An EventArgs that contains no event data
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID MenuAdapter_AfterRetrieveData (sender AS System.Object,
                                                       e      AS System.EventArgs):

        Consultingwerk.Framework.FrameworkSettings:WaitStateManager:SetWaitState () .

        Consultingwerk.Util.GarbageCollectorHelper:ClearUltraTreeNodes (THIS-OBJECT:MenuTree:Nodes).
        THIS-OBJECT:MenuTree:Nodes:Clear ().

        IF NOT VALID-OBJECT (oMenuDatasetController) AND
           VALID-OBJECT (MenuAdapter:DatasetController) AND
           TYPE-OF (MenuAdapter:DatasetController, Consultingwerk.SmartFramework.Menu.MenuDatasetController) THEN
            oMenuDatasetController = CAST (MenuAdapter:DatasetController, Consultingwerk.SmartFramework.Menu.MenuDatasetController).

        CreateRootNode ().

        PopulateTree (oRootNode).

        SelectFirstMenuEntry ().

        CATCH ple AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple).
        END CATCH.

        FINALLY:
            Consultingwerk.Framework.FrameworkSettings:WaitStateManager:ClearWaitState () .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Invokes the population of the tree after a node has been selected
                 and removes the DummyNode if nessesary.
        Notes:
        @param sender The source of the event
        @param e A CancelableEventArgs that contains the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID MenuTree_BeforeExpand (sender AS System.Object,
                                               e      AS Infragistics.Win.UltraWinTree.CancelableNodeEventArgs):

        DEFINE VARIABLE oNode AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.

        IF e:TreeNode:Nodes:Count = 1 THEN DO:

            IF e:TreeNode:Nodes[0]:Key BEGINS "DummyNode":U THEN DO:
                oNode = e:TreeNode:Nodes[0].

                e:TreeNode:Nodes:Remove (oNode).
                oNode:Dispose ().
                DELETE OBJECT oNode.
            END.

            PopulateTree (e:TreeNode).

        END.

        CATCH ple AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (ple).
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Raises the HasPendingChangesChanged
        Notes:
        @param e The Consultingwerk.EventArgs with the data for the event
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID OnHasPendingChangesChanged (e AS Consultingwerk.EventArgs):

        IF NOT VALID-OBJECT (e) THEN
            e = Consultingwerk.EventArgs:Empty .

        THIS-OBJECT:HasPendingChangesChanged:Publish (THIS-OBJECT, e) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Opens the User and Group Authorization for the given users and
                 group identified by the entries in the provided CharacterList instance
        Notes:
        @param poOwnerList The CharacterList providing the GUID's of the Users and Groups to load
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID OpenUserAndGroupAuthorization (poOwnerList AS CharacterDictionary):

        IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
            FrameworkSettings:WaitStateManager:SetWaitState ("Loading Security Settings":U,
                                                             THIS-OBJECT) .

        ASSIGN oOwnerList         = poOwnerList
               lRefreshHeaderInfo = TRUE.

        GetAuthorizationData ().

        MenuAdapter:BatchSize = 0 .
        MenuAdapter:RetrieveData ().

        PROCESS EVENTS.

        MenuTree:ExpandAll ().

        FINALLY:
            IF VALID-OBJECT (FrameworkSettings:WaitStateManager) THEN
                FrameworkSettings:WaitStateManager:ClearWaitState ("Loading Security Settings":U,
                                                                   THIS-OBJECT) .

            THIS-OBJECT:HasPendingChanges = FALSE .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Populates the details of a Node by iterating thru all columns.
        Notes:
        @param poColumnSet The columnset associated with the current node
        @param poNode The node to be populated
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID PopulateNodeCells (poColumnSet AS UltraTreeColumnSet,
                                           poNode AS UltraTreeNode):

        DEFINE VARIABLE iColumn      AS INTEGER   NO-UNDO.
        DEFINE VARIABLE cCellValue   AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cTagValue    AS CHARACTER NO-UNDO.
        DEFINE VARIABLE lEnableFiels AS LOGICAL   NO-UNDO.
        DEFINE VARIABLE lColumnVisible AS LOGICAL NO-UNDO.

        DO iColumn = 0 TO poColumnSet:Columns:Count - 1 :

            IF poColumnSet:Columns[iColumn]:Key = "MenuStructure":U THEN DO:

                ASSIGN
                    cTagValue  = "Label":U
                    lColumnVisible = TRUE.

                IF poNode:Equals (oRootNode) THEN
                    ASSIGN cCellValue = "Menu Structure":U.
                ELSE
                    cCellValue = oMenuDatasetController:GetMenuNameFromMenuGuid (poNode:Key).

            END.
            ELSE
                ASSIGN
                    cCellValue   = GetSecurityItemData (poNode:Key, iColumn, cTagValue)
                    lEnableFiels = cTagValue NE "":U
                    lColumnVisible = iColumn <= oOwnerList:Count
                    .

            IF iColumn >= 1 AND
               iColumn <= oOwnerList:Count THEN DO:
                IF lEnableFiels THEN
                    oTreeHelper:SetNodeColumnsEditable (poNode, 2).

                poNode:Cells[iColumn]:Column:Text = ENTRY (iColumn, oOwnerList:Values, oOwnerList:ValueDelimiter) .

                IF CAN-FIND (FIRST eSmartUser WHERE eSmartUser.UserGuid = ENTRY (iColumn, oOwnerList:Keys, oOwnerList:KeyDelimiter)) THEN
                    poNode:Cells[iColumn]:Column:HeaderAppearance:Image = oUserImage .
                ELSE
                    poNode:Cells[iColumn]:Column:HeaderAppearance:Image = oGroupImage .

                poNode:Cells[iColumn]:Column:HeaderAppearance:ImageHAlign  = HAlign:Left .
                poNode:Cells[iColumn]:Column:HeaderAppearance:ImageVAlign  = VAlign:Middle .

            END.

            poNode:Cells[iColumn]:Column:Visible = lColumnVisible.

            poNode:Cells[iColumn]:Tag   = cTagValue.
            poNode:Cells[iColumn]:Value = cCellValue.
            cCellValue = "?":U.
            lEnableFiels = FALSE.
        END.

        FINALLY:
            lRefreshHeaderInfo = FALSE.
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Populates all child records of the given GUID to the tree.
        Notes:
        @param poParentNode Object reference to the parent UltraTreeNode
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID PopulateTree (poParentNode AS Infragistics.Win.UltraWinTree.UltraTreeNode):

        DEFINE VARIABLE hQuery          AS HANDLE    NO-UNDO.
        DEFINE VARIABLE hBuffer         AS HANDLE    NO-UNDO.
        DEFINE VARIABLE cParentMenuGuid AS CHARACTER NO-UNDO.

        DEFINE VARIABLE oParentCollection AS Infragistics.Win.UltraWinTree.TreeNodesCollection NO-UNDO.

        IF VALID-OBJECT (poParentNode) AND poParentNode:HasNodes THEN
            RETURN.

        hBuffer = MenuAdapter:DataSet::eSmartMenu.

        IF VALID-OBJECT (poParentNode) THEN
            IF poParentNode:Equals(oRootNode) THEN
                ASSIGN
                    oParentCollection = poParentNode:Nodes
                    cParentMenuGuid   = "":U
                    .
            ELSE
                ASSIGN
                    oParentCollection = poParentNode:Nodes
                    cParentMenuGuid   = poParentNode:Key
                    .
        ELSE
            ASSIGN
                oParentCollection = MenuTree:Nodes
                cParentMenuGuid   = "":U
                .

        hQuery = Consultingwerk.Util.QueryHelper:CreatePreparedQuery (hBuffer,
                                                                      SUBSTITUTE ("FOR EACH eSmartMenu WHERE eSmartMenu.ParentMenuGuid = '&1'":U,
                                                                                  cParentMenuGuid)).

        DO WHILE NOT hQuery:QUERY-OFF-END ON ERROR UNDO, THROW:

            DEFINE VARIABLE oNode      AS Infragistics.Win.UltraWinTree.UltraTreeNode NO-UNDO.
            DEFINE VARIABLE oColumnSet AS UltraTreeColumnSet                          NO-UNDO.

            oNode = NEW Infragistics.Win.UltraWinTree.UltraTreeNode (hBuffer::MenuGuid, hBuffer::MenuName).

            oColumnSet = oTreeHelper:GetOrCreateNodeLevelBasedColumnSet (0).

            IF hBuffer::HasChild THEN
                CreateDummyChildNode (oNode).

            oParentCollection:Add (oNode).

            PopulateNodeCells (oColumnSet, oNode).

            hQuery:GET-NEXT ().
        END.

        FINALLY:
            IF VALID-HANDLE (hQuery) THEN
                DELETE OBJECT hQuery.
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Reloads authorization for the current owners
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID Reload ():

        IF VALID-OBJECT (oOwnerList) THEN
            THIS-OBJECT:Cancel () .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Selects the first real Menu Entry if possible
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID SelectFirstMenuEntry ():

        MenuTree:SelectedNodes:Clear ().

        IF oRootNode:HasNodes THEN DO:
            oRootNode:Nodes[0]:BringIntoView ().
            oRootNode:Nodes[0]:Selected = TRUE.

            MenuTree:ActiveNode = oRootNode:Nodes[0].
        END.
        ELSE
            oRootNode:Selected = TRUE.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Setup the complete layout of the columns for all levelso
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID SetupNodeLevelLayout ():

        DEFINE VARIABLE oColumnSet       AS UltraTreeColumnSet NO-UNDO.
        DEFINE VARIABLE cColumnKey       AS CHARACTER          NO-UNDO.
        DEFINE VARIABLE cColumnKeyPrefix AS CHARACTER          NO-UNDO.
        DEFINE VARIABLE cKeySubstitute   AS CHARACTER          NO-UNDO.
        DEFINE VARIABLE cColumnHeader    AS CHARACTER          NO-UNDO.

        DEFINE VARIABLE iColumn          AS INTEGER            NO-UNDO.
        DEFINE VARIABLE iLevel           AS INTEGER            NO-UNDO.
        DEFINE VARIABLE iColumnWidth     AS INTEGER            NO-UNDO.

        DO iLevel = 0 TO 9 :
            DO iColumn = 0 TO 20 :

                CASE iColumn :
                    WHEN 0 THEN ASSIGN
                                    cColumnKeyPrefix = "MenuStructure":U
                                    cKeySubstitute   = "&1":U
                                    cColumnHeader    = "Menu Item":U
                                    iColumnWidth     = 160
                                    .
                    OTHERWISE
                        ASSIGN
                            cColumnKeyPrefix = "Security":U
                            cKeySubstitute   = "&1&2":U
                            cColumnHeader    = "Security":U
                            iColumnWidth     = 220
                            .
                END CASE.

                cColumnKey = SUBSTITUTE (cKeySubstitute, cColumnKeyPrefix, iColumn).

                IF iLevel = 0 AND
                   iColumn = 0 AND
                   NOT VALID-OBJECT (oTreeHelper) THEN
                    oTreeHelper = NEW UltraTreeHelper (THIS-OBJECT:MenuTree,
                                                       160,
                                                       cColumnKey).


                oTreeHelper:AddColumnToNodeLevelColumnSet (iLevel,
                                                           cColumnKey,
                                                           cColumnHeader,
                                                           "System.String":U).

                oColumnSet = oTreeHelper:GetOrCreateNodeLevelBasedColumnSet (iLevel).

                oColumnSet:Columns[cColumnKey]:LayoutInfo:PreferredCellSize  = NEW System.Drawing.Size (iColumnWidth, 0).
                oColumnSet:Columns[cColumnKey]:LayoutInfo:PreferredLabelSize = NEW System.Drawing.Size (iColumnWidth, 0).
            END.
        END.

        oTreeHelper:ColumnResizer:RegisterNodeLevelsForSyncronization (1, 1, 1).
        oTreeHelper:ColumnResizer:RegisterNodeLevelsForSyncronization (2, 1, 1).
        oTreeHelper:ColumnResizer:RegisterNodeLevelsForSyncronization (3, 1, 1).
        oTreeHelper:ColumnResizer:RegisterNodeLevelsForSyncronization (4, 1, 1).
        oTreeHelper:ColumnResizer:RegisterNodeLevelsForSyncronization (5, 1, 1).
        oTreeHelper:ColumnResizer:RegisterNodeLevelsForSyncronization (6, 1, 1).
        oTreeHelper:ColumnResizer:RegisterNodeLevelsForSyncronization (7, 1, 1).
        oTreeHelper:ColumnResizer:RegisterNodeLevelsForSyncronization (8, 1, 1).
        oTreeHelper:ColumnResizer:RegisterNodeLevelsForSyncronization (9, 1, 1).

        oTreeHelper:HideColumnHeaderForNodeLevel (1).
        oTreeHelper:HideColumnHeaderForNodeLevel (2).
        oTreeHelper:HideColumnHeaderForNodeLevel (3).
        oTreeHelper:HideColumnHeaderForNodeLevel (4).
        oTreeHelper:HideColumnHeaderForNodeLevel (5).
        oTreeHelper:HideColumnHeaderForNodeLevel (6).
        oTreeHelper:HideColumnHeaderForNodeLevel (7).
        oTreeHelper:HideColumnHeaderForNodeLevel (8).
        oTreeHelper:HideColumnHeaderForNodeLevel (9).

        CATCH pleForEach AS Progress.Lang.Error :
            Consultingwerk.Util.ErrorHelper:ShowErrorMessage (pleForEach).
        END CATCH.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Stores changed Authorization data to the database
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID StoreAuthorizationData ():

        DEFINE VARIABLE oParameter      AS AuthorizationMaintenanceServiceParameter NO-UNDO .
        DEFINE VARIABLE oServiceAdapter AS IServiceAdapterWithInvokeTask            NO-UNDO .

        ObjectAssert:IsValid (FrameworkSettings:ServiceAdapter).
        ObjectAssert:TypeOf (FrameworkSettings:ServiceAdapter, "Consultingwerk.OERA.IServiceAdapterWithInvokeTask":U).

        oServiceAdapter = CAST (FrameworkSettings:ServiceAdapter, IServiceAdapterWithInvokeTask).

        oParameter = NEW AuthorizationMaintenanceServiceParameter (cSecurityRealmGuid,
                                                                   oOwnerList:Keys,
                                                                   THIS-OBJECT:AlternativeLoginCompanyKey) .

        oServiceAdapter:InvokeTask ("":U,
                                    "Consultingwerk.SmartFramework.Authorization.AuthorizationMaintenanceService":U,
                                    "StoreAuthorization":U,
                                    INPUT-OUTPUT DATASET dsAuthorizationMaintenance,
                                    oParameter,
                                    SessionManager:ContextDataset).

        FINALLY:
            GarbageCollectorHelper:DeleteObject (oParameter).

            THIS-OBJECT:HasPendingChanges = FALSE .
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Synchronization of all TreeEntries applying to the same FunctionGuid.
                 If one is changed all others have to reflect the same value.
        Notes:
        @param poTreeNodesCollection TreeNodesCollection to be processed
        @param pcFunctionGuid The Guid of the Function that was changed
        @param e An EventArgs instance that contains the data for the CellValueChanged event of the tree.
    ------------------------------------------------------------------------------*/
    METHOD PRIVATE VOID SynchronizeItems (poTreeNodesCollection AS TreeNodesCollection,
                                          pcFunctionGuid AS CHARACTER,
                                          e AS Infragistics.Win.UltraWinTree.CellValueChangedEventArgs):

        {Consultingwerk/foreach.i Infragistics.Win.UltraWinTree.UltraTreeNode oNode in poTreeNodesCollection}

            IF oMenuDatasetController:GetFunctionGuidOfMenuItem (oNode:Key) = pcFunctionGuid THEN
                oNode:Cells[e:Column:Index]:Value = e:CurrentValue.

            IF oNode:Nodes:Count > 0 THEN
                SynchronizeItems (oNode:Nodes, pcFunctionGuid, e).

        END.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartMenuSecurityMaintenanceControl class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartMenuSecurityMaintenanceControl ():

    END DESTRUCTOR .

END CLASS.
