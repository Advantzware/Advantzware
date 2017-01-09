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
    File        : SmartToolbarStateController
    Purpose     : State controller for SmartToolbarController and
                  SmartRadRibbonController. Extracted the state handling logic
                  from the SmartToolbarController to ensure the same state
                  model is implemented in both Ribbon style Smart Toolbars
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Apr 08 11:31:38 CEST 2015
    Notes       : This class is for internal use only!
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Interfaces.* FROM PROPATH .
USING Progress.Lang.*                             FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.* FROM PROPATH.
USING Consultingwerk.SmartComponents.Base.* FROM PROPATH.
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH.
USING Consultingwerk.Framework.Authorization.* FROM PROPATH.
USING Consultingwerk.Framework.Collections.* FROM PROPATH.
USING Consultingwerk.Assertion.* FROM PROPATH.

CLASS Consultingwerk.SmartComponents.Implementation.SmartToolbarStateController:

    DEFINE VARIABLE oSmartNavigationTarget AS ISmartNavigationTarget NO-UNDO .
    DEFINE VARIABLE oSmartTableIOTarget    AS ISmartTableIOTarget    NO-UNDO .

    DEFINE VARIABLE oRestrictedTools       AS CharacterList          NO-UNDO .

    /* Mike Fechner, Consultingwerk Ltd. 03.09.2013
       Static only on 10.2B to workaround too-many-temp-table issue
       Not static on 11.x to avoid issues with the combination of static
       and hybrids on 11.2 and 11.3, OE defect OE00240707
       http://knowledgebase.progress.com/articles/Article/000042229?popup=true */
    DEFINE PRIVATE
    &IF PROVERSION BEGINS "10.2":U &THEN
    STATIC
    &ENDIF TEMP-TABLE ttSmartNavigationTarget NO-UNDO
        FIELD RecordOwner             AS Progress.Lang.Object
        FIELD SmartNavigationTarget   AS Progress.Lang.Object
        FIELD SmartNavigationPosition AS CHARACTER
        INDEX RecordOwner IS UNIQUE RecordOwner SmartNavigationTarget.
        .

    /*------------------------------------------------------------------------------
        Purpose: Returns a reference to the current active SmartCommitTarget
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ActiveSmartCommitTarget AS ISmartCommitTarget NO-UNDO
    GET.
    SET (arg AS ISmartCommitTarget):
        THIS-OBJECT:ActiveSmartCommitTarget = arg .

        IF VALID-OBJECT (THIS-OBJECT:Toolbar) THEN
            THIS-OBJECT:Toolbar:RaiseActiveSmartCommitTargetChanged () .

        THIS-OBJECT:UpdateTransactionState () .
    END SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns a reference to the current active SmartNavigationTarget
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ActiveSmartNavigationTarget AS ISmartNavigationTarget NO-UNDO
    GET:
        RETURN oSmartNavigationTarget .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns a reference to the current active SmartTableIOTarget
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ActiveSmartTableIOTarget AS ISmartTableIOTarget NO-UNDO
    GET:
        RETURN oSmartTableIOTarget .
    END GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the List of TableIOTargetControl instances
        Notes:   See http://jira.consultingwerkcloud.com/browse/SCL-442 for details
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ListTableIOTargetControl AS ListTableIOTargetControl NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the List of SmartTableIOTargets as a generic List
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartTableIOTargets AS ListISmartTableIOTarget NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the Toolbar this control works with
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Toolbar AS ISmartToolbarControl NO-UNDO
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartToolbarStateController class
        Notes:
        @param poToolbar The reference to the SmartToolbarController or SmartRadRibbonController to work with
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartToolbarStateController (poToolbar AS ISmartToolbarControl):
        SUPER ().

        {Consultingwerk/Assertion/ObjectAssert/IsValid.i poToolbar """poToolbar"":U"} .

        ASSIGN THIS-OBJECT:Toolbar = poToolbar .

        THIS-OBJECT:SmartTableIOTargets = NEW ListISmartTableIOTarget () .
        THIS-OBJECT:ListTableIOTargetControl = NEW ListTableIOTargetControl () .

        oRestrictedTools = NEW CharacterList () .

    END CONSTRUCTOR.

     /*------------------------------------------------------------------------------
        Purpose: Activate the given SmartNavigationTarget if it is registered with
                 the SmartNavigationSource.
        Notes:   This Method has to be called if one Toolbar handels two or more
                 SmartDataSources at a time.
                 Enforced by Interface ISmartNavigationSource.
        @param poSmartNavigationTarget The ISmartNavigationTarget instance to activate
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ActivateSmartNavigationTarget (poSmartNavigationTarget AS ISmartNavigationTarget).

        DEFINE BUFFER ttSmartNavigationTarget FOR ttSmartNavigationTarget .

        IF poSmartNavigationTarget = ? THEN DO:
            oSmartNavigationTarget = ? .

            THIS-OBJECT:UpdateSmartNavigationTargetPosition (?, "":U).

            RETURN .
        END.

        FIND ttSmartNavigationTarget WHERE ttSmartNavigationTarget.RecordOwner           = THIS-OBJECT
                                       AND ttSmartNavigationTarget.SmartNavigationTarget = poSmartNavigationTarget NO-LOCK NO-ERROR.

        IF NOT AVAILABLE ttSmartNavigationTarget THEN
            UNDO, THROW NEW AppError ("Invalid SmartNavigationTarget!", 0).

        oSmartNavigationTarget = poSmartNavigationTarget .

        THIS-OBJECT:UpdateSmartNavigationTargetPosition (poSmartNavigationTarget,
                                                         ttSmartNavigationTarget.SmartNavigationPosition).

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Activate the given SmartTableIOTarget if it is registered as a
                 SmartTableIOSource.
        Notes:   This Method has to be called if one Toolbar handels two or more
                 SmartDataSources at a time.
                 Enforced by the Interface ISmartTableIOSource.
        @param poSmartTableIOTarget The ISmartTableIOTarget to activate
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID ActivateSmartTableIOTarget (poSmartTableIOTarget AS ISmartTableIOTarget).

        DEFINE VARIABLE oTableIOTargetControl AS TableIOTargetControl NO-UNDO .

        IF poSmartTableIOTarget = ? THEN DO:
            oSmartTableIOTarget = ? .

            THIS-OBJECT:UpdateSmartTableIOState (?, "":U).

            RETURN .
        END.

        oTableIOTargetControl = THIS-OBJECT:ListTableIOTargetControl:FindByTableIOTarget (poSmartTableIOTarget) .

        IF NOT VALID-OBJECT (oTableIOTargetControl) THEN
            UNDO, THROW NEW AppError ("Invalid SmartTableIOTarget!", 0).

        oSmartTableIOTarget = poSmartTableIOTarget.

        THIS-OBJECT:UpdateSmartTableIOState (poSmartTableIOTarget,
                                             oTableIOTargetControl:SmartTableIOState).

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Registers a new SmartCommitTarget
        Notes:
        @param poTarget The ISmartCommitTraget to add to this SmartToolbarController
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeregisterSmartCommitTarget (poTarget AS ISmartCommitTarget):

        IF poTarget = THIS-OBJECT:ActiveSmartCommitTarget THEN
            THIS-OBJECT:ActiveSmartCommitTarget = ? .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Deregisters a SmartNavigationTarget from the SmartNavigationSource.
        Notes:
        @param poSmartNavigationTarget The ISmartNavigationTarget to deregister
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeregisterSmartNavigationTarget (poSmartNavigationTarget AS ISmartNavigationTarget):

        DEFINE BUFFER ttSmartNavigationTarget FOR ttSmartNavigationTarget .

        FIND ttSmartNavigationTarget
            WHERE ttSmartNavigationTarget.RecordOwner           = THIS-OBJECT
              AND ttSmartNavigationTarget.SmartNavigationTarget = poSmartNavigationTarget
                    NO-ERROR .

        IF AVAILABLE ttSmartNavigationTarget THEN
          DELETE ttSmartNavigationTarget .

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Deregisters a SmartTableIOTarget with the SmartTableIOSource.
        Notes:
        @param poSmartTableIOTarget The ISmartTableIOTarget to deregister
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DeregisterSmartTableIOTarget (poSmartTableIOTarget AS ISmartTableIOTarget):

        DEFINE VARIABLE oTableIOTargetControl AS TableIOTargetControl NO-UNDO .

        IF oSmartTableIOTarget = poSmartTableIOTarget THEN DO:
            UpdateSmartTableIOState (?, "":U) .

            oSmartTableIOTarget = ?  .
        END.

        oTableIOTargetControl = THIS-OBJECT:ListTableIOTargetControl:FindByTableIOTarget (poSmartTableIOTarget) .

        IF VALID-OBJECT (oTableIOTargetControl) THEN
            THIS-OBJECT:ListTableIOTargetControl:Remove (oTableIOTargetControl) .

        THIS-OBJECT:SmartTableIOTargets:Remove(poSmartTableIOTarget) .

        poSmartTableIOTarget:SmartTableIOStateChanged:Unsubscribe (THIS-OBJECT:UpdateSmartTableIOState) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Handles disposing of the Toolbar Control during design time
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DesignTimeDisposedHandler ():

        DEFINE VARIABLE oNavigationTarget     AS ISmartNavigationTarget NO-UNDO .
        DEFINE VARIABLE oTableIOTargetControl AS TableIOTargetControl   NO-UNDO EXTENT .

        DEFINE VARIABLE i                     AS INTEGER                NO-UNDO.
        DEFINE VARIABLE iCount                AS INTEGER                NO-UNDO.

        FOR EACH ttSmartNavigationTarget WHERE ttSmartNavigationTarget.RecordOwner = THIS-OBJECT ON ERROR UNDO, THROW:

            oNavigationTarget = CAST (ttSmartNavigationTarget.SmartNavigationTarget,
                                      ISmartNavigationTarget) .

            IF oNavigationTarget:SmartNavigationSource = THIS-OBJECT THEN
                oNavigationTarget:SmartNavigationSource = ?  .

             CATCH err AS Progress.Lang.Error :
                /* ignore all errors from above */
             END CATCH.
        END.

        IF THIS-OBJECT:ListTableIOTargetControl:Count > 0 THEN DO:
            oTableIOTargetControl = THIS-OBJECT:ListTableIOTargetControl:ToArray() .

            ASSIGN iCount = EXTENT (oTableIOTargetControl) .

            DO i = 1 TO iCount ON ERROR UNDO, THROW:

                IF VALID-OBJECT (oTableIOTargetControl[i]:SmartTableIOTarget) AND
                    oTableIOTargetControl[i]:SmartTableIOTarget:SmartTableIOSource = THIS-OBJECT THEN
                    oTableIOTargetControl[i]:SmartTableIOTarget:SmartTableIOSource = ?  .

                CATCH err AS Progress.Lang.Error :
                    /* ignore all errors from above */
                END CATCH.

            END.
        END.

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Disables the tool with the given Key when it exists in this instance.
                 Invoked by DisableTools.
        Notes:
        @param  pcToolKey Key of the Tool ButtonTool to be disabled
        @return True if the Tool exists, False if the Tool does not Exist
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL DisableTool (pcToolKey AS CHARACTER):

        IF NOT VALID-OBJECT (THIS-OBJECT:Toolbar) THEN
            RETURN FALSE .

        IF THIS-OBJECT:Toolbar:DesignTime THEN
            RETURN FALSE .

        RETURN THIS-OBJECT:Toolbar:DisableControlTool (pcToolKey) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Disables a list of tools when they exist in this instance.
                 This Method is called from within the Toolbar depending on what
                 action has been performed and what actions are possible with the
                 current SmartTableIOTarget and SmartNavigationTarget.
        Notes:
        @param pcToolKeys Comma delimeted list of ButtonTool Keys to be disabled
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID DisableTools (pcToolKeys AS CHARACTER):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DO i = 1 TO NUM-ENTRIES(pcToolKeys):
            THIS-OBJECT:DisableTool (ENTRY(i, pcToolKeys)) .
        END.

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Enables the tool with the given Key when it exists in this instance.
        Notes:   Invoked by EnableTools.
        @param  pcToolKey Key of the Tool ButtonTool to be enabled
        @return True if the Tool exists, False if the Tool does not Exist
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL EnableTool (pcToolKey AS CHARACTER):

        DEFINE VARIABLE oTableIOTargetControl AS TableIOTargetControl NO-UNDO .

        IF NOT VALID-OBJECT (THIS-OBJECT:Toolbar) THEN
            RETURN FALSE .

        IF THIS-OBJECT:Toolbar:DesignTime THEN
            RETURN FALSE .

        IF pcToolKey BEGINS "TableIO":U AND VALID-OBJECT (oSmartTableIOTarget) AND THIS-OBJECT:Toolbar:HasTool (pcToolKey) THEN DO:

            oTableIOTargetControl = THIS-OBJECT:ListTableIOTargetControl:FindByTableIOTarget (oSmartTableIOTarget) .

            IF VALID-OBJECT (oTableIOTargetControl) THEN DO:
                IF oTableIOTargetControl:GetToolDisabled (pcToolKey) = TRUE THEN DO:
                    THIS-OBJECT:Toolbar:DisableControlTool (pcToolKey) .
                    RETURN FALSE .
                END.
            END.
        END.

        RETURN THIS-OBJECT:Toolbar:EnableControlTool (pcToolKey) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Enables a list of tools when they exist in this instance.
        Notes:   This Method is called from within the Toolbar depending on what
                 action has been performed and what actions are possible with the
                 current SmartTableIOTarget and SmartNavigationTarget.
        @param pcToolKeys Comma delimeted list of ButtonTool Keys to be disabled
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID EnableTools (pcToolKeys AS CHARACTER):

        DEFINE VARIABLE i AS INTEGER NO-UNDO.

        DO i = 1 TO NUM-ENTRIES(pcToolKeys):
            THIS-OBJECT:EnableTool (ENTRY(i, pcToolKeys)) .
        END.
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Allows to permanently disable TableIO Tools in the SmartToolbarController
        Notes:   Useful for applying security settings to the toolbar
                 http://confluence.consultingwerkcloud.com/wiki/pages/viewpage.action?pageId=722342
                 See http://jira.consultingwerkcloud.com/browse/SCL-442 for details on
                 extending this for custom buttons
        @param poSmartTableIOTarget The reference to the ISmartTableIOTarget for this an action should be disabled
        @param poTableIOAction The TableIOActionEnum member identifying the action to enable/disable
        @return Logical value indicating if the tool should be disabled. Set to FALSE to enable the tool
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL GetTableIOActionDisabled (poSmartTableIOTarget AS ISmartTableIOTarget,
                                                    poTableIOAction AS TableIOActionEnum):

        DEFINE VARIABLE oTableIOTargetControl AS TableIOTargetControl NO-UNDO .

        IF NOT VALID-OBJECT (poSmartTableIOTarget) OR NOT VALID-OBJECT (poTableIOAction) THEN
            RETURN ? .

        oTableIOTargetControl = THIS-OBJECT:ListTableIOTargetControl:FindByTableIOTarget (poSmartTableIOTarget) .

        /* Mike Fechner, Consultingwerk Ltd. 14.08.2010
           Error-Handling, when TableIO Target not known. */
        IF NOT VALID-OBJECT (oTableIOTargetControl) THEN
            UNDO, THROW NEW AppError ("Invalid TableIOTarget for GetTableIOActionDisabled.", 0) .

        CASE poTableIOAction:
            WHEN TableIOActionEnum:Add    THEN RETURN oTableIOTargetControl:TableIOAddDisabled    .
            WHEN TableIOActionEnum:Cancel THEN RETURN oTableIOTargetControl:TableIOCancelDisabled .
            WHEN TableIOActionEnum:Copy   THEN RETURN oTableIOTargetControl:TableIOCopyDisabled   .
            WHEN TableIOActionEnum:Delete THEN RETURN oTableIOTargetControl:TableIODeleteDisabled .
            WHEN TableIOActionEnum:Save   THEN RETURN oTableIOTargetControl:TableIOSaveDisabled   .
            WHEN TableIOActionEnum:Update THEN RETURN oTableIOTargetControl:TableIOUpdateDisabled .
            WHEN TableIOActionEnum:All    THEN RETURN oTableIOTargetControl:TableIOAddDisabled    AND
                                                      oTableIOTargetControl:TableIOCancelDisabled AND
                                                      oTableIOTargetControl:TableIOCopyDisabled   AND
                                                      oTableIOTargetControl:TableIODeleteDisabled AND
                                                      oTableIOTargetControl:TableIOSaveDisabled   AND
                                                      oTableIOTargetControl:TableIOUpdateDisabled .
        END CASE .

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Handles the Click event of a Tool
        Notes:   Depending on which ButtonTool has been clicked the corresponding
                 action is performed by calling a routine in the active
                 SmartNavigationTarget or SmartTableIOTarget.
                 This Method is executed as the Handler if the ToolClick Event of
                 the base class is fired.
        @param pcToolKey The key of the Tool that has been clicked
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID HandleToolClick (pcToolKey AS CHARACTER):

        IF VALID-OBJECT (oSmartNavigationTarget) THEN DO:
            CASE pcToolKey:
                /* Navigation Tools */
                WHEN "NavigationFirst":U
                    THEN oSmartNavigationTarget:FetchFirst () .

                WHEN "NavigationPrev":U
                    THEN oSmartNavigationTarget:FetchPrev () .

                WHEN "NavigationNext":U
                    THEN oSmartNavigationTarget:FetchNext () .

                WHEN "NavigationLast":U
                    THEN oSmartNavigationTarget:FetchLast () .
             END CASE .
        END.

        IF VALID-OBJECT(oSmartTableIOTarget) THEN DO:
             CASE pcToolKey:
                /* Table IO Tools */
                WHEN "TableIOAdd":U
                    THEN oSmartTableIOTarget:AddRecord () .

                WHEN "TableIOCopy":U
                    THEN oSmartTableIOTarget:CopyRecord () .

                WHEN "TableIOUpdate":U
                    THEN oSmartTableIOTarget:EnableFields (EnableFieldsEnum:Update) .

                WHEN "TableIOSave":U
                    THEN oSmartTableIOTarget:SaveChanges () .

                WHEN "TableIOCancel":U
                    THEN oSmartTableIOTarget:CancelUpdate () .

                WHEN "TableIODelete":U
                    THEN oSmartTableIOTarget:DeleteRecord () .
            END CASE .
        END.

        IF VALID-OBJECT(THIS-OBJECT:ActiveSmartCommitTarget) THEN DO:
            CASE pcToolKey:
               /* Transaction Tools */
               WHEN "TransactionUndo":U
                   THEN THIS-OBJECT:ActiveSmartCommitTarget:RejectTransaction () .

               WHEN "TransactionCommit":U
                   THEN THIS-OBJECT:ActiveSmartCommitTarget:CommitTransaction () .
            END CASE .
        END.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Returns if the given tool is restricted by the IToolbarItemSecurityService
        Notes:   Requires EndInit() to be executed first, not applicable to the
                 TableIO Tools as they are maintaind on a per SmartTableIOTarget base
        @param pcToolKey The key of the tool to test
        @return Logical value indicating of the given tool is restricted by the
                IToolbarItemSecurityService service
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC LOGICAL IsToolRestricted (pcToolKey AS CHARACTER):

        RETURN oRestrictedTools:ContainsValue (pcToolKey) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs Toolbar Security
        Notes:
        @param poSmartTableIOTarget The ISmartTableIOTarget to perform authorization for
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID PerformToolbarSecurity (poSmartTableIOTarget AS ISmartTableIOTarget):

        DEFINE VARIABLE oToolbarAuthorizationProvider AS IToolbarAuthorizationProvider NO-UNDO .
        DEFINE VARIABLE cSecurityKey                  AS CHARACTER                     NO-UNDO .
        DEFINE VARIABLE cFormTypeName                 AS CHARACTER                     NO-UNDO .

        oToolbarAuthorizationProvider = {Consultingwerk/get-service.i Consultingwerk.Framework.Authorization.IToolbarAuthorizationProvider} .

        IF NOT VALID-OBJECT (oToolbarAuthorizationProvider) THEN
            RETURN .

        ASSIGN cFormTypeName = THIS-OBJECT:Toolbar:GetFormTypeName() .

        IF cFormTypeName = ? THEN
            RETURN .

        IF poSmartTableIOTarget:SecurityKey > "":U THEN
            ASSIGN cSecurityKey = poSmartTableIOTarget:SecurityKey .
        ELSE
&IF NOT PROVERSION EQ "10.2B":U &THEN
            ASSIGN cSecurityKey = poSmartTableIOTarget:GetClass():TypeName .
&ELSE
            /* Mike Fechner, Consultingwerk Ltd. 14.03.2015
               GetClass():TypeName known to crash the AVM sometimes on 10.2B,
               so use GetType():FullName instead */
            IF TYPE-OF (poSmartTableIOTarget, System.Object) THEN
                ASSIGN cSecurityKey = CAST (CAST (poSmartTableIOTarget, Progress.Lang.Object), System.Object):GetType():FullName .
            ELSE
                RETURN .
&ENDIF

        THIS-OBJECT:SetTableIOActionDisabled (poSmartTableIOTarget,
                                              TableIOActionEnum:Add,
                                              oToolbarAuthorizationProvider:IsToolbarActionRestricted (THIS-OBJECT,
                                                                                                       "TableIOAdd":U,
                                                                                                       cFormTypeName,
                                                                                                       cSecurityKey)) .

        THIS-OBJECT:SetTableIOActionDisabled (poSmartTableIOTarget,
                                              TableIOActionEnum:Cancel,
                                              oToolbarAuthorizationProvider:IsToolbarActionRestricted (THIS-OBJECT,
                                                                                                       "TableIOCancel":U,
                                                                                                       cFormTypeName,
                                                                                                       cSecurityKey)) .

        THIS-OBJECT:SetTableIOActionDisabled (poSmartTableIOTarget,
                                              TableIOActionEnum:Copy,
                                              oToolbarAuthorizationProvider:IsToolbarActionRestricted (THIS-OBJECT,
                                                                                                       "TableIOCopy":U,
                                                                                                       cFormTypeName,
                                                                                                       cSecurityKey)) .

        THIS-OBJECT:SetTableIOActionDisabled (poSmartTableIOTarget,
                                              TableIOActionEnum:Delete,
                                              oToolbarAuthorizationProvider:IsToolbarActionRestricted (THIS-OBJECT,
                                                                                                       "TableIODelete":U,
                                                                                                       cFormTypeName,
                                                                                                       cSecurityKey)) .
        THIS-OBJECT:SetTableIOActionDisabled (poSmartTableIOTarget,
                                              TableIOActionEnum:Save,
                                              oToolbarAuthorizationProvider:IsToolbarActionRestricted (THIS-OBJECT,
                                                                                                       "TableIOSave":U,
                                                                                                       cFormTypeName,
                                                                                                       cSecurityKey)) .
        THIS-OBJECT:SetTableIOActionDisabled (poSmartTableIOTarget,
                                              TableIOActionEnum:Update,
                                              oToolbarAuthorizationProvider:IsToolbarActionRestricted (THIS-OBJECT,
                                                                                                       "TableIOUpdate":U,
                                                                                                       cFormTypeName,
                                                                                                       cSecurityKey)) .
    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs Toolbar Security for non standard tools
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID PerformToolbarSecurity ():

        DEFINE VARIABLE oToolbarAuthorizationProvider AS IToolbarAuthorizationProvider NO-UNDO .
        DEFINE VARIABLE cSecurityKey                  AS CHARACTER                     NO-UNDO .
        DEFINE VARIABLE cFormTypeName                 AS CHARACTER                     NO-UNDO .

        DEFINE VARIABLE cTableIOTools     AS CHARACTER     NO-UNDO .
        DEFINE VARIABLE cNavigationTools  AS CHARACTER     NO-UNDO .
        DEFINE VARIABLE cTransactionTools AS CHARACTER     NO-UNDO .
        DEFINE VARIABLE oToolKeys         AS CharacterList NO-UNDO .

        oToolbarAuthorizationProvider = {Consultingwerk/get-service.i Consultingwerk.Framework.Authorization.IToolbarAuthorizationProvider} .

        IF NOT VALID-OBJECT (oToolbarAuthorizationProvider) THEN
            RETURN .

        cFormTypeName = THIS-OBJECT:Toolbar:GetFormTypeName() .

        IF cFormTypeName = ? THEN
            RETURN .

        ASSIGN cSecurityKey      = cFormTypeName
               cTableIOTools     = 'TableIOAdd,TableIOCopy,TableIOUpdate,TableIOSave,TableIOCancel,TableIODelete':U
               cNavigationTools  = 'NavigationFirst,NavigationPrev,NavigationNext,NavigationLast':U
               cTransactionTools = 'TransactionCommit,TransactionUndo':U .

        oToolKeys = THIS-OBJECT:Toolbar:GetAllToolKeys() .

        {Consultingwerk/foreachPrimitiveList.i Character cToolKey in oToolKeys}

            IF LOOKUP (cToolKey, cTableIOTools) > 0 OR
               LOOKUP (cToolKey, cNavigationTools) > 0 OR
               LOOKUP (cToolKey, cTransactionTools) > 0 THEN
                NEXT .

            IF oToolbarAuthorizationProvider:IsToolbarActionRestricted (THIS-OBJECT,
                                                                        cToolKey,
                                                                        cSecurityKey,
                                                                        cSecurityKey) THEN DO:
                oRestrictedTools:Add (cToolKey) .

                THIS-OBJECT:Toolbar:DisableControlTool (cToolKey) .
            END.
        END .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Registers a new SmartCommitTarget
        Notes:
        @param poTarget The ISmartCommitTarget to register
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RegisterSmartCommitTarget (poTarget AS ISmartCommitTarget):

        IF NOT VALID-OBJECT (THIS-OBJECT:ActiveSmartCommitTarget) THEN
            THIS-OBJECT:ActiveSmartCommitTarget = poTarget .

        poTarget:SmartTransactionStateChanged:Subscribe (SmartTransactionStateChangedHandler) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Registers a SmartNavigationTarget with the SmartNavigationSource.
        Notes:   This Method is invoked by the SmartDataAdapter in the setter of
                 the Property SmartNavigationSource.
                 Enforced by the Interface ISmartNavigationSource.
        @param poSmartNavigationTarget The ISmartNavigationTarget to register
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RegisterSmartNavigationTarget (poSmartNavigationTarget AS ISmartNavigationTarget):

        CREATE ttSmartNavigationTarget.

        ASSIGN
            ttSmartNavigationTarget.RecordOwner           = THIS-OBJECT
            ttSmartNavigationTarget.SmartNavigationTarget = poSmartNavigationTarget
            .

        RELEASE ttSmartNavigationTarget.

        IF NOT VALID-OBJECT(oSmartNavigationTarget) THEN
            ActivateSmartNavigationTarget(poSmartNavigationTarget).

        /* Update the tooblar buttons when the link is added */
        UpdateSmartNavigationTargetPosition (poSmartNavigationTarget, poSmartNavigationTarget:RecordPosition) .

    END METHOD.

     /*------------------------------------------------------------------------------
        Purpose: Registers a SmartTableIOTarget with the SmartTableIOSource.
        Notes:   This Method is invoked by the SmartDataAdapter in the setter of
                 the Property SmartTableIOSource.
                 Enforced by the Interface ISmartTableIOSource.
        @param poSmartTableIOTarget The ISmartTableIOTarget to register
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID RegisterSmartTableIOTarget (poSmartTableIOTarget AS ISmartTableIOTarget):

        DEFINE VARIABLE oTableIOTargetControl AS TableIOTargetControl NO-UNDO .

        oTableIOTargetControl = THIS-OBJECT:ListTableIOTargetControl:FindByTableIOTarget (poSmartTableIOTarget) .

        IF VALID-OBJECT (oTableIOTargetControl) THEN
            UNDO, THROW NEW AppError ("Unable to register SmartTableIOTarget twice.", 0) .

        oTableIOTargetControl = THIS-OBJECT:Toolbar:InvokeNewTableIOTargetControl () .
        oTableIOTargetControl:SmartTableIOTarget = poSmartTableIOTarget .

        THIS-OBJECT:ListTableIOTargetControl:Add (oTableIOTargetControl) .

        THIS-OBJECT:SmartTableIOTargets:Add (poSmartTableIOTarget) .

        IF NOT VALID-OBJECT(oSmartTableIOTarget) THEN
            ActivateSmartTableIOTarget(poSmartTableIOTarget).

        poSmartTableIOTarget:SmartTableIOStateChanged:Subscribe (THIS-OBJECT:UpdateSmartTableIOState) .

        THIS-OBJECT:PerformToolbarSecurity (poSmartTableIOTarget) .

        /* Update the tooblar buttons when the link is added */
        UpdateSmartTableIOState (poSmartTableIOTarget, poSmartTableIOTarget:SmartTableIOState) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Allows to permanently disable TableIO Tools in the SmartToolbarController
        Notes:   Useful for applying security settings to the toolbar
                 http://confluence.consultingwerkcloud.com/wiki/pages/viewpage.action?pageId=722342
                 See http://jira.consultingwerkcloud.com/browse/SCL-442 for details on
                 extending this for custom buttons
        @param poSmartTableIOTarget The reference to the ISmartTableIOTarget for this an action should be disabled
        @param poTableIOAction The TableIOActionEnum member identifying the action to enable/disable
        @param plDisabled Logical value indicating if the tool should be disabled. Set to FALSE to enable the tool
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetTableIOActionDisabled (poSmartTableIOTarget AS ISmartTableIOTarget,
                                                 poTableIOAction AS TableIOActionEnum,
                                                 plDisabled AS LOGICAL):

        DEFINE VARIABLE oTableIOTargetControl AS TableIOTargetControl NO-UNDO .

        IF NOT VALID-OBJECT (poSmartTableIOTarget) OR NOT VALID-OBJECT (poTableIOAction) THEN
            RETURN .

        oTableIOTargetControl = THIS-OBJECT:ListTableIOTargetControl:FindByTableIOTarget (poSmartTableIOTarget) .

        /* Mike Fechner, Consultingwerk Ltd. 14.08.2010
           Error-Handling, when TableIO Target not known. */
        IF NOT VALID-OBJECT (oTableIOTargetControl) THEN
            UNDO, THROW NEW AppError ("Invalid TableIOTarget for SetTableIOActionDisabled.", 0) .

        CASE poTableIOAction:
            WHEN TableIOActionEnum:Add    THEN oTableIOTargetControl:TableIOAddDisabled    = plDisabled .
            WHEN TableIOActionEnum:Cancel THEN oTableIOTargetControl:TableIOCancelDisabled = plDisabled .
            WHEN TableIOActionEnum:Copy   THEN oTableIOTargetControl:TableIOCopyDisabled   = plDisabled .
            WHEN TableIOActionEnum:Delete THEN oTableIOTargetControl:TableIODeleteDisabled = plDisabled .
            WHEN TableIOActionEnum:Save   THEN oTableIOTargetControl:TableIOSaveDisabled   = plDisabled .
            WHEN TableIOActionEnum:Update THEN oTableIOTargetControl:TableIOUpdateDisabled = plDisabled .
            WHEN TableIOActionEnum:All    THEN
                                        ASSIGN oTableIOTargetControl:TableIOAddDisabled    = plDisabled
                                               oTableIOTargetControl:TableIOCancelDisabled = plDisabled
                                               oTableIOTargetControl:TableIOCopyDisabled   = plDisabled
                                               oTableIOTargetControl:TableIODeleteDisabled = plDisabled
                                               oTableIOTargetControl:TableIOSaveDisabled   = plDisabled
                                               oTableIOTargetControl:TableIOUpdateDisabled = plDisabled .
        END CASE .

        UpdateSmartTableIOState (poSmartTableIOTarget,
                                 oTableIOTargetControl:SmartTableIOState) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Allows to permanently disable TableIO Tools in the SmartToolbarController
        Notes:   Useful for applying security settings to the toolbar
                 http://confluence.consultingwerkcloud.com/wiki/pages/viewpage.action?pageId=722342
                 See http://jira.consultingwerkcloud.com/browse/SCL-442 for details on
                 extending this for custom buttons
        @param poSmartTableIOTarget The reference to the ISmartTableIOTarget for this an action should be disabled
        @param poTableIOAction The TableIOActionEnum member identifying the action to enable/disable
        @param poTableIOAction2 The TableIOActionEnum member identifying the action to enable/disable
        @param plDisabled Logical value indicating if the tool should be disabled. Set to FALSE to enable the tool
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetTableIOActionDisabled (poSmartTableIOTarget AS ISmartTableIOTarget,
                                                 poTableIOAction AS TableIOActionEnum,
                                                 poTableIOAction2 AS TableIOActionEnum,
                                                 plDisabled AS LOGICAL):

        THIS-OBJECT:Toolbar:SetTableIOActionDisabled (poSmartTableIOTarget,
                                                      poTableIOAction,
                                                      plDisabled) .

        THIS-OBJECT:Toolbar:SetTableIOActionDisabled (poSmartTableIOTarget,
                                                      poTableIOAction2,
                                                      plDisabled) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Allows to permanently disable TableIO Tools in the SmartToolbarController
        Notes:   Useful for applying security settings to the toolbar
                 http://confluence.consultingwerkcloud.com/wiki/pages/viewpage.action?pageId=722342
                 See http://jira.consultingwerkcloud.com/browse/SCL-442 for details on
                 extending this for custom buttons
        @param poSmartTableIOTarget The reference to the ISmartTableIOTarget for this an action should be disabled
        @param poTableIOAction The TableIOActionEnum member identifying the action to enable/disable
        @param poTableIOAction2 The TableIOActionEnum member identifying the action to enable/disable
        @param poTableIOAction3 The TableIOActionEnum member identifying the action to enable/disable
        @param plDisabled Logical value indicating if the tool should be disabled. Set to FALSE to enable the tool
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetTableIOActionDisabled (poSmartTableIOTarget AS ISmartTableIOTarget,
                                                 poTableIOAction AS TableIOActionEnum,
                                                 poTableIOAction2 AS TableIOActionEnum,
                                                 poTableIOAction3 AS TableIOActionEnum,
                                                 plDisabled AS LOGICAL):

        THIS-OBJECT:Toolbar:SetTableIOActionDisabled (poSmartTableIOTarget,
                                                      poTableIOAction,
                                                      plDisabled) .

        THIS-OBJECT:Toolbar:SetTableIOActionDisabled (poSmartTableIOTarget,
                                                      poTableIOAction2,
                                                      plDisabled) .

        THIS-OBJECT:Toolbar:SetTableIOActionDisabled (poSmartTableIOTarget,
                                                      poTableIOAction3,
                                                      plDisabled) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Allows to permanently disable TableIO Tools in the SmartToolbarController
        Notes:   Useful for applying security settings to the toolbar
                 http://confluence.consultingwerkcloud.com/wiki/pages/viewpage.action?pageId=722342
                 See http://jira.consultingwerkcloud.com/browse/SCL-442 for details on
                 extending this for custom buttons
        @param poSmartTableIOTarget The reference to the ISmartTableIOTarget for this an action should be disabled
        @param poTableIOAction The TableIOActionEnum member identifying the action to enable/disable
        @param poTableIOAction2 The TableIOActionEnum member identifying the action to enable/disable
        @param poTableIOAction3 The TableIOActionEnum member identifying the action to enable/disable
        @param poTableIOAction4 The TableIOActionEnum member identifying the action to enable/disable
        @param plDisabled Logical value indicating if the tool should be disabled. Set to FALSE to enable the tool
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetTableIOActionDisabled (poSmartTableIOTarget AS ISmartTableIOTarget,
                                                 poTableIOAction AS TableIOActionEnum,
                                                 poTableIOAction2 AS TableIOActionEnum,
                                                 poTableIOAction3 AS TableIOActionEnum,
                                                 poTableIOAction4 AS TableIOActionEnum,
                                                 plDisabled AS LOGICAL):

        THIS-OBJECT:Toolbar:SetTableIOActionDisabled (poSmartTableIOTarget,
                                                      poTableIOAction,
                                                      plDisabled) .

        THIS-OBJECT:Toolbar:SetTableIOActionDisabled (poSmartTableIOTarget,
                                                      poTableIOAction2,
                                                      plDisabled) .

        THIS-OBJECT:Toolbar:SetTableIOActionDisabled (poSmartTableIOTarget,
                                                      poTableIOAction3,
                                                      plDisabled) .

        THIS-OBJECT:Toolbar:SetTableIOActionDisabled (poSmartTableIOTarget,
                                                      poTableIOAction4,
                                                      plDisabled) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Allows to permanently disable TableIO Tools in the SmartToolbarController
        Notes:   Useful for applying security settings to the toolbar
                 http://confluence.consultingwerkcloud.com/wiki/pages/viewpage.action?pageId=722342
                 See http://jira.consultingwerkcloud.com/browse/SCL-442 for details on
                 extending this for custom buttons
        @param poSmartTableIOTarget The reference to the ISmartTableIOTarget for this an action should be disabled
        @param poTableIOAction The TableIOActionEnum member identifying the action to enable/disable
        @param poTableIOAction2 The TableIOActionEnum member identifying the action to enable/disable
        @param poTableIOAction3 The TableIOActionEnum member identifying the action to enable/disable
        @param poTableIOAction4 The TableIOActionEnum member identifying the action to enable/disable
        @param poTableIOAction5 The TableIOActionEnum member identifying the action to enable/disable
        @param plDisabled Logical value indicating if the tool should be disabled. Set to FALSE to enable the tool
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SetTableIOActionDisabled (poSmartTableIOTarget AS ISmartTableIOTarget,
                                                 poTableIOAction AS TableIOActionEnum,
                                                 poTableIOAction2 AS TableIOActionEnum,
                                                 poTableIOAction3 AS TableIOActionEnum,
                                                 poTableIOAction4 AS TableIOActionEnum,
                                                 poTableIOAction5 AS TableIOActionEnum,
                                                 plDisabled AS LOGICAL):

        THIS-OBJECT:Toolbar:SetTableIOActionDisabled (poSmartTableIOTarget,
                                                      poTableIOAction,
                                                      plDisabled) .

        THIS-OBJECT:Toolbar:SetTableIOActionDisabled (poSmartTableIOTarget,
                                                      poTableIOAction2,
                                                      plDisabled) .

        THIS-OBJECT:Toolbar:SetTableIOActionDisabled (poSmartTableIOTarget,
                                                      poTableIOAction3,
                                                      plDisabled) .

        THIS-OBJECT:Toolbar:SetTableIOActionDisabled (poSmartTableIOTarget,
                                                      poTableIOAction4,
                                                      plDisabled) .

        THIS-OBJECT:Toolbar:SetTableIOActionDisabled (poSmartTableIOTarget,
                                                      poTableIOAction5,
                                                      plDisabled) .

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the SmartTransactionStateChanged event of the SmartCommitTarget
        Notes:
        @param sender The object the raised the event
        @param e The System.EventArgs with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID SmartTransactionStateChangedHandler (sender AS System.Object,
                                                            e AS System.EventArgs):

        IF sender = THIS-OBJECT:ActiveSmartCommitTarget THEN
            UpdateTransactionState () .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the SmartNavigationTargetPositionChanged event of the SmartNavigationTarget
        Notes:
        @param sender The reference to the object that raised the evnet
        @param e The SmartNavigationTargetPositionChangedEventArgs instance with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID UpdateSmartNavigationTargetPosition (sender AS System.Object,
                                                            e AS SmartNavigationTargetPositionChangedEventArgs):

        DEFINE VARIABLE oSmartNavigationTarget AS ISmartNavigationTarget NO-UNDO .

        IF VALID-OBJECT (sender) AND TYPE-OF (sender, ISmartNavigationTarget) THEN
            ASSIGN oSmartNavigationTarget = CAST (sender, ISmartNavigationTarget) .

        UpdateSmartNavigationTargetPosition(oSmartNavigationTarget, e:SmartNavigationTargetPosition) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Handles the update of a Position from the SmartNavigationTarget.
                 This Method is called from the SmartDataAdapter derived Classes
                 if the PositionChangedHandler is executed or from inside the
                 ISmartNavigationSource implementing class when the Method
                 UpdateSmartNavigationTargetState or RegisterSmartNavigationTarget
                 are executed.
        Notes:   Enforced by Interface ISmartNavigationSource.
        @param poSmartNavigationTarget The reference to the current SmartNavigationTarget
        @param pcPosition The RecordPositionEnum member indicating the new navigation target position
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID UpdateSmartNavigationTargetPosition (poSmartNavigationTarget AS ISmartNavigationTarget,
                                                            pcPosition AS CHARACTER):

        DEFINE VARIABLE cState AS CHARACTER NO-UNDO.

        DEFINE BUFFER ttSmartNavigationTarget FOR ttSmartNavigationTarget .

        IF VALID-OBJECT (THIS-OBJECT:Toolbar) THEN
            THIS-OBJECT:Toolbar:BeginControlUpdate ().

        IF poSmartNavigationTarget = ? THEN DO:
            /* Mike Fechner, Consultingwerk Ltd. 05.02.2010
               When there is no active Navigation Target, set all buttons to disabled
               and (this is new) leave this routine. The remaining code is not required. */
            THIS-OBJECT:DisableTools ("NavigationFirst,NavigationPrev,NavigationNext,NavigationLast":U) .
            RETURN .
        END.

        /* Mike Fechner, Consultingwerk Ltd. 28.01.2010
           SmartNavigationTargetState goes over SmartNavigationTargetPosition */
        ASSIGN cState = poSmartNavigationTarget:SmartDataSourceState .

        IF cState = DataSourceStateEnum:Navigatable THEN DO:
            IF poSmartNavigationTarget = oSmartNavigationTarget THEN DO:
                CASE pcPosition:
                    WHEN RecordPositionEnum:FirstRecord THEN DO:
                        THIS-OBJECT:EnableTools ("NavigationNext,NavigationLast":U) .
                        THIS-OBJECT:DisableTools ("NavigationFirst,NavigationPrev":U) .
                    END.
                    WHEN RecordPositionEnum:LastRecord THEN DO:
                        THIS-OBJECT:EnableTools ("NavigationFirst,NavigationPrev":U) .
                        THIS-OBJECT:DisableTools ("NavigationNext,NavigationLast":U) .
                    END.
                    WHEN RecordPositionEnum:NotFirstOrLast THEN DO:
                        THIS-OBJECT:EnableTools ("NavigationFirst,NavigationPrev,NavigationNext,NavigationLast":U) .
                    END.
                    OTHERWISE
                        THIS-OBJECT:DisableTools ("NavigationFirst,NavigationPrev,NavigationNext,NavigationLast":U) .
                END CASE.
            END.
        END.
        ELSE
            /* Mike Fechner, Consultingwerk Ltd. 28.01.2010
               Currently in Update mode etc. */
            THIS-OBJECT:DisableTools ("NavigationFirst,NavigationPrev,NavigationNext,NavigationLast":U) .

        FIND FIRST ttSmartNavigationTarget WHERE ttSmartNavigationTarget.RecordOwner           = THIS-OBJECT
                                           AND   ttSmartNavigationTarget.SmartNavigationTarget = poSmartNavigationTarget
            NO-LOCK NO-ERROR .

        IF AVAILABLE(ttSmartNavigationTarget) THEN DO:
            ASSIGN ttSmartNavigationTarget.SmartNavigationPosition = pcPosition.

            RELEASE ttSmartNavigationTarget.
        END.

        FINALLY:
            IF VALID-OBJECT (THIS-OBJECT:Toolbar) THEN
                THIS-OBJECT:Toolbar:EndControlUpdate ().
        END FINALLY.

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the SmartNavigationTargetStateChanged event
        Notes:
        @param sender The reference to the object that raised the event
        @param e The SmartNavigationTargetStateChangedEventArgs instance with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID UpdateSmartNavigationTargetState (sender AS System.Object,
                                                            e AS SmartNavigationTargetStateChangedEventArgs):

        DEFINE VARIABLE oSmartNavigationTarget AS ISmartNavigationTarget NO-UNDO .

        IF VALID-OBJECT (sender) AND TYPE-OF (sender, ISmartNavigationTarget) THEN
            ASSIGN oSmartNavigationTarget = CAST (sender, ISmartNavigationTarget) .

        UpdateSmartNavigationTargetState (oSmartNavigationTarget, e:SmartNavigationTargetState) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Handles the update of a State from the SmartNavigationTarget.
                 This Method is called from the SmartDataAdapter derived Classes
                 if the Property setter of SmartDataSourceState is executed.
        Notes:   Enforced by Interface ISmartNavigationSource.
        @param poSmartNavigationTarget The reference to the current SmartNavigationTarget
        @param pcState The RecordPositionEnum member indicating the new navigation target state, Updating or Navigatable
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID UpdateSmartNavigationTargetState (poSmartNavigationTarget AS ISmartNavigationTarget,
                                                         pcState AS CHARACTER):

        DEFINE VARIABLE cRecordPosition AS CHARACTER NO-UNDO.

        IF VALID-OBJECT (THIS-OBJECT:Toolbar) THEN
            THIS-OBJECT:Toolbar:BeginControlUpdate ().

        CASE pcState:
            WHEN DataSourceStateEnum:Navigatable THEN DO:
                IF VALID-OBJECT(poSmartNavigationTarget) THEN DO:
                    ASSIGN cRecordPosition = poSmartNavigationTarget:RecordPosition .

                    UpdateSmartNavigationTargetPosition (poSmartNavigationTarget, cRecordPosition) .
                END.
            END .
            WHEN DataSourceStateEnum:Updating THEN DO:
                THIS-OBJECT:DisableTools ("NavigationFirst,NavigationPrev,NavigationNext,NavigationLast":U) .
            END .
        END CASE.

        FINALLY:
            IF VALID-OBJECT (THIS-OBJECT:Toolbar) THEN
                THIS-OBJECT:Toolbar:EndControlUpdate ().
        END FINALLY.

    END METHOD .

     /*------------------------------------------------------------------------------
        Purpose: Handles the update of a State from the SmartTableIOTarget.
                 This Method is called from the SmartViewerControl derived Classes
                 if the Property setter of SmartTableIOState is executed.
        Notes:   Enforced by Interface ISmartTableIOSource.
        @param poSmartTableIOTarget The reference to the current ISmartTableIOTarget
        @param pcSmartTableIOState The current state of the SmartTableIOTarget instance, e.g. AddCopyAllowed, FieldsDisabled, FieldsEnabled, ModifyingData, NoDataSource
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID UpdateSmartTableIOState (poSmartTableIOTarget AS ISmartTableIOTarget,
                                                pcSmartTableIOState AS CHARACTER):

        DEFINE VARIABLE oTableIOTargetControl AS TableIOTargetControl NO-UNDO .

        IF VALID-OBJECT (THIS-OBJECT:Toolbar) THEN
            THIS-OBJECT:Toolbar:BeginControlUpdate().

        IF poSmartTableIOTarget = ? THEN
            DisableTools ("TableIOAdd,TableIOCopy,TableIOUpdate,TableIODelete,TableIOSave,TableIOCancel":U) .
        ELSE IF poSmartTableIOTarget = oSmartTableIOTarget THEN
            CASE pcSmartTableIOState:
                WHEN TableIOStateEnum:AddCopyAllowed THEN DO:
                    EnableTools ("TableIOAdd,TableIOCopy":U) .
                    DisableTools ("TableIOUpdate,TableIOSave,TableIOCancel,TableIODelete":U) .
                END.
                WHEN TableIOStateEnum:FieldsDisabled THEN DO:
                    EnableTools ("TableIOAdd,TableIOCopy,TableIOUpdate,TableIODelete":U) .
                    DisableTools ("TableIOSave,TableIOCancel":U) .
                END.
                WHEN TableIOStateEnum:FieldsEnabled THEN DO:
                    EnableTools ("TableIOAdd,TableIOCopy,TableIODelete,TableIOCancel":U) .
                    DisableTools ("TableIOUpdate,TableIOSave":U) .
                END.
                WHEN TableIOStateEnum:ModifyingData THEN DO:
                    EnableTools ("TableIOSave,TableIOCancel":U) .
                    DisableTools ("TableIOAdd,TableIOCopy,TableIOUpdate,TableIODelete":U) .
                END.
                WHEN TableIOStateEnum:NoDataSource OR WHEN TableIOStateEnum:NoParentRecordAvailable THEN DO:
    /*                EnableTools ("":U) .*/
                    DisableTools ("TableIOAdd,TableIOCopy,TableIOUpdate,TableIODelete,TableIOSave,TableIOCancel":U) .
                END.
                WHEN TableIOStateEnum:NoRecordAvailable THEN DO:
                    EnableTools ("TableIOAdd":U) .
                    DisableTools ("TableIOCopy,TableIOUpdate,TableIOSave,TableIOCancel,TableIODelete":U) .
                END.
            END CASE.

        oTableIOTargetControl = THIS-OBJECT:ListTableIOTargetControl:FindByTableIOTarget (poSmartTableIOTarget) .

        IF VALID-OBJECT (oTableIOTargetControl) THEN DO:
            ASSIGN oTableIOTargetControl:SmartTableIOState = pcSmartTableIOState.
        END.

        FINALLY:
            IF VALID-OBJECT (THIS-OBJECT:Toolbar) THEN
                THIS-OBJECT:Toolbar:EndControlUpdate ().
        END FINALLY.

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Event handler for the SmartTableIOStateChange event of the TableIO
                 Target
        Notes:
        @param sender The reference to the object that raised the event
        @param e The SmartTableIOStateChangedEventArgs instance with the data for this event
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID UpdateSmartTableIOState (sender AS System.Object,
                                                e AS SmartTableIOStateChangedEventArgs):

        DEFINE VARIABLE oSmartTableIOTarget AS ISmartTableIOTarget NO-UNDO .
        DEFINE VARIABLE cState              AS CHARACTER           NO-UNDO .

        IF TYPE-OF (sender, ISmartTableIOTarget) THEN
            oSmartTableIOTarget = CAST (sender, ISmartTableIOTarget) .

        IF VALID-OBJECT (e) THEN
            ASSIGN cState = e:SmartTableIOState .

        UpdateSmartTableIOState (oSmartTableIOTarget, cState) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Updates the Buttons based on the TransactionState of the current SmartCommitTarget
        Notes:
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID UpdateTransactionState ():

        IF NOT VALID-OBJECT (THIS-OBJECT:ActiveSmartCommitTarget) THEN DO:
            THIS-OBJECT:DisableTools ("TransactionCommit,TransactionUndo":U) .
            RETURN .
        END.

        IF THIS-OBJECT:ActiveSmartCommitTarget:SmartTransactionState = TransactionStateEnum:PendingChanges THEN
            THIS-OBJECT:EnableTools ("TransactionCommit,TransactionUndo":U) .
        ELSE
            THIS-OBJECT:DisableTools ("TransactionCommit,TransactionUndo":U) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Destructor for the SmartToolbarStateController class
        Notes:
    ------------------------------------------------------------------------------*/
    DESTRUCTOR PUBLIC SmartToolbarStateController ():

        DEFINE BUFFER ttSmartNavigationTarget FOR ttSmartNavigationTarget  .

        FOR EACH ttSmartNavigationTarget WHERE ttSmartNavigationTarget.RecordOwner = THIS-OBJECT:
            DELETE ttSmartNavigationTarget.
        END.

        THIS-OBJECT:ListTableIOTargetControl:Clear () .

    END DESTRUCTOR.

END CLASS.
