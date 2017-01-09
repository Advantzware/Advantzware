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
    File        : ISmartToolbarControl
    Purpose     : Interface for the callbacks from the SmartToolbarStateController
                  to the SmartToolbarController and the SmartRadRibbonController
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Apr 08 11:34:10 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.Collections.*          FROM PROPATH .
USING Consultingwerk.SmartComponents.Enum.*           FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.*     FROM PROPATH .
USING Progress.Lang.*                                 FROM PROPATH .

INTERFACE Consultingwerk.SmartComponents.Interfaces.ISmartToolbarControl:

    /**
     * Purpose: Activate the given SmartNavigationTarget if it is registered with
     *          the SmartNavigationSource.
     * Notes:   This Method has to be called if one Toolbar handels two or more
     *          SmartDataSources at a time.
     *          Enforced by Interface ISmartNavigationSource.
     * @param poSmartNavigationTarget The ISmartNavigationTarget instance to activate
     */
    METHOD PUBLIC VOID ActivateSmartNavigationTarget (poSmartNavigationTarget AS ISmartNavigationTarget).

    /**
     * Purpose: Activate the given SmartTableIOTarget if it is registered as a
     *          SmartTableIOSource.
     * Notes:   This Method has to be called if one Toolbar handels two or more
     *          SmartDataSources at a time.
     *          Enforced by the Interface ISmartTableIOSource.
     * @param poSmartTableIOTarget The ISmartTableIOTarget to activate
     */
    METHOD PUBLIC VOID ActivateSmartTableIOTarget (poSmartTableIOTarget AS ISmartTableIOTarget) .

    /**
     * Purpose: Propery flags if the current instance of the SmartBindingSource
     *          is running at design time or runtime.
     * Notes:   Set during constructor using a check at the
     *          System.ComponentModel.LicenseManager
     */
    DEFINE PUBLIC PROPERTY DesignTime AS LOGICAL NO-UNDO
    GET.

    /**
     * Purpose: Begins a series of updates to the Control
     * Notes:   Note, this method is actually typically proxy to the .NET BeginUpdate
     *          method. It's named differently as the AVM .NET bridge sometimes has
     *          issues with accessing .NET methods through ABL interfaces
     */
    METHOD PUBLIC VOID BeginControlUpdate ().

    /**
     * Purpose: Disables the tool with the given Key when it exists in this instance.
     *          Invoked by DisableTools.
     * Notes:
     * @param  pcToolKey Key of the Tool ButtonTool to be disabled
     * @return True if the Tool exists, False if the Tool does not Exist
     */
    METHOD PUBLIC LOGICAL DisableControlTool (pcToolKey AS CHARACTER) .

    /**
     * Purpose: Enables the tool with the given Key when it exists in this instance.
     * Notes:   Invoked by EnableTools.
     * @param  pcToolKey Key of the Tool ButtonTool to be enabled
     * @return True if the Tool exists, False if the Tool does not Exist
     */
    METHOD PUBLIC LOGICAL EnableControlTool (pcToolKey AS CHARACTER) .

    /**
     * Purpose: Ends a series of updates to the Control
     * Notes:   Note, this method is actually typically proxy to the .NET EndUpdate
     *          method. It's named differently as the AVM .NET bridge sometimes has
     *          issues with accessing .NET methods through ABL interfaces
     */
    METHOD PUBLIC VOID EndControlUpdate ().

    /**
     * Purpose: Returns a CharacterList of all ToolKeys in the Toolbar
     * Notes:
     * @return The CharacterList of all ToolKeys in the Toolbar
     */
    METHOD PUBLIC CharacterList GetAllToolKeys ().

    /**
     * Purpose: Returns the type name of the Form that contains the Toolbar
     * Notes:
     * @return The type name of the Form that contains the Toolbar
     */
    METHOD PUBLIC CHARACTER GetFormTypeName ().

    /**
     * Purpose: Returns if the Toolbar has a tool with the given key
     * Notes:
     * @param pcToolKey The key of the tool to check for
     * @return Logical value indicating if the tool has a key with the given name
     */
    METHOD PUBLIC LOGICAL HasTool (pcToolKey AS CHARACTER).

    /**
     * Purpose: Raises the ActiveSmartCommitTargetChanged event
     * Notes:
     */
    METHOD PUBLIC VOID RaiseActiveSmartCommitTargetChanged () .

    /**
     * Purpose: Returns a new instance of the TableIOTargetControl class
     * Notes:
     * @return The reference to the new instance of the TableIOTargetControl class
     */
    METHOD PUBLIC TableIOTargetControl InvokeNewTableIOTargetControl ().

    /**
     * Purpose: Allows to permanently disable TableIO Tools in the SmartToolbarController
     * Notes:   Useful for applying security settings to the toolbar
     *          http://confluence.consultingwerkcloud.com/wiki/pages/viewpage.action?pageId=722342
     *          See http://jira.consultingwerkcloud.com/browse/SCL-442 for details on
     *          extending this for custom buttons
     * @param poSmartTableIOTarget The reference to the ISmartTableIOTarget for this an action should be disabled
     * @param poTableIOAction The TableIOActionEnum member identifying the action to enable/disable
     * @param plDisabled Logical value indicating if the tool should be disabled. Set to FALSE to enable the tool
     */
    METHOD PUBLIC VOID SetTableIOActionDisabled (poSmartTableIOTarget AS ISmartTableIOTarget,
                                                 poTableIOAction AS TableIOActionEnum,
                                                 plDisabled AS LOGICAL) .

END INTERFACE.
