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
    File        : ISmartGroupAssignTarget
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Wed Mar 04 16:57:34 CET 2009
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Interfaces.* FROM PROPATH .
USING Progress.Lang.* FROM ASSEMBLY .

INTERFACE Consultingwerk.SmartComponents.Interfaces.ISmartGroupAssignTarget:

    /**
     * Purpose: Returns the current SmartGroupAssignTarget (if any)
     * Notes:   Required to detect cyclic links
     */
    DEFINE PUBLIC PROPERTY SmartGroupAssignSource AS ISmartGroupAssignSource NO-UNDO
    GET.

    /**
     * Purpose: The SmartTableIOState property reflects the operationmode of the
     *          SmartViewerControl.
     *          Value changes of this Property are only made in the Class
     *          SmartViewerControl.
     * Notes:   Possible values (TableIOStateEnum):
     *              AddCopyAllowed
     *              FieldsDisabled
     *              FieldsEnabled
     *              ModifyingData
     *              NoDataSource
     */
    DEFINE PUBLIC PROPERTY SmartTableIOState AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Enables controls of the SmartViewerControl
     * Notes:   This Method is called by the Toolbar Classes SmartToolbarController
     *          or SmartToolStripContainer by pressing the Tools for Add or Edit Data.
     *          Enforced by Interface ISmartDataTarget or ISmartGroupAssignTarget.
     * @param pcFields The EnableFieldsEnum value indicating if the fields are enabled for Add or Update
     */
    METHOD PUBLIC VOID EnableFields (pcFields AS CHARACTER) .

    /**
     * Purpose: Ensures that this SmartViewerControl is visible
     * Notes:
     */
    METHOD PUBLIC VOID EnsureVisible () .

    /**
     * Purpose: Disable all enabled controls of the SmartViewerControl.
     *          This Method is called from inside the SmartViewerControl.
     * Notes:
     */
    METHOD PUBLIC VOID DisableFields () .

    /**
     * Purpose: Returns the reference to the Control that is bound to the named field
     *          of the binding source
     * Notes:
     * @param pcBindingPropertyName The name of the field in the ProBindingSource
     * @return The reference to the resulting Control
     */
    METHOD PUBLIC System.Windows.Forms.Control GetBoundControl (pcBindingPropertyName AS CHARACTER) .

    /**
     * Purpose: Restore the Current Values of all Controls on a Container implementing
     *          the Interface ISmartGroupAssignTarget.
     * Notes:
     */
    METHOD PUBLIC VOID RestoreValuesForCopy () .

    /**
     * Purpose: Saves the current changes to the database.
     *          This Method is called by the Toolbar Classes SmartToolbarController
     *          or SmartToolStripContainer by pressing the corresponding Tool.
     * Notes:
     */
    METHOD PUBLIC VOID SaveChanges () .

    /**
     * Purpose: Store the Current Values of all Controls on a Container implementing
     *          the Interface ISmartGroupAssignTarget.
     * Notes:
     */
    METHOD PUBLIC VOID StoreValuesForCopy () .

END INTERFACE.