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
    File        : ISmartTableIOTarget
    Purpose     : Interface declaring the members of a TableIO target
                  (viewer / browser) component
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jan 19 09:12:42 CET 2009
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Base.* FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.* FROM PROPATH .
USING Progress.Lang.* FROM ASSEMBLY .

INTERFACE Consultingwerk.SmartComponents.Interfaces.ISmartTableIOTarget:

    /**
     * Purpose: Event fired whenever the SmartTableIOState property has changed
     * Notes:
     * @param sender The object that raised the event
     * @param e The SmartTableIOStateChangedEventArgs with the data for this event
     */
    DEFINE PUBLIC EVENT SmartTableIOStateChanged SIGNATURE VOID (sender AS System.Object,
                                                                 e AS SmartTableIOStateChangedEventArgs).

    /**
     *  Purpose: Gets and sets the Toolbar Security Key
     *  Notes:
     */
    DEFINE PUBLIC PROPERTY SecurityKey AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Get and Set the current SmartTableIOSource of the SmartViewerControl.
     *          This Property is set by the developer when initializing a Control
     *          implementing the Interface ISmartDataTarget.
     * Notes:   Cannot be changed while the SmartViewerControl is in ModifyingData mode.
     */
    DEFINE PUBLIC PROPERTY SmartTableIOSource AS ISmartTableIOSource NO-UNDO
    GET.
    SET.

    /**
     * Purpose: The SmartTableIOState property reflects the operationmode of the
     *          SmartViewerControl.
     *          Value changes of this Property are only made in the Class
     *          SmartViewerControl.
     * Notes:   Possible values (TableIOStateEnum):
     *          - AddCopyAllowed
     *          - FieldsDisabled
     *          - FieldsEnabled
     *          - ModifyingData
     *          - NoDataSource
     */
    DEFINE PUBLIC PROPERTY SmartTableIOState AS CHARACTER NO-UNDO
    GET.

    /**
     * Purpose: Enables controls of the SmartTableIOTarget
     *          This Method is called by the Toolbar Classes SmartToolbarController
     *          or SmartToolStripContainer by pressing the Tools for Add or Edit Data.
     * Notes:
     * @param pcFields The EnableFieldsEnum value indicating if the fields are enabled for Add or Update
     */
    METHOD PUBLIC VOID EnableFields (pcFields AS CHARACTER) .

    /**
     * Purpose: Disable all enabled controls of the SmartViewerControl.
     *          This Method is called from inside the SmartViewerControl.
     * Notes:
     */
    METHOD PUBLIC VOID DisableFields () .

    /**
     * Purpose: Handles an error raised by validation routines
     * Notes:   Typically displays the Error message and focusses the field that
     *          caused the error
     * @param poError The reference to the validation error instance
     */
    METHOD PUBLIC VOID HandleValidationError (poError AS Progress.Lang.Error).

    /**
     * Purpose: Saves the current changes to the database.
     *          This Method is called by the Toolbar Classes SmartToolbarController
     *          or SmartToolStripContainer by pressing the corresponding Tool.
     * Notes:
     */
    METHOD PUBLIC VOID SaveChanges () .

    /**
     * Purpose: Add a new record to the SmartDataSource.
     *          This Method is called by the Toolbar Classes SmartToolbarController
     *          or SmartToolStripContainer by pressing the AddRecord Tool.
     * Notes:
     */
    METHOD PUBLIC VOID AddRecord () .

    /**
     * Purpose: Copy the current record to a new one.
     *          This Method is called by the Toolbar Classes SmartToolbarController
     *          or SmartToolStripContainer by pressing the corresponding Tool.
     * Notes:
     */
    METHOD PUBLIC VOID CopyRecord () .

    /**
     * Purpose: Reverts the changes to the current record
     *          This Method is called by the Toolbar Classes SmartToolbarController
     *          or SmartToolStripContainer by pressing the corresponding Tool.
     * Notes:
     */
    METHOD PUBLIC VOID CancelUpdate () .

    /**
     * Purpose: Delete the current record. The Method DeleteRow of the
     *          SmartDataSource is called to Handle this Task.
     *          This Method is called by the Toolbar Classes SmartToolbarController
     *          or SmartToolStripContainer by pressing the corresponding Tool.
     * Notes:
     */
    METHOD PUBLIC VOID DeleteRecord () .

END INTERFACE.
