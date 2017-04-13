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
    File        : ISmartBusinessEntityLookupDialog
    Purpose     :
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Apr 01 15:54:37 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Implementation.* .
USING Progress.Lang.* .

INTERFACE Consultingwerk.SmartComponents.Interfaces.ISmartBusinessEntityLookupDialog:

    /**
     * Purpose: Event raised when the lookups SmartBusinessEntityAdapter is initialized
     * Notes:
     * @param sender The reference to the object that raised the event
     * @param e The AfterInitializeBusinessEntityAdapterEventArgs instance with the data for this event
     */
    DEFINE PUBLIC EVENT AfterInitializeBusinessEntityAdapter SIGNATURE VOID (sender AS System.Object,
                                                                             e AS AfterInitializeBusinessEntityAdapterEventArgs).

    /**
     * Purpose: Name of the Type of SmartBusinessEntityAdapter used to lookup
     *          values from the backend.
     * Notes:
     */
    DEFINE PUBLIC PROPERTY AdapterType AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets/Sets the name of the AppServer Partition to be used
     *          by this SmartDataAdapter instance
     * Notes:
     */
    DEFINE PUBLIC PROPERTY AppServerPartition AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Returns the reference to the LookupBrowser instance
     * Notes:
     */
    DEFINE PUBLIC PROPERTY LookupBrowser AS Infragistics.Win.UltraWinGrid.UltraGrid NO-UNDO
    GET.

    /**
     * Purpose: Fields not to be shown in the SmartDataBrowser
     * Notes:
     */
    DEFINE PUBLIC PROPERTY LookupBrowserExcludeFields AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Fields to be shown in the SmartDataBrowser
     * Notes:
     */
    DEFINE PUBLIC PROPERTY LookupBrowserFields AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: QuerySort used when opening the Lookup Browser Dialog
     * Notes:
     */
    DEFINE PUBLIC PROPERTY LookupDialogQuerySort AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: QueryString used when opening the Lookup Browser Dialog
     * Notes:
     */
    DEFINE PUBLIC PROPERTY LookupDialogQueryString AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Fields that allow to Filter on the Lookup Browser Dialog
     * Notes:   Comma-Delimited, first entry is default
     */
    DEFINE PUBLIC PROPERTY LookupDialogFilterFields AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Operators for the LookupDialogFilterFields
     * Notes:   Comma-Delimited, first entry is default
     */
    DEFINE PUBLIC PROPERTY LookupDialogFilterOperators AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the name of the Business Entity that returns the data
     *          for the Lookup Dialog
     * Notes:
     */
    DEFINE PUBLIC PROPERTY LookupEntityName AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the name of the Entity Table that is retrieved from
     *          the Lookup Dialog Business Entity
     * Notes:
     */
    DEFINE PUBLIC PROPERTY LookupEntityTable AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets and sets the name of the Entity View Tables that is retrieved
     *          from the Lookup Dialog Business Entity
     * Notes:
     */
    DEFINE PUBLIC PROPERTY LookupEntityView AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Field that is returned by the SmartLookup
     * Notes:
     */
    DEFINE PUBLIC PROPERTY LookupKeyField AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Sets and sets a reference to the SmartBusinessEntityAdapter instance
     *          used by the SmartBusinessEntityAdapter
     * Notes:
     */
    DEFINE PUBLIC PROPERTY LookupSmartBusinessEntityAdapter AS SmartBusinessEntityAdapter NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Returns a Field Value from the Business Entity Adapter
     * Notes:
     * @param pcFieldName The name of the Field to return
     * @return The value of the field
     */
    METHOD PUBLIC CHARACTER GetLookupFieldValue (pcFieldName AS CHARACTER) .

    /**
     * Purpose: Initializes the DataAdapter used by the SmartBusinessEntityLookupDialog
     * Notes:
     */
    METHOD PUBLIC VOID InitializeDataAdapter () .

    /**
     * Purpose: Sets the Title of the Lookup Dialog
     * Notes:
     * @param pcText The Text to be used as the Forms Title
     */
    METHOD PUBLIC VOID SetText (pcText AS CHARACTER) .

END INTERFACE.