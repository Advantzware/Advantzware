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
    File        : ISmartBusinessEntityLookup
    Purpose     : Describes properties of the SmartBusinessEntityLookup
                  required for design time (SmartLookupDesignerSupport)
    Syntax      :
    Description : Interface intended to allow reuse of the SmartLookupDesignerSupport
                  code with alternativ implementations of the SmartBusinessEntityLookup
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Apr 01 12:22:22 CEST 2011
    Notes       :
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

INTERFACE Consultingwerk.SmartComponents.Interfaces.ISmartBusinessEntityLookupDesign:

    /**
     * Purpose: Columns (comma delimited list of Column Names) to be shown in the
     *          LookupBrowser.
     * Notes:   The value of this Property is defined during DesignTime.
     */
    DEFINE PUBLIC PROPERTY LookupBrowserFields AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: A comma seperated list of Control names. The value of the corresponding
     *          Columns gets filled into those Controls.
     *          The value of this Property is defined during DesignTime.
     * Notes:   Corresponding to the LookupFields
     */
    DEFINE PUBLIC PROPERTY LookupControls AS CHARACTER NO-UNDO
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
     * Purpose: QueryString used when opening the Lookup Browser Dialog
     * Notes:
     */
    DEFINE PUBLIC PROPERTY LookupDialogQueryString AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Dialog title for the LookupDialogForm.
     *          The value of this Property is defined during DesignTime.
     * Notes:
     */
    DEFINE PUBLIC PROPERTY LookupDialogTitle AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Name of the BusinessEntity for the Lookup.
     *          This Property is set by the Developer during DesignTime.
     * Notes:
     */
    DEFINE PUBLIC PROPERTY LookupEntityName AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: RootTable for this Instance of the BusinessEntity for the Lookup.
     *          This Property is set by the Developer during DesignTime.
     * Notes:
     */
    DEFINE PUBLIC PROPERTY LookupEntityTable AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: List of Tables to read with the EntityTable.
     *          This Property is set by the Developer during DesignTime.
     * Notes:
     */
    DEFINE PUBLIC PROPERTY LookupEntityView AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: A comma seperated list of Column names. The value of those fields
     *          gets filled into the corresponding Controls of the Property
     *          LookupControls.
     *          The value of this Property is defined during DesignTime.
     * Notes:   Corresponding to the LookupControls
     */
    DEFINE PUBLIC PROPERTY LookupFields AS CHARACTER NO-UNDO
    GET.
    SET.


    /**
     * Purpose: The value of the field which is returned by the SmartLookup.
     *          The value of this Property is defined during DesignTime.
     * Notes:
     */
    DEFINE PUBLIC PROPERTY LookupKeyField AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Gets/Sets the name of the Data Source column which is used for
     *          retrieving the LookupKeyValue property
     * Notes:   Data-Bindable Property
     */
    DEFINE PUBLIC PROPERTY LookupKeyValueColumn AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: QueryString used in PerformLookup method
     * Notes:
     */
    DEFINE PUBLIC PROPERTY LookupQueryString AS CHARACTER NO-UNDO
    GET.
    SET.

    /**
     * Purpose: Returns a reference to the Container component/control
     * Notes:   We were having issues wirth adding the .NET property Container
     *          to the Interface ISmartBusinessEntityLookup, hence the methode
     * @return The reference to the IContainer instance of the Lookup
     */
    METHOD PUBLIC System.ComponentModel.IContainer GetContainer ().

END INTERFACE.
