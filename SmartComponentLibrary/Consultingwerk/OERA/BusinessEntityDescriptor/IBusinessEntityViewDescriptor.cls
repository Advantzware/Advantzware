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
    File        : IBusinessEntityViewDescriptor
    Purpose     : Describes a Business Entity View
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jul 10 17:03:49 CEST 2015
    Notes       : http://confluence.consultingwerkcloud.com/wiki/display/SCL/Business+Entity+Descriptor
  ----------------------------------------------------------------------*/

USING Consultingwerk.OERA.BusinessEntityDescriptor.* FROM PROPATH .
USING Progress.Lang.*                                FROM PROPATH .

INTERFACE Consultingwerk.OERA.BusinessEntityDescriptor.IBusinessEntityViewDescriptor:

    /**
     * Purpose: Returns the EntityTable of the View
     * Notes:
     */
    DEFINE PUBLIC PROPERTY EntityTable AS CHARACTER NO-UNDO
    GET.

    /**
     * Purpose: Returns the EntityView of the View
     * Notes:   The EntityView as child tables of the EntityTable that are joined
     *          with the EntityTable, comma delimited list
     */
    DEFINE PUBLIC PROPERTY EntityView AS CHARACTER NO-UNDO
    GET.

    /**
     * Purpose: Returns the list of fields that should be used in a list view (browser, grid)
     * Notes:   Comma delimited list of field names either in the form of just field
     *          names (to use the first field with the matching name) or temp-table.field
     */
    DEFINE PUBLIC PROPERTY ListColumns AS CHARACTER NO-UNDO
    GET.

    /**
     * Purpose: Returns the Name of the View
     * Notes:
     */
    DEFINE PUBLIC PROPERTY Name AS CHARACTER NO-UNDO
    GET.

    /**
     * Purpose: Returns the Query String of the View
     * Notes:   May be empty to return all records from the Business Entity
     */
    DEFINE PUBLIC PROPERTY QueryString AS CHARACTER NO-UNDO
    GET.

    /**
     * Purpose: Returns the list of fields that should be used in a viewer view (details)
     * Notes:   Comma delimited list of field names either in the form of just field
     *          names (to use the first field with the matching name) or temp-table.field
     */
    DEFINE PUBLIC PROPERTY ViewerColumns AS CHARACTER NO-UNDO
    GET.

END INTERFACE.
