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
    File        : IBusinessEntityTableDescriptor
    Purpose     : Describes a Business Entity Table
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jul 10 17:22:30 CEST 2015
    Notes       : http://confluence.consultingwerkcloud.com/wiki/display/SCL/Business+Entity+Descriptor
  ----------------------------------------------------------------------*/

USING Consultingwerk.OERA.BusinessEntityDescriptor.* FROM PROPATH .
USING Progress.Lang.*                                FROM PROPATH .

INTERFACE Consultingwerk.OERA.BusinessEntityDescriptor.IBusinessEntityTableDescriptor:

    /**
     * Purpose: Returns the dictionary of Business Entity Field Descriptors
     * Notes:   Comma delimited list of column (field) names
     */
    DEFINE PUBLIC PROPERTY Fields AS ListBusinessEntityFieldDescriptor NO-UNDO
    GET.

    /**
     * Purpose: Returns the default sort option
     * Notes:   Including the BY and the table name
     */
    DEFINE PUBLIC PROPERTY DefaultSort AS CHARACTER NO-UNDO
    GET.

    /**
     * Purpose: Returns the mandatory columns
     * Notes:   Comma delimited list of column (field) names
     */
    DEFINE PUBLIC PROPERTY MandatoryColumns AS CHARACTER NO-UNDO
    GET.

    /**
     * Purpose: Returns the Temp Table Name
     * Notes:
     */
    DEFINE PUBLIC PROPERTY Name AS CHARACTER NO-UNDO
    GET.

    /**
     * Purpose: Returns the columns that do not support filtering
     * Notes:   Comma delimited list of column (field) names
     */
    DEFINE PUBLIC PROPERTY NonFilterableColumns AS CHARACTER NO-UNDO
    GET.

    /**
     * Purpose: Returns the columns that do not support sorting
     * Notes:   Comma delimited list of column (field) names
       */
    DEFINE PUBLIC PROPERTY NonSortableColumns AS CHARACTER NO-UNDO
    GET.

    /**
     * Purpose: Returns the Primary Key Fields
     * Notes:
     */
    DEFINE PUBLIC PROPERTY PrimaryKey AS CHARACTER NO-UNDO
    GET.

    /**
     * Purpose: Returns if the table is read-only
     * Notes:
     */
    DEFINE PUBLIC PROPERTY ReadOnly AS LOGICAL NO-UNDO
    GET.

    /**
     * Purpose: Returns the read only columns
     * Notes:   Comma delimited list of column (field) names
     */
    DEFINE PUBLIC PROPERTY ReadOnlyColumns AS CHARACTER NO-UNDO
    GET.

END INTERFACE.
