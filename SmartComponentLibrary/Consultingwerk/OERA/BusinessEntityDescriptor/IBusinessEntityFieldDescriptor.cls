/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    Purpose     : Describes a Business Entity Field
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Jul 10 17:22:30 CEST 2015
    Notes       : http://confluence.consultingwerkcloud.com/wiki/display/SCL/Business+Entity+Descriptor
  ----------------------------------------------------------------------*/

USING Consultingwerk.OERA.BusinessEntityDescriptor.* FROM PROPATH .
USING Progress.Lang.*                                FROM PROPATH .

INTERFACE Consultingwerk.OERA.BusinessEntityDescriptor.IBusinessEntityFieldDescriptor:

    /*------------------------------------------------------------------------------
        Purpose: Returns the Temp Table Name
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Name AS CHARACTER NO-UNDO
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns if the table is read-only
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SemanticType AS CHARACTER NO-UNDO
    GET.

END INTERFACE.
