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
    File        : IBusinessEntityDescriptor
    Purpose     : Describes properties of a Business Entity
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consulingwerk Ltd.
    Created     : Fri Jul 10 16:49:46 CEST 2015
    Notes       : http://confluence.consultingwerkcloud.com/wiki/display/SCL/Business+Entity+Descriptor
  ----------------------------------------------------------------------*/

USING Consultingwerk.OERA.BusinessEntityDescriptor.* FROM PROPATH .
USING Progress.Lang.*                                FROM PROPATH .

INTERFACE Consultingwerk.OERA.BusinessEntityDescriptor.IBusinessEntityDescriptor:

    /**
     * Purpose: Returns the Name of the Business Entity
     * Notes:
     */
    DEFINE PUBLIC PROPERTY BusinessEntityName AS CHARACTER NO-UNDO
    GET.

    /**
     * Purpose: Returns the descriptor of the default View of the Business Entity
     * Notes:
     */
    DEFINE PUBLIC PROPERTY DefaultView AS IBusinessEntityViewDescriptor NO-UNDO
    GET.

    /**
     * Purpose: Returns the Name of the Default View of the Business Entity
     * Notes:
     */
    DEFINE PUBLIC PROPERTY DefaultViewName AS CHARACTER NO-UNDO
    GET.

    /**
     * Purpose: Returns the List of invokable method descriptors
     * Notes:
     */
    DEFINE PUBLIC PROPERTY InvokableMethods AS ListInvokableMethodDescriptor NO-UNDO
    GET.

    /**
     * Purpose: Returns the List of temp table descriptors
     * Notes:
     */
    DEFINE PUBLIC PROPERTY Tables AS ListBusinessEntityTableDescriptor NO-UNDO
    GET.

    /**
     * Purpose: Returns the List of view descriptors
     * Notes:
     */
    DEFINE PUBLIC PROPERTY Views AS ListBusinessEntityViewDescriptor NO-UNDO
    GET.

END INTERFACE.
