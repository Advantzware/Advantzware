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
    File        : IInvokableMethodDescriptor
    Purpose     : Describes an Invokable Method
    Syntax      :
    Description :
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Jul 08 15:46:37 CEST 2015
    Notes       : http://confluence.consultingwerkcloud.com/wiki/display/SCL/Business+Entity+Descriptor
  ----------------------------------------------------------------------*/

USING Consultingwerk.OERA.BusinessEntityDescriptor.* FROM PROPATH .
USING Progress.Lang.* FROM PROPATH.

INTERFACE Consultingwerk.OERA.BusinessEntityDescriptor.IInvokableMethodDescriptor:

    /**
     * Purpose: Returns if the ProDataset parameter is expected in INPUT direction
     * Notes:
     */
    DEFINE PUBLIC PROPERTY DatasetInput AS LOGICAL NO-UNDO
    GET.

    /**
     * Purpose: Returns if the ProDataset parameter is expected in OUTPUT direction
     * Notes:
     */
    DEFINE PUBLIC PROPERTY DatasetOutput AS LOGICAL NO-UNDO
    GET.

    /**
     * Purpose: Returns the method name
     * Notes:
     */
    DEFINE PUBLIC PROPERTY Name AS CHARACTER NO-UNDO
    GET.

    /**
     * Purpose: Returns the class name of the parameter class
     * Notes:   The JSDO Generic Service uses this to deserialize the JSON parameter
     *          string without the need to rely on the SerializedTypeInfo in the JSON
     *          String
     */
    DEFINE PUBLIC PROPERTY ParameterClassName AS CHARACTER NO-UNDO
    GET.

END INTERFACE.
