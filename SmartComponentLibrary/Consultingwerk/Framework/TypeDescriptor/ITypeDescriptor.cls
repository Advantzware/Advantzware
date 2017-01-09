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
    File        : ITypeDescriptor
    Purpose     : Interface for Type Descriptor of an ABL class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Apr 29 06:54:01 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.TypeDescriptor.* FROM PROPATH . 
USING Progress.Lang.*                           FROM PROPATH .

INTERFACE Consultingwerk.Framework.TypeDescriptor.ITypeDescriptor:  

    /*------------------------------------------------------------------------------
        Purpose: Returns a description for the type
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Description AS CHARACTER NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the list of property descriptory
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PropertyDescriptors AS ListIPropertyDescriptor NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the name of the type that defines the property
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TypeName AS CHARACTER NO-UNDO 
    GET.
    SET.

END INTERFACE.
