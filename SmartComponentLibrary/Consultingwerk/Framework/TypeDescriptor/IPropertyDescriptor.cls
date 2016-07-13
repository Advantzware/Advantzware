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
    File        : IPropertyDescriptor
    Purpose     : Interface for the descriptor of a single object property
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Apr 29 06:58:50 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.TypeDescriptor.* FROM PROPATH .
USING Progress.Lang.*                           FROM PROPATH .

INTERFACE Consultingwerk.Framework.TypeDescriptor.IPropertyDescriptor:  
  
    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the type that defines the property
        Notes:   Returns the name of a base class when a property is inherited
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DefiningTypeName AS CHARACTER NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns if the property should be editable in a property sheed
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Editable AS LOGICAL NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the type of editor control to use for the property visualization
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY EditorType AS EditorTypeEnum NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns a comma delimited list of name/value pairs for the property
        Notes:   Applieas to EditorType = EditorTypeEnum:List
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ListItemPairs AS CHARACTER NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the label for the property 
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PropertyLabel AS CHARACTER NO-UNDO 
    GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the name of the property 
        Notes:
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY PropertyName AS CHARACTER NO-UNDO 
	GET.

    /*------------------------------------------------------------------------------
        Purpose: Returns if the property should be visible in a property sheed
        Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Visible AS LOGICAL NO-UNDO 
    GET.

END INTERFACE.
