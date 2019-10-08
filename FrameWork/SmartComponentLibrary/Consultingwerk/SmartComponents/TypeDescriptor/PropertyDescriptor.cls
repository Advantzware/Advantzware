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
    File        : PropertyDescriptor
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Feb 22 15:03:46 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.TypeDescriptor.IPropertyDescriptor.


USING Consultingwerk.SmartComponents.TypeDescriptor.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartComponents.TypeDescriptor.PropertyDescriptor 
    IMPLEMENTS IPropertyDescriptor: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the List of Attributes for the Property
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Attributes AS "System.Collections.Generic.List<System.Attribute>":U NO-UNDO 
    GET.
    PRIVATE SET . 

    /*------------------------------------------------------------------------------
        Purpose: Returns the DefaultValue for the Property
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY PropertyName AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the PropertyDescriptor class
        Notes:   
        @param pcPropertyName The Name of the Property in the PropertyDescriptor instance
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC PropertyDescriptor (pcPropertyName AS CHARACTER):
        SUPER ().
        
        ASSIGN THIS-OBJECT:PropertyName = pcPropertyName
               THIS-OBJECT:Attributes   = NEW "System.Collections.Generic.List<System.Attribute>" /*:U*/ () .
        
    END CONSTRUCTOR.

END CLASS.
