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
    File        : UnsupportedWidgetAttributeValueException
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Sep 05 09:51:09 CEST 2015
    Notes       : Non SERIALIZABLE, as the reference to the original .NET
                  Control is not serializable 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Exceptions.*                                    FROM PROPATH .
USING Consultingwerk.Windows.LegacyGuiMigration.Widgets.Exceptions.* FROM PROPATH .  
USING Progress.Lang.*                                                FROM PROPATH .
USING System.Windows.Forms.*                                         FROM ASSEMBLY . 

CLASS Consultingwerk.Windows.LegacyGuiMigration.Widgets.Exceptions.UnsupportedWidgetAttributeValueException 
    INHERITS Exception: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the .NET Control associated with this exception
        Notes:                          
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Attribute AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the .NET Control associated with this exception
        Notes:                          
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Control AS Control NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the UnsupportedWidgetAttributeValueException class
        Notes:                          
        @param pcErrorString The error message associated with this Exception object
        @param piMessageNumber The error message number associated with this Exception object
        @param poControl The reference to the Control that raised the exception
        @param pcAttribute The name of the Widget Attribute associated with this error
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC UnsupportedWidgetAttributeValueException (pcErrorString AS CHARACTER,
                                                                 piMessageNumber AS INTEGER,
                                                                 poControl AS Control,
                                                                 pcAttribute AS CHARACTER):        
    
        SUPER (pcErrorString, piMessageNumber).
    
        ASSIGN THIS-OBJECT:Control   = poControl
               THIS-OBJECT:Attribute = pcAttribute .            
    
    END CONSTRUCTOR.

END CLASS.
