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
    File        : SmartNavigationTargetStateChangedEventArgs
    Purpose     : Event Argument for the UpdateSmartNavigationTargetState event 
                  method of the ISmartNavigationSource classes
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Aug 21 23:33:54 CEST 2009
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Enum.* FROM PROPATH .
USING Progress.Lang.*                       FROM PROPATH .
USING System.EventArgs                      FROM ASSEMBLY . 

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.SmartComponents.Base.SmartNavigationTargetStateChangedEventArgs 
    INHERITS System.EventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the SmartNavigationTargetState that raised this event                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/        
    DEFINE PUBLIC PROPERTY SmartNavigationTargetState AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET.    

    /*------------------------------------------------------------------------------
        Purpose: Constrcutor for the SmartNavigationTargetStateChangedEventArgs class                                                                        
        Notes:            
        @param pcState The value for the SmartNavigationTargetState property                                                            
    ------------------------------------------------------------------------------*/        
    CONSTRUCTOR PUBLIC SmartNavigationTargetStateChangedEventArgs (pcState AS CHARACTER):
        SUPER ().
        
        IF NOT DataSourceStateEnum:IsValid (pcState) THEN 
            UNDO, THROW NEW AppError (SUBSTITUTE("Invalid DataSourceStateEnum value &1.", pcState), 0) . 
    
        THIS-OBJECT:SmartNavigationTargetState = pcState .    
        
    END CONSTRUCTOR.

END CLASS.