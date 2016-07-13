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
    File        : SmartTableIOStateChangedEventArgs 
    Purpose     : Event Argument for the SmartTableIOStateChanged event 
                  of the ISmartTableIOTarget classes
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Dec 22 20:50:51 CET 2009
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Enum.* FROM PROPATH .
USING Progress.Lang.*                       FROM PROPATH .
USING System.EventArgs                      FROM ASSEMBLY . 

CLASS Consultingwerk.SmartComponents.Base.SmartTableIOStateChangedEventArgs 
    INHERITS System.EventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the SmartTableIOState that raised this event                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/        
    DEFINE PUBLIC PROPERTY SmartTableIOState AS CHARACTER NO-UNDO 
    GET.
    PROTECTED SET.    

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartTableIOStateChangedEventArgs class                                                                       
        Notes:    
        @param pcState The value for the SmartTableIOState property                                                                    
    ------------------------------------------------------------------------------*/        
    CONSTRUCTOR PUBLIC SmartTableIOStateChangedEventArgs (pcState AS CHARACTER):
        SUPER ().
        
        IF NOT TableIOStateEnum:IsValid (pcState) THEN 
            UNDO, THROW NEW AppError (SUBSTITUTE("Invalid SmartTableIOState value of &1.", pcState), 0) . 
    
        THIS-OBJECT:SmartTableIOState = pcState .  
        
    END CONSTRUCTOR.

END CLASS.
