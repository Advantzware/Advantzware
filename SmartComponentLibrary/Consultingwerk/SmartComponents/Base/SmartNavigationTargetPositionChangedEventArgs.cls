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
    File        : SmartNavigationTargetPositionChangedEventArgs
    Purpose     : Event argument for the SmartNavigationTargetPositionChanged event
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Aug 21 23:25:51 CEST 2009
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.SmartComponents.Enum.* FROM PROPATH .
USING Progress.Lang.*                       FROM PROPATH .
USING System.EventArgs                      FROM ASSEMBLY . 

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.SmartComponents.Base.SmartNavigationTargetPositionChangedEventArgs 
    INHERITS System.EventArgs FINAL: 

     /*------------------------------------------------------------------------------
        Purpose: The CHARACTER value representing on RecordPositionEnum value                                                                        
        Notes:   
    ------------------------------------------------------------------------------*/        
    DEFINE PUBLIC PROPERTY SmartNavigationTargetPosition AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET.    

     /*------------------------------------------------------------------------------
        Purpose: Instantiates a new instance of a SmartNavigationTargetPositionChangedEventArgs                                                                        
        Notes:   Requires pcPosition                           
        @param pcPosition The CHARACTER value representing on RecordPositionEnum value                                              
    ------------------------------------------------------------------------------*/        
    CONSTRUCTOR PUBLIC SmartNavigationTargetPositionChangedEventArgs (pcPosition AS CHARACTER):
        SUPER ().
        
        IF NOT RecordPositionEnum:IsValid (pcPosition) THEN 
            UNDO, THROW NEW AppError (SUBSTITUTE("Invalid RecordPositionEnum value &1.", pcPosition), 0) . 
      
        THIS-OBJECT:SmartNavigationTargetPosition = pcPosition .
        
    END CONSTRUCTOR.

END CLASS.
