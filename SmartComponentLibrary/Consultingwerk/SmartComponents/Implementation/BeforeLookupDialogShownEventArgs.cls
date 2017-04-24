/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : BeforeLookupDialogShownEventArgs
    Purpose     : Event argument for the BeforeLookupDialogShown event 
                  of the SmartBusinessEntityLookup control 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Tue Jun 03 14:13:00 CEST 2014
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.*                                FROM PROPATH .
USING Consultingwerk.SmartComponents.Interfaces.*     FROM PROPATH .  
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .  
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Implementation.BeforeLookupDialogShownEventArgs 
    INHERITS EventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the Lookup Dialog instance 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY SmartBusinessEntityLookupDialog AS ISmartBusinessEntityLookupDialog NO-UNDO 
    GET.
    PUBLIC SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BeforeLookupDialogShownEventArgs class
        Notes:   
        @param poDialog The reference to the Lookup Dialog
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BeforeLookupDialogShownEventArgs (poDialog AS ISmartBusinessEntityLookupDialog):
        SUPER ().
        
        THIS-OBJECT:SmartBusinessEntityLookupDialog = poDialog . 
        
    END CONSTRUCTOR.

END CLASS.
