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
    File        : BeforeGetChangesEventArgs
    Purpose     : Event argument for the BeforeGetChanges event
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jul 30 08:14:17 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.EventArgs                        FROM PROPATH .
USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .  
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Implementation.BeforeGetChangesEventArgs 
    INHERITS EventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the handle of the ProDataset that contains the data on the client
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DatasetHandle AS HANDLE NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the BeforeGetChangesEventArgs class                                                                       
        Notes:                                                                        
        @param phDataset The handle of the ProDataset that contains the data on the client
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BeforeGetChangesEventArgs (phDataset AS HANDLE):
        SUPER ().
        
        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phDataset Consultingwerk.WidgetTypeEnum:Dataset} .
        
        THIS-OBJECT:DatasetHandle = phDataset . 
        
    END CONSTRUCTOR.

END CLASS.
