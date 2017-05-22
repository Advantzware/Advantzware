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
    File        : AfterGetChangesEventArgs
    Purpose     : Event handler for the AfterGetChanges event
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Jul 30 08:17:59 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Implementation.* FROM PROPATH .  
USING Progress.Lang.*                                 FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Implementation.AfterGetChangesEventArgs 
    INHERITS BeforeGetChangesEventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the handle of the ProDataset that contains the changes
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ChangesDatasetHandle AS HANDLE NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the AfterGetChangesEventArgs class                                                                       
        Notes:                                                                        
        @param phDataset The handle of the ProDataset that contains the data on the client
        @param phChangesDataset The handle of the ProDataset that contains the changes
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC AfterGetChangesEventArgs (phDataset AS HANDLE,
                                                 phChangesDataset AS HANDLE):
        SUPER (INPUT phDataset).
        
        {Consultingwerk/Assertion/HandleAssert/WidgetType.i phChangesDataset Consultingwerk.WidgetTypeEnum:Dataset} .
        
        THIS-OBJECT:ChangesDatasetHandle = phChangesDataset . 
        
    END CONSTRUCTOR.

END CLASS.
