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
    File        : SmartBrowserListChangedEventArgs
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Mon Nov 05 21:51:30 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.EventArgs.

USING Consultingwerk.SmartComponents.Base.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.SmartComponents.Base.SmartBrowserListChangedEventArgs INHERITS EventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the number of records from the BindingSource
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Count AS INTEGER NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns True if more data is available 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY MoreRecordsAvailable AS LOGICAL NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the SmartBrowserListChangedEventArgs class
        Notes:   
        @param piCount The number of records available
        @param plMoreRecords Logicla value indicating if batching is active
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartBrowserListChangedEventArgs (piCount AS INTEGER,
                                                         plMoreRecords AS LOGICAL):
        SUPER ().
        
        ASSIGN THIS-OBJECT:Count                = piCount
               THIS-OBJECT:MoreRecordsAvailable = plMoreRecords .
        
    END CONSTRUCTOR.

END CLASS.
