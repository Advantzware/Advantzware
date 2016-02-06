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
    File        : DataSourceUpdateStateEventArgs
    Purpose     : Event argument for the BeginUpdateStateEvent 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Jan 28 21:38:41 CET 2010
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.SmartComponents.Interfaces.* FROM PROPATH .
USING Progress.Lang.*                             FROM PROPATH .
USING Progress.Lang.*                             FROM PROPATH .
USING System.EventArgs                            FROM ASSEMBLY . 

CLASS Consultingwerk.SmartComponents.Implementation.DataSourceUpdateStateEventArgs 
    INHERITS System.EventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the updating data target                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY UpdatingDataTarget AS ISmartDataTarget NO-UNDO 
    GET.
    PROTECTED SET.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the DataSourceUpdateStateEventArgs class                                                                        
        Notes:                                                             
        @param poUpdatingDataTarget The reference to the updating data target           
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC DataSourceUpdateStateEventArgs (poUpdatingDataTarget AS ISmartDataTarget):
        SUPER ().
        
        THIS-OBJECT:UpdatingDataTarget = poUpdatingDataTarget .
        
    END CONSTRUCTOR.

END CLASS.
