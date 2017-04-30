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
    File        : BindingSourceChangedEventArgs
    Purpose     : Event Argument class for the BindingSourceChanged event
                  of the SmartDataAdapter class
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Dec 27 11:54:55 CET 2009
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Progress.Data.*  FROM ASSEMBLY . 
USING Progress.Lang.*  FROM PROPATH .
USING System.EventArgs FROM ASSEMBLY .

CLASS Consultingwerk.SmartComponents.Base.BindingSourceChangedEventArgs 
    INHERITS System.EventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the New ProBindingSource
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY New AS Progress.Data.BindingSource NO-UNDO 
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the Old ProBindingSource
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Old AS Progress.Data.BindingSource NO-UNDO 
    GET.
    PRIVATE SET.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the BindingSourceChangedEventArgs                                                              
        Notes:                                                      
        @param poOld The reference to the Progress.Data.BindingSource (ProBindingSource)                  
        @param poNew The reference to the Progress.Data.BindingSource (ProBindingSource)
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC BindingSourceChangedEventArgs (poOld AS Progress.Data.BindingSource,
                                                      poNew AS Progress.Data.BindingSource):
        SUPER ().
        
        ASSIGN THIS-OBJECT:Old = poOld
               THIS-OBJECT:New = poNew .
        
    END CONSTRUCTOR.

END CLASS.