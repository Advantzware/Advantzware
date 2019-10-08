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
    File        : ProgressChangedEventArgs
    Purpose     : Event argument class for the ProgressChanged event of the 
                  FileFileParser class  
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sun Jul 03 09:25:03 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Consultingwerk.EventArgs.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Studio.ProjectMinimizer.ProgressChangedEventArgs 
    INHERITS EventArgs: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Maximum value                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Maximum AS INTEGER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the current value                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Value AS INTEGER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ProgressChangedEventArgs class                                                                        
        Notes:                                                     
        @param piValue The current value
        @param piMaximum The maximum value                   
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ProgressChangedEventArgs (piValue AS INTEGER,
                                                 piMaximum AS INTEGER):
        SUPER ().
    
        ASSIGN THIS-OBJECT:Value   = piValue
               THIS-OBJECT:Maximum = piMaximum . 
        
    END CONSTRUCTOR.

END CLASS.
