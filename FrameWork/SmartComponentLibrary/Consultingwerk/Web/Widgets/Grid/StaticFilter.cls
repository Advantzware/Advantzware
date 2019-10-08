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
    File        : StaticFilter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Apr 26 21:35:07 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Consultingwerk.Web.Widgets.Grid.GridFilter.

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Web.Widgets.Grid.StaticFilter 
    INHERITS GridFilter: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Key of the Filter entry                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Key AS CHARACTER NO-UNDO 
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Label of the Filter entry                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Label AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Value of the Filter entry                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Value AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the StaticFilter class                                                                       
        Notes:                                          
        @param pcKey The Keyvalue of the Filter entry
        @param pcValue The Value of the    Filter entry
        @param pcLabel The Label of the Filter entry                          
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC StaticFilter (pcKey AS CHARACTER,
                                     pcValue AS CHARACTER,
                                     pcLabel AS CHARACTER):
        SUPER ().
        
        ASSIGN THIS-OBJECT:Key   = pcKey
               THIS-OBJECT:Value = pcValue
               THIS-OBJECT:Label = pcLabel . 
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the StaticFilter class                                                                    
        Notes:                                          
        @param pcKey The Keyvalue of the Filter entry
        @param pcValue The Value of the Filter entry
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC StaticFilter (pcKey AS CHARACTER,
                                     pcValue AS CHARACTER):
        
        THIS-OBJECT (pcKey, pcValue, pcValue) .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Performs HTML output of the Filter entry
        Notes:                                          
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID PerformOutput ():
        
        THIS-OBJECT:OUTPUT (SUBSTITUTE ('<option name="&1" value="&2">&3</option>',
                                        THIS-OBJECT:Key,
                                        THIS-OBJECT:Value,
                                        THIS-OBJECT:Label),
                            TRUE) .
    END METHOD.

END CLASS.
