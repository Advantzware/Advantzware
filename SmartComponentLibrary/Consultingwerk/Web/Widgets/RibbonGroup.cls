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
    File        : RibbonGroup
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Apr 05 20:48:46 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Web.Widgets.* FROM PROPATH .
USING Progress.Lang.*              FROM PROPATH .

CLASS Consultingwerk.Web.Widgets.RibbonGroup 
    INHERITS Component: 
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Caption of the RibbonGroup                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Caption AS CHARACTER NO-UNDO 
    GET.
    SET.     
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Key of the RibbonGroup                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Key AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets the list of Tools                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Tools AS ListTool NO-UNDO 
    GET.
    PRIVATE SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the RibbonGroup class                                                                        
        Notes:       
        @param pcKey The key for the RibbonGroup                                                                             
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RibbonGroup (pcKey AS CHARACTER):
        SUPER ().
        
        ASSIGN THIS-OBJECT:Tools = NEW ListTool () 
               THIS-OBJECT:Key   = pcKey .  
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the RibbonGroup class                                                                     
        Notes:                                                                        
        @param pcKey The key for the RibbonGroup
        @param pcCaption The caption for the RibbonGroup                                                                             
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RibbonGroup (pcKey AS CHARACTER, 
                                    pcCaption AS CHARACTER):
        THIS-OBJECT (pcKey).

        THIS-OBJECT:Caption = pcCaption . 

    END CONSTRUCTOR. 

END CLASS.
