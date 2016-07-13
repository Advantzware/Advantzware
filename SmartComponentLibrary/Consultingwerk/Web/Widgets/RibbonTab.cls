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
    File        : RibbonTab
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Apr 05 20:48:46 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Web.Widgets.* FROM PROPATH .
USING Progress.Lang.*              FROM PROPATH .

ROUTINE-LEVEL ON ERROR UNDO, THROW.

CLASS Consultingwerk.Web.Widgets.RibbonTab 
    INHERITS Component: 
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Caption of the RibbonTab                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Caption AS CHARACTER NO-UNDO 
    GET.
    SET.     
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Key of the RibbonTab                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Key AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets the list of RibbonGroups                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Groups AS ListRibbonGroup NO-UNDO 
    GET.
    PRIVATE SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the RibbonTab class                                                                     
        Notes:                                                                        
        @param pcKey The key for the RibbonTab
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RibbonTab (pcKey AS CHARACTER):
        SUPER ().
        
        ASSIGN THIS-OBJECT:Groups = NEW ListRibbonGroup () 
               THIS-OBJECT:Key    = pcKey .  
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the RibbonTab class                                                                     
        Notes:    
        @param pcKey The key for the RibbonTab
        @param pcCaption The caption for the RibbonTab                                                                             
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC RibbonTab (pcKey AS CHARACTER, 
                                  pcCaption AS CHARACTER):
        THIS-OBJECT (pcKey).

        THIS-OBJECT:Caption = pcCaption . 

    END CONSTRUCTOR. 

END CLASS.