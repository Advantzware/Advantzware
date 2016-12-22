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
    File        : ButtonTool
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Apr 05 21:03:04 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Web.Widgets.* FROM PROPATH .
USING Progress.Lang.*              FROM PROPATH .

CLASS Consultingwerk.Web.Widgets.ButtonTool 
    INHERITS ToolBase: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Image Name for this Button                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Image AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Size for this Button when displayed on a Ribbon Tab                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Size AS RibbonToolSize NO-UNDO 
    GET.
    SET. 


    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the (Link-) Target used by ButtonTool's defined with 
                 an URL
        Notes:   Only used in combination with the URL property
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Target AS CHARACTER NO-UNDO INIT "_top":U
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the URL this ButtonTool is linking to rather than 
                 submitting the Buttons key with the Form
        Notes:   Also uses the (Link-) Target Property
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY URL AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ButtonTool class                                                                       
        Notes:         
        @param pcKey The key of the ButtonTool                                                                           
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ButtonTool (pcKey AS CHARACTER):
        
        SUPER (INPUT pcKey).
        
        THIS-OBJECT:Size = RibbonToolSize:Normal .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ToolBase class                                                                       
        Notes:  
        @param pcKey The key of the ButtonTool                                                                           
        @param pcLabel The label of the ButtonTool
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ButtonTool (pcKey AS CHARACTER, 
                                   pcLabel AS CHARACTER):
        
        THIS-OBJECT (INPUT pcKey).
        
        THIS-OBJECT:Label = pcLabel  .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ToolBase class                                                                       
        Notes:                                                                        
        @param pcKey The key of the ButtonTool                                                                           
        @param pcLabel The label of the ButtonTool
        @param poSize The Size of the ButtonTool
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ButtonTool (pcKey AS CHARACTER, 
                                   pcLabel AS CHARACTER, 
                                   poSize AS RibbonToolSize):
        
        THIS-OBJECT (INPUT pcKey, pcLabel).
        
        THIS-OBJECT:Size = poSize .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ToolBase class                                                                       
        Notes:           
        @param pcKey The key of the ButtonTool                                                                           
        @param pcLabel The label of the ButtonTool
        @param poSize The Size of the ButtonTool
        @param pcImage The Image of the ButtonTool                                                                         
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ButtonTool (pcKey AS CHARACTER, 
                                   pcLabel AS CHARACTER, 
                                   poSize AS RibbonToolSize,
                                   pcImage AS CHARACTER):
        
        THIS-OBJECT (INPUT pcKey, pcLabel, poSize).
        
        THIS-OBJECT:Image = pcImage .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the ToolBase class                                                                       
        Notes:      
        @param pcKey The key of the ButtonTool                                                                           
        @param pcLabel The label of the ButtonTool
        @param pcImage The Image of the ButtonTool                                                                         
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ButtonTool (pcKey AS CHARACTER, 
                                   pcLabel AS CHARACTER, 
                                   pcImage AS CHARACTER):
        
        THIS-OBJECT (INPUT pcKey, pcLabel).
        
        THIS-OBJECT:Image = pcImage .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Performs the output for this web component                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID PerformOutput ():
        
        DEFINE VARIABLE cLink AS CHARACTER NO-UNDO.
        
        IF THIS-OBJECT:URL > '':U THEN
             ASSIGN cLink = SUBSTITUTE ('<a href="&1" target="&2">':U,
                                        THIS-OBJECT:URL, 
                                        THIS-OBJECT:Target) . 
        ELSE 
            ASSIGN cLink = '<a href="#">':U .
    
        THIS-OBJECT:OUTPUT (SUBSTITUTE ('                <li class="&3" action="&4"><span>&5':U, 
                                        THIS-OBJECT:Label, 
                                        THIS-OBJECT:Image,
                                        (IF THIS-OBJECT:Size = RibbonToolSize:Large THEN "SmartRibbonLinkLarge":U ELSE "SmartRibbonLinkNormal":U),
                                        THIS-OBJECT:Key,
                                        cLink), 
                            TRUE) . 
        THIS-OBJECT:OUTPUT (SUBSTITUTE ('                    <span><img src="&2" alt="&1"/></span>':U, 
                                        THIS-OBJECT:Label, 
                                        THIS-OBJECT:Image), 
                            TRUE) . 
        THIS-OBJECT:OUTPUT (SUBSTITUTE ('                    <span>&1</span>':U, 
                                        THIS-OBJECT:Label, 
                                        THIS-OBJECT:Image), 
                            TRUE) . 
        THIS-OBJECT:OUTPUT (SUBSTITUTE ('                </a></span></li>':U, 
                                        THIS-OBJECT:Label, 
                                        THIS-OBJECT:Image), 
                            TRUE) . 
        
    END METHOD.
    
END CLASS.
