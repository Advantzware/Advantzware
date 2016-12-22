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
    File        : TaskBarMenuItem
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 10 09:44:45 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Web.Widgets.*        FROM PROPATH . 
USING Consultingwerk.Web.Widgets.Ribbon.* FROM PROPATH .  
USING Progress.Lang.*                     FROM PROPATH .

CLASS Consultingwerk.Web.Widgets.Ribbon.TaskBarMenuItem
    INHERITS Component: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets of the Tool is enabled                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Enabled AS LOGICAL NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Key of the Tool                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Key AS CHARACTER NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Label of the Tool                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Label AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Image Name for this Button                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Image AS CHARACTER NO-UNDO 
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
        Purpose: Constructor for the TaskBarMenuItem class                                                                       
        Notes:          
        @param pcKey The key of the TaskBarMenuItem                                                                
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TaskBarMenuItem (pcKey AS CHARACTER):
        
        SUPER ().
        
        THIS-OBJECT:Key = pcKey . 
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TaskBarMenuItem class                                                                       
        Notes:                                                                        
        @param pcKey The key of the TaskBarMenuItem
        @param pcLabel The label of the TaskBarMenuItem                                                                 
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TaskBarMenuItem (pcKey AS CHARACTER, 
                                        pcLabel AS CHARACTER):
        
        THIS-OBJECT (INPUT pcKey).
        
        THIS-OBJECT:Label = pcLabel  .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TaskBarMenuItem class                                                                       
        Notes:          
        @param pcKey The key of the TaskBarMenuItem
        @param pcLabel The label of the TaskBarMenuItem                                                                 
        @param pcUrl The URL of the TaskBarMenuItem                                                                       
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TaskBarMenuItem (pcKey AS CHARACTER, 
                                        pcLabel AS CHARACTER, 
                                        pcUrl AS CHARACTER):
        
        THIS-OBJECT (INPUT pcKey, pcLabel).
        
        THIS-OBJECT:Url = pcUrl .
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Constructor for the TaskBarMenuItem class                                                                       
        Notes:                                                                        
        @param pcKey The key of the TaskBarMenuItem
        @param pcLabel The label of the TaskBarMenuItem                                                                 
        @param pcUrl The URL of the TaskBarMenuItem                                                                       
        @param pcImage The Image of the TaskBarMenuItem
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TaskBarMenuItem (pcKey AS CHARACTER, 
                                        pcLabel AS CHARACTER, 
                                        pcUrl AS CHARACTER,
                                        pcImage AS CHARACTER):
        
        THIS-OBJECT (INPUT pcKey, pcLabel, pcUrl).
        
        THIS-OBJECT:Image = pcImage .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Performs the generation of the required html code                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC VOID PerformOutput ():
        
        THIS-OBJECT:OUTPUT ('<li>':U).
        THIS-OBJECT:OUTPUT (SUBSTITUTE ('    <a href="&1" target="&2">':U, THIS-OBJECT:URL, THIS-OBJECT:Target)).
        THIS-OBJECT:OUTPUT ('    <span>':U).
        THIS-OBJECT:OUTPUT ('        <div>':U).
        THIS-OBJECT:OUTPUT (SUBSTITUTE ('            <img src="&1" width="30px" height="30px" border="0">':U, THIS-OBJECT:Image)).
        THIS-OBJECT:OUTPUT ('        </div>':U).
        THIS-OBJECT:OUTPUT ('        <div>':U).
        THIS-OBJECT:OUTPUT (THIS-OBJECT:LABEL).
        THIS-OBJECT:OUTPUT ('        </div>':U).
        THIS-OBJECT:OUTPUT ('    </span>':U).
        THIS-OBJECT:OUTPUT ('    </a>':U).
        THIS-OBJECT:OUTPUT ('</li>':U).

    END METHOD.

END CLASS.
