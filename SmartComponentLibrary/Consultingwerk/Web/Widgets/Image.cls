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
    File        : Image
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat May 26 23:59:36 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Web.Widgets.Control.

USING Consultingwerk.Web.Widgets.* FROM PROPATH .  
USING Progress.Lang.* FROM PROPATH .

CLASS Consultingwerk.Web.Widgets.Image INHERITS Control: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Alternative Text
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY AlternativeText AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the URL of the Image 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ImageUrl AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the Image class                                                                      
        Notes:                                  
        @param pcName The name of the Image Control                                      
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Image (pcName AS CHARACTER):
        SUPER (INPUT pcName).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the Image class                                                                      
        Notes:                                  
        @param pcName The name of the Image Control  
        @param pcImageUrl The Url of the Image                                    
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Image (pcName AS CHARACTER,
                              pcImageUrl AS CHARACTER):
                                  
        THIS-OBJECT (INPUT pcName).
        
        THIS-OBJECT:ImageUrl = pcImageUrl .
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Performs the output for the Button                                                               
        Notes:                                  
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID PerformOutput ():
        
        THIS-OBJECT:OUTPUT (SUBSTITUTE ('<img src="&1" border="0" alt="&6" name="&5" style="top: &2px; left: &3px; width: &4px" value="&1"></button>':U,
                                        THIS-OBJECT:ImageUrl,
                                        THIS-OBJECT:Top,
                                        THIS-OBJECT:Left,
                                        THIS-OBJECT:Width,
                                        THIS-OBJECT:Name,
                                        THIS-OBJECT:AlternativeText), 
                                        TRUE) .
    END METHOD .

END CLASS.
