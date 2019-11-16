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
    File        : TextContainer
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Sebastian Düngel / Consultingwerk Ltd.
    Created     : Thu Nov 28 16:28:06 CET 2013
    Notes       : Shows an TextContainer
  ----------------------------------------------------------------------*/
ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Web.Widgets.*         FROM PROPATH .  
USING Consultingwerk.Web.Widgets.Control.* FROM PROPATH.
USING Progress.Lang.*                      FROM PROPATH .

CLASS Consultingwerk.Web.Widgets.TextContainer
    INHERITS Control: 
        
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Error Text
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Text AS CHARACTER NO-UNDO 
    GET.
    SET. 
        
    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ErrorImageWidget class                                                                      
        Notes:                                  
        @param pcName The name of the Image Control                                      
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TextContainer (pcName AS CHARACTER):
        SUPER (INPUT pcName).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ErrorMessageWidget class                                                                      
        Notes:
        @param pcName The name of the ErrorMessageWidget Control                                     
        @param pcText Thedisplay error text
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC TextContainer (pcName AS CHARACTER,
                                      pcText AS LONGCHAR):
        THIS-OBJECT (INPUT pcName).
        
        THIS-OBJECT:Text = pcText.
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Performs the output for the error message widget                                                                
        Notes:                                  
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID PerformOutput ():
        
        THIS-OBJECT:OUTPUT (SUBSTITUTE ("<div name='&1' class='TextContainer'>":U + 
                                            "&2":U +
                                        "</div>":U,
                                        THIS-OBJECT:Name,
                                        THIS-OBJECT:Text)).
    END METHOD .
    
END CLASS.
