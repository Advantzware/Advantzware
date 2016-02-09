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
    File        : ErrorMessageWidget
    Purpose     : Shows an Error Message 
    Syntax      : 
    Description : 
    Author(s)   : Sebastian Düngel / Consultingwerk Ltd.
    Created     : Tue Mar 26 11:47:26 CET 2013
    Notes       : Shows an error message in a styled web div container 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

USING Consultingwerk.Web.Widgets.*         FROM PROPATH .  
USING Consultingwerk.Web.Widgets.Control.* FROM PROPATH.
USING Progress.Lang.*                      FROM PROPATH .

CLASS Consultingwerk.Web.Widgets.ErrorMessageWidget
    INHERITS Control: 
        
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Error Text
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ErrorText AS CHARACTER NO-UNDO 
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
        Purpose: Constructor of the ErrorImageWidget class                                                                      
        Notes:                                  
        @param pcName The name of the Image Control                                      
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ErrorMessageWidget (pcName AS CHARACTER):
        SUPER (INPUT pcName).
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the ErrorMessageWidget class                                                                      
        Notes:
        @param pcName The name of the ErrorMessageWidget Control                                     
        @param pcErrorText Thedisplay error text
        @param pcimageUrl The url of the displayed image                                     
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC ErrorMessageWidget (pcName AS CHARACTER,
                                           pcErrorText AS CHARACTER, 
                                           pcImageUrl AS CHARACTER):
        THIS-OBJECT (INPUT pcName).
        
        THIS-OBJECT:ImageUrl = pcImageUrl .
        THIS-OBJECT:ErrorText = pcErrorText.
        
    END CONSTRUCTOR.
    
    /*------------------------------------------------------------------------------
        Purpose: Performs the output for the error message widget                                                                
        Notes:                                  
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID PerformOutput ():
        
        THIS-OBJECT:OUTPUT (SUBSTITUTE ("<div name='&1'>":U + 
                                            "<table style='margin-left: auto;margin-right: auto;'>":U +
                                              "<tr>":U +
                                                "<td style='vertical-align: middle; width: 25px;'>":U +
                                                  "<img src='&2'>":U +
                                                "</td>":U +
                                                "<td style='vertical-align: middle;'>":U +
                                                  "<span style='font-family:Segoe UI,Tahoma,Verdana,sans-serif;font-size: 10pt;'>&3</span>":U +
                                                "</td>":U +
                                              "</tr>":U +
                                            "</table>":U +
                                        "</div>":U,
                                        THIS-OBJECT:Name,
                                        THIS-OBJECT:ImageUrl,
                                        THIS-OBJECT:ErrorText)).
    END METHOD .
    
END CLASS.
