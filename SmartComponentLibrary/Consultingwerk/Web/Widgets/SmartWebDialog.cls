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
    File        : SmartWebDialog
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Apr 12 20:11:58 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.*             FROM PROPATH . 
USING Consultingwerk.Web.Widgets.* FROM PROPATH .
USING Progress.Lang.*              FROM PROPATH .

CLASS Consultingwerk.Web.Widgets.SmartWebDialog INHERITS Control: 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the List of the Dialog Buttons                                                                    
        Notes:   Name as the internal value (key), Value for the Label                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Buttons AS Consultingwerk.ListNameValuePair NO-UNDO 
    GET.
    PROTECTED SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Text for this Dialog's Detail section                                                                    
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DetailText AS CHARACTER NO-UNDO 
    GET.
    SET.   
         
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Title for this Dialog's Detail section                                                                    
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DetailTitle AS CHARACTER NO-UNDO INIT "Details"{&TRAN}
    GET.
    SET. 
    
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Text for this Dialog                                                                    
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DialogText AS CHARACTER NO-UNDO 
    GET.
    SET.     
        
    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Title for this Dialog                                                                    
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY DialogTitle AS CHARACTER NO-UNDO INIT "Information"{&TRAN}
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Image Url for this Dialog                                                                    
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY ImageUrl AS CHARACTER NO-UNDO INIT "/SmartComponentsWeb/Images/MessageDialog/information.png":U
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartWebDialog class                                                                    
        Notes:               
        @param pcName The name of the Dialog Control instance                                                         
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartWebDialog (pcName AS CHARACTER):
        SUPER (INPUT pcName).
        
        THIS-OBJECT:Buttons = NEW ListNameValuePair () .
        
        THIS-OBJECT:InitializeButtons () . 
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartWebDialog class                                                                    
        Notes:                                                                        
        @param pcName The name of the Dialog Control instance                                                         
        @param pcDialogText The DialogText for the new Dialog instance
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartWebDialog (pcName AS CHARACTER,
                                       pcDialogText AS CHARACTER):

        THIS-OBJECT (pcName) .
        
        THIS-OBJECT:DialogText = pcDialogText . 

    END.

    /*------------------------------------------------------------------------------
        Purpose: Constructor of the SmartWebDialog class                                                                    
        Notes:                                                                        
        @param pcName The name of the Dialog Control instance                                                         
        @param pcDialogText The Dialog Text for the new Dialog instance
        @param pcDetailText The detail text for the new Dialog instance
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC SmartWebDialog (pcName AS CHARACTER,
                                       pcDialogText AS CHARACTER,
                                       pcDetailText AS CHARACTER):

        THIS-OBJECT (pcName, pcDialogText) .
        
        THIS-OBJECT:DetailText = pcDetailText . 

    END.

    /*------------------------------------------------------------------------------
        Purpose:                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD PROTECTED VOID InitializeButtons ():
        
        THIS-OBJECT:Buttons:Add ("ok":U, "OK"{&TRAN}) .
        THIS-OBJECT:Buttons:Add ("cancel":U, "Cancel"{&TRAN}) .

    END METHOD.

    /*------------------------------------------------------------------------------
        Purpose: Performs the Output for the Web Widget                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID PerformOutput ():
        
        DEFINE VARIABLE cButtons      AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cDetailButton AS CHARACTER NO-UNDO.
        
        {Consultingwerk/foreachABL.i NameValuePair oPair in THIS-OBJECT:Buttons}
        
            ASSIGN cButtons = cButtons + SUBSTITUTE ('                <button type="Button" value="&1">&2</button>~n':U,
                                                     oPair:Name,
                                                     oPair:Value) .
        
        END.
        
        IF THIS-OBJECT:DetailText > "":U THEN 
            ASSIGN cDetailButton = '            <div class="DetailButton">~n':U +
                                   '                <button type="Button" value="details"><img src="/SmartComponentsWeb/Images/MessageDialog/arrow_down_blue.png" width="16px" height="16px"></button>~n':U +
                                   '            </div>~n':U .
        
        THIS-OBJECT:OUTPUT (SUBSTITUTE ('    <div class="SmartDialog show">~n':U +
                                        '        <div class="Overlay">~n':U +
                                        '        </div>~n':U +
                                        '        <div class="Error">~n':U +
                                        '            <div class="Title">~n':U +
                                        '                <span>&1</span>~n':U +
                                        '            </div>~n':U +
                                        '            <div class="Image">~n':U +
                                        '                <img src="&5" width="32px" height="32px">~n':U +
                                        '            </div>~n':U +
                                        '            <div class="Body">~n':U +
                                        '                <span>&2</span>~n':U +
                                        '            </div>~n':U +
                                        '            <div class="StackTrace hidden">~n':U +
                                        '                <label>&3</label>~n':U +
                                        '                <br>~n':U +
                                        '                <span>&4</span>~n':U +
                                        '            </div>~n':U +
                                        '&7':U +
                                        '            <div class="Buttons">~n':U +
                                        '&6':U +
                                        '            </div>~n':U +
                                        '        </div>~n':U +
                                        '    </div>~n':U,
                                        THIS-OBJECT:DialogTitle,
                                        THIS-OBJECT:DialogText,
                                        THIS-OBJECT:DetailTitle,
                                        THIS-OBJECT:DetailText,
                                        THIS-OBJECT:ImageUrl,
                                        cButtons,
                                        cDetailButton), TRUE) . 

    END METHOD.

END CLASS.
