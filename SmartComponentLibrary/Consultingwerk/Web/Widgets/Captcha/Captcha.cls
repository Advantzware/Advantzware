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
    File        : Captcha
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd. 
    Created     : Fri Jun 01 23:38:27 CEST 2012
    Notes       : Not supported without SmartFramework / SmartDB
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i}

USING Consultingwerk.Web.*                 FROM PROPATH .  
USING Consultingwerk.Web.Widgets.*         FROM PROPATH .
USING Consultingwerk.Web.Widgets.Captcha.* FROM PROPATH .  
USING Progress.Lang.*                      FROM PROPATH .

CLASS Consultingwerk.Web.Widgets.Captcha.Captcha 
    INHERITS Control
    IMPLEMENTS IPerformInput:

    /*------------------------------------------------------------------------------
        Purpose: Returns the reference to the TextBox Control used for Captcha entry  
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY TextBox AS TextBox NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Returns True of the Captcha validation succeeded
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY Valid AS LOGICAL INITIAL FALSE NO-UNDO 
    GET.
    PRIVATE SET. 

    /*------------------------------------------------------------------------------
        Purpose: Constructor for the Captcha class                                                                        
        Notes:                                    
        @param pcKey The key of the Captach to be generated                                    
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PUBLIC Captcha (pcKey AS CHARACTER):
        SUPER (pcKey).
        
        THIS-OBJECT:Width = 200 .
        THIS-OBJECT:Height = 60 . 

        THIS-OBJECT:TextBox = NEW TextBox (pcKey) .
        THIS-OBJECT:TextBox:Width = 200 .
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Performs the Input for this web component
        Notes:   
        @return "TRUE" When the Captcha is valid, else ? 
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC CHARACTER PerformInput ():
        
        DEFINE VARIABLE cGuid  AS CHARACTER NO-UNDO.
        DEFINE VARIABLE cValue AS CHARACTER NO-UNDO.

        &IF DEFINED (SmartFramework) NE 0 &THEN        
        DEFINE BUFFER WebCaptcha FOR WebCaptcha . 

        ASSIGN cGuid  = WebUtilities:GetField (SUBSTITUTE ("&1_guid":U, THIS-OBJECT:Name)) 
               cValue = WebUtilities:GetField (THIS-OBJECT:Name) .

        /* Existing Captcha Record, then delete */
        FIND FIRST WebCaptcha WHERE WebCaptcha.CaptchaGuid = cGuid NO-ERROR .

        IF NOT AVAILABLE WebCaptcha THEN 
            RETURN ? .
            
        IF cValue <> WebCaptcha.CaptchaValue THEN 
            RETURN ? . 
            
        ASSIGN THIS-OBJECT:Valid = TRUE . 
        
        RETURN "TRUE":U . 

        FINALLY:
            IF AVAILABLE WebCaptcha THEN 
                DELETE WebCaptcha .         
        END FINALLY.

        &ELSE
        UNDO, THROW NEW Consultingwerk.Exceptions.NotSupportedException ("PerformInput":U,
                                                                         THIS-OBJECT:GetClass():TypeName) .
        &ENDIF

    END METHOD .

    /*------------------------------------------------------------------------------
        Purpose: Performs the output for this web component                                                                     
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    METHOD OVERRIDE PUBLIC VOID PerformOutput ():
        
        DEFINE VARIABLE cCaptchaUrl AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cID         AS CHARACTER NO-UNDO .
        DEFINE VARIABLE cString     AS CHARACTER NO-UNDO .
        
        &IF DEFINED (SmartFramework) NE 0 &THEN        
        DEFINE BUFFER WebCaptcha FOR WebCaptcha . 
        
        /* Existing Captcha Record, then delete */
        FIND FIRST WebCaptcha WHERE WebCaptcha.CaptchaGuid = WebUtilities:GetField (SUBSTITUTE ("&1_guid":U, THIS-OBJECT:Name)) NO-ERROR .
        IF AVAILABLE WebCaptcha THEN 
            DELETE WebCaptcha .
            
        /* Create Captcha Record */            
        CREATE WebCaptcha.
        ASSIGN WebCaptcha.CaptchaGuid      = GUID + GUID 
               WebCaptcha.CaptchaValue     = STRING (RANDOM (0, 999999), "999999":U)
               WebCaptcha.CaptchaTimeStamp = NOW .  
        
        FIND CURRENT WebCaptcha NO-LOCK .
        
        ASSIGN cCaptchaUrl = SUBSTITUTE ("&1/Consultingwerk/Web/Widgets/Captcha/get-captcha.w?Id=&2&&&3":U,
                                         WebContext:AppUrl,
                                         WebCaptcha.CaptchaGuid, 
                                         ETIME) .
        
        THIS-OBJECT:OUTPUT (SUBSTITUTE ('<img src="&1" border="0" alt="&6" name="&5" style="top: &2px; left: &3px; width: &4px"></button>':U,
                                        cCaptchaUrl,
                                        THIS-OBJECT:Top + 30,
                                        THIS-OBJECT:Left,
                                        THIS-OBJECT:Width), 
                                        TRUE) .
                                        
        THIS-OBJECT:OUTPUT (SUBSTITUTE ('<input type="hidden" name="&1_guid" value="&2">':U,
                                        THIS-OBJECT:Name,
                                        Consultingwerk.Web.WebUtilities:UrlEncode (WebCaptcha.CaptchaGuid, 
                                                                                   Consultingwerk.Web.UrlEncodeTypeEnum:Query)), 
                            TRUE) .
                         
        THIS-OBJECT:TextBox:Top = THIS-OBJECT:Top .
        THIS-OBJECT:TextBox:Left = THIS-OBJECT:Left . 
        THIS-OBJECT:TextBox:Width = THIS-OBJECT:Width .  
        THIS-OBJECT:TextBox:PerformOutput () .                         

        &ELSE
        UNDO, THROW NEW Consultingwerk.Exceptions.NotSupportedException ("PerformOutput":U,
                                                                         THIS-OBJECT:GetClass():TypeName) .
        &ENDIF
                                                
    END METHOD .

END CLASS.
