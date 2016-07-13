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
    File        : MessageFormImages
    Purpose     : Support for customization of default MessageForm images
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Oct 30 12:10:29 CEST 2010
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{ Consultingwerk/products.i }

USING Consultingwerk.Windows.Util.Forms.* FROM PROPATH .
USING Progress.Lang.*                     FROM PROPATH .

CLASS Consultingwerk.Windows.Util.Forms.MessageFormImages: 

    /*------------------------------------------------------------------------------
        Purpose: Error image resource                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY ImageError AS System.Drawing.Image NO-UNDO 
    GET.
    SET.     

    /*------------------------------------------------------------------------------
        Purpose: Information image resource                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY ImageInfo AS System.Drawing.Image NO-UNDO 
    GET.
    SET.     

    /*------------------------------------------------------------------------------
        Purpose: Question image resource                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY ImageQuestion AS System.Drawing.Image NO-UNDO 
    GET.
    SET.     

    /*------------------------------------------------------------------------------
        Purpose: Warning image resource                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY ImageWarning AS System.Drawing.Image NO-UNDO 
    GET.
    SET.     

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation                                                                         
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE MessageFormImages ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Static constructor, loades Consultingerk supplied default images                                                                      
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR STATIC MessageFormImages ():

        FILE-INFO:FILE-NAME = "Consultingwerk\Windows\Util\Forms\error.png"{&TRAN} .
        IF FILE-INFO:FULL-PATHNAME <> ? THEN 
            MessageFormImages:ImageError = Consultingwerk.Util.ImageHelper:Load (FILE-INFO:FULL-PATHNAME) .

        FILE-INFO:FILE-NAME = "Consultingwerk\Windows\Util\Forms\information.png"{&TRAN} .
        IF FILE-INFO:FULL-PATHNAME <> ? THEN 
            MessageFormImages:ImageInfo = Consultingwerk.Util.ImageHelper:Load (FILE-INFO:FULL-PATHNAME) .

        FILE-INFO:FILE-NAME = "Consultingwerk\Windows\Util\Forms\help2.png"{&TRAN} .
        IF FILE-INFO:FULL-PATHNAME <> ? THEN 
            MessageFormImages:ImageQuestion = Consultingwerk.Util.ImageHelper:Load (FILE-INFO:FULL-PATHNAME) .
        
        FILE-INFO:FILE-NAME = "Consultingwerk\Windows\Util\Forms\sign_warning.png"{&TRAN} .
        IF FILE-INFO:FULL-PATHNAME <> ? THEN 
            MessageFormImages:ImageWarning = Consultingwerk.Util.ImageHelper:Load (FILE-INFO:FULL-PATHNAME) .
        
    END CONSTRUCTOR.

END CLASS.
