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
    File        : MessageFormResources
    Purpose     : Support for translation of default MessageForm buttons
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Oct 30 12:10:29 CEST 2010
    Notes       : 
  ----------------------------------------------------------------------*/

ROUTINE-LEVEL ON ERROR UNDO, THROW.

{Consultingwerk/products.i} 

USING Consultingwerk.Util.Forms.* FROM PROPATH . 

CLASS Consultingwerk.Util.Forms.MessageFormResources: 

    /*------------------------------------------------------------------------------
        Purpose: Allows customization of the Label of a Cancel button                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY MessageForButtonCancel AS CHARACTER NO-UNDO INITIAL "&Cancel"{&TRAN} 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Allows customization of the Label of a No button                                                                       
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY MessageForButtonNo AS CHARACTER NO-UNDO INITIAL "&No"{&TRAN} 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Allows customization of the Label of an Ok button                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY MessageForButtonOk AS CHARACTER NO-UNDO INITIAL "&Ok"{&TRAN} 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Allows customization of the Label of a Retry button                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY MessageForButtonRetry AS CHARACTER NO-UNDO INITIAL "&Retry"{&TRAN} 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Allows customization of the Label of a Yes button                                                                        
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC STATIC PROPERTY MessageForButtonYes AS CHARACTER NO-UNDO INITIAL "&Yes"{&TRAN} 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Disallow instance creation                                                                         
        Notes:                                                                        
    ------------------------------------------------------------------------------*/
    CONSTRUCTOR PRIVATE MessageFormResources ():
        SUPER ().
        
    END CONSTRUCTOR.

    /*------------------------------------------------------------------------------
        Purpose: Initializes the Message Form strings from the TranslationProvider
        Notes:   
    ------------------------------------------------------------------------------*/
    METHOD PUBLIC STATIC VOID InitializeFromTranslationProvider ():
        
        MessageForButtonCancel = {Consultingwerk/translate.i &scope="'Button':U" &string="'&Cancel':U"} .
        MessageForButtonNo = {Consultingwerk/translate.i &scope="'Button':U" &string="'&No':U"} .
        MessageForButtonOk = {Consultingwerk/translate.i &scope="'Button':U" &string="'&Ok':U"} .
        MessageForButtonRetry = {Consultingwerk/translate.i &scope="'Button':U" &string="'&Retry':U"} .
        MessageForButtonYes = {Consultingwerk/translate.i &scope="'Button':U" &string="'&Yes':U"} .

    END METHOD .

END CLASS.