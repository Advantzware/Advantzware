/**********************************************************************
 * Copyright (C) 2006-2016 by Consultingwerk Ltd. ("CW") -            *
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
    File        : IMessageForm
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Oct 29 10:42:43 CEST 2010
    Notes       : 
  ----------------------------------------------------------------------*/

USING Consultingwerk.Framework.Enum.* .
USING Progress.Lang.* .

INTERFACE Consultingwerk.Util.Forms.IMessageForm:  

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the default Dialog Result                                                                     
        Notes:  
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY DefaultDialogResult AS Consultingwerk.Framework.Enum.DialogResultEnum NO-UNDO
    GET.
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the array of the message button labels                                                                      
        Notes:  
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY MessageButtonLabels AS CHARACTER EXTENT NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the array of the message button Dialog Results 
        Notes:  
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY MessageButtonDialogResults AS DialogResultEnum EXTENT NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Message Detail                                                                      
        Notes:  
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY MessageDetail AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Message Image                                                                      
        Notes:  
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY MessageImage AS Progress.Lang.Object NO-UNDO 
    GET. 
    SET.

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Message Text                                                                      
        Notes:  
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY MessageText AS CHARACTER NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: Gets and sets the Message Timer Value                                                                   
        Notes:  
    ------------------------------------------------------------------------------*/    
    DEFINE PUBLIC PROPERTY MessageTimer AS INTEGER NO-UNDO 
    GET.
    SET. 

END INTERFACE.
