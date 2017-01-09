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

    DEFINE PUBLIC PROPERTY DefaultDialogResult AS Consultingwerk.Framework.Enum.DialogResultEnum NO-UNDO
    GET.
    SET.

    DEFINE PUBLIC PROPERTY MessageButtonLabels AS CHARACTER EXTENT NO-UNDO 
    GET.
    SET. 

    DEFINE PUBLIC PROPERTY MessageButtonDialogResults AS DialogResultEnum EXTENT NO-UNDO 
    GET.
    SET. 

    DEFINE PUBLIC PROPERTY MessageDetail AS CHARACTER NO-UNDO 
    GET.
    SET. 

    DEFINE PUBLIC PROPERTY MessageImage AS Progress.Lang.Object NO-UNDO 
    GET. 
    SET.

    DEFINE PUBLIC PROPERTY MessageText AS CHARACTER NO-UNDO 
    GET.
    SET. 

END INTERFACE.