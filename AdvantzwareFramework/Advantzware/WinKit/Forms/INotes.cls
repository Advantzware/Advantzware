/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : INotes
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : Mike
    Created     : Sat Feb 06 23:48:46 CET 2016
    Notes       : 
  ----------------------------------------------------------------------*/

USING Advantzware.WinKit.Forms.* FROM PROPATH .  
USING Progress.Lang.*            FROM PROPATH .

INTERFACE Advantzware.WinKit.Forms.INotes:  

    /*------------------------------------------------------------------------------
        Purpose: 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY HasAttachments AS LOGICAL NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: 
        Notes:   
    ------------------------------------------------------------------------------*/
    DEFINE PUBLIC PROPERTY HasCustomFields AS LOGICAL NO-UNDO 
    GET.
    SET. 

    /*------------------------------------------------------------------------------
        Purpose: 
        Notes:   
    ------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY HasNotes AS LOGICAL NO-UNDO 
	GET.
	SET. 

    /*------------------------------------------------------------------------------
        Purpose: 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID DisableNotes ().

    /*------------------------------------------------------------------------------
        Purpose: 
        Notes:   
    ------------------------------------------------------------------------------*/
	METHOD PUBLIC VOID EnableNotes ().

END INTERFACE.
