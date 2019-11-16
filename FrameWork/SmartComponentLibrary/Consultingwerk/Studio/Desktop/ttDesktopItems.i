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
    File        : ttDesktopItems.i
    Purpose     : Temp-Table Definitions for Desktop Items 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Sun May 17 15:51:24 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttDesktopItems NO-UNDO 
    FIELD ItemOrder AS INTEGER
    FIELD ItemText AS CHARACTER 
    FIELD ItemImage AS CHARACTER 
    FIELD RunProcedure AS CHARACTER 
    FIELD OpenWebSite AS CHARACTER
    FIELD SpecialAction AS CHARACTER  
   INDEX Order IS PRIMARY ItemOrder .
  
