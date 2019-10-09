/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : ttPromsgs.i
    Purpose     : Temp-Table definition for PROMSGS import

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Sun Aug 17 00:26:46 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttPromsgs NO-UNDO 
    FIELD MsgNum         AS INTEGER 
    FIELD MsgText        AS CHARACTER 
    FIELD MsgDescription AS CHARACTER 
    FIELD Flag1          AS CHARACTER 
    FIELD Flag2          AS CHARACTER 
    
    INDEX MsgNum IS UNIQUE MsgNum .