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
    File        : crSAttachment.p
    Purpose     : Create trigger for SmartSecurityToken table of SmartDB

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Apr 17 20:43:11 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Main Block  *************************** */

TRIGGER PROCEDURE FOR CREATE OF SmartSecurityToken.

ASSIGN SmartSecurityToken.SecurityTokenGuid = GUID.
