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
    File        : crSDeletion.p
    Purpose     : Create trigger for SmartDeletion table of SmartDB

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Wed Oct 10 02:19:57 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Main Block  *************************** */

USING Consultingwerk.Framework.Session.* FROM PROPATH.

TRIGGER PROCEDURE FOR CREATE OF SmartDeletion.

{Consultingwerk/products.i}

ASSIGN SmartDeletion.DeletionGuid      = GUID
       SmartDeletion.DeletionTimeStamp = NOW .

IF SessionManager:UserName > "":U THEN
    SmartDeletion.DeletionUser = SessionManager:UserName .
ELSE
    SmartDeletion.DeletionUser = USERID (&IF DEFINED (SmartDB) NE 0 &THEN "{&SmartDB}":U &ELSE "SmartDB":U &ENDIF) .
