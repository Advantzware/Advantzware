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
    File        : crSCustomizationType.p
    Purpose     : Create trigger for SmartCustomizationType table of SmartDB

    Syntax      :

    Description : 

    Author(s)   : Marko Rueterbories / Consultingwerk Ltd.
    Created     : Wed Mar 09 07:23:57 CEST 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Main Block  *************************** */

USING Consultingwerk.Framework.Session.* FROM PROPATH.

TRIGGER PROCEDURE FOR CREATE OF SmartCustomizationType.

ASSIGN SmartCustomizationType.CustomizationTypeGuid = GUID.
