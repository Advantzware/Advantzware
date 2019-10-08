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
    File        : crSAttributeValue.p
    Purpose     : Create trigger for SmartAttributeValue table of SmartDB

    Syntax      :

    Description : 

    Author(s)   : Marko Rüterbories / Consultingwerk Ltd.
    Created     : Tue May 20 16:05:57 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Main Block  *************************** */

TRIGGER PROCEDURE FOR CREATE OF SmartAttributeValue.

ASSIGN SmartAttributeValue.AttributeValueGuid = GUID.

