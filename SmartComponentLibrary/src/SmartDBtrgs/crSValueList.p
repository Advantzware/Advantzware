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
    File        : crSValueList.p
    Purpose     : Create trigger for SmartValueList table of SmartDB

    Syntax      :

    Description : 

    Author(s)   : Marko R�terbories / Consultingwerk Ltd.
    Created     : Wed Oct 10 02:19:57 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Main Block  *************************** */

TRIGGER PROCEDURE FOR CREATE OF SmartValueList.

ASSIGN SmartValueList.ValueListGuid = GUID.
