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
    File        : crSEntityFieldMapping.p
    Purpose     : Create trigger for SmartEntityFieldMapping table of SmartDB

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Sat Nov 05 06:02:13 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Main Block  *************************** */

TRIGGER PROCEDURE FOR CREATE OF SmartEntityFieldMapping .

ASSIGN SmartEntityFieldMapping.EntityFieldMappingGuid = GUID.

