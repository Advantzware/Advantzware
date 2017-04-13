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
    File        : ttWidgetsByLocation.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Thu Mar 08 17:42:46 CET 2012
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE PRIVATE STATIC TEMP-TABLE ttWidgetsByLocation NO-UNDO
    FIELD WidgetRow    AS DECIMAL
    FIELD WidgetColumn AS DECIMAL
    FIELD WidgetHandle AS HANDLE
    INDEX Location WidgetRow WidgetColumn
    INDEX LocationByColumn WidgetColumn WidgetRow .