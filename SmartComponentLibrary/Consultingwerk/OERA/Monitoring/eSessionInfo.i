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
    File        : eSessionInfo.i
    Purpose     : Temp-Table with Session Information

    Syntax      :

    Description :

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 05.07.2016 13:16:56
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.OERA.Monitoring.ServiceManagerMonitoring", type="TempTable") .

DEFINE {&ACCESS} TEMP-TABLE eSessionInfo{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSessionInfoBefore{&SUFFIX} &ENDIF
    FIELD Pid AS INTEGER FORMAT ">,>>>,>>9":U INITIAL ? LABEL "Progress ID":T
    FIELD Started AS DATETIME-TZ FORMAT "99/99/9999 HH:MM:SS.SSS+HH:MM":U LABEL "Started":T

    INDEX Pid AS UNIQUE PRIMARY Pid ASCENDING

    .

