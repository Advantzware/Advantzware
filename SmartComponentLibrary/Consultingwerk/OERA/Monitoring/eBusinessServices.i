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
    File        : eBusinessServices.i
    Purpose     : Temp-Table with details about loaded Business Services

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 05.07.2016 14:02:46
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

@BusinessEntityGenerator (entityname="Consultingwerk.OERA.Monitoring.ServiceManagerMonitoring", type="TempTable") .

DEFINE {&ACCESS} TEMP-TABLE eBusinessServices{&SUFFIX} NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eBusinessServicesBefore{&SUFFIX} &ENDIF
    FIELD ServiceName AS CHARACTER
    FIELD ClassName AS CHARACTER
    FIELD Managed AS LOGICAL
    FIELD Started AS DATETIME-TZ
    FIELD LastRequest AS DATETIME-TZ

    INDEX ServiceName AS UNIQUE PRIMARY ServiceName ASCENDING

    .

    