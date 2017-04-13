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
    File        : dsServiceManagerMonitoring.i
    Purpose     : Business Entity for ServiceManagerMonitoring

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 05.07.2016 13:16:56
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}
&SCOPED-DEFINE SUFFIX {&SUFFIX}

&GLOBAL-DEFINE DATASET-NAME dsServiceManagerMonitoring

{ Consultingwerk/OERA/Monitoring/eBusinessServices.i &NO-BEFORE=YES }
{ Consultingwerk/OERA/Monitoring/eSessionInfo.i &NO-BEFORE=YES }


@BusinessEntityGenerator (entityname="Consultingwerk.OERA.Monitoring.ServiceManagerMonitoring", type="Dataset") .

DEFINE {&ACCESS} DATASET dsServiceManagerMonitoring{&SUFFIX} {&REFERENCE-ONLY} FOR eBusinessServices{&SUFFIX}, eSessionInfo{&SUFFIX} 

    .    
