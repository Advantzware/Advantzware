/**********************************************************************
 * Copyright (C) 2006-2015 by Consultingwerk Ltd. ("CW") -            *
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
    File        : dsTable.i
    Purpose     : Business Entity for Table

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : 03.10.2015 11:25:48
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}
&SCOPED-DEFINE SUFFIX {&SUFFIX}

&GLOBAL-DEFINE DATASET-NAME dsTable

{ Consultingwerk/SmartFramework/System/eSmartTable.i }


@BusinessEntityGenerator (entityname="Consultingwerk.SmartFramework.System.TableBusinessEntity", type="Dataset") .

DEFINE {&ACCESS} DATASET dsTable{&SUFFIX} {&REFERENCE-ONLY} FOR eSmartTable{&SUFFIX} 

    .    
