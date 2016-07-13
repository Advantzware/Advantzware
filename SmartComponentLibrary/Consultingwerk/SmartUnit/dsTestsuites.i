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
    File        : dsTestsuites.i
    Purpose     : Business Entity for Testsuites

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner
    Created     : 25.03.2013 19:01:39
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}

&GLOBAL-DEFINE DATASET-NAME dsTestsuites

{ Consultingwerk/SmartUnit/eTestsuite.i &NO-BEFORE=YES }
{ Consultingwerk/SmartUnit/eTestcase.i &NO-BEFORE=YES }


DEFINE {&ACCESS} DATASET dsTestsuites {&REFERENCE-ONLY} FOR eTestsuite, eTestcase 
    DATA-RELATION eTestsuiteeTestcaseRelation FOR eTestsuite, eTestcase 
        RELATION-FIELDS (TestsuiteName,TestsuiteName)
        NESTED FOREIGN-KEY-HIDDEN 

    .    
