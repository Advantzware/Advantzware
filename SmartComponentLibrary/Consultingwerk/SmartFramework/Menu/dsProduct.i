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
    File        : dsProduct.i
    Purpose     : Business Entity for SmartProduct

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner
    Created     : 06.04.2014 11:15:34
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}

&GLOBAL-DEFINE DATASET-NAME dsProduct

{ Consultingwerk/SmartFramework/Menu/eSmartProduct.i }


DEFINE {&ACCESS} DATASET dsProduct {&REFERENCE-ONLY} FOR eSmartProduct 

    .    
