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
    File        : eSmartProduct.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner
    Created     : 06.04.2014 11:15:34
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE eSmartProduct NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartProductBefore &ENDIF
    FIELD ProductGuid AS CHARACTER FORMAT "x(36)":U LABEL "ProductGuid":T SERIALIZE-NAME "ProductGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD ProductCode AS CHARACTER FORMAT "x(20)":U LABEL "Code":T SERIALIZE-NAME "ProductCode":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD ProductName AS CHARACTER FORMAT "x(80)":U LABEL "Name":T SERIALIZE-NAME "ProductName":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD ProductDescription AS CLOB FORMAT "x(8)":U LABEL "ProductDescription":T SERIALIZE-NAME "ProductDescription":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD ProductInstalled AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "Installed":T SERIALIZE-NAME "ProductInstalled":U XML-DATA-TYPE "boolean":U XML-NODE-TYPE "ELEMENT":U

    INDEX ProductCode AS UNIQUE ProductCode ASCENDING
    INDEX ProductGuid AS UNIQUE PRIMARY ProductGuid ASCENDING
    INDEX ProductName ProductName ASCENDING

    .
