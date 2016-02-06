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
    File        : ttLoginCompany.i
    Purpose     : 

    Syntax      :

    Description :  

    Author(s)   : Mike Fechner
    Created     : 08.01.2013 20:09:32
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE ttLoginCompany NO-UNDO {&REFERENCE-ONLY} &IF DEFINED (NO-BEFORE) EQ 0 &THEN BEFORE-TABLE eSmartLoginCompanyBefore &ENDIF
    FIELD LoginCompanyKey AS CHARACTER FORMAT "x(36)":U LABEL "LoginCompanyGuid":T SERIALIZE-NAME "LoginCompanyGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD LoginCompanyName AS CHARACTER FORMAT "x(20)":U LABEL "LoginCompanyName":T SERIALIZE-NAME "LoginCompanyName":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD LoginCompanyShort AS CHARACTER FORMAT "x(20)":U LABEL "Short Name":T SERIALIZE-NAME "LoginCompanyShort":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD LoginCompanyImageLarge AS CHARACTER FORMAT "x(50)":U LABEL "Large Image":T SERIALIZE-NAME "LoginCompanyImageLarge":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD LoginCompanyImageSmall AS CHARACTER FORMAT "x(50)":U LABEL "Small Image":T SERIALIZE-NAME "LoginCompanyImageSmall":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD LoginCompanyReferenceChar AS CHARACTER FORMAT "x(50)":U LABEL "Foreign Reference Character":T SERIALIZE-NAME "LoginCompanyReferenceChar":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD LoginCompanyReferenceDecimal AS DECIMAL FORMAT "->,>>,>>>,>>9.999999999":U INIT "0":U LABEL "Foreign Reference Decimal":T SERIALIZE-NAME "LoginCompanyReferenceDecimal":U XML-DATA-TYPE "decimal":U XML-NODE-TYPE "ELEMENT":U
    FIELD LoginCompanyReferenceInteger AS INTEGER FORMAT "->,>>>,>>9":U INIT "0":U LABEL "Foreign Reference Integer":T SERIALIZE-NAME "LoginCompanyReferenceInteger":U XML-DATA-TYPE "int":U XML-NODE-TYPE "ELEMENT":U
    FIELD LoginCompanyTenantDomain AS CHARACTER FORMAT "x(50)":U LABEL "Multi-Tenancy Domain":T SERIALIZE-NAME "LoginCompanyTenantDomain":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U

    INDEX LoginCompanyKey AS UNIQUE PRIMARY LoginCompanyKey ASCENDING

    .
