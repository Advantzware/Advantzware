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
    File        : ttMenuStructure.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd.
    Created     : Fri Dec 07 17:38:28 CET 2012
    Notes       : The fields names here use the term "Key" rather than the
                  Term "Guid" to make it more generic
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE {&ACCESS} TEMP-TABLE ttMenuStructure NO-UNDO {&REFERENCE-ONLY} 
    FIELD MenuKey AS CHARACTER FORMAT "x(36)":U LABEL "MenuGuid":T SERIALIZE-NAME "MenuGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD ParentMenuKey AS CHARACTER FORMAT "x(36)":U LABEL "ParentMenuGuid":T SERIALIZE-NAME "ParentMenuGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD MenuName AS CHARACTER FORMAT "x(20)":U LABEL "MenuName":T SERIALIZE-NAME "MenuName":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD MenuStructureType AS CHARACTER FORMAT "x(12)":U LABEL "MenuStructureType":T SERIALIZE-NAME "MenuStructureType":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD MenuBeginsAGroup AS LOGICAL FORMAT "yes/no":U INIT "no":U LABEL "MenuBeginsAGroup":T SERIALIZE-NAME "MenuBeginsAGroup":U XML-DATA-TYPE "boolean":U XML-NODE-TYPE "ELEMENT":U
    FIELD MenuSmallImage AS CHARACTER FORMAT "x(40)":U LABEL "MenuSmallImage":T SERIALIZE-NAME "MenuSmallImage":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD MenuLargeImage AS CHARACTER FORMAT "x(40)":U LABEL "MenuLargeImage":T SERIALIZE-NAME "MenuLargeImage":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD MenuSequence AS INTEGER FORMAT "->,>>>,>>9":U INIT "0":U LABEL "MenuSequence":T SERIALIZE-NAME "MenuSequence":U XML-DATA-TYPE "int":U XML-NODE-TYPE "ELEMENT":U
    FIELD FunctionKey AS CHARACTER FORMAT "x(36)":U LABEL "FunctionGuid":T SERIALIZE-NAME "FunctionGuid":U XML-DATA-TYPE "string":U XML-NODE-TYPE "ELEMENT":U
    FIELD HasChild AS LOGICAL FORMAT "yes/no":U INIT "FALSE":U LABEL "HasChild":T

    INDEX MenuKey AS UNIQUE PRIMARY MenuKey ASCENDING
    INDEX ParentMenuKeySequence ParentMenuKey ASCENDING MenuSequence ASCENDING
    .
