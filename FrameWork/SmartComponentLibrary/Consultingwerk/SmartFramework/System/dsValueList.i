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
    File        : dsValueList.i
    Purpose     : Business Entity for ValueList

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner
    Created     : 08.03.2013 16:34:16
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}

&GLOBAL-DEFINE DATASET-NAME dsValueList

{ Consultingwerk/SmartFramework/System/eSmartValueList.i }
{ Consultingwerk/SmartFramework/System/eSmartValueListEntry.i }


DEFINE {&ACCESS} DATASET dsValueList {&REFERENCE-ONLY} FOR eSmartValueList, eSmartValueListEntry 
    DATA-RELATION eSmartValueListeSmartValueListEn FOR eSmartValueList, eSmartValueListEntry 
        RELATION-FIELDS (ValueListGuid,ValueListGuid)

    .    
