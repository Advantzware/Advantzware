/**********************************************************************
 * Copyright (C) 2006-2014 by Consultingwerk Ltd. ("CW") -            *
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
    File        : dsTrigger.i
    Purpose     : Trigger Definition Dataset 

    Syntax      :

    Description : 

    Author(s)   : Mike Fechner / Consultingwerk Ltd. 
    Created     : Thu Aug 28 16:17:29 CEST 2014
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

&SCOPED-DEFINE ACCESS {&ACCESS}
&SCOPED-DEFINE REFERENCE-ONLY {&REFERENCE-ONLY}

&GLOBAL-DEFINE DATASET-NAME dsCustomer

{Consultingwerk/Studio/LegacyGuiMigration/Frame/ttTriggerCode.i}
{Consultingwerk/Studio/LegacyGuiMigration/Frame/ttTriggerEvent.i}

DEFINE {&ACCESS} DATASET dsTrigger {&REFERENCE-ONLY} FOR ttTriggerEvent, ttTriggerCode
    DATA-RELATION TriggerCodeRelation FOR ttTriggerEvent, ttTriggerCode RELATION-FIELDS (TriggerCodeGuid, TriggerCodeGuid) 
    . 
