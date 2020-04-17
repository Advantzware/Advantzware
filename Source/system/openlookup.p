/*------------------------------------------------------------------------
  File: system\openlookup.p
  Description: This is a wrapper program to call generic dynamic lookup
               windows\l-lookup.w
  Input Parameters:
    ipcCompany     :The company code
    ipcLookupField :The source field for which the lookup screen is called for
    ipiSubjectID   :Dynamic Subject ID
    ipcUserID      :Dynamic Subject User ID
    ipiParamValueID:Dynamic Parameter Value ID
  Output Parameters:
    opcReturnFields:Pipe separated list of return field values as output 
                    based on previous input list
    opcLookupField :Single return value which is to be returned from 
                    the lookup - this will populate in the field from 
                    where the lookup was opened
    oprRecID      :RecID of the row selected when a row is selected 
                    in the browse
  Author: Mithun Porandla
  Created: 13th March 2019
------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER ipcCompany      AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcLookupField  AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipiSubjectID    AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ipcUserID       AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipiParamValueID AS INTEGER   NO-UNDO.

DEFINE OUTPUT PARAMETER opcReturnFields AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER opcLookupField  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER oprRecID        AS RECID     NO-UNDO.

DEFINE VARIABLE iRecordLimit AS INTEGER NO-UNDO INITIAL 15000.

IF ipcUserID EQ "" THEN
ipcUserID = "_default".
IF ipiSubjectID GT 0 AND
   CAN-FIND(FIRST dynParamValue
            WHERE dynParamValue.subjectID    EQ ipiSubjectID
              AND dynParamValue.user-id      EQ ipcUserID
              AND dynParamValue.paramValueID EQ ipiParamValueID) THEN DO:
    RUN AOA/dynLookup.p (
        ipcCompany,
        ipiSubjectID,
        ipcUserID,
        ipiParamValueID,
        OUTPUT opcReturnFields,
        OUTPUT opcLookupField,
        OUTPUT oprRecID
        ).
END. /* if can-find */
ELSE
/* changed case statement into procedures before it reached 32k limit */
RUN VALUE(ipcLookupField).

/* ****************************************************************************************** */
/* ****************************************************************************************** */
/*                                                                                            */
/* Lookups should not be added here. Dynamic Lookups should be utilized moving forward.       */
/* RStark - 4.14.2020                                                                         */
/*                                                                                            */
/* Reference: N:\Documentation\UserManual\DynamicAOA\Dynamic AOA Lookup Developers Guide.docx */
/*                                                                                            */
/* ****************************************************************************************** */
/* ****************************************************************************************** */

PROCEDURE apiID: 
        RUN windows\l-lookup.w
           (/* Title of the lookup screen */
           INPUT "Outbound API ID",
           /* The source field for which the lookup screen is called for */
           INPUT "apiID",
           /* DB Table from which data is to be fetched */
           INPUT "APIOutbound",
           /* List of fields which are required in the query */
           INPUT "apiID,endPoint,isSSLEnabled,authType,requestVerb,clientID,Inactive", 
           /* List of fields which should be displayed in the browse */ 
           INPUT "apiID,clientID,endPoint,isSSLEnabled,authType,requestVerb,Inactive",
           /* List of field labels to override the default database field label */
           INPUT "API ID,Client ID, EndPoint, SSL Enabled, Auth Type, Request Verb, Inactive",
           /* List of field formats to override the default database field format */
           INPUT "X(30),X(30),X(60)",
           /* List of browse column width values to override the default column width in browse */
           INPUT "",
           /* List of fields for which field level search is enabled */
           INPUT "apiID,isSSLEnabled,authType,requestVerb,clientID,Inactive",
           /* List of fields for which sorting is enabled */
           INPUT "apiID",
           /* Where clause to select specific records */
           INPUT "APIOutbound.company EQ '" + ipcCompany + "'" ,
           /* List of fields for which the value is required to be returned when a row is selected in the browse */
           INPUT "apiID,clientID",
           /* Max record limit to prevent run away query */
           INPUT iRecordLimit,
           /* dynamic subject id */
           INPUT ipiSubjectID,
           /* dynamic user id */
           INPUT ipcUserID,
           /* dynamic parameter value id */
           INPUT ipiParamValueID,           
           /* Pipe separated list of return field values as output based on previous input list */
           OUTPUT opcReturnFields,
           /* Single return value which is to be returned from the lookup - this will populate in the field from where the lookup was opened */
           OUTPUT opcLookupField,
           /* RecID of the row selected when a row is selected in the browse */
           OUTPUT oprRecID) NO-ERROR.
END PROCEDURE.

PROCEDURE apiRoute: 
        RUN windows\l-lookup.w
           (/* Title of the lookup screen */
           INPUT "Inbound API ID",
           /* The source field for which the lookup screen is called for */
           INPUT "apiRoute",
           /* DB Table from which data is to be fetched */
           INPUT "APIInbound",
           /* List of fields which are required in the query */
           INPUT "apiRoute,requestVerb,requestDataType,canBeQueued,Inactive", 
           /* List of fields which should be displayed in the browse */ 
           INPUT "apiRoute,requestVerb,requestDataType,canBeQueued,Inactive",
           /* List of field labels to override the default database field label */
           INPUT "API Route,Request Verb,Request Data Type,Queued?,Inactive",
           /* List of field formats to override the default database field format */
           INPUT "X(40),X(20),X(20),YES/NO,YES/NO",
           /* List of browse column width values to override the default column width in browse */
           INPUT "",
           /* List of fields for which field level search is enabled */
           INPUT "apiRoute,requestVerb,requestDataType,canBeQueued,Inactive",
           /* List of fields for which sorting is enabled */
           INPUT "apiRoute",
           /* Where clause to select specific records */
           INPUT "" ,
           /* List of fields for which the value is required to be returned when a row is selected in the browse */
           INPUT "apiRoute",
           /* Max record limit to prevent run away query */
           INPUT iRecordLimit,
           /* dynamic subject id */
           INPUT ipiSubjectID,
           /* dynamic user id */
           INPUT ipcUserID,
           /* dynamic parameter value id */
           INPUT ipiParamValueID,           
           /* Pipe separated list of return field values as output based on previous input list */
           OUTPUT opcReturnFields,
           /* Single return value which is to be returned from the lookup - this will populate in the field from where the lookup was opened */
           OUTPUT opcLookupField,
           /* RecID of the row selected when a row is selected in the browse */
           OUTPUT oprRecID) NO-ERROR.
END PROCEDURE.

PROCEDURE cust-no:
       RUN windows\l-lookup.w
           (/* Title of the lookup screen */
           INPUT "Customer ID",
           /* The source field for which the lookup screen is called for */
           INPUT "cust-no",
           /* DB Table from which data is to be fetched */
           INPUT "cust",
           /* List of fields which are required in the query */
           INPUT "company,cust-no,name,city,state,type,sman,terr", 
           /* List of fields which should be displayed in the browse */ 
           INPUT "cust-no,name,city,state,type,sman,terr",
           /* List of field labels to override the default database field label */
           INPUT "Customer #,Customer Name,City,State,Customer Type,Sales Group,Territory",
           /* List of field formats to override the default database field format */
           INPUT ",X(30)",
           /* List of browse column width values to override the default column width in browse */
           INPUT ",40",
           /* List of fields for which field level search is enabled */
           INPUT "cust-no,name,city,state,type,sman,terr",
           /* List of fields for which sorting is enabled */
           INPUT "cust-no,name,city,state,type,sman,terr",
           /* Where clause to select specific records */
           INPUT "cust.company EQ '" + ipcCompany + "' AND cust.active NE 'I'" ,
           /* List of fields for which the value is required to be returned when a row is selected in the browse */
           INPUT "cust-no,name,city,state,type,sman,terr",
           /* Max record limit to prevent run away query */
           INPUT iRecordLimit,
           /* dynamic subject id */
           INPUT ipiSubjectID,
           /* dynamic user id */
           INPUT ipcUserID,
           /* dynamic parameter value id */
           INPUT ipiParamValueID,
           /* Pipe separated list of return field values as output based on previous input list */
           OUTPUT opcReturnFields,
           /* Single return value which is to be returned from the lookup - this will populate in the field from where the lookup was opened */
           OUTPUT opcLookupField,
           /* RecID of the row selected when a row is selected in the browse */
           OUTPUT oprRecID) NO-ERROR.
END PROCEDURE.

PROCEDURE dept:
       RUN windows\l-lookup.w
           (/* Title of the lookup screen */
           INPUT "Department Information",
           /* The source field for which the lookup screen is called for */
           INPUT "code",
           /* DB Table from which data is to be fetched */
           INPUT "dept",
           /* List of fields which are required in the query */
           INPUT "code,dscr", 
           /* List of fields which should be displayed in the browse */ 
           INPUT "code,dscr",
           /* List of field labels to override the default database field label */
           INPUT ",",
           /* List of field formats to override the default database field format */
           INPUT ",",
           /* List of browse column width values to override the default column width in browse */
           INPUT ",",
           /* List of fields for which field level search is enabled */
           INPUT "code,dscr",
           /* List of fields for which sorting is enabled */
           INPUT "code,dscr",
           /* Where clause to select specific records */
           INPUT "" ,
           /* List of fields for which the value is required to be returned when a row is selected in the browse */
           INPUT "code,dscr",
           /* Max record limit to prevent run away query */
           INPUT iRecordLimit,
           /* dynamic subject id */
           INPUT ipiSubjectID,
           /* dynamic user id */
           INPUT ipcUserID,
           /* dynamic parameter value id */
           INPUT ipiParamValueID,           
           /* Pipe separated list of return field values as output based on previous input list */
           OUTPUT opcReturnFields,
           /* Single return value which is to be returned from the lookup - this will populate in the field from where the lookup was opened */
           OUTPUT opcLookupField,
           /* RecID of the row selected when a row is selected in the browse */
           OUTPUT oprRecID) NO-ERROR.
END PROCEDURE.

PROCEDURE i-no:
       RUN windows\l-lookup.w
           (/* Title of the lookup screen */
           INPUT "Finished Good Item",
           /* The source field for which the lookup screen is called for */
           INPUT "i-no",
           /* DB Table from which data is to be fetched */
           INPUT "itemfg",
           /* List of fields which are required in the query */
           INPUT "company,i-no,i-name,cust-no,part-no,part-dscr1,part-dscr2,part-dscr3",
           /* List of fields which should be displayed in the browse */
           INPUT "i-no,i-name,cust-no,part-no,part-dscr1,part-dscr2,part-dscr3",
           /* List of field labels to override the default database field label */
           INPUT "Item #,Item Name,Customer,Cust Part #,Description 1,Description 2,Description 3",
           /* List of field formats to override the default database field format */
           INPUT "",
           /* List of browse column width values to override the default column width in browse */
           INPUT "20,40,,20",
           /* List of fields for which field level search is enabled */
           INPUT "i-no,i-name,cust-no,part-no,part-dscr1,part-dscr2,part-dscr3",
           /* List of fields for which sorting is enabled */
           INPUT "i-no,i-name,cust-no,part-no,part-dscr1,part-dscr2,part-dscr3",
           /* Where clause to select specific records */
           INPUT "itemfg.company EQ '" + ipcCompany + "' AND itemfg.stat EQ 'A'" ,
           /* List of fields for which the value is required to be returned when a row is selected in the browse */
           INPUT "i-no,i-name,cust-no,part-no,part-dscr1,part-dscr2,part-dscr3",
           /* Max record limit to prevent run away query */
           INPUT iRecordLimit,
           /* dynamic subject id */
           INPUT ipiSubjectID,
           /* dynamic user id */
           INPUT ipcUserID,
           /* dynamic parameter value id */
           INPUT ipiParamValueID,
           /* Pipe separated list of return field values as output based on previous input list */
           OUTPUT opcReturnFields,
           /* Single return value which is to be returned from the lookup - this will populate in the field from where the lookup was opened */
           OUTPUT opcLookupField,
           /* RecID of the row selected when a row is selected in the browse */
           OUTPUT oprRecID) NO-ERROR.
END PROCEDURE.

PROCEDURE item:
       RUN windows\l-lookup.w
           (/* Title of the lookup screen */
           INPUT "Raw Material Information",
           /* The source field for which the lookup screen is called for */
           INPUT "i-no",
           /* DB Table from which data is to be fetched */
           INPUT "item",
           /* List of fields which are required in the query */
           INPUT "company,i-no,i-name,i-dscr,i-code,mat-type,procat,cost-type",
           /* List of fields which should be displayed in the browse */
           INPUT "i-no,i-name,i-dscr,i-code,mat-type,procat,cost-type",
           /* List of field labels to override the default database field label */
           INPUT "Item #,Item Name,Description,Real,Material Type,Category,Cost Type",
           /* List of field formats to override the default database field format */
           INPUT ",,,",
           /* List of browse column width values to override the default column width in browse */
           INPUT "20,40,,",
           /* List of fields for which field level search is enabled */
           INPUT "i-no,i-name,i-dscr,i-code,mat-type,procat,cost-type",
           /* List of fields for which sorting is enabled */
           INPUT "i-no,i-name,i-dscr,i-code,mat-type,procat,cost-type",
           /* Where clause to select specific records */
           INPUT "item.company EQ '" + ipcCompany + "' " ,
           /* List of fields for which the value is required to be returned when a row is selected in the browse */
           INPUT "i-no,i-name,i-dscr,i-code,mat-type,procat,cost-type",
           /* Max record limit to prevent run away query */
           INPUT iRecordLimit,
           /* dynamic subject id */
           INPUT ipiSubjectID,
           /* dynamic user id */
           INPUT ipcUserID,
           /* dynamic parameter value id */
           INPUT ipiParamValueID,
           /* Pipe separated list of return field values as output based on previous input list */
           OUTPUT opcReturnFields,
           /* Single return value which is to be returned from the lookup - this will populate in the field from where the lookup was opened */
           OUTPUT opcLookupField,
           /* RecID of the row selected when a row is selected in the browse */
           OUTPUT oprRecID) NO-ERROR.
END PROCEDURE.

PROCEDURE itemfg:
       RUN windows\l-lookup.w
           (/* Title of the lookup screen */
           INPUT "Finished Good Item",
           /* The source field for which the lookup screen is called for */
           INPUT "i-no",
           /* DB Table from which data is to be fetched */
           INPUT "itemfg",
           /* List of fields which are required in the query */
           INPUT "company,i-no,i-name,part-dscr1,cust-no,est-no,style,procat,i-code,part-no,stat",
           /* List of fields which should be displayed in the browse */
           INPUT "i-no,i-name,part-dscr1,cust-no,est-no,style,procat,i-code,part-no,stat",
           /* List of field labels to override the default database field label */
           INPUT "Item #,Item Name",
           /* List of field formats to override the default database field format */
           INPUT "",
           /* List of browse column width values to override the default column width in browse */
           INPUT "20,,35,10,,,,,18",
           /* List of fields for which field level search is enabled */
           INPUT "i-no,i-name,part-dscr1,cust-no,est-no,style,procat,i-code,part-no,stat",
           /* List of fields for which sorting is enabled */
           INPUT "i-no,i-name,part-dscr1,cust-no,est-no,style,procat,i-code,part-no,stat",
           /* Where clause to select specific records */
           INPUT "itemfg.company EQ '" + ipcCompany + "' " ,
           /* List of fields for which the value is required to be returned when a row is selected in the browse */
           INPUT "i-no,i-name,part-dscr1,cust-no,est-no,style,procat,i-code,part-no,stat",
           /* Max record limit to prevent run away query */
           INPUT iRecordLimit,
           /* dynamic subject id */
           INPUT ipiSubjectID,
           /* dynamic user id */
           INPUT ipcUserID,
           /* dynamic parameter value id */
           INPUT ipiParamValueID,
           /* Pipe separated list of return field values as output based on previous input list */
           OUTPUT opcReturnFields,
           /* Single return value which is to be returned from the lookup - this will populate in the field from where the lookup was opened */
           OUTPUT opcLookupField,
           /* RecID of the row selected when a row is selected in the browse */
           OUTPUT oprRecID) NO-ERROR.
END PROCEDURE.

PROCEDURE job-no:
       RUN windows\l-lookup.w
           (/* Title of the lookup screen */
           INPUT "Job Information",
           /* The source field for which the lookup screen is called for */
           INPUT "job-no",
           /* DB Table from which data is to be fetched */
           INPUT "job-hdr",
           /* List of fields which are required in the query */
           INPUT "company,job-no,job-no2,frm,blank-no,i-no,est-no,ord-no,cust-no,opened,due-date,loc,due-time",
           /* List of fields which should be displayed in the browse */
           INPUT "job-no,job-no2,i-no,est-no,ord-no,cust-no,due-date,opened",
           /* List of field labels to override the default database field label */
           INPUT "Job #,Job #2,Item #,,Order #,,,Status",
           /* List of field formats to override the default database field format */
           INPUT "x(9),>9,X(20),X(10),>>>>>9,X(10)",
           /* List of browse column width values to override the default column width in browse */
           INPUT "9,8,20,10,,10,12",
           /* List of fields for which field level search is enabled */
           INPUT "job-no,job-no2,i-no,opened,due-date",
           /* List of fields for which sorting is enabled */
           INPUT "job-no,i-no,est-no,ord-no,cust-no",
           /* Where clause to select specific records */
           INPUT "job-hdr.company EQ '" + ipcCompany + "'",
           /* List of fields for which the value is required to be returned when a row is selected in the browse */
           INPUT "company,job-no,job-no2,frm,blank-no,i-no,est-no,ord-no,cust-no,opened,due-date",
           /* Max record limit to prevent run away query */
           INPUT iRecordLimit,
           /* dynamic subject id */
           INPUT ipiSubjectID,
           /* dynamic user id */
           INPUT ipcUserID,
           /* dynamic parameter value id */
           INPUT ipiParamValueID,
           /* Pipe separated list of return field values as output based on previous input list */
           OUTPUT opcReturnFields,
           /* Single return value which is to be returned from the lookup - this will populate in the field from where the lookup was opened */
           OUTPUT opcLookupField,
           /* RecID of the row selected when a row is selected in the browse */
           OUTPUT oprRecID) NO-ERROR.
END PROCEDURE.

PROCEDURE naics:
       RUN windows\l-lookup.w
           (/* Title of the lookup screen */
           INPUT "naics",
           /* The source field for which the lookup screen is called for */
           INPUT "naicsID",
           /* DB Table from which data is to be fetched */
           INPUT "naics",
           /* List of fields which are required in the query */
           INPUT "naicsID,description,Inactive",
           /* List of fields which should be displayed in the browse */
           INPUT "naicsID,description,Inactive",
           /* List of field labels to override the default database field label */
           INPUT "NAICS ID,Description",
           /* List of field formats to override the default database field format */
           INPUT "",
           /* List of browse column width values to override the default column width in browse */
           INPUT "6,,,",
           /* List of fields for which field level search is enabled */
           INPUT "naicsID,description,Inactive",
           /* List of fields for which sorting is enabled */
           INPUT "naicsID,description,Inactive",
           /* Where clause to select specific records */
           INPUT "" ,
           /* List of fields for which the value is required to be returned when a row is selected in the browse */
           INPUT "naicsID,description,Inactive",
           /* Max record limit to prevent run away query */
           INPUT iRecordLimit,
           /* dynamic subject id */
           INPUT ipiSubjectID,
           /* dynamic user id */
           INPUT ipcUserID,
           /* dynamic parameter value id */
           INPUT ipiParamValueID,
           /* Pipe separated list of return field values as output based on previous input list */
           OUTPUT opcReturnFields,
           /* Single return value which is to be returned from the lookup - this will populate in the field from where the lookup was opened */
           OUTPUT opcLookupField,
           /* RecID of the row selected when a row is selected in the browse */
           OUTPUT oprRecID) NO-ERROR.
END PROCEDURE.

PROCEDURE po-line:
       RUN windows\l-lookup.w
           (/* Title of the lookup screen */
           INPUT "Purchase Order Line Number",
           /* The source field for which the lookup screen is called for */
           INPUT "line",
           /* DB Table from which data is to be fetched */
           INPUT "po-ordl",
           /* List of fields which are required in the query */
           INPUT "company,po-no,line,i-no,stat", 
           /* List of fields which should be displayed in the browse */ 
           INPUT "po-no,line,i-no,stat",
           /* List of field labels to override the default database field label */
           INPUT ",,,Status",
           /* List of field formats to override the default database field format */
           INPUT "",
           /* List of browse column width values to override the default column width in browse */
           INPUT "20,7,40",
           /* List of fields for which field level search is enabled */
           INPUT "po-no,line,i-no,stat",
           /* List of fields for which sorting is enabled */
           INPUT "po-no,i-no,stat",
           /* Where clause to select specific records */
           INPUT "po-ordl.company EQ '" + ipcCompany + "'" ,
           /* List of fields for which the value is required to be returned when a row is selected in the browse */
           INPUT "po-no,line",
           /* Max record limit to prevent run away query */
           INPUT iRecordLimit,
           /* dynamic subject id */
           INPUT ipiSubjectID,
           /* dynamic user id */
           INPUT ipcUserID,
           /* dynamic parameter value id */
           INPUT ipiParamValueID,           
           /* Pipe separated list of return field values as output based on previous input list */
           OUTPUT opcReturnFields,
           /* Single return value which is to be returned from the lookup - this will populate in the field from where the lookup was opened */
           OUTPUT opcLookupField,
           /* RecID of the row selected when a row is selected in the browse */
           OUTPUT oprRecID) NO-ERROR.
END PROCEDURE.

PROCEDURE po-no:
       RUN windows\l-lookup.w
           (/* Title of the lookup screen */
           INPUT "Purchase Order Number",
           /* The source field for which the lookup screen is called for */
           INPUT "po-no",
           /* DB Table from which data is to be fetched */
           INPUT "po-ord",
           /* List of fields which are required in the query */
           INPUT "company,po-no,po-date,vend-no,cust-no,opened", 
           /* List of fields which should be displayed in the browse */ 
           INPUT "po-no,po-date,vend-no,opened",
           /* List of field labels to override the default database field label */
           INPUT ",,,Status",
           /* List of field formats to override the default database field format */
           INPUT "",
           /* List of browse column width values to override the default column width in browse */
           INPUT "20,20,30",
           /* List of fields for which field level search is enabled */
           INPUT "po-no,po-date,vend-no,opened",
           /* List of fields for which sorting is enabled */
           INPUT "po-no,po-date,vend-no,opened",
           /* Where clause to select specific records */
           INPUT "po-ord.company EQ '" + ipcCompany + "'" ,
           /* List of fields for which the value is required to be returned when a row is selected in the browse */
           INPUT "po-no",
           /* Max record limit to prevent run away query */
           INPUT iRecordLimit,
           /* dynamic subject id */
           INPUT ipiSubjectID,
           /* dynamic user id */
           INPUT ipcUserID,
           /* dynamic parameter value id */
           INPUT ipiParamValueID,           
           /* Pipe separated list of return field values as output based on previous input list */
           OUTPUT opcReturnFields,
           /* Single return value which is to be returned from the lookup - this will populate in the field from where the lookup was opened */
           OUTPUT opcLookupField,
           /* RecID of the row selected when a row is selected in the browse */
           OUTPUT oprRecID) NO-ERROR.
END PROCEDURE.

PROCEDURE procat:
       RUN windows\l-lookup.w
           (/* Title of the lookup screen */
           INPUT "FG Product Category Information",
           /* The source field for which the lookup screen is called for */
           INPUT "procat",
           /* DB Table from which data is to be fetched */
           INPUT "fgcat",
           /* List of fields which are required in the query */
           INPUT "company,procat,dscr", 
           /* List of fields which should be displayed in the browse */ 
           INPUT "procat,dscr",
           /* List of field labels to override the default database field label */
           INPUT ",",
           /* List of field formats to override the default database field format */
           INPUT ",",
           /* List of browse column width values to override the default column width in browse */
           INPUT ",",
           /* List of fields for which field level search is enabled */
           INPUT "procat,dscr",
           /* List of fields for which sorting is enabled */
           INPUT "procat,dscr",
           /* Where clause to select specific records */
           INPUT "fgcat.company EQ '" + ipcCompany + "'" ,
           /* List of fields for which the value is required to be returned when a row is selected in the browse */
           INPUT "procat",
           /* Max record limit to prevent run away query */
           INPUT iRecordLimit,
           /* dynamic subject id */
           INPUT ipiSubjectID,
           /* dynamic user id */
           INPUT ipcUserID,
           /* dynamic parameter value id */
           INPUT ipiParamValueID,           
           /* Pipe separated list of return field values as output based on previous input list */
           OUTPUT opcReturnFields,
           /* Single return value which is to be returned from the lookup - this will populate in the field from where the lookup was opened */
           OUTPUT opcLookupField,
           /* RecID of the row selected when a row is selected in the browse */
           OUTPUT oprRecID) NO-ERROR.
END PROCEDURE.
 
PROCEDURE release#:
       RUN windows\l-lookup.w
           (/* Title of the lookup screen */
           INPUT "Release Number",
           /* The source field for which the lookup screen is called for */
           INPUT "release#",
           /* DB Table from which data is to be fetched */
           INPUT "oe-relh",
           /* List of fields which are required in the query */
           INPUT "company,release#,rel-date,posted,stat,cust-no,ship-id,carrier", 
           /* List of fields which should be displayed in the browse */ 
           INPUT "release#,posted,stat,cust-no,ship-id,rel-date,carrier",
           /* List of field labels to override the default database field label */
           INPUT ",,,,,Release Date",
           /* List of field formats to override the default database field format */
           INPUT ",YES/NO",
           /* List of browse column width values to override the default column width in browse */
           INPUT "15,10,15,16,16",
           /* List of fields for which field level search is enabled */
           INPUT "release#,rel-date,posted,cust-no,ship-id",
           /* List of fields for which sorting is enabled */
           INPUT "release#,rel-date,posted,stat",
           /* Where clause to select specific records */
           INPUT "oe-relh.company EQ '" + ipcCompany + "' AND oe-relh.release# GT 0",
           /* List of fields for which the value is required to be returned when a row is selected in the browse */
           INPUT "release#",
           /* Max record limit to prevent run away query */
           INPUT iRecordLimit,
           /* dynamic subject id */
           INPUT ipiSubjectID,
           /* dynamic user id */
           INPUT ipcUserID,
           /* dynamic parameter value id */
           INPUT ipiParamValueID,           
           /* Pipe separated list of return field values as output based on previous input list */
           OUTPUT opcReturnFields,
           /* Single return value which is to be returned from the lookup - this will populate in the field from where the lookup was opened */
           OUTPUT opcLookupField,
           /* RecID of the row selected when a row is selected in the browse */
           OUTPUT oprRecID) NO-ERROR.
END PROCEDURE.

PROCEDURE triggerID: 
        RUN windows\l-lookup.w
           (/* Title of the lookup screen */
           INPUT "Outbound API Triggers",
           /* The source field for which the lookup screen is called for */
           INPUT "triggerID",
           /* DB Table from which data is to be fetched */
           INPUT "APIOutboundTrigger",
           /* List of fields which are required in the query */
           INPUT "triggerID,description,apiID,clientID,Inactive", 
           /* List of fields which should be displayed in the browse */ 
           INPUT "triggerID,description,apiID,clientID,Inactive",
           /* List of field labels to override the default database field label */
           INPUT "Trigger ID, Description,API ID,Client ID, Inactive",
           /* List of field formats to override the default database field format */
           INPUT "X(32),X(60)",
           /* List of browse column width values to override the default column width in browse */
           INPUT "",
           /* List of fields for which field level search is enabled */
           INPUT "triggerID,apiID,clientID,Inactive",
           /* List of fields for which sorting is enabled */
           INPUT "triggerID",
           /* Where clause to select specific records */
           INPUT "APIOutboundTrigger.company EQ '" + ipcCompany + "'" ,
           /* List of fields for which the value is required to be returned when a row is selected in the browse */
           INPUT "triggerID,apiID,clientID",
           /* Max record limit to prevent run away query */
           INPUT iRecordLimit,
           /* dynamic subject id */
           INPUT ipiSubjectID,
           /* dynamic user id */
           INPUT ipcUserID,
           /* dynamic parameter value id */
           INPUT ipiParamValueID,           
           /* Pipe separated list of return field values as output based on previous input list */
           OUTPUT opcReturnFields,
           /* Single return value which is to be returned from the lookup - this will populate in the field from where the lookup was opened */
           OUTPUT opcLookupField,
           /* RecID of the row selected when a row is selected in the browse */
           OUTPUT oprRecID) NO-ERROR.
END PROCEDURE.

PROCEDURE users-manager:
       RUN windows\l-lookup.w
           (/* Title of the lookup screen */
           INPUT "Users Manager Information",
           /* The source field for which the lookup screen is called for */
           INPUT "user_id",
           /* DB Table from which data is to be fetched */
           INPUT "users",
           /* List of fields which are required in the query */
           INPUT "user_id,user_name,phone,userType,isManager", 
           /* List of fields which should be displayed in the browse */ 
           INPUT "user_id,user_name,phone,userType,isManager",
           /* List of field labels to override the default database field label */
           INPUT ",",
           /* List of field formats to override the default database field format */
           INPUT ",",
           /* List of browse column width values to override the default column width in browse */
           INPUT ",",
           /* List of fields for which field level search is enabled */
           INPUT "user_id,user_name,phone,userType",
           /* List of fields for which sorting is enabled */
           INPUT "user_id,user_name,phone,userType",
           /* Where clause to select specific records */
           INPUT "users.isManager EQ Yes " ,
           /* List of fields for which the value is required to be returned when a row is selected in the browse */
           INPUT "user_id,user_name",
           /* Max record limit to prevent run away query */
           INPUT iRecordLimit,
           /* dynamic subject id */
           INPUT ipiSubjectID,
           /* dynamic user id */
           INPUT ipcUserID,
           /* dynamic parameter value id */
           INPUT ipiParamValueID,           
           /* Pipe separated list of return field values as output based on previous input list */
           OUTPUT opcReturnFields,
           /* Single return value which is to be returned from the lookup - this will populate in the field from where the lookup was opened */
           OUTPUT opcLookupField,
           /* RecID of the row selected when a row is selected in the browse */
           OUTPUT oprRecID) NO-ERROR.
END PROCEDURE.

PROCEDURE vend-no:
       RUN windows\l-lookup.w
           (/* Title of the lookup screen */
           INPUT "Vendor ID",
           /* The source field for which the lookup screen is called for */
           INPUT "vend-no",
           /* DB Table from which data is to be fetched */
           INPUT "vend",
           /* List of fields which are required in the query */
           INPUT "company,vend-no,name,city,state,active,type,loc", 
           /* List of fields which should be displayed in the browse */ 
           INPUT "vend-no,name,city,state,type,loc",
           /* List of field labels to override the default database field label */
           INPUT "",
           /* List of field formats to override the default database field format */
           INPUT "X(20),X(50)",
           /* List of browse column width values to override the default column width in browse */
           INPUT "",
           /* List of fields for which field level search is enabled */
           INPUT "vend-no,name,city,state",
           /* List of fields for which sorting is enabled */
           INPUT "vend-no,name,city,state",
           /* Where clause to select specific records */
           INPUT "vend.company EQ '" + ipcCompany + "'" + " AND vend.active EQ 'A'",
           /* List of fields for which the value is required to be returned when a row is selected in the browse */
           INPUT "vend-no,name",
           /* Max record limit to prevent run away query */
           INPUT iRecordLimit,
           /* dynamic subject id */
           INPUT ipiSubjectID,
           /* dynamic user id */
           INPUT ipcUserID,
           /* dynamic parameter value id */
           INPUT ipiParamValueID,           
           /* Pipe separated list of return field values as output based on previous input list */
           OUTPUT opcReturnFields,
           /* Single return value which is to be returned from the lookup - this will populate in the field from where the lookup was opened */
           OUTPUT opcLookupField,
           /* RecID of the row selected when a row is selected in the browse */
           OUTPUT oprRecID) NO-ERROR.
END PROCEDURE.
