/*------------------------------------------------------------------------
  File: system\openlookup.p
  Description: This is a wrapper program to call generic dynamic lookup
               windows\l-lookup.w
  Input Parameters:
    ip-company     :The company code
    ip-lookupField :The source field for which the lookup screen is called for
  Output Parameters:
    op-returnFields:Pipe separated list of return field values as output 
                    based on previous input list
    op-lookupField :Single return value which is to be returned from 
                    the lookup - this will populate in the field from 
                    where the lookup was opened
    op-recVal      :RecID of the row selected when a row is selected 
                    in the browse
  Author: Mithun Porandla
  Created: 13th March 2019
------------------------------------------------------------------------*/

DEFINE INPUT  PARAMETER ip-company      AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ip-lookupField  AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ip-subjectID    AS INTEGER   NO-UNDO.
DEFINE INPUT  PARAMETER ip-userid       AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ip-paramValueID AS INTEGER   NO-UNDO.

DEFINE OUTPUT PARAMETER op-returnFields AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER op-lookupField  AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER op-recval       AS RECID     NO-UNDO.

DEFINE VARIABLE iRecordLimit AS INTEGER NO-UNDO INITIAL 15000.

IF ip-userid EQ "" THEN
ip-userid = "_default".
IF ip-subjectID GT 0 AND
   CAN-FIND(FIRST dynParamValue
            WHERE dynParamValue.subjectID    EQ ip-subjectID
              AND dynParamValue.user-id      EQ ip-userid
              AND dynParamValue.paramValueID EQ ip-paramValueID) THEN DO:
    RUN AOA/dynLookup.p (
        ip-company,
        ip-subjectID,
        ip-userid,
        ip-paramValueID,
        OUTPUT op-returnFields,
        OUTPUT op-lookupField,
        OUTPUT op-recVal
        ).
END. /* if can-find */
ELSE
CASE ip-lookupField:
   WHEN "cust-no" THEN
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
           INPUT "cust.company EQ '" + ip-company + "' AND cust.active NE 'I'" ,
           /* List of fields for which the value is required to be returned when a row is selected in the browse */
           INPUT "cust-no,name,city,state,type,sman,terr",
           /* Max record limit to prevent run away query */
           INPUT iRecordLimit,
           /* dynamic subject id */
           INPUT ip-subjectID,
           /* dynamic user id */
           INPUT ip-userid,
           /* dynamic parameter value id */
           INPUT ip-paramValueID,
           /* Pipe separated list of return field values as output based on previous input list */
           OUTPUT op-returnFields,
           /* Single return value which is to be returned from the lookup - this will populate in the field from where the lookup was opened */
           OUTPUT op-lookupField,
           /* RecID of the row selected when a row is selected in the browse */
           OUTPUT op-recVal) NO-ERROR.
  WHEN "i-no" THEN
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
           INPUT "itemfg.company EQ '" + ip-company + "' AND itemfg.stat EQ 'A'" ,
           /* List of fields for which the value is required to be returned when a row is selected in the browse */
           INPUT "i-no,i-name,cust-no,part-no,part-dscr1,part-dscr2,part-dscr3",
           /* Max record limit to prevent run away query */
           INPUT iRecordLimit,
           /* dynamic subject id */
           INPUT ip-subjectID,
           /* dynamic user id */
           INPUT ip-userid,
           /* dynamic parameter value id */
           INPUT ip-paramValueID,
           /* Pipe separated list of return field values as output based on previous input list */
           OUTPUT op-returnFields,
           /* Single return value which is to be returned from the lookup - this will populate in the field from where the lookup was opened */
           OUTPUT op-lookupField,
           /* RecID of the row selected when a row is selected in the browse */
           OUTPUT op-recVal) NO-ERROR.
   WHEN "job-no" THEN
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
           INPUT "job-hdr.company EQ '" + ip-company + "'",
           /* List of fields for which the value is required to be returned when a row is selected in the browse */
           INPUT "company,job-no,job-no2,frm,blank-no,i-no,est-no,ord-no,cust-no,opened,due-date",
           /* Max record limit to prevent run away query */
           INPUT iRecordLimit,
           /* dynamic subject id */
           INPUT ip-subjectID,
           /* dynamic user id */
           INPUT ip-userid,
           /* dynamic parameter value id */
           INPUT ip-paramValueID,
           /* Pipe separated list of return field values as output based on previous input list */
           OUTPUT op-returnFields,
           /* Single return value which is to be returned from the lookup - this will populate in the field from where the lookup was opened */
           OUTPUT op-lookupField,
           /* RecID of the row selected when a row is selected in the browse */
           OUTPUT op-recVal) NO-ERROR.
    WHEN "apiID" THEN 
        RUN windows\l-lookup.w
           (/* Title of the lookup screen */
           INPUT "Outbound API ID",
           /* The source field for which the lookup screen is called for */
           INPUT "apiID",
           /* DB Table from which data is to be fetched */
           INPUT "APIOutbound",
           /* List of fields which are required in the query */
           INPUT "apiID,endPoint,isSSLEnabled,authType,requestVerb,clientID,isActive", 
           /* List of fields which should be displayed in the browse */ 
           INPUT "apiID,endPoint,isSSLEnabled,authType,requestVerb,clientID,isActive",
           /* List of field labels to override the default database field label */
           INPUT "",
           /* List of field formats to override the default database field format */
           INPUT "X(30),X(60)",
           /* List of browse column width values to override the default column width in browse */
           INPUT "",
           /* List of fields for which field level search is enabled */
           INPUT "apiID,isSSLEnabled,authType,requestVerb,clientID,isActive",
           /* List of fields for which sorting is enabled */
           INPUT "apiID",
           /* Where clause to select specific records */
           INPUT "" ,
           /* List of fields for which the value is required to be returned when a row is selected in the browse */
           INPUT "apiID",
           /* Max record limit to prevent run away query */
           INPUT iRecordLimit,
           /* dynamic subject id */
           INPUT ip-subjectID,
           /* dynamic user id */
           INPUT ip-userid,
           /* dynamic parameter value id */
           INPUT ip-paramValueID,           
           /* Pipe separated list of return field values as output based on previous input list */
           OUTPUT op-returnFields,
           /* Single return value which is to be returned from the lookup - this will populate in the field from where the lookup was opened */
           OUTPUT op-lookupField,
           /* RecID of the row selected when a row is selected in the browse */
           OUTPUT op-recVal
           ).
   WHEN "vend-no" THEN
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
           INPUT "vend.company EQ '" + ip-company + "'" + " AND vend.active EQ 'A'",
           /* List of fields for which the value is required to be returned when a row is selected in the browse */
           INPUT "vend-no,name",
           /* Max record limit to prevent run away query */
           INPUT iRecordLimit,
           /* dynamic subject id */
           INPUT ip-subjectID,
           /* dynamic user id */
           INPUT ip-userid,
           /* dynamic parameter value id */
           INPUT ip-paramValueID,           
           /* Pipe separated list of return field values as output based on previous input list */
           OUTPUT op-returnFields,
           /* Single return value which is to be returned from the lookup - this will populate in the field from where the lookup was opened */
           OUTPUT op-lookupField,
           /* RecID of the row selected when a row is selected in the browse */
           OUTPUT op-recVal) NO-ERROR.
   WHEN "po-no" THEN
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
           INPUT "po-no|DESCENDING,po-date,vend-no,opened",
           /* Where clause to select specific records */
           INPUT "po-ord.company EQ '" + ip-company + "'" ,
           /* List of fields for which the value is required to be returned when a row is selected in the browse */
           INPUT "po-no",
           /* Max record limit to prevent run away query */
           INPUT iRecordLimit,
           /* dynamic subject id */
           INPUT ip-subjectID,
           /* dynamic user id */
           INPUT ip-userid,
           /* dynamic parameter value id */
           INPUT ip-paramValueID,           
           /* Pipe separated list of return field values as output based on previous input list */
           OUTPUT op-returnFields,
           /* Single return value which is to be returned from the lookup - this will populate in the field from where the lookup was opened */
           OUTPUT op-lookupField,
           /* RecID of the row selected when a row is selected in the browse */
           OUTPUT op-recVal) NO-ERROR.
   WHEN "release#" THEN
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
           INPUT "release#|DESCENDING,rel-date,posted,stat",
           /* Where clause to select specific records */
           INPUT "oe-relh.company EQ '" + ip-company + "'" ,
           /* List of fields for which the value is required to be returned when a row is selected in the browse */
           INPUT "release#",
           /* Max record limit to prevent run away query */
           INPUT iRecordLimit,
           /* dynamic subject id */
           INPUT ip-subjectID,
           /* dynamic user id */
           INPUT ip-userid,
           /* dynamic parameter value id */
           INPUT ip-paramValueID,           
           /* Pipe separated list of return field values as output based on previous input list */
           OUTPUT op-returnFields,
           /* Single return value which is to be returned from the lookup - this will populate in the field from where the lookup was opened */
           OUTPUT op-lookupField,
           /* RecID of the row selected when a row is selected in the browse */
           OUTPUT op-recVal) NO-ERROR.
END CASE.
