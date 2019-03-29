/*------------------------------------------------------------------------

  File: util\openlookup.p

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

DEFINE INPUT  PARAMETER ip-company         AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ip-lookupField     AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER op-returnFields    AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER op-lookupField     AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER op-recval          AS RECID     NO-UNDO.

CASE ip-lookupField:
   WHEN "job-no" THEN
       RUN windows\l-lookup.w
           (/* Title of the lookup screen */
           INPUT "Job Information",
           /* The source field for which the lookup screen is called for */
           INPUT "job-no",
           /* DB Table from which data is to be fetched */
           INPUT "job-hdr",
           /* List of fields which are required in the query */
           INPUT "company,job-no,job-no2,i-no,est-no,ord-no,cust-no,opened,due-date,loc,due-time",
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
           INPUT "company,job-no,job-no2,i-no,est-no,ord-no,cust-no,opened,due-date",
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
           /* Pipe separated list of return field values as output based on previous input list */
           OUTPUT op-returnFields,
           /* Single return value which is to be returned from the lookup - this will populate in the field from where the lookup was opened */
           OUTPUT op-lookupField,
           /* RecID of the row selected when a row is selected in the browse */
           OUTPUT op-recVal) NO-ERROR.
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
           /* Pipe separated list of return field values as output based on previous input list */
           OUTPUT op-returnFields,
           /* Single return value which is to be returned from the lookup - this will populate in the field from where the lookup was opened */
           OUTPUT op-lookupField,
           /* RecID of the row selected when a row is selected in the browse */
           OUTPUT op-recVal) NO-ERROR.
END CASE.
		
