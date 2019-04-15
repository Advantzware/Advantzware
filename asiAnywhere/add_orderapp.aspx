<%@ Page Language="C#" AutoEventWireup="true" Debug="false" Inherits="add_orderapp" Codebehind="add_orderapp.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">

<html xmlns="http://www.w3.org/1999/xhtml" >
<head id="Head1" runat="server">
    <title>Order Entry : Add Order</title>    
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
     <link href="include/tree.css" rel="stylesheet" type="text/css" />
    
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
     
     <link rel="stylesheet" type="text/css" href="include/help.css">
     
     
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    <script language = "JavaScript" src="include/CalendarControl.js"> 
      
    </script>              
      
    
    <script language="javascript">

//        window.onload = setfocus;
//        function setfocus() {

//            if (document.getElementById("FormView1_VCustomerTextBox")) {
//                var cust = document.getElementById("FormView1_VCustomerTextBox");
//                cust.focus();
//            }
//        }

        function NumberOnly() {
            var AsciiValue = event.keyCode
            if ((AsciiValue >= 48 && AsciiValue <= 57) || (AsciiValue == 8 || AsciiValue == 127))
                event.returnValue = true;
            else
                event.returnValue = false;
        }
        
        function focusonsave() {
            if (event.keyCode == 13) {
                var save = document.getElementById("FormView1_InsertButton");
                save.focus();
            }
        }
        function focusval(obj) {
            obj.style.backgroundColor = 'blue';
            obj.style.color = 'white';
        }
        function blurval(obj) {
            obj.style.backgroundColor = 'Window';
            obj.style.color = 'WindowText';
        }
        function preLeave(fieldObj, fieldType, fieldFormat) {
            fieldObj.style.backgroundColor = 'Window';
            fieldObj.style.color = 'WindowText';
            fieldType = fieldType.toLowerCase();

            if ((fieldType == "") || (fieldType == "text")) {
                leaveField(fieldObj);
            }

            if (fieldType == "date") {
                if (fieldFormat == "") {
                    var dateFormat = "99/99/9999";
                } else { var dateFormat = fieldFormat; }
                checkDate(dateFormat, fieldObj, '01/01/1950', '12/31/3000', 0);
            }

            if (fieldType == "number") {
                if (fieldFormat == "") {
                    var numFormat = "(>>>>9)";
                } else { var numFormat = fieldFormat; }
                checkNum(numFormat, fieldObj, '?', '?', 0);
            }
        }

        function preEnter(fieldObj, canEdit) {
            fieldObj.style.backgroundColor = 'blue';
            fieldObj.style.color = 'white';
            if (canEdit == "no") {
                fieldObj.blur();
                leaveField(fieldObj);
            }

            enterField(fieldObj);
            return;
        }
        function dateshowcal() {
            var date = document.getElementById("FormView1_VOrdateLabel");
            date.style.backgroundColor = 'blue';
            date.style.color = 'white';
            showCalendarControl(date);
        }
        function duedatecal() {
            var date = document.getElementById("FormView1_VDueDateTextBox");
            date.style.backgroundColor = 'blue';
            date.style.color = 'white';
            showCalendarControl(date);
        }
        function expdatecal() {
            var date = document.getElementById("FormView1_VcExpTextBox");
            date.style.backgroundColor = 'blue';
            date.style.color = 'white';
            showCalendarControl(date);
        }
        function dateblurCal() {

            var date = document.getElementById("FormView1_VOrdateLabel");
            var due = document.getElementById("FormView1_DropDownList1");
            date.style.backgroundColor = 'Window';
            date.style.color = 'WindowText';
            due.focus();
        }
        function duedateblurcal() {
            var date = document.getElementById("FormView1_VDueDateTextBox");
            var pono = document.getElementById("FormView1_VPonumTextBox");
            date.style.backgroundColor = 'Window';
            date.style.color = 'WindowText';
            pono.focus();
        }
        function expdateblurcal() {
            var date = document.getElementById("FormView1_VcExpTextBox");
            var account = document.getElementById("FormView1_VCnumTextBox");
            date.style.backgroundColor = 'Window';
            date.style.color = 'WindowText';
            account.focus();
        }


        function underblurval(obj) {
            obj.style.backgroundColor = 'Window';
            obj.style.color = 'WindowText';

            var tax = document.getElementById("FormView1_VTaxgrTextBox");
            var pay = document.getElementById("FormView1_VTermsTextBox");
            var frt = document.getElementById("FormView1_RD1");
            var ptype = document.getElementById("FormView1_VCtypeTextBox");

            if (tax.disabled == true && pay.disabled == true && frt.disabled == true) {
                ptype.focus();
            }
        }
        function updateunderblurval(obj) {
            obj.style.backgroundColor = 'Window';
            obj.style.color = 'WindowText';
            var tax = document.getElementById("FormView1_VTaxgrTextBox");
            var pay = document.getElementById("FormView1_VTermsTextBox");
            var frt = document.getElementById("FormView1_RD1");
            var acc = document.getElementById("FormView1_VCnumTextBox");

            if (tax.disabled == true && pay.disabled == true && frt.disabled == true) {
                acc.focus();
            }
        }

        function contactblurval(obj) {
            obj.style.backgroundColor = 'Window';
            obj.style.color = 'WindowText';

            var sold = document.getElementById("FormView1_VSoldTextBox");
            var due = document.getElementById("FormView1_DropDownList1");

            if (sold.disabled == true) {
                due.focus();
            }

        }
        function quoteblurval(obj) {
            obj.style.backgroundColor = 'Window';
            obj.style.color = 'WindowText';

            if (document.getElementById("FormView1_RfqTextBox")) {
                var quote = document.getElementById("FormView1_RfqTextBox");

                if (quote.value != "" && quote.value != "0") {
                    var bill = document.getElementById("FormView1_VCustomerTextBox");
                    var soldto = document.getElementById("FormView1_VSoldTextBox");
                    var bill_img = document.getElementById("FormView1_CustomerLook");
                    var sold_img = document.getElementById("FormView1_Image13");

                    document.forms[0].Hiddensold.value = soldto.value;

                    bill.disabled = true;
                    soldto.disabled = true;

                    bill_img.style.visibility = 'hidden';
                    sold_img.style.visibility = 'hidden';

                    if (bill.disabled == true) {
                        var contact = document.getElementById("FormView1_VContactTextBox");
                        contact.focus();
                    }

                    //var NewWindow = window.open("quantity_lookup.aspx?est="+quote.value+"","typeordLookupWindow","width=500,height=200,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
                }
                else {
                    var quote = document.getElementById("FormView1_RfqTextBox");
                    var bill = document.getElementById("FormView1_VCustomerTextBox");
                    if (document.getElementById("FormView1_estimateTextBox") && (quote.value == "" || quote.value == "0")) {
                        var est = document.getElementById("FormView1_estimateTextBox");
                        est.focus();
                    }
                    if (bill.disabled != true) {
                        var bill = document.getElementById("FormView1_VCustomerTextBox");
                        bill.focus();
                    }
                    else {
                        var contact = document.getElementById("FormView1_VContactTextBox");
                        contact.focus();
                    }

                }

            }
        }
        function refblurval(obj) {
            obj.style.backgroundColor = 'Window';
            obj.style.color = 'WindowText';
            //    if(document.getElementById("FormView1_DropDownList2"))
            //    {
            //        var type=document.getElementById("FormView1_DropDownList2");
            //        type.focus();
            //    }
            if (document.getElementById("FormView1_RfqTextBox")) {
                var quote = document.getElementById("FormView1_RfqTextBox");
                quote.focus();
            }
            else if (document.getElementById("FormView1_VContactTextBox")) {
                var contact = document.getElementById("FormView1_VContactTextBox");
                contact.focus();
            }
        }


        function accblurval(obj) {
            obj.style.backgroundColor = 'Window';
            obj.style.color = 'WindowText';
            if (document.getElementById("FormView1_VCauthTextBox").disabled == true) {
                var cont = document.getElementById("FormView1_VContactTextBox");
                cont.focus();
            }
        }


        function duedateval() {
            var duedate = document.getElementById("FormView1_VDueDateTextBox").value;

            if (duedate.length > 1 && duedate.length < 3 && duedate.indexOf('/') != 1) {
                document.getElementById("FormView1_VDueDateTextBox").value = duedate + "/";
            }
            if (duedate.length > 4 && duedate.length < 6 && duedate.indexOf('/') != 3) {
                document.getElementById("FormView1_VDueDateTextBox").value = duedate + "/";
            }
        }
        function expdateval() {
            var duedate = document.getElementById("FormView1_VcExpTextBox").value;

            if (duedate.length > 1 && duedate.length < 3 && duedate.indexOf('/') != 1) {
                document.getElementById("FormView1_VcExpTextBox").value = duedate + "/";
            }
            if (duedate.length > 4 && duedate.length < 6 && duedate.indexOf('/') != 3) {
                document.getElementById("FormView1_VcExpTextBox").value = duedate + "/";
            }
        }

        function orderdateval() {


            var duedate = document.getElementById("FormView1_VOrdateLabel").value;

            if (duedate.length > 1 && duedate.length < 3 && duedate.indexOf('/') != 1) {
                document.getElementById("FormView1_VOrdateLabel").value = duedate + "/";
            }
            if (duedate.length > 4 && duedate.length < 6 && duedate.indexOf('/') != 3) {
                document.getElementById("FormView1_VOrdateLabel").value = duedate + "/";
            }

        }
        function setcust(obj) {
            obj.style.backgroundColor = 'Window';
            obj.style.color = 'WindowText';            
        }
        function setsave(obj) {
            obj.style.backgroundColor = 'Window';
            obj.style.color = 'WindowText';            
        }


        function contactcustomerlook() {
            var NewWindow = window.open("bill_customer_lookup.aspx", "ContactCustomerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }

        function BillCustomerLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11, ReturnObj12, ReturnObj13, ReturnObj14, ReturnObj15, ReturnObj16, ReturnObj17, ReturnObj18, ReturnObj19, ReturnObj20, ReturnObj21, ReturnObj22, ReturnObj23, ReturnObj24, ReturnObj25, ReturnObj26, ReturnObj27, ReturnObj28, ReturnObj29, ReturnObj30, ReturnObj31, ReturnObj32, ReturnObj33, ReturnObj34, ReturnObj35) {
            document.forms[0].FormView1_VCustomerTextBox.value = ReturnObj1;           
            document.forms[0].FormView1_VSoldTextBox.value = ReturnObj1;            
            document.getElementById('FormView1_VCustomerTextBox').focus();


            if (ReturnObj14 == "Prepaid") {
                document.forms[0].FormView1_RD1.checked = true;
            }
            else if (ReturnObj14 == "Bill") {
                document.forms[0].FormView1_RD2.checked = true;
            }
            else if (ReturnObj14 == "Collect") {
                document.forms[0].FormView1_RD3.checked = true;
            }
            else {
                document.forms[0].FormView1_RD4.checked = true;
            }
            if (ReturnObj17 == "DEST") {
                document.forms[0].FormView1_RD5.checked = true;
            }
            else {
                document.forms[0].FormView1_RD6.checked = true;
            }

            //var sname=document.getElementById("FormView1_VSnameTextBox");
            //sname.innerText = ReturnObj10;
            //document.forms[0].HiddenField11.value = ReturnObj10;

            document.forms[0].FormView1_VSnameTextBox.value = ReturnObj10;
            document.forms[0].FormView1_VSmanTextBox.value = ReturnObj9;
            document.forms[0].FormView1_VCarrierTextBox.value = ReturnObj18;
            document.forms[0].HiddenCarr.value = ReturnObj18;

            document.forms[0].FormView1_VContactTextBox.value = ReturnObj19;
            document.forms[0].FormView1_VOverpctTextBox.value = ReturnObj20;
            document.forms[0].FormView1_VUnderpctTextBox.value = ReturnObj21;
            document.forms[0].FormView1_VTermsTextBox.value = ReturnObj22;
            document.forms[0].HiddenTerm.value = ReturnObj22;
            document.forms[0].FormView1_VTermdscrTextBox.value = ReturnObj23;
            document.forms[0].HiddenTermdesc.value = ReturnObj23;
            document.forms[0].FormView1_VProdTextBox.value = ReturnObj24;
            document.forms[0].FormView1_VTaxgrTextBox.value = ReturnObj25;
            document.forms[0].FormView1_VcExpTextBox.value = ReturnObj26;
            document.forms[0].FormView1_VDueDateTextBox.value = ReturnObj27;
            document.forms[0].FormView1_VLastDateTextBox.value = ReturnObj28;
            document.forms[0].HiddenField10.value = ReturnObj28;

            document.forms[0].FormView1_DropDownList1.value = ReturnObj29;

            if (document.getElementById("FormView1_VPonumTextBox")) {
                var cust = document.getElementById("FormView1_VPonumTextBox");
                cust.focus();
            }
            document.getElementById("FormView1_VCustomerTextBox").onchange();
        }

        function estlook() {
            if (document.forms[0].FormView1_estimateTextBox)
                document.forms[0].FormView1_estimateTextBox.value = "";
            estimatelook();
        }
        function estlook12() {
            document.forms[0].FormView1_RfqTextBox.value = "";
            quotelook();
        }

       

        function typelook() {
            var NewWindow = window.open("type_ordlookup.aspx", "typeordLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function typeordLookup(ReturnObj1) {
            document.forms[0].FormView1_ordtypeTextBox.value = ReturnObj1;
        }
        function carrierlook() {
            var NewWindow = window.open("Carrier_lookup.aspx", "CarrierLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function Carrierlookup(ReturnObj1, ReturnObj2) {
            document.forms[0].FormView1_VCarrierTextBox.value = ReturnObj1;
            document.forms[0].HiddenCarr.value = ReturnObj1;
        }


        function salesreplook() {
            var NewWindow = window.open("salesrep_lookup.aspx", "SalesRepLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function SalesRepLookup(ReturnObj1, ReturnObj2) {
            document.forms[0].FormView1_VSmanTextBox.value = ReturnObj1;
            document.forms[0].FormView1_VSnameTextBox.value = ReturnObj2;
            document.getElementById('FormView1_VSmanTextBox').focus();
        }

        //function salesreplookcopy(){ 
        //  var NewWindow = window.open("salesrep_lookupcopy.aspx","SalesRepLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        //}
        //function SalesRepLookupcopy(ReturnObj1,ReturnObj2){ 
        //  document.forms[0].FormView2_VSmanTextBox.value = ReturnObj1;
        //  document.forms[0].FormView2_VSnameTextBox.value = ReturnObj2;
        //}

        function smancopylook1() {
            var NewWindow = window.open("sman_copylookup.aspx", "smancopyLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function smancopyLookup(ReturnObj1, ReturnObj2) {
            document.forms[0].FormView1_VSman2TextBox.value = ReturnObj1;
            document.forms[0].FormView1_VSname2TextBox.value = ReturnObj2;
        }

        //function smancopylook1copy(){ 
        //  var NewWindow = window.open("sman_copylookupcopy.aspx","smancopyLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        //}
        //function smancopyLookupcopy(ReturnObj1,ReturnObj2){ 
        //  document.forms[0].FormView2_VSman2TextBox.value = ReturnObj1;
        //  document.forms[0].FormView2_VSname2TextBox.value = ReturnObj2;
        //}

        function salesmanlook() {
            var NewWindow = window.open("salesman_lookup.aspx", "SalesManLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function smancopyLookup1(ReturnObj1, ReturnObj2) {
            document.forms[0].FormView1_VSman3TextBox.value = ReturnObj1;
            document.forms[0].FormView1_VSname3TextBox.value = ReturnObj2;
        }

        //function salesmanlookcopy(){ 
        //  var NewWindow = window.open("salesman_lookupcopy.aspx","SalesManLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        //}
        //function smancopyLookup1copy(ReturnObj1,ReturnObj2){ 
        //  document.forms[0].FormView2_VSman3TextBox.value = ReturnObj1;
        //  document.forms[0].FormView2_VSname3TextBox.value = ReturnObj2;
        // 
        //}

        function termslook() {
            var NewWindow = window.open("terms_lookup.aspx", "TermsLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function termsLookup(ReturnObj1, ReturnObj2) {
            document.forms[0].FormView1_VTermsTextBox.value = ReturnObj1;
            document.forms[0].HiddenTerm.value = ReturnObj1;
            document.forms[0].FormView1_VTermdscrTextBox.value = ReturnObj2;
            document.forms[0].HiddenTermdesc.value = ReturnObj2;
            document.getElementById('FormView1_VTermsTextBox').focus();
        }

        //function termslookcopy(){ 
        //  var NewWindow = window.open("terms_lookupcopy.aspx","TermsLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        //}
        //function termsLookupcopy(ReturnObj1,ReturnObj2){ 
        //  document.forms[0].FormView2_VTermsTextBox.value = ReturnObj1;
        //  document.forms[0].FormView2_VTermdscrTextBox.value = ReturnObj2;
        //}

        function taxlook() {
            var NewWindow = window.open("tax_lookup.aspx", "TaxLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function TaxLookup(ReturnObj1) {
            document.forms[0].FormView1_VTaxgrTextBox.value = ReturnObj1;
            document.getElementById('FormView1_VTaxgrTextBox').focus();
        }

        // function taxlookcopy(){ 
        //  var NewWindow = window.open("tax_lookupcopy.aspx","TaxLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        //}
        //function TaxLookupcopy(ReturnObj1){ 
        //  document.forms[0].FormView2_VTaxgrTextBox.value = ReturnObj1;
        // }

        function duelook() {
            var NewWindow = window.open("Due_lookup.aspx", "DueCodeLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function dueLookup(ReturnObj1) {
            document.forms[0].FormView1_VDueCodeTextBox.value = ReturnObj1;
        }

        function Date5look() {
            var NewWindow = window.open("date_lookup5.aspx", "DateLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function Datelookup5(obj) {
            document.forms[0].FormView1_VOrdateLabel.value = obj;
        }
        function Datelook5() {
            document.forms[0].FormView1_VOrdateLabel.value = "";
            Date5look();
        }

        function Datelook() {
            var NewWindow = window.open("date_lookup.aspx", "DateLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function Datelookup(obj) {
            document.forms[0].FormView1_VDueDateTextBox.value = obj;
        }

        //function Datelookcopy(){ 
        //  var NewWindow = window.open("date_lookupcopy.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        //  }
        //function Datelookupcopy(obj)
        //{
        //  document.forms[0].FormView2_VDueDateTextBox.value=obj;
        //}

        function Datelook1() {
            document.forms[0].FormView1_VDueDateTextBox.value = "";
            Datelook();
        }
        function Date2look() {
            var NewWindow = window.open("date_lookup2.aspx", "DateLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function Datelookup2(obj) {
            document.forms[0].FormView1_VLastDateTextBox.value = obj;
        }

        //function Date2lookcopy(){ 
        //  var NewWindow = window.open("date_lookup2copy.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        //  }
        //function Datelookup2copy(obj)
        //{
        //  document.forms[0].FormView2_VLastDateTextBox.value=obj;
        //}

        function Datelook2() {
            document.forms[0].FormView1_VLastDateTextBox.value = "";
            Date2look();
        }
        function Date3look() {
            var NewWindow = window.open("date3_lookup.aspx", "DateLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function Datelookup3(obj) {
            document.forms[0].FormView1_VProdDateTextBox.value = obj;
        }

        //function Date3lookcopy(){ 
        //  var NewWindow = window.open("date3_lookupcopy.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        //  }
        //function Datelookup3copy(obj)
        //{
        //  document.forms[0].FormView2_VProdDateTextBox.value=obj;
        //}

        function Datelook3() {
            document.forms[0].FormView1_VProdDateTextBox.value = "";
            Date3look();
        }
        function Date4look() {
            var NewWindow = window.open("date4_lookup.aspx", "DateLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function Datelookup4(obj) {
            document.forms[0].FormView1_VcExpTextBox.value = obj;
        }

        //function Date4lookcopy(){ 
        //  var NewWindow = window.open("date4_lookupcopy.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        //  }
        //function Datelookup4copy(obj)
        //{
        //  document.forms[0].FormView2_VcExpTextBox.value=obj;
        //}

        function Datelook4() {
            document.forms[0].FormView1_VcExpTextBox.value = "";
            Date4look();
        }
        function estimatelook() {
            var NewWindow = window.open("est_order_lookup.aspx", "ordestimateLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }

        function estimateLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11, ReturnObj12, ReturnObj13, ReturnObj14, ReturnObj15, ReturnObj16, ReturnObj17, ReturnObj18, ReturnObj19, ReturnObj20, ReturnObj21, ReturnObj22, ReturnObj23, ReturnObj24, ReturnObj25, ReturnObj26, ReturnObj27, ReturnObj28, ReturnObj29, ReturnObj30, ReturnObj31, ReturnObj32, ReturnObj33, ReturnObj34, ReturnObj35, ReturnObj36, ReturnObj37, ReturnObj38, ReturnObj39) {
            if (document.forms[0].FormView1_estimateTextBox)
                document.forms[0].FormView1_estimateTextBox.value = ReturnObj1;
            document.forms[0].FormView1_VCustomerTextBox.value = ReturnObj2;
            document.forms[0].FormView1_VCustNameTextBox.value = ReturnObj3;
            document.forms[0].FormView1_VSoldNameTextBox.value = ReturnObj25;
            document.forms[0].FormView1_VSoldTextBox.value = ReturnObj4;
            document.forms[0].FormView1_VSmanTextBox.value = ReturnObj5;
            document.forms[0].FormView1_VCarrierTextBox.value = ReturnObj6;
            document.forms[0].HiddenCarr.value = ReturnObj6;
            document.getElementById('FormView1_VCustomerTextBox').focus();

            if (ReturnObj7 == "P")
                document.forms[0].FormView1_RD1.checked = true;
            if (ReturnObj7 == "B")
                document.forms[0].FormView1_RD2.checked = true;
            if (ReturnObj7 == "C")
                document.forms[0].FormView1_RD3.checked = true;
            if (ReturnObj7 == "T")
                document.forms[0].FormView1_RD4.checked = true;

            document.forms[0].FormView1_DropDownList1.value = ReturnObj9;

            var job1 = document.getElementById("FormView1_VJobTextBox");
            var job2 = document.getElementById("FormView1_VJob2TextBox");
            job1.innerText = ReturnObj10;
            job2.innerText = ReturnObj11;

            document.forms[0].HiddenField8.value = ReturnObj10;
            document.forms[0].HiddenField9.value = ReturnObj11;
            document.forms[0].FormView1_VCustAddrTextBox.value = ReturnObj12;
            document.forms[0].FormView1_VSoldAddrTextBox.value = ReturnObj26;
            document.forms[0].FormView1_VcustAddr2TextBox.value = ReturnObj13;
            document.forms[0].FormView1_VSoldAddr2TextBox.value = ReturnObj27;
            document.forms[0].FormView1_VCityTextBox.value = ReturnObj14;
            document.forms[0].FormView1_VSoldCityTextBox.value = ReturnObj28;
            document.forms[0].FormView1_VStateTextBox.value = ReturnObj15;
            document.forms[0].FormView1_VSoldStateTextBox.value = ReturnObj29;
            document.forms[0].FormView1_VZipTextBox.value = ReturnObj16;
            document.forms[0].FormView1_VSoldZipTextBox.value = ReturnObj16;
            document.forms[0].FormView1_VContactTextBox.value = ReturnObj17;
            document.forms[0].FormView1_VTermsTextBox.value = ReturnObj18;
            document.forms[0].HiddenTerm.value = ReturnObj18;
            document.forms[0].FormView1_VOverpctTextBox.value = ReturnObj19;
            document.forms[0].FormView1_VUnderpctTextBox.value = ReturnObj20;
            document.forms[0].FormView1_VTaxgrTextBox.value = ReturnObj22;

            document.forms[0].FormView1_VSnameTextBox.value = ReturnObj36;
            document.forms[0].FormView1_VTermdscrTextBox.value = ReturnObj32;
            document.forms[0].HiddenTermdesc.value = ReturnObj32;
            document.forms[0].FormView1_VLastDateTextBox.value = ReturnObj33;
            document.forms[0].HiddenField10.value = ReturnObj33;
            document.forms[0].FormView1_VDueDateTextBox.value = ReturnObj34;
            document.forms[0].FormView1_VnewordernoTextbox.value = ReturnObj35;
            document.forms[0].FormView1_VProdTextBox.value = ReturnObj37;
            if (ReturnObj38 != "" && ReturnObj38 != "0") {
                document.forms[0].FormView1_RfqTextBox.value = ReturnObj38;
                document.forms[0].HiddenQuoteNum.value = ReturnObj38;
            }
            else {
                document.forms[0].FormView1_RfqTextBox.value = "";
                document.forms[0].HiddenQuoteNum.value = null;
            }
            if (ReturnObj21 == "DEST")
                document.forms[0].FormView1_RD5.checked = true;
            else
                document.forms[0].FormView1_RD6.checked = true;
            if (ReturnObj23 == "O")
                document.forms[0].FormView1_DropDownList2.SelectedIndex = 0;
            if (ReturnObj23 == "C")
                document.forms[0].FormView1_DropDownList2.SelectedIndex = 1;
            if (ReturnObj23 == "N")
                document.forms[0].FormView1_DropDownList2.SelectedIndex = 2;
            if (ReturnObj23 == "Q")
                document.forms[0].FormView1_DropDownList2.SelectedIndex = 3;
            if (ReturnObj23 == "R")
                document.forms[0].FormView1_DropDownList2.SelectedIndex = 4;
            if (ReturnObj23 == "T")
                document.forms[0].FormView1_DropDownList2.SelectedIndex = 5;
            if (ReturnObj23 == "X")
                document.forms[0].FormView1_DropDownList2.SelectedIndex = 6;

            if (ReturnObj39 == 4 || ReturnObj39 == 8) {
                var contect = document.getElementById("FormView1_VContactTextBox");
                contect.focus();
            }

            document.getElementById("FormView1_estimateTextBox").onchange();
        }


        function quotelook() {
            var NewWindow = window.open("quote_lookup.aspx", "ordestimateLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function quoteLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11, ReturnObj12, ReturnObj13, ReturnObj14, ReturnObj15, ReturnObj16, ReturnObj17, ReturnObj18, ReturnObj19, ReturnObj20, ReturnObj21, ReturnObj22, ReturnObj23, ReturnObj24, ReturnObj25, ReturnObj26, ReturnObj27, ReturnObj28, ReturnObj29, ReturnObj30, ReturnObj31, ReturnObj32, ReturnObj33, ReturnObj34, ReturnObj35, ReturnObj36, ReturnObj37, ReturnObj38, ReturnObj39) {
            if (document.forms[0].FormView1_estimateTextBox)
                document.forms[0].FormView1_estimateTextBox.value = ReturnObj1;
            document.forms[0].FormView1_VCustomerTextBox.value = ReturnObj2;
            document.forms[0].FormView1_VCustNameTextBox.value = ReturnObj3;
            document.forms[0].FormView1_VSoldNameTextBox.value = ReturnObj25;
            document.forms[0].FormView1_VSoldTextBox.value = ReturnObj4;

            document.forms[0].FormView1_VSmanTextBox.value = ReturnObj5;
            document.forms[0].FormView1_VCarrierTextBox.value = ReturnObj6;
            document.forms[0].HiddenCarr.value = ReturnObj6;
            if (document.forms[0].FormView1_estimateTextBox)
                document.getElementById('FormView1_estimateTextBox').focus();

            if (ReturnObj7 == "P")
                document.forms[0].FormView1_RD1.checked = true;
            if (ReturnObj7 == "B")
                document.forms[0].FormView1_RD2.checked = true;
            if (ReturnObj7 == "C")
                document.forms[0].FormView1_RD3.checked = true;
            if (ReturnObj7 == "T")
                document.forms[0].FormView1_RD4.checked = true;


            document.forms[0].FormView1_DropDownList1.value = ReturnObj9;


            var job1 = document.getElementById("FormView1_VJobTextBox");
            var job2 = document.getElementById("FormView1_VJob2TextBox");
            job1.innerText = ReturnObj10;
            job2.innerText = ReturnObj11;

            document.forms[0].HiddenField8.value = ReturnObj10;
            document.forms[0].HiddenField9.value = ReturnObj11;
            document.forms[0].FormView1_VCustAddrTextBox.value = ReturnObj12;
            document.forms[0].FormView1_VSoldAddrTextBox.value = ReturnObj26;
            document.forms[0].FormView1_VcustAddr2TextBox.value = ReturnObj13;
            document.forms[0].FormView1_VSoldAddr2TextBox.value = ReturnObj27;
            document.forms[0].FormView1_VCityTextBox.value = ReturnObj14;
            document.forms[0].FormView1_VSoldCityTextBox.value = ReturnObj28;
            document.forms[0].FormView1_VStateTextBox.value = ReturnObj15;
            document.forms[0].FormView1_VSoldStateTextBox.value = ReturnObj29;
            document.forms[0].FormView1_VZipTextBox.value = ReturnObj16;
            document.forms[0].FormView1_VSoldZipTextBox.value = ReturnObj16;
            document.forms[0].FormView1_VContactTextBox.value = ReturnObj17;
            document.forms[0].FormView1_VTermsTextBox.value = ReturnObj18;
            document.forms[0].HiddenTerm.value = ReturnObj18;
            document.forms[0].FormView1_VOverpctTextBox.value = ReturnObj19;
            document.forms[0].FormView1_VUnderpctTextBox.value = ReturnObj20;
            document.forms[0].FormView1_VTaxgrTextBox.value = ReturnObj22;


            document.forms[0].FormView1_VSnameTextBox.value = ReturnObj36;
            document.forms[0].FormView1_VTermdscrTextBox.value = ReturnObj32;
            document.forms[0].HiddenTermdesc.value = ReturnObj32;
            document.forms[0].FormView1_VLastDateTextBox.value = ReturnObj33;
            document.forms[0].HiddenField10.value = ReturnObj33;
            document.forms[0].FormView1_VDueDateTextBox.value = ReturnObj34;
            document.forms[0].FormView1_VnewordernoTextbox.value = ReturnObj35;

            document.forms[0].FormView1_VProdTextBox.value = ReturnObj37;
            document.forms[0].FormView1_RfqTextBox.value = ReturnObj38;
            document.forms[0].HiddenQuoteNum.value = ReturnObj38;
            if (ReturnObj21 == "DEST")
                document.forms[0].FormView1_RD5.checked = true;
            else
                document.forms[0].FormView1_RD6.checked = true;
            if (ReturnObj23 == "O")
                document.forms[0].FormView1_DropDownList2.SelectedIndex = 0;
            if (ReturnObj23 == "C")
                document.forms[0].FormView1_DropDownList2.SelectedIndex = 1;
            if (ReturnObj23 == "N")
                document.forms[0].FormView1_DropDownList2.SelectedIndex = 2;
            if (ReturnObj23 == "Q")
                document.forms[0].FormView1_DropDownList2.SelectedIndex = 3;
            if (ReturnObj23 == "R")
                document.forms[0].FormView1_DropDownList2.SelectedIndex = 4;
            if (ReturnObj23 == "T")
                document.forms[0].FormView1_DropDownList2.SelectedIndex = 5;
            if (ReturnObj23 == "X")
                document.forms[0].FormView1_DropDownList2.SelectedIndex = 6;


            var NewWindow = window.open("quantity_lookup.aspx?est=" + ReturnObj38 + "", "typeordLookupWindow", "width=500,height=300,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");

            document.getElementById("FormView1_RfqTextBox").onchange();
        }

        function QuantityLookup(ReturnObj1, ReturnObj2, ReturnObj3) {

            document.forms[0].HiddenQuoteQty.value = ReturnObj1;
            document.forms[0].HiddenQuotePrice.value = ReturnObj2;
            document.forms[0].HiddenQuoteUom.value = ReturnObj3;

            if (document.getElementById("FormView1_estimateTextBox")) {
                var est = document.getElementById("FormView1_estimateTextBox");
                est.focus();
            }
            else if (document.getElementById("FormView1_VContactTextBox")) {
                var contact = document.getElementById("FormView1_VContactTextBox");
                contact.focus();
            }
        }



        function ShipTOLook() {            
            var lookHidden = document.getElementById("FormView1_VCustomerTextBox").value;
            var NewWindow = window.open("soldto_lookup.aspx?look=" + lookHidden + "", "SoldToLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function ShipTOLook2() {
            var lookHidden = document.getElementById("FormView1_VCustomerTextBox2").value;
            var NewWindow = window.open("soldto_lookup.aspx?look=" + lookHidden + "", "SoldToLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function SoldToLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7) {
            document.forms[0].FormView1_VSoldTextBox.value = ReturnObj1;
            
            document.getElementById('FormView1_VSoldTextBox').focus();
        }
        function overrun() {
            var over = document.getElementById("FormView1_VOverpctTextBox").value;

            if (over.indexOf(".") != -1) {
                return;
            }
            else if (over.length > 2 && over.length < 4)
                over = over + ".";
            document.getElementById("FormView1_VOverpctTextBox").value = over;
        }
        function underrun() {
            var under = document.getElementById("FormView1_VUnderpctTextBox").value;
            if (under.indexOf(".") != -1) {
                return;
            }
            else if (under.length > 2 && under.length < 4)
                under = under + ".";
            document.getElementById("FormView1_VUnderpctTextBox").value = under;
        }
        
        
   function ItemQuoteLook()
{ 
  var quote1 = "";
  var cust1 = document.getElementById("FormView1_VCustomerLabel").innerText;
  
  var NewWindow = window.open("item_qut_look.aspx?quote="+quote1+"&cust="+cust1+"","QuoteLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  
}
function ItemQuoteLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11, ReturnObj12, ReturnObj13, ReturnObj14, ReturnObj15, ReturnObj16, ReturnObj17, ReturnObj18) {

    document.forms[0].FormView1_RfqTextBox.value = ReturnObj1;
    document.forms[0].HiddenFieldFgItem.value = ReturnObj2; 
    
}
</script>


<script type="text/javascript" language="javascript">
    
    var prev_val = "";
    var com_val = 0.00;
    var fright_val = 0.00;

    function get_prev_val() {
        if (document.forms[0].FormView2_ComTextBox.value == "")
            com_val = 0.00;
        else
            com_val = document.forms[0].FormView2_ComTextBox.value;
    }
    function calcom() {
        var t_comm = 0.00;
        var t_cost = 0.00;
        var add_val = 0.00;

        if (isNaN(document.forms[0].FormView2_ComTextBox.value) && document.forms[0].FormView2_ComTextBox.value == ".") {
            document.forms[0].FormView2_ComTextBox.value = "";
            return;
        }

        var length = document.forms[0].FormView2_ComTextBox.value.length;
        var last_char = document.forms[0].FormView2_ComTextBox.value.charAt(length - 1);
        if (isNaN(last_char) && last_char != ".") {
            document.forms[0].FormView2_ComTextBox.value = document.forms[0].FormView2_ComTextBox.value.replace(last_char, "");
        }
       

        prev_val = com_val;
        t_cost = document.forms[0].FormView2_CosTextBox.value;
        if (document.forms[0].FormView2_ComTextBox.value == "")
            t_comm = 0.00;
        else
            t_comm = document.forms[0].FormView2_ComTextBox.value;

        add_val = parseFloat(t_comm) - parseFloat(prev_val);
        t_cost = parseFloat(t_cost) + parseFloat(add_val);

        document.forms[0].FormView2_CosTextBox.value = t_cost.toFixed(2);
        document.forms[0].CosHiddenField.value = t_cost.toFixed(2);

        if (document.forms[0].FormView2_ComTextBox.value == "")
            com_val = 0.00;
        else
            com_val = document.forms[0].FormView2_ComTextBox.value;
    }
    function get_prev_fright() {
        if (document.forms[0].FormView2_FreigTextBox.value == "")
            fright_val = 0.00;
        else
            fright_val = document.forms[0].FormView2_FreigTextBox.value;
    }
    function calFright() {
        var t_freight = 0.00;
        var t_cost = 0.00;
        var t_revenue = 0.00;
        var add_val = 0.00;

        if (isNaN(document.forms[0].FormView2_FreigTextBox.value) && document.forms[0].FormView2_FreigTextBox.value == ".") {
            document.forms[0].FormView2_FreigTextBox.value = "";
            return;
        }

        var length = document.forms[0].FormView2_FreigTextBox.value.length;
        var last_char = document.forms[0].FormView2_FreigTextBox.value.charAt(length - 1);
        if (isNaN(last_char) && last_char != ".") {
            document.forms[0].FormView2_FreigTextBox.value = document.forms[0].FormView2_FreigTextBox.value.replace(last_char, "");
        }

        prev_val = fright_val;

        t_cost = document.forms[0].FormView2_CosTextBox.value;
        if (document.forms[0].FormView2_FreigTextBox.value == "")
            t_freight = 0.00;
        else
            t_freight = document.forms[0].FormView2_FreigTextBox.value;

        add_val = parseFloat(t_freight) - parseFloat(prev_val);
        t_cost = parseFloat(t_cost) + parseFloat(add_val);

        document.forms[0].FormView2_CosTextBox.value = t_cost.toFixed(2);
        document.aspnetForm.ctl00$CosHiddenField.value = t_cost.toFixed(2);

        if (document.forms[0].FormView2_BillFCheckBox.checked) {
            t_revenue = document.forms[0].FormView2_TotTextBox.value;
            t_revenue = parseFloat(t_revenue) + parseFloat(add_val);
            document.forms[0].FormView2_TotTextBox.value = t_revenue.toFixed(2);
            document.forms[0].TotHiddenField.value = t_revenue.toFixed(2);

            if (document.forms[0].FormView2_FrtaxTextBox.value == "True") {
                var tax = document.forms[0].FormView2_TaxesTextBox.value;
                var v_frt_tax_rate = document.forms[0].FormView2_FrttaxrateTextBox.value;

                tax = parseFloat(tax) + parseFloat(add_val) * parseFloat(v_frt_tax_rate) / 100;
                document.forms[0].FormView2_TaxesTextBox.value = tax.toFixed(2);
                document.forms[0].TaxesHiddenField.value = tax.toFixed(2);
            }
        }


        if (document.forms[0].FormView2_FreigTextBox.value == "")
            fright_val = 0.00;
        else
            fright_val = document.forms[0].FormView2_FreigTextBox.value;
    }
    function calBFright() {
        var cal_val = 0;
        var t_freight = 0.00;
        var t_revenue = document.forms[0].FormView2_TotTextBox.value;
        if (document.forms[0].FormView2_FreigTextBox.value == "")
            t_freight = 0.00;
        else
            t_freight = document.forms[0].FormView2_FreigTextBox.value;

        if (document.forms[0].FormView2_BillFCheckBox.checked)
            cal_val = 1;
        else
            cal_val = -1;

        t_revenue = parseFloat(t_revenue) + parseFloat(t_freight) * cal_val;
        document.forms[0].FormView2_TotTextBox.value = t_revenue.toFixed(2);
        document.forms[0].TotHiddenField.value = t_revenue.toFixed(2);

        if (document.forms[0].FormView2_FrtaxTextBox.value == "True") {
            var tax = document.forms[0].FormView2_TaxesTextBox.value;
            var v_frt_tax_rate = document.forms[0].FormView2_FrttaxrateTextBox.value;
            tax = parseFloat(tax) + (parseFloat(t_freight) * v_frt_tax_rate / 100) * cal_val;
            document.forms[0].FormView2_TaxesTextBox.value = tax.toFixed(2);
            document.forms[0].TaxesHiddenField.value = tax.toFixed(2);
        }
    }
</script>
    </head>
<body>
    <form id="form1" runat="server">
    <hd:header id="Header1" runat="server"></hd:header>
    <asp:ScriptManager ID="ScriptManager1" runat="server">
            </asp:ScriptManager>
     <asp:HiddenField ID="HiddenField1" runat="server" />
    <asp:HiddenField ID="HiddenField2" runat="server" />
    <asp:HiddenField ID="HiddenField3" runat="server" />
    <asp:HiddenField ID="HiddenField4" runat="server" />
    <asp:HiddenField ID="HiddenField5" runat="server" />
    <asp:HiddenField ID="HiddenField6" runat="server" />
    <asp:HiddenField ID="HiddenField7" runat="server" />
    <asp:HiddenField ID="HiddenField8" runat="server" />
    <asp:HiddenField ID="HiddenField9" runat="server" />
    <asp:HiddenField ID="HiddenField10" runat="server" />
    <asp:HiddenField ID="HiddenField11" runat="server" />
    <asp:HiddenField ID="HiddenCarr" runat="server" />
    <asp:HiddenField ID="HiddenTerm" runat="server" />
    <asp:HiddenField ID="HiddenTermdesc" runat="server" />
    <asp:HiddenField ID="Hiddensold" runat="server" />
    
    <asp:HiddenField ID="HiddenQuoteQty" runat="server" />
    <asp:HiddenField ID="HiddenQuotePrice" runat="server" />
    <asp:HiddenField ID="HiddenQuoteUom" runat="server" />
    <asp:HiddenField ID="HiddenQuoteNum" runat="server" />  
    <asp:HiddenField ID="HiddenFieldFgItem" runat="server" />  
    <asp:HiddenField ID="TaxesHiddenField" runat="server" />
<asp:HiddenField ID="TotHiddenField" runat="server" />
<asp:HiddenField ID="CosHiddenField" runat="server" />
    
        
            
     <table width="100%">
    <tr><td>
    <div >
     <div style="display:none;">
     <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
                                 <TR>
                                 <TD align=left nowrap><font size=+0><b>Add Order:  &nbsp; </b></font></TD>
                                 <TD vAlign="middle">
                                  <asp:linkbutton id="LinkButton1" runat="server" OnClick="LinkButton1_Click"></asp:linkbutton>
                                  </TD>
                                  <TD align="right"><font size=+0><b>Users&nbsp;&nbsp;</b></font></TD>
                                     <TD vAlign="middle" align="left">Logged as&nbsp;
                                                 <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
                                                 <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
                                                  Company:&nbsp;
                                                  <asp:label id="lblComp" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
                                   </TD>                               
                                    <TD vAlign="middle" width="20">&nbsp;</TD>
                                    <td width=30>&nbsp;</td>
                                    </TR>
                                    </TABLE></div>  
    
 
    <div>
    <br />
        <asp:HiddenField ID="PartialHiddenField" runat="server" />
        
    
    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" Font-Bold="true" OnDataBound="FormView1_DataBound" OnPreRender="FormView1_PreRender" OnUnload="FormView1_Unload">
        <ItemTemplate>
            <table width="0px">
                <tr>
                    <td colspan="900"><fieldset style="width:810px;">
                        <table class="shade" width="810px">
                            <tr>
                                <td><b>Order:</b></td>
                                <td>
                                    <asp:Label ID="VOrderNumLabel11" runat="server" Width="100px" BackColor="Turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VOrderNum") %>'></asp:Label>
                                </td>
                                <td><b>&nbsp;&nbsp;Type:</b></td>
                                <td>
                                    <asp:Label ID="ordtypeLabel" runat="server" Width="20px" BackColor="Turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("ordtype") %>'></asp:Label> 
                                </td>
                                <td style="display:none;"><b>Quote:</b></td>
                                <td  style="display:none;">
                                    <asp:Label ID="vRfqLabel" Width="50px" runat="server" BackColor="Turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vRfq") %>'></asp:Label>
                                </td>
                                <td nowrap>
                                 <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
                                 <TR>
                                 <TD align=left nowrap><font size=+0><b>&nbsp; </b></font></TD>
                                 <TD vAlign="middle">
                                  <asp:linkbutton id="LinkButton1" runat="server" OnClick="LinkButton1_Click"></asp:linkbutton>
                                  </TD>
                                  <TD align="right"><font size=+0><b>Users&nbsp;&nbsp;</b></font></TD>
                                     <TD vAlign="middle" align="left">Logged as&nbsp;
                                                 <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
                                                 <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
                                                  Company:&nbsp;
                                                  <asp:label id="lblComp" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
                                   </TD>                               
                                    <TD vAlign="middle" width="20">&nbsp;</TD>
                                    <td width=30>&nbsp;</td>
                                    </TR>
                                    </TABLE>  
                                </td>
            
                                <td  style="display:none;"><b><asp:Label ID="est_label" runat="server" Text="Estimate:"></asp:Label></b></td>
                                <td  style="display:none;"><asp:Label ID="EstimateTextBox" runat="server"  Width="70px" BackColor="Turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VEstimate") %>'></asp:Label></td>
                                <td style="display:none;"><b>Job:</b></td>
                                <td style="display:none;"><asp:Label ID="Label3" runat="server" Width="70px" BackColor="Turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VJob") %>'></asp:Label>
                                <asp:Label ID="Label4" runat="server" Width="30px" BackColor="Turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VJob2") %>'></asp:Label></td>
                                
                                <td nowrap><b>Last User:</b></td>
                                <td style="width:150px;">
                                    <asp:Label ID="VUseridLabel" runat="server" Width="50px" BackColor="Turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VUserid") %>'></asp:Label>
                                </td>
                                <td><b>Status:</b></td>
                                <td style="width:150px;">
                                    <asp:Label ID="VStatLabel" runat="server" Width="20px" BackColor="Turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VStat") %>'></asp:Label>
                                </td>
                           </tr>
                      </table></fieldset>
                  </td>
             </tr>
             
             
             <tr>
              <td colspan="2">
              <asp:Button ID="addButton" runat="server" CommandName="new"   CssClass="button" Text="Add"></asp:Button>
              <asp:Button ID="UpdatButton"  runat="server" CommandName="Edit" CssClass="buttonM" Text="Update" />
              <asp:Button ID="deleteButton" runat="server"  CssClass="button" CausesValidation="False" Text="Delete" OnClick="DeleteButton_Click" OnClientClick="return confirm('Are you sure you want to delete this record')"></asp:Button>
              &nbsp;&nbsp;&nbsp;         </td>
              </tr>
            
         
             <tr>
                <td colspan="2">        
                    <table>
                        <tr>
                            <td>
                                <fieldset style="width:290px; border:solid 1px black; background-color:#EFF3FB; height:150px;">
                                    <table>
                                        <tr>
                                            <td align="right" style="padding-right:5px"><b>Bill To:</b></td>
                                            <td>
                                                
                                                <a href="#" class="tooltip"  ><asp:Label ID="VCustomerLabel" runat="server" Width="200px"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VCustomer") %>'></asp:Label>  
                                                <div>                                                                                                
                                                <p><asp:Label ID="VCustNameLabel" runat="server" Width="212px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VCustName") %>'></asp:Label>	<br />
			                                    <asp:Label ID="VCustAddrLabel" runat="server" Width="212px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VCustAddr") %>'></asp:Label><br />
			                                    <asp:Label ID="VcustAddr2Label" runat="server" Width="212px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VcustAddr2") %>'></asp:Label><br />
			                                    <asp:Label ID="VCityLabel" runat="server" Width="127px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VCity") %>'></asp:Label>
                                                <asp:Label ID="VStateLabel" runat="server" Width="20px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VState") %>'></asp:Label>
                                                <asp:Label ID="VZipLabel" runat="server" Width="54px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VZip") %>'></asp:Label>
			                                    </p>
                                               </div>
                                                
                                                </a>
                                                

			   
			  <%-- <p><asp:Label ID="VCustNameLabel" runat="server" Width="212px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VCustName") %>'></asp:Label>	
			   <asp:Label ID="VCustAddrLabel" runat="server" Width="212px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VCustAddr") %>'></asp:Label>
			   <asp:Label ID="VcustAddr2Label" runat="server" Width="212px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VcustAddr2") %>'></asp:Label>
			   <asp:Label ID="VCityLabel" runat="server" Width="127px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VCity") %>'></asp:Label>
               <asp:Label ID="VStateLabel" runat="server" Width="20px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VState") %>'></asp:Label>
               <asp:Label ID="VZipLabel" runat="server" Width="54px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VZip") %>'></asp:Label>
			   </p>--%>


                                            </td>
                                         </tr>
                                         <tr>
                                            <td align="right" style="padding-right:5px"><b>Contact:</b></td>
                                            <td>
                                                <asp:Label ID="VContactLabel" Width="200px" runat="server"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VContact") %>'></asp:Label>
                                            </td>
                                        </tr>                                                                                 
                                        <tr>
                                                    <td align="right" style="padding-right:5px" nowrap><b>Payment Type:</b></td>
                                                    <td>
                                                        <asp:Label ID="VCtypeLabel" runat="server" Width="100px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VCtype") %>'></asp:Label>
                                                    </td>                                                    
                                                 </tr>
                                                 <tr>
                                                    <td align="right" style="padding-right:5px"><b>Account#:</b></td>
                                                    <td>
                                                        <asp:Label ID="VCnumLabel" runat="server" Width="125px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VCnum") %>'></asp:Label>
                                                    </td>
                                                 </tr>
                                                 <tr>
                                                    <td align="right" style="padding-right:5px"><b>Ref#:</b></td>
                                                    <td>
                                                        <asp:Label ID="VCauthLabel" runat="server" Width="190px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VCauth") %>'></asp:Label>                                                        
                                                        <asp:Label ID="lbl_rec_key" Visible="false" runat="server" Width="100px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("vRecKey") %>'></asp:Label>
                                                    </td>
                                                 </tr>
                                                 <tr>
                                                 <td align="right" style="padding-right:5px"><b>Expire</b></td>
                                                    <td>
                                                        <asp:Label ID="VcExpLabel" runat="server" Width="80px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VcExp","{0:MM/dd/yyyy}") %>'></asp:Label>
                                                    </td>
                                                 </tr>
                                        
                                        
                                        <tr>
                                                    <td nowrap colspan="2" ><b>Sales Rep</b></td>
                                                    
                                                 </tr>
                                                 <tr>
                                                 <td >
                                                 <asp:Label ID="VSmanLabel" runat="server" Width="50px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VSman") %>'></asp:Label>
                                                    </td><td>
                                                        <asp:Label ID="VSnameLabel" runat="server" Width="200px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VSname") %>'></asp:Label>
                                                 </td>
                                                 </tr>
                                                 
                                                 
                                  </table>
                              </fieldset>
                          </td>
                          <td>
                            <fieldset style="width:255px; border:solid 1px black; background-color:#EFF3FB; height:150px;">
                                <table>
                                    <tr>
                                        <td align="right" style="padding-right:5px"><b>Sold To:</b></td>
                                        <td>
                                            <a href="#" class="tooltip"  ><asp:Label ID="VSoldLabel" runat="server"  Width="182px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VSold") %>'></asp:Label>                                            
                                                <div >

			  
			   <p><asp:Label ID="VSoldNameLabel" runat="server" Width="212px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VSoldName") %>'></asp:Label>	
			   <asp:Label ID="VSoldAddrLabel" runat="server" Width="212px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VSoldAddr") %>'></asp:Label>
			   <asp:Label ID="VSoldAddr2Label" runat="server" Width="212px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VSoldAddr2") %>'></asp:Label>
			   <asp:Label ID="VSoldCityLabel" runat="server" Width="127px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VSoldCity") %>'></asp:Label>
               <asp:Label ID="VSoldStateLabel" runat="server" Width="20px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VSoldState") %>'></asp:Label>
               <asp:Label ID="VSoldZipLabel" runat="server" Width="54px"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VSoldZip") %>'></asp:Label>
			   </p>	  
		              </div>
		              </a> 

		 
                                        </td>
                                        
                                    </tr>
                                  
                                    <tr><td align="right" style="padding-right:5px"><b>Carrier:</b></td>
                                                    <td >
                                                        <asp:Label ID="VCarrierLabel" runat="server" Width="100px"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VCarrier") %>'></asp:Label>
                                                        
                                                    </td>
                                                 </tr>                                    
                                    
                                     <tr>
                                                    <td align="right" style="padding-right:5px;"><b>FOB:</b></td>
                                                    <td style="height: 23px"><b>
                                                        <asp:RadioButton ID="RD5" Enabled="false" runat="server" />Destination 
                                                        <asp:Label ID="Label5" Visible="false" runat="server" Text='<%# Bind("VFob") %>'></asp:Label>
                                                        <asp:RadioButton ID="RD6" Enabled="false" runat="server" />Origin</b>
                                                    </td>
                                                 </tr>
                                                 <tr>
                                                <td align="right" style="padding-right:5px" nowrap><b>Pay Terms:</b></td>
                                                <td>
                                                    <asp:Label ID="VTermsLabel" Width="50px" runat="server"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VTerms") %>'></asp:Label>
                                                </td> </tr>
                                                <tr><td></td>
                                                <td>
                                                    <asp:Label ID="VTermdscrLabel" runat="server" Width="180px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VTermdscr") %>'></asp:Label>
                                                </td></tr>
                                                <tr><td colspan="2"> <b>
                                                        <asp:RadioButton ID="RD1" Enabled="false" runat="server" />Prepaid 
                                                        <asp:Label ID="Label1" Visible="false" runat="server" Text='<%# Bind("VFreight") %>'></asp:Label>
                                                        <asp:RadioButton ID="RD2" Enabled="false" runat="server" />Bill
                                                        <asp:RadioButton ID="RD3" Enabled="false" runat="server" />Collect
                                                        <asp:RadioButton ID="RD4" Enabled="false" runat="server" />3rd Party
                                                     </b></td></tr>
                                                      <tr>
                                                <td align="left" style="padding-left:5px" colspan="2"><b>Previous Order#:</b>
                                                
                                                    <asp:Label ID="VProdLabel" Width="50px" runat="server"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VProd") %>'></asp:Label>
                                                </td>
                                            </tr>
                                            <tr><td></td></tr>
                                   
                                </table>
                             </fieldset>
                          </td>
                          <td>
                            <fieldset style="width:255px; border:solid 1px black; background-color:#EFF3FB; height:150px;">
                                <table>
                                    <tr>
                                        <td align="right" style="padding-right:5px"><b>Date:</b></td>
                                        <td>
                                            <asp:Label ID="VOrdateLabel" runat="server" Width="160px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VOrdate","{0:MM/dd/yyyy}") %>'></asp:Label>
                                        </td>
                                    </tr>
                                    <tr>
                                        <td align="right" style="padding-right:5px"><b>Due Date:</b></td>
                                        <td>
                                            <asp:Label ID="VDueCodeLabel" runat="server" Width="50px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VDueCode") %>'></asp:Label>
                                            <asp:Label ID="VDueDateLabel" Width="105px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("VDueDate","{0:MM/dd/yyyy}") %>'></asp:Label>
                                        </td>
                                     </tr>
                                     <tr>
                                        <td align="right" style="padding-right:5px"><b>Last Ship:</b></td>
                                        <td>
                                            <asp:Label ID="VLastDateLabel" Width="160px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("VLastDate","{0:MM/dd/yyyy}") %>'></asp:Label>
                                        </td>
                                     </tr>
                                    <tr>
                                        <td align="right" style="padding-right:5px"><b><asp:Label ID="prd_label" runat="server" Text="Prod Date:"></asp:Label></b></td>
                                        <td>
                                            <asp:Label ID="VProdDateLabel" runat="server" Width="160px"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VProdDate","{0:MM/dd/yyyy}") %>'></asp:Label>
                                        </td>
                                     </tr>
                                     <tr>
                                        <td align="right" style="padding-right:5px" nowrap><b>Customer Po#:</b></td>
                                        <td>
                                            <asp:Label ID="VPonumLabel" Width="160px" runat="server"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VPonum") %>'></asp:Label>
                                        </td>
                                     </tr>
                                     <tr style="display:none;">
                                                <td align="right" style="padding-right:5px"><b>Overrun%:</b></td>
                                                <td>
                                                    <asp:Label ID="VOverpctLabel" Width="50px" runat="server"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VOverpct","{0:##0.00}") %>'></asp:Label>
                                                </td>                                                
                                             </tr>
                                             <tr  style="display:none;">
                                             <td align="right" style="padding-right:5px"><b>Underrun%:</b></td>
                                                   <td> <asp:Label ID="VUnderpctLabel" Width="50px" runat="server"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VUnderpct","{0:##0.00}") %>'></asp:Label>
                                                </td>
                                             </tr>
                                             <tr>
                                                <td align="right" style="padding-right:5px"><b>Tax Code:</b></td>
                                                <td>
                                                    <asp:Label ID="VTaxgrLabel" Width="50px" runat="server"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" Text='<%# Bind("VTaxgr") %>'></asp:Label>
                                                </td>
                                             </tr>
                                     
                                  </table>
                               </fieldset>
                            </td>
                         </tr>
                      </table>
                   </td>
                </tr>
                
                           
                          
        </table>
    </td>
 </tr>
 
  <tr><td style="display:none"> <asp:Label ID="LineTotalLabel" runat="server" Text='<%# Bind("vLineTotal") %>'></asp:Label> </td></tr>
 </table>   
</ItemTemplate>

<EditItemTemplate>
    <asp:Panel ID="Panel1" DefaultButton="UpdateButton" runat="server">
        <table width="800">
            <tr>
                <td colspan="2"><fieldset style="width:812px;">
                    <table class="shade" width="812">
                        <tr>
                            <td> <b>Order#:</b></td>
                            <td>
                                <asp:Label ID="VOrderNumLabel" Width="80px" BackColor="turquoise" runat="server" Text='<%# Bind("VOrderNum") %>'></asp:Label>
                            </td>
                            <td><b>&nbsp;&nbsp;Type:</b> </td>
                            <td><%--<asp:TextBox ID="ordtypeTextBox" Width="30px" runat="server" Text='<%# Bind("ordtype") %>'></asp:TextBox>--%>
                                <asp:DropDownList ID="DropDownList2" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px" DataTextField='<%# Bind("ordtype") %>' runat="server" SelectedValue='<%# Bind("ordtype") %>'>
                                    <asp:ListItem Value="O">O- Original</asp:ListItem>
                                    <asp:ListItem Value="C">C- Change</asp:ListItem>
                                    <asp:ListItem Value="N">N- New</asp:ListItem>
                                    <asp:ListItem Value="Q">Q- Quality/Re-work</asp:ListItem>
                                    <asp:ListItem Value="R">R- Repeat</asp:ListItem>
                                    <asp:ListItem Value="T">T- Transfer</asp:ListItem>
                                    <asp:ListItem Value="X">X- Complete re-run</asp:ListItem>
                                 </asp:DropDownList>
                                 <%--<a href="#" onClick="typelook(); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a>--%>
                             </td>
                             <td style="display:none;"><b>Quote:</b></td>
                                    <td nowrap style="display:none;">
                                    
                                        <asp:Label ID="quoteTextBox" Width="60px" BackColor="turquoise" runat="server" Text='<%# Bind("VEstimate") %>'> </asp:Label>                                        
                                    </td>
                                    <td style="display:none;"> <b><asp:Label ID="est_Label" runat="server" Text="Estimate:"></asp:Label></b></td>
                                    <td nowrap style="display:none;">
                                        <asp:Label ID="estimateTextBox" Width="60px" BackColor="turquoise" runat="server" Text='<%# Bind("VEstimate") %>'> </asp:Label>
                                        <%--<a href="#" onClick="quotelook(); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a>--%>
                                    </td>
                            <td nowrap style="display:none;"> <b>Job#:</b></td>
                                <td style="display:none;"><asp:label ID="Label8" Width="50px" BackColor="turquoise" runat="server" Text='<%# Bind("VJob") %>'></asp:Label>
                                <td style="display:none;"><asp:label ID="Label9" Width="20px" BackColor="turquoise" runat="server" Text='<%# Bind("VJob2") %>'></asp:Label></td>  
                                
                            <td nowrap>
                                 <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
                                 <TR>
                                 
                                  <TD align="right"><font size=+0><b>&nbsp;&nbsp;Users&nbsp;&nbsp;</b></font></TD>
                                     <TD nowrap vAlign="middle" align="left">Logged as&nbsp;
                                                 <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
                                                 <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
                                                  Company:&nbsp;
                                                  <asp:label id="lblComp" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
                                   </TD>                               
                                   
                                    </TR>
                                    </TABLE>  
                                </td>                                
                            <td nowrap> <b>Last User:</b></td>
                            <td>
                                <asp:label ID="VUseridLabel" Width="60px" BackColor="turquoise" runat="server" Text='<%# Bind("VUserid") %>'></asp:Label>
                            </td>
                            <td nowrap><b>&nbsp;&nbsp;Status:</b></td>
                            <td style="width:150px">
                                <asp:Label ID="VStatLabel" Width="40px" BackColor="turquoise" runat="server" Text='<%# Bind("VStat") %>'></asp:Label>
                            </td>
                         </tr>
                      </table></fieldset>
                   </td>
                </tr>
                <tr>
                    <td>
                        <table>
                            <tr>
                                <td>
                                    <fieldset style="width:260px; border:solid 1px black; background-color:#EFF3FB; height:180px;">
                                        <table>
                                            <tr>
                                                <td align="right" style="padding-right:5px" ><b>Bill To:</b></td>
                                                <td nowrap>
                                                    <asp:TextBox Enabled="false" ID="VCustomerTextBox2" Width="120px" MaxLength="8" runat="server" Text='<%# Bind("VCustomer") %>'></asp:TextBox>
                                                    <%--<a href="#" onClick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>--%>
                                                </td>
                                            </tr>
                                            <tr>
                                                <td align="right" style="padding-right:5px"><b>Contact:</b></td>
                                                <td>
                                                    <asp:TextBox ID="VContactTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="160px" MaxLength="25" runat="server" Text='<%# Bind("VContact") %>'></asp:TextBox>
                                                </td>
                                             </tr>
                                             <tr>
                                                                            <td nowrap><b>Payment Type:</b></td>
                                                                            <td>
                                                                                <asp:TextBox ID="VCtypeTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="8" Width="150px" runat="server" Text='<%# Bind("VCtype") %>'></asp:TextBox>
                                                                            </td>
                                                                            
                                                                        </tr>
                                                                        <tr>
                                                                            <td align="right" style="padding-right:5px"><b>Account#:</b></td>
                                                                            <td>
                                                                                <asp:TextBox ID="VCnumTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="20" Width="150px" runat="server" Text='<%# Bind("VCnum") %>'></asp:TextBox>
                                                                            </td>
                                                                            <td></td>
                                                                        </tr>
                                                                        <tr>
                                                                            <td align="right" style="padding-right:5px"><b>Ref#:</b></td>
                                                                            <td>
                                                                                <asp:TextBox ID="VCauthTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="30" Width="150px" runat="server" Text='<%# Bind("VCauth") %>'></asp:TextBox>
                                                                            </td>
                                                                        </tr>
                                                                        <tr>
                                                                        <td align="right" style="padding-right:5px" nowrap><b>Expire:</b></td><td>
                                                                                <asp:TextBox ID="VcExpTextBox" MaxLength="10"  Width="53px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" ToolTip="MM/DD/YYYY" runat="server" Text='<%# Bind("VcExp","{0:MM/dd/yyyy}") %>'></asp:TextBox>
                                                                                <a href="#" onblur="FormView1_VcExpTextBox.focus()" tabindex="1" onClick="showCalendarControl(FormView1_VcExpTextBox); return false"><asp:Image ID="Image12" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                                                
                                                                            </td>
                                                                        </tr>
                                                                        
                                                                        <tr>
                                                                        <td><b>Sales Rep</b></td>
                                                                        <td><b>Sales Rep Name</b></td>                                                                        
                                                                     </tr>
                                                                     <tr>
                                                                        <td> 
                                                                            <asp:TextBox ID="VSmanTextBox" Enabled="false" Width="50px" runat="server" Text='<%# Bind("VSman") %>'></asp:TextBox>
                                                                            <%--<a href="#" onClick="salesreplook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>--%>
                                                                        </td>
                                                                        <td>
                                                                            <asp:TextBox ID="VSnameTextBox" Enabled="false" Width="100px" runat="server" Text='<%# Bind("VSname") %>'></asp:TextBox>
                                                                        </td>
                                                                        
                                                                     </tr>
                                             
                                          </table>
                                       </fieldset>
                                    </td>
                                    <td>
                                        <fieldset style="width:260px; border:solid 1px black; background-color:#EFF3FB; height:180px;">
                                            <table>
                                                <tr>
                                                    <td align="right" style="padding-right:5px"><b>Sold To:</b></td>
                                                    <td>
                                                        <asp:TextBox ID="VSoldTextBox" Width="120px" MaxLength="8"   runat="server" Text='<%# Bind("VSold") %>'></asp:TextBox>
                                                        <a href="#" tabindex="1" onClick="ShipTOLook2(); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                    </td>
                                                 </tr>
                                                                                                  
                                                 <tr>
                                                                <td align="right" style="padding-right:5px"><b>Carrier:</b></td>
                                                                <td>
                                                                    <asp:TextBox ID="VCarrierTextBox"  Width="100px" runat="server" Text='<%# Bind("VCarrier") %>'></asp:TextBox>
                                                                    <a href="#" tabindex="1" onClick="carrierlook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                                </td>
                                                            </tr>
                                                            <tr>
                                                                <td align="right" style="padding-right:5px"><b>FOB:</b></td>
                                                                <td><b>
                                                                    <asp:TextBox ID="fob_codeTextBox" Visible="false" runat="server" Text='<%# Bind("VFob") %>'></asp:TextBox>
                                                                    <asp:RadioButton ID="RD5" Enabled="false" GroupName="editstatus" runat="server" />Destination
                                                                    <asp:RadioButton ID="RD6" Enabled="false" GroupName="editstatus" runat="server" />Origin
                                                                </b></td>
                                                            </tr>
                                                            
                                                            <tr>
                                                                <td align="right" style="padding-right:5px"><b>Pay Terms:</b></td>
                                                                <td>
                                                                    <asp:TextBox  ID="VTermsTextBox" Enabled="false" Width="80px" MaxLength="5" runat="server" Text='<%# Bind("VTerms") %>'></asp:TextBox>
                                                                    <%--<a href="#" onClick="termslook(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>--%>
                                                                </td>                                                                
                                                            </tr>
                                                            <tr><td></td>
                                                            <td>
                                                                    <asp:TextBox ID="VTermdscrTextBox" Enabled="false" Width="150px" runat="server" Text='<%# Bind("VTermdscr") %>'></asp:TextBox>
                                                                </td></tr>
                                                                
                                                                <tr >       
                                                                <td colspan="2">
                                                                
                                                                    <asp:RadioButton ID="RD1" Enabled="false" GroupName="editstatus1" runat="server" />Prepaid
                                                                    <asp:TextBox ID="FRTextBox"  Visible="false" runat="server" Text='<%# Bind("VFreight") %>'></asp:TextBox>
                                                                    <asp:RadioButton ID="RD2" Enabled="false" GroupName="editstatus1"  runat="server" />Bill
                                                                    <asp:RadioButton ID="RD3" Enabled="false" GroupName="editstatus1" runat="server" />Collect
                                                                    <asp:RadioButton ID="RD4" Enabled="false" GroupName="editstatus1" runat="server" />3rd Party
                                                                 </td>
                                                             </tr>
                                                             <tr>
                                                                <td align="right" style="padding-right:5px" nowrap><b>Previous Order#:</b></td>
                                                                <td>
                                                                    <asp:TextBox ID="VProdTextBox" Width="50px" MaxLength="6" Enabled="false" runat="server" Text='<%# Bind("VProd") %>'></asp:TextBox>
                                                                    <asp:RegularExpressionValidator ID="RegularExpressionValidator1" runat="server" SetFocusOnError="true" ControlToValidate="VProdTextBox" ErrorMessage="Only Integer Number" Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}?\d*)$)"></asp:RegularExpressionValidator>
                                                                </td>
                                                            </tr>
                                              </table>
                                           </fieldset>
                                        </td>
                                        <td>
                                            <fieldset style="width:280px; border:solid 1px black; background-color:#EFF3FB; height:180px;">
                                                <table>
                                                    <tr>
                                                        <td align="right" style="padding-right:5px" nowrap><b>Date:</b></td>
                                                        <td nowrap><asp:TextBox ID="VOrdateLabel" MaxLength="10" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" ToolTip="MM/DD/YYYY" Width="80px" runat="server" Text='<%# Bind("VOrdate","{0:MM/dd/yyyy}") %>'></asp:TextBox>
                                                                   <a href="#" onblur="FormView1_VOrdateLabel.focus()" tabindex="1" onClick="showCalendarControl(FormView1_VOrdateLabel); return false"><asp:Image ID="Image14" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                                   
                                                        </td>
                                                    </tr>
                                                    <tr>
                                                        <td align="right" style="padding-right:5px" nowrap><b>Due Date:</b></td>
                                                        <td nowrap><%--<asp:TextBox ID="VDueCodeTextBox"  Width="50px"  runat="server" Text='<%# Bind("VDueCode") %>'></asp:TextBox>--%>
                                                            <asp:DropDownList ID="DropDownList1" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="100px" SelectedValue='<%# Bind("VDueCode") %>' DataTextField='<%# Bind("VDueCode") %>' runat="server">
                                                                <asp:ListItem Value="ASAP">ASAP- As Soon As Possible</asp:ListItem>
                                                                <asp:ListItem Value="NB4">NB4- Not Before</asp:ListItem>
                                                                <asp:ListItem Value="MUST"></asp:ListItem>
                                                                <asp:ListItem Value="HOT"></asp:ListItem>
                                                                <asp:ListItem Value="RUSH"></asp:ListItem>
                                                                <asp:ListItem Value="WO"></asp:ListItem>
                                                                <asp:ListItem Value="HOLD"></asp:ListItem>
                                                                <asp:ListItem Value="CR"></asp:ListItem>
                                                                <asp:ListItem Value="BY"></asp:ListItem>
                                                                <asp:ListItem Value="ON">ON</asp:ListItem>
                                                                <asp:ListItem Value="NH"></asp:ListItem>
                                                                <asp:ListItem Value="$$$">$$$- Credit Hold</asp:ListItem>
                                                                <asp:ListItem Value="AM">AM- AM Delivery</asp:ListItem>
                                                                <asp:ListItem Value="INK">INK- Waiting for Ink Info</asp:ListItem>
                                                             </asp:DropDownList>
                                                             <%--<a href="#" onClick="duelook(); return false"><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a>    --%>
                                                             <asp:TextBox ID="VDueDateTextBox" MaxLength="10" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" Width="53px" runat="server" ToolTip="MM/DD/YYYY" Text='<%# Bind("VDueDate","{0:MM/dd/yyyy}") %>'></asp:TextBox> 
                                                             <a href="#" onblur="FormView1_VDueDateTextBox.focus()" tabindex="1" onClick="showCalendarControl(FormView1_VDueDateTextBox); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                             
                                                          </td>
                                                       </tr>
                                                       <tr>
                                                            <td align="right" style="padding-right:5px" nowrap><b>Last Ship:</b></td>
                                                            <td nowrap>
                                                                <asp:TextBox ID="VLastDateTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Enabled="false" Width="80px" runat="server" Text='<%# Bind("VLastDate","{0:MM/dd/yyyy}") %>'></asp:TextBox>
                                                                <%--<a href="#" onClick="Date2look(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a>--%>
                                                            </td>
                                                       </tr>
                                                       <tr>
                                                            <td align="right" style="padding-right:5px" nowrap><b><asp:Label ID="prd_label" runat="server" Text="Prod Date:"></asp:Label></b></td>
                                                            <td nowrap>
                                                                <asp:TextBox Width="80px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" ID="VProdDateTextBox" Enabled="false" runat="server" Text='<%# Bind("VProdDate","{0:MM/dd/yyyy}") %>'></asp:TextBox>
                                                                <asp:CompareValidator ID="CompareValidator5" runat="server" ControlToValidate="VProdDateTextBox" Display="dynamic" Operator="DataTypeCheck" Type="Date" SetFocusOnError="true" ErrorMessage="Invalid Date"></asp:CompareValidator>
                                                                <%--<a href="#" onClick="Date3look(); return false"><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /></a>    --%>
                                                            </td>
                                                       </tr>
                                                       <tr>
                                                            <td align="right" style="padding-right:5px" nowrap><b>Customer Po#:</b></td>
                                                            <td nowrap>
                                                                <asp:TextBox ID="VPonumTextBox" onkeyup="focusonsave()" onfocus= "javascript:focusval(this)" onblur="setsave(this)" Width="80px" MaxLength="15" runat="server" Text='<%# Bind("VPonum") %>'></asp:TextBox>
                                                                
                                                            </td>
                                                       </tr>
                                                       <tr  style="display:none;">
                                                                <td align="right" style="padding-right:5px"><b>Overrun%:</b></td>
                                                                <td>
                                                                    <asp:TextBox ID="VOverpctTextBox" MaxLength="6" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" onkeyup="overrun()" Width="50px"  runat="server" Text='<%# Bind("VOverpct") %>'></asp:TextBox>
                                                                    <asp:CompareValidator ID="CompareValidator1" runat="server" ErrorMessage="Only Numbers" ControlToValidate="VOverpctTextBox" SetFocusOnError="true" Display="dynamic" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                                                </td>                                                                
                                                            </tr>
                                                            <tr  style="display:none;">
                                                            <td align="right" style="padding-right:5px"><b>Underrun%:</b></td>
                                                            <td>
                                                                    <asp:TextBox ID="VUnderpctTextBox" MaxLength="6" onfocus= "javascript:focusval(this)" onblur="javascript:updateunderblurval(this)" onkeyup="underrun()" Width="50px" runat="server" Text='<%# Bind("VUnderpct") %>'></asp:TextBox>
                                                                    <asp:CompareValidator ID="UnderCompareValidator" runat="server" ErrorMessage="Only Numbers" ControlToValidate="VUnderpctTextBox" SetFocusOnError="true" Display="dynamic" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                                                </td>
                                                            </tr>
                                                            <tr>
                                                                <td align="right" style="padding-right:5px"><b>Tax Code:</b></td>
                                                                <td>
                                                                    <asp:TextBox ID="VTaxgrTextBox" Width="50px" MaxLength="3" onfocus= "javascript:focusval(this)"  onblur="javascript:refblurval(this)" runat="server" Text='<%# Bind("VTaxgr") %>'></asp:TextBox>
                                                                    <a href="#" tabindex="1" onClick="taxlook(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                                </td>
                                                            </tr>
                                                    </table>
                                                 </fieldset>
                                              </td>
                                           </tr>
                                        </table>
                                     </td>
                                  </tr>
                                  
                                        
                                                  
                                                   <tr>
                                                        <td>
                                                            <asp:Button ID="UpdateButton" runat="server" Font-Bold="true" CssClass="buttonM" CausesValidation="true" OnClick="UpdateButon_click" Text="Save"></asp:Button>
                                                            <asp:Button ID="UpdateCancelButton" CssClass="button" runat="server" CausesValidation="False" CommandName="Cancel" Text="Cancel" OnClick="UpdateButton_Cancel_Click" Font-Bold="true"></asp:Button>
                                                        </td>
                                                </tr>
                                      </table>
                         </asp:Panel>
      </EditItemTemplate>
        
        <InsertItemTemplate>
            <asp:Panel ID="Panel1" DefaultButton="InsertButton" runat="server">
                
                <table width="800">
                    <tr>
                        <td colspan="2"><fieldset style="width:840px;">
                            <table class="shade" width="840">
                                <tr>
                                    <td nowrap><b>&nbsp;</b>
                                        <asp:Label ID="VOrderNumLabel"  Visible="false"  Width="20px" runat="server" Text='<%# Bind("VOrderNum") %>'></asp:Label> 
                                        <b>Order: &nbsp;</b>
                                            <asp:TextBox BackColor="Turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" ID="VnewordernoTextbox" Enabled="false" runat="server" Width="50px"></asp:TextBox>
                                    </td>
                                    <td><b>&nbsp;&nbsp;Type:</b> </td>
                                    <td><%--<asp:TextBox ID="ordtypeTextBox" Width="30px" runat="server" Text='<%# Bind("ordtype") %>'></asp:TextBox>--%>
                                        <asp:DropDownList ID="DropDownList2" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="100px" DataTextField='<%# Bind("ordtype") %>'  runat="server" SelectedValue='<%# Bind("ordtype") %>'>
                                            <asp:ListItem Value="O">O- Original</asp:ListItem>
                                            <asp:ListItem Value="C">C- Change</asp:ListItem>
                                            <asp:ListItem Value="N">N- New</asp:ListItem>
                                            <asp:ListItem Value="Q">Q- Quality/Re-work</asp:ListItem>
                                            <asp:ListItem Value="R">R- Repeat</asp:ListItem>
                                            <asp:ListItem Value="T">T- Transfer</asp:ListItem>
                                            <asp:ListItem Value="X">X- Complete re-run</asp:ListItem>
                                        </asp:DropDownList>
                                        <%--<a href="#" onClick="typelook(); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a>--%>
                                    </td>
                                    <td style="display:none;"> <b>Quote:</b></td>
                                    <td nowrap style="display:none;">
                                        <asp:TextBox ID="RfqTextBox" AutoPostBack="true" onfocus= "javascript:focusval(this)" onblur="javascript:quoteblurval(this)" OnTextChanged="Quote_TextChanged" Width="70px" runat="server" Text='<%# Bind("VEstimate") %>'> </asp:TextBox>
                                        <a href="#" tabindex="1" onClick="quotelook(); return false"><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                    </td>
                                    <td style="display:none;"> <b><asp:Label ID="est_Label" runat="server" Text="Estimate:"></asp:Label></b></td>
                                    <td nowrap style="display:none;">
                                        <asp:TextBox ID="estimateTextBox" AutoPostBack="true" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  Width="70px" runat="server" OnTextChanged="Estimate_TextChanged" Text='<%# Bind("VEstimate") %>'> </asp:TextBox>                                                                                
                                        <a href="#" tabindex="1" onClick="estimatelook(); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                    </td>
                                    <td style="display:none;"> <b>Job#:</b></td>
                                        <td style="display:none;"><asp:Label ID="VJobTextBox" Width="40px" BackColor="Turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("VJob") %>'></asp:Label>
                                        <td style="display:none;"><asp:Label ID="VJob2TextBox" Width="20px" BackColor="Turquoise" BorderColor="black" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("VJob2") %>'></asp:Label>&nbsp;&nbsp;</td> 
                                   
                                   <td nowrap>
                                 <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
                                 <TR>
                                 <TD align=left nowrap><font size=+0><b>&nbsp; </b></font></TD>
                                 <TD vAlign="middle">
                                  <asp:linkbutton id="LinkButton1" runat="server" OnClick="LinkButton1_Click"></asp:linkbutton>
                                  </TD>
                                  <TD align="right"><font size=+0><b>&nbsp;&nbsp;Users&nbsp;&nbsp;</b></font></TD>
                                     <TD nowrap vAlign="middle" align="left">Logged as&nbsp;
                                                 <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
                                                 <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
                                                  Company:&nbsp;
                                                  <asp:label id="lblComp" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
                                   </TD>                               
                                   
                                    </TR>
                                    </TABLE>  
                                </td>   
                                   <td nowrap> <b>Last User:</b></td>
                                   <td>
                                      <asp:label ID="VUseridLabel" Width="70px" runat="server" Text='<%# Bind("VUserid") %>'></asp:label>
                                   </td>
                                   <td><b>Status:</b></td>
                                   <td align="left" style="width:150px">
                                        <asp:Label ID="VStatLabel" Width="20px" runat="server" Text='<%# Bind("VStat") %>'></asp:Label>
                                   </td>
                               </tr>
                            </table></fieldset>
                         </td>
                      </tr>
                      <tr>
                            <td>
                                <table>
                                    <tr>
                                        <td>
                                            <fieldset style="width:270px; border:solid 1px black; background-color:#EFF3FB; height:180px;">
                                                <table>
                                                    <tr>
                                                        <td align="right" style="padding-right:5px"><b>Bill To:</b></td>
                                                        <td nowrap>
                                                            <asp:TextBox ID="VCustomerTextBox" onfocus= "javascript:focusval(this)" onblur="setcust(this)" Width="120px" MaxLength="8" AutoPostBack="true" OnTextChanged="BillTo_TextChanged" runat="server" Text='<%# Bind("VCustomer") %>'></asp:TextBox>
                                                            <a href="#" tabindex="1" onClick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                        </td>
                                                    </tr>
                                                    
                                                    <tr>
                                                        <td align="right" style="padding-right:5px"><b>Contact:</b></td>
                                                        <td>
                                                            <asp:TextBox ID="VContactTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:contactblurval(this)" Width="160px" MaxLength="25" runat="server" Text='<%# Bind("VContact") %>'></asp:TextBox>
                                                        </td>
                                                    </tr>
                                                    
                                                    <tr>
                                                        <td nowrap><b>Payment Type:</b></td>
                                                        <td>
                                                            <asp:TextBox ID="VCtypeTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="8" Width="150px" runat="server" Text='<%# Bind("VCtype") %>'></asp:TextBox>
                                                        </td>
                                                        
                                                    </tr>
                                                    <tr>
                                                    <td align="right" style="padding-right:5px"><b>Account#:</b></td>
                                                    <td>
                                                        <asp:TextBox ID="VCnumTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="20" Width="150px" runat="server" Text='<%# Bind("VCnum") %>'></asp:TextBox>
                                                    </td>
                                                </tr>
                                                <tr>
                                                    <td align="right" style="padding-right:5px"><b>Ref#:</b></td>
                                                    <td>
                                                        <asp:TextBox ID="VCauthTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="30" Width="150px" runat="server" Text='<%# Bind("VCauth") %>'></asp:TextBox> <%--onblur="javascript:refblurval(this)" --%>
                                                    </td>
                                                </tr>
                                                <tr>
                                                <td align="right" style="padding-right:5px" nowrap><b>Expire:</b></td>
                                                <td> <asp:TextBox ID="VcExpTextBox" MaxLength="10" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" Width="53px" ToolTip="MM/DD/YYYY" runat="server" Text='<%# Bind("VcExp","{0:MM/dd/yyyy}") %>'></asp:TextBox>
                                                            <a href="#" onblur="FormView1_VcExpTextBox.focus()" tabindex="1" onClick="showCalendarControl(FormView1_VcExpTextBox); return false"><asp:Image ID="Image12" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                            
                                                        </td>
                                                </tr>
                                                <tr>
                                                        <td><b>Sales Rep</b></td>
                                                        <td><b>Sales Rep Name</b></td>
                                                        
                                                    </tr>
                                                    <tr>
                                                        <td> 
                                                            <asp:TextBox ID="VSmanTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px" MaxLength="3" runat="server" Text='<%# Bind("VSman") %>'></asp:TextBox>
                                                            <a href="#"  tabindex="1" onClick="salesreplook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                        </td>
                                                        <td>
                                                            <asp:TextBox ID="VSnameTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="100px" runat="server" Text='<%# Bind("VSname") %>'></asp:TextBox>
                                                        </td>
                                                        
                                                    </tr>
                                                
                                                </table>
                                            </fieldset>
                                         </td>
                                         <td>
                                            <fieldset style="width:270px; border:solid 1px black; background-color:#EFF3FB; height:180px;">
                                                <table>
                                                    <tr>
                                                        <td align="right" style="padding-right:5px"><b>Sold To:</b></td>
                                                        <td nowrap>
                                                            <asp:TextBox ID="VSoldTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="120px" MaxLength="8"  runat="server" Text='<%# Bind("VSold") %>'></asp:TextBox>
                                                            <a href="#" tabindex="1" onClick="ShipTOLook(); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                        </td>
                                                    </tr>
                                                                                                        
                                                    <tr>
                                                        <td align="right" style="padding-right:5px"><b>Carrier:</b></td>
                                                        <td>
                                                            <asp:TextBox ID="VCarrierTextBox" Width="100px"  runat="server" Text='<%# Bind("VCarrier") %>'></asp:TextBox>
                                                            <a href="#" tabindex="1" onClick="carrierlook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                        </td>
                                                    </tr>
                                                    <tr>
                                                        <td align="right" style="padding-right:5px"><b>FOB:</b></td>
                                                        <td><b>
                                                            <asp:TextBox ID="fob_codeTextBox" Visible="false" runat="server" Text='<%# Bind("VFob") %>'></asp:TextBox>
                                                            <asp:RadioButton ID="RD5" Enabled="false" GroupName="editstatus" runat="server" />Destination
                                                            <asp:RadioButton ID="RD6" Enabled="false" GroupName="editstatus" runat="server" />Origin
                                                        </b></td>
                                                    </tr>
                                                    <tr>
                                                        <td align="right" style="padding-right:5px"><b>Pay Terms:</b></td>
                                                        <td nowrap>
                                                            <asp:TextBox  ID="VTermsTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px" MaxLength="5" runat="server" Text='<%# Bind("VTerms") %>'></asp:TextBox>
                                                            <a href="#" tabindex="1" onClick="termslook(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                         </td>                                                         
                                                    </tr>
                                                    <tr><td></td>
                                                    <td>
                                                            <asp:TextBox ID="VTermdscrTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="150px" runat="server" Text='<%# Bind("VTermdscr") %>'></asp:TextBox>
                                                         </td>
                                                    </tr>
                                                     <tr>
                                                        <td colspan="2"> 
                                                        
                                                            <asp:RadioButton ID="RD1" GroupName="editstatus1" runat="server" />Prepaid
                                                            <asp:TextBox ID="FRTextBox"  Visible="false" runat="server" Text='<%# Bind("VFreight") %>'></asp:TextBox>
                                                            <asp:RadioButton ID="RD2" GroupName="editstatus1"  runat="server" />Bill
                                                            <asp:RadioButton ID="RD3" GroupName="editstatus1" runat="server" />Collect
                                                            <asp:RadioButton ID="RD4" GroupName="editstatus1" runat="server" />3rd Party
                                                        </td>
                                                    </tr>
                                                    <tr>
                                                        <td align="right" style="padding-right:5px" nowrap><b>Previous Order#:</b></td>
                                                        <td rowspan="2" valign="top">
                                                            <asp:TextBox ID="VProdTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px" MaxLength="6" runat="server" Text='<%# Bind("VProd") %>'></asp:TextBox>
                                                            <asp:RegularExpressionValidator ID="RegularExpressionValidator1" runat="server" SetFocusOnError="true" ControlToValidate="VProdTextBox" ErrorMessage="Only Integer Number" Display="Dynamic" ValidationExpression="(^([0-9]*|\d*\d{1}?\d*)$)"></asp:RegularExpressionValidator>        
                                                        </td>
                                                    </tr>
                                                    <tr><td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</td></tr>
                                                 </table>
                                             </fieldset>
                                          </td>
                                          <td>
                                            <fieldset style="width:280px; border:solid 1px black; background-color:#EFF3FB; height:180px;">
                                                <table>
                                                    <tr>
                                                        <td align="right" style="padding-right:5px" nowrap><b>Date:</b></td>
                                                        <td nowrap>
                                                            <asp:Label ID="VOrdateLabel" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  Width="80px"  runat="server" Text='<%# Bind("VOrdate","{0:MM/dd/yyyy}") %>'></asp:Label>
                                                        </td>
                                                    </tr>
                                                    <tr>
                                                        <td align="right" style="padding-right:5px" nowrap><b>Due Date:</b></td>
                                                        <td nowrap><%--<asp:TextBox ID="VDueCodeTextBox"  Width="50px"  runat="server" Text='<%# Bind("VDueCode") %>'></asp:TextBox>--%>
                                                            <asp:DropDownList ID="DropDownList1" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="100px" SelectedValue='<%# Bind("VDueCode") %>' DataTextField='<%# Bind("VDueCode") %>' runat="server">
                                                                <asp:ListItem Value="ASAP">ASAP- As Soon As Possible</asp:ListItem>
                                                                <asp:ListItem Value="NB4">NB4- Not Before</asp:ListItem>
                                                                <asp:ListItem Value="MUST"></asp:ListItem>
                                                                <asp:ListItem Value="HOT"></asp:ListItem>
                                                                <asp:ListItem Value="RUSH"></asp:ListItem>
                                                                <asp:ListItem Value="WO"></asp:ListItem>
                                                                <asp:ListItem Value="HOLD"></asp:ListItem>
                                                                <asp:ListItem Value="CR"></asp:ListItem>
                                                                <asp:ListItem Value="BY"></asp:ListItem>
                                                                <asp:ListItem Value="ON">ON</asp:ListItem>
                                                                <asp:ListItem Value="NH"></asp:ListItem>
                                                                <asp:ListItem Value="$$$">$$$- Credit Hold</asp:ListItem>
                                                                <asp:ListItem Value="AM">AM- AM Delivery</asp:ListItem>
                                                                <asp:ListItem Value="INK">INK- Waiting for Ink Info</asp:ListItem>
                                                             </asp:DropDownList>
                                                             <%--<a href="#" onClick="duelook(); return false"><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a>    --%>
                                                             <asp:TextBox ID="VDueDateTextBox" MaxLength="10" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" ToolTip="MM/DD/YYYY" Width="53px" runat="server" Text='<%# Bind("VDueDate","{0:MM/dd/yyyy}") %>'></asp:TextBox>                 
                                                                <a href="#" onblur="FormView1_VDueDateTextBox.focus()" tabindex="1" onClick="showCalendarControl(FormView1_VDueDateTextBox); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                                
                                                         </td>
                                                     </tr>
                                                     <tr>
                                                        <td align="right" style="padding-right:5px" nowrap><b>Last Ship:</b></td>
                                                        <td nowrap>
                                                            <asp:TextBox ID="VLastDateTextBox" Enabled="false" Width="80px" runat="server" Text='<%# Bind("VLastDate","{0:MM/dd/yyyy}") %>'></asp:TextBox>
                                                            <%--<a href="#" onClick="Date2look(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a>--%>
                                                        </td>
                                                    </tr>
                                                    <tr>
                                                        <td align="right" style="padding-right:5px" nowrap><b>
                                                            <asp:Label ID="prd_label" runat="server" Text="Prod Date:"></asp:Label></b>
                                                        </td>
                                                        <td nowrap>
                                                            <asp:TextBox Width="80px" ID="VProdDateTextBox" Enabled="false" runat="server" Text='<%# Bind("VProdDate","{0:MM/dd/yyyy}") %>'></asp:TextBox>
                                                            <asp:CompareValidator ID="CompareValidator3" runat="server" ControlToValidate="VProdDateTextBox" Display="dynamic" Operator="DataTypeCheck" Type="Date" SetFocusOnError="true" ErrorMessage="Invalid Date"></asp:CompareValidator>
                                                            <%--<a href="#" onClick="Date3look(); return false"><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /></a>  --%>
                                                        </td>
                                                    </tr>
                                                    <tr>
                                                        <td align="right" style="padding-right:5px" nowrap><b>Customer Po#:</b></td>
                                                        <td nowrap>
                                                            <asp:TextBox ID="VPonumTextBox" onkeyup="focusonsave()" onfocus= "javascript:focusval(this)" onblur="javascript:setsave(this)" Width="80px" MaxLength="15" runat="server" Text='<%# Bind("VPonum") %>'></asp:TextBox>
                                                            
                                                        </td>
                                                    </tr>
                                                    <tr style="display:none;">
                                                        <td align="right" style="padding-right:5px"><b>Overrun%:</b></td>
                                                        <td>
                                                            <asp:TextBox ID="VOverpctTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="6" onkeyup="overrun()" Width="50px" runat="server" Text='<%# Bind("VOverpct") %>'></asp:TextBox>
                                                            <asp:CompareValidator ID="OverCompareValidator" runat="server" ErrorMessage="Only Numbers" ControlToValidate="VOverpctTextBox" SetFocusOnError="true" Display="dynamic" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                                        </td>
                                                        
                                                    </tr>
                                                    <tr  style="display:none;">
                                                    <td align="right" style="padding-right:5px"><b>Underrun%:</b></td> <td>
                                                            <asp:TextBox ID="VUnderpctTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:underblurval(this)" MaxLength="6" onkeyup="underrun()" Width="50px" runat="server" Text='<%# Bind("VUnderpct") %>'></asp:TextBox>
                                                            <asp:CompareValidator ID="UnderCompareValidator" runat="server" ErrorMessage="Only Numbers" ControlToValidate="VUnderpctTextBox" SetFocusOnError="true" Display="dynamic" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                                        </td>
                                                    </tr>
                                                    <tr>
                                                        <td align="right" style="padding-right:5px"><b>Tax Code:</b></td>
                                                        <td nowrap>
                                                            <asp:TextBox ID="VTaxgrTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px" MaxLength="3" runat="server" Text='<%# Bind("VTaxgr") %>'></asp:TextBox>
                                                            <a href="#" tabindex="1" onClick="taxlook(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                                        </td>
                                                    </tr>
                                                </table>
                                            </fieldset>
                                        </td>
                                    </tr>
                                </table>
                           </td>
                        </tr>
                        
                        
                 </td>
              </tr>
           </table>
           
          
           <tr>
                <td>
                    <asp:Button ID="InsertButton" runat="server" Font-Bold="true" CssClass="buttonM" OnClick="InsertButon_click" Text="Save"></asp:Button>
                    <asp:Button ID="UpdateCancelButton" OnClick="Btn_Insert_cancel" CssClass="button" runat="server" CausesValidation="False" CommandName="Cancel" Text="Cancel" Font-Bold="true" ></asp:Button>
        </asp:Panel>
     </InsertItemTemplate>
  </asp:FormView></div>    
  
  <div>
  <table id="itembuttonform" runat="server">
    <tr >
    <td>
    
    <asp:Button ID="Button2" runat="server" Text="Add" CssClass="button" OnClick="addlink_button_Click" />
    <asp:Button ID="Button3" runat="server" Text="Update" CssClass="button" OnClick="updatelink_button_Click" />
    <asp:Button ID="Button4" runat="server" Text="Delete" CssClass="button"  OnClientClick="return confirm('Are you sure you want to delete Line Item record')" OnClick="viewdelete_button_Click" />
    <%--<asp:Button ID="ordUpdatButton"  runat="server"  CssClass="buttonM" Text="Update Order" OnClick="ordUpdatButton_Click" />--%>
    </td>
    </tr></table>
  <asp:GridView ID="GridView1" runat="server" AllowPaging="True" AllowSorting="True"
                    AutoGenerateColumns="False" BorderStyle="Dotted" CssClass="Grid" 
                    DataSourceID="ObjectDataSource2" EmptyDataText="" Width="900px" OnSelectedIndexChanged="GridView1_SelectedIndexChanged" DataKeyNames="OrdNo,vLine,CustPart,Item1">
                    <EmptyDataRowStyle BorderColor="Gray" BorderStyle="Dotted" BorderWidth="0px" Font-Bold="True"
                        HorizontalAlign="Center" VerticalAlign="Middle" />
                    <Columns>
                        <asp:CommandField ButtonType="Image" HeaderText="" SelectImageUrl="~/Images/sel.gif"
                            SelectText="" ShowSelectButton="True">
                            <HeaderStyle ForeColor="White" />
                            <ItemStyle ForeColor="White" />
                        </asp:CommandField>
                        <asp:BoundField DataField="vLine" HeaderText="L#" SortExpression="vLine" >
                            <ItemStyle HorizontalAlign="Center" />
                        </asp:BoundField>
                        <asp:TemplateField HeaderText="Qty" SortExpression="quantity">
                            <EditItemTemplate>
                                <asp:TextBox ID="TextBox7" runat="server" Text='<%# Bind("quantity") %>'></asp:TextBox>
                            </EditItemTemplate>
                            <ItemTemplate>
                                <asp:Label ID="Label7" runat="server" Text='<%# Bind("quantity","{0:###,##0}") %>'></asp:Label>
                            </ItemTemplate>
                            <ItemStyle HorizontalAlign="Right" />
                        </asp:TemplateField>
                        <asp:TemplateField HeaderText="Price / UOM" SortExpression="price">
                            <EditItemTemplate>
                                <asp:TextBox ID="TextBox5" runat="server" Text='<%# Bind("price") %>'></asp:TextBox>
                            </EditItemTemplate>
                            <ItemTemplate>
                                <asp:Label ID="Label5" runat="server" Text='<%# Bind("price","{0:###,##0.00}") %>'></asp:Label>
                                <br />
                                <asp:Label ID="Label6" runat="server" Text='<%# Bind("uom") %>'></asp:Label>
                            </ItemTemplate>
                            <ItemStyle HorizontalAlign="Right" Wrap="False" Width="60px" />
                            <HeaderStyle Wrap="False" />
                            <FooterStyle Wrap="False" />
                        </asp:TemplateField>
                        <asp:TemplateField HeaderText="Item # /  Part #" SortExpression="Item1">
                            <EditItemTemplate>
                                <asp:TextBox ID="TextBox1" runat="server" Text='<%# Bind("Item1") %>'></asp:TextBox>
                            </EditItemTemplate>
                            <ItemTemplate>
                                <asp:Label ID="Label2" runat="server" Text='<%# Bind("Item1") %>'></asp:Label>
                                <br /> 
                              <asp:Label ID="Label11" runat="server" Text='<%# Bind("CustPart") %>'></asp:Label>  
                            </ItemTemplate>
                            <ItemStyle Wrap="False" />
                            <HeaderStyle Wrap="False" />
                            <FooterStyle Wrap="False" />
                        </asp:TemplateField>
                        <%--<asp:TemplateField HeaderText="Cust Part#" SortExpression="CustPart">
                               <ItemTemplate>                               
                              <asp:Label ID="Labelcustpart" runat="server" Text='<%# Bind("CustPart") %>'></asp:Label>  
                            </ItemTemplate>
                            <ItemStyle Wrap="False" />
                            <HeaderStyle Wrap="False" />
                            <FooterStyle Wrap="False" />
                        </asp:TemplateField>--%>
                        <asp:TemplateField HeaderText="Item Name#/Description " SortExpression="Name1">
                               <ItemTemplate>                               
                              <asp:Label ID="Label3" runat="server" Text='<%# Bind("Name1") %>'></asp:Label> 
                              <br />
                              <asp:Label ID="Label4" runat="server" Text='<%# Bind("Dscr") %>'></asp:Label>                             
                            </ItemTemplate>
                            <ItemStyle Wrap="False" />
                            <HeaderStyle Wrap="False" />
                            <FooterStyle Wrap="False" />
                        </asp:TemplateField>
                                              
                        
                         <asp:BoundField DataField="custpo" HeaderText="Cust Po #" SortExpression="custpo" />
                        <asp:TemplateField HeaderText="Due Date" SortExpression="requestdate">
                            <EditItemTemplate>
                                <asp:TextBox ID="TextBox2" runat="server" Text='<%# Bind("requestdate") %>'></asp:TextBox>
                            </EditItemTemplate>
                            <ItemTemplate>
                                <asp:Label ID="Label8" runat="server" Text='<%# Bind("requestdate","{0:d}") %>'></asp:Label>
                            </ItemTemplate>
                            <ItemStyle Width="60px" />
                        </asp:TemplateField>
                        <asp:BoundField DataField="est-no" HeaderText="Est #" SortExpression="est-no" />
                        <asp:BoundField DataField="job-no" HeaderText="Job #" SortExpression="job-no" />
                        <asp:TemplateField HeaderText="Over-Under %" SortExpression="Dscr">
                            <ItemTemplate>                                
                             <asp:Label ID="overLabel" runat="server" Text='<%# Bind("over","{0:###,##0.00}") %>'></asp:Label>
                             /
                             <asp:Label ID="underLabel" runat="server" Text='<%# Bind("under","{0:###,##0.00}") %>'></asp:Label>
                            </ItemTemplate>
                            <ItemStyle Wrap="False" HorizontalAlign="Center" />
                            <HeaderStyle Wrap="False" />
                            <FooterStyle Wrap="False" />
                        </asp:TemplateField>
                        <asp:TemplateField HeaderText="Disc" SortExpression="discount">
                            <EditItemTemplate>
                                <asp:TextBox ID="TextBox6" runat="server" Text='<%# Bind("discount") %>'></asp:TextBox>
                            </EditItemTemplate>
                            <ItemStyle HorizontalAlign="Right" />
                            <ItemTemplate>
                                <asp:Label ID="Label10" runat="server" Text='<%# Bind("discount","{0:###,##0.00}") %>'></asp:Label>
                            </ItemTemplate>
                        </asp:TemplateField>
                        <asp:TemplateField HeaderText="Total Price" SortExpression="extprice">
                            <EditItemTemplate>
                                <asp:TextBox ID="TextBox4" runat="server" Text='<%# Bind("extprice") %>'></asp:TextBox>
                            </EditItemTemplate>
                            <ItemStyle HorizontalAlign="Right" Width="65px" />
                            <ItemTemplate>
                                <asp:Label ID="Label9" runat="server" Text='<%# Bind("extprice","{0:###,##0.00}") %>'></asp:Label>
                            </ItemTemplate>
                        </asp:TemplateField>
                        <asp:CheckBoxField DataField="taxable" HeaderText="Tax" SortExpression="taxable" />
                        
                        <asp:TemplateField HeaderText="reckey" Visible="false">
                            
                            <ItemTemplate>
                                <asp:Label ID="Label_vReckey" runat="server" Text='<%# Bind("vReckey") %>'></asp:Label>
                            </ItemTemplate>
                        </asp:TemplateField>
                       
                    </Columns>
                    <RowStyle CssClass="shade" />
                    <SelectedRowStyle CssClass="GridSelected" BackColor="Yellow" />
                    <HeaderStyle BackColor="Teal" CssClass="gridrowhdr" ForeColor="White" HorizontalAlign="Center"
                        VerticalAlign="Middle" Wrap="False" />
                    <AlternatingRowStyle CssClass="GridItemOdd" />
                    
                </asp:GridView>
  </div>
  
  
  
  <div>   
    <br /><fieldset runat="server" id="orderfielset" style="width:400px;">
    <asp:FormView ID="FormView2" runat="server" CellPadding="4" DataSourceID="ObjectDataSource3" OnDataBound="FormView2_DataBound"
         >
        <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
        <EditRowStyle  />
        
         <ItemTemplate>
            
            <table class="shade">
            <tr>
            <td  align="right"><b>Bill Freight: </b></td>
            <td ><b> <asp:CheckBox ID="BillFreCheckBox" runat="server" Checked='<%# Bind("BillF") %>'
                Enabled="false" /></b></td>
            <%--<td  align="right"><b>Weight: </b></td>
            <td ><b><asp:Label ID="WeightLabel" runat="server" Text='<%# Bind("Weight1") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label> </b></td>--%>
            <td  align="right"><b>Freight: </b></td>
            <td ><b><asp:Label ID="FreightLabel" runat="server" Text='<%# Bind("Freig") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label> </b></td>
            
            </tr>
            <tr><td></td><td></td>
            <td  align="right"><b>Tax1: </b></td>
            <td ><b> <asp:Label ID="Tax1Label" runat="server" Text='<%# Bind("Taxes") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>
            </tr>
            <tr>            
            <td></td><td></td>
            <td  align="right"><b>Order Total: </b></td>
            <td ><b><asp:Label ID="TotalLabel" runat="server" Text='<%# Bind("Tot") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label> </b></td>
            </tr>
            <%--<tr>
            <td ><b>Commissions: </b></td>
            <td ><b><asp:Label ID="commLabel" runat="server" Text='<%# Bind("Com") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label> </b></td>
            <td  align="right"><b> </b></td>
            <td ><b> </b></td>
            <td align="right"><b>Cost: </b></td>
            <td ><b><asp:Label ID="costLabel" runat="server" Text='<%# Bind("Cos") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label> </b></td>                                             
            
            </tr>--%>
            <tr><td colspan="6">
            <asp:Button ID="UpdateButton" runat="server" Visible="false" CssClass="button" CausesValidation="True" CommandName="edit"
                Text="Update">
            </asp:Button>
            <asp:Button ID="CalFrightButton" runat="server" Visible="false" CssClass="button" OnClick="Freight_click" Text="Fright" OnClientClick="return confirm('Calculate Fright?')" > </asp:Button>
            </td></tr>
             
            </table>
           
        </ItemTemplate>
        
        <EditItemTemplate>
           <table class="shade">
            <tr><td align="right"><b>Weight:</b></td>
            <td><asp:TextBox ID="Weight1TextBox" runat="server" MaxLength="9" Width="70px" Text='<%# Bind("Weight1") %>'> </asp:TextBox></td>
            <td></td><td></td>
            <td align="right"><b>Tax:</b></td>                
            <td><asp:TextBox ID="TaxesTextBox" runat="server" Width="70px" Text='<%# Bind("Taxes") %>' Enabled="false"></asp:TextBox></td></tr>                                    
            <tr><td align="right"><b>Fright:</b></td>
            <td><asp:TextBox ID="FreigTextBox" runat="server" MaxLength="9" Width="70px" onfocus="get_prev_fright()" onkeyup="calFright()" Text='<%# Bind("Freig") %>'></asp:TextBox></td>
            <td align="right"><b>Bill Fright:</b></td>
            <td><asp:CheckBox ID="BillFCheckBox" runat="server" onclick="calBFright()" Checked='<%# Bind("BillF") %>' /></td>
            <td align="right"><b>Order Total:</b></td>
            <td><asp:TextBox ID="TotTextBox" runat="server" Text='<%# Bind("Tot") %>' Width="70px" Enabled="false"> </asp:TextBox></td></tr>                                    
            <tr><td align="right"><b>Commissions:</b></td>
            <td><asp:TextBox ID="ComTextBox" runat="server" MaxLength="9" Width="70px" onfocus="get_prev_val()" onkeyup="calcom()" Text='<%# Bind("Com") %>'></asp:TextBox></td>            
            <td></td><td></td>
            <td align="right"><b>Order Cost:</b></td>
            <td> <asp:TextBox ID="CosTextBox" runat="server" Text='<%# Bind("Cos") %>' Width="70px" Enabled="false"></asp:TextBox></td>                                                                       
            <td style="display:none"><b><asp:TextBox ID="FrtaxTextBox" runat="server" Text='<%# Eval("v-fr-tax") %>' ></asp:TextBox> </b></td> 
            <td style="display:none"><b><asp:TextBox ID="TaxrateTextBox" runat="server" Text='<%# Eval("v-tax-rate") %>' ></asp:TextBox> </b></td> 
            <td style="display:none"><b><asp:TextBox ID="FrttaxrateTextBox" runat="server" Text='<%# Eval("v-frt-tax-rate") %>' ></asp:TextBox> </b></td> 
            </tr>
             <tr><td>
             <asp:Button ID="UpdateButton" OnClick="Update_click" runat="server" CssClass="buttonM" CausesValidation="True" Text="Save"> </asp:Button>
            <asp:Button ID="UpdateCancelButton" CssClass="buttonM" runat="server" CausesValidation="False" CommandName="Cancel"
                Text="Cancel">
            </asp:Button></td></tr> </table> 
        </EditItemTemplate>
              
    </asp:FormView> </fieldset>
    <asp:ObjectDataSource ID="ObjectDataSource3" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="OrderTot" TypeName="Order">
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
            <asp:SessionParameter Name="prmOrderNum" SessionField="order_estweb_app" DefaultValue="" Type="String" />
            <asp:Parameter Name="prmWeight" Type="Decimal" />            
            <asp:Parameter Name="prmTax" Type="Decimal" />
            <asp:Parameter Name="prmFreight" Type="Decimal" />
            <asp:Parameter Name="prmFbill" Type="String" />
            <asp:Parameter Name="prmRevenue" Type="Decimal" />
            <asp:Parameter Name="prmCost" Type="Decimal" />
            <asp:Parameter Name="prmComm" Type="Decimal" />            
        </SelectParameters>
    </asp:ObjectDataSource>
  
  
  </div> 
  
  
  <div><br />
  
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}" SelectMethod="SelectEstEntry" TypeName="orderentry">
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
                <asp:Parameter Name="prmExt" Type="String" />
                <asp:SessionParameter Name="prmOrderNum" DefaultValue="" SessionField="order_estweb_app" Type="Int32" />           
                <asp:Parameter Name="prmCustomer" Type="String" />
                <asp:Parameter Name="prmUserid" Type="String" />
                <asp:Parameter Name="prmStat" Type="String" />
                <asp:Parameter Name="prmSold" Type="String" />
                <asp:Parameter Name="prmOrdate" Type="DateTime" />
                <asp:Parameter Name="prmSoldName" Type="String" />
                <asp:Parameter Name="prmDueCode" Type="String" />
                <asp:Parameter Name="prmDueDate" Type="DateTime" />
                <asp:Parameter Name="prmCustAddr" Type="String" />
                <asp:Parameter Name="prmSoldAddr" Type="String" />        
                <asp:Parameter Name="prmLastDate" Type="DateTime" />
                <asp:Parameter Name="prmcustAddr2" Type="String" />
                <asp:Parameter Name="prmSoldAddr2" Type="String" />
                <asp:Parameter Name="prmProdDate" Type="string" />
                <asp:Parameter Name="prmCity" Type="String" />
                <asp:Parameter Name="prmState" Type="String" />
                <asp:Parameter Name="prmZip" Type="String" />
                <asp:Parameter Name="prmSoldCity" Type="String" />
                <asp:Parameter Name="prmSoldState" Type="String" />
                <asp:Parameter Name="prmSoldZip" Type="String" />
                <asp:Parameter Name="prmPonum" Type="String" />
                <asp:Parameter Name="prmContact" Type="String" />
                <asp:Parameter Name="prmOverpct" Type="Decimal" />
                <asp:Parameter Name="prmUnderpct" Type="Decimal" />
                <asp:Parameter Name="prmTerms" Type="String" />
                <asp:Parameter Name="prmTermdscr" Type="String" />
                <asp:Parameter Name="prmProd" Type="Int32" />
                <asp:Parameter Name="prmTaxgr" Type="String" />
                <asp:Parameter Name="prmFreight" Type="String" />
                <asp:Parameter Name="prmCarrier" Type="String" />
                <asp:Parameter Name="prmFob" Type="String" />
                <asp:Parameter Name="prmSman" Type="String" />
                <asp:Parameter Name="prmSname" Type="String" />               
                <asp:Parameter Name="prmSman2" Type="String" />
                <asp:Parameter Name="prmSname2" Type="String" />                
                <asp:Parameter Name="prmSman3" Type="String" />
                <asp:Parameter Name="prmSname3" Type="String" />                
                <asp:Parameter Name="prmCtype" Type="String" />
                <asp:Parameter Name="prmcExp" Type="String" />
                <asp:Parameter Name="prmCnum" Type="String" />
                <asp:Parameter Name="prmCauth" Type="String" />
                <asp:Parameter Name="prmCustName" Type="String" />
                <asp:Parameter Name="prmType" Type="String" />
                <asp:Parameter Name="prmLine" Type="int32" />
                <asp:Parameter Name="prmWhis" Type="string" />
                <asp:Parameter Name="VRowid" Type="int64" />
                <asp:Parameter Name="prmJob" Type="String" />
                <asp:Parameter Name="prmJob2" Type="int32" />
                <asp:Parameter Name="prmEst" Type="string" />
                <asp:Parameter Name="prmSales1" Type="Decimal" />
                <asp:Parameter Name="prmSales2" Type="Decimal" />
                <asp:Parameter Name="prmSales3" Type="Decimal" />
                <asp:Parameter Name="prmComm1" Type="Decimal" />
                <asp:Parameter Name="prmComm2" Type="Decimal" />
                <asp:Parameter Name="prmComm3" Type="Decimal" />
                <asp:Parameter Name="prmQuote" Type="int32" />                
                <asp:Parameter Name="prmQty" Type="int32" />   
                <asp:Parameter Name="prmPrice" Type="decimal" /> 
                <asp:Parameter Name="prmUom" Type="string" />            
        </SelectParameters>
    </asp:ObjectDataSource>
    
    <asp:ObjectDataSource ID="ObjectDataSource2" OldValuesParameterFormatString="original_{0}" runat="server" SelectMethod="Selectview" TypeName="Order" >
                    <SelectParameters>
                        <asp:Parameter Name="prmUser" Type="String" />
                        <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
                        <asp:SessionParameter DefaultValue="" Name="prmOrderNum" SessionField="order_estweb_app"
                            Type="String" />
                        <asp:SessionParameter DefaultValue="" Name="prmItemNum" SessionField="item_app" Type="String" />
                    </SelectParameters>
                </asp:ObjectDataSource>
        
        <br /></div>  </div>
     </td></tr></table> 
       
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
</body>
</html>