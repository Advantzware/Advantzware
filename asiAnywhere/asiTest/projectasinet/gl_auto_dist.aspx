<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="gl_auto_dist" Codebehind="~/gl_auto_dist.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Generate Auto Distribution</title>  
         
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    <%--<script language = JavaScript>--%>
    
    <script language="VBScript">
    Function makeMsgBox(title,message,icon,buttons,defButton,mode)
        butVal = icon + buttons + defButton + mode
        makeMsgBox = MsgBox(message,butVal,title)
    End Function

</script>
<script language="javascript">
    function confirmPost() {
        var retVal = makeMsgBox("Confirmation", "Are you sure to distribute to cost account?", 48, 4, 256, 4096);
        if (retVal == 6) {
            document.forms[0].HiddenFieldPost.value = "Yes";
        }
        else {
            document.forms[0].HiddenFieldPost.value = "No";
        }
    }
</script>
    
    <script language = JavaScript>  
    
    var bSelected=false;
    function ChSel()
    {
        var theForm = document.forms['frmList'];
        if (!theForm) theForm = document.frmList;
        bSelected = !bSelected; 
        var i;
        for (i=0;i<theForm.chDelete.length;++i) theForm.chDelete[i].checked=bSelected;
    } 
    
    function OnKeyDown()
    {
        e = window.event;
        if (e.keyCode == 13)
        {
            e.cancel = true;
            var theForm = document.forms['frmList'];
            if (!theForm) theForm = document.frmList;                
            theForm.btnSearch.click();              
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
    }
        

        function jrnltext() {
            var jrnl = document.getElementById("transTextBox");
            jrnl.focus();
        }
        function periodtext() {
            var vend = document.getElementById("transTextBox");
            var pertext = document.getElementById("perTextBox");
            pertext.value = (vend.value).substring(0, 2);
        }



        function samevalue() {
            var beginc = document.getElementById("TextBox1");
            var endc = document.getElementById("TextBox2");
            endc.value = beginc.value;
        }

        function samevalue2() {
            var beginc = document.getElementById("TextBox1");
            var endc = document.getElementById("TextBox2");
            if (endc.value != beginc.value) {
                alert("Begin and End Customer Value must be same");
                endc.value = beginc.value;
                endc.focus();
            }
        }

        function contactcustomerlook() {
            var NewWindow = window.open("contact_customer_lookup.aspx", "ContactCustomerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }

        function ContactCustomerLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11) {
            document.forms[0].TextBox1.value = ReturnObj1;
            //document.forms[0].TextBox2.value = ReturnObj1;


        }

        function contactcustomerlook2() {
            var NewWindow = window.open("contact_customer_copylookup.aspx", "ContactCustomerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }

        function ContactCustomerCopyLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11) {
            //document.forms[0].TextBox1.value = ReturnObj1;
            document.forms[0].TextBox2.value = ReturnObj1;


        }

        function salesreplook() {
            var NewWindow = window.open("salesrep_lookup.aspx", "SalesRepLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function SalesRepLookup(ReturnObj1, ReturnObj2) {
            document.forms[0].besmanTextBox.value = ReturnObj1;
        }


        function smancopylook1() {
            var NewWindow = window.open("sman_copylookup.aspx", "smancopyLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function smancopyLookup(ReturnObj1, ReturnObj2) {
            document.forms[0].endsmanTextBox.value = ReturnObj1;
        }

        function Relook() {
            var item1 = document.getElementById("TextBox1").value;
            var NewWindow = window.open("reorder_item_lookup.aspx?item=" + item1 + "", "ReItemLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }

        function ReLookup(ReturnObj1) {
            document.forms[0].beitemTextBox.value = ReturnObj1;
        }
        function Relook2() {
            var item2 = document.getElementById("TextBox2").value;
            var NewWindow = window.open("reorder_item_lookup2.aspx?item1=" + item2 + "", "ReItemLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }

        function ReLookup2(ReturnObj1) {
            document.forms[0].enditTextBox.value = ReturnObj1;
        }

        function Datelook() {
            var NewWindow = window.open("date_lookup.aspx", "DateLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function Datelookup(obj) {
            document.forms[0].bedateTextBox.value = obj;
        }

        function Datelook1() {
            document.forms[0].bedateTextBox.value = "";
            Datelook();
        }
        function Date2look() {
            var NewWindow = window.open("date_lookup2.aspx", "DateLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function Datelookup2(obj) {
            document.forms[0].enddateTextBox.value = obj;
        }

        function Datelook2() {
            document.forms[0].enddateTextBox.value = "";
            Date2look();
        }
        function datevalidate() {
            var date = document.getElementById("bedateTextBox").value;

            if (date.length > 1 && date.length < 3 && date.indexOf('/') != 1) {
                document.getElementById("bedateTextBox").value = date + "/";
            }
            if (date.length > 4 && date.length < 6 && date.indexOf('/') != 3) {
                document.getElementById("bedateTextBox").value = date + "/";
            }
            var date2 = document.getElementById("enddateTextBox").value;

            if (date2.length > 1 && date2.length < 3 && date2.indexOf('/') != 1) {
                document.getElementById("enddateTextBox").value = date2 + "/";
            }
            if (date2.length > 4 && date2.length < 6 && date2.indexOf('/') != 3) {
                document.getElementById("enddateTextBox").value = date2 + "/";
            }
        }
        function lnk_pdf_Click() {
            var pdfile = document.getElementById("fdgfd").innerText;
            var NewWindow = window.open("print_download_list.aspx", "OrderListNotes", "width=600,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        var newvend = "";
        function vendorlook(obj1) {
            newvend = obj1;
            var NewWindow = window.open("corvend_lookup.aspx", "VendLookup", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }

        function VendLookup(ReturnObj1) {
            if (newvend == "1")
                document.forms[0].begvendTextBox.value = ReturnObj1;
            else
                document.forms[0].endvendTextBox.value = ReturnObj1;

        }
        var buy;
        function buyerlookup(obj2) {
            buy = obj2;
            var NewWindow = window.open("buyerlook.aspx", "buyerLookup", "width=500,height=400,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function buyerlook(ReturnObj1) {
            if (buy == "1") {
                document.forms[0].begbuyerTextBox.value = ReturnObj1;
            }
            else {
                document.forms[0].endbuyerTextBox.value = ReturnObj1;
            }

        }
        var stat;
        function statecodelook(obj3) {
            stat = obj3;
            var NewWindow = window.open("statecode_lookup.aspx", "StateCodeLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }

        function StateCodeLookup(ReturnObj1) {
            if (stat == "1") {
                document.forms[0].begstatTextBox.value = ReturnObj1;
            }
            else {
                document.forms[0].endstatTextBox.value = ReturnObj1;
            }
        }
        var vtyp;
        function vendortypelook(obj4) {
            vtyp = obj4;
            var NewWindow = window.open("vendtype_lookup.aspx", "vendtypeLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }
        function VendTypeLookup(ReturnObj1) {
            if (vtyp == "1") {
                document.forms[0].begvendTextBox.value = ReturnObj1;
            }
            else {
                document.forms[0].endvendTextBox.value = ReturnObj1;
            }
        }
        var act;
        function AccountLook(obj) {
            act = obj;
            var NewWindow = window.open("accountlook.aspx", "AccountLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
        }

        function AccountLookup(ReturnObj1) {
            if (act == "1") {
                document.forms[0].begactTextBox.value = ReturnObj1;
            }
            else {
                document.forms[0].endactTextBox.value = ReturnObj1;
            }          
        }
    

    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='begstatTextBox'>   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>
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
         <asp:HiddenField ID="HiddenFieldPost" runat="server" /> 
          <asp:Label ID="Label1" runat="server" ForeColor="red" Font-Bold="true"></asp:Label>
          
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=center nowrap><font size=+0><b>Generate Auto Distribution &nbsp;</b></font></TD>
          <TD vAlign="middle" nowrap >
            <asp:linkbutton id="hlkBackToMenu" OnClick="Back_tomenu_Click" runat="server" >Back to menu</asp:linkbutton>
          </TD>
          <TD vAlign="middle" align="center" nowrap >Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx"></asp:hyperlink>
          &nbsp;<b>Company:</b>&nbsp;<asp:Label ID="labelcompany" runat="server" Text="Label"></asp:Label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
      
      <table class="shade" width="640px">       
      <tr><td><table>
        <tr><td align="right" style="padding-right:5px"><b>Beginning Posting Date:</b></td>
          <td>
              <asp:TextBox ID="begdateTextBox" Width="100px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" runat="server"></asp:TextBox>
              <a href="#" onblur="document.getElementById('begdateTextBox').focus()" tabindex="1" onClick="showCalendarControl(begdateTextBox); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
          <td align="right" style="padding-right:5px"><b>Ending Posting Date:</b></td>
          <td>
              <asp:TextBox ID="enddateTextBox" Width="100px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" runat="server"></asp:TextBox>
              <a href="#" onblur="document.getElementById('enddateTextBox').focus()" tabindex="1" onClick="showCalendarControl(enddateTextBox); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
              </td></tr>
              <tr><td align="right" style="padding-right: 5px"><b>Beginning Acct#:</b></td><td>
          <asp:TextBox ID="begactTextBox"  width="160px" MaxLength="25" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="AccountLook(1); return false"><asp:Image ID="Image13" runat="server" ImageUrl="images/lookup_icon.gif" /></a>         
          </td>
      <td align="right" style="padding-right: 5px"><b>Ending Acct#:</b></td>
      <td nowrap>
        <asp:TextBox ID="endactTextBox" width="160px" MaxLength="25" runat="server"></asp:TextBox>
        <a href="#" tabindex="1" onClick="AccountLook(2); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>        
      </td>
        </tr>
        </table></td></tr>
        <tr><td align="center"><table>
        
        <tr><td align="right" style="padding-right:5px"><b>Transaction Date:</b></td>
          <td>
              <asp:TextBox ID="transTextBox" Width="100px" onfocus="this.select();javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );periodtext()" runat="server"></asp:TextBox>
              <a href="#" onblur="document.getElementById('transTextBox').focus()" tabindex="1" onClick="showCalendarControl(transTextBox); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td></tr>
              
              <tr><td align="right" style="padding-right: 5px"><b>Period:</b></td>
          <td nowrap><asp:TextBox ID="perTextBox" MaxLength="2" focus="jrnltext()" BackColor="Turquoise"  Width="50px" runat="server"></asp:TextBox>          
          </td>
       
          </tr>
        
        <tr><td align="right" style="padding-right: 5px"><b>Report?:</b></td>
        <td colspan="2"><b><asp:RadioButtonList ID="RadioButtonList1" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">             
                 
                 <asp:ListItem  Value="D"    Text="Detail" />
                 <asp:ListItem Value="S"    Text="Summary" />                
                 
         </asp:RadioButtonList></b>  </td></tr>
        
      </table></td></tr>
        
       
          
              
       
        
       
        <%--<tr><td colspan="2"><b> &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; 
        &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <b>OutPut To?:</b> <asp:RadioButtonList ID="RadioButtonList3" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
             
                 
                 <asp:ListItem   Value="no"   Text="Text File" />
                  <asp:ListItem  Value="yes"   Text="Excel" />
                 
         </asp:RadioButtonList></b></td></tr>--%>
         <tr><td colspan="2">
             &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; <asp:Button ID="submitbutton" OnClick="submitbutton_click" OnClientClick="confirmPost()" runat="server" class="buttonM" Text="Submit" />
             &nbsp;&nbsp;&nbsp;&nbsp;
          <asp:Label ID="OutPutFile" runat="server" Text="Print:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
              <asp:HyperLink ID="HyperLink1" runat="server" Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink>              
             </td>
          </tr>   
        
        </table>
          <asp:FormView ID="FormView1" Visible="False" runat="server" 
              DataSourceID="ObjectDataSource1">                                                                                                                                                             
              
             
              <ItemTemplate>
                  distpost:
                  <asp:Label ID="distpostLabel" runat="server" 
                      Text='<%# Bind("distpost") %>'></asp:Label><br />                 
                  genat:
                  <asp:Label ID="genatLabel" runat="server" Text='<%# Bind("genat") %>' />
                  <br />
              </ItemTemplate>                            
          </asp:FormView>
        
        
         
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
               SelectMethod="GenPostDistrib" TypeName="ledger">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmAction" DefaultValue="journal" Type="String" />
                  <asp:Parameter Name="prmBegacc" Type="String" />
                  <asp:Parameter Name="prmEndacc" Type="String" />
                  <asp:Parameter Name="prmBegindate" Type="String" />
                  <asp:Parameter Name="prmEnddate" Type="String" />
                  <asp:Parameter Name="prmOut" Type="String" />
                  <asp:Parameter Name="prmReport" Type="String" />
                  <asp:Parameter Name="prmPost" Type="String" /> 
                  <asp:Parameter Name="prmTrnsDate" Type="String" />
                  <asp:Parameter Name="prmPeriod" Type="String" />                  
              </SelectParameters>
          </asp:ObjectDataSource>
          
     
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

