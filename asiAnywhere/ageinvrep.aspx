<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="ageinvrep" Codebehind="ageinvrep.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Aged Inventory Report</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
    <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    
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
    
 function samevalue()
    {
    var beginc=document.getElementById("BeCustTextBox");
    var endc=document.getElementById("EndCustTextBox");
    endc.value=beginc.value;
    }
    
    function samevalue2()
    {
    var beginc=document.getElementById("TextBox5");
    var endc=document.getElementById("TextBox6");
    if(endc.value!=beginc.value)
    {
    /*alert("Begin and End Customer Value must be same");
    endc.value=beginc.value;
    endc.focus();
    */
    }
    }

    var cval = "";

    function contactcustomerlook(val) {
        cval = val;
  var NewWindow = window.open("contact_customer_lookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11) {
    if (cval == 1) {
        document.forms[0].BeCustTextBox.value = ReturnObj1;
        document.forms[0].EndCustTextBox.value = ReturnObj1;
        document.forms[0].EndCustTextBox.focus();
    }
    else if (cval == 2) {
    document.forms[0].EndCustTextBox.value = ReturnObj1;
    document.forms[0].EndCustTextBox.focus();
    }    
}

function Relook(){ 
  var NewWindow = window.open("reorder_item_lookup.aspx","ReItemLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ReLookup(ReturnObj1){
    document.forms[0].TextBox3.value = ReturnObj1;
    document.forms[0].TextBox3.focus();
}
function Relook2(){ 
  var NewWindow = window.open("reorder_item_lookup2.aspx","ReItemLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ReLookup2(ReturnObj1){
    document.forms[0].TextBox4.value = ReturnObj1;
    document.forms[0].TextBox4.focus();
}

function LocationLook() {
    var NewWindow = window.open("location_lookup.aspx", "LocationLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function LocationLookUp(ReturnObj1) {
    document.forms[0].BeWareTextBox.value = ReturnObj1;
    document.forms[0].BeWareTextBox.focus();
}
function Location2Look() {
    var NewWindow = window.open("location_lookup2.aspx", "LocationLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Location2LookUp(ReturnObj1) {
    document.forms[0].EndWareTextBox.value = ReturnObj1;
    document.forms[0].EndWareTextBox.focus();
}

function salesreplook(){ 
  var NewWindow = window.open("salesrep_lookup.aspx","SalesRepLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function SalesRepLookup(ReturnObj1,ReturnObj2){
    document.forms[0].BeSmanTextBox.value = ReturnObj1;
    document.forms[0].BeSmanTextBox.focus();
  }
  function smancopylook1(){ 
  var NewWindow = window.open("sman_copylookup.aspx","smancopyLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function smancopyLookup(ReturnObj1,ReturnObj2){
    document.forms[0].EndSmanTextBox.value = ReturnObj1;
    document.forms[0].EndSmanTextBox.focus();
}

function fglook() {
    var NewWindow = window.open("fgitem_lookup.aspx", "FGLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function FGLookup(ReturnObj1) {
    document.forms[0].BeItemTextBox.value = ReturnObj1;
    document.forms[0].BeItemTextBox.focus();
}

function itemlook() {
    var NewWindow = window.open("fgitem_translookup.aspx", "FGLookWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function FG2Lookup(ReturnObj1) {
    document.forms[0].EndItemTextBox.value = ReturnObj1;
    document.forms[0].EndItemTextBox.focus();
}
function job1look() {
    var NewWindow = window.open("job1_lookup.aspx", "Job1LookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Job1Lookup(ReturnObj1) {
    document.forms[0].BeJob1TextBox.value = ReturnObj1;
    document.forms[0].BeJob1TextBox.focus();
}
function job1translook() {
    var NewWindow = window.open("job1_translookup.aspx", "Job1LookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Job1transLookup(ReturnObj1) {
    document.forms[0].EndJob1TextBox.value = ReturnObj1;
    document.forms[0].EndJob1TextBox.focus();
}
    

    </script> 

      <link href="include/tree.css" rel="stylesheet" type="text/css" />
      <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='TextBoxdate'>   
        <hd:header id="Header1" runat="server"></hd:header>
      <div >       
            
                       
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=center nowrap><font size=+0><b>Aged Inventory Report&nbsp;</b></font></TD>
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
      </TABLE></div><div>
      <asp:HiddenField ID="HiddenField1" runat="server" />
         <asp:HiddenField ID="HiddenField2" runat="server" />
         <asp:HiddenField ID="HiddenField3" runat="server" />
         <asp:HiddenField ID="HiddenField4" runat="server" />
         <asp:HiddenField ID="HiddenField5" runat="server" />
         <asp:HiddenField ID="HiddenField6" runat="server" />
         <asp:HiddenField ID="HiddenField7" runat="server" />
                  
          <asp:Label ID="Label1" Font-Bold="true" ForeColor="red" runat="server" ></asp:Label>&nbsp;
          
          <asp:FormView ID="FormView2" Visible="false" runat="server" DataSourceID="ObjectDataSource2">
                            
              <ItemTemplate>                  
                  <asp:Label ID="CustLabel" Visible="false" runat="server" Text='<%# Bind("Cust") %>'></asp:Label><br />
              </ItemTemplate>
          </asp:FormView>
          <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="FillAlphabeticList" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmComp" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
           <table class="shade" width="520px">
           </table>
      <table class="shade" style="width: 620px">
     <tr><td align="right" style="padding-right: 5px; "><b>As of:</b></td>
          <td><asp:TextBox ID="TextBoxdate"  Width="100px" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"  runat="server"></asp:TextBox>
          <a href="#" onblur="document.getElementById('TextBoxdate').focus()"  tabindex="1" onClick="showCalendarControl(TextBoxdate); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>  </td>          
        </tr>
      
      <tr><td align="right" style="padding-right: 5px; " nowrap><b>Begining Salesman#:</b></td>
          <td nowrap><asp:TextBox ID="BeSmanTextBox" MaxLength="3" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="salesreplook(); return false"><asp:Image ID="FGLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
        <td align="right" style="padding-right: 5px" nowrap><b>Ending Salesman#:</b></td>
          <td nowrap style="width: 187px"><asp:TextBox ID="EndSmanTextBox" Width="100px" MaxLength="3" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="smancopylook1(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
          </tr>
          
            <tr> <td align="right" style="padding-right: 5px; " nowrap><b>Begining Customer:</b></td>
          <td><asp:TextBox ID="BeCustTextBox" MaxLength="8" Width="100px" onkeyup="samevalue()"  runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="contactcustomerlook(1); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
          
          
        <td align="right" style="padding-right: 5px" nowrap><b>Ending Customer:</b></td>
          <td style="width: 187px">
            <asp:TextBox ID="EndCustTextBox" MaxLength="8" Width="100px" runat="server"></asp:TextBox>            
            <a href="#" tabindex="1" onClick="contactcustomerlook(2); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td></tr>
        
        
        <tr><td align="right" style="padding-right: 5px; " nowrap><b>Begining Item:</b></td>
          <td>
            <asp:TextBox ID="BeItemTextBox"   runat="server" MaxLength="15" Width="100px"></asp:TextBox>
            <a href="#"  tabindex="1" onClick="fglook(); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
          </td>
      <td nowrap align="right" style="padding-right: 5px"><b>Ending Item:</b></td>
       <td  style="padding-right: 5px">
            <asp:TextBox ID="EndItemTextBox" MaxLength="15"  runat="server" Width="100px"></asp:TextBox>
              <a href="#"   tabindex="1" onClick="itemlook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
      </tr>
      <tr><td align="right" style="padding-right: 5px; " nowrap><b>Begining Job:</b></td>
          <td>
            <asp:TextBox ID="BeJob1TextBox"  runat="server" Width="70px"></asp:TextBox>
             <asp:TextBox ID="BeJob2TextBox" MaxLength="3" runat="server" Width="25px"></asp:TextBox>
             <a href="#"   tabindex="1" onClick="job1look(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
      <td nowrap align="right" style="padding-right: 5px"><b>Ending Job:</b></td>
       <td  style="padding-right: 5px">
            <asp:TextBox ID="EndJob1TextBox"  MaxLength="6"  runat="server" Width="70px"></asp:TextBox>
            <asp:TextBox ID="EndJob2TextBox" MaxLength="3"   runat="server" Width="25px"></asp:TextBox>
              <a href="#"   tabindex="1" onClick="job1translook(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
      </tr>
      <tr><td align="right" style="padding-right: 5px; " nowrap><b>Begining Warehouse:</b></td>
          <td>
            <asp:TextBox ID="BeWareTextBox"    runat="server" Width="100px"></asp:TextBox>
            <a href="#" tabindex="1" onClick="LocationLook(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>          
          </td>
      <td nowrap align="right" style="padding-right: 5px"><b>Ending Warehouse:</b></td>
       <td  style="padding-right: 5px">
            <asp:TextBox ID="EndWareTextBox" MaxLength="5"   runat="server" Width="100px"></asp:TextBox>
              <a href="#" tabindex="1" onClick="Location2Look(); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a></td>
      </tr>
      <tr><td align="right" style="padding-right: 5px; " nowrap><b>Inventory Class(es):</b></td>
          <td colspan="3">
            <asp:TextBox ID="ClassTextBox"    runat="server" Width="385px"></asp:TextBox>                       
          </td>      
      </tr>
      <tr><td></td><td colspan="3"><b>(Blank for all Inventory Classes)</b> 
      </td></tr>
      <tr><td align="right" style="padding-right: 5px"><b>Aged Days1:</b></td>
      <td nowrap colspan="3">
      <asp:TextBox ID="day1TextBox" Width="40px"  runat="server" ></asp:TextBox> 
      <asp:CompareValidator ID="CompareValidator1" ControlToValidate="day1TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Integer" SetFocusOnError="true" runat="server" ErrorMessage="Enter Integer Value"></asp:CompareValidator>
      &nbsp;&nbsp;<b>2:</b>
      <asp:TextBox ID="day2TextBox" Width="40px" runat="server" ></asp:TextBox>
      <asp:CompareValidator ID="CompareValidator2" ControlToValidate="day2TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Integer" SetFocusOnError="true" runat="server" ErrorMessage="Enter Integer Value"></asp:CompareValidator>
      &nbsp;&nbsp;<b>3:</b>
      <asp:TextBox ID="day3TextBox" Width="40px" runat="server" ></asp:TextBox>
      <asp:CompareValidator ID="CompareValidator3" ControlToValidate="day3TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Integer" SetFocusOnError="true" runat="server" ErrorMessage="Enter Integer Value"></asp:CompareValidator>
      &nbsp;&nbsp;<b>4:</b>
      <asp:TextBox ID="day4TextBox" Width="40px" runat="server" ></asp:TextBox>
      <asp:CompareValidator ID="CompareValidator4" ControlToValidate="day4TextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Integer" SetFocusOnError="true" runat="server" ErrorMessage="Enter Integer Value"></asp:CompareValidator>
      </td>
      </tr>
      <tr><td align="right" style="padding-right: 5px"><b>Print?:</b></td>
      <td colspan="3">
           <asp:RadioButtonList ID="RadioButtonList_show" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">                              
                 <asp:ListItem  Value="Quantity"    Text="Quantity" />
                  <asp:ListItem   Value="Value"  Text="Value" />                                   
         </asp:RadioButtonList>&nbsp;&nbsp;&nbsp;&nbsp;
          <asp:RadioButtonList ID="RadioButtonList_price" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">                              
                 <asp:ListItem  Value="Avg"    Text="Average Price" />
                  <asp:ListItem   Value="Sell"  Text="Sell Price" />                                  
         </asp:RadioButtonList>
      </td>
      </tr>
      <tr><td align="right" style="padding-right: 5px"><b>Print?:</b></td>
      <td colspan="3">
           <asp:RadioButtonList ID="RadioButtonList1" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">                              
                 <asp:ListItem  Value="Comments"    Text="Comments" />
                  <asp:ListItem Value="Days Old"  Text="Days Old (From Last Receipt)"   />                               
         </asp:RadioButtonList>
      </td>
      </tr>
      <tr><td align="right" style="padding-right: 5px"><b>Sort?:</b></td>
      <td colspan="3">
           <asp:RadioButtonList ID="RadioButtonList2" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">                              
                 <asp:ListItem   Value="Item#"   Text="Item#" />
                  <asp:ListItem   Value="Customer Part#"  Text="Customer Part#" />                                  
         </asp:RadioButtonList>
      </td>
      </tr>
        </table>
          <table class="shade" width="620px">
          <tr>
            <td align="left" style="padding-left: 25px; " nowrap >
               <b> <asp:CheckBox ID="CheckBox1" runat="server" Text="Include Customer Owned Warehouse?" /></b>    </td>
            <td><b><asp:CheckBox ID="CheckBox2" runat="server"  Text="Print Cost?" /></b></td>
          </tr>
           <tr>
            <td align="left" style="padding-left: 25px; " nowrap >
                <b><asp:CheckBox ID="CheckBox3" runat="server" Text="Page Break By Customer?" /></b>    </td>
            <td><b><asp:CheckBox ID="CheckBox4" runat="server" Text="Exclude Negative Sell Value from Totals?"   /></b></td>
          </tr>
           <tr>
            <td align="left" style="padding-left: 25px; " nowrap >
                <b><asp:CheckBox ID="CheckBox5" runat="server" Text="Print Items <90 Days Old?" /></b>    </td>
            <td><b><asp:CheckBox ID="CheckBox6" runat="server" Text="Subtotal Value By Customer?" /></b></td>
          </tr>
           <tr>
            <td align="left" style="padding-left: 25px; " nowrap >
                <b><asp:CheckBox ID="CheckBox7" runat="server" Text="Print Customer Part#?" /></b>    </td>
            <td><b><asp:CheckBox ID="CheckBox8" runat="server" Text="Print Last Shipment Date?"  /></b></td>
          </tr>
          </table>
        
        <table class="shade" width="620px">
         <tr><td colspan="2" align="left" style="padding-left:10px">
         &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
         <b>Output to?  <asp:RadioButtonList ID="RadioButtonList_out"  RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
                 <asp:ListItem   Value="No"   Text="Text File" />
                 <asp:ListItem  Value="Yes"  Text="Excel" />                 
         </asp:RadioButtonList></b></td></tr>
         
         <tr><td style="width: 615px" colspan="2">
              <asp:Button ID="submitbutton" OnClick="submitbutton_click" runat="server" class="buttonM" Text="Submit" />
               &nbsp;&nbsp;&nbsp;&nbsp;
          <asp:Label ID="OutPutFile" runat="server" Text="Output File:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
              <asp:HyperLink ID="HyperLink1" runat="server" Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink>
              </td>
          </tr>     
         </table>
           <asp:FormView ID="FormView1" Visible="False" runat="server" 
              DataSourceID="ObjectDataSource1" >                                           
                                           
              <ItemTemplate>
                  rageifile:
                  <asp:Label ID="rageifileLabel" runat="server" 
                      Text='<%# Bind("rageifile") %>'></asp:Label><br />
                  
              </ItemTemplate>
               
               
          </asp:FormView>
      
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="RageinvRep" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmAction" Type="String"  />
                  <asp:Parameter Name="prmAsof" Type="String" />
                  <asp:Parameter Name="prmBeSman" Type="String" />
                  <asp:Parameter Name="prmEndSman" Type="String" />
                  <asp:Parameter Name="prmBeCust" Type="String" />
                  <asp:Parameter Name="prmEndCust" Type="string" />
                  <asp:Parameter Name="prmBeItem" Type="string" />
                  <asp:Parameter Name="prmEndItem" Type="string" />
                  <asp:Parameter Name="prmBeJob" Type="string" />
                  <asp:Parameter Name="prmEndJob" Type="String" />
                  <asp:Parameter Name="prmBeJob2" Type="String" />
                  <asp:Parameter Name="prmEndjob2" Type="String" />
                  <asp:Parameter Name="prmBeWare" Type="String" />
                  <asp:Parameter Name="prmEndWare" Type="String" />
                  <asp:Parameter Name="prmInvClass" Type="String" />
                  <asp:Parameter Name="prmAgedDay1" Type="Int32" />
                  <asp:Parameter Name="prmAgedDay2" Type="Int32" />
                  <asp:Parameter Name="prmAgedDay3" Type="Int32" />
                  <asp:Parameter Name="prmAgedDay4" Type="Int32" />
                  <asp:Parameter Name="prmRdPrice" Type="String" />
                  <asp:Parameter Name="prmQtyValue" Type="String" />
                  <asp:Parameter Name="prmComment" Type="String" />
                  <asp:Parameter Name="prmSort" Type="String" />
                  <asp:Parameter Name="prmCustWhse" Type="String" />
                  <asp:Parameter Name="prmPgBreak" Type="String" />
                  <asp:Parameter Name="prmTbCurr" Type="String" />
                  <asp:Parameter Name="prmCustPart" Type="String" />
                  <asp:Parameter Name="prmCost" Type="string" />
                  
                  <asp:Parameter Name="prmNegSale" Type="String" />
                  <asp:Parameter Name="prmValCust" Type="String" />
                  <asp:Parameter Name="prmLastShip" Type="String" />
                  <asp:Parameter Name="prmOut" Type="String" />
                  
              </SelectParameters>
          </asp:ObjectDataSource>
     
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>
<script language="javascript" type="text/javascript">
    if (document.getElementById("CheckBox1").checked || document.getElementById("CheckBox2").checked) {
        document.getElementById("RadioButtonList3").childNodes[0].disabled = false;
        document.getElementById("RadioButtonList3").childNodes[1].disabled = false;
        document.getElementById("RadioButtonList3").childNodes[2].disabled = false;
        document.getElementById("RadioButtonList3").childNodes[3].disabled = false;
    }
    else {
        document.getElementById("RadioButtonList3").childNodes[0].disabled = true;
        document.getElementById("RadioButtonList3").childNodes[1].disabled = true;
        document.getElementById("RadioButtonList3").childNodes[2].disabled = true;
        document.getElementById("RadioButtonList3").childNodes[3].disabled = true;
    }
</script>
