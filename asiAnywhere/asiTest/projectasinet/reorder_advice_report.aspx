<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="reorder_advice_report_list" Codebehind="reorder_advice_report.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Reorder Advice Report</title>
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
    var beginc=document.getElementById("TextBox1");
    var endc=document.getElementById("TextBox2");
    endc.value=beginc.value;
    }
    
    function samevalue2()
    {
    var beginc=document.getElementById("TextBox1");
    var endc=document.getElementById("TextBox2");
    if(endc.value!=beginc.value)
    {
    alert("Begin and End Customer Value must be same");
    endc.value=beginc.value;
    endc.focus();
    }
    }
    
    function contactcustomerlook(){ 
  var NewWindow = window.open("contact_customer_lookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11){
    document.forms[0].TextBox1.value = ReturnObj1;
    document.forms[0].TextBox2.value = ReturnObj1;
    document.forms[0].TextBox1.focus();
      
}
function contactcustomerlook2(){ 
  var NewWindow = window.open("contact_customer_copylookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerCopyLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11){

    document.forms[0].TextBox2.value = ReturnObj1;
    document.forms[0].TextBox2.focus();
  }

function Relook() 
{
    var item1 = document.getElementById("TextBox1").value; 
    var NewWindow = window.open("reorder_item_lookup.aspx?item="+item1 +"","ReItemLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ReLookup(ReturnObj1)
{
    document.forms[0].TextBox3.value = ReturnObj1;
    document.forms[0].TextBox3.focus();
}
function Relook2() {
    var item2 = document.getElementById("TextBox2").value;
    var NewWindow = window.open("reorder_item_lookup2.aspx?item1=" + item2 + "", "ReItemLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ReLookup2(ReturnObj1) {
    document.forms[0].TextBox4.value = ReturnObj1;
    document.forms[0].TextBox4.focus();
}

function procatlook() {
    var NewWindow = window.open("procat_lookup.aspx", "ProcatLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function procatLookup(ReturnObj1) {
    document.forms[0].TextBox5.value = ReturnObj1;
    document.forms[0].TextBox5.focus();
}
function procat2look() {
    var NewWindow = window.open("procat_lookup2.aspx", "ProcatLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function procat2Lookup(ReturnObj1) {
    document.forms[0].TextBox6.value = ReturnObj1;
    document.forms[0].TextBox6.focus();
}

function LocationLook() {
    var NewWindow = window.open("location_lookup.aspx", "LocationLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function LocationLookUp(ReturnObj1) {
    document.forms[0].TextBox7.value = ReturnObj1;
    document.forms[0].TextBox7.focus();
}
function Location2Look() {
    var NewWindow = window.open("location_lookup2.aspx", "LocationLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Location2LookUp(ReturnObj1) {
    document.forms[0].TextBox8.value = ReturnObj1;
    document.forms[0].TextBox8.focus();
}

function Datelook(){ 
  var NewWindow = window.open("date_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup(obj)
{
  document.forms[0].TextBox9.value=obj;
}
function Datelook1()
{
  document.forms[0].TextBox9.value="";
  Datelook();
}
function datevalidate()
{
    var date=document.getElementById("TextBox9").value;
    
    if(date.length>1 && date.length<3 && date.indexOf('/')!=1)
    {
        document.getElementById("TextBox9").value = date + "/";
    }
    if(date.length>4 && date.length<6 && date.indexOf('/')!=3)
    {
        document.getElementById("TextBox9").value = date + "/";
    }

}

function CheckClick() {
    if (document.forms[0].CheckBox4.checked == true) {
        document.forms[0].CheckBox2.disabled = "true";
        document.forms[0].RadioButtonList5_0.disabled = "true";
        document.forms[0].RadioButtonList5_1.disabled = "true";
        document.forms[0].RadioButtonList6_0.disabled = "true";
        document.forms[0].RadioButtonList6_1.disabled = "true";
        document.forms[0].RadioButtonList6_2.disabled = "true";
    }
    else {
        document.forms[0].CheckBox2.disabled = false;
        document.forms[0].RadioButtonList5_0.disabled = false;
        document.forms[0].RadioButtonList5_1.disabled = false;
        document.forms[0].RadioButtonList6_0.disabled = false;
        document.forms[0].RadioButtonList6_1.disabled = false;
        document.forms[0].RadioButtonList6_2.disabled = false;
    }
}

    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='TextBox1'>   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>       
            
                       
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=center nowrap><font size=+0><b>Reorder Advice Report&nbsp;</b></font></TD>
          <TD vAlign="middle" nowrap >
            <asp:linkbutton id="hlkBackToMenu" OnClick="Back_tomenu_Click" runat="server" >Back to menu</asp:linkbutton>
          </TD>
          <TD vAlign="middle" align="center" nowrap >Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
            &nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx">Change password</asp:hyperlink>
          &nbsp;<b>Company:</b>&nbsp;<asp:Label ID="labelcompany" runat="server" Text="Label"></asp:Label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
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
         <asp:HiddenField ID="HiddenField12" runat="server" />
         <asp:HiddenField ID="HiddenField13" runat="server" />
          <asp:Label ID="Label1" Font-Bold="true" ForeColor="red" runat="server" ></asp:Label>
          
          <asp:FormView ID="FormView2" runat="server" DataSourceID="ObjectDataSource2">
                            
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
     
      <tr><td align="right" style="padding-right: 5px"><b>Begining Customer#:</b></td><td>
          <asp:TextBox ID="TextBox1" OnTextChanged="TextBox1_textchanged" onkeyup="samevalue()" MaxLength="8"  width="105px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
      <td align="right" style="padding-right: 5px"><b>Ending Customer#:</b></td><td><asp:TextBox ID="TextBox2" MaxLength="8"  width="105px" runat="server"></asp:TextBox>
      <a href="#" tabindex="1" onClick="contactcustomerlook2(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
      </td>
        </tr>
      <tr><td align="right" style="padding-right: 5px"><b>Begining Item#:</b></td>
          <td nowrap><asp:TextBox ID="TextBox3" MaxLength="15" Width="105px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="Relook(); return false"><asp:Image ID="FGLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
        <td align="right" style="padding-right: 5px"><b>Ending Item#:</b></td>
          <td nowrap><asp:TextBox ID="TextBox4" MaxLength="15" Width="105px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="Relook2(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
          </tr>
          
            <tr> <td align="right" style="padding-right: 5px"><b>Begining Category:</b></td>
          <td><asp:TextBox ID="TextBox5" MaxLength="5" Width="105px" runat="server"></asp:TextBox>
            <a href="#" tabindex="1" onClick="procatlook(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
        <td align="right" style="padding-right: 5px"><b>Ending Category:</b></td>
          <td><asp:TextBox ID="TextBox6" MaxLength="5" Width="105px" runat="server"></asp:TextBox>
            <a href="#" tabindex="1" onClick="procat2look(); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td></tr>
        
        
        <tr><td align="right" style="padding-right: 5px"><b>Begining Warehouse:</b></td>
          <td><asp:TextBox ID="TextBox7" MaxLength="5" Width="105px" runat="server"></asp:TextBox>
            <a href="#" tabindex="1" onClick="LocationLook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
        <td align="right" style="padding-right: 5px"><b>Ending Warehouse:</b></td>
          <td><asp:TextBox ID="TextBox8" MaxLength="5" Width="105px" runat="server"></asp:TextBox>
            <a href="#" tabindex="1" onClick="Location2Look(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td></tr>
        </table>
          <table class="shade" width="520px">
          <tr><td align="right" style="padding-right: 5px" ><b>Include quantity on order with quantity on hand ?</b></td>
          <td> <asp:CheckBox ID="CheckBox1" runat="server" /></td>
             
          <td align="right" style="padding-right: 5px"><b>Print Customer Part# ?</b></td>
         <td> <asp:CheckBox ID="CheckBox2" runat="server" /></td>
          </tr>
          <tr><td align="right" style="padding-right: 5px"><b>Include customer owned warehouse ?</b></td>
          <td> <asp:CheckBox ID="CheckBox3" runat="server" /></td>
          <td align="right" style="padding-right: 5px"><b>Print 6 months history ?</b></td>
         <td> <asp:CheckBox ID="CheckBox4" runat="server" onclick="CheckClick();" /></td>
          </tr>
          
           <tr> <td align="right" style="padding-right: 5px"><b>Print item below reorders point only ?</b></td>
           <td> <asp:CheckBox ID="CheckBox5" runat="server" /></td>
           <td align="right" style="padding-right: 5px"><b>Print dashes between each line ?</b></td>
           <td> <asp:CheckBox ID="CheckBox6" runat="server" /></td></tr></table>
        
        <table class="shade" width="520px">
           
         <tr><td nowrap colspan="2"><b>CalcQOH?</b>
             <asp:RadioButtonList ID="RadioButtonList1" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
             
                 
                 <asp:ListItem      Text="Allocated" />
                  <asp:ListItem     Text="Scheduled" />
                 <asp:ListItem      Text="Actual" />
                   <asp:ListItem    Text="All Releases" />
         </asp:RadioButtonList> &nbsp; &nbsp;<b>As of</b>&nbsp; &nbsp;<asp:TextBox ID="TextBox9" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" MaxLength="12" ToolTip="MM/DD/YYYY" Width="100px" runat="server"></asp:TextBox> 
         <a href="#" tabindex="2" onblur="document.getElementById('TextBox9').focus()" onClick="showCalendarControl(TextBox9); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
         </td></tr>
         
         <tr><td nowrap colspan="2">&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;
         <b>Print?</b>
             <asp:RadioButtonList ID="RadioButtonList2" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="3" Font-Bold ="true" runat="server">
                  <asp:ListItem      Text="Stocked" />
                  <asp:ListItem     Text="Not Stocked" />
                 <asp:ListItem      Text="All" />
                  
         </asp:RadioButtonList> &nbsp; &nbsp;
         </td></tr>
         
         <tr><td nowrap colspan="2">&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;
         <b>Print?</b>
             <asp:RadioButtonList ID="RadioButtonList3" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="3" Font-Bold ="true" runat="server">
                  <asp:ListItem      Text="Purchased" />
                  <asp:ListItem     Text="Manufactured" />
                 <asp:ListItem      Text="All" />
                  
         </asp:RadioButtonList> &nbsp; &nbsp;
         </td></tr>
         <tr><td nowrap colspan="2">&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;
         <b>Print?</b>
             <asp:RadioButtonList ID="RadioButtonList4" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="3" Font-Bold ="true" runat="server">
                  <asp:ListItem      Text="Lot Controlled" />
                  <asp:ListItem     Text="Reorder" />
                 <asp:ListItem      Text="All" />
                  
         </asp:RadioButtonList> &nbsp; &nbsp;
         </td></tr>
         <tr><td nowrap colspan="2">&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;
         <b>Print?</b>
             <asp:RadioButtonList ID="RadioButtonList5" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="3" Font-Bold ="true" runat="server">
                  <asp:ListItem      Text="Qty Avail" />
                  <asp:ListItem     Text="Vendor" />
                                   
         </asp:RadioButtonList> &nbsp; &nbsp;
         </td></tr>
         <tr><td nowrap colspan="2">&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp;
         <b>Print?</b>
             <asp:RadioButtonList ID="RadioButtonList6" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="3" Font-Bold ="true" runat="server">
                  <asp:ListItem      Text="Price" />
                  <asp:ListItem     Text="Vendor" />
                 <asp:ListItem      Text="Max Qty" />
                  
         </asp:RadioButtonList> &nbsp; &nbsp;
         </td></tr>
        
        <tr><td >
           <b>Print To:</b><b><asp:RadioButtonList ID="RadioButtonList8" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
                  <asp:ListItem      Text="Pdf" />
                  <asp:ListItem      Text="Excel" />
                  
                 </asp:RadioButtonList></b>
           </td></tr>
        <tr><td>
              <asp:Button ID="submitbutton" OnClick="submitbutton_click" runat="server" class="buttonM" Text="Submit" />
              &nbsp;&nbsp;<asp:Label ID="OutputLabel" runat="server" Text="Output File:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
          &nbsp;
          <asp:HyperLink ID="HyperLink1" runat="server"  Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink> 
              
              </td>
          </tr>     
         </table>
           <asp:FormView ID="FormView1" Visible="false" runat="server" DataSourceID="ObjectDataSource1" OnPreRender="FormView1_PreRender">
                            
              <ItemTemplate>
                  aFile:
                  <asp:Label ID="aFileLabel" runat="server" Text='<%# Bind("aFile") %>'></asp:Label><br />
                  vFile:
                  <asp:Label ID="vFileLabel" runat="server" Text='<%# Bind("vFile") %>'></asp:Label><br />
                  bFile:
                  <asp:Label ID="bFileLabel" runat="server" Text='<%# Bind("bFile") %>'></asp:Label><br />
                 
              </ItemTemplate>
               
          </asp:FormView>
      
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="ReOrder" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmOut" Type="String" />
                  <asp:Parameter Name="prmBeginCust" Type="String" />
                  <asp:Parameter Name="prmEndCust" Type="String" />
                  <asp:Parameter Name="prmBeginItem" Type="String" />
                  <asp:Parameter Name="prmEndItem" Type="String" />
                  <asp:Parameter Name="prmBeginCategory" Type="String" />
                  <asp:Parameter Name="prmEndCategory" Type="String" />
                  <asp:Parameter Name="prmBeginWare" Type="String" />
                  <asp:Parameter Name="prmEndWare" Type="String" />
                  
                  <asp:Parameter Name="prmQtyOnHand" Type="Boolean" />
                  <asp:Parameter Name="prIncQoh" Type="Boolean" />
                  <asp:Parameter Name="prmBelow" Type="Boolean" />
                  <asp:Parameter Name="prmPart" Type="Boolean" />
                  <asp:Parameter Name="prmHistory" Type="Boolean" />
                  <asp:Parameter Name="prmDash" Type="Boolean" />
                  <asp:Parameter Name="prmRdqoh" Type="String" />
                  <asp:Parameter Name="prmDate" Type="DateTime" />
                  
                  <asp:Parameter Name="prmPrintStock" Type="String" />
                  <asp:Parameter Name="prmPrintPurch" Type="String" />
                  <asp:Parameter Name="prmPrintLot" Type="String" />
                  <asp:Parameter Name="prmPrintQty" Type="String" />
                  <asp:Parameter Name="prmPrintPrice" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
     
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>    
    <script language="javascript">
        if (document.forms[0].CheckBox4.checked == true) {
            document.forms[0].CheckBox2.disabled = "true";
            document.forms[0].RadioButtonList5_0.disabled = "true";
            document.forms[0].RadioButtonList5_1.disabled = "true";
            document.forms[0].RadioButtonList6_0.disabled = "true";
            document.forms[0].RadioButtonList6_1.disabled = "true";
            document.forms[0].RadioButtonList6_2.disabled = "true";
        }
        else {
            document.forms[0].CheckBox2.disabled = false;
            document.forms[0].RadioButtonList5_0.disabled = false;
            document.forms[0].RadioButtonList5_1.disabled = false;
            document.forms[0].RadioButtonList6_0.disabled = false;
            document.forms[0].RadioButtonList6_1.disabled = false;
            document.forms[0].RadioButtonList6_2.disabled = false;
        }
    </script>    
    </form>
  </body>
</HTML>

