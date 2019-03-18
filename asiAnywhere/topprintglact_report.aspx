<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="topprintglact_report" Codebehind="topprintglact_report.aspx.cs" %>

<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>List Request</title>
    
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
     
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
        <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    <script language="javascript"  src="include/CalendarControl.js" > </script>
    <script language = JavaScript>

        window.onload = chk_enbl;
    
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
    var beginc=document.getElementById("begincust_TextBox");
    var endc=document.getElementById("endcust_TextBox");
    endc.value=beginc.value;
    }
    
    function samevalue2()
    {
    var beginc=document.getElementById("begincust_TextBox");
    var endc=document.getElementById("endcust_TextBox");
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
  document.forms[0].begincust_TextBox.value = ReturnObj1;
  document.forms[0].beginame_TextBox.value = ReturnObj2;
    
}
function contactcustomerlook2(){ 
  var NewWindow = window.open("contact_customer_copylookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerCopyLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11){

    document.forms[0].endcust_TextBox.value = ReturnObj1;
    document.forms[0].endnameTextBox.value = ReturnObj2;
  }
function orderlook(){ 
  var NewWindow = window.open("order_lookup.aspx","OrderLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function OrderLookup(ReturnObj1){ 
  document.forms[0].beginorder_TextBox.value = ReturnObj1;
 
}
function customerpolook(){ 
  var NewWindow = window.open("customerpo_lookup.aspx","EstimateLookupWindow","width=450,height=300,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustomerPOLookup(ReturnObj1){ 
  document.forms[0].custpo_TextBox.value = ReturnObj1;
 
}
function fglook(){ 
  var NewWindow = window.open("fgitem_lookup.aspx","FGLookupWindow","width=450,height=300,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function FGLookup(ReturnObj1){ 
  document.forms[0].beginitem_TextBox.value = ReturnObj1;
  
}
function job1look(){ 
  var NewWindow = window.open("job1_lookup.aspx","Job1LookupWindow","width=450,height=300,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Job1Lookup(ReturnObj1){ 
  document.forms[0].joborder_TextBox.value = ReturnObj1;
  
}

function order2look(){ 
  var NewWindow = window.open("order_translookup.aspx","OrderLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Order2Lookup(ReturnObj1){   
  document.forms[0].endorder_TextBox.value = ReturnObj1;
}
function customerpo2look(){ 
  var NewWindow = window.open("customerpo2_lookup.aspx","EstimateLookupWindow","width=450,height=300,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CustomerPO2Lookup(ReturnObj1){   
  document.forms[0].endcustpo_TextBox.value = ReturnObj1;
}
function fg2look(){ 
  var NewWindow = window.open("fgitem_translookup.aspx","FGLookupWindow","width=450,height=300,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function FG2Lookup(ReturnObj1){   
  document.forms[0].enditem_TextBox.value = ReturnObj1;
}
function job1translook(){ 
  var NewWindow = window.open("job1_translookup.aspx","Job1LookupWindow","width=450,height=300,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Job1transLookup(ReturnObj1){   
  document.forms[0].endjoborder_TextBox.value = ReturnObj1;
}
function insertsub()
{
var job=document.getElementById("joborder2_TextBox");
var val=document.forms[0].joborder2_TextBox.value;
job.value="-" + val;
}

function insertsub2()
{
var job=document.getElementById("endjoborder2_TextBox");
var val=document.forms[0].endjoborder2_TextBox.value;
job.value="-" + val;
}

var act = "";
function AccountLook(obj) {
    act = obj;
    var NewWindow = window.open("accountlook.aspx", "AccountLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function AccountLookup(ReturnObj1, ReturnObj2) {
    if (act == "1") {
        document.forms[0].begact_TextBox.value = ReturnObj1;
        document.forms[0].begdscr_TextBox.value = ReturnObj2;
    }
    else {
        document.forms[0].endact_TextBox.value = ReturnObj1;
        document.forms[0].enddscr_TextBox.value = ReturnObj2;
    }
}
function chk_enbl() {
    var chk1 = document.getElementById("chk_note");
    if (chk1.checked == true) {
        var rd1 = document.getElementById("RadioButtonList2");
        rd1.disabled = false;
    }
    else {
        var rd1 = document.getElementById("RadioButtonList2");
        rd1.disabled = true;
    }
}
    
    </script> 

      <link href="include/tree.css" rel="stylesheet" type="text/css" />
  </head>    
   <body>
    <form id="frmList" runat="server"   defaultfocus ='txt_supplierscode' >   
       
      <div>
                      
      <TABLE id="tblTop" cellSpacing="3" border="0">
        <TR>
          
          <TD align=center nowrap><font size=+0><b>GL Accounts List&nbsp;</b></font></TD>
          
        </TR>
      </TABLE>
      
          <asp:HiddenField ID="hid_act_rel" runat="server" />
          <asp:HiddenField ID="hid_tb_inst" runat="server" />
          <asp:HiddenField ID="hid_bom" runat="server" />
          <asp:HiddenField ID="hid_revise" runat="server" />
          <asp:HiddenField ID="hid_reprint" runat="server" />
          <asp:HiddenField ID="hid_sch_rel" runat="server" />
          <asp:HiddenField ID="hid_ship_to" runat="server" />
          <asp:HiddenField ID="hid_cons_frm" runat="server" />
          <asp:HiddenField ID="hid_whs_months" runat="server" />
          <asp:HiddenField ID="hid_months" runat="server" />
          
          <asp:Label ID="Label1" runat="server" ForeColor="red" Font-Bold="true"></asp:Label>
          
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
       <fieldset style="width:550px" class="shade">
       <table>
       <tr><td nowrap><b>List Order: </b></td><td> <asp:RadioButtonList ID="RadioButtonList1" CellSpacing="1" Width="450px" RepeatColumns="2" runat="server">
       <asp:ListItem Text="1 By Account No" Value="1"></asp:ListItem>
       <asp:ListItem Text="2 By Description" Value="2"></asp:ListItem>
       </asp:RadioButtonList></td></tr>
       </table>
       
       </fieldset>
       <fieldset  style="width:550px" class="shade"><legend>Selection Parameter</legend> 
      <table class="shade">
        
        <tr>
            <td align="right" style="padding-right:5px"><b>Beginning Account#:</b></td>
            <td nowrap>
                <asp:TextBox ID="begact_TextBox"  runat="server"></asp:TextBox>
                <a href="#" tabindex="1" onClick="AccountLook(1); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            
        </tr>
        
        <tr>
            <td align="right" style="padding-right:5px"><b>Beginning Description:</b></td>
            <td>
                <asp:TextBox ID="begdscr_TextBox" runat="server"></asp:TextBox>
                
            </td>
            
        </tr>
        <tr>
        <td align="right" style="padding-right:5px"><b>Ending Account#:</b></td>
            <td>
                <asp:TextBox ID="endact_TextBox"  runat="server"></asp:TextBox>
                 <a href="#" tabindex="1" onClick="AccountLook(2); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
        </tr>
        
        <tr>
        <td align="right" style="padding-right:5px"><b>Ending Description:</b></td>
            <td>
                <asp:TextBox ID="enddscr_TextBox"  runat="server"></asp:TextBox>                 
            </td>
        </tr>
        
       <tr>
       <td></td>
       <td nowrap>
       <b><asp:CheckBox ID="chk_shipto" Text="Show History" runat="server" /></b>
       </td> 
       </tr>              
       
       </table>
       </fieldset>
        <fieldset  style="width:550px" class="shade"><legend>Show Options</legend>
            <table>
                <tr>
                    <td nowrap>
                       <b>&nbsp;&nbsp;&nbsp;&nbsp; <asp:CheckBox ID="chk_note" onclick="chk_enbl()" runat="server"  Text="Show Notes?" /></b></td>
                       <td><asp:RadioButtonList ID="RadioButtonList2" CellSpacing="1"  RepeatColumns="3" runat="server">
                       <asp:ListItem Text="All Notes" Value="1"></asp:ListItem>
                       <asp:ListItem Text="Viewed Only" Value="2"></asp:ListItem>
                       <asp:ListItem Text="Not Viewed Only" Value="3"></asp:ListItem>
                       </asp:RadioButtonList>
                    </td>
                    
                </tr>
                 <tr>
                    <td nowrap>
                       <b> &nbsp;&nbsp;&nbsp;&nbsp; <asp:CheckBox ID="chk_parameter" runat="server" Text="Show Selection Parameter?" /></b>
                    </td>
                    
                </tr>
                <tr>
                    <td nowrap>
                       <b> &nbsp;&nbsp;&nbsp;&nbsp; <asp:CheckBox ID="chk_misc" runat="server" Text="Show Misc. Field Values?" /></b>
                    </td>
                    
                </tr>
                <tr>
                    <td nowrap>
                       <b>&nbsp;&nbsp;&nbsp;&nbsp;  <asp:CheckBox ID="chk_add" runat="server" Text="Show Address?" /></b>
                    </td>
                    
                </tr>
                <tr>
                    <td nowrap>
                       <b>&nbsp;&nbsp;&nbsp;&nbsp;  <asp:CheckBox ID="chk_phone" runat="server" Text="Show Phone?" /></b>
                    </td>
                    
                </tr>
            </table>
        </fieldset>
        
      
           
          <asp:Button ID="SubmitButton" runat="server" CssClass="button" Text="Submit" OnClick="SubmitButton_Click"></asp:Button> 
                  <%--&nbsp;&nbsp;<asp:Label ID="OutputLabel" runat="server" Text="Output File:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
          &nbsp;
          <asp:HyperLink ID="HyperLink1" runat="server"  Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink> --%>
                             
                              
                              
         <asp:FormView ID="FormView1" Visible="False" runat="server" DataSourceID="ObjectDataSource1" OnPreRender="FormView1_PreRender">
            
            <ItemTemplate>
                actrpt:
                <asp:Label ID="actprtLabel" runat="server" Text='<%# Bind("actrpt") %>'></asp:Label><br />
                ext:
                <asp:Label ID="extLabel" runat="server" Text='<%# Bind("ext") %>'></asp:Label><br />
            </ItemTemplate>
                         
        </asp:FormView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}" SelectMethod="GLAccountReportUp" TypeName="ledger">
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter Name="prmAction" Type="String" />                                
                <asp:Parameter Name="prmbeact" Type="String" />
                <asp:Parameter Name="prmbegactdr" Type="String" />
                <asp:Parameter Name="prmendact" Type="String" />
                <asp:Parameter Name="prmendactdr" Type="String" />
                <asp:Parameter Name="prmshwhist" Type="String" />
                <asp:Parameter Name="prmshwnote" Type="String" />
                <asp:Parameter Name="prmshwselc" Type="String" />
                <asp:Parameter Name="prmshwmisc" Type="String" />
                <asp:Parameter Name="prmshwadd" Type="String" />
                <asp:Parameter Name="prmshwphn" Type="String" />
                <asp:Parameter Name="prmalnote" Type="String" />
                <asp:Parameter Name="prmOut" Type="String" />
                <asp:Parameter Name="prmListOrder" Type="String" />
                <asp:Parameter Name="prmReckey" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
    </div>
       
    </form>
</body>
</html>
