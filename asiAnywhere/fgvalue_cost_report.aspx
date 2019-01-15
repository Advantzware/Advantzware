<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="fgvalue_cost_report_list" Codebehind="fgvalue_cost_report.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>FG Value/Cost by Whs/Bin/Tag Report</title>
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
    <script language = "JavaScript" src="include/CalendarControl.js"></script>
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


  function Relook() {
      var item1 = document.getElementById("TextBox1").value; 
  var NewWindow = window.open("reorder_item_lookup.aspx?item="+item1 +"","ReItemLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ReLookup(ReturnObj1){
    document.forms[0].TextBox3.value = ReturnObj1;
    document.forms[0].TextBox3.focus();
}
function Relook2() {
    var item2 = document.getElementById("TextBox2").value;
  var NewWindow = window.open("reorder_item_lookup2.aspx?item1="+ item2 +"","ReItemLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ReLookup2(ReturnObj1){
    document.forms[0].TextBox4.value = ReturnObj1;
    document.forms[0].TextBox4.focus();
}
function LocationLook(){ 
  var NewWindow = window.open("location_lookup.aspx","LocationLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function LocationLookUp(ReturnObj1){
    document.forms[0].TextBox7.value = ReturnObj1;
    document.forms[0].TextBox7.focus();
}
function Location2Look(){ 
  var NewWindow = window.open("location_lookup2.aspx","LocationLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Location2LookUp(ReturnObj1){
    document.forms[0].TextBox8.value = ReturnObj1;
    document.forms[0].TextBox8.focus();
}
function binlook(){ 
  var NewWindow = window.open("bin_lookup.aspx","BinLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function BinLookup(ReturnObj1){
    document.forms[0].BeBinTextBox.value = ReturnObj1;
    document.forms[0].BeBinTextBox.focus();
}
function bin2look(){ 
  var NewWindow = window.open("bin_lookup2.aspx","BinLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Bin2Lookup(ReturnObj1){
    document.forms[0].EndBinTextBox.value = ReturnObj1;
    document.forms[0].EndBinTextBox.focus();
}
function procatlook(){ 
  var NewWindow = window.open("procat_lookup.aspx","ProcatLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function procatLookup(ReturnObj1){
    document.forms[0].BeCatTextBox.value = ReturnObj1;
    document.forms[0].BeCatTextBox.focus();
}
function procat2look(){ 
  var NewWindow = window.open("procat_lookup2.aspx","ProcatLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function procat2Lookup(ReturnObj1){
    document.forms[0].EndCatTextBox.value = ReturnObj1;
    document.forms[0].EndCatTextBox.focus();
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


    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='TextBox1'>   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>       
            
                       
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=center nowrap><font size=+0><b>FG Value/Cost by Whs/Bin/Tag Report&nbsp;</b></font></TD>
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
         
         <asp:HiddenField ID="HiddenField14" runat="server" />
         <asp:HiddenField ID="HiddenField15" runat="server" />
         <asp:HiddenField ID="HiddenField16" runat="server" />
         <asp:HiddenField ID="HiddenField17" runat="server" />
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
          
      <table class="shade" width="700px">
         <tr><td align="right" style="padding-right: 5px" nowrap><b>As of:</b>&nbsp; &nbsp;</td>
         <td><asp:Label ID="DateTextBox" Font-Bold="true" BackColor="Turquoise" BorderStyle="solid" BorderWidth="1px" Width="100px" runat="server"></asp:Label> 
         
         </td>
         <td align="right" style="padding-right: 5px" nowrap>
         <%--<b>Only Show QOH that is Older then:</b></td><td><asp:Label ID="TextBox10" Font-Bold="true" BorderWidth="1px" BackColor="Turquoise" BorderStyle="solid" Width="40px" runat="server"></asp:Label>--%>
         </td></tr>
      <tr><td align="right" style="padding-right: 5px"><b>Begining Customer#:</b></td><td>
          <asp:TextBox ID="TextBox1"  onkeyup="samevalue()" MaxLength="8"  width="105px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
      <td align="right" style="padding-right: 5px"><b>Ending Customer#:</b></td><td><asp:TextBox ID="TextBox2" MaxLength="8" width="105px" runat="server"></asp:TextBox>
      <a href="#" tabindex="1" onClick="contactcustomerlook2(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
      </td>
        </tr>
        <tr><td align="right" style="padding-right: 5px"><b>Begining Warehouse:</b></td>
          <td><asp:TextBox ID="TextBox7" MaxLength="5" Width="105px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="LocationLook(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
        <td align="right" style="padding-right: 5px"><b>Ending Warehouse:</b></td>
          <td><asp:TextBox ID="TextBox8" MaxLength="5" Width="105px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="Location2Look(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td></tr>
          
        <tr><td align="right" style="padding-right: 5px"><b>Begining Bin:</b></td>
          <td><asp:TextBox ID="BeBinTextBox" MaxLength="8" Width="105px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="binlook(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
        <td align="right" style="padding-right: 5px"><b>Ending Bin:</b></td>
          <td><asp:TextBox ID="EndBinTextBox" MaxLength="8" Width="105px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="bin2look(); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td></tr>  
          
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
          <td><asp:TextBox ID="BeCatTextBox" MaxLength="8" Width="105px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="procatlook(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
        <td align="right" style="padding-right: 5px"><b>Ending Category:</b></td>
          <td><asp:TextBox ID="EndCatTextBox" MaxLength="8" Width="105px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="procat2look(); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td></tr>
        
        
        
        </table>
          <table class="shade" width="700px">
          <tr><td align="left" style="padding-left:10px" nowrap colspan="2"><b>Sort By:</b>
             <asp:RadioButtonList ID="RadioButtonList1" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
             
                 
                 <asp:ListItem      Text="Customer#" />
                  <asp:ListItem     Text="FG Item#" />
                 <asp:ListItem      Text="Part#" />
                   <asp:ListItem    Text="product Category" />
                   <asp:ListItem    Text="Whs/Bin" />
         </asp:RadioButtonList> &nbsp; &nbsp;</td></tr>
         <tr><td align="left" style="padding-left:10px" nowrap><b>Item Code?</b>
             <asp:RadioButtonList ID="RadioButtonList2" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
             
                 
                 <asp:ListItem      Text="Stock#" />
                  <asp:ListItem     Text="Custom" />
                 <asp:ListItem      Text="All" />
                   
         </asp:RadioButtonList> &nbsp; &nbsp;</td>
         <td align="left" style="padding-left:10px">
         <b><asp:CheckBox ID="CheckBox_rec_date" Text="Print Receipt Date?" runat="server" /></b>
         </td></tr>
         <tr><td align="left" style="padding-left:10px" nowrap ><b>Print ?</b>
             <asp:RadioButtonList ID="RadioButtonList3" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
                             
                 <asp:ListItem      Text="Qty" />
                  <asp:ListItem     Text="MSF" />
                                  
         </asp:RadioButtonList> &nbsp; &nbsp;</td>
         <td align="left" style="padding-left:10px">
         <b><asp:CheckBox ID="CheckBox_comp_only" Text="Print Set and Components Only?" runat="server" /></b>
         </td></tr>
          <tr><td align="left" style="padding-left:10px" ><b>
           <asp:CheckBox ID="CheckBox1" Text="Print Sale Price?" runat="server" /></b>
           &nbsp;&nbsp;&nbsp;&nbsp;&nbsp<b>From:</b>
             <asp:RadioButtonList ID="RadioButtonList4" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
                           
                 <asp:ListItem      Text="FG#" />
                  <asp:ListItem     Text="Order" />                                   
         </asp:RadioButtonList> &nbsp; &nbsp;</td>
         <td align="left" style="padding-left:10px">
         <b><asp:CheckBox ID="CheckBox_sub_tot" Text="Print Subtotals?" runat="server" /></b>
         </td>
          </tr>
          <tr><td align="left" style="padding-left:10px"><b>
          <asp:CheckBox ID="CheckBox2" Text="Include Zero Balances" runat="server" /></b></td>
          <td align="left" style="padding-left:10px">
          <b><asp:CheckBox ID="CheckBox_act_rel_qty" Text="Print Actual Release Qty?" runat="server" /></b>
          </td>
          </tr>
          
           <tr> <td align="left" style="padding-left:10px"><b>
           <asp:CheckBox ID="CheckBox4" Text="Include Customer owned Warehouse?" runat="server" /></b></td>
           <td align="left" style="padding-left:10px"><b>
          <asp:CheckBox ID="CheckBox3" Text="Print Customer Part#?" runat="server" /></b></td>
           </tr>
           
            <tr> <td align="left" style="padding-left:10px"><b>
           <asp:CheckBox ID="CheckBox8" Text="Only Customer owned Warehouse?" runat="server" /></b></td>
           <td align="left" style="padding-left:10px"><b>
           <asp:CheckBox ID="CheckBox5" Text="Print PO#?" runat="server" /></b></td>
           </tr>
           
           <tr> <td align="left" style="padding-left:10px"><b>
            <asp:CheckBox ID="CheckBox6" Text="Print Cost?" runat="server" />
            <asp:CheckBox ID="CheckBox7" Text="If Yes-DL/Mat Only?" runat="server" /></b></td>
            <td align="left" style="padding-left:10px"><b>If Yes-PO Type?</b>
            <asp:RadioButtonList ID="RadioButtonList5" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
                      
                 <asp:ListItem      Text="Line" />
                  <asp:ListItem     Text="Header" />
                                   
         </asp:RadioButtonList> 
            </td>
           </tr>
           <tr><td>
              <asp:Button ID="submitbutton" OnClick="submitbutton_click" runat="server" class="buttonM" Text="Submit" />
              &nbsp;&nbsp;&nbsp;&nbsp;
              <asp:Label ID="OutputLabel" runat="server" Text="Output File:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
              &nbsp;
              <asp:HyperLink ID="HyperLink1" runat="server"  Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink></td>
          </tr>  
           </table>
       
          <asp:FormView ID="FormView1" runat="server" Visible="false" DataSourceID="ObjectDataSource1" >
                            
              <ItemTemplate>
                  abc:
                  <asp:Label ID="abcLabel" runat="server" Text='<%# Bind("abc") %>'></asp:Label><br />
                  vfgvalue:
                  <asp:Label ID="vfgvalueLabel" runat="server" Text='<%# Bind("vfgvalue") %>'></asp:Label><br />
              </ItemTemplate>
            
          </asp:FormView>
      
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="FgValueCost" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmFgcost" Type="String" />
                  <asp:Parameter Name="prmDate" Type="DateTime" />
                  <asp:Parameter Name="prmDaysOld" Type="Int32" />
                  <asp:Parameter Name="prmBeginCust" Type="String"  />
                  <asp:Parameter Name="prmEndCust" Type="String" />
                  <asp:Parameter Name="prmBeginWhse" Type="String" />
                  <asp:Parameter Name="prmEndWhse" Type="String" />
                  <asp:Parameter Name="prmBeginLocBin" Type="String" />
                  <asp:Parameter Name="prmEndLocBin" Type="String" />
                  <asp:Parameter Name="prmBeginItem" Type="String" />
                  <asp:Parameter Name="prmEndItem" Type="String" />
                  <asp:Parameter Name="prmBeginCat" Type="String" />
                  <asp:Parameter Name="prmEndCat" Type="String" />
                  <asp:Parameter Name="prmSort" Type="String" />
                  <asp:Parameter Name="prmIcode" Type="String" />
                  <asp:Parameter Name="prmPrint" Type="String" />
                  <asp:Parameter Name="prmFrom" Type="String" />
                  <asp:Parameter Name="prmSellPrice" Type="String" />
                  <asp:Parameter Name="prmIncludeZero" Type="String" />
                  <asp:Parameter Name="prmIncludeCustWhse" Type="String" />
                  <asp:Parameter Name="prmIncludeOnlyCustWhse" Type="String" />
                  <asp:Parameter Name="prmPrintCost" Type="String" />
                  <asp:Parameter Name="prmDlMat" Type="String" />
                  <asp:Parameter Name="prmPrintCustPart" Type="String" />
                  <asp:Parameter Name="prmPrintPo" Type="String" />
                  <asp:Parameter Name="prmPoType" Type="String" />
                  <asp:Parameter Name="prmPrintDate" Type="String" />
                  <asp:Parameter Name="prmPrintCompOnly" Type="String" />
                  <asp:Parameter Name="prmPrintSubTot" Type="String" />
                  <asp:Parameter Name="prmPrintActRelQty" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
    </div>
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>

