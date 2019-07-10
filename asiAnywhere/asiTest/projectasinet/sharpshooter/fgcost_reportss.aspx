<%@ Page Language="c#" AutoEventWireup="true" Debug="false" Inherits="projectasinet.sharpshooter.fgcost_reportss" Codebehind="fgcost_reportss.aspx.cs" %>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>DL, Mat, & GSA by Whs/Bin/Tag</title>
    <LINK href=../include/styless.css" type="text/css" rel="stylesheet"/>    
    <script language="javascript" src="../include/insert.js"></script>
    <LINK REL="stylesheet" TYPE="text/css" HREF="../include/CalendarControl.css" >
    <script language = "JavaScript" src="../include/CalendarControl.js"></script>
    <script language="javascript" src="../include/date.js"></script>
    <script language="javascript" src="../include/event.js"></script>
        
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
    function samevalue() {
        var beginc = document.getElementById("TextBox2");
        var endc = document.getElementById("TextBox5");
        endc.value = beginc.value;
    }

    function contactcustomerlook() {
        var NewWindow = window.open("contact_customer_lookup.aspx", "ContactCustomerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function ContactCustomerLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11) {
        document.forms[0].TextBox2.value = ReturnObj1;
        document.forms[0].TextBox2.focus();
    }
    function contactcustomerlook2() {
        var NewWindow = window.open("contact_customer_copylookup.aspx", "ContactCustomerLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function ContactCustomerCopyLookup(ReturnObj1, ReturnObj2, ReturnObj3, ReturnObj4, ReturnObj5, ReturnObj6, ReturnObj7, ReturnObj8, ReturnObj9, ReturnObj10, ReturnObj11) {

        document.forms[0].TextBox5.value = ReturnObj1;
        document.forms[0].TextBox5.focus();
    }


    function Relook() {
        var item1 = document.getElementById("TextBox2").value;
        var NewWindow = window.open("reorder_item_lookup.aspx?item=" + item1 + "", "ReItemLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function ReLookup(ReturnObj1) {
        document.forms[0].TextBox3.value = ReturnObj1;
        document.forms[0].TextBox3.focus();
    }
    function Relook2() {
        var item2 = document.getElementById("TextBox5").value;
        var NewWindow = window.open("reorder_item_lookup2.aspx?item1=" + item2 + "", "ReItemLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }

    function ReLookup2(ReturnObj1) {
        document.forms[0].TextBox4.value = ReturnObj1;
        document.forms[0].TextBox4.focus();
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

    function procatlook() {
        var NewWindow = window.open("procat_lookup.aspx", "ProcatLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function procatLookup(ReturnObj1) {
        document.forms[0].BeCatTextBox.value = ReturnObj1;
        document.forms[0].BeCatTextBox.focus();
    }
    function procat2look() {
        var NewWindow = window.open("procat_lookup2.aspx", "ProcatLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
    }
    function procat2Lookup(ReturnObj1) {
        document.forms[0].EndCatTextBox.value = ReturnObj1;
        document.forms[0].EndCatTextBox.focus();
    }
      
   </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='TextBox1'>   
       
      <div>
        <asp:HiddenField ID="HiddenField1" runat="server" />  
        <asp:HiddenField ID="HiddenField2" runat="server" />  
            
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=center nowrap><font size=+0><b>DL, Mat, & GSA by Whs/Bin/Tag&nbsp;</b></font></TD>
          <TD vAlign="middle" nowrap >
            <asp:linkbutton id="hlkBackToMenu" OnClick="Back_tomenu_Click" runat="server" ></asp:linkbutton>
          </TD>
          <TD vAlign="middle" align="center" nowrap >Logged as&nbsp;
            <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;
            <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>            
          &nbsp;<b>Company:</b>&nbsp;<asp:Label ID="labelcompany" runat="server" Text="Label"></asp:Label></TD>
          
         
          <TD vAlign="middle" width="20">&nbsp;</TD>
          
          <td width=30>&nbsp;</td>
        </TR>
      </TABLE>
       <asp:Label ID="Label1" runat="server" ForeColor="red" Font-Bold="true"></asp:Label>
       <asp:FormView ID="FormView2" Visible="false" runat="server" DataSourceID="ObjectDataSource2">
                            
              <ItemTemplate>                  
                  <asp:Label ID="CustLabel"  runat="server" Text='<%# Bind("Cust") %>'></asp:Label><br />
              </ItemTemplate>
          </asp:FormView>
          <asp:ObjectDataSource ID="ObjectDataSource2" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="FillAlphabeticList" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmComp" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
       
      <table class="shade" >
      <tr>
        <td colspan="2">&nbsp;</td>
        <td align="right" style="padding-right: 5px; " nowrap><b>As of:</b></td>
          <td>
            <asp:TextBox ID="TextBox1" onfocus="this.select();javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"  runat="server" Width="100px"></asp:TextBox>
            <a href="#" onblur="document.getElementById('TextBox1').focus()"  tabindex="1" onClick="showCalendarControl(TextBox1); return false"><asp:Image ID="Image5" runat="server" ImageUrl="~/images/lookup_icon.gif" /></a>          
         </td>      
      </tr>       
      <tr><td align="right" style="padding-right: 5px"><b>Begining Customer#:</b></td><td>
          <asp:TextBox ID="TextBox2" MaxLength="8" onkeyup="samevalue()"  width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="~/images/lookup_icon.gif" /></a>
          </td>
        <td align="right" style="padding-right: 5px"><b>Ending Customer#:</b></td><td><asp:TextBox ID="TextBox5" MaxLength="8" width="100px" runat="server"></asp:TextBox>
            <a href="#" tabindex="1" onClick="contactcustomerlook2(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="~/images/lookup_icon.gif" /></a>
        </td>
      </tr>           
      <tr>
        <td align="right" style="padding-right: 5px"><b>Begining Warehouse:</b></td>
          <td><asp:TextBox MaxLength="5" ID="TextBox7" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="LocationLook(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="~/images/lookup_icon.gif" /></a>
        </td>
        <td align="right" style="padding-right: 5px"><b>Ending Warehouse:</b></td>
          <td><asp:TextBox MaxLength="5" ID="TextBox8" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="Location2Look(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="~/images/lookup_icon.gif" /></a>
        </td>
       </tr>
       <tr>
         <td align="right" style="padding-right: 5px"><b>Begining Item#:</b></td>
          <td nowrap><asp:TextBox MaxLength="15" ID="TextBox3" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="Relook(); return false"><asp:Image ID="FGLookup" runat="server" ImageUrl="~/images/lookup_icon.gif" /></a>
          </td>
         <td align="right" style="padding-right: 5px"><b>Ending Item#:</b></td>
          <td nowrap><asp:TextBox MaxLength="15" ID="TextBox4" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="Relook2(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="~/images/lookup_icon.gif" /></a>
          </td>
        </tr>          
        <tr> 
            <td align="right" style="padding-right: 5px"><b>Begining Category:</b></td>
            <td><asp:TextBox MaxLength="8" ID="BeCatTextBox" Width="100px" runat="server"></asp:TextBox>
                <a href="#" tabindex="1" onClick="procatlook(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="~/images/lookup_icon.gif" /></a>
            </td>
                <td align="right" style="padding-right: 5px"><b>Ending Category:</b></td>
          <td><asp:TextBox MaxLength="8" ID="EndCatTextBox" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="procat2look(); return false"><asp:Image ID="Image8" runat="server" ImageUrl="~/images/lookup_icon.gif" /></a>
          </td>
        </tr> 
        <tr></tr><tr></tr>
        <tr>
            <td nowrap align="right"><b>Item Code?</b></td>
            <td colspan="3"> <asp:RadioButtonList ID="RadioButtonList1" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">                              
                 <asp:ListItem      Text="Stock" />
                  <asp:ListItem     Text="Custom" />
                 <asp:ListItem      Text="All" />                   
         </asp:RadioButtonList> &nbsp;</td>
         </tr>
         
       <tr><td nowrap align="right"><b>Sort?</b></td>
            <td colspan="3"> <asp:RadioButtonList ID="RadioButtonList2" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
                 <asp:ListItem      Text="Customer#" />
                  <asp:ListItem     Text="FG Item#" />
                 <asp:ListItem      Text="Part#" />
                   <asp:ListItem    Text="Product Category" />
                   <asp:ListItem    Text="Whs/Bin" />
         </asp:RadioButtonList> &nbsp;</td></tr>                  
         
       <tr><td nowrap align="right"><b>Print?</b></td>
             <td colspan="3"><asp:RadioButtonList ID="RadioButtonList3" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">                             
                 <asp:ListItem      Text="Qty" />
                  <asp:ListItem     Text="MSF" />                                  
         </asp:RadioButtonList> &nbsp;</td>
       </tr>
     
       <tr>
            <td>&nbsp;</td>
            <td colspan="3" style="padding-left:80px">
                <b><asp:CheckBox ID="CheckBox1" Text="Print Customer Part#" runat="server" /></b>
            </td>        
       </tr>
        
                        
      <tr><td colspan="4" align="left" style="padding-left:10px">
            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                <b>Output to?  
                    <asp:RadioButtonList ID="RadioButtonList_out"  RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">                              
                        <asp:ListItem   Value="No"   Text="Text File" />
                        <asp:ListItem  Value="Yes"  Text="Excel" />                 
                    </asp:RadioButtonList>
                </b>
         </td></tr>                    
          <tr><td colspan="3">
            &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
            <asp:Button ID="submitbutton" OnClick="submitbutton_click" CausesValidation="true" runat="server" class="buttonM" Text="Submit" />
          &nbsp;&nbsp;&nbsp;&nbsp;
          <asp:Label ID="OutPutFile" runat="server" Text="Output File:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
              <asp:HyperLink ID="HyperLink1" runat="server" Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink>
          </td></tr>
          </table>
       
          <asp:FormView ID="FormView1" Visible="False" runat="server" 
              DataSourceID="ObjectDataSource1">                                                                                                                                                             
              <ItemTemplate>
                  vFgCostFile:
                  <asp:Label ID="vFgCostFileLabel" runat="server" 
                      Text='<%# Bind("vFgCostFile") %>'></asp:Label><br />                 
              </ItemTemplate>                            
          </asp:FormView>
          
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="SelectFgcostRep" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmAction" Type="String" />
                  <asp:Parameter Name="prmAsofdt" Type="String" />
                  <asp:Parameter Name="prmBeginCust" Type="String" />
                  <asp:Parameter Name="prmEndCust" Type="String" />
                  <asp:Parameter Name="prmBeginWhse" Type="String" />
                  <asp:Parameter Name="prmEndWhse" Type="String" />
                  <asp:Parameter Name="prmBeginItem" Type="String" />
                  <asp:Parameter Name="prmEndItem" Type="String" />
                  <asp:Parameter Name="prmBeginCat" Type="String" />
                  <asp:Parameter Name="prmEndCat" Type="String" />
                  <asp:Parameter Name="prmIcode" Type="String" />
                  <asp:Parameter Name="prmSort" Type="String" />
                  <asp:Parameter Name="prmPrint" Type="String" />
                  <asp:Parameter Name="prmCustPart" Type="String" />
                  <asp:Parameter Name="prmOut" Type="String" />
              </SelectParameters>
          </asp:ObjectDataSource>
     
    </div>
   
    </form>
  </body>
</HTML>


