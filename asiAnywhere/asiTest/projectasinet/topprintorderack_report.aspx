<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="topprintorderack_report" Codebehind="topprintorderack_report.aspx.cs" %>

<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Print Order Acknowledgements</title>
    
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
     
    <link href="include/tree.css" rel="stylesheet" type="text/css" />
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
    <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >
        <script language="javascript" src="include/date.js"></script>
    <script language="javascript" src="include/event.js"></script>
    <script language="javascript" src="include/insert.js"></script>
    <script language="javascript"  src="include/CalendarControl.js" > </script>
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
  document.forms[0].endcust_TextBox.value = ReturnObj1;
    
}
function contactcustomerlook2(){ 
  var NewWindow = window.open("contact_customer_copylookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerCopyLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11){ 
  
  document.forms[0].endcust_TextBox.value = ReturnObj1;
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
    
    </script> 

      <link href="include/tree.css" rel="stylesheet" type="text/css" />
  </head>    
   <body>
    <form id="frmList" runat="server"   defaultfocus ='txt_supplierscode' >   
       
      <div>
                      
      <TABLE id="tblTop" cellSpacing="3" border="0">
        <TR>
          
          <TD align=center nowrap><font size=+0><b>Print Order Acknowledgements&nbsp;</b></font></TD>
          
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
       <fieldset class="shade">
      <table class="shade">
        <tr>
            <td align="right" style="padding-right:5px"><b>Beginning Order#:</b></td>
            <td>
                <asp:TextBox ID="beginorder_TextBox" runat="server"></asp:TextBox>
                <a href="#" onClick="orderlook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                <asp:CompareValidator ID="begain_order_validate" runat="server" Display="dynamic" ControlToValidate="beginorder_TextBox" Type="integer" Operator="DataTypeCheck" ErrorMessage="Invalid Data"></asp:CompareValidator>
            </td>
            <td align="right" style="padding-right:5px"><b>Ending Order#:</b></td>
            <td>
                <asp:TextBox ID="endorder_TextBox" runat="server"></asp:TextBox>
                <a href="#" onClick="order2look(); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                <asp:CompareValidator ID="CompareValidator1" runat="server" Display="dynamic" ControlToValidate="endorder_TextBox" Type="integer" Operator="DataTypeCheck" ErrorMessage="Invalid Data"></asp:CompareValidator>
            </td>
        </tr>
        <tr>
            <td align="right" style="padding-right:5px"><b>Beginning Customer#:</b></td>
            <td nowrap>
                <asp:TextBox ID="begincust_TextBox" onkeyup="samevalue()" onblur="samevalue()" runat="server"></asp:TextBox>
                <a href="#" onClick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td align="right" style="padding-right:5px"><b>Ending Customer#:</b></td>
            <td>
                <asp:TextBox ID="endcust_TextBox"  runat="server"></asp:TextBox>
                 <a href="#" onClick="contactcustomerlook2(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
        </tr>
        
        <tr>
            <td align="right" style="padding-right:5px"><b>Beginning Order Date:</b></td>
            <td>
                <asp:TextBox ID="txt_begin_date" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );" ToolTip="MM/DD/YYYY" runat="server"></asp:TextBox>
                
                <a href="#" onClick="showCalendarControl(txt_begin_date); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
            <td align="right" style="padding-right:5px"><b>Ending Order Date:</b></td>
            <td>
                <asp:TextBox ID="txt_end_date" onfocus="javascript:preEnter( this, 'yes' );" onblur="javascript:preLeave( this, 'date', '99/99/9999' );"  ToolTip="MM/DD/YYYY" runat="server"></asp:TextBox>
               
               <a href="#" onClick="showCalendarControl(txt_end_date); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
            </td>
        </tr>
        <tr>
            <td align="right" style="padding-right:5px"><b>Beginning Release#:</b></td>
            <td>
                <asp:TextBox ID="txt_begin_rel" runat="server"></asp:TextBox>                
                <%--<a href="#" onClick="job1look(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>--%>
            </td>
            <td align="right" style="padding-right:5px"><b>Ending Release#:</b></td>
            <td>
                <asp:TextBox ID="txt_end_rel" runat="server"></asp:TextBox>                
                <%--<a href="#" onClick="job1translook(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>--%>
            </td>
        </tr>
        </table>
        </fieldset>   
        <fieldset class="shade">
            <table>
                <tr>
                    <td></td>
                    <td nowrap>
                        <b><asp:CheckBox ID="chk_reprint_ack" Text="Do you want to reprint Acknowledgement?" runat="server" /></b>
                    </td>            
                    <td>
                        <b><asp:CheckBox ID="chk_cons_form" Text="Consolidate Forms" runat="server" /></b>
                    </td>
                </tr>
                <tr>
                    <td></td>
                    <td nowrap>
                        <b><asp:CheckBox ID="chk_ware_months" Enabled="false" Text="WareHouse Months" runat="server" /></b>
                    </td>
                    <td>
                        <b><asp:RadioButtonList ID="rdl_month" Enabled="false" runat="server" RepeatColumns="2" RepeatLayout="flow">
                            <asp:ListItem>3 Months</asp:ListItem>
                            <asp:ListItem>6 Months</asp:ListItem>
                        </asp:RadioButtonList></b>
                    </td>
                </tr>
            </table>
        </fieldset>
        <fieldset class="shade">
            <table>
                <tr>
                    <td nowrap>
                       <b> <asp:CheckBox ID="chk_sch_rel" runat="server" Text="Print Scheduled Releases?" /></b>
                    </td>
                    <td nowrap>
                       <b> <asp:CheckBox ID="chk_spec_notes" runat="server" Text="Print Spec Notes?" /></b>
                    </td>
                    <td nowrap>
                       <b> <asp:CheckBox ID="chk_ship" runat="server" Text="Print ShipTo Address?" /></b>
                    </td>
                </tr>
                 <tr>
                    <td nowrap>
                       <b> <asp:CheckBox ID="chk_act_rel" runat="server" Text="Print Actual Releases?" /></b>
                    </td>
                    <td nowrap>
                      <b>  <asp:CheckBox ID="chk_revised" runat="server" Text="Print REVISED?" /></b>
                    </td>
                    <td nowrap>
                      <b>  <asp:CheckBox ID="chk_bom" runat="server" Text="Print Bill of Material?" /></b>
                    </td>
                </tr>
            </table>
        </fieldset>
        
       <%-- <tr>
            <td >
                <b>Output To:</b><b><asp:RadioButtonList ID="RadioButtonList8" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
                  <asp:ListItem      Text="Pdf" />
                  <asp:ListItem      Text="Excel" />
                  
                 </asp:RadioButtonList></b>
           </td></tr>--%>
           
          <table><tr><td colspan="2"><asp:Button ID="SubmitButton" runat="server" CssClass="button" Text="Submit" OnClick="SubmitButton_Click"></asp:Button> 
                  <%--&nbsp;&nbsp;<asp:Label ID="OutputLabel" runat="server" Text="Output File:" Font-Bold="True" Font-Size="Larger" ForeColor="Blue"></asp:Label>
          &nbsp;
          <asp:HyperLink ID="HyperLink1" runat="server"  Target="_blank" Font-Bold="True" Font-Size="Larger" ForeColor="Red"></asp:HyperLink> --%>
                              </td></tr></table>
                              
                              
         <asp:FormView ID="FormView1" Visible="False" runat="server" DataSourceID="ObjectDataSource1" OnPreRender="FormView1_PreRender">
            
            <ItemTemplate>
                vFile:
                <asp:Label ID="vFileLabel" runat="server" Text='<%# Bind("vFile") %>'></asp:Label><br />
                vvsdss:
                <asp:Label ID="vvsdssLabel" runat="server" Text='<%# Bind("vvsdss") %>'></asp:Label><br />
            </ItemTemplate>
             <EditItemTemplate>
                 vFile:
                 <asp:TextBox ID="vFileTextBox" runat="server" Text='<%# Bind("vFile") %>'>
                 </asp:TextBox><br />
                 vvsdss:
                 <asp:TextBox ID="vvsdssTextBox" runat="server" Text='<%# Bind("vvsdss") %>'>
                 </asp:TextBox><br />
                 <asp:LinkButton ID="UpdateButton" runat="server" CausesValidation="True" CommandName="Update"
                     Text="Update">
                 </asp:LinkButton>
                 <asp:LinkButton ID="UpdateCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                     Text="Cancel">
                 </asp:LinkButton>
             </EditItemTemplate>
             <InsertItemTemplate>
                 vFile:
                 <asp:TextBox ID="vFileTextBox" runat="server" Text='<%# Bind("vFile") %>'>
                 </asp:TextBox><br />
                 vvsdss:
                 <asp:TextBox ID="vvsdssTextBox" runat="server" Text='<%# Bind("vvsdss") %>'>
                 </asp:TextBox><br />
                 <asp:LinkButton ID="InsertButton" runat="server" CausesValidation="True" CommandName="Insert"
                     Text="Insert">
                 </asp:LinkButton>
                 <asp:LinkButton ID="InsertCancelButton" runat="server" CausesValidation="False" CommandName="Cancel"
                     Text="Cancel">
                 </asp:LinkButton>
             </InsertItemTemplate>
             
            
             
        </asp:FormView>
        <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}" SelectMethod="TopOrderAckReport" TypeName="reports">
            <SelectParameters>
                <asp:Parameter Name="prmUser" Type="String" />
                <asp:Parameter Name="prmAction" Type="String" />
                <asp:Parameter Name="prmOut" Type="String" />
                <asp:Parameter Name="prmBeginOrder" Type="Int32" />
                <asp:Parameter Name="prmEndOrder" Type="Int32" />
                <asp:Parameter Name="prmBeginCust" Type="String" />
                <asp:Parameter Name="prmEndCust" Type="String" />
                <asp:Parameter Name="prmBeginOrdDate" Type="DateTime" />
                <asp:Parameter Name="prmEndOrdDate" Type="DateTime" />
                <asp:Parameter Name="prmBeginRel" Type="Int32" />
                <asp:Parameter Name="prmEndRel" Type="Int32" />
                <asp:Parameter Name="prmReprintAck" Type="String" />
                <asp:Parameter Name="prmConsForm" Type="String" />
                <asp:Parameter Name="prmWareHouse" Type="String" />
                <asp:Parameter Name="prmMonths" Type="String" />
                <asp:Parameter Name="prmSchRel" Type="String" />
                <asp:Parameter Name="prmSpecNotes" Type="String" />
                <asp:Parameter Name="prmShipAddr" Type="String" />
                <asp:Parameter Name="prmActRel" Type="String" />
                <asp:Parameter Name="prmPrintRevised" Type="String" />
                <asp:Parameter Name="prmBillMat" Type="String" />
            </SelectParameters>
        </asp:ObjectDataSource>
    </div>
       
    </form>
</body>
</html>
