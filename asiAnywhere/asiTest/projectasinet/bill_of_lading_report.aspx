<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="bill_of_lading_report" Codebehind="bill_of_lading_report.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<%@ Register TagPrefix="uc" TagName="Signature"  Src="~/SignatureControl/ctlSignature.ascx" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Print Bills of Lading Report</title>
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
 function samebol()
 {
 var begbol = document.getElementById("TextBox3");
 var endbol = document.getElementById("TextBox4");
 endbol.value=begbol.value;
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
  document.forms[0].TextBox2.focus();
    
}

function bolnumlook() {
    var posted = "No";
    var check1 = document.getElementById("CheckBox_post");
    if (check1.checked)
        posted = "Yes";
    
    var custval = document.forms[0].TextBox1.value;
    var NewWindow = window.open("bolnum_rep_lookup.aspx?customer=" + custval +"&post="+ posted +"", "BolNumLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function bolnumreplook(ReturnObj1){ 
  document.forms[0].TextBox3.value = ReturnObj1;
  document.forms[0].TextBox4.value = ReturnObj1;
  document.forms[0].TextBox4.focus();
}
function bolnumendlookup() {
    var posted = "No";
    var check1 = document.getElementById("CheckBox_post");
    if (check1.checked)
        posted = "Yes";   
    var custval = document.forms[0].TextBox2.value;
    var NewWindow = window.open("bolnum_end_lookup.aspx?customer=" + custval + "&post=" + posted + "", "BolNumLookupWindow", "width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function bolnumendlook(ReturnObj1){
    document.forms[0].TextBox4.value = ReturnObj1;
    document.forms[0].TextBox4.focus();
}

function Datelook(){ 
  var NewWindow = window.open("date_lookup.aspx","DateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
  }
function Datelookup(obj)
{
    document.forms[0].TextBox9.value = obj;
    document.forms[0].TextBox9.focus();
}
function Datelook1()
{
  document.forms[0].TextBox9.value="";
  Datelook();
}
    </script> 
    <script language="javascript">
        function EnableSaveButton(enable) {
            // you can use the javascript variable to check if there is a valid signature
            // also there is IsValid property for server side (see ReadMe.pdf)

            var btnSave = document.getElementById("btnSave");

            if (enable) {
                if (window.frmSign.IsValid) {
                    btnSave.disabled = "";
                }
                else {
                    btnSave.disabled = "disabled";
                }
            }
            else
                btnSave.disabled = "disabled";
        }
		  
		</script>

      <link href="include/tree.css" rel="stylesheet" type="text/css" />
  </head>    
   <body>
    <form id="frmList" runat="server"  defaultfocus='TextBox1'>   
        <hd:header id="Header1" runat="server"></hd:header>
      <div>       
            
                       
      <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
        <TR>
            
          <TD align=center nowrap><font size=+0><b>Print Bills of Lading&nbsp;</b></font></TD>
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
         
         <asp:HiddenField ID="HiddenField7" runat="server" />
         <asp:HiddenField ID="HiddenField8" runat="server" />
         <asp:HiddenField ID="HiddenField9" runat="server" />
         <asp:HiddenField ID="HiddenField10" runat="server" />
         <asp:HiddenField ID="HiddenField11" runat="server" />
         
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
      <tr><td></td><td >
          <b><asp:CheckBox ID="CheckBox_post" Text="Posted" runat="server" /></b></td></tr>
      <tr><td align="right" style="padding-right: 5px"><b>Begining Customer#:</b></td><td>
          <asp:TextBox ID="TextBox1"  onkeyup="samevalue()" MaxLength="8" width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="contactcustomerlook(); return false"><asp:Image ID="CustomerLook" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
          </td>
      <td align="right" style="padding-right: 5px"><b>Ending Customer#:</b></td><td><asp:TextBox ID="TextBox2" MaxLength="8" width="100px" runat="server"></asp:TextBox></td>
        </tr>
      <tr><td align="right" style="padding-right: 5px"><b>Begining BOL#:</b></td>
          <td nowrap><asp:TextBox onkeyup="samebol()" ID="TextBox3" MaxLength="8" Width="100px" runat="server"></asp:TextBox>
          <a href="#" tabindex="1" onClick="bolnumlook(); return false"><asp:Image ID="BolLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
              <asp:CompareValidator ID="CompareValidator1" ControlToValidate="TextBox3" runat="server" Display="Dynamic" Operator="DataTypeCheck" Type="integer" ErrorMessage="Only Integer Value"></asp:CompareValidator>
          </td>
        <td align="right" style="padding-right: 5px"><b>Ending BOL#:</b></td>
          <td nowrap><asp:TextBox ID="TextBox4" MaxLength="8" Width="100px" runat="server"></asp:TextBox>
         <a href="#" tabindex="1" onClick="bolnumendlookup(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
         <asp:CompareValidator ID="CompareValidator2" ControlToValidate="TextBox4" runat="server" Display="Dynamic" Operator="DataTypeCheck" Type="integer" ErrorMessage="Only Integer Value"></asp:CompareValidator>
          </td>
          </tr>
          
            <tr> <td align="right" style="padding-right: 5px"><b>Begining Order#:</b></td>
          <td nowrap><asp:TextBox ID="TextBox5" MaxLength="8" Width="100px" runat="server"></asp:TextBox>
          <asp:CompareValidator ID="CompareValidator5" ControlToValidate="TextBox5" runat="server" Display="Dynamic" Operator="DataTypeCheck" Type="integer" ErrorMessage="Only Integer Value"></asp:CompareValidator>
          </td>
          <td align="right" style="padding-right: 5px"><b>Ending Order#:</b></td>
          <td nowrap><asp:TextBox ID="TextBox6" MaxLength="8" Width="100px" runat="server"></asp:TextBox>
          <asp:CompareValidator ID="CompareValidator3" ControlToValidate="TextBox6" runat="server" Display="Dynamic" Operator="DataTypeCheck" Type="integer" ErrorMessage="Only Integer Value"></asp:CompareValidator>
          </td></tr>
        
        
        </table>
          <table class="shade" width="520px">
          <tr><td align="center" ><b> &nbsp; &nbsp; &nbsp;<asp:CheckBox ID="CheckBox1" Text="Reprint Bill Of Ladings?" runat="server" /></b></td>          
          </tr>
             <tr>
          <td align="center" ><b>&nbsp; &nbsp; &nbsp;&nbsp;
              <asp:CheckBox ID="CheckBox2" text="Print Number Of Pallets?" runat="server" /></b></td>
         
          </tr>
          <%--<tr><td align="Center" ><b> <asp:CheckBox ID="CheckBox3" Text="Reprint Posted BOL?" runat="server" /></b></td></tr>--%>
          
          <tr><td align="center"><b>&nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
              <asp:CheckBox ID="CheckBox4" text = "Print Bar Coded Pack List?" runat="server" /></b></td>
          
          </tr>
           <tr><td ><b>&nbsp; &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;
           &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;&nbsp; &nbsp;&nbsp; 
              <asp:CheckBox ID="RePrintCheckBox" text = "Reprint Posted Bill of Lading" runat="server" /></b>
              &nbsp;&nbsp; &nbsp;
              <b><asp:CheckBox ID="PostedCheckBox" Text="Post Bol?" runat="server" /></b>
              </td>
          
          </tr>           
        
           
         <tr><td nowrap colspan="2"><b>&nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
             &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp;
             Print?</b> &nbsp; &nbsp;&nbsp;
             <asp:RadioButtonList ID="RadioButtonList1" RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="5" Font-Bold ="true" runat="server">
             
                 
                 <asp:ListItem      Text="BOL" />
                  <asp:ListItem     Text="Certificate For Compliance" />
                 
         </asp:RadioButtonList>&nbsp;</td></tr>
        <tr><td>
              <asp:Button ID="submitbutton" OnClick="submitbutton_click" runat="server" class="buttonM" Text="Submit" /></td>
          </tr>     
         </table>
            <asp:FormView ID="FormView1" Visible="false"  runat="server" DataSourceID="ObjectDataSource1" OnPreRender="FormView1_PreRender">
                            
              <ItemTemplate>
                  abc:
                  <asp:Label ID="abcLabel" runat="server" Text='<%# Bind("abc") %>'></asp:Label><br />
                  vbilloflad:
                  <asp:Label ID="vbillofladLabel" runat="server" Text='<%# Bind("vbilloflad") %>'>
                  </asp:Label><br />
              </ItemTemplate>
                
               
               
          </asp:FormView>
      
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
              SelectMethod="SeBillofRep" TypeName="reports">
              <SelectParameters>
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:Parameter Name="prmBillAct" Type="String" />
                  <asp:Parameter Name="prmBeginCust" Type="String"  />
                  <asp:Parameter Name="prmEndCust" Type="String" />
                  
                  <asp:Parameter Name="prmBeBoll" Type="Int32" />
                  <asp:Parameter Name="prmEndBoll" Type="Int32" />
                  <asp:Parameter Name="prmBeOrder" Type="Int32" />
                  <asp:Parameter Name="prmEndOrder" Type="Int32" />
                  <asp:Parameter Name="prmBillofLad" Type="String" />
                  <asp:Parameter Name="prmPallet" Type="String" />
                  <asp:Parameter Name="prmPostedBol" Type="String" />
                  <asp:Parameter Name="prmPackList" Type="String" />
                  <asp:Parameter Name="prmPrint" Type="String" />
                  <asp:Parameter Name="prmBolPost" Type="String" />               
                  
              </SelectParameters>
          </asp:ObjectDataSource>
    </div>
    
    
    <ft:footer id="Footer1" runat="server"></ft:footer>
    </form>
  </body>
</HTML>


