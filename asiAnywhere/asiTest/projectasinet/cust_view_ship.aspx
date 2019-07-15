<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="customers_view_ship" Codebehind="cust_view_ship.aspx.cs" %>

<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>Ship To</title>
    <LINK href="include/style2.css" type="text/css" rel="stylesheet"/>
   <LINK REL="stylesheet" TYPE="text/css" HREF="include/CalendarControl.css" >

    <script language = "JavaScript" src="include/CalendarControl.js">
    </script>
  
    
<script language="javascript"  >


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
function focusval(obj)
{
    obj.style.backgroundColor='blue';
    obj.style.color = 'white';
}
function blurval(obj)
{
    obj.style.backgroundColor='Window';
    obj.style.color='WindowText';
} 
function timeblurval(obj)
{
    obj.style.backgroundColor='Window';
    obj.style.color='WindowText';
    if(document.getElementById("FormView1_vshipidTextBox"))
    {
        var shipid=document.getElementById("FormView1_vshipidTextBox");
        shipid.focus();
    }
    else if(document.getElementById("FormView1_vshipnameTextBox"))
    {
        var name=document.getElementById("FormView1_vshipnameTextBox");
        name.focus();
    }
}    
    
function zipcodelook()
{ 
    var NewWindow = window.open("zipcode_lookup.aspx","ZipCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ZipCodeLookup(ReturnObj1, ReturnObj2, ReturnObj3)
{  
    document.forms[0].FormView1_vshipcityTextBox.value = ReturnObj2;
    document.forms[0].FormView1_vshipzipTextBox.value = ReturnObj1;
    document.forms[0].FormView1_vshipstateTextBox.value = ReturnObj3;  
}
function citylook()
{ 
    var NewWindow = window.open("city_lookup.aspx","CityCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function CityCodeLookup(ReturnObj1)
{  
   document.forms[0].FormView1_vshipcityTextBox.value = ReturnObj1;    
}
function statecodelook()
{ 
    var NewWindow = window.open("statecode_lookup.aspx","StateCodeLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function StateCodeLookup(ReturnObj1)
{ 
  document.forms[0].FormView1_vshipstateTextBox.value = ReturnObj1;
}

function carrierlook()
{ 
    var loc1 = document.getElementById("FormView1_vlocTextBox").value;
    var NewWindow = window.open("custcarr_lookup.aspx?loc="+loc1+"","CarrierlookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Carrlookup(ReturnObj1)
{   
  document.forms[0].FormView1_vcarrierTextBox.value = ReturnObj1;  
}
 
function currencylook()
{ 
    var NewWindow = window.open("currency_lookup.aspx","CurrencyLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function deliveryzonelook()
{ 
    var carr = document.getElementById("FormView1_vcarrierTextBox").value;
    var NewWindow = window.open("zone_lookup.aspx?zone="+carr+"","DelZoneLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function zoneLookup(ReturnObj1, ReturnObj2)
{  
    document.forms[0].FormView1_vdestcodeTextBox.value = ReturnObj1;  
}
 
function shiptolook()
{ 
    var NewWindow = window.open("contact_customer_lookup.aspx","ShipTOLookupWindow","width=630,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function ContactCustomerLookup(ReturnObj1, ReturnObj2,ReturnObj3, ReturnObj4,ReturnObj5, ReturnObj6,ReturnObj7,ReturnObj8, ReturnObj9,ReturnObj10, ReturnObj11,ReturnObj12)
{  
    document.forms[0].FormView1_vshipidTextBox.value = ReturnObj1; 
    document.forms[0].FormView1_vshipnameTextBox.value = ReturnObj2; 
    document.forms[0].FormView1_vshipaddr1TextBox.value = ReturnObj3; 
    document.forms[0].FormView1_vshipaddr2TextBox.value = ReturnObj4; 
    document.forms[0].FormView1_vshipcityTextBox.value = ReturnObj5; 
    document.forms[0].FormView1_vshipstateTextBox.value = ReturnObj6; 
    document.forms[0].FormView1_vshipzipTextBox.value = ReturnObj7;  
    document.forms[0].FormView1_vcarrierTextBox.value = ReturnObj12;  
}
 
function taxcodelook()
{ 
    var NewWindow = window.open("tax_lookup.aspx","TaxLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function TaxLookup(ReturnObj1, ReturnObj2)
{  
  document.forms[0].FormView1_vtaxcodeTextBox.value = ReturnObj1;  
}
 
function locationlook()
{ 
    var NewWindow = window.open("location_lookup.aspx","LocationLookUpWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function LocationLookUp(ReturnObj1, ReturnObj2)
{ 
    
    document.forms[0].FormView1_vlocTextBox.value = ReturnObj1;     
 }
 
function binlook()
{ 
    var loc1 = document.getElementById("FormView1_vlocTextBox").value;
    var NewWindow = window.open("custbin_lookup.aspx?binloc="+loc1+"","BinLookUpWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function CustBinLookup(ReturnObj1, ReturnObj2)
{ 
    document.forms[0].FormView1_vlocbinTextBox.value = ReturnObj1;  
}
function palletlook()
{ 
  var NewWindow = window.open("custplate_lookup.aspx","PalletLookUpWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function plateLookup(ReturnObj1)
{ 
  document.forms[0].FormView1_vpalletTextBox.value = ReturnObj1;  
}

    
 </script>

 </head>    

   <body>
        <form id="frmList" runat="server"  defaultfocus='cust_TextBox'>   
            <hd:header id="Header1" runat="server"></hd:header>
                <div>            
                    <TABLE id="tblTop" cellSpacing="3" align="center" border="0" Width="100%">
                        <TR>
                            <TD width=30>&nbsp;</TD>
                            <TD align=center nowrap><font size=+0><b>Ship To&nbsp;</b></font></TD>
                            <td nowrap>
                                <asp:LinkButton ID="backtomenuLinkButton" OnClick ="Back_tomenu_Click" runat="server">Back to menu</asp:LinkButton></td>
          
                            <TD  align="left" nowrap>Logged as&nbsp;
                                <asp:label id="lblUser" runat="server" Font-Bold="True">&nbsp;</asp:label>&nbsp;&nbsp;&nbsp;            
                                <asp:linkbutton id="hlnkLogOut" runat="server" OnClick="hlnkLogOut_Click">Log out</asp:linkbutton>
                                &nbsp;&nbsp;<asp:hyperlink id="hlnkChangePwd" runat="server" NavigateUrl="changepwd.aspx"></asp:hyperlink>
                                &nbsp;<b>Company: &nbsp;</b><asp:label id="labelcompany"   runat="server" Font-Bold="True">&nbsp;</asp:label>
                            </TD>
          
         
                            <TD vAlign="middle" width="20">&nbsp;</TD>          
                            <td width=30>&nbsp;</td>
                         </TR>
                    </TABLE>
                    <table>
                        <tr bgcolor="gray">
                            <td><div  id="navigation" style="width:100%">
		                        <ul nowrap> <li  > 
                                <asp:LinkButton ID="lnk_Listcustomers" runat="server" OnClick="lnk_Listcustomers_Click" >List Customers</asp:LinkButton></li>
                                <li><asp:LinkButton ID="lnk_viewcustomers" runat="server"  OnClick="lnk_viewcustomers_Click"  >View Customers</asp:LinkButton></li>
                                <li><asp:LinkButton ID="lnk_listship" runat="server" OnClick="lnk_listship_Click" >List Ship To</asp:LinkButton></li>
                                <li class="selected"><asp:LinkButton ID="lnk_viewship" runat="server"  OnClick="lnk_viewship_Click"  > View Ship To</asp:LinkButton></li>
                                <li><asp:LinkButton ID="lnk_listsold" runat="server" OnClick="lnk_listsold_Click" >List Sold To </asp:LinkButton></li>
                                <li><asp:LinkButton ID="lnk_viewsold" runat="server" OnClick="lnk_viewsold_Click" >View Sold To</asp:LinkButton></li></ul></div>
                            </td>      
                        </tr>
                    </table>
                    <fieldset style="background-color:#EFF3FB;  width:750px;">
                        <legend style="color:Blue ">Reference Information </legend>
                            <table width="750px" >
                                <tr>
                                    <td align="right" style="padding-right:5px"><b>Customer:</b></td>
                                    <td>
                                        <asp:Label ID="Customer" runat="server" Font-Bold="true" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"></asp:Label>
                                    </td>
                                    <td align="right" style="padding-right:5px"><b>Name:</b></td>
                                    <td>
                                        <asp:Label ID="CustName" Font-Bold="true" runat="server" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"></asp:Label>
                                    </td>
                               </tr>
                          </table>    
                     </fieldset>
                <asp:HiddenField ID="HiddenField1" runat="server" />
                <asp:HiddenField ID="HiddenField2" runat="server" />
                <asp:HiddenField ID="HiddenField3" runat="server" />
      
                    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" OnDataBound="FormView1_DataBound">
                        <EditItemTemplate>
                         <asp:Panel ID="update_panel" runat="server" DefaultButton="UpdateButton">
                           <fieldset>
                            <table class="shade">
                                <tr>
                                    <td align="right" style="padding-right:5px" nowrap><b>Ship To Id:</b></td>
                                    <td>
                                        <asp:Label ID="vshipidLabel" BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Width="150" Text='<%# Bind("vshipid") %>'></asp:Label>
                                    </td>
                                    <td align="right" style="padding-right:5px"><b>Contact:</b></td>
                                    <td>
                                        <asp:TextBox ID="vcontactTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="130" runat="server" Text='<%# Bind("vcontact") %>'></asp:TextBox>
                                    </td>
                                    <td align="right" style="padding-right:5px"><b>Dock#:</b></td>
                                    <td>
                                        <asp:TextBox ID="vdocklocTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" width="80px" runat="server" Text='<%# Bind("vdockloc") %>'></asp:TextBox>
                                    </td>
                                </tr>
                                <tr>
                                    <td align="right" style="padding-right:5px"><b>Name:</b></td>
                                    <td>
                                        <asp:TextBox ID="vshipnameTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="150" runat="server" Text='<%# Bind("vshipname") %>'></asp:TextBox>
                                    </td>
                                    <td align="right" style="padding-right:5px"><b>Phone:</b></td>
                                    <td> 
                                        <asp:TextBox ID="vareacodeTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="38px" MaxLength="3" runat="server" Text='<%# Bind("vareacode") %>'></asp:TextBox>
                                        
                                        <asp:TextBox ID="vphoneTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="90px" MaxLength="10" runat="server" Text='<%# Bind("vphone") %>'></asp:TextBox>
                                        
                                    </td>
                                    <td align="right" style="padding-right:5px"><b>Dock Hours:</b></td>
                                    <td>
                                        <asp:TextBox ID="vdockhourTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vdockhour") %>'></asp:TextBox>
                                    </td>
                                </tr>
                                <tr>
                                    <td align="right" style="padding-right:5px"><b>Address:</b></td>
                                    <td>
                                        <asp:TextBox ID="vshipaddr1TextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="150" runat="server" Text='<%# Bind("vshipaddr1") %>'></asp:TextBox>
                                    </td>
                                    <td align="right" style="padding-right:5px"><b>Fax:</b></td>
                                    <td nowrap>
                                        <asp:TextBox ID="vfaxAreaCodeTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="38px"  MaxLength="3" runat="server" Text='<%# Bind("vfaxAreaCode") %>'></asp:TextBox>
                                        
                                        <asp:TextBox ID="vfaxNumberTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="90px" MaxLength="10" runat="server" Text='<%# Bind("vfaxNumber") %>'></asp:TextBox>
                                        
                                    </td>
                                    <td align="right" style="padding-right:5px"><b>Warehouse:</b></td>
                                    <td nowrap>
                                        <asp:TextBox ID="vlocTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vloc") %>'></asp:TextBox>
                                        <a href="#" tabindex="1" onClick ="locationlook(); return false"><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                    </td>
                                </tr>
                                <tr>
                                    <td></td>
                                    <td>
                                        <asp:TextBox ID="vshipaddr2TextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="150" runat="server" Text='<%# Bind("vshipaddr2") %>'></asp:TextBox>
                                    </td>
                                    <td align="right" style="padding-right:5px"><b>JD Ediw:</b></td>
                                    <td>
                                        <asp:TextBox ID="vfi_jdedidTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="130" runat="server" Text='<%# Bind("vfi_jdedid") %>'></asp:TextBox>
                                    </td>
                                    <td align="right" style="padding-right:5px"><b>Bin:</b></td>
                                    <td nowrap>
                                        <asp:TextBox ID="vlocbinTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vlocbin") %>'></asp:TextBox>
                                        <a href="#" tabindex="1" onClick ="binlook(); return false"><asp:Image ID="binlookImage" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                    </td>
                                </tr>
                                <tr>
                                    <td align="right" style="padding-right:5px"><b>City:</b></td>
                                    <td nowrap> 
                                        <asp:TextBox ID="vshipcityTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px" runat="server" Text='<%# Bind("vshipcity") %>'></asp:TextBox>
                                        <a href="#" tabindex="1" onClick="citylook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a> 
                                        <asp:TextBox ID="vshipstateTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="30px" runat="server" Text='<%# Bind("vshipstate") %>'></asp:TextBox>
                                        <a href="#" tabindex="1" onClick="statecodelook(); return false"><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a> 
                                        <asp:TextBox ID="vshipzipTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="40px"  runat="server" Text='<%# Bind("vshipzip") %>'></asp:TextBox>
                                        <a href="#" onClick ="zipcodelook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                    </td>
                                    <td align="right" style="padding-right:5px"><b>Broker:</b></td>
                                    <td>
                                        <asp:CheckBox ID="vbrokerCheckBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Checked='<%# Bind("vbroker") %>' />
                                    </td>
                                    <td align="right" style="padding-right:5px"><b>Carrier:</b></td>
                                    <td>
                                        <asp:TextBox ID="vcarrierTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vcarrier") %>'></asp:TextBox>
                                        <a href="#" tabindex="1" onClick ="carrierlook(); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                    </td>
                                </tr>
                                <tr>
                                    <td align="right" style="padding-right:5px"><b>Tax Code:</b></td>
                                    <td nowrap colspan="3">
                                        <asp:TextBox ID="vtaxcodeTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vtaxcode") %>'></asp:TextBox>
                                        <a href="#" tabindex="1" onClick ="taxcodelook(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                    <b>Mandatory Tax:</b>
                                        <asp:TextBox ID="vtb_mandatorytaxTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="50px" runat="server" Text='<%# Bind("vtb_mandatorytax") %>'></asp:TextBox>
                                    <b>Billable:</b>
                                        <asp:CheckBox ID="vbillCheckBox" runat="server" Checked='<%# Bind("vbill") %>' />
                                    </td>
                    
                                    <td align="right" style="padding-right:5px"><b>Zone:</b></td>
                                    <td nowrap>
                                        <asp:TextBox ID="vdestcodeTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vdestcode") %>'></asp:TextBox>
                                        <a href="#" tabindex="1" onClick ="deliveryzonelook(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                    </td>
                                </tr>
                                <tr>
                                    <td align="right" style="padding-right:5px"><b>N</b></td>
                                    <td colspan="3" nowrap>
                                        <asp:TextBox ID="vnotes1TextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Height="20px" MaxLength="60"  Width="380px" runat="server" Text='<%# Bind("vnotes1") %>'></asp:TextBox>
                                    </td>                    
                                    <td align="right" style="padding-right:5px"><b>Corrugated Pallet:</b></td>
                                    <td nowrap>
                                        <asp:TextBox ID="vpalletTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vpallet") %>'></asp:TextBox>
                                        <a href="#" tabindex="1" onClick="palletlook(); return false"><asp:Image ID="palletImage" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                    </td>
                                </tr>
                                <tr>
                                    <td align="right" style="padding-right:5px"><b>O</b></td>
                                    <td colspan="3" nowrap>
                                        <asp:TextBox ID="vnotes2TextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Height="20px" MaxLength="60"   Width="380px" runat="server" Text='<%# Bind("vnotes2") %>'></asp:TextBox>
                                    </td>                    
                                    <td align="right" style="padding-right:5px"><b>Ship Meth:</b></td>
                                    <td> 
                                        <%--<asp:CheckBox ID="vshipmethCheckBox" runat="server" Checked='<%# Bind("vshipmeth") %>' />--%>
                                        <asp:RadioButtonList ID="RadioButtonList1" RepeatLayout="Flow" CellSpacing="1" RepeatColumns="2" Font-Bold ="true"  SelectedValue='<%# Bind("vshipmeth") %>'  runat="server">
                                            <asp:ListItem Text="Case" Value="True"></asp:ListItem>
                                            <asp:ListItem Text="Pallet" Value="False"></asp:ListItem>
                                        </asp:RadioButtonList>                       
                                    </td>
                                </tr>
                                <tr>
                                    <td align="right" style="padding-right:5px"><b>T</b></td>
                                    <td colspan="3" nowrap>
                                        <asp:TextBox ID="vnotes3TextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Height="20px" MaxLength="60"   Width="380px" runat="server" Text='<%# Bind("vnotes3") %>'></asp:TextBox>
                                    </td>
                                    <td align="right" style="padding-right:5px"><b>Charge:</b></td>
                                    <td>
                                        <asp:TextBox ID="vdelchgTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vdelchg") %>'></asp:TextBox>
                                        <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="vdelchgTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator>
                                    </td>
                                </tr>
                                <tr>
                                    <td align="right" style="padding-right:5px"><b>E</b></td>
                                    <td colspan="3" nowrap>
                                        <asp:TextBox Height="20px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)"  MaxLength="60"  ID="vnotes4TextBox" Width="380px" runat="server" Text='<%# Bind("vnotes4") %>'></asp:TextBox>
                                    </td>                 
                                    <td align="right" style="padding-right:5px"><b>Time:</b></td>
                                    <td nowrap>
                                        <asp:TextBox ID="vdeltimeTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:timeblurval(this)" runat="server" Text='<%# Bind("vdeltime") %>'></asp:TextBox>
                                        <asp:CompareValidator ID="CompareValidator2" runat="server" ControlToValidate="vdeltimeTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator>
                                    </td>
                                </tr>
                                <tr>
                                    <td colspan="2">
                                        <asp:Button ID="UpdateButton" runat="server" CssClass="button" CausesValidation="True" onclick="UpdateButton_Click" Text="Save"> </asp:Button>
                                        <asp:Button ID="UpdateCancelButton" CssClass="button" runat="server" CausesValidation="False" CommandName="Cancel" Text="Cancel"></asp:Button>
                                    </td>
                                </tr>
                            </table>
                            </fieldset>
                          </asp:Panel>
                    </EditItemTemplate>
                    
              <InsertItemTemplate>
                <asp:Panel ID="update_panel" runat="server" DefaultButton="InsertButton">
                    <fieldset>
                    <table class="shade">
                        <tr>
                            <td align="right" style="padding-right:5px"><b>Ship To Id:</b></td>
                            <td nowrap>
                                <asp:TextBox ID="vshipidTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Width="150" Text='<%# Bind("vshipid") %>'></asp:TextBox>
                                <a href="#" tabindex="1" onClick="shiptolook(); return false"><asp:Image ID="Image8" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </td>
                            <asp:RequiredFieldValidator ID="RequiredFieldValidator1" ControlToValidate="vshipidTextBox" Display="dynamic" SetFocusOnError="true" runat="server" ErrorMessage="This field cannot be blank"></asp:RequiredFieldValidator>
                            <td align="right" style="padding-right:5px"><b>Contact:</b></td>
                            <td nowrap>
                                <asp:TextBox ID="vcontactTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="130" runat="server" Text='<%# Bind("vcontact") %>'></asp:TextBox>
                            </td>
                            <td align="right" style="padding-right:5px"><b>Dock#:</b></td>
                            <td nowrap>
                                <asp:TextBox ID="vdocklocTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vdockloc") %>'></asp:TextBox>
                            </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px"><b>Name:</b></td>
                            <td>
                                <asp:TextBox ID="vshipnameTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="150" runat="server" Text='<%# Bind("vshipname") %>'></asp:TextBox>
                            </td>
                            <td align="right" style="padding-right:5px"><b>Phone:</b></td>
                            <td nowrap> 
                                <asp:TextBox ID="vareacodeTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="38px" MaxLength="3" runat="server" Text='<%# Bind("vareacode") %>'></asp:TextBox>
                                
                                <asp:TextBox ID="vphoneTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="90px" MaxLength="10" runat="server" Text='<%# Bind("vphone") %>'></asp:TextBox>
                                
                            </td>
                            <td align="right" style="padding-right:5px"><b>Dock Hours:</b></td>
                            <td>
                                <asp:TextBox ID="vdockhourTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vdockhour") %>'></asp:TextBox>
                            </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px"><b>Address:</b></td>
                            <td>
                                <asp:TextBox ID="vshipaddr1TextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="150" runat="server" Text='<%# Bind("vshipaddr1") %>'></asp:TextBox>
                            </td>
                            <td align="right" style="padding-right:5px"><b>Fax:</b></td>
                            <td nowrap>
                                <asp:TextBox ID="vfaxAreaCodeTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="38px" MaxLength="3" runat="server" Text='<%# Bind("vfaxAreaCode") %>'></asp:TextBox>
                                
                                <asp:TextBox ID="vfaxNumberTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="90px" MaxLength="10" runat="server" Text='<%# Bind("vfaxNumber") %>'></asp:TextBox>
                                
                            </td>
                            <td align="right" style="padding-right:5px"><b>Warehouse:</b></td>
                            <td nowrap>
                                <asp:TextBox ID="vlocTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vloc") %>'></asp:TextBox>
                                <a href="#" tabindex="1" onClick ="locationlook(); return false"><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </td>
                        </tr>
                        <tr>
                            <td></td>
                            <td nowrap>
                                <asp:TextBox ID="vshipaddr2TextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="150" runat="server" Text='<%# Bind("vshipaddr2") %>'></asp:TextBox>
                            </td>
                            <td align="right" style="padding-right:5px"><b>JD Ediw:</b></td>
                            <td>
                                <asp:TextBox ID="vfi_jdedidTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="130" runat="server" Text='<%# Bind("vfi_jdedid") %>'></asp:TextBox>
                            </td>
                            <td align="right" style="padding-right:5px"><b>Bin:</b></td>
                            <td nowrap>
                                <asp:TextBox ID="vlocbinTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vlocbin") %>'></asp:TextBox>
                                <a href="#" tabindex="1" onClick ="binlook(); return false"><asp:Image ID="binlookImage" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px"><b>City:</b></td>
                            <td nowrap> 
                                <asp:TextBox ID="vshipcityTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="80px" runat="server" Text='<%# Bind("vshipcity") %>'></asp:TextBox>
                                <a href="#" tabindex="1" onClick="citylook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>  
                                <asp:TextBox ID="vshipstateTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="30px" runat="server" Text='<%# Bind("vshipstate") %>'></asp:TextBox>
                                <a href="#" tabindex="1" onClick="statecodelook(); return false"><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>   
                                <asp:TextBox ID="vshipzipTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Width="40px"  runat="server" Text='<%# Bind("vshipzip") %>'></asp:TextBox>
                                <a href="#" tabindex="1" onClick="zipcodelook(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </td>
                            <td align="right" style="padding-right:5px"><b>Broker:</b></td>
                            <td>
                                <asp:CheckBox ID="vbrokerCheckBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Checked='<%# Bind("vbroker") %>' />
                            </td>
                            <td align="right" style="padding-right:5px"><b>Carrier:</b></td>
                            <td nowrap>
                                <asp:TextBox ID="vcarrierTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vcarrier") %>'></asp:TextBox>
                                <a href="#" tabindex="1" onClick ="carrierlook(); return false"><asp:Image ID="Image6" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px"><b>Tax Code:</b></td>
                            <td colspan="3" nowrap>
                                <asp:TextBox ID="vtaxcodeTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vtaxcode") %>'></asp:TextBox>
                                <a href="#" tabindex="1" onClick ="taxcodelook(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            <b>Mandatory Tax:</b>
                                <asp:TextBox ID="vtb_mandatorytaxTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" ToolTip="Enter the decimal/integer value" Width="50px" runat="server" Text='<%# Bind("vtb_mandatorytax") %>'></asp:TextBox>
                            <b>Billable:</b>
                                <asp:CheckBox ID="vbillCheckBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Checked='<%# Bind("vbill") %>' />
                            </td>                   
                            <td align="right" style="padding-right:5px"><b>Zone:</b></td>
                            <td nowrap>
                                <asp:TextBox ID="vdestcodeTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vdestcode") %>'></asp:TextBox>
                                <a href="#" onClick ="deliveryzonelook(); return false"><asp:Image ID="Image7" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                            </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px"><b>N</b></td>
                            <td colspan="3" nowrap>
                                <asp:TextBox ID="vnotes1TextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Height="20px" MaxLength="60"   Width="380px" runat="server" Text='<%# Bind("vnotes1") %>'></asp:TextBox>
                            </td>
                            <td align="right" style="padding-right:5px"><b>Corrugated Pallet:</b></td>
                            <td nowrap>
                                <asp:TextBox ID="vpalletTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vpallet") %>'></asp:TextBox>
                                <a href="#" onClick="palletlook(); return false"><asp:Image ID="palletImage" runat="server" ImageUrl="images/lookup_icon.gif" /></a> 
                            </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px"><b>O</b></td>
                            <td colspan="3">
                                <asp:TextBox ID="vnotes2TextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Height="20px" MaxLength="60"  Width="380px" runat="server" Text='<%# Bind("vnotes2") %>'></asp:TextBox>
                            </td>                    
                            <td align="right" style="padding-right:5px"><b>Ship Meth:</b></td> 
                            <td> 
                                <asp:RadioButtonList ID="RadioButtonList1" RepeatLayout="Flow" CellSpacing="1" RepeatColumns="2" SelectedValue='<%# Bind("vshipmeth") %>'  runat="server">
                                    <asp:ListItem Text="Case" Value="yes"></asp:ListItem>
                                    <asp:ListItem Text="Pallet" Value="no"></asp:ListItem>
                                </asp:RadioButtonList>
                            </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px"><b>T</b></td>
                            <td colspan="3">
                                <asp:TextBox ID="vnotes3TextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" Height="20px" MaxLength="60"  Width="380px" runat="server" Text='<%# Bind("vnotes3") %>'></asp:TextBox>
                            </td>
                            <td align="right" style="padding-right:5px"><b>Charge:</b></td>
                            <td>
                                <asp:TextBox ID="vdelchgTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" runat="server" Text='<%# Bind("vdelchg") %>'></asp:TextBox>
                                <asp:CompareValidator ID="CompareValidator1" runat="server" ControlToValidate="vdelchgTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator>
                            </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px"><b>E</b></td>
                            <td colspan="3">
                                <asp:TextBox Height="20px" onfocus= "javascript:focusval(this)" onblur="javascript:blurval(this)" MaxLength="60"   ID="vnotes4TextBox" Width="380px" runat="server" Text='<%# Bind("vnotes4") %>'></asp:TextBox>
                            </td>
                  
                            <td align="right" style="padding-right:5px"><b>Time:</b></td>
                            <td>
                                <asp:TextBox ID="vdeltimeTextBox" onfocus= "javascript:focusval(this)" onblur="javascript:timeblurval(this)" runat="server" Text='<%# Bind("vdeltime") %>'></asp:TextBox>
                                <asp:CompareValidator ID="CompareValidator18" runat="server" ControlToValidate="vdeltimeTextBox" SetFocusOnError="true" Display="dynamic" Operator="dataTypeCheck" Type="double" ErrorMessage="Invalid Number"></asp:CompareValidator>
                            </td>
                        </tr>
                        <tr>
                            <td colspan="2">
                                <asp:Button ID="InsertButton" runat="server" CssClass="button" CausesValidation="True" OnClick="addButton_Click" Text="Save"></asp:Button>
                                <asp:Button ID="InsertCancelButton" runat="server" CssClass="button" CausesValidation="False" CommandName="Cancel" Text="Cancel"></asp:Button>
                            </td>
                        </tr>
                    </table>
                    </fieldset>
                  </asp:Panel>
             </InsertItemTemplate>
             
             <ItemTemplate>
                <fieldset>
                  <table class="shade" >
                        <tr>
                            <td align="right" style="padding-right:5px"><b>Ship To Id:</b></td>
                            <td>
                                <asp:Label ID="vshipidLabel" Width="200px"  BackColor="turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vshipid") %>'></asp:Label>
                            </td>
                            <td align="right" style="padding-right:5px"><b>Contact:</b></td>
                            <td>
                                <asp:Label ID="vcontactLabel" Width="180px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vcontact") %>'></asp:Label>
                            </td>
                            <td align="right" style="padding-right:5px"><b>Dock#:</b></td>
                            <td>
                                <asp:Label ID="vdocklocLabel" Width="120px"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  runat="server" Text='<%# Bind("vdockloc") %>'></asp:Label>
                            </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px"><b>Name:</b></td>
                            <td>
                                <asp:Label ID="vshipnameLabel"  Width="200px"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vshipname") %>'></asp:Label>
                            </td>
                            <td align="right" style="padding-right:5px"><b>Phone:</b></td>
                            <td>
                                <asp:Label ID="vareacodeLabel"  Width="48px"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vareacode") %>'></asp:Label>
                                <asp:Label ID="vphoneLabel" runat="server" Width="130px"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  Text='<%# Bind("vphone") %>'></asp:Label>
                            </td>
                            <td align="right" style="padding-right:5px"><b>Dock Hours:</b></td>
                            <td>
                                <asp:Label ID="vdockhourLabel"  Width="120px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  runat="server" Text='<%# Bind("vdockhour") %>'></asp:Label>
                            </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px"><b>Address:</b></td>
                            <td>
                                <asp:Label ID="vshipaddr1Label"  Width="200px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  runat="server" Text='<%# Bind("vshipaddr1") %>'></asp:Label>
                            </td>
                            <td align="right" style="padding-right:5px"><b>Fax:</b></td>
                            <td>
                                <asp:Label ID="vfaxAreaCodeLabel" runat="server"  Width="48px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  Text='<%# Bind("vfaxAreaCode") %>'></asp:Label>
                                <asp:Label ID="vfaxNumberLabel" BackColor="Turquoise" Width="130px"  BorderColor="white" BorderStyle="solid" BorderWidth="1px"  runat="server" Text='<%# Bind("vfaxNumber") %>'></asp:Label>
                            </td>
                            <td align="right" style="padding-right:5px"><b>Warehouse:</b></td>
                            <td>
                                <asp:Label ID="vlocLabel"  Width="120px" runat="server" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  Text='<%# Bind("vloc") %>'></asp:Label>
                            </td>
                        </tr>
                        <tr>
                            <td></td>
                            <td>
                                <asp:Label ID="vshipaddr2Label"  Width="200px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  runat="server" Text='<%# Bind("vshipaddr2") %>'></asp:Label>
                            </td>
                            <td align="right" style="padding-right:5px"><b>JD Ediw:</b></td>
                            <td>
                                <asp:Label ID="vfi_jdedidLabel"  Width="180px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  runat="server" Text='<%# Bind("vfi_jdedid") %>'></asp:Label>
                            </td>
                            <td align="right" style="padding-right:5px"><b>Bin:</b></td>
                            <td>
                                <asp:Label ID="vlocbinLabel"  Width="120px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  runat="server" Text='<%# Bind("vlocbin") %>'></asp:Label>
                            </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px"><b>City:</b></td>
                            <td>
                                <asp:Label ID="vshipcityLabel" BackColor="Turquoise"  Width="100px" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  runat="server" Text='<%# Bind("vshipcity") %>'></asp:Label>
                                <asp:Label ID="vshipstateLabel" BackColor="Turquoise" Width="40px"  BorderColor="white" BorderStyle="solid" BorderWidth="1px"  runat="server" Text='<%# Bind("vshipstate") %>'></asp:Label>
                                <asp:Label ID="vshipzipLabel" BackColor="Turquoise" Width="50px"  BorderColor="white" BorderStyle="solid" BorderWidth="1px"  runat="server" Text='<%# Bind("vshipzip") %>'></asp:Label>
                            </td>
                            <td align="right" style="padding-right:5px"><b>Broker:</b></td>
                            <td>
                                <asp:CheckBox ID="vbrokerCheckBox" BackColor="Turquoise"   BorderColor="white" BorderStyle="solid" BorderWidth="1px"  runat="server" Checked='<%# Bind("vbroker") %>' Enabled="false" />
                            </td>
                            <td align="right" style="padding-right:5px"><b>Carrier:</b></td>
                            <td>
                                <asp:Label ID="vcarrierLabel" Width="120px"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  runat="server" Text='<%# Bind("vcarrier") %>'></asp:Label>
                            </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px"><b>Tax Code:</b></td>
                            <td colspan="3">
                                <asp:Label ID="vtaxcodeLabel" Width="150px"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  runat="server" Text='<%# Bind("vtaxcode") %>'></asp:Label>
                                <b>Mandatory Tax:</b>
                                    <asp:Label ID="vtb_mandatorytaxLabel"    Width="50px"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Text='<%# Bind("vtb_mandatorytax") %>'> </asp:Label>
                                <b>Billable:</b>
                                    <asp:CheckBox ID="vbillCheckBox" BackColor="Turquoise"   BorderColor="white" BorderStyle="solid" BorderWidth="1px" runat="server" Checked='<%# Bind("vbill") %>' Enabled="false" />
                            </td>
                            <td align="right" style="padding-right:5px"><b>Zone:</b></td>
                            <td>
                                <asp:Label ID="vdestcodeLabel" Width="120px"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  runat="server" Text='<%# Bind("vdestcode") %>'></asp:Label>
                            </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px"><b>N</b></td>
                            <td colspan="3">
                                <asp:Label ID="vnotes1Label" Width="380px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  runat="server" Text='<%# Bind("vnotes1") %>'></asp:Label>
                            </td>                    
                            <td align="right" style="padding-right:5px"><b>Corrugated Pallet:</b></td>
                            <td>
                                <asp:Label ID="vpalletLabel"  Width="120px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  runat="server" Text='<%# Bind("vpallet") %>'></asp:Label>
                            </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px"><b>O</b></td>
                            <td colspan="3">
                                <asp:Label ID="vnotes2Label"  Width="380px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  runat="server" Text='<%# Bind("vnotes2") %>'></asp:Label>
                            </td>
                    
                            <td align="right" style="padding-right:5px"><b>Ship Meth:</b></td>
                            <td> 
                                <asp:RadioButtonList ID="RadioButtonList1" RepeatLayout="Flow" CellSpacing="1" RepeatColumns="2" SelectedValue='<%# Bind("vshipmeth") %>' Enabled="false" runat="server">
                                    <asp:ListItem Text="Case" Value="True"></asp:ListItem>
                                    <asp:ListItem Text="Pallet" Value="False"></asp:ListItem>
                                </asp:RadioButtonList>
                            </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px"><b>T</b></td>
                            <td colspan="3">
                                <asp:Label ID="vnotes3Label"  Width="380px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  runat="server" Text='<%# Bind("vnotes3") %>'></asp:Label>
                            </td>
                            <td align="right" style="padding-right:5px"><b>Charge:</b></td>
                            <td>
                                <asp:Label ID="vdelchgLabel" Width="100px"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  runat="server" Text='<%# Bind("vdelchg") %>'></asp:Label>
                            </td>
                        </tr>
                        <tr>
                            <td align="right" style="padding-right:5px"><b>E</b></td>
                            <td colspan="3">
                                <asp:Label ID="vnotes4Label" Width="380px"  BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  runat="server" Text='<%# Bind("vnotes4") %>'></asp:Label>
                            </td>
                            <td align="right" style="padding-right:5px"><b>Time:</b></td>
                            <td>
                                <asp:Label ID="vdeltimeLabel"  Width="100px" BackColor="Turquoise" BorderColor="white" BorderStyle="solid" BorderWidth="1px"  runat="server" Text='<%# Bind("vdeltime") %>'></asp:Label>
                            </td>
                        </tr>
                        <tr>
                            <td nowrap colspan="2">
                                <asp:Button ID="AddButton" runat="server" CssClass="button" CommandName="new"  CausesValidation="false" Text="Add" ></asp:Button>
                                <asp:Button ID="UpdateButton" runat="server" CssClass="button" CommandName="edit"  CausesValidation="false" Text="Update" ></asp:Button>
                                <asp:Button ID="DeleteButton" runat="server" CssClass="button"  OnClick="Deletebutton_Click" CausesValidation="false" Text="Delete" OnClientClick="return confirm('Are you sure to want to Delete?')" ></asp:Button>
                            </td>
                        </tr>                   
                 </table>
                 </fieldset>
          </ItemTemplate>
    </asp:FormView>  
    
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}" SelectMethod="Selectship" TypeName="contact">
            <SelectParameters>
                  <asp:Parameter Name="prmAction" DefaultValue="View" Type="String" />
                  <asp:Parameter Name="prmComp" Type="String" />
                  <asp:Parameter Name="prmUser" Type="String" />
                  <asp:SessionParameter Name="prmCustomer" SessionField="customer1_list_cust" Type="string" />
                  <asp:SessionParameter Name="prmshipid"  SessionField="customer_list_shipto" Type="String"  />
                  <asp:Parameter Name="prmreckey" Type="string"/>
                  <asp:Parameter Name="prmshipno" Type="int32"  />
                  <asp:Parameter Name="prmshipname" Type="String" />
                  <asp:Parameter Name="prmshipcity" Type="String" />
                  <asp:Parameter Name="prmshipstate" Type="String" />
                  <asp:Parameter Name="prmshipzip" Type="String" />
                  <asp:Parameter Name="prmshipaddr1" Type="String" />
                  <asp:Parameter Name="prmshipaddr2" Type="String" />
                  <asp:Parameter Name="prmcontact" Type="String" />
                  <asp:Parameter Name="prmareacode" Type="String" />
                  <asp:Parameter Name="prmphone" Type="String" />
                  <asp:Parameter Name="prmtaxcode" Type="String" />
                  <asp:Parameter Name="prmbroker" Type="String" />
                  <asp:Parameter Name="prmbill" Type="String" />
                  <asp:Parameter Name="prmdockloc" Type="String" />
                  <asp:Parameter Name="prmdockhour" Type="String" />
                  <asp:Parameter Name="prmlocbin" Type="String" />
                  <asp:Parameter Name="prmcarrier" Type="String" />
                  <asp:Parameter Name="prmpallet" Type="String" />
                  <asp:Parameter Name="prmshipmeth" Type="String" />
                  <asp:Parameter Name="prmdelchg" Type="Decimal" />
                  <asp:Parameter Name="prmdeltime" Type="Decimal" />
                  <asp:Parameter Name="prmdestcode" Type="String" />
                  <asp:Parameter Name="prmnotes1" Type="String" />
                  <asp:Parameter Name="prmnotes2" Type="String" />
                  <asp:Parameter Name="prmnotes3" Type="String" />
                  <asp:Parameter Name="prmnotes4" Type="String" />
                  <asp:Parameter Name="prmfaxAreaCode" Type="String" />
                  <asp:Parameter Name="prmfaxNumber" Type="String" />
                  <asp:Parameter Name="prmfi_jdedid" Type="String" />
                  <asp:Parameter Name="prmtb_mandatorytax" Type="Decimal" />
                  <asp:Parameter Name="prmloc" Type="String" />
           </SelectParameters>
      </asp:ObjectDataSource>
      
         <asp:Button ID="newaddButton" runat="server" CssClass="button"  OnClick="newaddButton_Click" Text="Add" />
      </div>
   <ft:footer id="Footer1" runat="server"></ft:footer>
 </form>
</body>
</HTML>

