<%@ Page Language="C#" MasterPageFile="~/MasterPage3.master" AutoEventWireup="true" Debug="true" Inherits="fg_item1" Title="FgItem" Codebehind="fgitem.aspx.cs" %>
<asp:Content ID="Content1" ContentPlaceHolderID="ContentPlaceHolder1" Runat="Server">

<script language="javascript">
    window.onload = setfocus;
    function setfocus() {
        document.forms[0].ctl00$ContentPlaceHolder1$FormView1$CustTextBox.focus();
    }
function contactcustomerlook()
{ 
    var NewWindow = window.open("contact_customer_lookup.aspx","ContactCustomerLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function ContactCustomerLookup(ReturnObj1,ReturnObj2,ReturnObj3,ReturnObj4,ReturnObj5,ReturnObj6,ReturnObj7,ReturnObj8,ReturnObj9, ReturnObj10,ReturnObj11)
{ 
    document.forms[0].ctl00$ContentPlaceHolder1$FormView1$CustTextBox.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$FormView1$CustNameTextBox.value = ReturnObj2;
    document.forms[0].ctl00$ContentPlaceHolder1$FormView1$CustTextBox.focus();
}

function locationlook()
{ 
    var NewWindow = window.open("location_lookup.aspx","LocationLookUpWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function LocationLookUp(ReturnObj1, ReturnObj2)
{
    document.forms[0].ctl00$ContentPlaceHolder1$FormView1$WareHouseTextBox.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$FormView1$WareHouseTextBox.focus();    
}

function binlook()
{     
    var loc1 = document.forms[0].ctl00$ContentPlaceHolder1$FormView1$WareHouseTextBox.value;
    var NewWindow = window.open("custbin_lookup.aspx?binloc="+loc1+"","BinLookUpWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function CustBinLookup(ReturnObj1, ReturnObj2)
{
    document.forms[0].ctl00$ContentPlaceHolder1$FormView1$BinTextBox.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$FormView1$BinTextBox.focus();  
}

function dielook()
{
    var NewWindow = window.open("die_lookup.aspx","DieLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function DieLookup(ReturnObj1)
{
    document.forms[0].ctl00$ContentPlaceHolder1$FormView1$dieTextBox.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$FormView1$dieTextBox.focus();
}
function Platelook()
{
    var NewWindow = window.open("Plate_lookup.aspx","DieLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function plateLookup(ReturnObj1)
{
    document.forms[0].ctl00$ContentPlaceHolder1$FormView1$PlateTextBox.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$FormView1$PlateTextBox.focus();
}



function stylelook()
{
    var NewWindow = window.open("Style_Look.aspx","StyleLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function Style_LookUp(ReturnObj1, ReturnObj2)
{
    document.forms[0].ctl00$ContentPlaceHolder1$FormView1$StyleTextBox.value = ReturnObj1;
    document.getElementById("ctl00_ContentPlaceHolder1_FormView1_styledescLabel").innerText = ReturnObj2;
    document.forms[0].ctl00$ContentPlaceHolder1$FormView1$StyleTextBox.focus();
}


function categorylookup()
{ 
    var NewWindow =window.open("CategoryLookup.aspx","CategoryWindow","width=600,height=420,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}

function categoryLookUp(ReturnObj1, ReturnObj2)
{ 
    document.forms[0].ctl00$ContentPlaceHolder1$FormView1$CategTextBox.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$FormView1$RptTextBox.value = ReturnObj2;
    document.forms[0].ctl00$ContentPlaceHolder1$FormView1$RptTextBox.focus();
}

function estimatelook()
{     
    var cust = document.getElementById("ctl00_ContentPlaceHolder1_FormView1_CustTextBox").value; 
    var NewWindow = window.open("estimate_lookup.aspx?customer_val="+cust +"","EstimateLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function EstimateLookup(ReturnObj1)
{
    document.forms[0].ctl00$ContentPlaceHolder1$FormView1$EstimateTextBox.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$FormView1$EstimateTextBox.focus();
}

function uomlook()
{     
    var NewWindow = window.open("Uom_lookup.aspx","UomLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function UomLookup(ReturnObj1)
{
    document.forms[0].ctl00$ContentPlaceHolder1$FormView1$UOMTextBox.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$FormView1$UOMTextBox.focus();
}

function freightlook()
{     
    var NewWindow = window.open("Freight_lookup.aspx","FreightLookupWindow","width=500,height=500,scrollbars=1,toolbars=1,statusbar=1,resizeable=1");
}
function FreightLookup(ReturnObj1, ReturnObj2)
{ 
    document.forms[0].ctl00$ContentPlaceHolder1$FormView1$FreightTextBox.value = ReturnObj1;
    document.forms[0].ctl00$ContentPlaceHolder1$FormView1$FreClassTextBox.value = ReturnObj2;
    document.forms[0].ctl00$ContentPlaceHolder1$FormView1$FreightTextBox.focus();
}


function calTotCost()
{       
    var tot_cost = 0.00;
    if(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$StdMatTextBox.value != "")
    {
        tot_cost = tot_cost + parseFloat(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$StdMatTextBox.value);       
    }    
    if(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$StdLabTextBox.value != "")
    {
        tot_cost = tot_cost + parseFloat(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$StdLabTextBox.value);        
    }
    if(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$StdVarTextBox.value != "")
    {
        tot_cost = tot_cost + parseFloat(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$StdVarTextBox.value);       
    }    
    if(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$StdFixTextBox.value != "")
    {
        tot_cost = tot_cost + parseFloat(document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$StdFixTextBox.value);               
    }       
           
    var totcost = document.aspnetForm.ctl00$ContentPlaceHolder1$FormView1$StdTotTextBox;
    totcost.value = tot_cost;        
   
}
</script>

<div>
    <br />
    <asp:LinkButton OnClick="LinkButton_Click" ID="LinkButton1" runat="server">List Order</asp:LinkButton>
    <asp:LinkButton OnClick="LinkButton2_Click" ID="LinkButton2" runat="server">List Order</asp:LinkButton>
    <asp:LinkButton OnClick="LinkButton3_Click" ID="LinkButton3" runat="server">List Order</asp:LinkButton>
       
    <asp:HiddenField ID="HiddenField1" runat="server" />
    <asp:HiddenField ID="HiddenField2" runat="server" />
    <asp:HiddenField ID="HiddenField3" runat="server" />
    <asp:HiddenField ID="HiddenField4" runat="server" />
    <asp:HiddenField ID="HiddenField5" runat="server" />
     <asp:HiddenField ID="HiddenField6" runat="server" />
    <asp:FormView ID="FormView1" runat="server" DataSourceID="ObjectDataSource1" OnPreRender="formview_prerander" OnDataBound="FormView1_DataBound"
        Width="967px" CellPadding="4" ForeColor="#333333" OnItemUpdated="FormView1_ItemUpdated" AllowPaging="True">
        <EditItemTemplate>              
        <table>                                     
            <tr>
                <td align="right" style="width: 63px; height: 28px;"><b>FG Item#:</b></td>
                <td style="width: 142px; height: 28px;">
                    <b><asp:TextBox ID="Item1TextBox"  runat="server" Text='<%# Bind("Item1") %>'></asp:TextBox></b>
                </td>
        
                <td align="right" style="width: 112px; height: 28px;">
                    <b><asp:CheckBox ID="IsSetCheckBox" runat="server" Checked='<%# Bind("IsSet") %>' />SetHeader?</b>
                </td>        
                <td colspan="2" align="left" >
        <asp:RadioButtonList ID="purchaseRadioButtonList"  RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="4"  
            SelectedValue ='<%# Bind("Purchase") %>' Font-Bold="true"     runat="server">
             <asp:ListItem value="True"  Text="Purchased"   />
             <asp:ListItem value="False" Text="Manufactured" />                                                         
         </asp:RadioButtonList>
         </td>
        
      
        <td style="width: 191px; height: 28px;" align="left">
           <asp:RadioButtonList ID="stockRadioButtonList"  RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="4"  
            SelectedValue ='<%# Bind("stock") %>' Font-Bold="true"    runat="server">
             <asp:ListItem value="S"  Text="Stock Item"   />
             <asp:ListItem value="C" Text="Custom Box" />                                                         
         </asp:RadioButtonList>
             
        </td>
                <td style="width: 199px; height: 28px;" align="left">
                    <b><asp:CheckBox ID="TaxCheckBox" runat="server" Checked='<%# Bind("Tax") %>' />Taxable</b>
                </td>
                               
                
            </tr>
                                      
            <tr>
                <td align="right" style="width: 63px">
                    <b>Cust Part#:</b>
                </td>
                <td colspan="4">                    
                    <b> <asp:TextBox ID="PartTextBox" runat="server" Text='<%# Bind("Part") %>'></asp:TextBox></b> 
                        <asp:RequiredFieldValidator ID="RequiredFieldValidator1" ControlToValidate="PartTextBox" Display="Dynamic" runat="server" ErrorMessage="Cust Part is mandatory"></asp:RequiredFieldValidator>
                    <b>Cust#:</b>
                    <b><asp:TextBox ID="CustTextBox" runat="server" Text='<%# Bind("Cust") %>'></asp:TextBox></b> 
                    <a href="#" tabindex="1" onClick="contactcustomerlook(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>            
                        <asp:RequiredFieldValidator ID="RequiredFieldValidator2" ControlToValidate="CustTextBox" Display="Dynamic" runat="server" ErrorMessage="Customer no is mandatory"></asp:RequiredFieldValidator>
                    <b><asp:TextBox ID="CustNameTextBox" runat="server" Text='<%# Bind("CustName") %>'></asp:TextBox></b>
                        <asp:RequiredFieldValidator ID="RequiredFieldValidator3" ControlToValidate="CustNameTextBox" Display="Dynamic" runat="server" ErrorMessage="Customer Name is mandatory"></asp:RequiredFieldValidator>
                </td>       
            </tr>
                          
            <tr>
                <td align="right" style="width: 65px; height: 21px;">
                    <b>Item Name:</b>
                </td>
                <td style="width: 142px; height: 21px;">
                    <b><asp:TextBox ID="Name1TextBox" runat="server" Text='<%# Bind("Name1") %>'> </asp:TextBox></b>
                    <asp:RequiredFieldValidator ID="RequiredFieldValidator4" ControlToValidate="Name1TextBox" Display="Dynamic" runat="server" ErrorMessage="Item Name is mandatory"></asp:RequiredFieldValidator>
                </td>
                <td align="right" style="width: 112px; height: 21px;">
                    <b>Shipping Method</b>
                </td>
                <td style="height: 21px;" colspan="4"><b>
          <asp:RadioButtonList ID="ShipRadioButtonList"  RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="4"  
            SelectedValue ='<%# Bind("casepal") %>' Font-Bold="true"     runat="server">
             <asp:ListItem value="True"  Text="Case"   />
             <asp:ListItem value="False" Text="Pallet" />                                                         
         </asp:RadioButtonList>  </b>&nbsp;&nbsp;&nbsp;
         <b> Status:  <asp:RadioButtonList ID="statusRadioButtonList"  RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="4"  
            SelectedValue ='<%# Bind("Stat") %>'  DataTextField='<%# Bind("Stat") %>' Font-Bold="true"    runat="server">
             <asp:ListItem value="A"  Text="Active"   />
             <asp:ListItem value="I" Text="Inactive" />                                                         
         </asp:RadioButtonList>
               </b>
        </td>       
            </tr>
              
            <tr><td colspan="7">
                <table><tr><td>
                    <table><tr><td colspan="2" valign="top">
                        <table>                                                         
                            <tr>
                                <td align="right" style="width: 56px"><b>Desc 1:</b></td>
                                <td style="width: 142px"><b><asp:TextBox ID="DscrTextBox" runat="server" Text='<%# Bind("Dscr") %>'></asp:TextBox></b></td>
                            </tr>
                            
                            <tr>
                                <td align="right" style="width: 56px"><b>Desc 2:</b></td>
                                <td style="width: 142px"><b><asp:TextBox ID="Dscr2TextBox" runat="server" Text='<%# Bind("Dscr2") %>'> </asp:TextBox></b></td>
                            </tr>
                            <tr>
                                <td align="right" style="width: 56px"><b> Dscr3:</b></td>
                                <td style="width: 142px"><b><asp:TextBox ID="Dscr3TextBox" runat="server" Text='<%# Bind("Dscr3") %>'></asp:TextBox></b></td>                  
                            </tr>
                        </table>
                        
                        <fieldset style="height:250px">
                        <table>
                            <tr>
                                <td align="right" style="width: 53px"><b>Estimate:</b></td>
                                <td nowrap><b><asp:TextBox ID="EstimateTextBox" runat="server" Text='<%# Bind("Estimate") %>'></asp:TextBox></b>
                                    <a href="#" tabindex="1" onClick="estimatelook(); return false"><asp:Image ID="EstimateLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                </td>
                            </tr>                              
                            <tr>
                                <td align="right" style="width: 53px"><b> Style:</b></td>
                                <td ><b><asp:TextBox ID="StyleTextBox" runat="server" Text='<%# Bind("Style") %>'></asp:TextBox></b>
                                    <a href="#" tabindex="1" onClick="stylelook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a> 
                                </td>
                            </tr>
                            <tr>
                                <td align="right" style="width: 53px"></td> 
                                <td style="width: 142px"><b><asp:Label ID="styledescLabel" Width="120px" runat="server" Text='<%# Bind("styledesc") %>'></asp:Label></b></td>
                            </tr>
                            <tr>
                                <td align="right" style="width: 53px; height: 16px;"><b>Die#:</b></td>
                                <td><b><asp:TextBox ID="dieTextBox" runat="server" Text='<%# Bind("die") %>'>  </asp:TextBox></b>
                                    <a href="#" tabindex="1" onclick="dielook(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                </td> 
                            </tr>
                            <tr>
                                <td align="right" style="width: 53px; height: 16px;"><b> Plate#: </b></td>
                                <td nowrap><b><asp:TextBox ID="PlateTextBox" runat="server" Text='<%# Bind("Plate") %>'></asp:TextBox></b>
                                    <a href="#" tabindex="1" onclick="Platelook(); return false"><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                </td>
                            </tr>
                            <tr>
                                <td align="right" style="width: 53px; height: 16px;"><b> CAD#: </b></td>
                                <td style="width: 142px; height: 16px;"><b><asp:TextBox ID="CadTextBox" runat="server" Text='<%# Bind("Cad") %>'></asp:TextBox></b></td>
                            </tr>
                            <tr>
                                <td align="right" style="width: 53px; height: 16px;"><b> SPC/QC#: </b></td>
                                <td style="width: 142px; height: 16px;"><b><asp:TextBox ID="SPCTextBox" runat="server" Text='<%# Bind("SPC") %>'></asp:TextBox></b></td>
                            </tr>
                            <tr>
                                <td align="right" style="width: 53px; height: 16px;"><b> UPC#: </b></td>
                                <td style="width: 142px; height: 16px;"><b><asp:TextBox ID="UPCTextBox" runat="server" Text='<%# Bind("UPC") %>'></asp:TextBox></b></td>
                            </tr>                                                    
                        </table>
                        </fieldset>
                    </td>
                    <td valign="top" >
                        <fieldset>
                            <table>
                                <tr>
                                    <td align="right" style="width: 112px"><b>Sell Price:</b></td>
                                    <td style="width: 96px"><b><asp:TextBox ID="SellTextBox" runat="server"  Width="80px" Text='<%# Bind("Sell") %>'></asp:TextBox></b></td>
                                    <td align="left" colspan="2"><b>UOM:<asp:TextBox ID="UOMTextBox" runat="server" Width="40px" Text='<%# Bind("UOM") %>'></asp:TextBox></b>
                                        <a href="#" tabindex="1" onClick="uomlook(); return false" ><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                    </td>
                                   <td>
                                        <asp:DropDownList ID="DropDownList1" Width="100px" SelectedValue='<%# Bind("typecode") %>' DataTextField='<%# Bind("typecode") %>' runat="server">
                                            <asp:ListItem Value="O">O- Original</asp:ListItem>
                                            <asp:ListItem Value="C">C- Change</asp:ListItem>
                                            <asp:ListItem Value="N">N- New</asp:ListItem>
                                            <asp:ListItem Value="Q">Q- Quality/Re-work</asp:ListItem>
                                            <asp:ListItem Value="R">R- Repeat</asp:ListItem>
                                            <asp:ListItem Value="T">T- Transfer</asp:ListItem>
                                            <asp:ListItem Value="X">X- Complete re-run</asp:ListItem>
                                            <asp:ListItem Value=""></asp:ListItem>
                                        </asp:DropDownList>
                                    </td>
                                </tr>
                                <tr>
                                    <td align="right" style="width: 112px"><b>FG Category:</b></td>
                                    <td><b><asp:TextBox ID="CategTextBox" runat="server" Width="80px" Text='<%# Bind("Categ") %>'> </asp:TextBox></b>
                                        <a href="#" tabindex="1" onClick="categorylookup(); return false" ><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                    </td>
                                    <td style="width: 133px; height: 18px;"><b><asp:TextBox ID="RptTextBox" runat="server" Text='<%# Bind("Rpt") %>'> </asp:TextBox></b></td>
                                    <td align="right" style="width: 117px; height: 18px;"><b> Currency: </b></td>
                                    <td style="width: 99px; height: 18px;"><b><asp:TextBox ID="CurrTextBox" runat="server" Width="80px" Text='<%# Bind("Curr") %>'> </asp:TextBox></b></td>
                                </tr>
                                <tr>
                                    <td align="right" style="width: 112px"><b> WareHouse:</b></td>
                                    <td><b><asp:TextBox ID="WareHouseTextBox" runat="server" Width="80px" Text='<%# Bind("WareHouse") %>'></asp:TextBox></b>
                                        <a href="#" tabindex="1" onClick ="locationlook(); return false"><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                    </td>
                                    <td style="width: 133px"><b> </b></td>
                                    <td align="right" style="width: 117px"><b>Inventory Class:</b></td>
                                    <td style="width: 99px"><b><asp:TextBox ID="InventoryTextBox" runat="server" Width="80px" Text='<%# Bind("Inventory") %>'></asp:TextBox></b></td>
                                </tr>
                                <tr>
                                    <td align="right" style="width: 112px"><b> Bin:</b></td>
                                    <td nowrap><b><asp:TextBox ID="BinTextBox" runat="server" Width="80px" Text='<%# Bind("Bin") %>'></asp:TextBox></b>
                                        <a href="#" tabindex="1" onClick ="binlook(); return false"><asp:Image ID="binlookImage" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                    </td>
                                    <td style="width: 133px"><b></b></td>
                                    <td align="right" style="width: 117px"><b>Cycle Count Code: </b></td>
                                    <td style="width: 99px"><b><asp:TextBox ID="CycleTextBox" runat="server" Width="80px" Text='<%# Bind("Cycle") %>'></asp:TextBox></b></td>
                                </tr>
                                <tr>
                                    <td align="right" style="width: 112px"><b>Count:</b></td>
                                    <td style="width: 96px"><b><asp:TextBox ID="CountTextBox" runat="server" Width="80px" Text='<%# Bind("Count") %>'></asp:TextBox></b></td>
                                    <td style="width: 133px"><b> </b></td>
                                    <td align="right" style="width: 117px"><b>Production Code:</b></td>
                                    <td style="width: 99px"><b><asp:TextBox ID="ProductionTextBox" runat="server" Width="80px" Text='<%# Bind("Production") %>'> </asp:TextBox></b>
                                        <asp:RequiredFieldValidator ID="RequiredFieldValidator5" ControlToValidate="ProductionTextBox" Display="Dynamic" runat="server" ErrorMessage="Production Code is mandatory"></asp:RequiredFieldValidator>
                                    </td>
                                    
                                </tr>
                                <tr>
                                    <td align="right" style="width: 112px"><b>Wt per 100:</b></td>
                                    <td style="width: 96px"><b><asp:TextBox ID="WeightTextBox" runat="server" Width="80px" Text='<%# Bind("Weight") %>'></asp:TextBox></b></td>
                                    <td style="width: 133px"><b> </b></td>
                                    <td align="right" style="width: 117px"><b>Packing Note:</b></td>
                                    <td style="width: 99px"><b><asp:TextBox ID="PackingTextBox" runat="server" Text='<%# Bind("Packing") %>'></asp:TextBox></b></td>
                                </tr>                                
                                <tr>
                                    <td align="right" style="width: 112px"><b>Freight Class:</b></td>
                                    <td nowrap><b><asp:TextBox ID="FreightTextBox" Width="80px" runat="server" Text='<%# Bind("Freight") %>'></asp:TextBox></b>
                                        <a href="#" tabindex="1" onClick ="freightlook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                    </td>
                                    <td style="width: 133px"><b><asp:TextBox ID="FreClassTextBox" runat="server" Text='<%# Bind("FreClass") %>'></asp:TextBox></b></td>
                                    <td style="width: 117px"><b> </b></td>
                                    <td style="width: 99px"><b> </b></td>
                                </tr>
                                <tr>
                                    <td style="width: 112px"><b></b></td>
                                    <td colspan="3"><b><asp:CheckBox ID="ExemptCheckBox" runat="server" Checked='<%# Bind("Exempt") %>' />Exempt From Customer Discount?</b></td>
                                    <td style="width: 99px"><b></b></td>
                                </tr>
                            </table>
                        </fieldset>     
                        <fieldset>
                             <table>
                                <tr>
                                    <td align="right" style="padding-left:5px"><b><asp:Label ID="costlabel" runat="server" Text="Std Mat'l Cost:"></asp:Label></b></td>
                                    <td ><b><asp:TextBox ID="StdMatTextBox" runat="server" Text='<%# Bind("StdMat") %>'></asp:TextBox></b>
                                    <asp:CompareValidator ID="CompareValidator3" runat="server" ErrorMessage="Only Numbers" ControlToValidate="StdMatTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                    </td>
                                    <td ><b><asp:Label ID="Label33" runat="server" Text="Total Std Cost:"></asp:Label></b></td>
                                    <td ><b><asp:TextBox ID="StdTotTextBox" runat="server" Text='<%# Bind("StdTot") %>'></asp:TextBox></b>
                                    <asp:CompareValidator ID="CompareValidator1" runat="server" ErrorMessage="Only Numbers" ControlToValidate="StdTotTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                    </td>
                                </tr>
                                <tr>
                                    <td align="right" style="padding-left:5px"><b><asp:Label ID="Label34" runat="server" Text="Std Labor Cost:"></asp:Label></b></td>
                                    <td><b><asp:TextBox ID="StdLabTextBox" runat="server" Text='<%# Bind("StdLab") %>'></asp:TextBox></b>
                                    <asp:CompareValidator ID="CompareValidator2" runat="server" ErrorMessage="Only Numbers" ControlToValidate="StdLabTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator></td>
                                    <td align="right" style="padding-left:5px"><b><asp:Label ID="Label35" runat="server" Text="Average Cost:"></asp:Label></b></td>
                                    <td><b><asp:TextBox ID="AverageTextBox" runat="server" Text='<%# Bind("Average") %>'></asp:TextBox></b>
                                    <asp:CompareValidator ID="CompareValidator4" runat="server" ErrorMessage="Only Numbers" ControlToValidate="AverageTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator></td>
                                </tr>
                                <tr>
                                    <td align="right" style="padding-left:5px"><b><asp:Label ID="Label36" runat="server" Text="Std Var OH Cost:"></asp:Label></b></td>
                                    <td><b><asp:TextBox ID="StdVarTextBox" runat="server" Text='<%# Bind("StdVar") %>'></asp:TextBox></b>
                                    <asp:CompareValidator ID="CompareValidator5" runat="server" ErrorMessage="Only Numbers" ControlToValidate="StdVarTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator></td>  
                                    <td align="right" style="padding-left:5px"><b><asp:Label ID="Label37" runat="server" Text="Last Cost:"></asp:Label></b></td>
                                    <td><b><asp:TextBox ID="LastCostTextBox" runat="server" Text='<%# Bind("LastCost") %>'></asp:TextBox></b>
                                    <asp:CompareValidator ID="CompareValidator6" runat="server" ErrorMessage="Only Numbers" ControlToValidate="LastCostTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator></td>
                                </tr>
                                <tr>
                                    <td align="right" style="padding-left:5px"><b><asp:Label ID="Label38" runat="server" Text=" Std Fix OH Cost:"></asp:Label></b></td>
                                    <td><b><asp:TextBox ID="StdFixTextBox" runat="server" Text='<%# Bind("StdFix") %>'></asp:TextBox></b>
                                    <asp:CompareValidator ID="CompareValidator7" runat="server" ErrorMessage="Only Numbers" ControlToValidate="StdFixTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator></td>
                                    <td align="right" style="padding-left:5px"><b><asp:Label ID="Label39" runat="server" Text="Cost UOM:"></asp:Label></b></td>
                                    <td><b><asp:TextBox ID="LastUomTextBox" runat="server" Text='<%# Bind("LastUom") %>'></asp:TextBox></b></td>
                                </tr>
                            </table>
                        </fieldset> 
                        </td>
                    </tr>  
                </table>
            </td>
            </tr>                            
            <tr>
                <td style="display:none"><b><asp:Label ID="usercostframLabel" Width="120px" runat="server" Text='<%# Bind("usercostfram") %>'></asp:Label></b></td>
                <td colspan="2"> 
                    <asp:Button ID="UpdateButton" runat="server" CssClass="button"  Text="Save" OnClick="UpdateButton_Click1"></asp:Button>
                     <asp:Button ID="copy_save_Button" runat="server" CssClass="button"  Text="Save" OnClick="InsertButton_Click1" ></asp:Button>
                    <asp:Button ID="UpdateCancelButton" CssClass="button"  runat="server" CausesValidation="False" CommandName="Cancel" Text="Cancel"></asp:Button>  
                </td>    
            </tr>        
            </table>    
                                                                             
           
           
           <%-- typecode:
           <asp:TextBox ID="typecodeTextBox" runat="server" Text='<%# Bind("typecode") %>'>
           </asp:TextBox><br />--%>
                    
        </EditItemTemplate>
        
        
        <InsertItemTemplate>
        <table>                                     
            <tr>
                <td align="right" style="width: 63px; height: 28px;"><b>FG Item#:</b></td>
                <td style="width: 142px; height: 28px;">
                    <b><asp:TextBox ID="Item1TextBox" runat="server" Text='<%# Bind("Item1") %>'></asp:TextBox></b>
                </td>
        
                <td align="right" style="width: 112px; height: 28px;">
                    <b><asp:CheckBox ID="IsSetCheckBox" runat="server" Checked='<%# Bind("IsSet") %>' />SetHeader?</b>
                </td>        
                <td colspan="2" align="left" >
        <asp:RadioButtonList ID="purchaseRadioButtonList"  RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="4"  
            SelectedValue ='<%# Bind("Purchase") %>' Font-Bold="true"    runat="server">
             <asp:ListItem value="True"  Text="Purchased"   />
             <asp:ListItem value="False" Text="Manufactured" />                                                         
         </asp:RadioButtonList>
         </td>
        
      
        <td style="width: 191px; height: 28px;" align="left">
           <asp:RadioButtonList ID="stockRadioButtonList"  RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="4"  
            SelectedValue ='<%# Bind("stock") %>' Font-Bold="true"    runat="server">
             <asp:ListItem value="S"  Text="Stock Item"   />
             <asp:ListItem value="C" Text="Custom Box" />                                                         
         </asp:RadioButtonList>
             
        </td>
                <td style="width: 199px; height: 28px;" align="left">
                    <b><asp:CheckBox ID="TaxCheckBox" runat="server" Checked='<%# Bind("Tax") %>' />Taxable</b>
                </td>
            </tr>
                                      
            <tr>
                <td align="right" style="width: 63px">
                    <b>Cust Part#:</b>
                </td>
                <td colspan="4">
                    <b> <asp:TextBox ID="PartTextBox" runat="server" Text='<%# Bind("Part") %>'></asp:TextBox></b> 
                        <asp:RequiredFieldValidator ID="RequiredFieldValidator1" ControlToValidate="PartTextBox" Display="Dynamic" runat="server" ErrorMessage="Cust Part is mandatory"></asp:RequiredFieldValidator>
                    <b>Cust#:</b>
                    <b><asp:TextBox ID="CustTextBox" runat="server" Text='<%# Bind("Cust") %>'></asp:TextBox></b> 
                    <a href="#" tabindex="1" onClick="contactcustomerlook(); return false"><asp:Image ID="Image5" runat="server" ImageUrl="images/lookup_icon.gif" /></a>      
                        <asp:RequiredFieldValidator ID="RequiredFieldValidator2" ControlToValidate="CustTextBox" Display="Dynamic" runat="server" ErrorMessage="Customer no is mandatory"></asp:RequiredFieldValidator>
                    <b><asp:TextBox ID="CustNameTextBox" runat="server" Text='<%# Bind("CustName") %>'></asp:TextBox></b>
                        <asp:RequiredFieldValidator ID="RequiredFieldValidator3" ControlToValidate="CustNameTextBox" Display="Dynamic" runat="server" ErrorMessage="Customer Name is mandatory"></asp:RequiredFieldValidator>
                </td>       
            </tr>
                          
            <tr>
                <td align="right" style="width: 65px; height: 21px;">
                    <b>Item Name:</b>
                </td>
                <td style="width: 142px; height: 21px;">
                    <b><asp:TextBox ID="Name1TextBox" runat="server" Text='<%# Bind("Name1") %>'> </asp:TextBox></b>
                        <asp:RequiredFieldValidator ID="RequiredFieldValidator4" ControlToValidate="Name1TextBox" Display="Dynamic" runat="server" ErrorMessage="Item Name is mandatory"></asp:RequiredFieldValidator>
                </td>
                <td align="right" style="width: 112px; height: 21px;">
                    <b>Shipping Method</b>
                </td>
                 <td style="height: 21px;" colspan="4"><b>
          <asp:RadioButtonList ID="ShipRadioButtonList"  RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="4"  
            SelectedValue ='<%# Bind("casepal") %>' Font-Bold="true"    runat="server">
             <asp:ListItem value="True"  Text="Case"   />
             <asp:ListItem value="False" Text="Pallet" />                                                         
         </asp:RadioButtonList>  </b>&nbsp;&nbsp;&nbsp;
         <b> Status:  <asp:RadioButtonList ID="statusRadioButtonList"  RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="4"  
            SelectedValue ='<%# Bind("Stat") %>' Font-Bold="true"     runat="server">
             <asp:ListItem value="A"  Text="Active"   />
             <asp:ListItem value="I" Text="Inactive" />                                                         
         </asp:RadioButtonList>
               </b>
        </td>
            </tr>
              
            <tr><td colspan="7">
                <table><tr><td>
                    <table><tr><td colspan="2" valign="top">
                        <table>                                                         
                            <tr>
                                <td align="right" style="width: 56px"><b>Desc 1:</b></td>
                                <td style="width: 142px"><b><asp:TextBox ID="DscrTextBox" runat="server" Text='<%# Bind("Dscr") %>'></asp:TextBox></b></td>
                            </tr>
                            
                            <tr>
                                <td align="right" style="width: 56px"><b>Desc 2:</b></td>
                                <td style="width: 142px"><b><asp:TextBox ID="Dscr2TextBox" runat="server" Text='<%# Bind("Dscr2") %>'> </asp:TextBox></b></td>
                            </tr>
                            <tr>
                                <td align="right" style="width: 56px"><b> Dscr3:</b></td>
                                <td style="width: 142px"><b><asp:TextBox ID="Dscr3TextBox" runat="server" Text='<%# Bind("Dscr3") %>'></asp:TextBox></b></td>                  
                            </tr>
                        </table>
                        
                        <fieldset style="height:250px">
                        <table>
                            <tr>
                                <td align="right" style="width: 53px"><b>Estimate:</b></td>
                                <td nowrap><b><asp:TextBox ID="EstimateTextBox" runat="server" Text='<%# Bind("Estimate") %>'></asp:TextBox></b>
                                   <a href="#" tabindex="1" onClick="estimatelook(); return false"><asp:Image ID="EstimateLookup" runat="server" ImageUrl="images/lookup_icon.gif" /></a> 
                                </td>
                            </tr>                              
                            <tr>
                                <td align="right" style="width: 53px"><b> Style:</b></td>
                                <td><b><asp:TextBox ID="StyleTextBox" runat="server" Text='<%# Bind("Style") %>'></asp:TextBox></b>
                                    <a href="#" tabindex="1" onClick="stylelook(); return false"><asp:Image ID="Image2" runat="server" ImageUrl="images/lookup_icon.gif" /></a> 
                                </td>
                            </tr>
                            <tr>
                                <td align="right" style="width: 53px"></td> 
                                <td style="width: 142px"><b><asp:Label ID="styledescLabel" Width="120px" runat="server" Text='<%# Bind("styledesc") %>'></asp:Label></b></td>
                            </tr>
                            <tr>
                                <td align="right" style="width: 53px; height: 16px;"><b>Die#:</b></td>
                                <td nowrap><b><asp:TextBox ID="dieTextBox" runat="server" Text='<%# Bind("die") %>'>  </asp:TextBox></b>
                                    <a href="#" tabindex="1" onclick="dielook(); return false"><asp:Image ID="Image9" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                </td> 
                            </tr>
                            <tr>
                                <td align="right" style="width: 53px; height: 16px;"><b> Plate#: </b></td>
                                <td><b><asp:TextBox ID="PlateTextBox" runat="server" Text='<%# Bind("Plate") %>'></asp:TextBox></b>
                                    <a href="#" tabindex="1" onclick="Platelook(); return false"><asp:Image ID="Image11" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                </td>
                            </tr>
                            <tr>
                                <td align="right" style="width: 53px; height: 16px;"><b> CAD#: </b></td>
                                <td style="width: 142px; height: 16px;"><b><asp:TextBox ID="CadTextBox" runat="server" Text='<%# Bind("Cad") %>'></asp:TextBox></b></td>
                            </tr>
                            <tr>
                                <td align="right" style="width: 53px; height: 16px;"><b> SPC/QC#: </b></td>
                                <td style="width: 142px; height: 16px;"><b><asp:TextBox ID="SPCTextBox" runat="server" Text='<%# Bind("SPC") %>'></asp:TextBox></b></td>
                            </tr>
                            <tr>
                                <td align="right" style="width: 53px; height: 16px;"><b> UPC#: </b></td>
                                <td style="width: 142px; height: 16px;"><b><asp:TextBox ID="UPCTextBox" runat="server" Text='<%# Bind("UPC") %>'></asp:TextBox></b></td>
                            </tr>                                                    
                        </table>
                        </fieldset>
                    </td>
                    <td valign="top" >
                        <fieldset>
                            <table>
                                <tr>
                                    <td align="right" style="width: 112px"><b>Sell Price:</b></td>
                                    <td style="width: 96px"><b><asp:TextBox ID="SellTextBox" runat="server" Width="80px" Text='<%# Bind("Sell") %>'></asp:TextBox></b></td>
                                    <td align="left" colspan="2"><b>UOM:<asp:TextBox ID="UOMTextBox" runat="server" Width="40px" Text='<%# Bind("UOM") %>'></asp:TextBox></b>
                                        <a href="#" tabindex="1" onClick="uomlook(); return false" ><asp:Image ID="Image3" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                    </td>
                                    
                                    <td>
                                        <asp:DropDownList ID="DropDownList1" Width="100px" SelectedValue='<%# Bind("typecode") %>' DataTextField='<%# Bind("typecode") %>' runat="server">
                                            <asp:ListItem Value="O">O- Original</asp:ListItem>
                                            <asp:ListItem Value="C">C- Change</asp:ListItem>
                                            <asp:ListItem Value="N">N- New</asp:ListItem>
                                            <asp:ListItem Value="Q">Q- Quality/Re-work</asp:ListItem>
                                            <asp:ListItem Value="R">R- Repeat</asp:ListItem>
                                            <asp:ListItem Value="T">T- Transfer</asp:ListItem>
                                            <asp:ListItem Value="X">X- Complete re-run</asp:ListItem>
                                            <asp:ListItem Value=""></asp:ListItem>
                                        </asp:DropDownList>
                                    </td>
                                </tr>
                                <tr>
                                    <td align="right" style="width: 112px; height: 18px;"><b>FG Category:</b></td>
                                    <td nowrap><b><asp:TextBox ID="CategTextBox" runat="server" Width="80px" Text='<%# Bind("Categ") %>'> </asp:TextBox></b>
                                        <a href="#" tabindex="1" onClick="categorylookup(); return false" ><asp:Image ID="Image1" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                    </td>
                                    <td style="width: 133px; height: 18px;"><b><asp:TextBox ID="RptTextBox" runat="server" Text='<%# Bind("Rpt") %>'> </asp:TextBox></b></td>
                                    <td align="right" style="width: 117px; height: 18px;"><b> Currency: </b></td>
                                    <td style="width: 99px; height: 18px;"><b><asp:TextBox ID="CurrTextBox" runat="server" width="80px" Text='<%# Bind("Curr") %>'> </asp:TextBox></b></td>
                                </tr>
                                <tr>
                                    <td align="right" style="width: 112px"><b> WareHouse:</b></td>
                                    <td nowrap><b><asp:TextBox ID="WareHouseTextBox" runat="server" Width="80px" Text='<%# Bind("WareHouse") %>'></asp:TextBox></b>
                                        <a href="#" tabindex="1" onClick ="locationlook(); return false"><asp:Image ID="Image10" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                    </td>
                                    <td style="width: 133px"><b> </b></td>
                                    <td align="right" style="width: 117px"><b>Inventory Class:</b></td>
                                    <td style="width: 99px"><b><asp:TextBox ID="InventoryTextBox" runat="server" width="80px" Text='<%# Bind("Inventory") %>'></asp:TextBox></b></td>
                                </tr>
                                <tr>
                                    <td align="right" style="width: 112px"><b> Bin:</b></td>
                                    <td nowrap><b><asp:TextBox ID="BinTextBox" runat="server" Width="80px" Text='<%# Bind("Bin") %>'></asp:TextBox></b>
                                        <a href="#" tabindex="1" onClick ="binlook(); return false"><asp:Image ID="binlookImage" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                    </td>
                                    <td style="width: 133px"><b></b></td>
                                    <td align="right" style="width: 117px"><b>Cycle Count Code: </b></td>
                                    <td style="width: 99px"><b><asp:TextBox ID="CycleTextBox" runat="server" width="80px" Text='<%# Bind("Cycle") %>'></asp:TextBox></b></td>
                                </tr>
                                <tr>
                                    <td align="right" style="width: 112px"><b>Count:</b></td>
                                    <td style="width: 96px"><b><asp:TextBox ID="CountTextBox" runat="server" Width="80px" Text='<%# Bind("Count") %>'></asp:TextBox></b></td>
                                    <td style="width: 133px"><b> </b></td>
                                    <td align="right" style="width: 117px"><b>Production Code:</b></td>
                                    <td style="width: 99px"><b><asp:TextBox ID="ProductionTextBox" runat="server" width="80px" Text='<%# Bind("Production") %>'> </asp:TextBox></b>
                                        <asp:RequiredFieldValidator ID="RequiredFieldValidator5" ControlToValidate="ProductionTextBox" Display="Dynamic" runat="server" ErrorMessage="Production Code is mandatory"></asp:RequiredFieldValidator>
                                    </td>
                                </tr>
                                <tr>
                                    <td align="right" style="width: 112px"><b>Wt per 100:</b></td>
                                    <td style="width: 96px"><b><asp:TextBox ID="WeightTextBox" runat="server" Width="80px" Text='<%# Bind("Weight") %>'></asp:TextBox></b></td>
                                    <td style="width: 133px"><b> </b></td>
                                    <td align="right" style="width: 117px"><b>Packing Note:</b></td>
                                    <td style="width: 99px"><b><asp:TextBox ID="PackingTextBox" runat="server" Text='<%# Bind("Packing") %>'></asp:TextBox></b></td>
                                </tr>                                
                                <tr>
                                    <td align="right" style="width: 112px"><b>Freight Class:</b></td>
                                    <td nowrap><b><asp:TextBox ID="FreightTextBox" runat="server" Width="80px" Text='<%# Bind("Freight") %>'></asp:TextBox></b>
                                        <a href="#" tabindex="1" onClick ="freightlook(); return false"><asp:Image ID="Image4" runat="server" ImageUrl="images/lookup_icon.gif" /></a>
                                    </td>
                                    <td style="width: 133px"><b><asp:TextBox ID="FreClassTextBox" runat="server" Text='<%# Bind("FreClass") %>'></asp:TextBox></b></td>
                                    <td style="width: 117px"><b> </b></td>
                                    <td style="width: 99px"><b> </b></td>
                                </tr>
                                <tr>
                                    <td style="width: 112px"><b></b></td>
                                    <td colspan="3"><b><asp:CheckBox ID="ExemptCheckBox" runat="server" Checked='<%# Bind("Exempt") %>' />Exempt From Customer Discount?</b></td>
                                    <td style="width: 99px"><b></b></td>
                                </tr>
                            </table>
                        </fieldset>     
                        <fieldset>
                            <table>
                                <tr>
                                    <td align="right" style="padding-left:5px"><b><asp:Label ID="costlabel" runat="server" Text="Std Mat'l Cost:"></asp:Label></b></td>
                                    <td ><b><asp:TextBox ID="StdMatTextBox" runat="server" Text='<%# Bind("StdMat") %>'></asp:TextBox></b>
                                    <asp:CompareValidator ID="CompareValidator3" runat="server" ErrorMessage="Only Numbers" ControlToValidate="StdMatTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                    </td>
                                    <td ><b><asp:Label ID="Label33" runat="server" Text="Total Std Cost:"></asp:Label></b></td>
                                    <td ><b><asp:TextBox ID="StdTotTextBox" runat="server" Text='<%# Bind("StdTot") %>'></asp:TextBox></b>
                                    <asp:CompareValidator ID="CompareValidator1" runat="server" ErrorMessage="Only Numbers" ControlToValidate="StdTotTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator>
                                    </td>
                                </tr>
                                <tr>
                                    <td align="right" style="padding-left:5px"><b><asp:Label ID="Label34" runat="server" Text="Std Labor Cost:"></asp:Label></b></td>
                                    <td><b><asp:TextBox ID="StdLabTextBox" runat="server" Text='<%# Bind("StdLab") %>'></asp:TextBox></b>
                                    <asp:CompareValidator ID="CompareValidator2" runat="server" ErrorMessage="Only Numbers" ControlToValidate="StdLabTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator></td>
                                    <td align="right" style="padding-left:5px"><b><asp:Label ID="Label35" runat="server" Text="Average Cost:"></asp:Label></b></td>
                                    <td><b><asp:TextBox ID="AverageTextBox" runat="server" Text='<%# Bind("Average") %>'></asp:TextBox></b>
                                    <asp:CompareValidator ID="CompareValidator4" runat="server" ErrorMessage="Only Numbers" ControlToValidate="AverageTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator></td>
                                </tr>
                                <tr>
                                    <td align="right" style="padding-left:5px"><b><asp:Label ID="Label36" runat="server" Text="Std Var OH Cost:"></asp:Label></b></td>
                                    <td><b><asp:TextBox ID="StdVarTextBox" runat="server" Text='<%# Bind("StdVar") %>'></asp:TextBox></b>
                                    <asp:CompareValidator ID="CompareValidator5" runat="server" ErrorMessage="Only Numbers" ControlToValidate="StdVarTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator></td>  
                                    <td align="right" style="padding-left:5px"><b><asp:Label ID="Label37" runat="server" Text="Last Cost:"></asp:Label></b></td>
                                    <td><b><asp:TextBox ID="LastCostTextBox" runat="server" Text='<%# Bind("LastCost") %>'></asp:TextBox></b>
                                    <asp:CompareValidator ID="CompareValidator6" runat="server" ErrorMessage="Only Numbers" ControlToValidate="LastCostTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator></td>
                                </tr>
                                <tr>
                                    <td align="right" style="padding-left:5px"><b><asp:Label ID="Label38" runat="server" Text=" Std Fix OH Cost:"></asp:Label></b></td>
                                    <td><b><asp:TextBox ID="StdFixTextBox" runat="server" Text='<%# Bind("StdFix") %>'></asp:TextBox></b>
                                    <asp:CompareValidator ID="CompareValidator7" runat="server" ErrorMessage="Only Numbers" ControlToValidate="StdFixTextBox" Display="dynamic" SetFocusOnError="true" Operator="datatypecheck" Type="double"></asp:CompareValidator></td>
                                    <td align="right" style="padding-left:5px"><b><asp:Label ID="Label39" runat="server" Text="Cost UOM:"></asp:Label></b></td>
                                    <td><b><asp:TextBox ID="LastUomTextBox" runat="server" Text='<%# Bind("LastUom") %>'></asp:TextBox></b></td>
                                </tr>
                            </table>
                        </fieldset> 
                        </td>
                    </tr>  
                </table>
            </td>
            </tr> 
                                                               
            <tr>
                <td colspan="2"> 
                    <asp:Button ID="InsertButton" CssClass="button" runat="server" CausesValidation="True"  Text="Save" OnClick="InsertButton_Click1"></asp:Button>
                    <asp:Button ID="InsertCancelButton" CssClass="button"  runat="server" CausesValidation="False" CommandName="Cancel" Text="Cancel"></asp:Button>
                </td>    
            </tr>        
        </table> 
        </InsertItemTemplate>
        <ItemTemplate>           
        <table>
        <tr>
        <td align="right" style="width: 63px; height: 28px;">
        <b>FG Item#:</b>
        </td>
        <td style="width: 142px; height: 28px;">
        <b><asp:Label ID="ItemLabel" runat="server" Text='<%# Bind("Item1") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="130px"></asp:Label> </b>
        </td>
        <td align="right" style="width: 112px; height: 28px;">
        <b><asp:CheckBox ID="CheckBox7" runat="server" Checked='<%# Bind("IsSet") %>' Enabled="false" /> SetHeader? </b>
        </td>
        <td colspan="2" align="left" >
        <asp:RadioButtonList ID="purchaseRadioButtonList"  RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="4"  
            SelectedValue ='<%# Bind("Purchase") %>' Font-Bold="true" Enabled="false"    runat="server">
             <asp:ListItem value="True"  Text="Purchased"   />
             <asp:ListItem value="False" Text="Manufactured" />                                                         
         </asp:RadioButtonList>
         </td>
        
      
        <td style="width: 191px; height: 28px;" align="left">
           <asp:RadioButtonList ID="stockRadioButtonList"  RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="4"  
            SelectedValue ='<%# Bind("stock") %>' Font-Bold="true" Enabled="false"    runat="server">
             <asp:ListItem value="S"  Text="Stock Item"   />
             <asp:ListItem value="C" Text="Custom Box" />                                                         
         </asp:RadioButtonList>
             
        </td>
        <td style="width: 199px; height: 28px;" align="left"> <asp:CheckBox ID="CheckBox9" runat="server" Checked='<%# Bind("Tax") %>' Enabled="false" style="position: relative" /><strong>Taxable</strong></td>
            
        </tr>
        
        <tr>
        <td align="right" style="width: 63px">
        <b>Cust Part#:</b>
        </td>
        <td colspan="4">
        <b><asp:Label ID="Label1" runat="server" Text='<%# Bind("Part") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label> </b>
        <b>Cust#:</b>
        <b>&nbsp;<asp:Label ID="Label2" runat="server" Text='<%# Bind("Cust") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="120px"></asp:Label></b>
        <b> <asp:Label ID="Label3" runat="server" Text='<%# Bind("CustName") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="140px"></asp:Label></b></td>
        
        <td align="right" style="width: 191px">
        <b></b></td>
        <td style="width: 99px">
        </td>
        </tr>
        <tr>
        <td align="right" style="width: 65px; height: 21px;">
        <b>Item Name:</b>
        </td>
        <td style="width: 142px; height: 21px;">
        <b><asp:Label ID="NameLabel" runat="server" Text='<%# Bind("Name1") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="140px"></asp:Label> </b>
        </td>
        <td align="right" style="width: 112px; height: 21px;">
        <b>Shipping Method:</b></td>
        <td style="height: 21px;" colspan="4"><b>
          <asp:RadioButtonList ID="ShipRadioButtonList"  RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="4"  
            SelectedValue ='<%# Bind("casepal") %>' Font-Bold="true"  Enabled="false"   runat="server">
             <asp:ListItem value="True"  Text="Case"   />
             <asp:ListItem value="False" Text="Pallet" />                                                         
         </asp:RadioButtonList>  </b>&nbsp;&nbsp;&nbsp;
         <b> Status:  <asp:RadioButtonList ID="statusRadioButtonList"  RepeatLayout="Flow"  CellSpacing="1" RepeatColumns="4"  
            SelectedValue ='<%# Bind("Stat") %>' DataTextField='<%# Bind("Stat") %>'  Font-Bold="true" Enabled="false"    runat="server">
             <asp:ListItem value="A"  Text="Active"   />
             <asp:ListItem value="I" Text="Inactive" />                                                         
         </asp:RadioButtonList>
               </b>
        </td>
        </tr>
        
        <tr><td colspan="7"><table>
        <tr>
        <td>
        <table>
                
        <tr><td colspan="2">
        <table>
        
        <tr>
        <td align="right" style="width: 56px">
        <b>Desc 1:</b>
        </td>
        <td style="width: 142px">
        <b><asp:Label ID="Label4" runat="server" Text='<%# Bind("Dscr") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="200px"></asp:Label> </b>
        </td></tr>
        <tr><td align="right" style="width: 56px">
        
        <b>Desc 2:</b></td>
        <td style="width: 142px">
        <b> 
            <asp:Label ID="Dscr2Label" runat="server" Text='<%# Bind("Dscr2") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="200px"></asp:Label></b></td></tr>
        <tr><td align="right" style="width: 56px">
        <b>Desc 3:</b></td>
        <td style="width: 142px">
        <b> 
            <asp:Label ID="Dscr3Label" runat="server" Text='<%# Bind("Dscr3") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="200px"></asp:Label></b></td>
        </tr>
        </table>
        <fieldset style="height:200px">
        <table>
        <tr>
        <td align="right" style="width: 53px">
        <b>Estimate#:
             </b>
        </td>
        <td style="width: 142px">
        <b><asp:Label ID="Label12" runat="server" Text='<%# Bind("Estimate") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="125px"></asp:Label> </b>
        </td></tr>
        <tr><td align="right" style="width: 53px">
        <b>Style:</b>
        </td>
        <td style="width: 142px">
        <b><asp:Label ID="Label15" runat="server" Text='<%# Bind("Style") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="125px"></asp:Label> </b>
        </td></tr>
        <tr><td></td>
        <td><asp:Label ID="styledescLabel" BackColor="turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="125px" runat="server" Text='<%# Bind("styledesc") %>'></asp:Label></td></tr>
        
        <tr><td align="right" style="width: 53px; height: 16px;">
        <b>Die#:</b> </td>
        <td style="width: 142px; height: 16px;">
        <b><asp:Label ID="Label18" runat="server" Text='<%# Bind("die") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="125px"></asp:Label>  </b>
        </td></tr>
        <tr><td align="right" style="width: 53px">
        <b>Plate#:</b>
        </td>
        <td style="width: 142px">
        <b><asp:Label ID="Label21" runat="server" Text='<%# Bind("Plate") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="125px"></asp:Label> </b>
        </td></tr>
        <tr><td align="right" style="width: 53px"><b>CAD#:</b></td>
        <td style="width: 142px"><b><asp:Label ID="Label23" runat="server" Text='<%# Bind("Cad") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="125px"></asp:Label></b></td></tr>
        <tr><td align="right" style="width: 53px">
            <strong>SPC/QC#: </strong>
        </td>       
        <td style="width: 142px">
        <b><asp:Label ID="Label26" runat="server" Text='<%# Bind("SPC") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="125px"></asp:Label></b></td></tr>
        <tr><td align="right" style="width: 53px">
            <strong>UPC#: </strong>
        </td>       
        <td style="width: 142px">
        <b><asp:Label ID="upcnoLabel" runat="server" Text='<%# Bind("UPC") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="125px"></asp:Label></b></td></tr>
        </tr>   
        </table>
        </td>
        <td valign="top" >
        <fieldset>
           <table>
           <tr><td align="right" style="width: 112px">
        <b>Sell Price:</b>
        </td>
        <td style="width: 96px">
        <b><asp:Label ID="Label5" runat="server" Text='<%# Bind("Sell","{0:###,##0.00}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label> </b>
        </td>
        <td align="left" colspan="2">
            <strong>UOM:</strong><b>&nbsp;<asp:Label ID="Label6" runat="server" Text='<%# Bind("UOM") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="40px"></asp:Label> </b>
        </td>
        <td style="width: 99px">
         <asp:DropDownList ID="DropDownList1" Width="100px" SelectedValue='<%# Bind("typecode") %>' Enabled="false" DataTextField='<%# Bind("typecode") %>' runat="server">
                                            <asp:ListItem Value="O">O- Original</asp:ListItem>
                                            <asp:ListItem Value="C">C- Change</asp:ListItem>
                                            <asp:ListItem Value="N">N- New</asp:ListItem>
                                            <asp:ListItem Value="Q">Q- Quality/Re-work</asp:ListItem>
                                            <asp:ListItem Value="R">R- Repeat</asp:ListItem>
                                            <asp:ListItem Value="T">T- Transfer</asp:ListItem>
                                            <asp:ListItem Value="X">X- Complete re-run</asp:ListItem>
                                            <asp:ListItem Value=""></asp:ListItem>
                                        </asp:DropDownList>
        </td></tr>
        <tr>
        
        <td align="right" style="width: 112px; height: 18px;">
        <b>FG Category:</b>
        </td>
        <td style="width: 96px; height: 18px;">
        <b><asp:Label ID="Label8" runat="server" Text='<%# Bind("Categ") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b>
        </td>
        <td style="width: 133px; height: 18px;">
        <b><asp:Label ID="Label9" runat="server" Text='<%# Bind("Rpt") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="120px"></asp:Label> </b>
        </td>
        <td align="right" style="width: 117px; height: 18px;">
        <b>Currency:</b>
        </td>
        <td style="width: 99px; height: 18px;">
        <b> <asp:Label ID="Label7" runat="server" Text='<%# Bind("Curr") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>
        </tr>
        <tr>
        <td align="right" style="width: 112px">
        <b>WareHouse:</b>
        </td>
        <td style="width: 96px">
        <b><asp:Label ID="Label10" runat="server" Text='<%# Bind("WareHouse") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label> </b>
        </td>
        <td style="width: 133px">
        <b> </b>
        </td>
        <td align="right" style="width: 117px">
        <b>Inventory Class:</b></td>
        <td style="width: 99px">
        <b><asp:Label ID="Label11" runat="server" Text='<%# Bind("Inventory") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b></td>
        </tr>
        <tr>
        
        <td align="right" style="width: 112px">
        <b>Bin:</b>
        </td>
        <td style="width: 96px">
        <b><asp:Label ID="Label13" runat="server" Text='<%# Bind("Bin") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b>
        </td>
        <td style="width: 133px">
        <b> </b>
        </td>
        <td align="right" style="width: 117px">
        <b>Cycle Count Code: </b>
        </td>
        <td style="width: 99px">
        <asp:Label ID="Label14" runat="server" Text='<%# Bind("Cycle") %>' Font-Bold="True" BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></td>
        </tr>
        <tr>
        
        <td align="right" style="width: 112px">
        <b>Count: </b>
        </td>
        <td style="width: 96px">
        <b><asp:Label ID="Label16" runat="server" Text='<%# Bind("Count") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label> </b>
        </td>
        <td style="width: 133px">
        <b> </b>
        </td>
        <td align="right" style="width: 117px">
        <b>Production Code:</b>
        </td>
        <td style="width: 99px">
        <asp:Label ID="Label17" runat="server" Text='<%# Bind("Production") %>' Font-Bold="True" BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></td>
        </tr>
        
        
        <tr>
        
        <td align="right" style="width: 112px">
        <b>Wt per 100:</b>
        </td>
        <td style="width: 96px">
        <b> <asp:Label ID="Label19" runat="server" Text='<%# Bind("Weight","{0:###,##0.00}") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b>
        </td>
        <td style="width: 133px">
        <b> </b>
        </td>
        <td align="right" style="width: 117px">
        <b>Packing Note:</b>
        </td>
        <td style="width: 99px">
        <asp:Label ID="Label20" runat="server" Text='<%# Bind("Packing") %>' Font-Bold="True" BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="160px"></asp:Label></td>
        </tr>
        <tr>
        
        <td align="right" style="width: 112px">
        <b>Freight Class:</b>
        </td>
        <td style="width: 96px">
        <b> <asp:Label ID="Label22" runat="server" Text='<%# Bind("Freight") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="80px"></asp:Label></b>
        </td>
        <td style="width: 133px">
        <b> 
            <asp:Label ID="FreClassLabel" runat="server" Text='<%# Bind("FreClass") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="120px"></asp:Label></b></td>
        <td style="width: 117px">
        <b> </b>
        </td>
        <td style="width: 99px">
        <b> </b>
        </td>
        </tr>
        <tr>
        
        <td style="width: 112px"><b></b></td>
            <td colspan="3">
         <b>   <asp:CheckBox ID="ExemptCheckBox" runat="server" Checked='<%# Bind("Exempt") %>'
                Enabled="false" />Exempt From Customer Discount?</b><b></b><b></b></td>
        <td style="width: 99px"><b></b></td>
        </tr>
           </table>
           </fieldset>
           <fieldset style="display:none" >
           <table id="custtable" runat="server">
           <tr><td align="right" style="padding-left:5px"><b><asp:Label ID="costlabel" runat="server" Text="Std Mat'l Cost:"></asp:Label>
           </b></td>
           <td><asp:Label ID="Label24" runat="server" Text='<%# Bind("StdMat") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="120px"></asp:Label></td>
           <td align="right" style="padding-left:5px"><b><asp:Label ID="Label33" runat="server" Text="Total Std Cost:"></asp:Label>
           </b></td>
           <td><asp:Label ID="Label25" runat="server" Text='<%# Bind("StdTot") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="120px"></asp:Label></td></tr>
           <tr><td align="right" style="padding-left:5px"><b><asp:Label ID="Label34" runat="server" Text="Std Labor Cost:"></asp:Label>
           </b></td>
           <td><asp:Label ID="Label27" runat="server" Text='<%# Bind("StdLab") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="120px"></asp:Label></td>
           <td align="right" style="padding-left:5px"><b><asp:Label ID="Label35" runat="server" Text="Average Cost:"></asp:Label>
           </b></td>
           <td><asp:Label ID="Label28" runat="server" Text='<%# Bind("Average") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="120px"></asp:Label></td></tr>
           <tr><td align="right" style="padding-left:5px"><b><asp:Label ID="Label36" runat="server" Text="Std Var OH Cost:"></asp:Label>
           </b></td>
           <td><asp:Label ID="Label29" runat="server" Text='<%# Bind("StdVar") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="120px"></asp:Label></td>
           <td align="right" style="padding-left:5px"><b><asp:Label ID="Label37" runat="server" Text="Last Cost:"></asp:Label>
           </b></td>
           <td><asp:Label ID="Label30" runat="server" Text='<%# Bind("LastCost") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="120px"></asp:Label></td></tr>
           <tr><td align="right" style="padding-left:5px"><b><asp:Label ID="Label38" runat="server" Text=" Std Fix OH Cost:"></asp:Label>
          </b></td>
           <td><asp:Label ID="Label31" runat="server" Text='<%# Bind("StdFix") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="120px"></asp:Label></td>
           <td align="right" style="padding-left:5px"><b><asp:Label ID="Label39" runat="server" Text="Cost UOM:"></asp:Label>
           </b></td>
           <td><asp:Label ID="Label32" runat="server" Text='<%# Bind("LastUom") %>' BackColor="Turquoise" BorderColor="Black" BorderStyle="Solid" BorderWidth="1px" Width="120px"></asp:Label></td></tr>
           
           </table>
           
           </fieldset>
           
           
        </td>                       
        </tr>        
        </table>        
        </td>                       
        </tr>  
        
        <tr><td colspan="2">
         <asp:Button ID="AddButton" runat="server" CssClass="button" CausesValidation="False" CommandName="new" Text="Add"></asp:Button>
                        <asp:Button ID="UpdateButton" runat="server" CssClass="button" CausesValidation="False" OnClick="Update_button_Click" CommandName="edit" Text="Update"></asp:Button>
                         <asp:Button ID="Copy_button" runat="server" CssClass="button" CausesValidation="False" OnClick="Copy_button_Click"  CommandName="edit" Text="Copy"></asp:Button>
                        <asp:Button ID="DeleteButton" runat="server" CssClass="button" CausesValidation="False"  OnClick="DeleteButton_Click"  OnClientClick="return confirm('Are you sure you want to delete this record')" Text="Delete"></asp:Button>
                        <asp:Button ID="recal_Button" runat="server" CssClass="button" CausesValidation="False"  OnClick="recal_cost_Click"  Text="Recalc Costs"></asp:Button>
        
        </td>        
        </tr> 
        </table>
        
            <div>
            </div>          
            
        </ItemTemplate>
        <FooterStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
        <EditRowStyle BackColor="#EFF3FB" />
        <RowStyle BackColor="#EFF3FB" />
        <PagerStyle BackColor="#EFF3FB" ForeColor="Black" HorizontalAlign="Center" Font-Bold="True" />
        <HeaderStyle BackColor="#507CD1" Font-Bold="True" ForeColor="White" />
        <PagerSettings FirstPageText="First" LastPageText="Last" Mode="NextPreviousFirstLast" NextPageText="Next" PreviousPageText="Previous" />
    </asp:FormView>
    
    <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" OldValuesParameterFormatString="original_{0}"
        SelectMethod="SelectFgItem" TypeName="fgitem" >
        <SelectParameters>
            <asp:Parameter Name="prmUser" Type="String" />
            <asp:Parameter Name="prmCust" Type="String" />
            <asp:Parameter DefaultValue="Select" Name="prmAction" Type="String" />
            <asp:Parameter  Name="prmOrderNum"  Type="String" />
            <asp:SessionParameter Name="prmItemNum" SessionField="item_list_item" Type="String" />
            <asp:Parameter Name="prmPart" Type="String" />
            <asp:Parameter Name="prmName" Type="String" />
            <asp:Parameter Name="prmDscr" Type="String" />
            <asp:Parameter Name="prmDscr2" Type="String" />
            <asp:Parameter Name="prmDscr3" Type="String" />
            <asp:Parameter Name="prmIsSet" Type="String" />
            <asp:Parameter Name="prmIsCust" Type="String" />
            <asp:Parameter Name="prmCustName" Type="String" />
            <asp:Parameter Name="prmTax" Type="String" />
            <asp:Parameter Name="prmPurchase" Type="String" />
            <asp:Parameter Name="prmEstimate" Type="String" />
            <asp:Parameter Name="prmStyle" Type="String" />
            <asp:Parameter Name="prmdie" Type="String" />
            <asp:Parameter Name="prmPlate" Type="String" />
            <asp:Parameter Name="prmCad" Type="String" />
            <asp:Parameter Name="prmSPC" Type="String" />
            <asp:Parameter Name="prmUPC" Type="String" />
            <asp:Parameter Name="prmSell" Type="String" />
            <asp:Parameter Name="prmUom" Type="String" />
            <asp:Parameter Name="prmCurr" Type="String" />
            <asp:Parameter Name="prmCateg" Type="String" />
            <asp:Parameter Name="prmRpt" Type="String" />
            <asp:Parameter Name="prmWareHouse" Type="String" />
            <asp:Parameter Name="prmBin" Type="String" />
            <asp:Parameter Name="prmCount" Type="String" />
            <asp:Parameter Name="prmWeight" Type="String" />
            <asp:Parameter Name="prmFreight" Type="String" />
            <asp:Parameter Name="prmFreClass" Type="String" />
            <asp:Parameter Name="prmInventory" Type="String" />
            <asp:Parameter Name="prmCycle" Type="String" />
            <asp:Parameter Name="prmProduction" Type="String" />
            <asp:Parameter Name="prmPacking" Type="String" />
            <asp:Parameter Name="prmMat" Type="String" />
            <asp:Parameter Name="prmLab" Type="String" />
            <asp:Parameter Name="prmVar" Type="String" />
            <asp:Parameter Name="prmFix" Type="String" />
            <asp:Parameter Name="prmStd" Type="String" />
            <asp:Parameter Name="prmAvg" Type="String" />
            <asp:Parameter Name="prmLast" Type="String" />
            <asp:Parameter Name="prmlUom" Type="String" />
            <asp:Parameter Name="prmExempt" Type="String" />
            <asp:Parameter Name="prmStat" Type="String" />
            <asp:Parameter Name="prmstock" Type="String" />
            <asp:Parameter Name="prmcasepal" Type="String" />
            <asp:Parameter Name="prmtypecode" Type="String" />
            <asp:Parameter Name="prmNewItem" Type="String" />
        </SelectParameters>
        
    </asp:ObjectDataSource>
          
</div>
</asp:Content>

