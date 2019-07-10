<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="corr_print_gsm3" Codebehind="corr_print_gsm3.aspx.cs" %>
<%@ Register Src="footer.ascx" TagName="Footer" TagPrefix="ft" %>
<%@ Register Src="header.ascx" TagName="Header" TagPrefix="hd" %>
<html xmlns="http://www.w3.org/1999/xhtml" >
  <head id="Head1" runat="server">
    <title>GS&A Detail</title>
    
    <LINK href="include/style.css" type="text/css" rel="stylesheet"/>
     
    <%--<link href="include/tree.css" rel="stylesheet" type="text/css" />--%>
    <link href="include/dhtmlwindow.css" rel="stylesheet" type="text/css" />
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

    function unitperpallet() {
        var palletcount = document.getElementById("FormView1_ld_gsa_war_cntTextBox");
        var unitcount = document.getElementById("FormView1_ld_gsa_war_u_cTextBox");
        var unitpercount = document.getElementById("FormView1_ld_gsa_war_u_pTextBox");

        var pc = unitcount.innerHTML * unitpercount.value;
        if (pc > 0)
        palletcount.innerHTML = pc;

    var totpall = document.getElementById("FormView1_ld_gsa_war_palTextBox");
    var totunit = document.getElementById("FormView1_ld_gsa_war_uniTextBox");
    totpall.value = totunit.innerHTML / unitpercount.value;

}

function GsaSelect() {
    var gsa = document.getElementById("FormView1_CheckBox1");
    var markup = document.getElementById("FormView1_ld_gsa_warTextBox");
    var fm = document.getElementById("FormView1_ld_gsa_fmTextBox");
    var unitcount = document.getElementById("FormView1_ld_gsa_war_u_cTextBox");
    var palletcount = document.getElementById("FormView1_ld_gsa_war_cntTextBox");
    var totunit = document.getElementById("FormView1_ld_gsa_war_uniTextBox");
    var unitperpallet = document.getElementById("FormView1_ld_gsa_war_u_pTextBox");
    var totpallet = document.getElementById("FormView1_ld_gsa_war_palTextBox");
    var costperpall = document.getElementById("FormView1_ld_gsa_war_amtTextBox");

    var handcharge = document.getElementById("FormView1_ld_gsa_war_hdlTextBox");
    var delmonth = document.getElementById("FormView1_ld_gsa_war_perTextBox");
    var totch = document.getElementById("FormView1_ld_gsa_war_totTextBox");

    if (gsa.checked == true) {
        markup.disabled = false;
        fm.disabled = false;
        unitcount.disabled = false;
        palletcount.disabled = false;
        totunit.disabled = false;
        unitperpallet.disabled = false;
        totpallet.disabled = false;
        costperpall.disabled = false;
        handcharge.disabled = false;
        delmonth.disabled = false;
        totch.disabled = false;
    }
    else {
        markup.disabled = true;
        fm.disabled = true;
        unitcount.disabled = true;
        palletcount.disabled = true;
        totunit.disabled = true;
        unitperpallet.disabled = true;
        totpallet.disabled = true;
        costperpall.disabled = true;
        handcharge.disabled = true;
        delmonth.disabled = true;
        totch.disabled = true;
    }

}

    </script> 
  </head>    
   <body>
    <form id="frmList" runat="server"   defaultfocus ='' >   
        <%--<hd:header id="Header1" runat="server"></hd:header>--%>
      <div>
            
          <asp:HiddenField ID="HiddenField1" runat="server" />
          <asp:HiddenField ID="HiddenField2" runat="server" />
          <asp:HiddenField ID="HiddenField3" runat="server" />
          <asp:HiddenField ID="HiddenField4" runat="server" />
          <asp:Label ID="Label1" runat="server" ForeColor="red" Font-Bold="true"></asp:Label>
            
          <asp:FormView ID="FormView1" runat="server" OnDataBound="formview1_databound" DataSourceID="ObjectDataSource1">
              <EditItemTemplate>
              <table><tr><td>
              <table class="shade">
              <tr><td><b>Cost</b></td>
              <td><b>Mat'l%</b></td><td><b>Cost</b></td><td><b>Labor%</b></td></tr>
              
              <tr><td><asp:Label ID="vMatcost1Label" runat="server" Text='<%# Bind("vMatcost1") %>' /></td>
              <td><asp:Label ID="vMatpct1Label" runat="server" Text='<%# Bind("vMatpct1") %>' /></td>              
              <td><asp:Label ID="vLabcost1Label" runat="server" Text='<%# Bind("vLabcost1") %>' /></td>
              <td><asp:Label ID="vLabpct1Label" runat="server" Text='<%# Bind("vLabpct1") %>' /></td></tr>
              
              <tr><td><asp:Label ID="vMatcost2Label" runat="server" Text='<%# Bind("vMatcost2") %>' /></td>
              <td> <asp:Label ID="vMatpct2Label" runat="server" Text='<%# Bind("vMatpct2") %>' /></td>
              <td><asp:Label ID="vLabcost2Label" runat="server" Text='<%# Bind("vLabcost2") %>' /></td>
              <td><asp:Label ID="vLabpct2Label" runat="server" Text='<%# Bind("vLabpct2") %>' /></td></tr>
              
              <tr><td><asp:Label ID="vMatcost3Label" runat="server" Text='<%# Bind("vMatcost3") %>' /></td>
              <td><asp:Label ID="vMatpct3Label" runat="server" Text='<%# Bind("vMatpct3") %>' /></td>
              <td><asp:Label ID="vLabcost3Label" runat="server" Text='<%# Bind("vLabcost3") %>' /></td>
              <td><asp:Label ID="vLabpct3Label" runat="server" Text='<%# Bind("vLabpct3") %>' /></td></tr>
              
              <tr><td> <asp:Label ID="vMatcost4Label" runat="server" Text='<%# Bind("vMatcost4") %>' /></td>
              <td><asp:Label ID="vMatpct4Label" runat="server" Text='<%# Bind("vMatpct4") %>' /></td>
              <td><asp:Label ID="vLabcost4Label" runat="server" Text='<%# Bind("vLabcost4") %>' /></td>
              <td><asp:Label ID="vLabpct4Label" runat="server" Text='<%# Bind("vLabpct4") %>' /></td></tr>
              
              <tr><td><asp:Label ID="vMatcost5Label" runat="server" Text='<%# Bind("vMatcost5") %>' /></td>
              <td><asp:Label ID="vMatpct5Label" runat="server" Text='<%# Bind("vMatpct5") %>' /></td>
              <td><asp:Label ID="vLabcost5Label" runat="server" Text='<%# Bind("vLabcost5") %>' /></td>
              <td><asp:Label ID="vLabpct5Label" runat="server" Text='<%# Bind("vLabpct5") %>' /></td></tr>
              
               <tr><td><asp:Label ID="vMatcost6Label" runat="server" Text='<%# Bind("vMatcost6") %>' /></td>
               <td><asp:Label ID="vMatpct6Label" runat="server" Text='<%# Bind("vMatpct6") %>' /></td>
               <td><asp:Label ID="vLabcost6Label" runat="server" Text='<%# Bind("vLabcost6") %>' /></td>
               <td> <asp:Label ID="vLabpct6Label" runat="server" Text='<%# Bind("vLabpct6") %>' /></td></tr>
              </table></td>
              <td nowrap valign="top">
              
              <table ><tr><td width="137px"><b>Overrides for Qty:</b></td>
              <td><asp:Label ID="ld_qtyTextBox" runat="server" Text='<%# Bind("[ld-qty]") %>' /></td></tr>
              
              <tr><td><b>GS&A Board:</b></td>
              <td nowrap><asp:TextBox ID="ld_gsa_brdTextBox" runat="server" Text='<%# Bind("[ld-gsa-brd]") %>' />
              <asp:CompareValidator ID="CompareValidator1" ControlToValidate="ld_gsa_brdTextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Double" SetFocusOnError="true"  runat="server" ErrorMessage="Enter only Decimal Value"></asp:CompareValidator>                  
              </td></tr>                  
              
              <tr><td><b>GS&A Material:</b></td>
              <td nowrap><asp:TextBox ID="ld_gsa_matTextBox" runat="server" Text='<%# Bind("[ld-gsa-mat]") %>' />
              <asp:CompareValidator ID="CompareValidator2" ControlToValidate="ld_gsa_matTextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Double" SetFocusOnError="true"  runat="server" ErrorMessage="Enter only Decimal Value"></asp:CompareValidator>                  
              </td></tr>
              
              <tr><td><b>GS&A Labor:</b></td>
              <td nowrap><asp:TextBox ID="ld_gsa_labTextBox" runat="server" Text='<%# Bind("[ld-gsa-lab]") %>' />
              <asp:CompareValidator ID="CompareValidator3" ControlToValidate="ld_gsa_labTextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Double" SetFocusOnError="true"  runat="server" ErrorMessage="Enter only Decimal Value"></asp:CompareValidator>                  
              </td></tr>
              <tr>
              <td colspan="2" align="right" style="padding-right:5px">
                  <b><asp:CheckBox ID="CheckBox1" OnClick="GsaSelect()" Text="Update Same GSA values other Quantities" runat="server" /></b>
              </td></tr>
              </table>
              <fieldset><table>
              <tr><td><b>Wherehousing Markup:</b></td>
              <td nowrap><asp:TextBox ID="ld_gsa_warTextBox" runat="server" Text='<%# Bind("[ld-gsa-war]") %>' />  
              <asp:CompareValidator ID="CompareValidator4" ControlToValidate="ld_gsa_warTextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Double" SetFocusOnError="true"  runat="server" ErrorMessage="Enter only Decimal Value"></asp:CompareValidator>                              
              </td></tr>
              
              <tr><td><b>Broken Comm:</b></td>
              <td nowrap><asp:TextBox ID="ld_gsa_fmTextBox" runat="server" Text='<%# Bind("[ld-gsa-fm]") %>' />
              <asp:CompareValidator ID="CompareValidator5" ControlToValidate="ld_gsa_fmTextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Double" SetFocusOnError="true"  runat="server" ErrorMessage="Enter only Decimal Value"></asp:CompareValidator>                  
              </td></tr>
              
              <tr><td><b>Unit Count:</b></td>
              <td nowrap><asp:TextBox ID="ld_gsa_war_u_cTextBox" runat="server" Text='<%# Bind("[ld-gsa-war-u-c]") %>' />
              <asp:CompareValidator ID="CompareValidator12" ControlToValidate="ld_gsa_war_u_cTextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Integer" SetFocusOnError="true"  runat="server" ErrorMessage="Enter only Integer Value"></asp:CompareValidator>                  
              </td></tr>
              
              <tr><td><b>Pallet Count:</b></td>
              <td nowrap><asp:TextBox ID="ld_gsa_war_cntTextBox"  runat="server" Text='<%# Bind("[ld-gsa-war-cnt]") %>' />
              <asp:CompareValidator ID="CompareValidator14" ControlToValidate="ld_gsa_war_u_cTextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Integer" SetFocusOnError="true"  runat="server" ErrorMessage="Enter only Integer Value"></asp:CompareValidator>                  
              </td></tr>
              
              <tr><td><b>Total Units:</b></td>
              <td nowrap><asp:TextBox ID="ld_gsa_war_uniTextBox" runat="server" Text='<%# Bind("[ld-gsa-war-uni]") %>' />
              <asp:CompareValidator ID="CompareValidator13" ControlToValidate="ld_gsa_war_uniTextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Double" SetFocusOnError="true"  runat="server" ErrorMessage="Enter only Decimal Value"></asp:CompareValidator>                              
              </td></tr>
              
              <tr><td><b>Units per Pallet:</b></td>
              <td nowrap><asp:TextBox ID="ld_gsa_war_u_pTextBox" onkeyup="unitperpallet()" MaxLength="6" runat="server" Text='<%# Bind("[ld-gsa-war-u-p]") %>' />
              <asp:CompareValidator ID="CompareValidator6" ControlToValidate="ld_gsa_war_u_pTextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Integer" SetFocusOnError="true"  runat="server" ErrorMessage="Enter only Integer Value"></asp:CompareValidator>                  
              </td></tr>
              
              <tr><td><b>Total Pallet:</b></td>
              <td nowrap><asp:TextBox ID="ld_gsa_war_palTextBox" runat="server" Text='<%# Bind("[ld-gsa-war-pal]") %>' />
              <asp:CompareValidator ID="CompareValidator7" ControlToValidate="ld_gsa_war_palTextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Integer" SetFocusOnError="true"  runat="server" ErrorMessage="Enter only Integer Value"></asp:CompareValidator>                  
              </td></tr>
              
              <tr><td><b>Cost per Pallet:</b></td>
              <td><asp:TextBox ID="ld_gsa_war_amtTextBox" runat="server" Text='<%# Bind("[ld-gsa-war-amt]") %>' />
              <asp:CompareValidator ID="CompareValidator8" ControlToValidate="ld_gsa_war_amtTextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Integer" SetFocusOnError="true"  runat="server" ErrorMessage="Enter only Integer Value"></asp:CompareValidator>                  
              </td></tr>
              
                <tr><td><b>Pallet Handling Charge:</b></td>
              <td nowrap><asp:TextBox ID="ld_gsa_war_hdlTextBox" runat="server"  Text='<%# Bind("[ld-gsa-war-hdl]") %>' />
              <asp:CompareValidator ID="CompareValidator9" ControlToValidate="ld_gsa_war_hdlTextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Double" SetFocusOnError="true"  runat="server" ErrorMessage="Enter only Decimal Value"></asp:CompareValidator>                  
              </td></tr>
              
                <tr><td><b>Pallets Delivered/Month:</b></td>
              <td nowrap><asp:TextBox ID="ld_gsa_war_perTextBox" runat="server"  Text='<%# Bind("[ld-gsa-war-per]") %>' />
              <asp:CompareValidator ID="CompareValidator10" ControlToValidate="ld_gsa_war_perTextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Integer" SetFocusOnError="true"  runat="server" ErrorMessage="Enter only Integer Value"></asp:CompareValidator>                  
              </td></tr>
              
                <tr><td><b>Total Charge:</b></td>
              <td nowrap><asp:TextBox ID="ld_gsa_war_totTextBox"  runat="server" Text='<%# Bind("[ld-gsa-war-tot]") %>' />
              <asp:CompareValidator ID="CompareValidator11" ControlToValidate="ld_gsa_war_totTextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Double" SetFocusOnError="true"  runat="server" ErrorMessage="Enter only Decimal Value"></asp:CompareValidator>                  
              </td></tr>
              
              </table></fieldset>
              </td>
              </tr>
              <tr><td>
               <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" OnClick="updatebutton_click" CssClass="button"  Text="Save" />
       &nbsp;
        <input type="button" name="Cancel" class="buttonM" id="close" value="Cancel" onClick="javascript:self.close();window.opener.location.reload();"  />
              </td></tr>
              </table>
                 
                 
              </EditItemTemplate>
              
              <ItemTemplate>
                                    
                  ld-gsa-fm:
                  <asp:Label ID="ld_gsa_fmLabel" runat="server" 
                      Text='<%# Bind("[ld-gsa-fm]") %>' />
                  <br />
                  ld-qty:
                  <asp:Label ID="ld_qtyLabel" runat="server" Text='<%# Bind("[ld-qty]") %>' />
                  <br />
                  ld-gsa-war-tot:
                  <asp:Label ID="ld_gsa_war_totLabel" runat="server" 
                      Text='<%# Bind("[ld-gsa-war-tot]") %>' />
                  <br />
                  ld-gsa-war-cnt:
                  <asp:Label ID="ld_gsa_war_cntLabel" runat="server" 
                      Text='<%# Bind("[ld-gsa-war-cnt]") %>' />
                  <br />
                  ld-gsa-war-u-p:
                  <asp:Label ID="ld_gsa_war_u_pLabel" runat="server" 
                      Text='<%# Bind("[ld-gsa-war-u-p]") %>' />
                  <br />
                  ld-gsa-war-uni:
                  <asp:Label ID="ld_gsa_war_uniLabel" runat="server" 
                      Text='<%# Bind("[ld-gsa-war-uni]") %>' />
                  <br />
                  ld-gsa-war-u-c:
                  <asp:Label ID="ld_gsa_war_u_cLabel" runat="server" 
                      Text='<%# Bind("[ld-gsa-war-u-c]") %>' />
                  <br />
                  ld-gsa-war-hdl:
                  <asp:Label ID="ld_gsa_war_hdlLabel" runat="server" 
                      Text='<%# Bind("[ld-gsa-war-hdl]") %>' />
                  <br />
                  ld-gsa-brd:
                  <asp:Label ID="ld_gsa_brdLabel" runat="server" 
                      Text='<%# Bind("[ld-gsa-brd]") %>' />
                  <br />
                  ld-gsa-mat:
                  <asp:Label ID="ld_gsa_matLabel" runat="server" 
                      Text='<%# Bind("[ld-gsa-mat]") %>' />
                  <br />
                  ld-gsa-lab:
                  <asp:Label ID="ld_gsa_labLabel" runat="server" 
                      Text='<%# Bind("[ld-gsa-lab]") %>' />
                  <br />
                  ld-gsa-war:
                  <asp:Label ID="ld_gsa_warLabel" runat="server" 
                      Text='<%# Bind("[ld-gsa-war]") %>' />
                  <br />
                  ld-gsa-war-pal:
                  <asp:Label ID="ld_gsa_war_palLabel" runat="server" 
                      Text='<%# Bind("[ld-gsa-war-pal]") %>' />
                  <br />
                  ld-gsa-war-amt:
                  <asp:Label ID="ld_gsa_war_amtLabel" runat="server" 
                      Text='<%# Bind("[ld-gsa-war-amt]") %>' />
                  <br />
                  ld-gsa-war-per:
                  <asp:Label ID="ld_gsa_war_perLabel" runat="server" 
                      Text='<%# Bind("[ld-gsa-war-per]") %>' />
                  <br />
                  <asp:LinkButton ID="UpdateButton" runat="server" CausesValidation="True" 
                      CommandName="edit" Text="Update" />
              </ItemTemplate>
          </asp:FormView>
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" 
              OldValuesParameterFormatString="original_{0}" SelectMethod="SelectGsa3" 
              TypeName="Corrugated">
              <SelectParameters>
                  <asp:Parameter DefaultValue="select"  Name="prmAction" Type="String" />
                  <asp:Parameter DefaultValue="Admin" Name="prmUser" Type="String" />
                  <asp:QueryStringParameter QueryStringField="probe"  Name="prmRowId" Type="String" />
                  <asp:Parameter  Name="prmIpQty" Type="Int32" />
                  <asp:Parameter  Name="prmIpRels" Type="Int32" />
                  <asp:SessionParameter SessionField="order_corrugated_est"  Name="prmEst" Type="String" />
                  <asp:Parameter  Name="prmQty" Type="Int32" />
                  <asp:Parameter  Name="prmBoard" Type="Decimal" />
                  <asp:Parameter  Name="prmMaterial" Type="Decimal" />
                  <asp:Parameter  Name="prmLabor" Type="Decimal" />
                  <asp:Parameter  Name="prmWHMark" Type="Decimal" />
                  <asp:Parameter Name="prmBrComm" Type="Decimal" />
                  <asp:Parameter  Name="prmUnCount" Type="Int32" />
                  <asp:Parameter  Name="prmPallCount" Type="Int32" />
                  <asp:Parameter  Name="prmTotUnit" Type="Decimal" />
                  <asp:Parameter  Name="prmUnitprPal" Type="Int32" />
                  <asp:Parameter  Name="prmPaHandCh" Type="Decimal" />
                  <asp:Parameter  Name="prmPallDelMon" Type="Int32" />
                  <asp:Parameter  Name="prmTotCharge" Type="Int32" />
                  <asp:Parameter  Name="prmCostPerPall" Type="Decimal" />
                  <asp:Parameter  Name="prmTotPall" Type="Int32" />                  
                <asp:QueryStringParameter QueryStringField="seq" Name="prmSeq" Type="Int32" />
                  
                  <asp:Parameter  Name="prmGetqty" Type="Int32" />
                  <asp:SessionParameter  Name="prmForm" SessionField="order_corrugated_formno" Type="Int32" />
                  <asp:SessionParameter  Name="prmBlank" SessionField="order_corrugated_blankno" Type="Int32" />
                  
                  <asp:Parameter Name="prmMatchup" Type="Decimal" />
                  <asp:Parameter Name="prmDoGsa" Type="String" />
                  <asp:Parameter Name="prmDoMr" Type="String" />
                  <asp:Parameter Name="prmDoSpeed" Type="String" />
                  <asp:Parameter Name="prmDropRc" Type="String" />
                  <asp:Parameter Name="prmInkAlForms" Type="String" />
                  <asp:Parameter Name="prmEstList" Type="String" />
                  <asp:Parameter Name="prmVendor" Type="String" />
                  
                  <asp:Parameter Name="prmGsaMat" Type="Decimal" />
                  <asp:Parameter Name="prmGsaLab" Type="Decimal" />
                  <asp:Parameter Name="prmGsaWar" Type="Decimal" />
                  <asp:Parameter Name="prmGsaFm" Type="Decimal" />
                  <asp:Parameter Name="prmGsaMonth" Type="Int32" />
              </SelectParameters>
          </asp:ObjectDataSource>
          <br />
          <br />
            
    </div>
    </form>
</body>
</html>
