<%@ Page Language="c#" AutoEventWireup="true" Debug="true" Inherits="fold_gsa_set" Codebehind="fold_gsa_set.aspx.cs" %>
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
              
              <table ><tr><td align="right" style="padding-right:5px"><b>Overrides for Qty:</b></td>
              <td><asp:Label ID="ld_qtyTextBox" runat="server" Text='<%# Bind("[ld-qty]") %>' /></td></tr>
              
              <tr><td align="right" style="padding-right:5px"><b>GS&A Board:</b></td>
              <td><asp:TextBox ID="ld_gsa_brdTextBox" runat="server" Text='<%# Bind("[ld-gsa-brd]") %>' />
              <asp:CompareValidator ID="CompareValidator1" ControlToValidate="ld_gsa_brdTextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Double" SetFocusOnError="true"  runat="server" ErrorMessage="Enter only Decimal Value"></asp:CompareValidator>                  
              </td></tr>                  
              
              <tr><td align="right" style="padding-right:5px"><b>GS&A Material:</b></td>
              <td><asp:TextBox ID="ld_gsa_matTextBox" runat="server" Text='<%# Bind("[ld-gsa-mat]") %>' />
              <asp:CompareValidator ID="CompareValidator2" ControlToValidate="ld_gsa_matTextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Double" SetFocusOnError="true"  runat="server" ErrorMessage="Enter only Decimal Value"></asp:CompareValidator>                  
              </td></tr>
              
              <tr><td align="right" style="padding-right:5px"><b>GS&A Labor:</b></td>
              <td><asp:TextBox ID="ld_gsa_labTextBox" runat="server" Text='<%# Bind("[ld-gsa-lab]") %>' />
              <asp:CompareValidator ID="CompareValidator3" ControlToValidate="ld_gsa_labTextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Double" SetFocusOnError="true"  runat="server" ErrorMessage="Enter only Decimal Value"></asp:CompareValidator>                  
              </td></tr>
              
              <tr><td align="right" style="padding-right:5px"><b>Wherehousing Markup:</b></td>
              <td><asp:TextBox ID="ld_gsa_warTextBox" runat="server" Text='<%# Bind("[ld-gsa-war]") %>' />             
              </td></tr>                
                <tr><td align="right" style="padding-right:5px"><b>Fold%:</b></td>
              <td><asp:TextBox ID="foldTextBox"  runat="server" Text='<%# Bind("[foldgsa]") %>' />
              <asp:CompareValidator ID="CompareValidator11" ControlToValidate="foldTextBox" Display="Dynamic" Operator="DataTypeCheck" Type="Double" SetFocusOnError="true"  runat="server" ErrorMessage="Enter only Decimal Value"></asp:CompareValidator>                  
              </td></tr>
              
              </table>
              </td>
              </tr>
              <tr><td>
               <asp:Button ID="UpdateButton" runat="server" CausesValidation="True" OnClick="updatebutton_click" CssClass="button"  Text="Save" />
       &nbsp;
        <input type="button" name="Cancel" class="buttonM" id="close" value="Cancel" onClick="javascript:self.close();window.opener.location.reload();"  />
              </td></tr>
              </table>
                 
                 
              </EditItemTemplate>
              
              
          </asp:FormView>
          <asp:ObjectDataSource ID="ObjectDataSource1" runat="server" 
              OldValuesParameterFormatString="original_{0}" SelectMethod="FoldSetGsa" 
              TypeName="Corrugated">
              <SelectParameters>
                  <asp:Parameter DefaultValue="select"  Name="prmAction" Type="String" />
                  <asp:Parameter DefaultValue="Admin" Name="prmUser" Type="String" />
                  <asp:QueryStringParameter QueryStringField="probe"  Name="prmRowId" Type="String" />
                  <asp:Parameter Name="prmIpQty" Type="Int32" />
                  <asp:Parameter Name="prmIpRels" Type="Int32" />
                  <asp:SessionParameter SessionField="order_folding_est"  Name="prmEst" Type="String" />
                  <asp:Parameter Name="prmQty" Type="Int32" />
                  <asp:Parameter Name="prmBoard" Type="Decimal" />
                  <asp:Parameter Name="prmMaterial" Type="Decimal" />
                  <asp:Parameter Name="prmLabor" Type="Decimal" />
                  <asp:Parameter Name="prmWHMark" Type="Decimal" />
                  <asp:Parameter Name="prmGsafm" Type="Decimal" />
                  <asp:QueryStringParameter QueryStringField="seq" Name="prmSeq" Type="Int32" />
                  <asp:Parameter Name="prmGetqty" Type="Int32" />
                  
                  <asp:SessionParameter Name="prmForm" SessionField="order_folding_formno" Type="Int32" />
                  <asp:SessionParameter Name="prmBlank" SessionField="order_folding_blankno" Type="Int32" />
                 
                  <asp:Parameter Name="prmDoGsa" Type="String" />
                  <asp:Parameter Name="prmDoMr" Type="String" />
                  <asp:Parameter Name="prmDoSpeed" Type="String" />                  
                  <asp:Parameter Name="prmEstList" Type="String" />
                  <asp:Parameter Name="prmVendor" Type="String" />
                  <asp:Parameter Name="prmGsaMat" Type="Decimal" />
                  <asp:Parameter Name="prmGsaLab" Type="Decimal" />
                  <asp:Parameter Name="prmGsaWar" Type="Decimal" />  
                  <asp:Parameter Name="prmGsaMonth" Type="Int32" /> 
                  
              </SelectParameters>
          </asp:ObjectDataSource>
          <br />
          <br />
            
    </div>
    </form>
</body>
</html>
