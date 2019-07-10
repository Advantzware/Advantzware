using System;
using System.Data;
using System.Configuration;
using System.Collections;
using System.Web;
using System.Web.Security;
using System.Web.UI;
using System.Web.UI.WebControls;
using System.Web.UI.WebControls.WebParts;
using System.Web.UI.HtmlControls;

public partial class fg_item1 : System.Web.UI.Page
{
    int copybutton = 0;
    string usercust = "";
    protected void Page_Load(object sender, EventArgs e)
    {     
        if (Session["Item_Inquiry_value_check"] != null)
        {           
            LinkButton1.Visible = false;
            LinkButton2.Visible = false;
            LinkButton3.Visible = false;
        }
        else
        {
            //ImageButton iteminquiry = (ImageButton)Master.FindControl("listitem");
            //iteminquiry.Visible = false;
            HtmlGenericControl iteminquiry = (HtmlGenericControl)this.Page.Master.FindControl("lilistitem");
            iteminquiry.Attributes.Add("style", "display:none");

            if (Session["view_order_entry_pages"] == null)
            {
                LinkButton2.Visible = false;
            }
            if (Session["view_order_entry_pages"] != null)
            {
                LinkButton1.Visible = false;
            }

            if (Session["view_order_entry_pages_with_estimate"] != null)
            {
                LinkButton1.Visible = false;
                LinkButton2.Visible = false;
            }
            if (Session["view_order_entry_pages_with_estimate"] == null)
            {
                LinkButton3.Visible = false;
            }
        }
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        if (Session["User"] != null)
        {

            string vUserId = UserLogin.UserName;
            string vPage = "fgitem.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();
            //Response.Write(Page);
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
            //lblComp.Text = PrmComp;
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
            if (aUsers == "external")
            {
                usercust = "external";
            }
            if (aUsers == "internal")
            {
                usercust = "internal";
            }
        }
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Select";
       
        try
        {
            Label name = (Label)Master.FindControl("lbl_page");
            name.Text = "View Item";
            FormView1.ChangeMode(FormViewMode.ReadOnly);
            
            Label fgitemname = (Label)FormView1.FindControl("ItemLabel");
            Session["newfgitemnameval"] = fgitemname.Text;
        }
        catch { }


        //ImageButton viewitem = (ImageButton)Master.FindControl("viewitem");
        //viewitem.ImageUrl = "~/img/viewitem1.jpg";
        if (!Page.IsPostBack)
        {
        
        }
    }
    protected void FormView1_ItemUpdated(object sender, FormViewUpdatedEventArgs e)
    {
        //this.FormView1.DataBind();
    }

    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        try
        {
            if (FormView1.CurrentMode == FormViewMode.ReadOnly)
            {              
                Button add = (Button)FormView1.FindControl("AddButton");
                Button update = (Button)FormView1.FindControl("UpdateButton");
                Button delete = (Button)FormView1.FindControl("DeleteButton");
                Button recal_Button = (Button)FormView1.FindControl("recal_Button");
                Button copy = (Button)FormView1.FindControl("Copy_button");

                if (Session["item_show_hide_button"] != null)
                {
                    add.Visible = false;
                    update.Visible = false;
                    delete.Visible = false;
                    recal_Button.Visible = false;
                    copy.Visible = false;
                }
                UserClass.CheckLogin(Page);
                UserClass UserLogin = (UserClass)Session["User"];
                if (Session["User"] != null)
                {

                    string vUserId = UserLogin.UserName;
                    string vPage = "fgitem.aspx";
                    string aUsers = null;
                    string PrmComp = null;
                    bool vCanCreate = false;
                    bool vCanRun = false;
                    bool vCanUpdate = false;
                    bool vCanDelete = false;

                    func1 f1 = new func1();
                    //Response.Write(Page);
                    f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);
                    //lblComp.Text = PrmComp;                    
                    if (aUsers == "external")
                    {
                        Label matelcost = (Label)FormView1.FindControl("Label24");
                        Label totalcost = (Label)FormView1.FindControl("Label25");
                        Label labercost = (Label)FormView1.FindControl("Label27");
                        Label averagecost = (Label)FormView1.FindControl("Label28");
                        Label varohcost = (Label)FormView1.FindControl("Label29");
                        Label lastcost = (Label)FormView1.FindControl("Label30");
                        Label fixohcost = (Label)FormView1.FindControl("Label31");
                        Label costuom = (Label)FormView1.FindControl("Label32");
                        Label label1 = (Label)FormView1.FindControl("costlabel");
                        Label label2 = (Label)FormView1.FindControl("Label33");
                        Label label3 = (Label)FormView1.FindControl("Label34");
                        Label label4 = (Label)FormView1.FindControl("Label35");
                        Label label5 = (Label)FormView1.FindControl("Label36");
                        Label label6 = (Label)FormView1.FindControl("Label37");
                        Label label7 = (Label)FormView1.FindControl("Label38");
                        Label label8 = (Label)FormView1.FindControl("Label39");

                        recal_Button.Visible = false;
                        matelcost.Visible = false;
                        totalcost.Visible = false;
                        labercost.Visible = false;
                        averagecost.Visible = false;
                        varohcost.Visible = false;
                        lastcost.Visible = false;
                        fixohcost.Visible = false;
                        costuom.Visible = false;
                        label1.Visible = false;
                        label2.Visible = false;
                        label3.Visible = false;
                        label4.Visible = false;
                        label5.Visible = false;
                        label6.Visible = false;
                        label7.Visible = false;
                        label8.Visible = false;

                    }
                }

            }
            if (FormView1.CurrentMode == FormViewMode.Edit)
            {
                Button copy = (Button)FormView1.FindControl("copy_save_Button");
                Button update = (Button)FormView1.FindControl("UpdateButton");
                TextBox fgitem = (TextBox)FormView1.FindControl("Item1TextBox");
                String usercostfram = ((Label)FormView1.FindControl("usercostframLabel")).Text;
                if (copybutton == 1)
                {
                    update.Visible = false;
                }
                if (copybutton == 2)
                {
                    copy.Visible = false;
                    fgitem.ReadOnly = true;
                    fgitem.BackColor = System.Drawing.Color.Turquoise;
                }

                if (usercostfram != "Yes")
                {
                    TextBox StdMat      = (TextBox)FormView1.FindControl("StdMatTextBox");
                    TextBox StdTot      = (TextBox)FormView1.FindControl("StdTotTextBox");
                    TextBox StdLab      = (TextBox)FormView1.FindControl("StdLabTextBox");
                    TextBox Average     = (TextBox)FormView1.FindControl("AverageTextBox");
                    TextBox StdVar      = (TextBox)FormView1.FindControl("StdVarTextBox");
                    TextBox LastCost    = (TextBox)FormView1.FindControl("LastCostTextBox");
                    TextBox StdFix      = (TextBox)FormView1.FindControl("StdFixTextBox");
                    TextBox LastUom     = (TextBox)FormView1.FindControl("LastUomTextBox");

                    StdMat.Enabled = false;
                    StdMat.BackColor = System.Drawing.Color.Turquoise;
                    StdTot.Enabled = false;
                    StdTot.BackColor = System.Drawing.Color.Turquoise;
                    StdLab.Enabled = false;
                    StdLab.BackColor = System.Drawing.Color.Turquoise;
                    Average.Enabled = false;
                    Average.BackColor = System.Drawing.Color.Turquoise;
                    StdVar.Enabled = false;
                    StdVar.BackColor = System.Drawing.Color.Turquoise;
                    LastCost.Enabled = false;
                    LastCost.BackColor = System.Drawing.Color.Turquoise;
                    StdFix.Enabled = false;
                    StdFix.BackColor = System.Drawing.Color.Turquoise;
                    LastUom.Enabled = false;
                    LastUom.BackColor = System.Drawing.Color.Turquoise;
                }                                   
                    if (usercust == "external")
                    {
                        TextBox matelcost = (TextBox)FormView1.FindControl("StdMatTextBox");
                        TextBox totalcost = (TextBox)FormView1.FindControl("StdTotTextBox");
                        TextBox labercost = (TextBox)FormView1.FindControl("StdLabTextBox");
                        TextBox averagecost = (TextBox)FormView1.FindControl("AverageTextBox");
                        TextBox varohcost = (TextBox)FormView1.FindControl("StdVarTextBox");
                        TextBox lastcost = (TextBox)FormView1.FindControl("LastCostTextBox");
                        TextBox fixohcost = (TextBox)FormView1.FindControl("StdFixTextBox");
                        TextBox costuom = (TextBox)FormView1.FindControl("LastUomTextBox");

                        Label label1 = (Label)FormView1.FindControl("costlabel");
                        Label label2 = (Label)FormView1.FindControl("Label33");
                        Label label3 = (Label)FormView1.FindControl("Label34");
                        Label label4 = (Label)FormView1.FindControl("Label35");
                        Label label5 = (Label)FormView1.FindControl("Label36");
                        Label label6 = (Label)FormView1.FindControl("Label37");
                        Label label7 = (Label)FormView1.FindControl("Label38");
                        Label label8 = (Label)FormView1.FindControl("Label39");


                        matelcost.Visible = false;
                        totalcost.Visible = false;
                        labercost.Visible = false;
                        averagecost.Visible = false;
                        varohcost.Visible = false;
                        lastcost.Visible = false;
                        fixohcost.Visible = false;
                        costuom.Visible = false;
                        label1.Visible = false;
                        label2.Visible = false;
                        label3.Visible = false;
                        label4.Visible = false;
                        label5.Visible = false;
                        label6.Visible = false;
                        label7.Visible = false;
                        label8.Visible = false;

                    
                }

            }
           
        }catch
        {}

        if (FormView1.CurrentMode == FormViewMode.Insert)
        {                    
                if (usercust == "external")
                {
                    TextBox matelcost = (TextBox)FormView1.FindControl("StdMatTextBox");
                    TextBox totalcost = (TextBox)FormView1.FindControl("StdTotTextBox");
                    TextBox labercost = (TextBox)FormView1.FindControl("StdLabTextBox");
                    TextBox averagecost = (TextBox)FormView1.FindControl("AverageTextBox");
                    TextBox varohcost = (TextBox)FormView1.FindControl("StdVarTextBox");
                    TextBox lastcost = (TextBox)FormView1.FindControl("LastCostTextBox");
                    TextBox fixohcost = (TextBox)FormView1.FindControl("StdFixTextBox");
                    TextBox costuom = (TextBox)FormView1.FindControl("LastUomTextBox");

                    Label label1 = (Label)FormView1.FindControl("costlabel");
                    Label label2 = (Label)FormView1.FindControl("Label33");
                    Label label3 = (Label)FormView1.FindControl("Label34");
                    Label label4 = (Label)FormView1.FindControl("Label35");
                    Label label5 = (Label)FormView1.FindControl("Label36");
                    Label label6 = (Label)FormView1.FindControl("Label37");
                    Label label7 = (Label)FormView1.FindControl("Label38");
                    Label label8 = (Label)FormView1.FindControl("Label39");


                    matelcost.Visible = false;
                    totalcost.Visible = false;
                    labercost.Visible = false;
                    averagecost.Visible = false;
                    varohcost.Visible = false;
                    lastcost.Visible = false;
                    fixohcost.Visible = false;
                    costuom.Visible = false;
                    label1.Visible = false;
                    label2.Visible = false;
                    label3.Visible = false;
                    label4.Visible = false;
                    label5.Visible = false;
                    label6.Visible = false;
                    label7.Visible = false;
                    label8.Visible = false;
                                    
            }
        }
    }

    protected void UpdateButton_Click1(object sender, EventArgs e)
    {
        CheckBox IsSet = (CheckBox)FormView1.FindControl("IsSetCheckBox");        
        CheckBox Tax = (CheckBox)FormView1.FindControl("TaxCheckBox");
        TextBox custpart = (TextBox)FormView1.FindControl("PartTextBox");
        TextBox Cust = (TextBox)FormView1.FindControl("CustTextBox");
        TextBox CustName = (TextBox)FormView1.FindControl("CustNameTextBox");
        TextBox Name = (TextBox)FormView1.FindControl("Name1TextBox");
        CheckBox Casepal = (CheckBox)FormView1.FindControl("casepalCheckBox");
        TextBox Stat = (TextBox)FormView1.FindControl("StatTextBox");        
        TextBox Dscr = (TextBox)FormView1.FindControl("DscrTextBox");
        TextBox Dscr2 = (TextBox)FormView1.FindControl("Dscr2TextBox");
        TextBox Dscr3 = (TextBox)FormView1.FindControl("Dscr3TextBox");                                        
        TextBox Estimate = (TextBox)FormView1.FindControl("EstimateTextBox");
        TextBox Style = (TextBox)FormView1.FindControl("StyleTextBox");
        TextBox die = (TextBox)FormView1.FindControl("dieTextBox");
        TextBox Plate = (TextBox)FormView1.FindControl("PlateTextBox");
        TextBox Cad = (TextBox)FormView1.FindControl("CadTextBox");
        TextBox SPC = (TextBox)FormView1.FindControl("SPCTextBox");
        TextBox UPC = (TextBox)FormView1.FindControl("UPCTextBox");
        TextBox Sell = (TextBox)FormView1.FindControl("SellTextBox");
        TextBox Uom = (TextBox)FormView1.FindControl("UOMTextBox");
        TextBox Categ = (TextBox)FormView1.FindControl("CategTextBox");
        TextBox Rpt = (TextBox)FormView1.FindControl("RptTextBox");
        TextBox Curr = (TextBox)FormView1.FindControl("CurrTextBox");        
        TextBox WareHouse = (TextBox)FormView1.FindControl("WareHouseTextBox");
        TextBox Inventory = (TextBox)FormView1.FindControl("InventoryTextBox");
        TextBox Bin = (TextBox)FormView1.FindControl("BinTextBox");
        TextBox Cycle = (TextBox)FormView1.FindControl("CycleTextBox");
        TextBox Count = (TextBox)FormView1.FindControl("CountTextBox");
        TextBox Production = (TextBox)FormView1.FindControl("ProductionTextBox");
        TextBox Weight = (TextBox)FormView1.FindControl("WeightTextBox");
        TextBox Packing = (TextBox)FormView1.FindControl("PackingTextBox");
        TextBox Freight = (TextBox)FormView1.FindControl("FreightTextBox");
        TextBox FreClass = (TextBox)FormView1.FindControl("FreClassTextBox");                            
        CheckBox Exempt = (CheckBox)FormView1.FindControl("ExemptCheckBox");

        RadioButtonList purchase = (RadioButtonList)FormView1.FindControl("purchaseRadioButtonList");
        RadioButtonList stock = (RadioButtonList)FormView1.FindControl("stockRadioButtonList");
        RadioButtonList ship = (RadioButtonList)FormView1.FindControl("ShipRadioButtonList");
        RadioButtonList status = (RadioButtonList)FormView1.FindControl("statusRadioButtonList");
        DropDownList type = (DropDownList)FormView1.FindControl("DropDownList1");

        if (IsSet.Checked)
        {
            HiddenField1.Value = "Yes";
        }
        else
            HiddenField1.Value = "No";
        if (purchase.SelectedIndex == 0)
        {
            HiddenField2.Value = "Yes";
        }
        if (purchase.SelectedIndex == 1)
        {
            HiddenField2.Value = "No";
        }
        if (stock.SelectedIndex == 0)
            HiddenField3.Value = "S";
        if (stock.SelectedIndex == 1)
            HiddenField3.Value = "C";

        if (ship.SelectedIndex == 0)
            HiddenField4.Value = "Yes";
        if (ship.SelectedIndex == 1)
            HiddenField4.Value = "No";

        if (status.SelectedIndex == 0)
            HiddenField5.Value = "A";
        if (status.SelectedIndex == 1)
            HiddenField5.Value = "I";
        if (Tax.Checked)
            HiddenField6.Value = "Yes";
        else
            HiddenField6.Value = "No";
       

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Update";

        ObjectDataSource1.SelectParameters["prmPart"].DefaultValue = custpart.Text.Trim();
        ObjectDataSource1.SelectParameters["prmName"].DefaultValue = Name.Text.Trim();
        ObjectDataSource1.SelectParameters["prmDscr"].DefaultValue = Dscr.Text.Trim();
        ObjectDataSource1.SelectParameters["prmDscr2"].DefaultValue = Dscr2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmDscr3"].DefaultValue = Dscr3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmIsSet"].DefaultValue = HiddenField1.Value;
        ObjectDataSource1.SelectParameters["prmIsCust"].DefaultValue = Cust.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustName"].DefaultValue = CustName.Text.Trim();
        ObjectDataSource1.SelectParameters["prmTax"].DefaultValue = HiddenField6.Value;
        ObjectDataSource1.SelectParameters["prmPurchase"].DefaultValue = HiddenField2.Value;
        ObjectDataSource1.SelectParameters["prmEstimate"].DefaultValue = Estimate.Text.Trim();
        ObjectDataSource1.SelectParameters["prmStyle"].DefaultValue = Style.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdie"].DefaultValue = die.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPlate"].DefaultValue = Plate.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCad"].DefaultValue = Cad.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSPC"].DefaultValue = SPC.Text.Trim();
        ObjectDataSource1.SelectParameters["prmUPC"].DefaultValue = UPC.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSell"].DefaultValue = Sell.Text.Trim();
        ObjectDataSource1.SelectParameters["prmUOM"].DefaultValue = Uom.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCurr"].DefaultValue = Curr.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCateg"].DefaultValue = Categ.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRpt"].DefaultValue = Rpt.Text.Trim();
        ObjectDataSource1.SelectParameters["prmWareHouse"].DefaultValue = WareHouse.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBin"].DefaultValue = Bin.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCount"].DefaultValue = Count.Text.Trim();
        ObjectDataSource1.SelectParameters["prmWeight"].DefaultValue = Weight.Text.Trim();
        ObjectDataSource1.SelectParameters["prmFreight"].DefaultValue = Freight.Text.Trim();
        ObjectDataSource1.SelectParameters["prmFreClass"].DefaultValue = FreClass.Text.Trim();
        ObjectDataSource1.SelectParameters["prmInventory"].DefaultValue = Inventory.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCycle"].DefaultValue = Cycle.Text.Trim();
        ObjectDataSource1.SelectParameters["prmProduction"].DefaultValue = Production.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPacking"].DefaultValue = Packing.Text.Trim();
        ObjectDataSource1.SelectParameters["prmExempt"].DefaultValue = Exempt.Text.Trim();
        ObjectDataSource1.SelectParameters["prmStat"].DefaultValue = HiddenField5.Value;
        ObjectDataSource1.SelectParameters["prmstock"].DefaultValue = HiddenField3.Value;
        ObjectDataSource1.SelectParameters["prmcasepal"].DefaultValue = HiddenField4.Value;
        ObjectDataSource1.SelectParameters["prmtypecode"].DefaultValue = type.SelectedValue;

        if (usercust == "internal")
        {            
            TextBox StdMat = (TextBox)FormView1.FindControl("StdMatTextBox");
            TextBox StdTot = (TextBox)FormView1.FindControl("StdTotTextBox");
            TextBox StdLab = (TextBox)FormView1.FindControl("StdLabTextBox");
            TextBox Average = (TextBox)FormView1.FindControl("AverageTextBox");
            TextBox StdVar = (TextBox)FormView1.FindControl("StdVarTextBox");
            TextBox LastCost = (TextBox)FormView1.FindControl("LastCostTextBox");
            TextBox StdFix = (TextBox)FormView1.FindControl("StdFixTextBox");
            TextBox LastUom = (TextBox)FormView1.FindControl("LastUomTextBox");

            ObjectDataSource1.SelectParameters["prmMat"].DefaultValue = StdMat.Text.Trim();
            ObjectDataSource1.SelectParameters["prmLab"].DefaultValue = StdLab.Text.Trim();
            ObjectDataSource1.SelectParameters["prmVar"].DefaultValue = StdVar.Text.Trim();
            ObjectDataSource1.SelectParameters["prmFix"].DefaultValue = StdFix.Text.Trim();
            ObjectDataSource1.SelectParameters["prmStd"].DefaultValue = StdTot.Text.Trim();
            ObjectDataSource1.SelectParameters["prmAvg"].DefaultValue = Average.Text.Trim();
            ObjectDataSource1.SelectParameters["prmLast"].DefaultValue = LastCost.Text.Trim();
            ObjectDataSource1.SelectParameters["prmlUom"].DefaultValue = LastUom.Text.Trim();

        }


    }


    protected void InsertButton_Click1(object sender, EventArgs e)
    {
        TextBox fgitem = (TextBox)FormView1.FindControl("Item1TextBox");
        CheckBox IsSet = (CheckBox)FormView1.FindControl("IsSetCheckBox");
        CheckBox Tax = (CheckBox)FormView1.FindControl("TaxCheckBox");
        TextBox custpart = (TextBox)FormView1.FindControl("PartTextBox");
        TextBox Cust = (TextBox)FormView1.FindControl("CustTextBox");
        TextBox CustName = (TextBox)FormView1.FindControl("CustNameTextBox");
        TextBox Name = (TextBox)FormView1.FindControl("Name1TextBox");
        CheckBox Casepal = (CheckBox)FormView1.FindControl("casepalCheckBox");
        TextBox Stat = (TextBox)FormView1.FindControl("StatTextBox");
        TextBox Dscr = (TextBox)FormView1.FindControl("DscrTextBox");
        TextBox Dscr2 = (TextBox)FormView1.FindControl("Dscr2TextBox");
        TextBox Dscr3 = (TextBox)FormView1.FindControl("Dscr3TextBox");
        TextBox Estimate = (TextBox)FormView1.FindControl("EstimateTextBox");
        TextBox Style = (TextBox)FormView1.FindControl("StyleTextBox");
        TextBox die = (TextBox)FormView1.FindControl("dieTextBox");
        TextBox Plate = (TextBox)FormView1.FindControl("PlateTextBox");
        TextBox Cad = (TextBox)FormView1.FindControl("CadTextBox");
        TextBox SPC = (TextBox)FormView1.FindControl("SPCTextBox");
        TextBox UPC = (TextBox)FormView1.FindControl("UPCTextBox");
        TextBox Sell = (TextBox)FormView1.FindControl("SellTextBox");
        TextBox Uom = (TextBox)FormView1.FindControl("UOMTextBox");
        TextBox Categ = (TextBox)FormView1.FindControl("CategTextBox");
        TextBox Rpt = (TextBox)FormView1.FindControl("RptTextBox");
        TextBox Curr = (TextBox)FormView1.FindControl("CurrTextBox");
        TextBox WareHouse = (TextBox)FormView1.FindControl("WareHouseTextBox");
        TextBox Inventory = (TextBox)FormView1.FindControl("InventoryTextBox");
        TextBox Bin = (TextBox)FormView1.FindControl("BinTextBox");
        TextBox Cycle = (TextBox)FormView1.FindControl("CycleTextBox");
        TextBox Count = (TextBox)FormView1.FindControl("CountTextBox");
        TextBox Production = (TextBox)FormView1.FindControl("ProductionTextBox");
        TextBox Weight = (TextBox)FormView1.FindControl("WeightTextBox");
        TextBox Packing = (TextBox)FormView1.FindControl("PackingTextBox");
        TextBox Freight = (TextBox)FormView1.FindControl("FreightTextBox");
        TextBox FreClass = (TextBox)FormView1.FindControl("FreClassTextBox");
        CheckBox Exempt = (CheckBox)FormView1.FindControl("ExemptCheckBox");

        RadioButtonList purchase = (RadioButtonList)FormView1.FindControl("purchaseRadioButtonList");
        RadioButtonList stock = (RadioButtonList)FormView1.FindControl("stockRadioButtonList");
        RadioButtonList ship = (RadioButtonList)FormView1.FindControl("ShipRadioButtonList");
        RadioButtonList status = (RadioButtonList)FormView1.FindControl("statusRadioButtonList");
        DropDownList type = (DropDownList)FormView1.FindControl("DropDownList1");

        if (IsSet.Checked)
        {
            HiddenField1.Value = "Yes";
        }
        else
            HiddenField1.Value = "No";
        if (purchase.SelectedIndex == 0)
        {
            HiddenField2.Value = "Yes";
        }
        if (purchase.SelectedIndex == 1)
        {
            HiddenField2.Value = "No";
        }
        if (stock.SelectedIndex == 0)
            HiddenField3.Value = "S";
        if (stock.SelectedIndex == 1)
            HiddenField3.Value = "C";

        if (ship.SelectedIndex == 0)
            HiddenField4.Value = "Yes";
        if (ship.SelectedIndex == 1)
            HiddenField4.Value = "No";

        if (status.SelectedIndex == 0)
            HiddenField5.Value = "A";
        if (status.SelectedIndex == 1)
            HiddenField5.Value = "I";
        if (Tax.Checked)
            HiddenField6.Value = "Yes";
        else
            HiddenField6.Value = "No";


        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Save";
        ObjectDataSource1.SelectParameters["prmNewItem"].DefaultValue = fgitem.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPart"].DefaultValue = custpart.Text.Trim();
        ObjectDataSource1.SelectParameters["prmName"].DefaultValue = Name.Text.Trim();
        ObjectDataSource1.SelectParameters["prmDscr"].DefaultValue = Dscr.Text.Trim();
        ObjectDataSource1.SelectParameters["prmDscr2"].DefaultValue = Dscr2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmDscr3"].DefaultValue = Dscr3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmIsSet"].DefaultValue = HiddenField1.Value;
        ObjectDataSource1.SelectParameters["prmIsCust"].DefaultValue = Cust.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCustName"].DefaultValue = CustName.Text.Trim();
        ObjectDataSource1.SelectParameters["prmTax"].DefaultValue = HiddenField6.Value;
        ObjectDataSource1.SelectParameters["prmPurchase"].DefaultValue = HiddenField2.Value;
        ObjectDataSource1.SelectParameters["prmEstimate"].DefaultValue = Estimate.Text.Trim();
        ObjectDataSource1.SelectParameters["prmStyle"].DefaultValue = Style.Text.Trim();
        ObjectDataSource1.SelectParameters["prmdie"].DefaultValue = die.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPlate"].DefaultValue = Plate.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCad"].DefaultValue = Cad.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSPC"].DefaultValue = SPC.Text.Trim();
        ObjectDataSource1.SelectParameters["prmUPC"].DefaultValue = UPC.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSell"].DefaultValue = Sell.Text.Trim();
        ObjectDataSource1.SelectParameters["prmUOM"].DefaultValue = Uom.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCurr"].DefaultValue = Curr.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCateg"].DefaultValue = Categ.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRpt"].DefaultValue = Rpt.Text.Trim();
        ObjectDataSource1.SelectParameters["prmWareHouse"].DefaultValue = WareHouse.Text.Trim();
        ObjectDataSource1.SelectParameters["prmBin"].DefaultValue = Bin.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCount"].DefaultValue = Count.Text.Trim();
        ObjectDataSource1.SelectParameters["prmWeight"].DefaultValue = Weight.Text.Trim();
        ObjectDataSource1.SelectParameters["prmFreight"].DefaultValue = Freight.Text.Trim();
        ObjectDataSource1.SelectParameters["prmFreClass"].DefaultValue = FreClass.Text.Trim();
        ObjectDataSource1.SelectParameters["prmInventory"].DefaultValue = Inventory.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCycle"].DefaultValue = Cycle.Text.Trim();
        ObjectDataSource1.SelectParameters["prmProduction"].DefaultValue = Production.Text.Trim();
        ObjectDataSource1.SelectParameters["prmPacking"].DefaultValue = Packing.Text.Trim();
        ObjectDataSource1.SelectParameters["prmExempt"].DefaultValue = Exempt.Text.Trim();
        ObjectDataSource1.SelectParameters["prmStat"].DefaultValue = HiddenField5.Value;
        ObjectDataSource1.SelectParameters["prmstock"].DefaultValue = HiddenField3.Value;
        ObjectDataSource1.SelectParameters["prmcasepal"].DefaultValue = HiddenField4.Value;
        ObjectDataSource1.SelectParameters["prmtypecode"].DefaultValue = type.SelectedValue;

        if (usercust == "internal")
        {
            TextBox StdMat = (TextBox)FormView1.FindControl("StdMatTextBox");
            TextBox StdTot = (TextBox)FormView1.FindControl("StdTotTextBox");
            TextBox StdLab = (TextBox)FormView1.FindControl("StdLabTextBox");
            TextBox Average = (TextBox)FormView1.FindControl("AverageTextBox");
            TextBox StdVar = (TextBox)FormView1.FindControl("StdVarTextBox");
            TextBox LastCost = (TextBox)FormView1.FindControl("LastCostTextBox");
            TextBox StdFix = (TextBox)FormView1.FindControl("StdFixTextBox");
            TextBox LastUom = (TextBox)FormView1.FindControl("LastUomTextBox");

            ObjectDataSource1.SelectParameters["prmMat"].DefaultValue = StdMat.Text.Trim();
            ObjectDataSource1.SelectParameters["prmLab"].DefaultValue = StdLab.Text.Trim();
            ObjectDataSource1.SelectParameters["prmVar"].DefaultValue = StdVar.Text.Trim();
            ObjectDataSource1.SelectParameters["prmFix"].DefaultValue = StdFix.Text.Trim();
            ObjectDataSource1.SelectParameters["prmStd"].DefaultValue = StdTot.Text.Trim();
            ObjectDataSource1.SelectParameters["prmAvg"].DefaultValue = Average.Text.Trim();
            ObjectDataSource1.SelectParameters["prmLast"].DefaultValue = LastCost.Text.Trim();
            ObjectDataSource1.SelectParameters["prmlUom"].DefaultValue = LastUom.Text.Trim();

        }
    }

    protected void DeleteButton_Click(object sender, EventArgs e)
    {
        Label item = (Label)FormView1.FindControl("ItemLabel");
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Delete";
        ObjectDataSource1.SelectParameters["prmItemNum"].DefaultValue = item.Text.Trim();

    }

    protected void LinkButton2_Click(object sender, EventArgs e)
    {
        Response.Redirect("order_entry.aspx");
    }
    protected void LinkButton_Click(object sender, EventArgs e)
    {
        Response.Redirect("order_inquiry.aspx");
    }

    protected void LinkButton3_Click(object sender, EventArgs e)
    {
        Response.Redirect("order_estimate.aspx");
    }
    protected void formview_prerander(object sender, EventArgs e)
    {
        try
        {
            Label item = (Label)FormView1.FindControl("ItemLabel");
            Session["item_list_item"] = item.Text;
        }
        catch { }
    }
    protected void Copy_button_Click(object sender, EventArgs e)
    {
        copybutton = 1;  
    }
    protected void Update_button_Click(object sender, EventArgs e)
    {
        copybutton = 2;
    }

    protected void recal_cost_Click(object sender, EventArgs e)
    {
        Label item = (Label)FormView1.FindControl("ItemLabel");
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "Recalcost";
        ObjectDataSource1.SelectParameters["prmItemNum"].DefaultValue = item.Text.Trim();
                       
    }
}