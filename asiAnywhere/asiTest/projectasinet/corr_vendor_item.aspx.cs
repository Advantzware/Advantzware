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

public partial class corr_vendor_item : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {       
        if (!Page.IsPostBack)
        {
            if (Session["User"] == null)
            {
                Response.Write("Session Expire Login again");
                return;
            }
            ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "select";
            FormView1.ChangeMode(FormViewMode.Edit);
        }
        //UserClass.CheckLogin(Page);
        try
        {
            UserClass UserLogin = (UserClass)Session["User"];
            ObjectDataSource1.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        }
        catch {
            Response.Write("Session Expire Login again");
        }

    }
    protected void btnSearch_Click(object sender, EventArgs e)
    {

    }
    protected void FormView1_DataBound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.Edit)
        {
            Label uom = (Label)FormView1.FindControl("vITEMTextBox");
            TextBox win28 = (TextBox)FormView1.FindControl("roll_w28TextBox");
            TextBox win30 = (TextBox)FormView1.FindControl("roll_w30TextBox");
            try
            {
                win28.Text = "999";
                win30.Text = "999";
                Session["coruom_lookup_item"] = uom.Text;
            }
            catch { }

            Label showlabel = (Label)FormView1.FindControl("showLabel");
           HtmlTable showtable = (HtmlTable)FormView1.FindControl("showtable");
            try
            {
                if (showlabel.Text == "Yes")
                    showtable.Visible = true;
                else
                    showtable.Visible = false;
            }
            catch { }
            try
            {
                Label rec_id1 = (Label)FormView1.FindControl("roid1Label");
                Label rec_id2 = (Label)FormView1.FindControl("roid2Label");
                TextBox vender = (TextBox)FormView1.FindControl("vend_itemTextBox");


                Session["corr_vendor_item_recid1"] = rec_id1.Text.Trim();
                Session["corr_vendor_item_recid2"] = rec_id2.Text.Trim();

                ObjectDataSource1.SelectParameters["prmVendItem"].DefaultValue = vender.Text.Trim();
                ObjectDataSource1.SelectParameters["prmRowid1"].DefaultValue = Convert.ToString(Session["corr_vendor_item_recid1"]);
                ObjectDataSource1.SelectParameters["prmRowid2"].DefaultValue = Convert.ToString(Session["corr_vendor_item_recid2"]);
            }
            catch { }

        }
        if (FormView1.CurrentMode == FormViewMode.ReadOnly)
        {
            Label showlabel = (Label)FormView1.FindControl("showLabel");
            HtmlTable showtable = (HtmlTable)FormView1.FindControl("showtable");
            try
            {
                if (showlabel.Text == "Yes")
                    showtable.Visible = true;
                else
                    showtable.Visible = false;
            }
            catch { }
            try
            {
                Label rec_id1 = (Label)FormView1.FindControl("Label1");
                Label rec_id2 = (Label)FormView1.FindControl("Label2");
                Label vender = (Label)FormView1.FindControl("vend_itemLabel");


                Session["corr_vendor_item_recid1"] = rec_id1.Text.Trim();
                Session["corr_vendor_item_recid2"] = rec_id2.Text.Trim();

                ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "View";
                ObjectDataSource1.SelectParameters["prmVendItem"].DefaultValue = vender.Text.Trim();
                ObjectDataSource1.SelectParameters["prmRowid1"].DefaultValue = Convert.ToString(Session["corr_vendor_item_recid1"]);
                ObjectDataSource1.SelectParameters["prmRowid2"].DefaultValue = Convert.ToString(Session["corr_vendor_item_recid2"]);
            }
            catch { }
        }
    }

    protected void Save_ButtonClick(object sender, EventArgs e)
    {
        Label rec_id1 = (Label)FormView1.FindControl("roid1Label");
        Label rec_id2 = (Label)FormView1.FindControl("roid2Label");
        Label venderitem = (Label)FormView1.FindControl("vITEMTextBox");
        TextBox stdum = (TextBox)FormView1.FindControl("stdumTextBox");
        TextBox venderItemno = (TextBox)FormView1.FindControl("vend_itemTextBox");
        TextBox vender = (TextBox)FormView1.FindControl("vend_noTextBox");
        TextBox costdate = (TextBox)FormView1.FindControl("updated_dateTextBox");

        TextBox runqty1 = (TextBox)FormView1.FindControl("run_qty1TextBox");
        TextBox runqty2 = (TextBox)FormView1.FindControl("run_qty2TextBox");
        TextBox runqty3 = (TextBox)FormView1.FindControl("run_qty3TextBox");
        TextBox runqty4 = (TextBox)FormView1.FindControl("run_qty4TextBox");
        TextBox runqty5 = (TextBox)FormView1.FindControl("run_qty5TextBox");
        TextBox runqty6 = (TextBox)FormView1.FindControl("run_qty6TextBox");
        TextBox runqty7 = (TextBox)FormView1.FindControl("run_qty7TextBox");
        TextBox runqty8 = (TextBox)FormView1.FindControl("run_qty8TextBox");
        TextBox runqty9 = (TextBox)FormView1.FindControl("run_qty9TextBox");
        TextBox runqty10 = (TextBox)FormView1.FindControl("run_qty10TextBox");

        TextBox runcost1 = (TextBox)FormView1.FindControl("run_cost1TextBox");
        TextBox runcost2 = (TextBox)FormView1.FindControl("run_cost2TextBox");
        TextBox runcost3 = (TextBox)FormView1.FindControl("run_cost3TextBox");
        TextBox runcost4 = (TextBox)FormView1.FindControl("run_cost4TextBox");
        TextBox runcost5 = (TextBox)FormView1.FindControl("run_cost5TextBox");
        TextBox runcost6 = (TextBox)FormView1.FindControl("run_cost6TextBox");
        TextBox runcost7 = (TextBox)FormView1.FindControl("run_cost7TextBox");
        TextBox runcost8 = (TextBox)FormView1.FindControl("run_cost8TextBox");
        TextBox runcost9 = (TextBox)FormView1.FindControl("run_cost9TextBox");
        TextBox runcost10 = (TextBox)FormView1.FindControl("run_cost10TextBox");

        TextBox setup1 = (TextBox)FormView1.FindControl("setups1TextBox");
        TextBox setup2 = (TextBox)FormView1.FindControl("setups2TextBox");
        TextBox setup3 = (TextBox)FormView1.FindControl("setups3TextBox");
        TextBox setup4 = (TextBox)FormView1.FindControl("setups4TextBox");
        TextBox setup5 = (TextBox)FormView1.FindControl("setups5TextBox");
        TextBox setup6 = (TextBox)FormView1.FindControl("setups6TextBox");
        TextBox setup7 = (TextBox)FormView1.FindControl("setups7TextBox");
        TextBox setup8 = (TextBox)FormView1.FindControl("setups8TextBox");
        TextBox setup9 = (TextBox)FormView1.FindControl("setups9TextBox");
        TextBox setup10 = (TextBox)FormView1.FindControl("setups10TextBox");

        TextBox rollw1 = (TextBox)FormView1.FindControl("roll_w1TextBox");
        TextBox rollw2 = (TextBox)FormView1.FindControl("roll_w2TextBox");
        TextBox rollw3 = (TextBox)FormView1.FindControl("roll_w3TextBox");
        TextBox rollw4 = (TextBox)FormView1.FindControl("roll_w4TextBox");
        TextBox rollw5 = (TextBox)FormView1.FindControl("roll_w5TextBox");
        TextBox rollw6 = (TextBox)FormView1.FindControl("roll_w6TextBox");
        TextBox rollw7 = (TextBox)FormView1.FindControl("roll_w7TextBox");
        TextBox rollw8 = (TextBox)FormView1.FindControl("roll_w8TextBox");
        TextBox rollw9 = (TextBox)FormView1.FindControl("roll_w9TextBox");
        TextBox rollw10 = (TextBox)FormView1.FindControl("roll_w10TextBox");
        TextBox rollw11 = (TextBox)FormView1.FindControl("roll_w11TextBox");
        TextBox rollw12 = (TextBox)FormView1.FindControl("roll_w12TextBox");
        TextBox rollw13 = (TextBox)FormView1.FindControl("roll_w13TextBox");
        TextBox rollw14 = (TextBox)FormView1.FindControl("roll_w14TextBox");
        TextBox rollw15 = (TextBox)FormView1.FindControl("roll_w15TextBox");
        TextBox rollw16 = (TextBox)FormView1.FindControl("roll_w16TextBox");
        TextBox rollw17 = (TextBox)FormView1.FindControl("roll_w17TextBox");
        TextBox rollw18 = (TextBox)FormView1.FindControl("roll_w18TextBox");
        TextBox rollw19 = (TextBox)FormView1.FindControl("roll_w19TextBox");
        TextBox rollw20 = (TextBox)FormView1.FindControl("roll_w20TextBox");
        TextBox rollw21 = (TextBox)FormView1.FindControl("roll_w21TextBox");
        TextBox rollw22 = (TextBox)FormView1.FindControl("roll_w22TextBox");
        TextBox rollw23 = (TextBox)FormView1.FindControl("roll_w23TextBox");
        TextBox rollw24 = (TextBox)FormView1.FindControl("roll_w24TextBox");
        TextBox rollw25 = (TextBox)FormView1.FindControl("roll_w25TextBox");
        TextBox rollw26 = (TextBox)FormView1.FindControl("roll_w26TextBox");
        TextBox rollw27 = (TextBox)FormView1.FindControl("roll_w27TextBox");
        TextBox rollw28 = (TextBox)FormView1.FindControl("roll_w28TextBox");
        TextBox rollw29 = (TextBox)FormView1.FindControl("roll_w29TextBox");
        TextBox rollw30 = (TextBox)FormView1.FindControl("roll_w30TextBox");

        TextBox widthmin = (TextBox)FormView1.FindControl("TextBoxLabel3");
        TextBox widthcst = (TextBox)FormView1.FindControl("TextBoxLabel4");
        TextBox lengthmin = (TextBox)FormView1.FindControl("TextBoxLabel5");
        TextBox lengthcst = (TextBox)FormView1.FindControl("TextBoxLabel6");

        Session["corr_vendor_item_recid1"] = rec_id1.Text.Trim();
        Session["corr_vendor_item_recid2"] = rec_id2.Text.Trim(); 

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "AddVendor";
        ObjectDataSource1.SelectParameters["prmRowid1"].DefaultValue = Convert.ToString(Session["corr_vendor_item_recid1"]);
        ObjectDataSource1.SelectParameters["prmRowid2"].DefaultValue = Convert.ToString(Session["corr_vendor_item_recid2"]);
        ObjectDataSource1.SelectParameters["prmStdum"].DefaultValue = stdum.Text.Trim();
        ObjectDataSource1.SelectParameters["prmVendItem"].DefaultValue = venderItemno.Text.Trim();
        ObjectDataSource1.SelectParameters["prmVendNO"].DefaultValue = vender.Text.Trim();
        ObjectDataSource1.SelectParameters["prmDate"].DefaultValue = costdate.Text.Trim();
       
        ObjectDataSource1.SelectParameters["prmRunQty1"].DefaultValue = runqty1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRunQty2"].DefaultValue = runqty2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRunQty3"].DefaultValue = runqty3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRunQty4"].DefaultValue = runqty4.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRunQty5"].DefaultValue = runqty5.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRunQty6"].DefaultValue = runqty6.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRunQty7"].DefaultValue = runqty7.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRunQty8"].DefaultValue = runqty8.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRunQty9"].DefaultValue = runqty9.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRunQty10"].DefaultValue = runqty10.Text.Trim();

        ObjectDataSource1.SelectParameters["prmRunCost1"].DefaultValue = runcost1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRunCost2"].DefaultValue = runcost2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRunCost3"].DefaultValue = runcost3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRunCost4"].DefaultValue = runcost4.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRunCost5"].DefaultValue = runcost5.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRunCost6"].DefaultValue = runcost6.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRunCost7"].DefaultValue = runcost7.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRunCost8"].DefaultValue = runcost8.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRunCost9"].DefaultValue = runcost9.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRunCost10"].DefaultValue = runcost10.Text.Trim();

        ObjectDataSource1.SelectParameters["prmSetups1"].DefaultValue = setup1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSetups2"].DefaultValue = setup2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSetups3"].DefaultValue = setup3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSetups4"].DefaultValue = setup4.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSetups5"].DefaultValue = setup5.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSetups6"].DefaultValue = setup6.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSetups7"].DefaultValue = setup7.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSetups8"].DefaultValue = setup8.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSetups9"].DefaultValue = setup9.Text.Trim();
        ObjectDataSource1.SelectParameters["prmSetups10"].DefaultValue = setup10.Text.Trim();

        ObjectDataSource1.SelectParameters["prmRollw1"].DefaultValue = rollw1.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRollw2"].DefaultValue = rollw2.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRollw3"].DefaultValue = rollw3.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRollw4"].DefaultValue = rollw4.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRollw5"].DefaultValue = rollw5.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRollw6"].DefaultValue = rollw6.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRollw7"].DefaultValue = rollw7.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRollw8"].DefaultValue = rollw8.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRollw9"].DefaultValue = rollw9.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRollw10"].DefaultValue = rollw10.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRollw11"].DefaultValue = rollw11.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRollw12"].DefaultValue = rollw12.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRollw13"].DefaultValue = rollw13.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRollw14"].DefaultValue = rollw14.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRollw15"].DefaultValue = rollw15.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRollw16"].DefaultValue = rollw16.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRollw17"].DefaultValue = rollw17.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRollw18"].DefaultValue = rollw18.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRollw19"].DefaultValue = rollw19.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRollw20"].DefaultValue = rollw20.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRollw21"].DefaultValue = rollw21.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRollw22"].DefaultValue = rollw22.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRollw23"].DefaultValue = rollw23.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRollw24"].DefaultValue = rollw24.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRollw25"].DefaultValue = rollw25.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRollw26"].DefaultValue = rollw26.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRollw27"].DefaultValue = rollw27.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRollw28"].DefaultValue = rollw28.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRollw29"].DefaultValue = rollw29.Text.Trim();
        ObjectDataSource1.SelectParameters["prmRollw30"].DefaultValue = rollw30.Text.Trim();

        ObjectDataSource1.SelectParameters["prmWidthmin"].DefaultValue = widthmin.Text.Trim();
        ObjectDataSource1.SelectParameters["prmLengthmin"].DefaultValue = lengthmin.Text.Trim();
        ObjectDataSource1.SelectParameters["prmWidthcst"].DefaultValue = widthcst.Text.Trim();
        ObjectDataSource1.SelectParameters["prmLengthcst"].DefaultValue = lengthcst.Text.Trim();

        FormView1.ChangeMode(FormViewMode.ReadOnly);
    }
    protected void UpdateButtonCencel_Click(object sender, EventArgs e)
    {
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "View";
    }

}
