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

public partial class corr_vend_fgitem : System.Web.UI.Page
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
        catch { }

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
            win28.Text = "999";
            win30.Text = "999";
            Session["coruom_lookup_item"] = uom.Text;
           
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
        TextBox cust = (TextBox)FormView1.FindControl("custTextBox");

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

        TextBox rollw1 = (TextBox)FormView1.FindControl("roll_w27TextBox");
        TextBox rollw2 = (TextBox)FormView1.FindControl("roll_w28TextBox");
        TextBox rollw3 = (TextBox)FormView1.FindControl("roll_w29TextBox");
        TextBox rollw4 = (TextBox)FormView1.FindControl("roll_w30TextBox");
        TextBox markup = (TextBox)FormView1.FindControl("MarkupTextBox");
        
        Session["corr_vendor_item_recid1"] = rec_id1.Text.Trim();
        Session["corr_vendor_item_recid2"] = rec_id2.Text.Trim(); 

        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "AddVendor";
        ObjectDataSource1.SelectParameters["prmRowid1"].DefaultValue = Convert.ToString(Session["corr_vendor_item_recid1"]);
        ObjectDataSource1.SelectParameters["prmRowid2"].DefaultValue = Convert.ToString(Session["corr_vendor_item_recid2"]);
        ObjectDataSource1.SelectParameters["prmStdum"].DefaultValue = stdum.Text.Trim();
        ObjectDataSource1.SelectParameters["prmVendItem"].DefaultValue = venderItemno.Text.Trim();
        ObjectDataSource1.SelectParameters["prmVendNO"].DefaultValue = vender.Text.Trim();
        ObjectDataSource1.SelectParameters["prmCust"].DefaultValue = cust.Text.Trim();
       
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
        ObjectDataSource1.SelectParameters["prmMarkup"].DefaultValue = markup.Text.Trim();
        

        FormView1.ChangeMode(FormViewMode.ReadOnly);
    }
    protected void UpdateButtonCencel_Click(object sender, EventArgs e)
    {
        ObjectDataSource1.SelectParameters["prmAction"].DefaultValue = "View";
    }

}
