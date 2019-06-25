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

public partial class fold_print : System.Web.UI.Page
{
    protected void Page_Load(object sender, EventArgs e)
    {
        Fold_Print_ObjectDataSource.SelectParameters["prmAction"].DefaultValue = "Select";
        Fold_Print_ObjectDataSource2.SelectParameters["prmAction"].DefaultValue = "View";
        
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "fold_print.aspx";
            string aUsers = null;
            string PrmComp = null;
            bool vCanCreate = false;
            bool vCanRun = false;
            bool vCanUpdate = false;
            bool vCanDelete = false;

            func1 f1 = new func1();            
            f1.CheckProgramPermissions(vPage, vUserId, ref  vCanCreate, ref  vCanRun, ref  vCanUpdate, ref  vCanDelete, ref  PrmComp, ref  aUsers);

            Label compname = (Label)Master.FindControl("lblComp");
            Label username = (Label)Master.FindControl("lblUser");
            Label labelname = (Label)Master.FindControl("lbl_page");
            compname.Text = PrmComp;
            username.Text = UserLogin.UserName;
            labelname.Text = "Folding";

            if (aUsers == "external")
            {
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }
        if (!Page.IsPostBack)
        {
           
            Session["fold_print_index"] = null;
           
        }

        try
        {
            
            Image img_mov_col = (Image)Master.FindControl("Image5");
            img_mov_col.Visible = false;
        }
        catch { }
          //  Response.Write(Session["fold_print_index"]);
            try
            {
                Print_GridView.SelectedIndex = Convert.ToInt32(Session["fold_print_index"]);
                if (Session["fold_print_index"] == null)
                {
                    Print_GridView.SelectedIndex = 0;
                    Session["fold_est_line"] = ((Label)Print_GridView.SelectedRow.FindControl("Label1")).Text;
                    
                }
            }
            catch { }
    }
    protected void Btn_Update_Click(object sender, EventArgs e)
    {
        Label fullcost = (Label)Print_FormView.FindControl("vFullCostLabel");
        TextBox margin = (TextBox)Print_FormView.FindControl("vMarginTextBox");
        Label comm = (Label)Print_FormView.FindControl("vCommLabel");
        Label net = (Label)Print_FormView.FindControl("vNetLabel");
        TextBox selling = (TextBox)Print_FormView.FindControl("vSellPriceTextBox");
        Label totsheet = (Label)Print_FormView.FindControl("vTotalSheetLabel");
        DropDownList qt = (DropDownList)Print_FormView.FindControl("vQty2TextBox");


        Label compname = (Label)Master.FindControl("lblComp");
        Fold_Print_ObjectDataSource2.SelectParameters["prmAction"].DefaultValue = "Update";
        Fold_Print_ObjectDataSource2.SelectParameters["prmComp"].DefaultValue = compname.Text;
        Fold_Print_ObjectDataSource2.SelectParameters["prmEstimate"].DefaultValue = Convert.ToString(Session["order_folding_est"]);
        Fold_Print_ObjectDataSource2.SelectParameters["prmLine"].DefaultValue = Convert.ToString(Session["fold_est_line"]);

        Fold_Print_ObjectDataSource2.SelectParameters["prmFullCost"].DefaultValue = fullcost.Text.Trim();
        Fold_Print_ObjectDataSource2.SelectParameters["prmMargin"].DefaultValue = margin.Text.Trim();
        Fold_Print_ObjectDataSource2.SelectParameters["prmComm"].DefaultValue = comm.Text.Trim();
        Fold_Print_ObjectDataSource2.SelectParameters["prmNet"].DefaultValue = net.Text.Trim();
        Fold_Print_ObjectDataSource2.SelectParameters["prmSellPrice"].DefaultValue = selling.Text.Trim();
        Fold_Print_ObjectDataSource2.SelectParameters["prmTotalSheet"].DefaultValue = totsheet.Text.Trim();
        Fold_Print_ObjectDataSource2.SelectParameters["prmQty2"].DefaultValue = qt.SelectedValue;
       
        Print_FormView.ChangeMode(FormViewMode.ReadOnly);
        Response.Write("<script>window.location.href='fold_print.aspx'</script>");
    }
    protected void Delete_Button_Click(object sender, EventArgs e)
    {
        Label compname = (Label)Master.FindControl("lblComp");
        Fold_Print_ObjectDataSource2.SelectParameters["prmAction"].DefaultValue = "Delete";
        Fold_Print_ObjectDataSource2.SelectParameters["prmComp"].DefaultValue = compname.Text;
        Fold_Print_ObjectDataSource2.SelectParameters["prmEstimate"].DefaultValue = Convert.ToString(Session["order_folding_est"]);
        Fold_Print_ObjectDataSource2.SelectParameters["prmLine"].DefaultValue = Convert.ToString(Session["fold_est_line"]);
        Print_FormView.ChangeMode(FormViewMode.ReadOnly);
        Response.Write("<script>window.location.href='fold_print.aspx'</script>");
        Session["fold_print_index"] = null;
        
    }
    protected void Print_GridView_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["fold_print_index"] = Print_GridView.SelectedIndex;
        Session["fold_est_line"] = ((Label)Print_GridView.SelectedRow.FindControl("Label1")).Text;
                
    }

    protected void Btn_view_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource2.SelectParameters["prmAction"].DefaultValue = "Alpha";
        ObjectDataSource2.SelectParameters["prmOut"].DefaultValue = "No";

        ObjectDataSource2.SelectParameters["prmEstimate"].DefaultValue = Convert.ToString(Session["order_folding_est"]);
        ObjectDataSource2.SelectParameters["prmFormNo"].DefaultValue = Convert.ToString(Session["order_folding_formno"]);
        ObjectDataSource2.SelectParameters["prmBlankNo"].DefaultValue = Convert.ToString(Session["order_folding_blankno"]);
        ObjectDataSource2.SelectParameters["prmLine"].DefaultValue = Convert.ToString(Session["fold_est_line"]);


        try
        {
            Label path = (Label)FormView1.FindControl("vCorrEstimateFileLabel");
            string path2 = @"/pdfs/" + path.Text;

            if (path.Text != "")
            {
                Session["corr_est_list_printtext"] = path2;
                if (path2 != "")
                {
                    if (!Request.Browser.Browser.Contains("Safari"))
                        Response.Write("<script>window.open('print_corr_text.aspx'); target='_blank'</script>");
                    else
                        Response.Redirect("FoldEstimatePrint.aspx");
                }
            }
            else
            {
                //Label1.Text = "No Pdf Exists";
            }
        }
        catch
        {
            //Label1.Text = "No Pdf Exists";
        }        

    }
    protected void print_formview_databound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.ReadOnly)
        {            
            if(Print_GridView.Rows.Count > 0)
            Btn_calc.Visible = false;
            
        }
        if (Print_FormView.CurrentMode == FormViewMode.Edit)
        {
            Label commname = (Label)Print_FormView.FindControl("commnamelabel");
            Label grossname = (Label)Print_FormView.FindControl("grossnameLabel");
            Label comm = (Label)Print_FormView.FindControl("vCommLabel");
            TextBox gross = (TextBox)Print_FormView.FindControl("vGrossTextBox");
            if (Convert.ToString(Session["fold_est_print_logic_showhide"]) == "no")
            {
                grossname.Visible = false;
                gross.Visible = false;
            }
            else
            {
                comm.Visible = false;
                commname.Visible = false;
            }

        }
       
    }

    protected void Btn_importprice_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];

        Fold_Print_ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        Fold_Print_ObjectDataSource2.SelectParameters["prmAction"].DefaultValue = "ImportPrice";        
        Fold_Print_ObjectDataSource2.SelectParameters["prmBlk"].DefaultValue = Convert.ToString(Session["order_folding_blankno"]);        
    }

    protected void cal_text_change(object sender, EventArgs e)
    {
        Label comm = (Label)Print_FormView.FindControl("vCommLabel");
        TextBox gross = (TextBox)Print_FormView.FindControl("vGrossTextBox");
        Label net = (Label)Print_FormView.FindControl("vNetLabel");
        TextBox market = (TextBox)Print_FormView.FindControl("vMarginTextBox");
        TextBox sellingprice = (TextBox)Print_FormView.FindControl("vSellPriceTextBox");        
        Label totalsheet = (Label)Print_FormView.FindControl("vTotalSheetLabel");
                
        Label factcost = (Label)Print_FormView.FindControl("vTotalFactCostLabel");
        Label fullcost = (Label)Print_FormView.FindControl("vFullCostLabel");
        DropDownList q = (DropDownList)Print_FormView.FindControl("vDropDownQ");
        if (gross.Text == "")
            gross.Text = "0";
        if (market.Text == "")
            market.Text = "0";
        if (sellingprice.Text == "")
            sellingprice.Text = "0";
        try
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            Corrugated curr = new Corrugated();
            DataSet ds = new DataSet();
            ds = curr.ProbeWatif("FoldingWhatifCal", UserLogin.UserName, HiddenField1.Value, Convert.ToDecimal(gross.Text), Convert.ToDecimal(net.Text), Convert.ToDecimal(sellingprice.Text), "", 0, 0, 0, 0, Convert.ToDecimal(market.Text.Trim()), Convert.ToDecimal(factcost.Text), Convert.ToDecimal(fullcost.Text), Convert.ToString(Session["order_folding_est"]), Convert.ToInt32(Session["fold_est_line"]));
            if (HiddenField1.Value != "M")
            {
                market.Text = ds.Tables[0].Rows[0][7].ToString();
            }
            if (HiddenField1.Value != "S")
            {
                sellingprice.Text = ds.Tables[0].Rows[0][2].ToString();
            }
            
            net.Text = ds.Tables[0].Rows[0][1].ToString();
            if (HiddenField1.Value != "G")
            {
                gross.Text = ds.Tables[0].Rows[0][0].ToString();
            }
            fullcost.Text = ds.Tables[0].Rows[0][9].ToString();
            comm.Text = ds.Tables[0].Rows[0][10].ToString();

            if (HiddenField1.Value == "M")
            {
              if(gross.Visible == true)
                gross.Focus();
              else
                  sellingprice.Focus();
            }
            else if (HiddenField1.Value == "G")
                sellingprice.Focus();
            else if (HiddenField1.Value == "S")
                q.Focus();
            
        }
        catch { }
    }

    protected void GridView_RowDataBound(object sender, GridViewRowEventArgs e)
    {
       
        try
        {
            if (Convert.ToString(Session["fold_est_print_logic_showhide"]) == "no")
                Print_GridView.Columns[7].Visible = false;
            else
                Print_GridView.Columns[6].Visible = false;

        }
        catch { }
       
       
                
    }

    protected void FormView_MiscSub_ondataBound(object sender, EventArgs e)
    {
        
    }


    protected void Quote_Button_Click(object sender, EventArgs e)
    {
        Label compname = (Label)Master.FindControl("lblComp");
        UserClass UserLogin = (UserClass)Session["User"];
        Fold_Print_ObjectDataSource2.SelectParameters["prmAction"].DefaultValue = "Quote";
        Fold_Print_ObjectDataSource2.SelectParameters["prmComp"].DefaultValue = compname.Text;
        Fold_Print_ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        Fold_Print_ObjectDataSource2.SelectParameters["prmEstimate"].DefaultValue = Convert.ToString(Session["order_folding_est"]);
        Fold_Print_ObjectDataSource2.SelectParameters["prmLine"].DefaultValue = Convert.ToString(Session["fold_est_line"]);
        Fold_Print_ObjectDataSource2.SelectParameters["prmForm"].DefaultValue = Convert.ToString(Session["order_folding_formno"]);


        Session["prmEstimate_quote_list"] = Session["order_folding_est"];
        Response.Write("<script>window.location.href='BrowseQuote.aspx'</script>");

    }
}
