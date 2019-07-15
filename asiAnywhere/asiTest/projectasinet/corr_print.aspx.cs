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

public partial class corr_print : System.Web.UI.Page
{
    string gfactcost;
    string gmargin;
    string gfullcost;
    string gqty;
    protected void Page_Load(object sender, EventArgs e)
    {
        Session["viewQuote_formview2_index"] = null;
        Print_ObjectDataSource.SelectParameters["prmAction"].DefaultValue = "Select";
        Print_ObjectDataSource2.SelectParameters["prmAction"].DefaultValue = "View";
       
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        if (Session["User"] != null)
        {
            string vUserId = UserLogin.UserName;
            string vPage = "corr_print.aspx";
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
            labelname.Text = "Corrugated";

            if (aUsers == "external")
            {
            }
            if (vCanRun == false)
            {
                Response.Write("<script>alert('Sorry! You don't have permission to access this page');</script>");
                Response.Write("<script>window.location.href = 'login.aspx';</script>");

            }
        }
        try
        {
            /*ImageButton print = (ImageButton)Master.FindControl("Img_Print");
            print.ImageUrl = "~/Images/print_1.jpg";*/
            Image img_mov_col = (Image)Master.FindControl("Image5");
            img_mov_col.Visible = false;
            Print_GridView.SelectedIndex = Convert.ToInt32(Session["corr_print_index"]);
            int rows = Print_GridView.Rows.Count;

            if (rows == 0)
            {
                Print_FormView.Visible = false;
            }
            if (Session["corr_print_index"] == null)
            {
                Print_GridView.SelectedIndex = 0;
                Session["corr_print_line"] = ((Label)Print_GridView.SelectedRow.FindControl("Label1")).Text;
            }
            gfactcost = Print_GridView.SelectedRow.Cells[1].Text;
            gmargin = Print_GridView.SelectedRow.Cells[3].Text;
            gfullcost = Print_GridView.SelectedRow.Cells[2].Text;
            gqty = Print_GridView.SelectedRow.Cells[0].Text;
        }
        catch { }
    }
    protected void Btn_Update_Click(object sender, EventArgs e)
    {
        //try
       // {
            TextBox gross = (TextBox)Print_FormView.FindControl("vGrossTextBox");
            TextBox net = (TextBox)Print_FormView.FindControl("vNetTextBox");
            TextBox sell = (TextBox)Print_FormView.FindControl("vTextSellPrice");
            DropDownList q = (DropDownList)Print_FormView.FindControl("vDropDownQ");            
            TextBox boardper = (TextBox)Print_FormView.FindControl("vTextBoardPer");
            TextBox bcontm = (TextBox)Print_FormView.FindControl("vTextBoardContM");
            TextBox bcont = (TextBox)Print_FormView.FindControl("vTextBoardCont");
                   
            Print_ObjectDataSource2.SelectParameters["prmAction"].DefaultValue = "Update";
            Print_ObjectDataSource2.SelectParameters["prmTotalFactCost"].DefaultValue = gfactcost;
            Print_ObjectDataSource2.SelectParameters["prmFullCost"].DefaultValue = gfullcost;
            //Print_ObjectDataSource2.SelectParameters["prmMargin"].DefaultValue = gmargin;
            Print_ObjectDataSource2.SelectParameters["prmFormQty"].DefaultValue = gqty;
            Print_ObjectDataSource2.SelectParameters["prmGross"].DefaultValue = gross.Text.Trim();
            Print_ObjectDataSource2.SelectParameters["prmNet"].DefaultValue = net.Text.Trim();
            Print_ObjectDataSource2.SelectParameters["prmSellPrice"].DefaultValue = sell.Text.Trim();
            Print_ObjectDataSource2.SelectParameters["prmQty2"].DefaultValue = q.SelectedValue;
            Print_ObjectDataSource2.SelectParameters["prmBoard"].DefaultValue = boardper.Text.Trim();
            Print_ObjectDataSource2.SelectParameters["prmBoardContM"].DefaultValue = bcontm.Text.Trim();
            Print_ObjectDataSource2.SelectParameters["prmBoardCont"].DefaultValue = bcont.Text.Trim();
            Print_ObjectDataSource2.SelectParameters["prmLine"].DefaultValue = Convert.ToString(Session["corr_print_line"]);
            Print_FormView.ChangeMode(FormViewMode.ReadOnly);
            Response.Write("<script>window.location.href='corr_print.aspx'</script>");
       // }
        //catch { }
    }
    protected void Print_GridView_SelectedIndexChanged(object sender, EventArgs e)
    {
        Session["corr_print_index"] = Print_GridView.SelectedIndex;
        Session["corr_print_line"] = ((Label)Print_GridView.SelectedRow.FindControl("Label1")).Text;
    }
    protected void Btn_Delete_Click(object sender, EventArgs e)
    {
        Label compname = (Label)Master.FindControl("lblComp");
        Print_ObjectDataSource2.SelectParameters["prmAction"].DefaultValue = "Delete";
        Print_ObjectDataSource2.SelectParameters["prmComp"].DefaultValue=compname.Text;
        Print_ObjectDataSource2.SelectParameters["prmEstimate"].DefaultValue=Convert.ToString(Session["order_corrugated_est"]);
        Print_ObjectDataSource2.SelectParameters["prmLine"].DefaultValue=Convert.ToString(Session["corr_print_line"]);
        
        Response.Write("<script>window.location.href='corr_print.aspx'</script>");
       
        Session["corr_print_index"] = null;
    }
    
    protected void Btn_view_Click(object sender, EventArgs e)
    {
        UserClass.CheckLogin(Page);
        UserClass UserLogin = (UserClass)Session["User"];
        ObjectDataSource2.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
        ObjectDataSource2.SelectParameters["prmAction"].DefaultValue = "Alpha";
        ObjectDataSource2.SelectParameters["prmOut"].DefaultValue = "No";

        ObjectDataSource2.SelectParameters["prmEstimate"].DefaultValue = Convert.ToString(Session["order_corrugated_est"]);
        ObjectDataSource2.SelectParameters["prmFormNo"].DefaultValue = Convert.ToString(Session["order_corrugated_formno"]);
        ObjectDataSource2.SelectParameters["prmBlankNo"].DefaultValue = Convert.ToString(Session["order_corrugated_blankno"]);
        ObjectDataSource2.SelectParameters["prmLine"].DefaultValue = Convert.ToString(Session["corr_print_line"]);


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
                            Response.Redirect("CorrEstimatePrint.aspx");
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

    protected void Btn_printbox_Click(object sender, EventArgs e)
    {
       
        if (Print_GridView.Rows.Count >= 1)
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            ObjectDataSource3.SelectParameters["prmUser"].DefaultValue = UserLogin.UserName;
            ObjectDataSource3.SelectParameters["prmAction"].DefaultValue = "Alpha";
            ObjectDataSource3.SelectParameters["prmOut"].DefaultValue = "Yes";

            ObjectDataSource3.SelectParameters["prmEstimate"].DefaultValue = Convert.ToString(Session["order_corrugated_est"]);
            ObjectDataSource3.SelectParameters["prmFormNo"].DefaultValue = Convert.ToString(Session["order_corrugated_formno"]);
            ObjectDataSource3.SelectParameters["prmBlankNo"].DefaultValue = Convert.ToString(Session["order_corrugated_blankno"]);
            ObjectDataSource3.SelectParameters["prmLine"].DefaultValue = Convert.ToString(Session["corr_print_line"]);


            try
            {
                Label path = (Label)FormView_PrintBox.FindControl("vCorrEstimateBoxFileLabel");
                string path2 = @"/pdfs/" + path.Text;
                if (path.Text != "")
                {
                    Session["corr_est_list_printtext"] = path2;
                    if (path2 != "")
                    {
                        if (!Request.Browser.Browser.Contains("Safari"))
                            Response.Write("<script>window.open('print_corr_text.aspx'); target='_blank'</script>");
                        /*else
                            Response.Redirect("CorrEstimatePrint.aspx");
                        */
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
        else
        {
            HttpContext.Current.Response.Write("<script>alert('No Probe Record is available.')</script>" );
            HttpContext.Current.Response.Write("<script>history.back()</script>");
        }
    }

    protected void Btn_calc_Click(object sender, EventArgs e)
    {   
        Response.Write("<script>window.open('estimate_analysis.aspx', 'EstimateAnalysisWindow', 'width=500, height=500'); target='_blank'</script>");
    }

    protected void print_formview_databound(object sender, EventArgs e)
    {
        if (FormView1.CurrentMode == FormViewMode.ReadOnly)
        {
            Btn_calc.Visible = false;
            PrintBox.Visible = false;
        }
        if (Print_FormView.CurrentMode == FormViewMode.Edit)
        {
            TextBox gross = (TextBox)Print_FormView.FindControl("vGrossTextBox");
            gross.Focus();
        }
    }

    protected void cal_text_change(object sender, EventArgs e)
    {
        TextBox gross = (TextBox)Print_FormView.FindControl("vGrossTextBox");
        TextBox net = (TextBox)Print_FormView.FindControl("vNetTextBox");
        TextBox sellingprice = (TextBox)Print_FormView.FindControl("vTextSellPrice");
        TextBox board = (TextBox)Print_FormView.FindControl("vTextBoardPer");
        TextBox boardcountm = (TextBox)Print_FormView.FindControl("vTextBoardContM");
        TextBox boardcount = (TextBox)Print_FormView.FindControl("vTextBoardCont");
        Label boardm = (Label)Print_FormView.FindControl("Label8");
        Label market = (Label)Print_FormView.FindControl("Label4");
        Label factcost = (Label)Print_FormView.FindControl("Label2");
        Label fullcost = (Label)Print_FormView.FindControl("Label3");
        DropDownList q = (DropDownList)Print_FormView.FindControl("vDropDownQ");
        try
        {
            UserClass.CheckLogin(Page);
            UserClass UserLogin = (UserClass)Session["User"];
            Corrugated curr = new Corrugated();
            DataSet ds = new DataSet();
            ds = curr.ProbeWatif("cal", UserLogin.UserName, HiddenField1.Value, Convert.ToDecimal(gross.Text), Convert.ToDecimal(net.Text), Convert.ToDecimal(sellingprice.Text), "", Convert.ToDecimal(boardm.Text), Convert.ToDecimal(board.Text), Convert.ToDecimal(boardcountm.Text), Convert.ToDecimal(boardcount.Text), Convert.ToDecimal(market.Text), Convert.ToDecimal(factcost.Text), Convert.ToDecimal(fullcost.Text), Convert.ToString(Session["order_corrugated_est"]), Convert.ToInt32(Session["corr_print_line"]));
            if (HiddenField1.Value != "G")
            {
                gross.Text = ds.Tables[0].Rows[0][0].ToString();
            }
            if (HiddenField1.Value != "N")
            {
                net.Text = ds.Tables[0].Rows[0][1].ToString();
            }
            if (HiddenField1.Value != "S")
            {
                sellingprice.Text = ds.Tables[0].Rows[0][2].ToString();
            }
            if (HiddenField1.Value != "B")
            {
                board.Text = ds.Tables[0].Rows[0][4].ToString();
            }
            if (HiddenField1.Value != "BCM")
            {
                boardcountm.Text = ds.Tables[0].Rows[0][5].ToString();
            }
            if (HiddenField1.Value != "BC$")
            {
                boardcount.Text = ds.Tables[0].Rows[0][6].ToString();
            }
            market.Text = ds.Tables[0].Rows[0][7].ToString();
            fullcost.Text = ds.Tables[0].Rows[0][9].ToString();

            if (HiddenField1.Value == "G")
                net.Focus();
            else if (HiddenField1.Value == "N")
                sellingprice.Focus();
            else if (HiddenField1.Value == "S")
                board.Focus();
            else if (HiddenField1.Value == "B")
                boardcountm.Focus();
            else if (HiddenField1.Value == "GCM")
                boardcount.Focus();
            else if (HiddenField1.Value == "BC$")
                gross.Focus();
        }
        catch { }
    }

    protected void Quote_Button_Click(object sender, EventArgs e)
    {
        Label compname = (Label)Master.FindControl("lblComp");
        Print_ObjectDataSource2.SelectParameters["prmAction"].DefaultValue = "Quote";
        Print_ObjectDataSource2.SelectParameters["prmComp"].DefaultValue = compname.Text;
        Print_ObjectDataSource2.SelectParameters["prmEstimate"].DefaultValue = Convert.ToString(Session["order_corrugated_est"]);
        Print_ObjectDataSource2.SelectParameters["prmLine"].DefaultValue = Convert.ToString(Session["corr_print_line"]);
        Print_ObjectDataSource2.SelectParameters["prmForm"].DefaultValue = Convert.ToString(Session["order_corrugated_formno"]);
        

        Session["prmEstimate_quote_list"] = Session["order_corrugated_est"];
        //Response.Redirect("BrowseQuote.aspx");
        Response.Write("<script>window.location.href='BrowseQuote.aspx'</script>");

    }
}
